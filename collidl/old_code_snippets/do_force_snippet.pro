
;--------------------------------------------------------------

if (do_force eq 1) then begin

  bondsenergy=bondsenergy(0:bondcount-1)
  bondsx=bondsx(0:bondcount-1)
  bondsy=bondsy(0:bondcount-1)

  triangulate, bondsx, bondsy, btriangles, boutermost, CONNECTIVITY = bedges  ;bedges, btriangles created here!
  ;boutermost=0

  ;bedges=0   moved down below *****

  bcutoff=3*howmuch
  ;get an interpolated image of the bonds angles
  benergy=trigrid(bondsx,bondsy,bondsenergy,btriangles, NX=(!xss+1),NY=(!yss+1))

  ;Also down below, but these are large memory hogs, and may need to be removed for this
  ;code to work on large images.
  btriangles=0
  bondsenergy=0

  ; Do gaussian smoothing on the bond angle file
  smoothbenergy=fltarr(!xss+1,!yss+1)
  smooth,benergy, smoothbenergy, weights, howmuch

  print, 'Displaying smoothed force field...'
  x=findgen(!xss+1)#replicate(1.,!yss+1)
  y=replicate(1.,!xss+1)#findgen(!yss+1)
  ;      smoothbangle1=interpolate(smoothbangle,x/4.0,y/4.0,/cubic)


  smoothbenergy=reverse(bytscl(smoothbenergy, MIN=0, MAX=bondslength^2.0/20))
  image1=[[[smoothbenergy]],[[0*smoothbenergy]],[[0*smoothbenergy]]]
  smoothbenergy=0  ;local to this if, can be killed here
  showimage,image1,3,wbenergy
  saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'force.tif',/tiff

  energimg=readimage(0)

  ;      Now we have origimg, bondsimg, energimg to work with



  newimg=origimg*.5+energimg*.5

  energimg=0  ;only exists in this method, so remove here
  ;try,newimg




  newred=reform(newimg[0,*,*])
  newgreen=reform(newimg[1,*,*])
  newblue=reform(newimg[2,*,*])
  newbred=reform(bondsimg[0,*,*])
  newbgreen=reform(bondsimg[1,*,*])
  newbblue=reform(bondsimg[2,*,*])


  whatbondstokeep=where((rgbcolor(newbred,newbgreen,newbblue) ne !colorwhite) and (rgbcolor(newbred,newbgreen,newbblue) ne !colorbond))
  newred[whatbondstokeep]=newbred[whatbondstokeep]
  newgreen[whatbondstokeep]=newbgreen[whatbondstokeep]
  newblue[whatbondstokeep]=newbblue[whatbondstokeep]
  showimage,[[[newred]],[[newgreen]],[[newblue]]],3,wcombinedenerg

  saveimage,strmid(fs[i],0,strlen(fs[i])-4)+'energall.tif', /TIFF
  ;      nang=size(angimg)
  ;      angimg1=fltarr(nang[2],nang[3],3)
  ;      angimg1[*,*,0]=angimg[0,*,*]
  ;      angimg1[*,*,1]=angimg[1,*,*]
  ;      angimg1[*,*,2]=angimg[2,*,*]
  ;      window,9
  ;      tv, angimg1,true=3

  ;      print,'Writing angle TIFF file'
  ;      write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'smooth.tif',bytscl(smoothbangle,MAX= !pi/3,MIN=0)



  bedges=0 ;created in do_force and do_bonds conditionals only!
endif ;end of if (do_force eq 1)
