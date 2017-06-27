
;This program is supposed to read an image file, determine the sphere positions,
;do the triangulation and generate the orientation field

; MAIN PROGRAM

; Search for current work area to find code changes in progress
; USAGE notes (1/31/2006, by Matt Trawick)
; 1.  Start IDL
; 2. At IDL prompt, type: cd,'C:\Research Data\Collidl'
; 3. File -> Open... collidlv1.48.pro, or whatever the latest version is
; 4. Run -> Compile All
; 5. Run -> Compile All  (Again!)
; 6. At IDL prompt, type: main, spheresize=6, /stay (or whatever switches you want).

;v1.48 notes (as of 8/09/05):
;   By Matt Trawick
;   Adding a new internal switch, "postscript_defects", which generates a postscript file output of
;   disclinations and dislocations, similar to the old C version.  (Advantage: Postscript has arbitrarily high
;   resolution, so image has potential to look much nicer.  Disadvantage: slow, need to convert manually to .tif
;   file eventually.
;
;v1.45 notes (as of 6/23/05):
;   By Brian Salmons
;
;   This version seeks to eliminate some of the annoying problems inherent in the program and
;   in v1.4.  There are many changes, including:
;     -Now checks whether to close the windows automatically when there are multiple images run through
;     -Now checks to see if any images were actually selected, preventing a crash
;     -Global workaround.  At the cost of slight speed (about two seconds at 4096x4096 pixels),
;      the IDL global variables have been worked around and are passed.  This prevents the need to
;      constantly reset the IDE and allows multiple images to be run in a single pass as originally
;      designed.
;     -Better memory clearing.  Memory clears that were in if-loops have now been moved to exterior locations
;      or cleared in multiple areas.  Some variables include testing to determine whether they are used,
;      indicating whether they should be created or not.
;     -Better memory management.  Some variables used to be created at the beginning that were not used until
;      the end.  Many of these have been fixed and most variables are created when needed and cleared after.
;     -New Switch: /filtered added.  This indicates that the image has already been filtered externally before
;      loading into this application, and thus additional filtering is not needed or may be harmfull to
;      location data.  This tag simply skips the filter process within the code.  Also helps on extremely
;      large images in which there is not enough memory for the fft code in the band pass filter.
;     -Now allows image "trains" (multiple images within one run) to be analyzed (including large images).
;
;;      -Windows only version.  Relies on Virtual Memory, rather than IDL Pixmap for greater range
;
;
;   On a final note, the /hardspheres and /noimage switches are UNTESTED in this version (due to
;   no available testing data), and it is unknown at this time if they are broken.
;
;   Feasible maximum image size in this version is 4096x4096, seems regardless of scale.
;
;
;
;
;V1.4 notes (as of 6/22/05):
;   By Brian Salmons
;
;   This version of the program has limitations placed on it by the interface with windows
;   in how it displays large images.  On our test machine, images of 2048x2048 pixels
;   could be displayed in all four windows at once (original, bonds, angle, and all).
;   However, at the next largest power of two (4096x4096 pixels), Windows ran out of virtual
;   memory of the backbuffer.  A backbuffer is needed, due to the save files being generated
;   from the displayed images; without the backbuffer, only what is on screen is saved and
;   the rest is lost.  Similarly, the pixmap buffers also ran out of memory, and thus had
;   to be switched to remain=1 (windows virtual memory), eliminating the functionality on X-windows
;   systems at this size and larger.  To solve this, only two images can be displayed at a time using
;   windows virtual memory (worth checking with IDL pixmap for X-windows functionality, though pixmap overflow
;   forced us to comment out the copy code in the saveimage() routine.  In its current state, it is simply
;   TVRD().  Therefore, it may not be possible to similarly use pixmaps for displaying)).  Therefore,
;   an automatic check is in place to close windows if the open image is larger than 2048x2048.
;
;   In future versions, there will also be a similar switch on display method in the displaying of images,
;   so that small image function can be restored to X-windows machines, while allowing Windows to operate
;   on larger images than pixmap memory allows.  Furthermore, try blocks will be in place to allow
;   backwards compatibility for machines to default from both pixmap and to closing images
;   if memory is not available.
;
;   ALL of these conditions to the operation of this program as found above ARE machine specific.  Different
;   machines are capable of different values, and in this version, code modification is the only way to change
;   for individual machines.  *To be fixed in future versions.
;
;   V1.4 changes:
;      -Now allows any size image, with a feasible maximum of around 4096x4096 pixels, subject to
;          individual machines capabilities.
;      -Memory allocations are not universal.  This allows for drastically faster smaller image
;          analysis and a theoretically infinite top end.  *Requires reseting of IDE to operate on different
;        sized images!*
;      -Forces closing of first two windows (original image and bonds) at image sizes over 2048x2048
;          pixels to free virtual memory.
;      -Closing of non-relevant arrays and some deletions.  Some arrays were unneeded or only temporarily
;          usefull.  These now close after finished to free memory.
;      -New switches:
;             -/Scale: scales the image to 1024.   -or-
;             -Scale=(number): scales the image by a factor of (number).
;             -spheresize=(number): sets the spheresize to (number), rather than arbitray values.
;      -Obsolete switches:
;             -/big          (no longer available)
;             -/simu        (replaced by spheresize)
;             -/twomicron     (replaced by spheresize)
;             -/onemicron     (replaced by spheresize)
;      -Windows only version.  Relies on Virtual Memory, rather than IDL Pixmap for greater range
;
;
;   On a final note, the /hardspheres and /noimage switches are UNTESTED in this version (due to
;   no available testing data), and it is unknown at this time if they are broken.
;



pro main,saveloc=saveloc,flip=flip,scale=scale,spheresize=sphere_diameter,simu=simu,stay=stay,wait=wait,twomicron=twomicron,unfilt=unfilt,noimage=noimage, onemicron=onemicron, hardsphere=hardsphere, filtered=filtered

    ;*****************************************************

       ; Safe Switches :
       ;     /filtered : tells IDL that the image is already filtered, and does not need the additional
       ;        bandpass filter.  This causes it to skip the call to bpass() and can help with large
       ;        images that occupy too much memory for internal filtering to be accomplished along side
       ;        analysis.  In this case, external pre-filtering is a better option.
       ;     /flip: tells IDL to invert the image colors before analyzing it.  May become standard if
       ;       our procedure has this being used every run
       ;     /scale : if set using /scale, will automatically scale the image to 1024x1024.  If set using
       ;        scale=(number), then the number becomes the scale factor
       ;     /spheresize : set using the spheresize=(number), it sets the sphere_diameter variable,
       ;        making /twomicron,/simu,/onemicron obsolete
       ;     /stay : in order to keep the windows on the screen. Useful
       ;        when working with only one file, a pain otherwise
       ;     /wait : waits for 5 secs after each file processed
       ;
       ;
       ; Untested Switches:
       ;     /noimage : gets the vertices from a user routine rather than from a file (currently untested!)
       ;     /hardsphere : to use with the hardsphere simu (currently untested!)
       ;
       ;
       ; Old Switches:
       ;    /big : replaced by the functionality of the scale keyword. (no longer available)
       ;    /simu : sets the sphere-locator parameters to Dan Vega's simulation size (obsolete: use spheresize)
       ;    /twomicron : sets the parameters for 2-micron picture (obsolete: use spheresize)
       ;    /onemicron : sets the parameters for 1-micron picture (obsolete: use spheresize)
    ;*****************************************************

       ; These are various switches used to control the behaviour of the
       ; program
       ; 0 = do it, 1 = don't
       ; CAREFUL, this is different from the truth value in C !


       ; all of these are old ways of doing correlations, either not
       ; reliabe or way too slow
       translatecorr=1 ; calculate correlations by the image-translation technique
       directcorr=1 ; calculate correlations directly
       ffcorr=1 ; do the Fourier transform trick to get correlations
       brutecorr=1 ; do the brute force calculations in IDL.
       disccorr=1 ; locate dislocations and calculate their distribution function

       ; this is the latest trick, works great
       Ccorr=1; spawn an external c-program (called Jcor) which does
         ; the correlation calculations fast. Best way so far. By far.



       Gcorr=1 ; spawn an external c-program (called Grobcor) which does
         ; the translational correlation calculations fast.
         ; Best way so far. By far.


       gofr=1; whether to do radial distributions g(r)
       do_bonds=0; whether to make the smoothed angle field

       do_force=1; whether to draw the force field.

       showdiscbefore=0; whether to display the
          ; disclinations before drawing dislocations
       do_angle_histogram = 'yes'  ;'yes' or 'no' for whether to output angle histogram file
       do_postscript_defects = 'yes' ;'yes' or 'no' for whether to output postscript file for disclinations, dislocations.
    ;********************************************************

       ; in order to redraw windows
       device,retain=1

       ; create gaussian weights used for smoothing the angular field
       ; howmuch (pixels out of 256) controls the smoothing of the angular correlation file

       howmuch=16
       weights=fltarr(2*howmuch+1,2*howmuch+1)
       norm=0
       for aa=-howmuch, howmuch do begin
       for bb=-howmuch, howmuch do begin
         weights[aa+howmuch, bb+howmuch]=exp(-(aa*aa+bb*bb)/(howmuch*howmuch))
         norm=norm+weights[aa+howmuch, bb+howmuch]
       endfor
       endfor
       weights=weights/norm
       W=fltarr(1000)
       W[*]=1




;     cd, "E:\matt\research\idl_trawick\simsph\w8.25_eta.4\smaller\trials1-9"
;     cd, "/home/angelscu/temp/081202/1.25"
;     cd, "/usr/temp/HardSpheres/fromthanos" ; make this your favorite data directory
       ; read files either as a list (.txt file with all names) or by selecting them by hand
       fs=dialog_pickfile(get_path=ps,/multiple_files)
       ;fs="perfect.tif"
       if ((n_elements(fs) gt 0) AND (strcmp(fs[0],'') eq 0)) then begin
       if ((strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif') and (strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'dat'))then readlist,fs(0),fs,path=ps else fs=[fs]
       ; this is where all the info is dumped, then written to the summary file
       Summary_of_Data=fltarr(10, n_elements(fs))
print,fs

    if (do_angle_histogram eq 'yes') then begin
       angle_histogram = lonarr(60,n_elements(fs))
       endif

; THE MASTER LOOP
timervar=-1
print,'number of files is',n_elements(fs)


time0=systime(1)
       for i=0,n_elements(fs)-1 do begin
       timervar=timervar+1
         close,/all
         print,'Now working on : ',fs(i)
        windowsclosed=0

;For Hardsphere only
         if (keyword_set(hardsphere)) then begin
          openr,33,fs(i)
          readf,33,nvertices
          data3=intarr(3,nvertices)
          data1=intarr(3,nvertices+1)
          readf,33,data3
          data1[*,1:*]=data3
          data3=0
          DEFSYSV, '!xss',479.0
          DEFSYSV, '!yss',959.0
;          DEFSYSV, '!xss',1.0*max(data1[0,*])
;          DEFSYSV, '!yss',1.0*max(data1[1,*])

         end else begin

          if(keyword_set(unfilt)) then begin
              unfilt1=strpos(fs[i],"fil")
              if(unfilt1 eq -1) then begin
                 print, "IF USED WITH /UNFILT, YOU MUST INPUT ONLY NAMES THAT CONTAIN FIL"
                 return
              end else begin
                 unfiltname=strmid(fs[i],0,unfilt1)+strmid(fs[i],unfilt1+3,strlen(fs[i]))
                 dataunfilt=read_tiff(unfiltname)
              end
              ;print, fs[i],"   ",unfiltname
              ;stop
          end


          ;count=0

          data=byte(bytscl(read_tiff(fs(i))));
          imagesize=size(data)
          if (keyword_set(flip)) then begin
            data=255-temporary(data)
          endif
          xs=imagesize[1]
          ys=imagesize[2]

         if (keyword_set(scale)) then begin
            ; scale to 1024X1024
            if (scale eq 1) then begin
        data=Interpolate(data,findgen((1024/(imagesize[1]*1.0))*imagesize[1])/(1024/(imagesize[1]*1.0)),findgen((1024/(imagesize[2]*1.0))*imagesize[2])/(1024/(imagesize[2]*1.0)),/grid)
                if (keyword_set(unfilt)) then begin
 dataunfilt=Interpolate(dataunfilt,findgen((1024/(imagesize[1]*1.0))*imagesize[1])/(1024/(imagesize[1]*1.0)),findgen((1024/(imagesize[2]*1.0))*imagesize[2])/(1024/(imagesize[2]*1.0)),/grid)
             endif
            endif else begin
            ; scale by scale(variable)
        data=Interpolate(data,findgen(scale*imagesize[1])/scale,findgen(scale*imagesize[2])/scale,/grid)
                if (keyword_set(unfilt)) then begin
 dataunfilt=Interpolate(dataunfilt,findgen(scale*imagesize[1])/scale,findgen(scale*imagesize[2])/scale,/grid)
                endif
            endelse
            ;reset the image size if scaled
            imagesize=size(data)
        endif



         if (keyword_set(sphere_diameter)) then begin
         endif else begin
             sphere_diameter =6
         endelse
            print,'using sphere size',sphere_diameter
          ; parameters for the sphere locator (obsolete)
          if (keyword_set(simu)) then begin
              sphere_diameter=8
          endif

          if (keyword_set(twomicron)) then begin
              sphere_diameter=6
          endif

          if (keyword_set(onemicron)) then begin
              sphere_diameter=7
          endif


          ; locate the centers of the spheres
          ; these are (C) John Crocker http://glinda.lrsm.upenn.edu/~weeks/idl/
          ;data2=bpass(data,1,sphere_diameter)


          ;this removes the need for the memory hogging data2 and implements the /filtered switch
          if (keyword_set(filtered)) then begin
           data1=feature(data,sphere_diameter)
          endif else begin
            data1=feature(bpass(data,1,sphere_diameter),sphere_diameter)
           endelse

          ns=size(data)   ;the same as imagesize?

          ; Various global variables, like x-size, y-size, various colors
          DEFSYSV, '!xss',ns[1]-1   ;is this needed?
          DEFSYSV, '!yss',ns[2]-1   ;is this needed?

         end ;ends else loop

ns=0



         ;R+256L*(G+256L*B)
; Why set this every time it runs?
         DEFSYSV, '!draw_recursively', 1
         DEFSYSV, '!recursion_level',0L
         DEFSYSV, '!max_recursion_level', 5000L
         DEFSYSV, '!colordisc7' ,  rgbcolor(0,255,0) ; green
         DEFSYSV, '!colordisc5' ,  rgbcolor(255,0,0); red
         DEFSYSV, '!colordisc4' , rgbcolor(255,0,255) ; magenta
         DEFSYSV, '!colordisc8' ,  rgbcolor(0,255,255) ; cyan
         DEFSYSV, '!colordisloc' , rgbcolor(255,255,0) ; yellow
         DEFSYSV, '!colorbond' , rgbcolor(0,0,255); blue
         DEFSYSV, '!colorwhite' , rgbcolor(255,255,255) ; white

         ; used to hold the cooordinates of the particles

       ; previously global, then passed, now in a common block to avoid passing.
         Common globalgood, goodx, goody      ;this is very slightly slower, but it IS cleaner than passing 392039238 times

         goodsize = size(data1)
         goodx=fltarr(goodsize[2]-1)   ;probably exist until end, previously global
         goody=fltarr(goodsize[2]-1)   ;probably exist until end, previously global
         goodsize=0

         print, "X,Y image size : ", !xss+1, !yss+1

         ;For Hardsphere only
         if(keyword_set(hardsphere)) then begin
          data=bytarr(!xss+1,!yss+1)
          hardsphererad=6
          for xhard=-hardsphererad,hardsphererad do begin
            for yhard=-hardsphererad,hardsphererad do begin
              data[(data1[0,*]+xhard)>(-1)<(!xss+1),(data1[1,*]+yhard)>(-1)<(!yss+1)] $
                =data[(data1[0,*]+xhard)>(-1)<(!xss+1),(data1[1,*]+yhard)>(-1)<(!yss+1)]+255.0*exp(-0.15*(xhard*xhard+yhard*yhard)/hardsphererad*hardsphererad)
            end
          end
          data=bytscl(data)
         end
          data=reverse(data,2)

         ;why is this extra variable needed?  Can't that be falsified later to save memory?
          img=[[[data]],[[data]],[[data]]]
          ; the showimage package is a really smart way of drawing a window

         ;if (imagesize[1]*imagesize[2] lt 4194304) then begin ;2048*2048  works... but the code needs to be changed
                                             ;           to handle this
          showimage,bytscl(img,min=0,max=255),3,wimage
          ;endif else begin
          ; draw_app_scroll2,bytscl(img,min=0,max=255),3,wimage
          ;endelse




          if ((do_force eq 0) OR (do_bonds eq 0)) then origimg=readimage(0)  ;only used in do_force

          if (keyword_set(unfilt)) then begin
              dataunfilt=reverse(dataunfilt,2)
              imgunfilt=[[[dataunfilt]],[[dataunfilt]],[[dataunfilt]]]
              ; the showimage package is a really smart way of drawing a window
              showimage,bytscl(imgunfilt,min=0,max=255),3,wimageunfilt
               origimgunfilt=readimage(0)
          end

       if (imagesize[1]*imagesize[2] gt 4194304) then begin ;4194304 is 2048x2048
         widget_control,wimage,/destroy
         windowsclosed=1
       endif

         img=bytarr(!xss+1,!yss+1,3)+255
         showimage,img,3,wbonds
        ;to save memory, set it to the minimum requirements
         img=0
         nvertices=n_elements(data1[0,1:*])
         print, nvertices

         goodx[0:nvertices-1]=data1[0,1:*]
         goody[0:nvertices-1]=data1[1,1:*]


;Matt was fiddling with these next few lines to try to avoid an apparent vertex at (0,0)
goodx_ok=data1[0,1:*]
goody_ok=data1[1,1:*]
       if (keyword_set(saveloc)) then begin
            openw,locationfile,strmid(fs[0],0,strlen(fs[0])-4)+'location_data.dat',/get_lun
            for LongIndex=0L, nvertices do begin
              printf,locationfile,data1[0,LongIndex],data1[1,LongIndex]
            endfor
            free_lun,locationfile
       endif
data1=0
         ; at the exit, edges will have a weird format, read idl help
         ; on "triangulate" - this is the main source of errors in code
;      triangulate, goodx, goody, triangles, outermost, CONNECTIVITY = edges
         triangulate, goodx_ok, goody_ok, triangles, outermost, CONNECTIVITY = edges
    triangles=0
    outermost=0
    goodx_ok=0
    goody_ok=0


         areavertex=1.0*!xss*!yss/nvertices
         bondlength=sqrt(areavertex*4/sqrt(3))
         print, "Average bond length a=",bondlength

         disc=fltarr(nvertices)
         inbounds=intarr(nvertices)
         inprocess=intarr(nvertices)
         ;ButtInOnAble=replicate(1,nvertices,2)   not used until later!
         ;ButtInOnAble=intarr(nvertices,2)
         ;ButtInOnAble=1 ;initially, all vertices can be butted in on.
         ;fcorr=fltarr(floor(sqrt(1.0*!xss*!xss+1.0*!yss*!yss)))  made later, as needed
         ;ncorr=fltarr(floor(sqrt(1.0*!xss*!xss+1.0*!yss*!yss)))  made later, as needed
       print, nvertices
         MAX_BOND_NUMBER=nvertices*3.25
         bondsx=fltarr(MAX_BOND_NUMBER)
         bondsy=fltarr(MAX_BOND_NUMBER)
         bondsangle=fltarr(MAX_BOND_NUMBER)
         ;bondsenergy=fltarr(MAX_BOND_NUMBER)
         bondsl=fltarr(MAX_BOND_NUMBER)
         bondcount=0L;

         discx=fltarr(MAX_BOND_NUMBER)
         discy=fltarr(MAX_BOND_NUMBER)
         discangle=fltarr(MAX_BOND_NUMBER)
         disccount=0L;

         inboundsmult=1.5
         nedges=n_elements(edges)


         inbounds[where((goodx gt inboundsmult*bondlength) and (goodx lt !xss-inboundsmult*bondlength) and (goody gt inboundsmult*bondlength) and (goody lt !yss-inboundsmult*bondlength))]=1;



         for i1=long(0),nvertices-1 do begin
          disc[i1]=edges[i1+1]-edges[i1] ; # neighbors
          for j1=edges[i1],edges[i1+1]-1 do begin
              ; collect angle data from the bond
              if (i1 lt edges[j1]) then begin
                 plots,[goodx[edges[j1]]/!xss,goodx[i1]/!xss],[1-goody[edges[j1]]/!yss,1-goody[i1]/!yss],/normal,color=!colorbond,thick=1.5
                 bondsx[bondcount]=(goodx[i1]+goodx[edges[j1]])/2
                 bondsy[bondcount]=(goody[i1]+goody[edges[j1]])/2
                 bondsl[bondcount]=sqrt((goodx[i1]-goodx[edges[j1]])^2.0+(goody[i1]-goody[edges[j1]])^2.0)
                 ang1=!pi+atan(goody[i1]-goody[edges[j1]],goodx[i1]-goodx[edges[j1]])
                 ang=ang1-(!pi/3.)*floor(ang1*3/!pi)
                 bondsangle[bondcount]=ang

                 bondcount=bondcount+1
              endif
          endfor
         endfor

         bondslength=total(bondsl)/bondcount
         print, "The Bondlength = ", bondslength

       ;This only allows bondsenergy to be created if it is going to be used.
         if (keyword_set(do_force) eq 0) then begin
            bondsenergy=fltarr(MAX_BOND_NUMBER)  ;unsure if this line is needed.  Is this
                                   ;just a remnant of c?
          bondsenergy=(bondsl-bondslength)^2.0   ;only is used later if do_force is 0
         endif

         unbounddisc=disc-6; unbound disclinationality phew !
         bondsl=0

         ;size of a disclination square on the screen
         discsize=1

         disc5=0 ; # inbounds 5s, total
         disc7=0 ; # inbounds 7s, total

       if (showdiscbefore eq 0) then begin
         for i1=long(0),nvertices-1 do begin


          if (disc[i1] lt 5) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc4
              endfor
          end

          if (disc[i1] eq 5) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc5
              endfor

          end



          if (disc[i1] gt 7) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc8
              endfor
          end

          if (disc[i1] eq 7) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc7
              endfor
          end

         endfor

       end


         ; runs Matt's code on finding dislocations
         ; at the return, bound will have the bonded nighbours, in the "edges" format
         ; read the idl help on "triangulate" for an explanation of the format
         ; bound[i] will be 1 if there is a bond or 0 otherwise
         bound=intarr(nedges)
         ButtInOnAble=replicate(1,nvertices,2)
         find_unbound_disc,nvertices,edges,unbounddisc, inbounds,inprocess, bound, ButtInOnAble
         inprocess=0
       ButtInOnAble=0

         disc5=0; total # 5s
         disc7=0; total # 7s
         unbound5=0
         unbound7=0



         ; now will draw the dislocations
         for i1=long(0),nvertices-1 do begin
          for j1=edges[i1],edges[i1+1]-1 do begin
              ; collect angle data from the bond
              if ((bound[j1]) and (i1 lt edges[j1])) then begin
                 plots,[goodx[i1]/!xss,goodx[edges[j1]]/!xss],[1-goody[i1]/!yss,1-goody[edges[j1]]/!yss],/normal,color=!colordisloc,thick=2
                 discx[disccount]=(goodx[i1]+goodx[edges[j1]])/2
                 discy[disccount]=(goody[i1]+goody[edges[j1]])/2
                 discmult=1.0
                 if (disc[i1] gt disc[edges[j1]]) then discmult=-1.0
                 ang1=!pi+atan(discmult*(goody[i1]-goody[edges[j1]]),discmult*(goodx[i1]-goodx[edges[j1]]))
                 discangle[disccount]=ang1
                 disccount=disccount+1
              endif
          endfor
         endfor

         for i1=long(0),nvertices-1 do begin
          ;the sizes for bound and unbound disclinations
          discsize=1
          if (unbounddisc[i1] ne 0) then discsize=2
          if ((unbounddisc[i1] eq -1) and (disc[i1] eq 5) and (inbounds[i1])) then begin
              unbound5=unbound5+1
          end
          if (disc[i1] lt 5) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc4
              endfor
          end

          if (disc[i1] eq 5) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc5
              endfor
              if ((goodx[i1] gt inboundsmult*bondlength) and (goodx[i1] lt !xss-inboundsmult*bondlength) and (goody[i1] lt !yss-inboundsmult*bondlength) and (goody[i1] gt inboundsmult*bondlength)) then begin
                 disc5=disc5+1

          end
          end


          if ((unbounddisc[i1] eq 1) and (disc[i1] eq 7) and (inbounds[i1])) then begin
              unbound7=unbound7+1
          end

          if (disc[i1] gt 7) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc8
              endfor
          end

          if (disc[i1] eq 7) then begin
              for discind=-discsize,discsize do begin
                 plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc7
              endfor
              if ((goodx[i1] gt inboundsmult*bondlength) and (goodx[i1] lt !xss-inboundsmult*bondlength) and (goody[i1] lt !yss-inboundsmult*bondlength) and (goody[i1] gt inboundsmult*bondlength)) then begin
                 disc7=disc7+1
          end
          end

         endfor


if (do_postscript_defects eq 'yes') then begin

       openw,postscript_defects_unit,strmid(fs[0],0,strlen(fs[0])-4)+'postscript_defects.ps',/get_lun
       printf,postscript_defects_unit,'%!PS'
       printf,postscript_defects_unit,'%%BoundingBox: ',0,0,imagesize[1],imagesize[2]
       printf,postscript_defects_unit,'1.00 1.00 setlinewidth'
       disc_circle_radius = 3
       unbound_disc_circle_radius = 5
       dislocation_linewidth = 3


       for i1=long(0),nvertices-1 do begin
         if (disc[i1] ne 6) then begin
           ;print out all of the disclinations
           if (disc[i1] lt 5) then printf,postscript_defects_unit,'1 0 1 setrgbcolor'
           if (disc[i1] eq 5) then printf,postscript_defects_unit,'1 0 0 setrgbcolor'
           if (disc[i1] eq 7) then printf,postscript_defects_unit,'0 1 0 setrgbcolor'
           if (disc[i1] gt 7) then printf,postscript_defects_unit,'0 1 1 setrgbcolor'
           printf,postscript_defects_unit,'newpath'
           printf,postscript_defects_unit,goodx[i1],imagesize[2]-goody[i1],disc_circle_radius,' 0 360 arc'
           printf,postscript_defects_unit,'closepath fill'
           if (unbounddisc[i1] ne 0) then begin
             ;now circle the unbound disclinations...
             if (unbounddisc[i1] eq -2) then printf,postscript_defects_unit,'1 0 1 setrgbcolor'
             if (unbounddisc[i1] eq -1) then printf,postscript_defects_unit,'1 0 0 setrgbcolor'
             if (unbounddisc[i1] eq 1) then printf,postscript_defects_unit,'0 1 0 setrgbcolor'
             if (unbounddisc[i1] eq 2) then printf,postscript_defects_unit,'0 1 1 setrgbcolor'
             printf,postscript_defects_unit,'newpath'
             printf,postscript_defects_unit,goodx[i1],imagesize[2]-goody[i1],unbound_disc_circle_radius,' 0 360 arc'
             printf,postscript_defects_unit,'closepath stroke'
             endif
           endif
         endfor ;i1
         ; now draw the dislocations
        printf,postscript_defects_unit,dislocation_linewidth,dislocation_linewidth,' setlinewidth'
        printf,postscript_defects_unit,'1 1 0 setrgbcolor'

        for i1=long(0),nvertices-1 do begin
          for j1=edges[i1],edges[i1+1]-1 do begin
              ; collect angle data from the bond
              if ((bound[j1]) and (i1 lt edges[j1])) then begin
                printf,postscript_defects_unit,'newpath'
                printf,postscript_defects_unit, goodx[i1],imagesize[2]-goody[i1], ' moveto'
                printf,postscript_defects_unit, goodx[edges[j1]],imagesize[2]-goody[edges[j1]], ' lineto'
                printf,postscript_defects_unit,'closepath stroke'
                endif
            endfor ;j1
          endfor ;i1
       free_lun,postscript_defects_unit
       endif ;do_postscript_defects

edges = 0
unbounddisc=0
disc=0 ;never used again
inbounds=0


         print, "Found total 5's and 7's : ", disc5, disc7
         print, "Found total unbound 5's and 7's : ", unbound5, unbound7




         Summary_of_Data[2,i]=unbound5
         Summary_of_Data[3,i]=unbound7

         ; Now we need to find the dislocations and calculate their correlations


         ;output the bond image in the TIFF format.
         saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'bonds.tif',/tiff
         ;will use this for the overlaying of the images
         if ((do_force eq 0) OR (do_bonds eq 0)) then bondsimg=readimage(0)


       if (imagesize[1]*imagesize[2] gt 4194304) then begin ;4194304 is 2048x2048
            widget_control,wbonds,/destroy
        endif
       ; good trick :  total(bound) = 2*#dislocations
         Summary_of_Data[4,i]=total(bound)/2

bound=0 ;never used again


       if (disccorr eq 0) then begin

         print, "Found ", disccount, " disclocations"

         ; Will spawn an external C program which does the robcor procedure very fast...
         ; First, need to write out the bonds angle file...
         openw,u,'bonds.dat',/get_lun
         sampling=1; The decimation rate in getting the bonds

         printf,u,sampling

         printf,u,disccount

         for iii=0L, disccount-1 do begin
          printf,u,discx[iii],discy[iii],discangle[iii]
         endfor

         ;discx=0  Moved down below due to if loop *********************
         ;discy=0
         ;discangle=0

         free_lun,u

         spawn, './robcor2pi'

         robcor=read_ascii('robcor.dat');
         dcorr=transpose(robcor.field1[0,*])
         ncorr=transpose(robcor.field1[2,*])
         fcorr=transpose(robcor.field1[1,*])

         dimplot=100
         dimwin=500

         CorrelationLenghTest=abs(fcorr(0:dimplot)-exp(-1.0))
         CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
         print,'The direct correlation length seed was ', Min_Subscript, ' .'

         A=[.5,Min_Subscript]



         window,1,xsize=dimwin+1,ysize=dimwin
         plot, dcorr(0:dimplot), fcorr(0:dimplot), xtitle='pixels', ytitle='g6(r)',psym=0
         maxdisc=where((fcorr gt shift(fcorr,1)) and (fcorr gt shift(fcorr,-1)))

         dcorr=temporary(dcorr(maxdisc))
         fcorr=temporary(fcorr(maxdisc))
         ncorr=temporary(ncorr(maxdisc))

         ;dcorr=dcorr(maxdisc)
         ;fcorr=fcorr(maxdisc)
         ;ncorr=ncorr(maxdisc)

         dimfit=n_elements(maxdisc)-1
        maxdisc=0

         minfit=0;
         iter=0
         chisq=0
         yfit = curvefit(dcorr(minfit:dimfit), fcorr(minfit:dimfit), ncorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
         X=indgen(dimfit)
         ;plot out the determined function in a different color
         ;F = (EXP(-X/A[0]))
         F = A[0]*(EXP(-X/A[1]))
         oplot,F,psym=0, color=254
         ;blue equals 256*127
         x=0


         print,'The dislocation dislocation correlation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
         print,'It required ', iter , ' iterations, chisq=',chisq

         Summary_of_Data[0,i]=A[1]



       end
         discx=0
         discy=0
         discangle=0



;gofr g(r)
       if (gofr eq 0) then begin
         grsize=300 ; Size for the g(r) plot
         grscan=10; grscan^2 is the number of averaged vertices
         grplaces=fltarr(grscan^2); gives a bond in each of those squares
         grsizereal=grsize+30;


         grside=1.3*bondlength ;side of a square in which we are sure to capture at least one vertex

         for gri=0,grscan-1 do begin
         for grj=0,grscan-1 do begin
          grindex=gri*grscan+grj
          grxtemp=grsizereal+(!xss-2*grsizereal)*gri/grscan
          grytemp=grsizereal+(!yss-2*grsizereal)*grj/grscan
          grtemp=where((grxtemp gt goodx) and (grytemp gt goody) and (grxtemp lt goodx+grside) and (grytemp lt goody+grside))
          grplaces[grindex]=grtemp[1]
          print, "The x,y of the ", grindex ,"-th square are : ", grxtemp,grytemp
          print, "The coords of the vertex in square ", grindex, " are ", goodx[grplaces[grindex]],goody[grplaces[grindex]]
         endfor
         endfor
         grtemp=0

         gr=fltarr(grsize+1)
         gind=findgen(grsize+1)
         gind=!pi*((gind+1)^2-gind^2)
         nscan=0

         for i2=0,grscan^2-1 do begin

          i1=grplaces[i2]
          print, "Working on vertex ", i1, " x=",goodx[i1], " y=", goody[i1], "in square ", i2
          for ivar=0,grsize do begin
              closevert=(where((sqrt((goodx-goodx[i1])^2.0+(goody-goody[i1])^2.0) le ivar+1) and (sqrt((goodx-goodx[i1])^2.0+(goody-goody[i1])^2.0) gt ivar)))
              if(closevert[0] ne -1) then begin
                 nvar=size(closevert)
              end else begin
                 nvar=[0,0]
              end
              gr[ivar]=gr[ivar]+nvar[1]
          endfor

          nscan=nscan+1
         endfor
         grplaces=0
        closevert=0
        nvar=0

         window,10

         fgr=areavertex*gr/(gind*nscan)
         plot,(indgen(300))/bondlength,fgr-1
         gind=0



         fgrplus=shift(fgr,1)
         fgrminus=shift(fgr,-1)
         fgr=fgr[1:*]
         fgrplus=fgrplus[1:*]
         fgrminus=fgrminus[1:*]
         wheremaxfgr=where((fgr gt fgrplus) and (fgr gt fgrminus))+1
         fgrminus=0
         fgrplus=0

         openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'gofrpeaks.dat',/get_lun

         for LongIndex=0L,n_elements(wheremaxfgr)-1 do begin
          printf,u,wheremaxfgr[LongIndex]/bondlength,fgr[wheremaxfgr[LongIndex]-1]-1
         endfor
         free_lun,u
         close,u
         wheremaxfgr=0




         saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'gofr.tif', /tiff

    end
;gofr   g(r)





       if (do_bonds eq 0) then begin

         bondsx=bondsx(0:bondcount-1)
         bondsy=bondsy(0:bondcount-1)
         bondsangle=bondsangle(0:bondcount-1)
         print,'Found ', bondcount,' Delauney bonds'


         triangulate, bondsx, bondsy, btriangles, boutermost, CONNECTIVITY = bedges   ;bedges, btriangles gets created here!
         bcutoff=3*howmuch
         ;get an interpolated image of the bonds angles
         bcosangle=trigrid(bondsx,bondsy,cos(6.0*bondsangle),btriangles, NX=(!xss+1),NY=(!yss+1))
         bsinangle=trigrid(bondsx,bondsy,sin(6.0*bondsangle),btriangles, NX=(!xss+1),NY=(!yss+1))

         ; Do gaussian smoothing on the bond angle file
         smoothbcosangle=fltarr(!xss+1,!yss+1)
         smoothbsinangle=fltarr(!xss+1,!yss+1)
print,'starting doing the smoothing...'
t0=systime(1)
;      smooth,bcosangle, smoothbcosangle, weights, howmuch
;      smooth,bsinangle, smoothbsinangle, weights, howmuch
         fftsmooth,bcosangle, smoothbcosangle, weights, long(bondlength);howmuch
         fftsmooth,bsinangle, smoothbsinangle, weights, long(bondlength);howmuch
print,'done doing the smoothing...','elapsed time = ',systime(1)-t0
bcosangle=0
bsinangle=0

         smoothbangle=!pi+float(atan(smoothbsinangle, smoothbcosangle))
         smoothbangle=smoothbangle/6   ; mark temporary
;      smoothbangle=smoothbangle-(!pi/3)*floor(smoothbangle*3/!pi)
smoothbsinangle=0
smoothbcosangle=0


         print, 'Displaying smoothed bond angle...'
         x=findgen(!xss+1)#replicate(1.,!yss+1)
         y=replicate(1.,!xss+1)#findgen(!yss+1)
;      smoothbangle1=interpolate(smoothbangle,x/4.0,y/4.0,/cubic)

         image1=colortable(reverse(smoothbangle,2), fs[i],9)
         showimage,image1,3,wbangle

         saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'angle.tif',/tiff

         if (do_angle_histogram eq 'yes') then begin
             single_angle_histogram = histogram(smoothbangle,min =0,binsize = !PI/(3 * 60), nbins = 61)
          angle_histogram[*,i] = single_angle_histogram[0:59]
          angle_histogram[0,i] = temporary(angle_histogram[0,i]) + single_angle_histogram[60]
          endif
         angimg=readimage(0)
         single_angle_histogram=0
       if (imagesize[1]*imagesize[2] gt 4194304) then begin ;4194304 is 2048x2048
         widget_control,wbangle,/destroy
         windowsclosed=1
       endif

;      Now we have origimg, bondsimg, angimg to work with



         newimg=origimg*.5+angimg*.5

         if(keyword_set(unfilt)) then begin
          newimg=(origimg*.5+origimgunfilt*.5)*.7+angimg*.3
         end
         ;try,newimg
         angimg=0



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
         showimage,[[[newred]],[[newgreen]],[[newblue]]],3,wcombined


         saveimage,strmid(fs[i],0,strlen(fs[i])-4)+'all.tif', /TIFF
;      nang=size(angimg)
;      angimg1=fltarr(nang[2],nang[3],3)
;      angimg1[*,*,0]=angimg[0,*,*]
;      angimg1[*,*,1]=angimg[1,*,*]
;      angimg1[*,*,2]=angimg[2,*,*]
;      window,9
;      tv, angimg1,true=3

;      print,'Writing angle TIFF file'
         write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'smooth.tif',bytscl(smoothbangle,MAX= !pi/3,MIN=0)


       bedges=0 ; bedges gets created in do_force and do_bonds conditionals only!
       btriangles=0 ; btriangles gets created in do_force and do_bonds conditionals only!
       end
       imagesize=0
       if ((brutecorr eq 1) and (Ccorr eq 1)) then bondsangle=0 ;isn't used later if so.




       if (do_force eq 0) then begin

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

         if(keyword_set(unfilt)) then begin
          newimg=(origimg*.5+origimgunfilt*.5)*.7+energimg*.3
         end
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
       end
       image1=0
       btriangles=0
       newimg=0
       bondsimg=0
       newred=0
       newgreen=0
       newblue=0
       newbred=0
       newbgreen=0
       newbblue=0
       whatbondstokeep=0
       boutermost=0
       origimg=0
       weights=0



; ********   Calculating the bond-bond correlation function


         dimplot1=800


         ;fcorr=fltarr(floor(sqrt(!xss*!xss+!yss*!yss)))  can be moved into the if, as ccorr reassigns
         ;ncorr=fltarr(floor(sqrt(!xss*!xss+!yss*!yss)))  can be moved into the if, as ccorr reassigns




;brutecorr
       if (Brutecorr eq 0) then begin
         ;Trying to do it directly...

         fcorr=fltarr(floor(sqrt(1.0*!xss*!xss+1.0*!yss*!yss)))
         ncorr=fltarr(floor(sqrt(1.0*!xss*!xss+1.0*!yss*!yss)))

         multiplier=8
         count=bondcount/multiplier

         for iiii=1L,count-1 do begin
          iii=iiii*multiplier
          bondsd=floor(sqrt((bondsx(0:iii)-bondsx[iii])^2+(bondsy(0:iii)-bondsy[iii])^2))
          bondscorr=cos(6.0*(bondsangle(0:iii)-bondsangle[iii]))

          if ((iii mod 1000) eq 0) then begin
              print, 'Working on bond #',iii
              if (1 eq 0) then begin
                 index=where(ncorr)
                 restrict=where(index lt dimplot1)
                 index=index[restrict]
                 window,1,xsize=dimplot1+1,ysize=600
                 plot, index, fcorr[index]/ncorr[index], xtitle='pixels', ytitle='g6(r)',psym=0
              endif
          endif



          for jjjj=0L,iiii do begin
              jjj=jjjj*multiplier
              fcorr[bondsd[jjj]]=fcorr[bondsd[jjj]]+bondscorr[jjj]
              ncorr[bondsd[jjj]]=ncorr[bondsd[jjj]]+1
          endfor
         endfor
       endif
       bondsd=0
       bondscorr=0
;brutecorr



;ccorr
       ;********************
    if (Ccorr eq 0) then begin

       ; Will spawn an external C program which does the robcor procedure very fast...
       ; First, need to write out the bonds angle file...
       openw,u,'bonds.dat',/get_lun
       sampling=2; The decimation rate in getting the bonds

       printf,u,sampling

       printf,u,bondcount

       for iii=0L, bondcount-1 do begin
         printf,u,bondsx[iii],bondsy[iii],bondsangle[iii]
       endfor

       bondsangle=0
       bondsx=0
       bondsy=0

       free_lun,u

;     spawn, './robcor'

       spawn, 'robcor',/hide

       robcor=read_ascii('robcor.dat');
       dcorr=transpose(robcor.field1[0,*])
       ncorr=transpose(robcor.field1[2,*])
       fcorr=transpose(robcor.field1[1,*])

       print, ncorr[1:200]
       dimplot=600    ; current work area!  previous code was dimplot=300


       window,1,xsize=dimplot+1,ysize=dimplot
       plot, dcorr(0:dimplot), fcorr(0:dimplot), xtitle='pixels', ytitle='g6(r)',psym=0

       CorrelationLenghTest=abs(fcorr(0:dimplot)-exp(-1.0))
       CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
       print,'The direct correlation length seed was ', Min_Subscript, ' .'

       A=[.5,Min_Subscript]


       dimfit=400;
       minfit=20;
       iter=0
       chisq=0
       yfit = curvefit(dcorr(minfit:dimfit), fcorr(minfit:dimfit), ncorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
       X=indgen(dimfit)
       ;plot out the determined function in a diffference color
       ;F = (EXP(-X/A[0]))
       F = A[0]*(EXP(-X/A[1]))
       oplot,F,psym=0, color=254
       ;blue equals 256*127
       print, ncorr[minfit:dimfit]
       print, 'and'
       print, ncorr(minfit:dimfit)


       print,'The C correlation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
       ;print,'The imagetranslate correlation length was ', A[0],' and its coefficient was', ' N/A'
       print,'It required ', iter , ' iterations, chisq=',chisq

       print, 'Writing robcor file...'
       openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'robcor.dat',/get_lun

       for LongIndex=0L,n_elements(ncorr)-1 do begin
         printf,u,dcorr[LongIndex],fcorr[LongIndex], ncorr[LongIndex]
       endfor
       free_lun,u
       close,u

       Summary_of_Data[1,i]=A[1]
    endif
       bondsx=0
       bondsy=0
       dcorr=0
       fcorr=0
       ncorr=0
;ccorr



;Gcorr
       ;********************
    if (Gcorr eq 0) then begin




       Gn=size(data) & Gn=Gn(1:Gn(0))
       Gf=abs(shift(fft(data),Gn(0)/2,Gn(1)/2))
       Gf(Gn(0)/2, Gn(1)/2)=0
       bGf=Gf[Gn[0]/2-70:Gn[0]/2+70,Gn[1]/2-70:Gn[1]/2+70]
    data=0
       showimage,bytscl(bGf),3,wGfft

;HEY!  THIS ISN'T USED
       tmp=max(Gf,Gw)

       Gx=(Gw mod Gn(0))-Gn(0)/2
       Gy=Gw/Gn(0)-Gn(1)/2
       G=fltarr(6,2)
       bGf=0
       tmp

       for Gk=0,5 do begin
         Gtheta=Gk*!pi/3.
         Grot=[[cos(Gtheta),sin(Gtheta)],[-sin(Gtheta),cos(Gtheta)]]
         GR=[Gx,Gy]#Grot
         G[Gk,0]=GR[0]
         G[Gk,1]=GR[1]
         Gff=Gf[Gn[0]/2+GR[0]-20:Gn[0]/2+GR[0]+20, Gn[1]/2+GR[1]-20:Gn[1]/2+GR[1]+20]
         Gmax=max(Gff,Gw)
         G[Gk,0]=((Gw mod 41)+GR[0]-20)
         G[Gk,1]=(Gw/41+GR[1]-20)
         print,"Max value =", Gff[(Gw mod 41),Gw/41]
         print, "Neighbouring values = "
         for Gj=-5,5 do begin
         for Gi=-5,5 do begin
          if(Gf[(Gw mod 41)+Gi+Gn[0]/2+GR[0]-20,Gw/41+Gn[1]/2+Gj+GR[1]-20] gt Gf[Gn[0]/2+(Gw mod 41)+GR[0]-20,Gn[1]/2+Gw/41+GR[1]-20]) then print, "Alarm,neighboring value higher ! "
         end
         end
         gr=0
         gf=0
         Grot=0

         ;print,"The radius of the Fourier radius for peak ",Gk," is ", sqrt(G[Gk,0]^2+G[Gk,1]^2)

               plots,G[Gk,0]+70, G[Gk,1]+70,color=1000L,/device,psym=3,thick=1,symsize=2


       end


       Gfactors=1+(indgen(100)-50.)/250.0
       Gdifs=fltarr(100)

       for Gkk=0,99 do begin
         Gdifs[Gkk]=Gscale(G,Gfactors[Gkk])
       end
       Gmin=min(Gdifs,Gbestfactor)
       Gfact=Gfactors[Gbestfactor]
       print, "Gfact=",Gfact
       ;stop
       Gfactors=0
       Gdifs=0

       ;Gfact=1

       Gxreal=Gx*2.0*!pi/Gn(0)
       Gyreal=Gy*2.0*!pi*Gfact/Gn(1)
       print, "Greal =  ", Gxreal, Gyreal
       Gn=0
       Gx=0
       Gy=0


       ;showimage,data,1,datanew
            ;plots,[500,500+300./Gxreal], [500,500-300./Gyreal],color=!colorbond, thick=2,/device


       ;stop

       openw,u,'vertices.dat',/get_lun
       sampling=5; The decimation rate in getting the bonds

       printf,u,sampling
       printf,u,nvertices
       printf,u,1*Gxreal,-1*Gyreal

       for iii=0L, nvertices-1 do begin
         printf,u,goodx[iii],goody[iii]/Gfact
       endfor
       free_lun,u

       spawn, './Grobcor'

       Grobcor=read_ascii('Grobcor.dat');
       dGcorr=transpose(Grobcor.field1[0,*])
       nGcorr=transpose(Grobcor.field1[2,*])
       fGcorr=transpose(Grobcor.field1[1,*])

       dimplot=300


       window,1,xsize=dimplot+1,ysize=dimplot
       plot, dGcorr(0:dimplot), fGcorr(0:dimplot), xtitle='pixels', ytitle='gG(r)',psym=0
       oplot, dGcorr(0:dimplot), bytscl(nGcorr(0:dimplot))/255.,psym=0, color=rgbcolor(0,255,0)
       GCorrelationLenghTest=abs(fGcorr(0:dimplot)-exp(-1.0))
       GCorrelationLengthSeed=min(GCorrelationLenghTest,GMin_Subscript)
       print,'The direct correlation length seed was ', GMin_Subscript, ' .'


       A=[1,GMin_Subscript]

       ;A=[GMin_Subscript]


       dimfit=200;
       minfit=20;
       iter=0
       chisq=0
       yfit = curvefit(dGcorr(minfit:dimfit), fGcorr(minfit:dimfit), nGcorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
       ;yfit = curvefit(dGcorr(minfit:dimfit), fGcorr(minfit:dimfit), nGcorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct1', ITMAX=100, ITER=iter, chisq=chisq)
       X=indgen(dimfit)
       ;plot out the determined function in a diffference color
       ;F = (EXP(-X/A[0]))
       F = A[0]*(EXP(-X/A[1]))
       oplot,F,psym=0, color=254

       ;blue equals 256*127


       print,'The C  Gcorrelation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
       ;print,'The imagetranslate correlation length was ', A[0],' and its coefficient was', ' N/A'
       ;print,'The C  Gcorrelation length was ', A[0],'+/-',Sigma_a[0]
       print,'It required ', iter , ' iterations, chisq=',chisq

       print, 'Writing robcor file...'
       openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'Grobcor.dat',/get_lun

       for LongIndex=0L,n_elements(nGcorr)-1 do begin
         printf,u,dGcorr[LongIndex],fGcorr[LongIndex], nGcorr[LongIndex]
       endfor
       free_lun,u
       close,u

       Summary_of_Data[5,i]=A[1]
       ;Summary_of_Data[5,i]=A[0]


    endif
    data=0
;Gcorr



;translatecorr
       ;********************     Trying to redo the autocorrelation by the image-displacement technique
       if(translatecorr eq 0) then begin
         f1corr=fltarr(500)
         n1corr=fltarr(500)

         maxcorr=40
         for iii=-maxcorr, maxcorr do begin
         print,iii+maxcorr, ' out of ', 2*maxcorr, ' steps performed'
         for jjj=-maxcorr, maxcorr do begin
          d=floor(sqrt(iii*iii+jjj*jjj))
          banglemoved=shift(bangle,iii,jjj)

          xstart=max([iii,0])
          xend=min([255+iii,255])

          ystart=max([jjj,0])
          yend=min([255+jjj,255])

          xyarea=1L*(yend-ystart+1)*(xend-xstart+1)

          coscorr=cos(6*(bangle(xstart:xend,ystart:yend)-banglemoved(xstart:xend,ystart:yend)))
          f1corr[d]=f1corr[d]+total(coscorr)
          n1corr[d]=n1corr[d]+xyarea


         endfor
         endfor
         banglemoved=0


         dimplot=300

         index=where(n1corr)
         ;restrict=where(index lt dimplot)
         index=index[where(index lt dimplot)]
         window,2,xsize=dimplot+1,ysize=dimplot
         plot, index, f1corr[index]/n1corr[index], xtitle='pixels', ytitle='g6(r)',psym=0



         CorrelationLenghTest=abs(f1corr[index]/n1corr[index]-exp(-1.0))
         CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
         print,'The direct correlation length seed was ', Min_Subscript, ' .'

         A=[.5,Min_Subscript]

         dimfit=floor(maxcorr*sqrt(2))
         ;restrict2=where(index lt dimfit)
         index=index[where(index lt dimfit)]
         ;restrict2=where(index gt 5)
         index=index[where(index lt dimfit)]
         iter=0
         chisq=0
         yfit = curvefit(index, f1corr[index]/n1corr[index], n1corr[index], A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)

         X=indgen(dimfit)
         ;plot out the determined function in a diffference color
         ;F = (EXP(-X/A[0]))
         F = A[0]*(EXP(-X/A[1]))
         oplot,F,psym=0, color=254
         ;blue equals 256*127


         print,'The image translate correlation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
         ;print,'The imagetranslate correlation length was ', A[0],' and its coefficient was', ' N/A'
         print,'It required ', iter , ' iterations, chisq=',chisq

         print, 'Writing robcor file...'
         openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'robcor.dat',/get_lun

          for LongIndex=0L,dimfit-1 do begin
          printf,u,LongIndex,f1corr[LongIndex]/n1corr[Longindex], n1corr[LongIndex]
         endfor
         n1corr=0
         f1corr=0
         free_lun,u
         close,u


         Summary_of_Data[1,i]=A[1]


       endif
;translatecorr




; From here on the code is modified from Chris' Stripes Code


;ffcorr

         !xss=255;
         !yss=255;


       if (ffcorr eq 0) then begin
         ;create the exp^6*imaginary*theta array
         OrderParameter=complex(cos(6.0*smoothbangle),sin(6.0*smoothbangle))

         ;The autocorrelation I return is already shifted
         DoAutoCorrelation,OrderParameter,RepeatSpacing,AutoCorrelation,IntensityArray,IntensityCountArray


         ;next we perform a curvefit, fit the function to y=a[0]*exp(-x/a[1])
         ;we set five parameters: the functions X array, Y array
         ;the weights W, and seed parameters a[0] (coefficient) and a[1] (correlation length)


         CorrelationLenghTest=abs(IntensityArray(0:200)-exp(-1.0))
         CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
         Nfit=5*Min_Subscript < 200
         NFit=NFit > 10
         start=0
         Y=IntensityArray(start:Nfit)
         X=findgen(Nfit-start+1)+start




         print,'The correlation length seed was ', Min_Subscript, ' .'
         ;A=[1,Min_Subscript]
         A=[Min_Subscript]
         ;OK, now call it
         yfit = CURVEFIT(X, Y, W(start:Nfit), A, SIGMA_A, FUNCTION_NAME = 'funct1')
         window,0, xsize=!xss+1, ysize=!yss+1
         plot, IntensityArray(0:!xss), xtitle='pixels', ytitle='g2(r)',psym=0
         ;plot out the determined function in a different color
         F = (EXP(-X/A[0]))
         ;F = A[0] * (EXP(-X/A[1]))
         oplot, F,psym=0, color=254
         ;blue equals 256*127
         print,'The correlation length was ', A[0],' and its coefficient was ', 'N/A','.'
         ;print,'The correlation length was ', A[1],' and its coefficient was ', A[0],'.'
         ;record the data in the array for summary
         ;Summary_of_Data[0,i]=A[1]
         Summary_of_Data[1,i]=A[0]
         ;y=0 down below *******************************************
         ;w=0


         ;tv,bytscl(-255 * alog10( abs(AutoCorrelation))/Max(-alog10(abs(AutoCorrelation))))



         ;write the correlation function out

         if (1 eq 0) then begin
         openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'robcor.dat',/get_lun
         n=size(IntensityArray)
         for LongIndex=0L,n(2)-1 do begin
          printf,u,LongIndex,IntensityArray[LongIndex], IntensityCountArray[LongIndex]
         endfor
         free_lun,u
         close,u
       end
         ;window,5,xsize=2*!xss,ysize=2*!yss
         ;Slide_Image, bytscl(AutoCorrelation),SLIDE_WINDOW=MISCHA,group=g,FULL_WINDOW=8
         ;the below line destroys the Slide_image
         ;widget_control, g,/destroy
       endif
       g=0
       y=0
       w=0
;ffcorr


; From here on the code is taken from Anglecorr.pro, and the point is to do the direct
; correlation on the image itself rather than on the fourier transform


;direct
    if (directcorr eq 0) then begin
       pi=3.141592
       dimplot=256
       sampling=8 ; the size of the sampling square for direct correlations
       dimfit=64 ; the size of the picture over which correlation function is fitted
       fitcutoff=4

       n=size(smoothbangle);
       dim1=n(1)
       dim2=n(2)
       n=0
       dim=ceil(dim1*sqrt(2))
       corr=fltarr(dim)
       pairs=fltarr(dim)
       ;help, corr

       samplingdim1=dim1/sampling
       samplingdim2=dim2/sampling
       yvar1=0
       yvar2=0

       for j=1L,samplingdim1*samplingdim2-1 do begin
         xvar1=sampling*(j mod samplingdim1)
         yvar2=yvar1
         yvar1=sampling*(j/samplingdim1)
         ;if yvar2 ne yvar1 then begin
         ;    print, 'Line # = ',yvar1
         ;endif
         for k=0L,j do begin

          xvar2=sampling*(k mod samplingdim1)
          yvar2=sampling*(k/samplingdim1)
          d=round(sqrt((xvar1-xvar2)*(xvar1-xvar2)+(yvar1-yvar2)*(yvar1-yvar2)))
          ;print, 'd=',d
          corr[d]=temporary(corr[d])+cos(6*(smoothbangle[xvar1,yvar1]-smoothbangle[xvar2,yvar2]))
          pairs[d]=temporary(pairs[d])+1;
         endfor
       endfor
       smoothbangle=0

       for j=0L,dim-1 do begin
         if pairs[j] ne 0 then begin
          corr[j]=temporary(corr[j])/pairs[j]
         endif
       endfor

       CorrelationLenghTest=abs(corr-exp(-1.0))
       CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
       print,'The direct correlation length seed was ', Min_Subscript, ' .'
CorrelationLenghTest=0
CorrelationLengthSeed=0

       A=[1,Min_Subscript]
       ;A=[Min_Subscript]



       ;window,0,xsize=dim1/2+50,ysize=dim2/2+50
       ;tvscl,angle,25,25

       index=where(pairs)
       ;restrict=where(index lt dimplot)
       index=index[where(index lt dimplot)]


       window,1,xsize=dimplot+1,ysize=dimplot+1
       plot, index, corr[index], xtitle='pixels', ytitle='g6(r)',psym=0

       ;restrict2=where((index lt dimfit) and (index gt fitcutoff))
       index=index[where((index lt dimfit) and (index gt fitcutoff))]
       yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct')

       yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
       index=0
       corr=0
       pairs=0


       X=indgen(dimplot)
       ;plot out the determined function in a diffference color
       ;F = (EXP(-X/A[0]))
       F = A[0]*(EXP(-X/A[1]))
       oplot,F,psym=0, color=254
       ;blue equals 256*127

       print,'The direct correlation length was ', A[1],' and its coefficient was', A[0],' iter=',iter,' chisq=',chisq
       ;print,'The direct correlation length was ', A[0],' and its coefficient was', ' N/A'
       Summary_of_Data[2,i]=A[1]

    endif
    x=0
    smoothbangle=0
;direct

         print, 'All done with ',fs[i],'.'

         if (keyword_set(wait)) then begin
          ; to give you time to examine the images
          wait,5
         endif


         if (keyword_set(stay) eq 0) then begin
          ; destroy all the windows created
          if (do_force eq 0) then begin
              widget_control,wbenergy,/destroy
              widget_control,wcombinedenerg,/destroy
          ;    widget_control,wbonds,/destroy
         ;     widget_control,wimage,/destroy



          end else begin
          if (do_bonds eq 0) then begin
          ;    widget_control,wbangle,/destroy
              widget_control,wcombined,/destroy
          ;    widget_control,wbonds,/destroy
          ;    widget_control,wimage,/destroy
          end else begin
          if (Gcorr eq 0) then begin
          ;    widget_control,wbonds,/destroy
              widget_control,wimage,/destroy
          ;    widget_control,wGfft,/destroy
          end else begin
          ;    widget_control,wbonds,/destroy
          ;    widget_control,wimage,/destroy
          if (windowsclosed eq 1) then begin ;4194304 is 2048x2048
            widget_control,wbenergy,/destroy
            widget_control,wcombinedenerg,/destroy
       endif
          end
          end
          end
          if (keyword_set(unfilt)) then begin
              widget_control,wimageunfilt,/destroy
          end
          if ((Ccorr eq 0) or (disccorr eq 0)) then wdelete,1

           if (windowsclosed eq 0) then begin
            widget_control,wbonds,/destroy
            widget_control,wimage,/destroy
              widget_control,wbangle,/destroy
          endif

         endif




       endfor ; main loop that reads each input file
; MASTER LOOP ENDS

print, 'DONE WITH BIG LOOP'
if (do_angle_histogram eq 'yes') then begin
       openw,angle_histogram_unit,strmid(fs[0],0,strlen(fs[0])-4)+'angle_histogram.dat',/get_lun
       printf,format='(60I)',angle_histogram_unit,angle_histogram
       free_lun,angle_histogram_unit
       endif
       angle_histogram=0




       ; output all data into the summary file
       openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'summary.dat',/get_lun
       for LongIndex=0L, n_elements(fs)-1 do begin
         printf,u,Summary_of_Data[0,LongIndex],Summary_of_Data[1,LongIndex],Summary_of_Data[2,LongIndex],Summary_of_Data[3,LongIndex],Summary_of_Data[4,LongIndex],Summary_of_Data[5,LongIndex]," ",fs[LongIndex]
       endfor
       free_lun,u

print,'total time:',systime(1)-time0,'elapsed seconds'

endif else begin
print, 'No opened images.'
endelse


end








; **************************************************************************************
;
;
;  Begin Functions
;
;
; **************************************************************************************






pro try, newimg

         newred=reform(newimg[0,*,*])
         newgreen=reform(newimg[1,*,*])
         newblue=reform(newimg[2,*,*])
         showimage,[[[newred]],[[newgreen]],[[newblue]]],3,wcombined

end


function readimage, dummy
    current_window = !d.window
    xsize = !d.x_size
    ysize = !d.y_size
    ;window, /pixmap,/free, xsize=xsize, ysize=ysize, retain=1
    ;device, copy=[0, 0, xsize, ysize, 0, 0, current_window]
    image = tvrd(0,0,xsize,ysize,order=0,true=1)
    ;wdelete, !d.window
    wset, current_window
    return,image
end


pro fftsmooth, input, output, weights, howmuch

print, 'from fftsmooth: howmuch = ',howmuch

    x=(size(input))[1]
    y=(size(input))[2]
    bigweights=input*0
    norm=0
    for aa=0, howmuch do begin
    for bb=0, howmuch do begin
       bigweights[aa,bb]=exp(-(aa*aa+bb*bb)/(howmuch*howmuch))
       bigweights[x-aa-1,bb]=bigweights[aa,bb]
       bigweights[x-aa-1,y-bb-1]=bigweights[aa,bb]
       bigweights[aa,y-bb-1]=bigweights[aa,bb]
       norm=norm+4*bigweights[aa,bb]
       endfor
       endfor
    bigweights=bigweights/norm
    output=fft(fft(input)*fft(bigweights),/inverse)
end

pro smooth, input, output, weights, howmuch

    for aa=-howmuch, howmuch do begin
    for bb=-howmuch, howmuch do begin
       output=output+weights[aa+howmuch, bb+howmuch]*shift(input,aa,bb)
    endfor
    endfor
;for greater speed, but not quite as fast as fftsmooth, consider:
;output=convol(input,weights)
end



;********************************************************
;USED FOR FITTING THE EXPONENTIAL DECAY
;********************************************************
PRO funct, X, A, F, PDER

      F = A[0] * (EXP(-X/A[1]))
    ;If the function is called with four parameters,
    ;calculate the partial derivatives:


IF N_PARAMS() GE 4 THEN BEGIN
    ;PDER's column dimension is equal to the number of elements
    ;in xi and its row dimension is equal to the number of
    ;parameters in the function F:
        pder = FLTARR(N_ELEMENTS(X), 2)
    ;Compute the partial derivatives with respect to a0 and
    ;place in the first row of PDER.
       pder[*, 0] =  EXP(-X/A[1])
    ;Compute the partial derivatives with respect to a1 and
    ;place in the second row of PDER.

       pder[*, 1] = (A[0]*X/(A[1]*A[1]))*EXP(-X/A[1])
ENDIF
END



PRO funct1, X, A, F, PDER

F =  (EXP(-X/A[0]))
;If the function is called with four parameters,
;calculate the partial derivatives:


  IF N_PARAMS() GE 4 THEN BEGIN
;PDER's column dimension is equal to the number of elements
;in xi and its row dimension is equal to the number of
;parameters in the function F:
    pder = FLTARR(N_ELEMENTS(X), 1)
;Compute the partial derivatives with respect to a0 and
;place in the first row of PDER.
    pder[*, 0] = (X/(A[0]*A[0]))* EXP(-X/A[0])
;Compute the partial derivatives with respect to a1 and
;place in the second row of PDER.

  ENDIF
END






;************************************************************************
;AUTOCORRELATION PROCEDURE
;
;we will take a complex order parameter array of size x,y and put it into a bigger
;array of size 2x, 2y, padding with zeros as per usual, so we can do
;autocorrelations oflong distances.
;in doing so we now have a big jumpat the edges - from 0 to whatever the
;data is - so we  put a smooth descent at edges
;this trick is the Wiener Khitchine short cut - see my thesis, chapter 6?
;for quickly calculating an autocorrelation.
;INPUT: complex ORDERPARAMETER, int REPEATSPACING
;OUTPUT: COMPLEX SHIFTEDAUTOCORRELATION, INTENSITYARRAY, INTENSITYCOUNTARRAY
;
;************************************************************************
pro DoAutoCorrelation, OrderParameter,RepeatSpacing,ShiftedAutoCorrelation, IntensityArray,IntensityCountArray


         n=size(OrderParameter)
         xs=n[1]-1
         ys=n[2]-1


         BigFloatArray=fltarr(2*(xs+1),2*(ys+1))
         BigOrderParameterArray=complex(BigFloatArray,BigFloatArray)
         BigOrderParameterArray(((xs+1)/2):((xs+1)/2)+xs,((ys+1)/2):((ys+1)/2)+ys)=OrderParameter


;HERE I SHOULD SMOOTH THE EDGE STEP.
;do the left strip


         LeftStrip=BigOrderParameterArray( (((xs+1)/2)-1*RepeatSpacing):((xs+1)/2)+1*RepeatSpacing,(((ys+1)/2)-RepeatSpacing):((ys+1)/2)+ys+RepeatSpacing)
         LeftStrip= SMOOTH( LeftStrip, RepeatSpacing, /EDGE_TRUNCATE )
         BigOrderParameterArray( (((xs+1)/2)-1*RepeatSpacing):((xs+1)/2)+1*RepeatSpacing,(((ys+1)/2)-RepeatSpacing):((ys+1)/2)+ys+RepeatSpacing)=LeftStrip


         RightStrip=BigOrderParameterArray( (( xs+(xs+1)/2)-1*RepeatSpacing):xs+((xs+1)/2)+1*RepeatSpacing,(((ys+1)/2)-RepeatSpacing):((ys+1)/2)+ys+RepeatSpacing)
         RightStrip= SMOOTH( RightStrip, RepeatSpacing, /EDGE_TRUNCATE )
         BigOrderParameterArray( (( xs+(xs+1)/2)-1*RepeatSpacing):xs+((xs+1)/2)+1*RepeatSpacing,(((ys+1)/2)-RepeatSpacing):((ys+1)/2)+ys+RepeatSpacing)=RightStrip

         TopStrip=BigOrderParameterArray( (((xs+1)/2)-1*RepeatSpacing):xs+((xs+1)/2)+1*RepeatSpacing,(((ys+1)/2)-RepeatSpacing):((ys+1)/2)+RepeatSpacing)
         TopStrip= SMOOTH( TopStrip, RepeatSpacing, /EDGE_TRUNCATE )
         BigOrderParameterArray( (((xs+1)/2)-1*RepeatSpacing):xs+((xs+1)/2)+1*RepeatSpacing,(((ys+1)/2)-RepeatSpacing):((ys+1)/2)+RepeatSpacing)=TopStrip

         BottomStrip=BigOrderParameterArray( (((xs+1)/2)-1*RepeatSpacing):xs+((xs+1)/2)+1*RepeatSpacing,ys+(((ys+1)/2)-RepeatSpacing):ys+((ys+1)/2)+RepeatSpacing)
         BottomStrip= SMOOTH( BottomStrip, RepeatSpacing, /EDGE_TRUNCATE )
         BigOrderParameterArray( (((xs+1)/2)-1*RepeatSpacing):xs+((xs+1)/2)+1*RepeatSpacing,ys+(((ys+1)/2)-RepeatSpacing):ys+((ys+1)/2)+RepeatSpacing)=BottomStrip




         ;f=shift(fft(data),n(0)/2,n(1)/2)
         ;window,4,xsize=n(0)+140,ysize=n(1)+140
         ;tvscl,alog(abs(f)),100,100 & tv,bytscl(rebin(abs(f),n(0)/8,n(1)/8),max=2),100,100


           MyFFT = FFT(BigOrderParameterArray, -1) ;forward fft
;      MyFFT=fft(data1,-1) ;delete this
         n=size(MyFFT) & n=n(1:n(0))

           ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
           ;window,6, xsize=n(0), ysize=n(1)
         ;tvscl,alog(abs(Dummy))  ;,100,100
         ;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;      tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


         SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


         AutoCorrelation=FFT(SquaredFFT,1)       ;backwards fft
         ;autocorrelation should be in the AutoCorrelation array

         ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
         ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

       ; window,6, xsize=xs, ysize=ys
       ; tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

         IntensityArray=fltarr(1,2*xs+1)
         IntensityCountArray=intarr(1,2*xs+1)
;
;      for xx=0L,((2*xs)+1) do begin
;          for yy=0L,((2*ys)+1) do begin
;                Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;                IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;                IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;               endfor
;      endfor




;note that each quadrant is done separately.
; there is probably a more intelligent way to write this
; but I'm a lazy sod.

;1 top left
         for xx=0L,((xs)) do begin
          for yy=0L,((ys)) do begin
                   Radius=round(sqrt(xx*xx+yy*yy))
                   IntensityArray[Radius]=IntensityArray[Radius]+float(AutoCorrelation(xx,yy)) ;just get real component with float
                   IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
                 endfor
         endfor

;2 top right
         for xx=(xs+1)*1L,(2*xs+1) do begin
          for yy=0L,(ys) do begin
                   Radius=round(sqrt((1+2*xs-xx)*(1+2*xs-xx)+yy*yy))
                   IntensityArray[Radius]=IntensityArray[Radius]+float(AutoCorrelation(xx,yy)) ;just get real component with float
                   IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
                 endfor
         endfor

;3 bottom left
         for xx=0L,(xs) do begin
          for yy=(ys+1)*1L,(2*ys+1) do begin
                   Radius=round(sqrt(xx*xx+(2*ys+1-yy)*(2*ys+1-yy)))
                   IntensityArray[Radius]=IntensityArray[Radius]+float(AutoCorrelation(xx,yy)) ;just get real component with float
                   IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
                 endfor
         endfor

;4 bottom right
         for xx=(xs)*1L,(2*xs+1) do begin
          for yy=(ys)*1L,(2*ys+1) do begin
                   Radius=round(sqrt((2*xs+1-xx)*(2*xs+1-xx)+(2*ys+1-yy)*(2*ys+1-yy)))
                   IntensityArray[Radius]=IntensityArray[Radius]+float(AutoCorrelation(xx,yy)) ;just get real component with float
                   IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
                 endfor
         endfor



;once we have the sum of the correlation intensities for all pairs, averaging over
;all theta for that particular distance, we need to get the AVERAGE correlation
;intensity by dividing by the number of pairs.

         for xx=0L,((2*xs)) do begin
                 if (IntensityCountArray[xx] ne 0) then  IntensityArray[xx]=IntensityArray[xx]/(1.0*IntensityCountArray[xx])
                 endfor
         Normalizer=IntensityArray[0]
         for xx=0L,(2*xs) do begin
                   IntensityArray[xx]=IntensityArray[xx]/Normalizer
                 endfor

end






function colorred,index
return,255*(1+sin(index*2*!pi/255))/2
end

function colorgreen,index
return,255*(1+sin(index*2*!pi/255-2*!pi/3))/2
end

function colorblue,index
return,255*(1+sin(index*2*!pi/255+2*!pi/3))/2
end


function colortable,verbose=verbose,data,fs,winid
    if(keyword_set(verbose)) then begin
       fs=dialog_pickfile(get_path=ps)
       if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif' then readlist,fs(0),fs,path=ps else fs=[fs]
       print,'Now doing: ',fs(0)
       data1=read_tiff(fs(0))
       fname=fs(0)
    endif else begin
       data1=data*255*3./(!pi)
       fname=fs
    endelse

       n=size(data1);
       dim1=n(1);
       dim2=n(2);

       image2=fltarr(3,dim1,dim2)
       ;imrd=colorred(data1)
       ;imgr=colorgreen(data1)
       ;imbl=colorblue(data1)

       r=[6*replicate(42,43),6*(42-indgen(42)),6*replicate(0,86),6*indgen(42),6*replicate(42,43)]
       g=[6*replicate(0,86),6*indgen(42),6*replicate(42,43),6*replicate(42,43),6*(42-indgen(42))]
       b=[6*indgen(42),6*replicate(42,43),6*replicate(42,43),6*(42-indgen(42)),6*replicate(0,86)]
       imrd=r(data1)
       imgr=g(data1)
       imbl=b(data1)




       image2(0,*,*)=imrd
       image2(1,*,*)=imgr
       image2(2,*,*)=imbl
       image1=[[[imrd]],[[imgr]],[[imbl]]]
       ;print,image
       ;help,image1
       return, image1


;     tvscl,image1,true=3
;     write_tiff, strmid(fname,0,strlen(fname)-4)+'color.tif', bytscl(image2),0
;     smpltiff,strmid(fs[i],0,strlen(fs[i])-4)+'color.tif',bytscl(image1)

end




function sign_of, x

    if (x lt 0) then return, -1
    if (x gt 0) then return,  1
    return,0

end



pro make_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound

    nn1=edges[vertex1+1]-edges[vertex1]
    nn2=edges[vertex2+1]-edges[vertex2]

    if ((nn1 eq 6) or (nn2 eq 6)) then stop

    var1=edges[vertex1]+where(edges[edges[vertex1]:edges[vertex1+1]-1] eq vertex2)
    bound[var1]=bound[var1]+1
    var1=edges[vertex2]+where(edges[edges[vertex2]:edges[vertex2+1]-1] eq vertex1)
    bound[var1]=bound[var1]+1
    var2=unbounddisc[vertex1]
    if (var2 eq 0)  then begin
       var21=0
    end else begin; Not quite sure if this is right, need to check matt's code
       var21=var2/abs(var2)
    end
    unbounddisc[vertex1]=var2-var21

    var2=unbounddisc[vertex2]
    if (var2 eq 0)  then begin
       var21=0
    end else begin; Not quite sure if this is right, need to check matt's code
       var21=var2/abs(var2)
    end
    unbounddisc[vertex2]=var2-var21

;   print, "making bond between ",vertex1,vertex2

    if (!draw_recursively eq 0) then begin
       Common globalgood, goodx, goody
       plots,[!goodx[vertex1]/!xss,!goodx[vertex2]/!xss],[1-!goody[vertex1]/!yss,1-!goody[vertex2]/!yss],/normal,color=!colordisloc,thick=2
    end
end




pro break_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound

    var1=edges[vertex1]+where(edges[edges[vertex1]:edges[vertex1+1]-1] eq vertex2)
    bound[var1]=bound[var1]-1


    var1=edges[vertex2]+where(edges[edges[vertex2]:edges[vertex2+1]-1] eq vertex1)
    bound[var1]=bound[var1]-1

    nn1=edges[vertex1+1]-edges[vertex1]-6
    nn2=edges[vertex2+1]-edges[vertex2]-6

    unbounddisc[vertex1]=unbounddisc[vertex1]+sign_of(nn1)
    unbounddisc[vertex2]=unbounddisc[vertex2]+sign_of(nn2)

;   print, "breaking bond between ",vertex1,vertex2
    ;erase old bond

    if (!draw_recursively eq 0) then begin
    Common globalgood, goodx, goody
       plots,[!goodx[vertex1]/!xss,!goodx[vertex2]/!xss],[1-!goody[vertex1]/!yss,1-!goody[vertex2]/!yss],/normal,color=!colorwhite,thick=2
       plots,[!goodx[vertex1]/!xss,!goodx[vertex2]/!xss],[1-!goody[vertex1]/!yss,1-!goody[vertex2]/!yss],/normal,color=!colorbond,thick=1.5
    end
end




function it_would_help_to_bond_nicely ,vertex1,vertex2,edges, unbounddisc, inbounds, inprocess,bound,outofboundsok
    nvertices=n_elements(inbounds)
    if ((sign_of(unbounddisc[vertex1]) eq -sign_of(unbounddisc[vertex2])) and (inprocess(vertex2) eq 0) ) then begin
    ;  if (((unbounddisc[vertex1]*unbounddisc[vertex2] lt 0)) and (inprocess(vertex2) eq 0) ) then begin
    if ((inbounds(vertex2) eq 1) or (outofboundsok eq 1)) then begin
       return,1
    end
    end else begin
       return,0
    end
end





function try_to_find_a_mate_nicely_for, vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound,outofboundsok
;   print, "try_to_find_mates_nicely_for",vertex
    for n=edges[vertex], edges[vertex+1]-1 do begin
       if(it_would_help_to_bond_nicely(vertex,edges[n],edges, unbounddisc,inbounds, inprocess,bound,outofboundsok) eq 1) then begin
         make_single_bond_between,vertex, edges[n],nvertices,edges,unbounddisc, inbounds, inprocess,bound
         return,1
       end
    endfor
    return,0
end


; There is a problem with sign of...

function it_would_help_me_to_butt_in_on, vertex1, vertex2,edges,unbounddisc, inbounds, inprocess,bound,outofboundsok
    nvertices=n_elements(inbounds)
    nn1=edges[vertex1+1]-edges[vertex1]-6
    nn2=edges[vertex2+1]-edges[vertex2]-6
    if (sign_of(nn1) eq -sign_of(nn2)) then begin
;   if ((unbounddisc[vertex1]*unbounddisc[vertex2] lt 0)) then begin
    if (inprocess(vertex2) eq 0) then begin
       return,1
    end
    end else begin
       return,0
    end
end

function can_retract_from, vertex1, vertex2, edges, unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok
;   print, "called the motherfucker"
    inprocess[vertex1]=1
    break_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound
;   print, !recursion_level


    if (!recursion_level gt !max_recursion_level) then goto,can_retract_end
    if ((inbounds[vertex2] ne 1)) then begin
       inprocess[vertex1]=0
       return,1
    end

    !recursion_level=!recursion_level+1


    if ((try_to_find_a_mate_nicely_for(vertex2,nvertices,edges,unbounddisc, inbounds, inprocess,bound,outofboundsok) eq 1)) then begin
       inprocess[vertex1]=0
       !recursion_level=!recursion_level-1
       return,1
    end

    if (try_to_find_a_mate_rudely_for(vertex2,nvertices,edges,unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok) eq 1) then begin
       inprocess[vertex1]=0
       !recursion_level=!recursion_level-1
       return,1
    end

    !recursion_level=!recursion_level-1

can_retract_end :
    make_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound
    inprocess[vertex1]=0
    return,0
end

function can_butt_in_on, vertex1, vertex2,edges, unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok
    nvertices=n_elements(inbounds)

    if ((inbounds[vertex2] eq 0) and (outofboundsok eq 0)) then begin
       return,0
    end
    if not(ButtInOnAble[vertex2,outofboundsok]) then begin
       return,0
       end
    inprocess[vertex1]=1

    for i=edges[vertex2], edges[vertex2+1]-1 do begin
       if (bound[i] gt 0) and ((inbounds[edges[i]] eq 1) or (outofboundsok eq 1)) then begin
       if (can_retract_from(vertex2, edges[i],edges, unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok) eq 1) then begin
         inprocess(vertex1)=0
         return,1
       end
       end
    endfor
    inprocess(vertex1)=0
    ButtInOnAble[vertex2,outofboundsok] = 0
    return,0
end


function try_to_find_a_mate_rudely_for, vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble,outofboundsok

    for n=edges[vertex], edges[vertex+1]-1 do begin
       if (it_would_help_me_to_butt_in_on(vertex, edges[n],edges,unbounddisc, inbounds, inprocess,bound,outofboundsok) eq 1 ) then begin
       if (can_butt_in_on(vertex, edges[n],edges, unbounddisc,inbounds, inprocess,bound, ButtInOnAble,outofboundsok) eq 1) then begin
         make_single_bond_between,vertex, edges[n],nvertices,edges,unbounddisc, inbounds, inprocess,bound
         return,1
       end
       end
    endfor

    return,0
end

pro try_to_find_mates_for,vertex,nvertices,edges,unbounddisc, inbounds, inprocess, bound, ButtInOnAble
    if (inbounds[vertex] eq 1) then begin
       inprocess(vertex)=1
jump0:   if (unbounddisc[vertex] eq 0) then goto,jump1
       if (try_to_find_a_mate_nicely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound,0) eq 0) then goto,jump1
       goto,jump0

jump1:   if (unbounddisc[vertex] eq 0) then goto,jump2
       if (try_to_find_a_mate_rudely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble,0) eq 0) then goto,jump2
       goto,jump1

jump2:   if (unbounddisc[vertex] eq 0) then goto,jump3
       if (try_to_find_a_mate_nicely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, 1) eq 0) then goto,jump3
       goto,jump2

jump3:   if (unbounddisc[vertex] eq 0) then goto,jump4
       if (try_to_find_a_mate_rudely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble, 1) eq 0) then goto,jump4
       goto,jump3

jump4:   inprocess(vertex)=0
    end
end


pro find_unbound_disc,nvertices,edges,unbounddisc, inbounds,inprocess,bound, ButtInOnAble
    for i=long(0), nvertices-1 do begin
;     print, "vertex ", i, unbounddisc[i]
       try_to_find_mates_for,i,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble
    end
end


function rgbcolor,R,G,B

    return,R+256L*(G+256L*B)

end


function Gscale,G,mult
    Gcopy=G
    Gcopy[*,1]=G[*,1]*mult
    Gradius=sqrt(Gcopy[*,1]^2.+Gcopy[*,0]^2.)
    Grad=total(Gradius)/6
    ;print, "Grad for mult=",mult," is ", Grad
    result=0
    Gdif=sqrt(total((Gradius-Grad)^2.)/6.)
    ;print, "Gdif for mult=",mult," is ", Gdif
    return,Gdif

end


;*********************************************************************************
; This code is a workaround for the limitations placed on virtual memory, modified
; from code given by RSI.


; File: draw_app_scroll2.pro
; Syntax: DRAW_APP_SCROLL2, my_large_image_array
; Purpose: Demonstrate how a very large image can be displayed most
; efficiently in IDL. The algorithm uses a scrollable WIDGET_DRAW,
; implemented with the /APP_SCROLL keyword, and displaying just the
; tile of the image array that is needed to fill up the whole viewport
; of the WIDGET_DRAW. Notice how the TVRD() in the event handler reads
; only the displayed subset of the full image.
; Instructions: I tested a dummy random-striped dataset with the
; following code:
; IDL> img = bytarr(8192, 8192)
; IDL> for i = 0, 255 do img[*, i*32:i*32+31] = byte(randomu(seed) * 255)
; IDL> draw_app_scroll2, img

; Event-handler routine. Does nothing in this example.
PRO draw_app_scroll2_event, ev
COMMON GLOBAL_IMAGE, IMAGE, CURRENT_IMAGE_SUBSET

; In this simple app we assume that 'ev.id' must be the WIDGET_DRAW.
widget_control, ev.id, GET_VALUE=winID
wset, winID

; Check the event type. If the event is a viewport event
; (type 3), redraw the image in the viewport using the
; new X and Y coordinates contained in the event structure.
; Note that we can use this simple check for the value of
; the TYPE field because there are no other widgets in this
; example to generate events; in a more complex widget
; application, a more sophisticated check would be necessary.
if (ev.TYPE eq 3) then tv, IMAGE[ev.x:ev.x+511, ev.Y:ev.y+511]

; Demonstrate TVRD, using CURRENT_IMAGE_SUBSET. Only the
; 400x400 block displayed in the scroll window is captured by
; TVRD. CURRENT_IMAGE_SUBSET is a global variable that holds
; the latest display.
;CURRENT_IMAGE_SUBSET = tvrd()
;help, CURRENT_IMAGE_SUBSET
;window, XSIZE=400, YSIZE=400
;tv, CURRENT_IMAGE_SUBSET
END

; Widget creation routine.
PRO draw_app_scroll2, img, nlayers, base    ;this is to replicate the functionality
                             ;of the original showimage.pro
COMMON GLOBAL_IMAGE, IMAGE, CURRENT_IMAGE_SUBSET
IMAGE = img
n=size(img)

base = widget_base()
; Create the draw widget. The size of the viewport is set to
; 400x400 pixels, but the size of the "virtual" drawable area is
; set equal to the dimensions of the image array using the
; XSIZE and YSIZE keywords. "/APP_SCROLL" makes sure that your
; application does not actually have to find full duplicated
; extra storage for the graphics display of the 'image' array.
wDraw = widget_draw(base, X_SCROLL_SIZE=512, Y_SCROLL_SIZE=512, $
    XSIZE=n[1], YSIZE=n[2], /APP_SCROLL, retain=2)

widget_control, base, /REALIZE
widget_control, wDraw, GET_VALUE=drawID
wset, drawID
;device, get_decomposed=current_decomposed
;current_order = !order
;
;- Set image to display from bottom up
;
;!order = 0
;
;- Display the image
;
;if nlayers eq 1 then begin
;
;  device, decomposed=0
;  tv, IMAGE[0:511,0:511,0:2]
;
;endif else begin
;
;  device, decomposed=1
;  tv, IMAGE[0:511,0:511], true=3
;
;endelse
;
;- Restore decomposed mode and display order
;
;device, decomposed=current_decomposed
;!order = current_order

tv, image[0:512,0:512]

xmanager, 'draw_app_scroll2', base, /NO_BLOCK

END


;This program is a script to extract the image from an app_scroll widget
;it works by grabbing portions of the window by TVRD

pro getimage, base






end
