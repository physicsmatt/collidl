;--------------------------------------------------------------
;gofr g(r)
if (gofr eq 1) then begin
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

endif ;(gofr eq 1)
;gofr   g(r)
