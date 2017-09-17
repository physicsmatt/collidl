;brutecorr
if (Brutecorr eq 1) then begin
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
endif ;(Brutecorr eq 1)
