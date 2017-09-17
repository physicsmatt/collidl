;translatecorr
;********************     Trying to redo the autocorrelation by the image-displacement technique
if(translatecorr eq 1) then begin
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


endif  ;translatecorr
