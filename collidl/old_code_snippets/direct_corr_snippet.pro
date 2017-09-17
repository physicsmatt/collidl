; From here on the code is taken from Anglecorr.pro, and the point is to do the direct
; correlation on the image itself rather than on the fourier transform


;direct
if (directcorr eq 1) then begin
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
