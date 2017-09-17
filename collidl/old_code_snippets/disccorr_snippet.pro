;--------------------------------------------------------------
if (disccorr eq 1) then begin

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


  print,'The dislocation correlation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
  print,'It required ', iter , ' iterations, chisq=',chisq

  Summary_of_Data[0,i]=A[1]



endif ;(disccorr eq 1)
