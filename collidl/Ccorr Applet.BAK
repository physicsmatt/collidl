; Freshly minted Ccorr Applet! wooooha

     spawn, './robcor'
       spawn, 'robcor',/hide

      robcor=read_ascii('robcor.dat');
       dcorr=transpose(robcor.field1[0,*])
       ncorr=transpose(robcor.field1[2,*])
       fcorr=transpose(robcor.field1[1,*])

       dimplot=600    ; current work area!  previous code was dimplot=300


       window,1,xsize=dimplot+1,ysize=dimplot
       plot, dcorr(0:dimplot), fcorr(0:dimplot), xtitle='pixels', ytitle='g6(r)',psym=0

       CorrelationLenghTest=abs(fcorr(0:dimplot)-exp(-1.0))
       CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
       print,'The direct correlation length seed was ', Min_Subscript, ' .'

       A=[.5,Min_Subscript]


       dimfit=200;
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


end