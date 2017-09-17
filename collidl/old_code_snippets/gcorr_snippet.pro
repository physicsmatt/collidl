;Gcorr
;********************
if (Gcorr eq 1) then begin




  Gn=size(data) & Gn=Gn(1:Gn(0))
  Gf=abs(shift(fft(data),Gn(0)/2,Gn(1)/2))
  Gf(Gn(0)/2, Gn(1)/2)=0
  bGf=Gf[Gn[0]/2-70:Gn[0]/2+70,Gn[1]/2-70:Gn[1]/2+70]
  data=0
  showimage,bytscl(bGf),1,wGfft

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
      endfor
    endfor
    gr=0
    gf=0
    Grot=0

    ;print,"The radius of the Fourier radius for peak ",Gk," is ", sqrt(G[Gk,0]^2+G[Gk,1]^2)

    plots,G[Gk,0]+70, G[Gk,1]+70,color=1000L,/device,psym=3,thick=1,symsize=2


  endfor


  Gfactors=1+(indgen(100)-50.)/250.0
  Gdifs=fltarr(100)

  for Gkk=0,99 do begin
    Gdifs[Gkk]=Gscale(G,Gfactors[Gkk])
  endfor
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


endif ;(Gcorr eq 1)
