;--------------------------------------------------------------




; From here on the code is modified from Chris' Stripes Code


;ffcorr

xss_new=255 ;changed from !xss by Matt, 7/2017.  Seemed wrong to redefine !xss and !yss here.
;Note: I'm not even sure 255 is the right value; this could be an old mistake that never got fixed. --MT, 7/2017
yss_new=255;


if (ffcorr eq 1) then begin
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
  window,0, xsize=xss_new+1, ysize=yss_new+1
  plot, IntensityArray(0:xss_new), xtitle='pixels', ytitle='g2(r)',psym=0
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




