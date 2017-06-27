
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

;********************************************************
;USED FOR MEASURING THE ANGLE FIELD
;call with image img,
;********************************************************
function CreateAngleField,img, RepeatSpacing

		SmoothRange=RepeatSpacing
 		gradx = shift(img,1,0)
        	gradx=img-1.0*gradx
       		grady = shift(img,0,1)
       		grady=img-1.0*grady
		absgrad=sqrt(gradx*gradx+grady*grady)
		func1=smooth(2*gradx*grady,SmoothRange)
		func2=smooth(gradx^2-grady^2,SmoothRange)


; smooththeta is the angle normal to the cylinders, ranging from -pi/2 to +pi/2.
		smooththeta=(atan(func1,func2))/2

		return, smooththeta


end
;********************************************************
;ANISOTROPICALLY SMOOTHS THE IMAGE ALONG THE CYLINDERS LOCALLY
;********************************************************
function CreateSmoothedImage,OriginalImage,smooththeta, AnisotropicBlurRange


			AxisOrientation=smooththeta+(!PI/2.0)
			AnisotropicBlur=OriginalImage
			AnisotropicBlur=AnisotropicBlur*1L
			n=size(OriginalImage)
			xs=n[1]-1
			ys=n[2]-1


			SmoothedImage=0L*AnisotropicBlur ;convert to Long


			for xx=AnisotropicBlurRange,xs-AnisotropicBlurRange do begin
			for yy=AnisotropicBlurRange,ys-AnisotropicBlurRange do begin

			for j=-AnisotropicBlurRange,AnisotropicBlurRange do begin
				ShiftX=round(j*cos(AxisOrientation(xx,yy)))
				ShiftY=round(j*sin(AxisOrientation(xx,yy)))
				SmoothedImage(xx,yy)=SmoothedImage(xx,yy)+AnisotropicBlur(xx+ShiftX,yy+ShiftY)
			;	if ((abs(ShiftX) gt 1) or (abs(ShiftY) gt 1) ) then print,ShiftX,ShiftY
			endfor
			endfor ;xx
			endfor ;yy

			SmoothedImage=Round(SmoothedImage/(2.0*AnisotropicBlurRange+1.0))
			return, SmoothedImage

end

;********************************************************
;procedure 	LocateDisclinations needs the smooththeta image, a blank
;LOOPSUM image of same dimensions for putting in the disclinations,
; and the range over which to smooth
;************************************************************
pro LocateDisclinations,smooththeta, LoopSum, SmoothRange


			n=size(smooththeta)
			xs=n[1]-1
			ys=n[2]-1



			SmoothTheta0=smooththeta
			SmoothTheta1=shift(smooththeta,0,1)
			SmoothTheta2=shift(smooththeta,1,1)
			SmoothTheta3=shift(smooththeta,1,0)


;			LoopSum=0*data1 ;piss poor way to generate array of zeros
			LoopSum=LoopSum-1*((SmoothTheta1-SmoothTheta0)ge(!PI/2.0))
			LoopSum=LoopSum+1*((SmoothTheta1-SmoothTheta0)lt(-!PI/2.0))

			LoopSum=LoopSum-1*((SmoothTheta2-SmoothTheta1)ge(!PI/2.0))
			LoopSum=LoopSum+1*((SmoothTheta2-SmoothTheta1)lt(-!PI/2.0))

			LoopSum=LoopSum-1*((SmoothTheta3-SmoothTheta2)ge(!PI/2.0))
			LoopSum=LoopSum+1*((SmoothTheta3-SmoothTheta2)lt(-!PI/2.0))

			LoopSum=LoopSum-1*((SmoothTheta0-SmoothTheta3)ge(!PI/2.0))
			LoopSum=LoopSum+1*((SmoothTheta0-SmoothTheta3)lt(-!PI/2.0))
;			window,2,xsize=xs,ysize=ys
;			tvscl, abs(LoopSum)


			;should zero out the edges of LoopSum
			LoopSum(*,0:ROUND(1.0*SmoothRange))=0 ;bottom
			LoopSum(*,ys-ROUND(1.0*SmoothRange): ys)=0 ;top
			LoopSum(0:ROUND(1.0*SmoothRange), *)=0 ;left
			LoopSum(xs-ROUND(1.0*SmoothRange): xs, *)=0 ;right

;			tvscl, abs(LoopSum)



end

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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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

end;************************************************************************
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
;			MyFFT=fft(data1,-1) ;delete this
			n=size(MyFFT) & n=n(1:n(0))

		    ;Dummy=shift(MyFFT,n(0)/2,n(1)/2)
		    ;window,6, xsize=n(0), ysize=n(1)
			;tvscl,alog(abs(Dummy))  ;,100,100
			;tv,bytscl(rebin(abs(Dummy),n(0)/8,n(1)/8),max=2)


;			tv,bytscl(-255 * alog10( abs(MyFFT))/Max(-alog10(abs(MyFFT))))


			SquaredFFT=(MyFFT)*CONJ(MyFFT) ; lop off the phase to prepare for autocorrelation


			AutoCorrelation=FFT(SquaredFFT,1)		;backwards fft
			;autocorrelation should be in the AutoCorrelation array

			ShiftedAutoCorrelation=shift(AutoCorrelation,n(0)/2,n(1)/2)
			ShiftedAutoCorrelation=ShiftedAutoCorrelation(xs/2:3*xs/2+1,ys/2:3*ys/2+1)

		;	window,6, xsize=xs, ysize=ys
		;	tvscl,bytscl(ShiftedAutoCorrelation)

;now let's average this .....properly
;it's a 2D array, but one dimension is one long.

			IntensityArray=fltarr(1,2*xs+1)
			IntensityCountArray=intarr(1,2*xs+1)
;
;			for xx=0L,((2*xs)+1) do begin
;				for yy=0L,((2*ys)+1) do begin
;							Radius=round(sqrt((xx)*(xx)+(yy)*(yy)))
;							IntensityArray[Radius]=IntensityArray[Radius]+float(ShiftedAutoCorrelation(xx,yy)) ;just get real component with float
;							IntensityCountArray[Radius]=IntensityCountArray[Radius]+1
;						endfor
;			endfor




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
;************************************************************
;function draw_cross used to put color cross on image
;************************************************************

function draw_cross,img,x,y
	n=SIZE(img)
	xs=n[1]-1
	ys=n[2]-1
	;print,w

	img1=img
	xinit=(x-5)>0
	xend=(x+5)<(xs-1)
	yinit=(y-5)>0
	yend=(y+5)<(ys-1)
	img1[x,yinit:yend,0]=0
	img1[xinit:xend,y,0]=0
	img1[x,yinit:yend,1]=255
	img1[xinit:xend,y,1]=255
	img1[x,yinit:yend,2]=0
	img1[xinit:xend,y,2]=0


	return,img1
end
;************************************************************
;function make_green makes a green circle on image
;************************************************************

function make_green,img,x,y
	n=SIZE(img)
	xs=n[1]-1
	ys=n[2]-1
	;print,w
	x=fix(x)
	y=fix(y)
	img1=img
	xinit=(x-2)>0
	xend=(x+2)<(xs-1)
	yinit=(y-2)>0
	yend=(y+2)<(ys-1)
	img1[xinit:xend,yinit:yend,0]=0
	img1[xinit:xend,yinit:yend,0]=0
	img1[xinit:xend,yinit:yend,1]=255
	img1[xinit:xend,yinit:yend,1]=255
	img1[xinit:xend,yinit:yend,2]=0
	img1[xinit:xend,yinit:yend,2]=0


	return,img1
end
;************************************************************
;make_red makes a red circle
;************************************************************

function make_red,img,x,y
	n=SIZE(img)
	xs=n[1]-1
	ys=n[2]-1
	;print,w
	x=fix(x)
	y=fix(y)
	img1=img
	xinit=(x-2)>0
	xend=(x+2)<(xs-1)
	yinit=(y-2)>0
	yend=(y+2)<(ys-1)
	img1[xinit:xend,yinit:yend,0]=255
	img1[xinit:xend,yinit:yend,0]=255
	img1[xinit:xend,yinit:yend,1]=0
	img1[xinit:xend,yinit:yend,1]=0
	img1[xinit:xend,yinit:yend,2]=0
	img1[xinit:xend,yinit:yend,2]=0


	return,img1
end
;************************************************************
;function draw_line puts a line in the color image
;************************************************************

function draw_line,img,x1,y1,x2,y2

	n=SIZE(img)
	xs=n[1]-1
	ys=n[2]-1
	x1=fix(x1)
	y1=fix(y1)
	x2=fix(x2)
	y2=fix(y2)
	img1=img
	d=fix(sqrt((x1-x2)^2+(y1-y2)^2))
	for r=0,d do begin
		xreal=x1+r*(x2-x1)/d
		yreal=y1+r*(y2-y1)/d
		if(xreal ge 0) and (xreal lt xs) and (yreal ge 0) and (yreal lt ys) then begin
			img1[xreal,yreal,0]=0
			img1[xreal,yreal,0]=0
			img1[xreal,yreal,1]=0
			img1[xreal,yreal,1]=0
			img1[xreal,yreal,2]=255
			img1[xreal,yreal,2]=255
		endif
	endfor
	return,img1
end
;*************************************************************
;*************************************************************
;WE START THE PROGRAM HERE
;*************************************************************
;*************************************************************
pro main,data

;Here we set the constants used in the program downstream
;*************************************************************
	RepeatSpacing=8
	SmoothRange=2*RepeatSpacing
	AnisotropicBlurRange=4

;*************************************************************

	fs=dialog_pickfile(get_path=ps,/multiple_files)


		if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif' then readlist,fs(0),fs,path=ps else fs=[fs]
		Summary_of_Data=fltarr(10, n_elements(fs))

		for FileNumber=0,n_elements(fs)-1 do begin
			print,'Now working on : ',fs(FileNumber)

			OriginalImage=read_tiff(fs(FileNumber))
		;	shrink the data set to fit it on the small laptop screen
;			OriginalImage=OriginalImage(0:255,0:255)
			n=size(OriginalImage)
			xs=n[1]-1
			ys=n[2]-1
;note that the # of x pixels is xs+1, # of y pixels is ys+1

			window,1,xsize=xs+1,ysize=ys+1
			tvscl,reverse(OriginalImage,2)


			;CreateAngleField creates an array of the angle, -PI/2 to +PI/2 at each location
			smooththeta=CreateAngleField(OriginalImage, 2*RepeatSpacing)

			window,2,xsize=xs+1,ysize=ys+1
			tvscl,reverse(SMOOTHTHETA,2)


			;CreateSmoothedImage anisotropically locally blurs the image along cylinder
		SmoothedImage=CreateSmoothedImage(OriginalImage,smooththeta, AnisotropicBlurRange)

			window,3,xsize=xs,ysize=ys
			tvscl,reverse(SmoothedImage,2)
			write_tiff,strmid(fs[FileNumber],0,strlen(fs[FileNumber])-4)+'smth.tif',SmoothedImage


			LoopSum=0*OriginalImage ;piss poor way to generate array of zeros
			;LocateDisclinations finds  disclinations, labels locations with +/-1 in the array LoopSum
			LocateDisclinations,smooththeta, LoopSum, 2*SmoothRange

			Plusdisc=where(LoopSum eq 1)
			Minusdisc=where(LoopSum eq -1)

			print, "Found ",n_elements(Plusdisc)," plus disclinations"
			print, "Found ",n_elements(Minusdisc)," minus disclinations"

			Summary_of_Data[2,FileNumber]=n_elements(Plusdisc)
			Summary_of_Data[3,FileNumber]=n_elements(Minusdisc)

;now we should figure out how to put these defects into an array
;for which correlation functions could be mapped out
;DisclinationArray=fltarray(sign,x,y,file number (usually FileNumber)
;note that the first row is full of zeros since I don't know a better way to do it.
;and we have mastered how to add additional columns

;Dan figured out how to make a color image, which is defined as "img"
			Color_Image=[[[OriginalImage]],[[OriginalImage]],[[OriginalImage]]]


			DisclinationArray=fltarr(4,1)

			for i2=0,n_elements(Plusdisc)-1 do begin
				ydisc=Plusdisc[i2]/(xs+1)
				xdisc=Plusdisc[i2]-ydisc*(xs+1)
				Color_Image=make_green(Color_Image,xdisc,ydisc)
				;must add another row somehow; we CONCATENATE ARRAYs
				;below, adding one more row with the submerged brackets
				DisclinationArray=[[DisclinationArray],[1,xdisc,ydisc,FileNumber]]
			endfor

			for i2=0,n_elements(Minusdisc)-1 do begin
				ydisc=Minusdisc[i2]/(xs+1)
				xdisc=Minusdisc[i2]-ydisc*(xs+1)
				Color_Image=make_red(Color_Image,xdisc,ydisc)
				DisclinationArray=[[DisclinationArray],[-1,xdisc,ydisc,FileNumber]]
			endfor


			window,2,xsize=xs,ysize=ys
			tvscl,reverse(Color_Image,2),true=3


			imgtiff=bytarr(3,xs+1,ys+1)
			for i2=0,xs do begin
				for j2=0,ys do begin
					imgtiff[0,i2,j2]=Color_Image[i2,j2,0]
					imgtiff[1,i2,j2]=Color_Image[i2,j2,1]
					imgtiff[2,i2,j2]=Color_Image[i2,j2,2]
				endfor
			endfor




;below we threshhold into values of 0 and 255  in two steps
			ThresholdImage=BYTSCL( OriginalImage , MAX=255, MIN=0, TOP=1)
			ThresholdImage=ThresholdImage*255

;write_tiff,'duh1.tif',ThresholdImage
;isolated = 1, terminal =2, in line =3,
;below we skeletonize image
;the original THIN routine works poorly,
;the supplied example from IDL via Mischa works better

	ThinImage = morphThinExample(ThresholdImage)
;			ThinImage = THIN(ThresholdImage-255,/NEIGHBOR_COUNT)
;below we count the number of terminals
;THIS DOESN'T WORK ANYMORE NOW THAT *THIN* DOES NOT WORK
			B = WHERE(ThinImage eq 1, count)
			print, 'number of isolated',count
			C = WHERE(ThinImage eq 2, count)
			print, 'number of terminals',count
			D = WHERE(ThinImage eq 3, count)
			print, 'number of in line',count
;  we could modify this to label the dislocations
;

			window,4,xsize=xs,ysize=ys
			tvscl,reverse(ThinImage,2)


;create the exp^2*imaginary*theta array
			OrderParameter=complex(cos(2.0*smooththeta),sin(2.0*smooththeta))
			window,5,xsize=xs,ysize=ys
			tvscl,reverse((OrderParameter))

			;The autocorrelation I return is already shifted
			DoAutoCorrelation,OrderParameter,RepeatSpacing,AutoCorrelation,IntensityArray,IntensityCountArray

			window,6, xsize=xs, ysize=ys
			tvscl,reverse(bytscl(abs(AutoCorrelation)),2)


;next we perform a curvefit, fit the function to y=a[0]*exp(-x/a[1])
;we set five parameters: the functions X array, Y array
;the weights W, and seed parameters a[0] (coefficient) and a[1] (correlation length)

		Y=IntensityArray(0:xs/4)
		X=findgen(1+xs/4)
		W=1.0/Y
		W(*)=1


CorrelationLenghTest=abs(Y-exp(-1.0))
CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
print,'The correlation length seed was ', Min_Subscript, ' .'
A=[1,Min_Subscript]
;OK, now call it
yfit = CURVEFIT(X, Y, W, A, SIGMA_A, FUNCTION_NAME = 'funct')
window,7, xsize=xs+1, ysize=ys+1
plot, IntensityArray(0:xs), xtitle='pixels', ytitle='g2(r)',psym=0
;plot out the determined function in a diffference color
F = A[0] * (EXP(-X/A[1]))
oplot, F,psym=0, color=254
;blue equals 256*127

	print,'The correlation length was ', A[1],' and its coefficient was ', A[0],'.'
	;record the data in the array for summary
	Summary_of_Data[0,FileNumber]=A[1]
	Summary_of_Data[1,FileNumber]=A[0]


;tv,bytscl(-255 * alog10( abs(AutoCorrelation))/Max(-alog10(abs(AutoCorrelation))))


write_tiff,strmid(fs[FileNumber],0,strlen(fs[FileNumber])-4)+'color.tif',imgtiff


;write the correlation function out
	openw,u,strmid(fs[FileNumber],0,strlen(fs[FileNumber])-4)+'robcor.dat',/get_lun
	n=size(IntensityArray)
	for LongIndex=0L,n(2)-1 do begin
    printf,u,LongIndex,IntensityArray[LongIndex], IntensityCountArray[LongIndex]
	endfor
	free_lun,u
	close,u

;window,5,xsize=2*xs,ysize=2*ys
;Slide_Image, bytscl(AutoCorrelation),SLIDE_WINDOW=MISCHA,group=g,FULL_WINDOW=8
;the below line destroys the Slide_image
;widget_control, g,/destroy
print, 'All done with ',fs[FileNumber],'.'

;WDELETE,1
;WDELETE,2
;WDELETE,3
;WDELETE,4
;WDELETE,5
;WDELETE,6
;WDELETE,7

			endfor ;THIS END FOR IS THE MASTER LOOP FOR READING ALL THE FILES
;here we want to output the Summary_of_Data information.


openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'summary.dat',/get_lun
for LongIndex=0L, n_elements(fs)-1 do begin
 printf,u,Summary_of_Data[0,LongIndex],"   ",Summary_of_Data[1,LongIndex],Summary_of_Data[2,LongIndex],'   ',Summary_of_Data[3,LongIndex],'    ',fs[LongIndex]
endfor
free_lun,u
;close,u

print,'All done with every file.'

end
