
;This program is supposed to read a set of sphere coordinates from a file, 
;do the triangulation and generate the orientation field 

pro smooth, input, output, weights, howmuch

	for aa=-howmuch, howmuch do begin
	for bb=-howmuch, howmuch do begin
		output=output+weights[aa+howmuch, bb+howmuch]*shift(input,aa,bb)
	endfor
	endfor

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
	img1[x,yinit:yend,0]=0
	img1[xinit:xend,y,0]=0
	img1[x,yinit:yend,1]=255
	img1[xinit:xend,y,1]=255
	img1[x,yinit:yend,2]=0
	img1[xinit:xend,y,2]=0


	return,img1
end

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
	img1[x,yinit:yend,0]=255
	img1[xinit:xend,y,0]=255
	img1[x,yinit:yend,1]=0
	img1[xinit:xend,y,1]=0
	img1[x,yinit:yend,2]=0
	img1[xinit:xend,y,2]=0


	return,img1
end

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


;****************************************************************************
























pro main,data
		device,retain=2

		RepeatSpacing=2
		; howmuch (pixels out of 256) controls the smoothing of the angular correlation file
		howmuch=RepeatSpacing*2 ;(repeatspacing units)

		weights=fltarr(2*howmuch+1,2*howmuch+1)

		norm=0
		for aa=-howmuch, howmuch do begin
		for bb=-howmuch, howmuch do begin
			weights[aa+howmuch, bb+howmuch]=exp(-aa*aa-bb*bb)
			norm=norm+weights[aa+howmuch, bb+howmuch]		
		endfor
		endfor

		weights=weights/norm
		

		
		Wdata=read_ascii('weights.dat',count=count)
		W=Wdata.field1[*]
		W=W/Total(W)
		;help,W
		;print,'Read ',count,' weight points from weights.dat'
		;window,1
		;plot,W
		W[*]=1		

		fs=dialog_pickfile(get_path=ps,/multiple_files)
		if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'dat' then readlist,fs(0),fs,path=ps else fs=[fs]
		Summary_of_Data=fltarr(10, n_elements(fs))
		for i=0,n_elements(fs)-1 do begin
			
			print,'Now working on : ',fs(i)

			count=0
			data1=read_ascii(fs(i), delimiter=',', count=count);
			print, 'Count # spheres = ', count
			;help,data1

			xs=1023
			ys=1023

			img=fltarr(1024,1024,3)

			;window,0,xsize=xs+100,ysize=ys+100
			;tvscl,img,50,50,true=3


			showimage,img,3


			goodx=data1.field1[0,*]
			goody=data1.field1[1,*]			

			nvertices=n_elements(goodx)-1
			goodx=goodx(1:nvertices)
			goody=goody(1:nvertices)
			
			
			triangulate, goodx, goody, triangles, outermost, CONNECTIVITY = edges
			



		if (0 eq 0) then begin
			disc=fltarr(nvertices,2)
			bondsx=fltarr(100000)
			bondsy=fltarr(100000)
			bondsangle=fltarr(100000)
			bondcount=0L;
			for i1=0,nvertices-1 do begin
				;if  ((i1 mod 1000) eq 0) then begin
				;	print, i1, ' spheres analyzed...'
				;endif
				
				disc[i1]=edges[i1+1]-edges[i1]
				for j1=edges[i1],edges[i1+1]-1 do begin
					if (i1 lt edges[j1]) then begin
						;img=draw_line(img,goodx[i1],goody[i1],goodx[edges[j1]],goody[edges[j1]])
						
						plots,[goodx[edges[j1]]/1024,goodx[i1]/1024],[goody[edges[j1]]/1024,goody[i1]/1024],/normal,color=256*130,thick=2
						
						bondsx[bondcount]=(goodx[i1]+goodx[edges[j1]])/2
						bondsy[bondcount]=(goody[i1]+goody[edges[j1]])/2				
						ang1=!pi+atan(goody[i1]-goody[edges[j1]],goodx[i1]-goodx[edges[j1]])
						ang=ang1-(!pi/3)*floor(ang1*3/!pi)
						bondsangle[bondcount]=ang
						bondcount=bondcount+1							
					endif
				endfor
				
			endfor			
			
			
			
			discsize=2
			for i1=0,nvertices-1 do begin
				

				if (disc[i1] lt 6) then begin
				for discind=-discsize,discsize do begin
				plots,[(goodx[i1]-discsize)/1024,(goodx[i1]+discsize)/1024],[(goody[i1]+discind)/1024,(goody[i1]+discind)/1024],/normal,color=255	
				endfor
				endif

				if (disc[i1] gt 6) then begin
				for discind=-discsize,discsize do begin
				plots,[(goodx[i1]-discsize)/1024,(goodx[i1]+discsize)/1024],[(goody[i1]+discind)/1024,(goody[i1]+discind)/1024],/normal,color=256*255	
				endfor
				endif



			endfor
		

			bondsx=bondsx(0:bondcount)
			bondsy=bondsy(0:bondcount)
			bondsangle=bondsangle(0:bondcount)
			print,'Found ', bondcount,' Delauney bonds'		
			triangulate, bondsx, bondsy, btriangles, boutermost, CONNECTIVITY = bedges			
			bangle=trigrid(bondsx,bondsy,bondsangle,btriangles, NX=256,NY=256)
			;help,bangle
			;tvscl,bangle
;			print,'Writing angle TIFF file'
;			write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'angle.tif',bytscl(bangle,MAX= !pi/3,MIN=0)




; Do gaussian smoothing


			smoothbangle=fltarr(256,256)
			smooth,bangle, smoothbangle, weights, howmuch
			
			print, 'Displaying smoothed bond angle...'
			showimage,bytscl(smoothbangle),1
			
			print,'Writing angle TIFF file'
			write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'smooth.tif',bytscl(smoothbangle,MAX= !pi/3,MIN=0)


						

		endif







; From here on the code is modified from Chris' Stripes Code


			xs=255;
			ys=255;
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
			window,0, xsize=xs+1, ysize=ys+1
			plot, IntensityArray(0:xs), xtitle='pixels', ytitle='g2(r)',psym=0
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
 

			;tv,bytscl(-255 * alog10( abs(AutoCorrelation))/Max(-alog10(abs(AutoCorrelation))))

 

			;write the correlation function out
			openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'robcor.dat',/get_lun
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
			
 


; From here on the code is taken from Anglecorr.pro, and the point is to do the direct
; correlation on the image itself rather than on the fourier transform


		pi=3.141592
		dimplot=256
		sampling=4 ; the size of the sampling square for direct correlations
		dimfit=64 ; the size of the picture over which correlation function is fitted


		n=size(smoothbangle);
		dim1=n(1)
		dim2=n(2)
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
			;	print, 'Line # = ',yvar1
			;endif
			for k=0L,j do begin

				xvar2=sampling*(k mod samplingdim1)
				yvar2=sampling*(k/samplingdim1)
				d=round(sqrt((xvar1-xvar2)*(xvar1-xvar2)+(yvar1-yvar2)*(yvar1-yvar2)))
				;print, 'd=',d
				corr[d]=corr[d]+cos(6*(smoothbangle[xvar1,yvar1]-smoothbangle[xvar2,yvar2]))
				pairs[d]=pairs[d]+1;
			endfor
		endfor

		for j=0L,dim-1 do begin
			if pairs[j] ne 0 then begin
				corr[j]=corr[j]/pairs[j]
			endif
		endfor

		CorrelationLenghTest=abs(corr-exp(-1.0))
		CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
		print,'The direct correlation length seed was ', Min_Subscript, ' .'

		A=[1,Min_Subscript]
		;A=[Min_Subscript]



		;window,0,xsize=dim1/2+50,ysize=dim2/2+50
		;tvscl,angle,25,25

		index=where(pairs)
		restrict=where(index lt dimplot)
		index=index[restrict]

		
		window,1,xsize=dimplot+1,ysize=dimplot+1
		plot, index, corr[index], xtitle='pixels', ytitle='g6(r)',psym=0
		
		restrict2=where(index lt dimfit)
		index=index[restrict2]
		yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct')


		X=indgen(dimfit)
		;plot out the determined function in a diffference color
		;F = (EXP(-X/A[0]))
		F = A[0]*(EXP(-X/A[1]))
		oplot,F,psym=0, color=254
		;blue equals 256*127

		print,'The direct correlation length was ', A[1],' and its coefficient was', A[0]
		;print,'The direct correlation length was ', A[0],' and its coefficient was', ' N/A'
		Summary_of_Data[2,i]=A[0]
		print, 'All done with ',fs[i],'.'
















		if (1 eq 0) then begin

			; This part makes the nice triangulation image... Not necessary now

			disc=fltarr(nvertices,2)
			for i1=0,nvertices-1 do begin
				print, 'Sphere # ',i1
				disc[i1]=edges[i1+1]-edges[i1]
				for j1=edges[i1],edges[i1+1]-1 do begin
				if (i1 lt edges[j1]) then begin
						img=draw_line(img,goodx[i1],goody[i1],goodx[edges[j1]],goody[edges[j1]])
					endif
				endfor
			endfor

			for i1=0,nvertices-1 do begin


				if (disc[i1] eq 5) then img=make_green(img,goodx[i1],goody[i1])
				if 	(disc[i1] eq 7) then img=make_red(img,goodx[i1],goody[i1])

			endfor


			imgtiff=bytarr(3,xs+1,ys+1)
			for i2=0,xs do begin
				for j2=0,ys do begin
					imgtiff[0,i2,j2]=img[i2,j2,0]
					imgtiff[1,i2,j2]=img[i2,j2,1]
					imgtiff[2,i2,j2]=img[i2,j2,2]
				endfor
			endfor
			print,'Writing TIFF file'
			write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'color.tif',imgtiff
			print,'Done with TIFF file'
			;window,1,xsize=xs+100,ysize=ys+100
			;tvscl,img,50,50,true=3
		endif


	
		endfor ; main loop that reads each input file

		openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'summary.dat',/get_lun
		for LongIndex=0L, n_elements(fs)-1 do begin
			printf,u,Summary_of_Data[0,LongIndex],"   ",Summary_of_Data[1,LongIndex],Summary_of_Data[2,LongIndex],'   ',Summary_of_Data[3,LongIndex],'    ',fs[LongIndex]
		endfor
		free_lun,u

end
