
;This program is supposed to read an image file, determine the sphere positions,
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















pro main,small=small,simu=simu,stay=stay,wait=wait,twomicron=twomicron


		; 0 = do it, 1 = don't
		translatecorr=1
		directcorr=1
		ffcorr=1
		Ccorr=0
		brutecorr=1

		gofr=1; whether to do radial distributions




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



		;Wdata=read_ascii('weights.dat',count=count)
		;W=Wdata.field1[*]
		;W=W/Total(W)
		;help,W
		;print,'Read ',count,' weight points from weights.dat'
		;window,1
		;plot,W
		W=fltarr(1000)
		W[*]=1

		fs=dialog_pickfile(get_path=ps,/multiple_files)
		if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif' then readlist,fs(0),fs,path=ps else fs=[fs]
		Summary_of_Data=fltarr(10, n_elements(fs))
		for i=0,n_elements(fs)-1 do begin

			print,'Now working on : ',fs(i)

			count=0

			data=read_tiff(fs(i));
			if (keyword_set(small)) then begin
				data=Interpolate(data,findgen(1024)/2,findgen(1024)/2,/grid)
			endif

			


			ns=size(data)
			xs=ns[1]-1
			ys=ns[2]-1

			sphere_diameter=4

			if (keyword_set(twomicron)) then begin
				sphere_diameter=5
			end

			if (keyword_set(simu)) then begin
				sphere_diameter=8
			endif 

			data=bpass(data,1,sphere_diameter)
			data1=feature(data,sphere_diameter)


			;help,data1


			img=bytarr(xs+1,ys+1,3)+255
			showimage,img,3,wbonds


			goodx=data1[0,*]
			goody=data1[1,*]

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

						plots,[goodx[edges[j1]]/xs,goodx[i1]/xs],[1-goody[edges[j1]]/ys,1-goody[i1]/ys],/normal,color=256L*256*255,thick=2

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
			disc5=0
			disc7=0


			for i1=0,nvertices-1 do begin
				
				if (disc[i1] lt 6) then begin
				for discind=-discsize,discsize do begin

				plots,[(goodx[i1]-discsize)/xs,(goodx[i1]+discsize)/xs],[1-(goody[i1]+discind)/ys,1-(goody[i1]+discind)/ys],/normal,color=255
				endfor
				if ((goodx[i1]<.95*xs) and (goodx[i1]>.05*xs) and (goody[i1]<.95*ys) and (goody[i1]>.05*ys)) then begin
					disc5=disc5+1
				endif
				endif

				if (disc[i1] gt 6) then begin
				for discind=-discsize,discsize do begin
				plots,[(goodx[i1]-discsize)/xs,(goodx[i1]+discsize)/xs],[1-(goody[i1]+discind)/ys,1-(goody[i1]+discind)/ys],/normal,color=256*255
				endfor
				if ((goodx[i1]<.95*xs) and (goodx[i1]>.05*xs) and (goody[i1]<.95*ys) and (goody[i1]>.05*ys)) then begin
					disc7=disc7+1
				endif
				endif



			endfor
			print,disc5
			

			saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'bonds.tif', /tiff



	if (gofr eq 0) then begin
			grsize=300 ; Size for the g(r) plot
			grscan=10; grscan^2 is the number of averaged vertices
			grplaces=fltarr(grscan^2); gives a bond in each of those squares
			grsizereal=grsize+30;

			areavertex=xs*ys/nvertices
			bondlength=sqrt(areavertex*4/sqrt(3))
			print, "Average bond length a=",bondlength
			grside=1.3*bondlength ;side of a square in which we are sure to capture at least one vertex

			for gri=0,grscan-1 do begin
			for grj=0,grscan-1 do begin
				grindex=gri*grscan+grj
				grxtemp=grsizereal+(xs-2*grsizereal)*gri/grscan
				grytemp=grsizereal+(ys-2*grsizereal)*grj/grscan
				grtemp=where((grxtemp gt goodx) and (grytemp gt goody) and (grxtemp lt goodx+grside) and (grytemp lt goody+grside))
				grplaces[grindex]=grtemp[1]
				print, "The x,y of the ", grindex ,"-th square are : ", grxtemp,grytemp
				print, "The coords of the vertex in square ", grindex, " are ", goodx[grplaces[grindex]],goody[grplaces[grindex]] 
			endfor
			endfor
						
			gr=fltarr(grsize+1)
			gind=findgen(grsize+1)
			gind=!pi*((gind+1)^2-gind^2)
			nscan=0

			for i2=0,grscan^2-1 do begin
						
				i1=grplaces[i2]								
				print, "Working on vertex ", i1, " x=",goodx[i1], " y=", goody[i1], "in square ", i2
				for ivar=0,grsize do begin
					;print, "ivar= ", ivar
				
					closevert=(where((sqrt((goodx-goodx[i1])^2.0+(goody-goody[i1])^2.0) le ivar+1) and (sqrt((goodx-goodx[i1])^2.0+(goody-goody[i1])^2.0) gt ivar)))	
					if(closevert[0] ne -1) then begin
						;print, goodx[closevert]
						;print, "   "
						;print, goody[closevert]	
						nvar=size(closevert)
					end else begin
						nvar=[0,0]
					end
					;print, "# neghbours at ", ivar," is ", nvar[1]
					gr[ivar]=gr[ivar]+nvar[1]					
				
				endfor
					
				nscan=nscan+1		
			endfor

			
			window,10

			fgr=areavertex*gr/(gind*nscan)
			plot,(indgen(300))/bondlength,fgr-1



			fgrplus=shift(fgr,1)
			fgrminus=shift(fgr,-1)
			fgr=fgr[1:*]
			fgrplus=fgrplus[1:*]
			fgrminus=fgrminus[1:*]
			wheremaxfgr=where((fgr gt fgrplus) and (fgr gt fgrminus))+1
			
			openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'gofrpeaks.dat',/get_lun

			for LongIndex=0L,n_elements(wheremaxfgr)-1 do begin
				printf,u,wheremaxfgr[LongIndex]/bondlength,fgr[wheremaxfgr[LongIndex]-1]-1
			endfor
			free_lun,u
			close,u

			

	
			saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'gofr.tif', /tiff
			
	end			
		
		





			bondsx=bondsx(0:bondcount-1)
			bondsy=bondsy(0:bondcount-1)
			bondsangle=bondsangle(0:bondcount-1)
			print,'Found ', bondcount,' Delauney bonds'


; ********		Calculating the bond-bond correlation function


			fcorr=fltarr(floor(sqrt(xs*xs+ys*ys)))
			ncorr=fltarr(floor(sqrt(xs*xs+ys*ys)))

			dimplot1=800
			

			if (Brutecorr eq 0) then begin
			;Trying to do it directly...

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
			endif


;********************

		if (Ccorr eq 0) then begin

; Will spawn an external C program which does the robcor procedure very fast...
; First, need to write out the bonds angle file...
		openw,u,'bonds.dat',/get_lun
		sampling=16; The decimation rate in getting the bonds

		printf,u,sampling

		printf,u,bondcount

		for iii=0L, bondcount-1 do begin
			printf,u,bondsx[iii],bondsy[iii],bondsangle[iii]
		endfor
		free_lun,u

		spawn, './robcor'

		robcor=read_ascii('robcor.dat');

		dcorr=transpose(robcor.field1[0,*])
		ncorr=transpose(robcor.field1[2,*])
		fcorr=transpose(robcor.field1[1,*])

		dimplot=300


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


		print,'The image translate correlation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
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



		endif

;********************


			triangulate, bondsx, bondsy, btriangles, boutermost, CONNECTIVITY = bedges
			bcutoff=5
			bangle=trigrid(bondsx,bondsy,bondsangle,btriangles, NX=256+2*bcutoff,NY=256+2*bcutoff)
			print, 'Bangle was cutoff by ', 100*2*bcutoff/256, ' percent'
			bangle=bangle(bcutoff:255+bcutoff,bcutoff:255+bcutoff)


;******************** 	Trying to redo the autocorrelation by the image-displacement technique
		if(translatecorr eq 0) then begin
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


			dimplot=300

			index=where(n1corr)
			restrict=where(index lt dimplot)
			index=index[restrict]
			window,2,xsize=dimplot+1,ysize=dimplot
			plot, index, f1corr[index]/n1corr[index], xtitle='pixels', ytitle='g6(r)',psym=0



			CorrelationLenghTest=abs(f1corr[index]/n1corr[index]-exp(-1.0))
			CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
			print,'The direct correlation length seed was ', Min_Subscript, ' .'

			A=[.5,Min_Subscript]

			dimfit=floor(maxcorr*sqrt(2))
			restrict2=where(index lt dimfit)
			index=index[restrict2]
			restrict2=where(index gt 5)
			index=index[restrict2]
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
			free_lun,u
			close,u


			Summary_of_Data[1,i]=A[1]


		endif


;			print,'Writing angle TIFF file'
;			write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'angle.tif',bytscl(bangle,MAX= !pi/3,MIN=0)




; Do gaussian smoothing


			smoothbangle=fltarr(256,256)
			smooth,bangle, smoothbangle, weights, howmuch

			print, 'Displaying smoothed bond angle...'
			showimage,smoothbangle*255*3/!pi,1,wbangle

			print,'Writing angle TIFF file'
			write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'smooth.tif',bytscl(smoothbangle,MAX= !pi/3,MIN=0)




		endif







; From here on the code is modified from Chris' Stripes Code


			xs=255;
			ys=255;

		if (ffcorr eq 0) then begin
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

		if (1 eq 0) then begin
			openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'robcor.dat',/get_lun
			n=size(IntensityArray)
			for LongIndex=0L,n(2)-1 do begin
				printf,u,LongIndex,IntensityArray[LongIndex], IntensityCountArray[LongIndex]
			endfor
			free_lun,u
			close,u
 		end
			;window,5,xsize=2*xs,ysize=2*ys
			;Slide_Image, bytscl(AutoCorrelation),SLIDE_WINDOW=MISCHA,group=g,FULL_WINDOW=8
			;the below line destroys the Slide_image
			;widget_control, g,/destroy


		endif

; From here on the code is taken from Anglecorr.pro, and the point is to do the direct
; correlation on the image itself rather than on the fourier transform


	if (directcorr eq 0) then begin
		pi=3.141592
		dimplot=256
		sampling=8 ; the size of the sampling square for direct correlations
		dimfit=64 ; the size of the picture over which correlation function is fitted
		fitcutoff=4

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

		restrict2=where((index lt dimfit) and (index gt fitcutoff))
		index=index[restrict2]
		yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct')

		yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)



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

		Summary_of_Data[0,i]=disc5

		if (keyword_set(wait)) then begin
			wait,1
		endif

		if (keyword_set(stay) eq 0) then begin
			widget_control,wbangle,/destroy
			widget_control,wbonds,/destroy
			
			wdelete,1
			;wdelete,2
			;wdelete,3
			;wdelete,4
		endif

		endfor ; main loop that reads each input file




		openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'summary.dat',/get_lun
		for LongIndex=0L, n_elements(fs)-1 do begin
			printf,u,Summary_of_Data[0,LongIndex],Summary_of_Data[1,LongIndex],Summary_of_Data[2,LongIndex],Summary_of_Data[3,LongIndex]," ",fs[LongIndex]
		endfor
		free_lun,u

end
