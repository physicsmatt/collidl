
FUNCTION sint2d, x, y, f

;Get size of input array.
stat = SIZE( f )
xsize = stat[ 1 ]
ysize = stat[ 2 ]

;Half-size of the kernel.
delta = 10

;Compute integer and fractional parts of input position.
ix = FIX( x )
fx = x - ix
iy = FIX( y )
fy = y - iy

yoff = MIN( [ iy, delta ] )
ly   = iy - yoff
hy   = iy + yoff
IF hy GE ysize THEN hy = ysize-1
ny   = hy - ly + 1

vals = FLTARR( ny )
FOR j = 0, ny-1 DO BEGIN
   xoff = MIN( [ ix, delta ] )
   lx = ix - xoff
   hx = ix + xoff
   IF hx GE xsize THEN hx = xsize-1
   r1 = f[ lx : hx, ly+j ]
   x1 = fx + xoff
   vals[j] = sint( x1, r1 )
ENDFOR
RETURN, sint( fy+yoff, vals )

END



FUNCTION sint, x, f

dampfac = 3.25
ksize   = 21

nx = N_ELEMENTS( x )
nf = N_ELEMENTS( f )

ix = FIX( x )
fx = x - ix
z = WHERE( fx EQ 0, countz )
i = WHERE( fx NE 0, counti )

r = x * 0

IF countz NE 0 THEN BEGIN
   ;There are integral values of the independent variable. Select the function
   ;values directly for these points.
   r[ z ] = f[ ix[z] ]
ENDIF

IF counti NE 0 THEN BEGIN
   ;Use sinc interpolation for the points having fractional values.
   FOR point=0, counti-1 DO BEGIN
      xkernel = ( FINDGEN( ksize ) - 10 ) - fx[ i[ point ] ]
      u1 = xkernel / dampfac
      u2 = !pi * xkernel
      sinc = EXP( -( u1*u1 ) ) * SIN( u2 ) / u2
      lobe = ( INDGEN( ksize ) - 10 ) + ix[ i[point] ]
      vals = FLTARR( ksize )
      w = WHERE( lobe LT 0, count )
      IF count NE 0 THEN vals[w] = f[0]
      w = WHERE( lobe GE 0 AND lobe LT nf, count )
      IF count NE 0 THEN vals[w] = f[ lobe[w] ]
      w = WHERE( lobe GE nf, count )
      IF count NE 0 THEN vals[w] = f[ nf-1 ]
      r[ i[ point ] ] = TOTAL( sinc * vals )
   ENDFOR
ENDIF

RETURN, r

END



PRO findmax, in_x, in_y, in_f, out_xmax, out_ymax, out_fmax, $
             DELTA=in_delta, EPS=in_eps, GOLD=in_gold


IF KEYWORD_SET( in_delta  ) THEN delta = in_delta ELSE delta = 1.0
IF KEYWORD_SET( in_eps ) THEN eps = in_eps ELSE eps = 1.0E-1
IF KEYWORD_SET( in_gold ) THEN gold = in_gold ELSE gold = 1.0E-4

;Set the starting values.
i = in_x
j = in_y
h = delta + gold
n = 0
REPEAT BEGIN
   ;Set two test points along the x-axis.
   x1 = i - h
   x2 = i + h

   ;Set two test points along the y-axis.
   y1 = j - h
   y2 = j + h

   ;Compute the function at the offset locations.
   f1 = sint2d( x2,  j, in_f )
   f2 = sint2d(  i, y2, in_f )
   f3 = sint2d( x1,  j, in_f )
   f4 = sint2d(  i, y1, in_f )

   ;Shift the x location of the next maximum.  If symmetric, no shift.
   IF f1 GT f3 THEN i = i + h
   IF f3 GT f1 THEN i = i - h

   ;Shift the y location of the next maximum.  If symmetric, no shift.
   IF f2 GT f4 THEN j = j + h
   IF f4 GT f2 THEN j = j - h

   ;Halve the search interval.
   h = 0.5 * h
   n = n + 1
   ;Test for convergence.
ENDREP UNTIL h LT eps

;MESSAGE, 'Number of iterations ' + STRING( n ), /INFO

;Store the results into the output parameters.
out_xmax = i
out_ymax = j
out_fmax = sint2d( i, j, in_f )

END


pro mylocmax, img, eps ,mask=m, where=w, ix=ix, iy=iy, sort=srt, $
          values=v, value_image=vimg, noedge=noed, help=hlp

        fuzz = 1.e-8            ; Ignore values below this.

		n=size(img)

		xs=n[1]-1
		ys=n[2]-1

        ;-----  Shift four ways  -----
        dx1 = shift(img,1,0)
        dx1[*,0]=0
        dx2 = shift(img,-1,0)
        dx2[*,ys]=0
        dy1 = shift(img,0,1)
        dy1[0,*]=0
        dy2 = shift(img,0,-1)
		dy2[xs,*]=0
        ;------  compare each pixel to 4 surrounding values  -------
        m = (img ge dx1) and (img ge dx2) and (img ge dy1) and (img ge dy2)
        if keyword_set(noed) then imgfrm, m, 0
        ;------  number of local maxima  --------

        for ii=0L,n_elements(m)-1 do begin
        	y=ii/(xs+1)
        	x=ii-y*(xs+1)
			;print, "Mylocmax : ",x,y

			if (m[x,y] eq 1) then begin
				xinit=x-eps>0
				xend=x+eps<xs-1
				yinit=y-eps>0
				yend=y+eps<ys-1

				m[xinit:xend,yinit:yend]=0
				m[x,y]=1
			endif
        endfor

        w = where(m eq 1, count)        ; Find local maxima.

        fzz = img(w)                    ; Pull out values.
        wfzz = where(fzz lt fuzz, c)    ; Look for values below fuzz.
        if c gt 0 then begin            ; Found any?
          m(w(wfzz)) = 0                ;   Yes, zap them.
          w = where(m eq 1, count)      ;   Now try again for local maxima.
        endif
        ;------  if any continue  ------
        if count gt 0 then begin
          if n_elements(vimg) eq 0 then begin   ; Pick off values at maxima.
            v = img(w)
          endif else begin
            v = vimg(w)
          endelse
          if keyword_set(srt) then begin        ; Sort?
            is= reverse(sort(v))                ; yes.
            v = v(is)
            w = w(is)
          endif
          ;one2two, w, img, ix, iy               ; Convert to 2-d indices.
        endif

    return
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

pro main,data
	fs=dialog_pickfile(get_path=ps,/multiple_files)


		if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif' then readlist,fs(0),fs,path=ps else fs=[fs]

		for i=0,n_elements(fs)-1 do begin

			print,'Now working on : ',fs(i)

			data1=read_tiff(fs(i))

			n=size(data1)
			xs=n[1]-1
			ys=n[2]-1

			data1=data1(0:xs,0:ys)
			help,data1

			window,0,xsize=xs+100,ysize=ys+100
			img=[[[data1]],[[data1]],[[data1]]]
			tvscl,img,50,50,true=3
			mylocmax,data1,2,where=w,values=v


			goodx=[0]
			goody=[0]
			for j=0,n_elements(w)-1 do begin
				y=w[j]/(xs+1)
				x=w[j]-y*(xs+1)
				if (x ge 3) and (x le xs-3) and (y ge 3) and (y le ys-3) then begin
					xmax=x
					ymax=y
					findmax,x,y,data1,xmax,ymax,val
					goodx=[goodx,xmax]
					goody=[goody,ymax]
					;print,"Old x,y : ",x,", ",y,"  New x,y : ", xmax, ", ",ymax
					;img=draw_cross(img,fix(x),fix(y))
				endif





			endfor


			Print,"Starting Triangulation"


			nvertices=n_elements(goodx)-1
			goodx=goodx(1:nvertices)
			goody=goody(1:nvertices)
			triangulate, goodx, goody, CONNECTIVITY = edges

			disc=fltarr(nvertices,2)
			for i1=0,nvertices-1 do begin
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

			window,1,xsize=xs+100,ysize=ys+100
			tvscl,img,50,50,true=3
			imgtiff=bytarr(3,xs+1,ys+1)
			for i2=0,xs do begin
				for j2=0,ys do begin
					imgtiff[0,i2,j2]=img[i2,j2,0]
					imgtiff[1,i2,j2]=img[i2,j2,1]
					imgtiff[2,i2,j2]=img[i2,j2,2]
				endfor
			endfor
			write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'color.tif',imgtiff

		endfor


end