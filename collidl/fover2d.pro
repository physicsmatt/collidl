;function fover2d,image,points,radius=radius,big=big,nodot=nodot,circle=circle $
;   field=field
;
; see http://glinda.lrsm.upenn.edu/~weeks/idl
;    for more information and software updates
;+
; NAME:
;		fover2d
; PURPOSE:
;		Overlay points onto a 2d image.
; CALLING SEQUENCE:
;		newimage=foverlay,image,points
; INPUTS:
;		image:	2D data set onto which the points should be overlaid
;		points: (2,npoints) array of overlay points
; OUTPUTS:
;		returns an image ready for 'tv'.  The color palette
;		is adjusted to be the foverlay palette.  Redraws screen.
; KEYWORDS:
;		radius sets size of circles
;       /big doubles the picture in size in each direction
;       /nodot turns off the black dot at the center of each circle
;       /circle draws a circle around each point, rather than a disk
; PROCEDURE:
;		Rescale the image to leave some color table indices free.
;		Make the rest of the color table into a grey ramp and
;		turn the 3 highest indices into 3 new colors.  Plot a
;       disk at each particle position.
; MODIFICATION HISTORY:
;       Lookup table code taken from David G. Grier's f_overlay
;       routine.  Mostly written by Eric Weeks in summer '98.
;       Ability to handle movies added 2-20-99.
;       Sped up 4-99
;-

;First a utility function....

; circarray.pro,  started 6-22-98 by ERW
;   shortened into fo_circ 2-20-99 by ERW
;
function fo_circ,array,radius=radius,center=center,circle=circle
; returns an array, size equal to "array" variable, with value 1
; everywhere within a circle of diameter of the array size.  Circle
; is at center of array.
;
; 'radius' sets a radius different from the default radius (half the array size)
; 'center' overrides the default center of the circle

s=size(array)
result=array*0

sx=s(1) & sy=s(2)
minsize = (sx < sy)
cx=(sx-1)*0.5 & cy=(sy-1)*0.5
if keyword_set(center) then begin
	cx=center(0)
	cy=center(1)
endif
if keyword_set(radius) then begin
	irad=radius
endif else begin
	irad=minsize/2
endelse
jrad=(irad-1.2)*(irad-1.2)
irad = irad*irad
if (keyword_set(circle)) then begin
	for j=0,sy-1 do begin
		rad2 = (cy-j)*(cy-j)
		for k=0,sx-1 do begin
			rad3 = rad2 + (cx-k)*(cx-k)
			result(k,j) = ((rad3 le irad) and (rad3 ge jrad))
		endfor
	endfor
endif else begin
	for j=0,sy-1 do begin
		rad2 = (cy-j)*(cy-j)
		for k=0,sx-1 do begin
			rad3 = rad2 + (cx-k)*(cx-k)
			result(k,j) = (rad3 le irad)
		endfor
	endfor
endelse

return,result
end





function fover2d,image,points,radius=radius,big=big,nodot=nodot,   $
	circle=circle,field=field

if not keyword_set(radius) then radius=5
if (not keyword_set(nodot)) then nodot=0
if not keyword_set(circle) then begin
	circle=0 
endif else begin
	nodot=1
endelse

nc = !d.table_size
if nc eq 0 then message,'Device has static color tables: cannot adjust'
red = byte(indgen(nc))
green = red
blue = red
green(nc-1) = 0b
;blue(nc-1)=128b
tvlct,red,green,blue


;output = byte ( ((image*0.98) mod (nc-5)) + floor(image / (nc-5))*256 )
output = byte ( ((image*0.98) mod (nc-5)) )
x=reform(points(0,*))
y=reform(points(1,*))
s = size(output)
if (s(0) eq 3) then begin
	; 3-D array
	nel=n_elements(points(*,0))
	t = reform(points(nel-1,*))
	tmax=max(t,min=tmin)
	nzz=n_elements(output(0,0,*))
	if (keyword_set(field)) then begin
		w=where(t mod 2 eq 0)
		x=x(w) & y=y(w) & t=t(w)/2
	endif
endif
if (keyword_set(big)) then begin
	if (s(0) eq 2) then begin
		output=rebin(output,s(1)*2,s(2)*2);        2-D array
	endif else begin
		output=rebin(output,s(1)*2,s(2)*2,s(3));   3-D array
	endelse
	x = x * 2 & y = y * 2
endif
x2=n_elements(output(*,0,0));		3rd array index just for safety
y2=n_elements(output(0,*,0))
xmax=max(x,min=xmin)
ymax=max(y,min=ymin)
if (xmin lt 0 or ymin lt 0 or xmax ge x2 or ymax ge y2) then begin
	message,'points are outside picture',/inf
	w=where((x ge 0) and (x lt x2))
	x=x(w) & y=y(w)
	w=where((y ge 0) and (y lt y2))
	x=x(w) & y=y(w)
endif

stdblob=fo_circ(bytarr(radius*2+1,radius*2+1),circle=circle)

for i = 0L,n_elements(x)-1L do begin
	minx = long((x(i)-radius) > 1)
	miny = long((y(i)-radius) > 1)
	maxx = long((x(i)+radius) < (x2-1))
	maxy = long((y(i)+radius) < (y2-1))
	tempx1=minx-long((x(i)-radius))
	tempy1=miny-long((y(i)-radius))
	tempx2=2*radius-(long(x(i)+radius)-maxx)
	tempy2=2*radius-(long(y(i)+radius)-maxy)
	blob=stdblob(tempx1:tempx2,tempy1:tempy2)
	if (maxx-minx) gt (tempx2-tempx1) then maxx=minx+tempx2-tempx1
	if (maxy-miny) gt (tempy2-tempy1) then maxy=miny+tempy2-tempy1
	;foo=[x(i)-minx,y(i)-miny]
	;blob=fo_circ(output[minx:maxx,miny:maxy],  $
	;		center=foo,radius=radius,circle=circle)
	blob = (blob < 1) * (nc - 1b)
	if (s(0) eq 2) then begin
		; 2-D image
		output(minx:maxx,miny:maxy) =			$
			output(minx:maxx,miny:maxy) > blob
		if (nodot eq 0) then begin
			if (x(i) gt 1) and (x(i) lt (x2-1)) then $
				output(x(i)-1:x(i)+1,y(i))=0b
			if (y(i) gt 1) and (y(i) lt (y2-1)) then $
				output(x(i),y(i)-1:y(i)+1)=0b
		endif
	endif else begin
		; 3-D image
		tnow=t(i)-tmin
		if (tnow lt nzz) then begin
			output(minx:maxx,miny:maxy,tnow) =			$
				output(minx:maxx,miny:maxy,tnow) > blob
			if (nodot eq 0) then begin
				if (x(i) gt 1) and (x(i) lt (x2-1)) then $
					output(x(i)-1:x(i)+1,y(i),tnow)=0b
				if (y(i) gt 1) and (y(i) lt (y2-1)) then $
					output(x(i),y(i)-1:y(i)+1,tnow)=0b
			endif
		endif
	endelse
endfor

tv,output
return, output
end


