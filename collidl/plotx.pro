;pro plotx,tra,nparticle,x=x,y=y,z=z,result,izz=izz, $
;  dr=dr,notisotropic=notisotropic,dataz=dataz,_extra=eee
;
; plotx			Eric Weeks, 9-98
;
; see http://glinda.lrsm.upenn.edu/~weeks/idl
;    for more information and software updates
;
; default:  it will plot nparticle_x(t)
; 
; if /y or /z is set, will plot y(t) or z(t)
; if two options are set (ie /x,/y) it will plot both (ie (x,y))
; if /z is used, it assumes tra(2,*) is the z-coordinate.  To change
;     this, set 'izz' to the column you wish to use.
; 'result' will return the trajectory that you select
; /dr puts sqrt(dx^2+dy^2) in the 2nd column (use both /dr /z to
;    plot dr(t))
; /notisotropic uses /xsty and /ysty to avoid scaling the axes improperly
; _extra gets passed to 'plot'
; 
; /notisotropic added 3-13-99 by ERW
; px_circ added 9-24-01 by ERW
;
;pro plotx,tra,nparticle,x=x,y=y,z=z,result,izz=izz, $
;  dr=dr,notisotropic=notisotropic,dataz=dataz,_extra=eee

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	sets the usersymbol (8) to be an open circle
;	this function returns the number 8 so you can
;	just say psym = px_circ( ... ) in the plot command	
;
function px_circ,radius = radius, thick = thick, fill = fill,$
	 dx = dx, dy = dy, left = left, right = right

if not keyword_set( radius ) then radius = 1.
if not keyword_set( thick ) then thick = 1.
if not keyword_set( dx ) then dx = 0.
if not keyword_set( dy ) then dy = 0.

t=findgen(37) * (!pi*2/36)

if keyword_set( right ) then t = t(0:18)
if keyword_set( left ) then t = [t(18:*),t(0)]

x = sin(t)*radius + dx
y = cos(t)*radius + dy

if keyword_set( fill ) then $
	usersym,x,y,/fill $
else	usersym,x,y,thick=thick

return,8	
end

;============================================================

pro plotx,tra,nparticle,x=x,y=y,z=z,result,izz=izz, $
  dr=dr,notisotropic=notisotropic,dataz=dataz,_extra=eee

if (not keyword_set(izz)) then izz=2
zflag=0
if (keyword_set(dataz)) then begin
	if (n_elements(dataz) gt 1) then begin
		zflag=1
	endif
endif

s=size(tra)
if (s(0) eq 2) then uber=1

if (keyword_set(uber)) then begin
	ndat=n_elements(tra(*,0))
	w=where(tra(ndat-1,*) eq nparticle)
	traj=tra(*,w)
	if (zflag eq 1) then begin
		traj(2,*)=dataz(w)
		izz=2
	endif
	thetime=traj(ndat-2,*)
endif else begin
	w=where(tra(0,nparticle,*) gt 0)
	traj=reform(tra(*,nparticle,w))
	nt=n_elements(tra(0,0,*))
	temp=indgen(nt)
	thetime=temp(w)
endelse

result=traj;	return trajectory, if wanted

if (keyword_set(dr)) then begin
	thelength=n_elements(traj(0,*))
	tempdx=traj(0:1,1:*)-traj(0:1,0:thelength-2)
	tempdx=total(tempdx*tempdx,1)
	traj(2,0)=0.0
	traj(2,1:*)=sqrt(tempdx)
endif

xset=keyword_set(x)
yset=keyword_set(y)
zset=keyword_set(z)
numset=xset+yset+zset
if (numset le 1) then begin
	if (yset) then begin
		plot,thetime,traj(1,*),/ynozero,_extra=eee
	endif else begin
		if (zset) then begin
			plot,thetime,traj(izz,*),/ynozero,_extra=eee
		endif else begin
			; DEFAULT if not /y or /z
			plot,thetime,traj(0,*),/ynozero,_extra=eee
		endelse
	endelse
endif else begin
	; plot XY, XZ, or YZ (no such thing as XYZ -- yet)
	if (xset and yset) then begin
		traj=traj(0:1,*)
	endif else begin
		if (xset) then begin
			traj=traj([0,izz],*)
		endif else begin
			traj=traj([1,izz],*)
		endelse
	endelse
	if (not keyword_set(notisotropic)) then begin
		xyzmin=fltarr(2)
		xyzmax=fltarr(2)
		xyzmax(0)=max(traj(0,*),min=themin)
		xyzmin(0)=themin
		xyzmax(1)=max(traj(1,*),min=themin)
		xyzmin(1)=themin
		cent=(xyzmin+xyzmax)*0.5
		delt=max(xyzmax-xyzmin)*0.5
		minx = cent(0)-delt
		miny = cent(1)-delt
		maxx = cent(0)+delt
		maxy = cent(1)+delt
		plot,traj(0,*),traj(1,*),/isotropic,              $
			xrange=[minx,maxx],yrange=[miny,maxy],/ynozero,_extra=eee
		oplot,[traj(0,0)],[traj(1,0)],psym=px_circ(/fill,radius=1.5)
		oplot,[traj(0,0)],[traj(1,0)],psym=px_circ(/fill),color=0
	endif else begin
		plot,traj(0,*),traj(1,*),/xsty,/ysty,/ynozero,_extra=eee
		oplot,[traj(0,0)],[traj(1,0)],psym=px_circ(/fill,radius=1.5)
		oplot,[traj(0,0)],[traj(1,0)],psym=px_circ(/fill),color=0
	endelse
endelse

end
