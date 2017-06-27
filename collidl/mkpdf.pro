; started 7-22-98 by Eric Weeks
;
; see http://glinda.lrsm.upenn.edu/~weeks/idl
;    for more information and software updates
;
; this has evolved from s28--->mkpdf2--->updf-->mkpdf
;
; dim option added 9-15-98
; ubertracker'ized on 9-17-98
; fixed bug when dim=1 on 3-4-99

function mkpdf,tr,dt,dim=dim,interpolated=interpolated,careful=careful
; tr is tracked data
; returns an array, dim x N
; result(0,*) is dx, result(1,*) is dy, result(2,*) is dz if applicable
; works for any number of dimensions
; /interpolated if you are using pre-interpolated data (no gaps in time)
; /careful if you want to ensure non-overlapping dt's
;
; tends to be a memory hog, unfortunately -- you get essentially
; two bonus copies of 'tr' in the following line:
;       dx=tr-shift(tr,0,i)

if (not keyword_set(dt)) then dt=1
if (not keyword_set(dim)) then dim=2

s=size(tr)
if (s(0) eq 2) then uber=1

prt = fltarr(1,dim)

if (keyword_set(uber)) then begin
	; ====== UBERTRACKER ======
	ndat=n_elements(tr(*,0))
	totaltime=max(tr(ndat-2,*))
	mindt=1L
	if (keyword_set(interpolated)) then mindt=dt
	for i=long(dt),long(mindt),-1L do begin
		dx=tr-shift(tr,0,i)
		w=where((dx(ndat-1,*) eq 0),ngood);		particle ID#
		if (ngood ge 1) then begin
			mxdt=max(dx(ndat-2,w),min=mndt)
			if ((dt ge mndt) and (dt le mxdt)) then begin
				w2=where(dx(ndat-2,w) eq dt,ngood2);	time stamp
				if (ngood2 ge 1) then begin
					if (not keyword_set(careful)) then begin
						info = transpose(reform(dx(0:dim-1,w(w2))))
						if (dim eq 1) then info=reform(info)
						prt=[prt,info]
					endif else begin
						; let's be /careful
						t0=tr(ndat-2,w(w2)) mod dt
						id0=tr(ndat-1,w(w2))
						u=[-1,uniq(id0)]+1
						t00=t0(u)
						if (dim eq 1) then begin
							for j=0,n_elements(u)-2 do begin
								;w3=where(id0 eq id0(u(j)),nw3)
								if (u(j+1)-1 eq u(j)) then $
									w3=u(j) else w3=lindgen(u(j+1)-1-u(j))+u(j)
								w4=where(t0(w3) eq t00(j),nw4)
								info=transpose(reform(dx(0,w2(w3(w4)))))
								info=reform(info)
								prt=[prt,info]
							endfor
						endif else begin
							for j=0,n_elements(u)-2 do begin
								;w3=where(id0 eq id0(u(j)),nw3)
								if (u(j+1)-1 eq u(j)) then $
									w3=u(j) else w3=lindgen(u(j+1)-1-u(j))+u(j)
								w4=where(t0(w3) eq t00(j),nw4)
								info=transpose(reform(dx(0:dim-1,w2(w3(w4)))))
								prt=[prt,info]
							endfor
						endelse
					endelse
				endif
			endif 
			if (dt gt mxdt) then i=-1L
		endif
	endfor
endif else begin
	; ====== OLD TRACKER ======
	totaltime=n_elements(tr(0,0,*))
	if (dim eq 2) then begin
		for i=0,totaltime-dt-1 do begin
			w1=where((tr(0,*,i) gt 0) and (tr(0,*,i+dt) gt 0))
			dx=reform(tr(0,w1,i)-tr(0,w1,i+dt))
			dy=reform(tr(1,w1,i)-tr(1,w1,i+dt))
			info=[[dx],[dy]]
			prt=[prt,info]
		endfor
	endif else begin
		for i=0,totaltime-dt-1 do begin
			w1=where((tr(0,*,i) gt 0) and (tr(0,*,i+dt) gt 0))
			dx=reform(tr(0,w1,i)-tr(0,w1,i+dt))
			dy=reform(tr(1,w1,i)-tr(1,w1,i+dt))
			dz=reform(tr(2,w1,i)-tr(2,w1,i+dt))
			info=[[dx],[dy],[dz]]
			prt=[prt,info]
		endfor
	endelse
endelse


if (n_elements(prt(*,0)) eq 1) then begin
	return,-1
endif

dx=0
info=0
w=0
w2=0
return,transpose(prt(1:*,*))
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


