; seeall.pro			10-29-98	Eric Weeks
;
; see http://glinda.lrsm.upenn.edu/~weeks/idl
;    for more information and software updates

pro seeall,tr,dot=dot,izz=izz,dr=dr,short=short,noz=noz,id=id, $
	zdata=zdata
; currently works only for ubertracker data
; izz sets the 4th graph to be tr(izz,*)
; /dr to plot sqrt(dx*dx+dy*dy) instead of z-coord
; /short to show shortest first
; /noz to suppress 3rd graph
; id=# to only show a particular id#
; zdata gives zdata, overrides 'z'

if (not keyword_set(izz)) then izz=2
if (not keyword_set(zdata)) then zdata=-1

len=lentrk(tr)
on_error,1;			return to $MAIN

s=sort(len(0,*))
if (keyword_set(short)) then s=reverse(s)
x1=0.08
x2=0.48
y1=0.05
y2=0.45
dx=0.5
dy=0.5
if (not keyword_set(dr)) then dr=0

if (keyword_set(id)) then begin
	if (n_elements(id) eq 1) then begin
		w=where(len(1,*) eq id)
		s=w
	endif else begin
		nid=n_elements(id)
		len=len(*,1:nid)
		len(1,*)=id
		s=sort(len(0,*))
		if (keyword_set(short)) then s=reverse(s)
	endelse
endif

eness=n_elements(s)

if (not keyword_set(dot)) then begin
	for i=eness-1,0,-1 do begin
	  plotx,/x,/y,tr,len(1,s(i)),position=[x1,y1+dy,x2,y2+dy],title="trajectory"
		plotx,/x,tr,len(1,s(i)),position=[x1,y1,x2,y2],title="x-coord", $
			/noerase,/xsty
		plotx,/y,tr,len(1,s(i)),position=[x1+dx,y1+dy,x2+dx,y2+dy], $
			title="y-coord", /noerase,/xsty
		if (not keyword_set(noz)) then begin
			plotx,/z,tr,len(1,s(i)),position=[x1+dx,y1,x2+dx,y2], $
				title="z-coord", /noerase,/xsty,izz=izz,dr=dr,dz,dataz=zdata
		endif
		var=''
		prom='enter=continue, ctrl-d=end' + string(round(len(1,s(i))))
		prom=prom+" (duration"+string(round(len(0,s(i))))+") :"
		if (i gt 0) then begin
			read,prompt=prom,var
		endif else begin
			print,prom
		endelse
	endfor
endif else begin
	for i=eness-1,0,-1 do begin
		plotx,/x,/y,tr,len(1,s(i)),position=[x1,y1+dy,x2,y2+dy], $
			title="trajectory",psym=3
		plotx,/x,tr,len(1,s(i)),position=[x1,y1,x2,y2],title="x-coord", $
			/noerase,/xsty,psym=3
		plotx,/y,tr,len(1,s(i)),position=[x1+dx,y1+dy,x2+dx,y2+dy], $
			title="y-coord", /noerase,/xsty,psym=3
		if (not keyword_set(noz)) then begin
			plotx,/z,tr,len(1,s(i)),position=[x1+dx,y1,x2+dx,y2], $
				title="z-coord", /noerase,/xsty,psym=3,izz=izz,dr=dr, $
				dataz=zdata
		endif
		var=''
		prom='enter=continue, ctrl-d=end' + string(round(len(1,s(i))))
		prom=prom+" (duration"+string(round(len(0,s(i))))+") :"
		if (i gt 0) then begin
			read,prompt=prom,var
		endif else begin
			print,prom
		endelse
	endfor
endelse

return

end
