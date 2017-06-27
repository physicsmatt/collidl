;
; dead simple routine to analyse 2+1d 'stacks' into trackable files
; whammy = [lnoise,extent1,extent2,masscut]
;
;  ppretrack -- Peter's version (begun 7/8/97)
;  jpretrack -- John's version (begun 7/8/98)
;
; 3-13-99: whammy=[lnoise,extent1,extent2,masscut,sep] is optional
pro ejpretrack,name,whammy,invert=invert,field=field,first=first,fskip=fskip,$
	gdf=gdf

if n_elements(whammy) lt 4 then $
	message,'usage: pretrack,fname,[lnoise,extent1,extent2,masscut],invert=invert,field=field'

lnoise = whammy(0)
extent1 = whammy(1)
extent2 = whammy(2)
masscut = whammy(3)
s=size(whammy)
if (s(1) eq 5) then sep=whammy(4)

filen = findfile(name)
if filen(0) eq '' then message,"No file '"+name+"' found"
nfiles = n_elements(filen)

;  fskip is the frame/field # increment during time lapse video
if not keyword_set(fskip) then fskip=1

for j =0,nfiles-1 do begin

	print,'reading image stack: '+filen(j)
	if keyword_set(gdf) then stk = read_gdf(filen(j)) else $
		stk = read_nih(filen(j)) 

	if keyword_set(invert) then stk = 255b-stk

	ns = n_elements(stk(0,0,*))
	if ns eq 1 then message,'jpretrack is for analysing movies, not frames'

	ss=size(stk(*,*,0))
	sx = ss(1)

	if keyword_set(first) then ns = 1  ;handy for a quick looksee...

	res = fltarr(6)
	if keyword_set(field) then begin
	   for i = 0,ns-1 do begin
		print,'processing fields of frame'+strcompress(i)+' out of'+$
			strcompress(ns)+'....'
		im = bpass(fieldof(stk(*,*,i),/even),lnoise,extent1,/field)
if (s(1) eq 4) then $
f = feature(im(2*extent1:sx-2*extent1,*),extent2,masscut = masscut,/field) $
else $
f = feature(im(2*extent1:sx-2*extent1,*),extent2,sep,masscut = masscut,/field)
		if f(0) ne -1 then begin
			f(0,*,*) = f(0,*,*)+2*extent1
			nf = n_elements(f(0,*))
			res = [[res],[f,fltarr(1,nf)+2*i*fskip]]
		endif
		im = bpass(fieldof(stk(*,*,i),/odd),lnoise,extent1,/field)
if (s(1) eq 4) then $
f = feature(im(2*extent1:sx-2*extent1,*),extent2,masscut = masscut,/field) $
else $
f = feature(im(2*extent1:sx-2*extent1,*),extent2,sep,masscut = masscut,/field)
		if f(0) ne -1 then begin
			f(0,*,*) = f(0,*,*)+2*extent1
			f(1,*) = f(1,*)+1
			nf = n_elements(f(0,*))
			res = [[res],[f,fltarr(1,nf)+2*i*fskip+1]]
		endif
	   endfor
	endif else begin
	   for i = 0,ns-1 do begin
		print,'processing frame'+strcompress(i)+' out of'+$
			strcompress(ns)+'....'
		im = bpass(stk(*,*,i),lnoise,extent1)
		f = feature(im,extent2,masscut = masscut)
		nf = n_elements(f(0,*))
		if (f(0) ne -1) then res = [[res],[f,fltarr(1,nf)+i*fskip]]
	   endfor
	endelse

	wname = 'xys.'+filen(j)
	print,'writing output file to:' + wname
	write_gdf,res(*,1:*),wname

endfor

end
