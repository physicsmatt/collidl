function read_nih,fname,first=first,last=last
;
;   reads 2 and 3 dimensional 8-bit TIFF files as produced by 
;   NIH Image.  It tries to detect and fix 'endian' problems.
;   Also allows user to read a subset of frames, which is useful 
;   for really big files.
;
f = findfile(fname)	; check to see if the file is actually *there*
if f(0) eq '' then message,'File: "'+fname+'" not found!'

cookie = intarr(1)
head = intarr(383)

; check for endian independent cookie
openr,1,fname
readu,1,cookie
if (cookie(0) ne 19789 and cookie(0) ne 18761) then begin
	close,1
	message,'"'+fname+'" is not a valid NIH Image tiff file!'
endif
readu,1,head

; fix endian
if head(2) eq 2048 then head = swap_endian(head)
; check endian
if head(2) ne 8 then begin
	close,1
	message,'"'+fname+'" is not a valid NIH Image tiff file!'
endif

tmp = FSTAT(1)

x = head(14) + 0L
y = head(20) + 0L
z = head(257) + 0L
if z eq 0 then z = 1L			; fix for 2d nih tiff images

; watch out for EOF errors- is the file big enough?
sz = (x*y*z) + 768L
if sz gt tmp.size then $
	message,'"'+fname+'" is not a valid NIH Image tiff file!'

; do Peter's browsing thing
s = 0
if keyword_set( first ) then begin
	if (first le z-1) and (first ge 0) then s=first $
		else begin
			close,1
			message,'invalid first position!!'
		endelse
endif
if keyword_set( last ) then begin
	if (last ge s) and (last le z-1) then z=last $
		else begin
			close,1
			message,'invalid last position!'
		endelse
endif

if (s gt 0) then begin
	z = z - s + 1
	offset = x * y * s + 768L
	point_lun,1,offset
endif

; read in the data
im = bytarr(x,y,z)
readu,1,im
close,1

; fix the data up for IDL conventions
for i = 0,z-1 do im(*,*,i) = 255b-rotate(im(*,*,i),7)

return,im

end





