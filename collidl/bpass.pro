; ********** start of bpass.pro
;+
; NAME:
;     bpass
; PURPOSE:
;     Implements a real-space bandpass filter which suppress
;     pixel noise and slow-scale image variations while
;     retaining information of a characteristic size.
;
; CATEGORY:
;     Image Processing
; CALLING SEQUENCE:
;     res = dgfilter( image, lnoise, lobject )
; INPUTS:
;     image:   The two-dimensional array to be filtered.
;     lnoise: Characteristic lengthscale of noise in pixels.
;      Additive noise averaged over this length should
;      vanish. MAy assume any positive floating value.
;     lobject: A length in pixels somewhat larger than a typical
;      object. Must be an odd valued integer.
; OUTPUTS:
;     res: filtered image.
; PROCEDURE:
;     simple 'wavelet' convolution yields spatial bandpass filtering.
; NOTES:
; MODIFICATION HISTORY:
;     Written by David G. Grier, The University of Chicago, 2/93.
;     Greatly revised version DGG 5/95.
;     Added /field keyword JCC 12/95.
;     Revised & added 'stack','voxel' capability JCC 5/97.
;
;   This code 'bpass.pro' is copyright 1997, John C. Crocker and
;   David G. Grier.  It should be considered 'freeware'- and may be
;   distributed freely in its original form when properly attributed.
;-
function bpass, image, lnoise, lobject, field = field, noclip=noclip

nf = n_elements(image[0,0,*])
;on_error, 2          ; go to caller on error

b = float( lnoise )
w = round( lobject > (2. * b) )
N = 2*w + 1

r = (findgen( N ) - w)/(2. * b)
xpt = exp( -r^2 )
xpt = xpt / total(xpt)
factor = ( total(xpt^2) - 1/N )

gx = xpt
gy = transpose(gx)

bx = fltarr(N) - 1./N
by = transpose(bx)

if keyword_set( field ) then begin
    if N mod 4 eq 1 then indx = 2*indgen(w+1)$
         else indx = 1+ (2*indgen(w))
    gy = gy(indx)
    gy = gy/total(gy)

    nn = n_elements(indx)
    by = fltarr(nn) - 1./nn
endif

res = float(image)
; do x and y convolutions
for i = 0,nf-1 do begin
    g = convol( float(image[*,*,i]), gx )
    g = convol( g, gy )

    b = convol( float(image[*,*,i]), bx )
    b = convol( b, by )

    res(*,*,i) = g-b
endfor

if keyword_set( noclip ) then $
    return,res/factor $
else $
    return,res/factor > 0

end

;*********** end of bpass.pro












