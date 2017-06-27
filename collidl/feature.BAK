;+
; NAME:
;     Feature
; PURPOSE:
;     Finds and measures roughly circular 'features' within
;     an image.
; CATEGORY:
;     Image Processing
; CALLING SEQUENCE:
;     f = feature( image, diameter [, separation, masscut = masscut,
;      min = min, iterate = iterate, /field, /quiet ] )
; INPUTS:
;     image:   (nx,ny) array which presumably contains some
;      features worth finding
;     diameter: a parameter which should be a little greater than
;      the diameter of the largest features in the image.
;      Diameter MUST BE ODD valued.
;     separation: an optional parameter which specifies the
;      minimum allowable separation between feature
;      centers. The default value is diameter+1.
;     masscut:Setting this parameter saves runtime by reducing the
;      runtime wasted on low mass 'noise' features.
;     min:     Set this optional parameter to the minimum allowed
;      value for the peak brightness of a feature. Useful
;      for limiting the number of spurious features in
;      noisy images.
;     field:   Set this keyword if image is actually just one field
;      of an interlaced (e.g. video) image. All the masks
;      will then be constructed with a 2:1 aspect ratio.
;     quiet:   Supress printing of informational messages.
;     iterate: if the refined centroid position is too far from
;      the initial estimate, iteratively recalc. the centroid
;      using the last cetroid to position the mask.  This
;      can be useful for really noisy data, or data with
;      flat (e.g. saturated) peaks.  Use with caution- it
;      may 'climb' hills and give you multiple hits.
; OUTPUTS:
;     f(0,*):  this contains the x centroid positions, in pixels.
;     f(1,*): this contains the y centroid positions, in pixels.
;     f(2,*): this contains the integrated brightness of the
;      features.
;     f(3,*): this contains the square of the radius of gyration
;      of the features.
;     f(4,*): this contains the eccentricity, which should be
;      zero for circularly symmetric features and order
;      one for very elongated images.
; SIDE EFFECTS:
;     Displays the number of features found on the screen.
; RESTRICTIONS:
;     To work properly, the image must consist of bright,
;     circularly symmetric regions on a roughly zero-valued
;     background. To find dark features, the image should be
;     inverted and the background subtracted. If the image
;     contains a large amount of high spatial frequency noise,
;     performance will be improved by first filtering the image.
;     BPASS will remove high spatial frequency noise, and
;     subtract the image background and thus provides a good
;     complement to using this program. Individual features
;     should NOT overlap.
; PROCEDURE:
;     First, identify the positions of all the local maxima in
;     the image ( defined in a circular neighborhood with diameter
;     equal to 'diameter' ). Around each of these maxima, place a
;     circular mask, of diameter 'diameter', and calculate the x & y
;     centroids, the total of all the pixel values, and the radius
;     of gyration and the 'eccentricity' of the pixel values within
;     that mask. If the initial local maximum is found to be more
;     than 0.5 pixels from the centroid and iterate is set, the mask
;     is moved and the data are re-calculated. This is useful for
;     noisy data. If the restrictions above are adhered to, and the
;     features are more than about 5 pixels across, the resulting x
;     and y values will have errors of order 0.1 pixels for
;     reasonably noise free images.
;
; *********        READ THE FOLLOWING IMPORTANT CAVEAT!        **********
;     'feature' is capable of finding image features with sub-pixel
;     accuracy, but only if used correctly- that is, if the
;     background is subtracted off properly and the centroid mask
;     is larger than the feature, so that clipping does not occur.
;     It is an EXCELLENT idea when working with new data to plot
;     a histogram of the x-positions mod 1, that is, of the
;     fractional part of x in pixels.  If the resulting histogram
;     is flat, then you're ok, if its strongly peaked, then you're
;     doing something wrong- but probably still getting 'nearest
;     pixel' accuracy.
;
;     For a more quantitative treatment of sub-pixel position
;     resolution see:
;     J.C. Crocker and D.G. Grier, J. Colloid Interface Sci.
;     *179*, 298 (1996).
;
; MODIFICATION HISTORY:
;     This code is inspired by feature_stats2 written by
;      David G. Grier, U of Chicago,         1992.
;     Written by John C. Crocker, U of Chicago, optimizing
;      runtime and measurement error,         10/93.
;     Added field keyword,                  4/94.
;     Added eccentricity parameter,              5/95.
;     Added quiet keyword            12/95.
;     Added iteration, fixed up the radius/diameter fiasco and
;     did some debugging which improves non-centroid data.  4/96.
;
;   This code 'feature.pro' is copyright 1997, John C. Crocker and
;   David G. Grier.  It should be considered 'freeware'- and may be
;   distributed freely in its original form when properly attributed.
;-
;
;   produce a parabolic mask
;
function rsqd,w,h

if n_params() eq 1 then h = w
r2 = fltarr(w,h,/nozero)
xc = float(w-1) / 2.
yc = float(h-1) / 2.
x = (findgen(w) - xc)
x = x^2
y = (findgen(h)- yc)
y = y^2

for j = 0, h-1 do begin
    r2(*,j) = x + y(j)
endfor

return,r2
end
;
;   produce a 'theta' mask
;
function thetarr,w

theta = fltarr(w,w,/nozero)
xc = float(w-1) / 2.
yc = float(w-1) / 2.

x = (findgen(w) - xc)
x(xc) = 1e-5
y = (findgen(w) - yc)

for j = 0, w-1 do begin
    theta(*,j) = atan( y(j),x )
endfor

return,theta
end
;
;   This routine returns the even or odd field of an image
;
function fieldof,array,odd=odd,even=even

sz = size(array)
if sz(0) ne 2 then message,"Argument must be a two-dimensional array!"
if keyword_set(odd) then f=1 else f=0

ny2 = fix( (sz(2)+(1-f))/2 )
rows = indgen(ny2)*2 + f
return,array(*,rows)

end
;
;   barrel "shifts" a floating point arr by a fractional pixel amount,
;     by using a 'lego' interpolation technique.
;
function fracshift,im,shiftx,shifty

ipx = fix( shiftx )
ipy = fix( shifty )
fpx = shiftx - ipx
fpy = shifty - ipy
if fpx lt 0 then begin
    fpx=fpx+1 & ipx=ipx-1
endif
if fpy lt 0 then begin
    fpy=fpy+1 & ipy=ipy-1
endif

image = im

imagex  = shift( image,ipx+1,ipy   )
imagey  = shift( image,ipx  ,ipy+1 )
imagexy = shift( image,ipx+1,ipy+1 )
image   = shift( image,ipx  ,ipy   )

res   = ( (1. - fpx) * (1. - fpy) * image   ) + $
    ( (     fpx) * (1. - fpy) * imagex  ) + $
    ( (1. - fpx) * (     fpy) * imagey  ) + $
    ( (     fpx) * (     fpy) * imagexy )

return,res
end
;
;   John's version of local_max2, which supports the field keyword
;   and is otherwise identical.
;
function lmx, image, sep, min = min, field = field

range = fix(sep/2)
a = bytscl(image)
w = round( 2 * range + 1 )  ; width of sample region
s = rsqd( w )      ; sample region is circular
good = where( s le range^2 )
mask = bytarr( w, w )
mask(good) = 1b
yrange = range
if keyword_set( field ) then begin
    mask = fieldof( mask, /even )
    yrange = fix(range/2.) +1
endif

b = dilate( a, mask, /gray )    ; find local maxima in given range

          ; but don't include pixels from the
          ; background which will be too dim
if not keyword_set( min ) then begin
    h = histogram( a )
    for i = 1, n_elements(h) - 1 do $
       h(i) = h(i) + h(i-1)
    h = float( h ) / max( h )
    min = 0
    while h(min) lt 0.64 do $
       min = min + 1
    min = min + 1
    endif

r = where( a eq b and a ge min )

          ; Discard maxima within range of the edge
sz = size( a )
nx = sz(1) & ny = sz(2)
x = r mod nx & y = r / nx
x0 = x - range & x1 = x + range
y0 = y - yrange & y1 = y + yrange
good = where( x0 ge 0 and x1 lt nx and y0 ge 0 and y1 lt ny,ngood )
if ngood lt 1 then $
    return,[-1]
r = r(good)
x = x(good) & y = y(good)
x0 = x0(good) & x1 = x1(good)
y0 = y0(good) & y1 = y1(good)
          ; There may be some features which get
          ; found twice or which have flat peaks
          ; and thus produce multiple hits.  Find
          ; and clear such spurious points.
c = 0b * a
c(r) = a(r)
center = w * yrange + range
for i = 0D, n_elements(r) - 1D do begin
    b = c(x0(i):x1(i),y0(i):y1(i))

    b =  b * mask   ; look only in circular region
    m = max( b, location )
    if location ne center then $
       c(x(i),y(i)) = 0b
    endfor
          ; Ideally, the above routine would shrink
          ; clusters of points down to their center.
          ; As written, this will leave the lower
          ; right (?) pixel of a cluster.

r = where( c ne 0 )     ; What's left are valid maxima.

return,r         ; return their locations

end
;
;   John's version of DGG's feature_stats2.
;   which: a) avoids some unnecessary computation (convolutions)
;     b) uses fractional shift techniques to reduce pixel bias in m and Rg
;     c) has the field keyword
;
function feature, image, extent, sep,  min=min, masscut = masscut, field = field,$
     quiet = quiet,iterate = iterate

extent = fix(extent)
if (extent mod 2) eq 0 then begin
    message,'Requires an odd extent.  Adding 1...',/inf
    extent = extent + 1
    endif

sz = size( image )
nx = sz(1)
ny = sz(2)
if n_params() eq 2 then sep = extent+1

;   Put a border around the image to prevent mask out-of-bounds
a = fltarr( nx + extent, ny + extent )
a(extent/2:(extent/2)+nx-1,extent/2:(extent/2)+ny-1) = float( image )
nx = nx + extent

;   Finding local maxima
if keyword_set( field ) then $
    if not keyword_set( min ) then loc = lmx(a,sep,/field) else $
       loc=lmx(a,sep,min=min,/field) $
else $
    if not keyword_set( min ) then loc = lmx(a,sep) else $
       loc=lmx(a,sep,min=min)

if loc(0) eq -1 then return,-1
x  = float( loc mod nx )
y  = float( loc / nx )

nmax=n_elements(loc)
xl = x - fix(extent/2)
xh = xl + extent -1
m  = fltarr(nmax)

;   Set up some masks
rsq = rsqd( extent )
t = thetarr( extent )
mask = rsq le (float(extent)/2.)^2
mask2 = make_array( extent , extent , /float, /index ) mod (extent ) + 1.
mask2 = mask2 * mask
mask3= (rsq * mask) + (1./6.)
cen = float(extent-1)/2.
cmask = cos(2*t) * mask
smask = sin(2*t) * mask
cmask(cen,cen) = 0.
smask(cen,cen) = 0.

;   Extract fields of the masks, if necessary
if keyword_set( field ) then begin
    suba = fltarr(extent , fix(extent/2) , nmax)
    mask = fieldof(mask,/odd)
    xmask = fieldof(mask2,/odd)
    ymask = fieldof(transpose(mask2),/odd)
    mask3 = fieldof(mask3,/odd)
    cmask = fieldof(cmask,/odd)
    smask = fieldof(smask,/odd)

    halfext = fix( extent /2 )
    yl = y - fix(halfext/2)
    yh = yl + halfext -1
    yscale = 2
    ycen = cen/2

endif else begin
    suba = fltarr(extent, extent, nmax)
    xmask = mask2
    ymask = transpose( mask2 )

    yl = y - fix(extent/2)
    yh = yl + extent -1
    yscale = 1
    ycen = cen
endelse

;   Estimate the mass
for i=0L,nmax-1 do m(i) = total( a(xl(i):xh(i),yl(i):yh(i)) * mask )

if keyword_set( masscut ) then begin
    w = where( m gt masscut, nmax )
    if nmax eq 0 then begin
       message,'No features found!',/inf
       return,-1
    endif
    xl = xl(w)
    xh = xh(w)
    yl = yl(w)
    yh = yh(w)
    x = x(w)
    y = y(w)
    m = m(w)
endif

if not keyword_set(quiet) then message, strcompress( nmax ) + ' features found.',/inf

;   Setup some result arrays
xc = fltarr(nmax)
yc = fltarr(nmax)
rg = fltarr(nmax)
e  = fltarr(nmax)

;   Calculate feature centers
for i=0L,nmax-1 do begin
    xc(i) = total( a(xl(i):xh(i),yl(i):yh(i)) * xmask )
    yc(i) = total( a(xl(i):xh(i),yl(i):yh(i)) * ymask )
endfor

;   Correct for the 'offset' of the centroid masks
xc = xc / m - ((float(extent)+1.)/2.)
yc = (yc / m - ((float(extent)+1.)/2.)) / yscale

;   Iterate any bad initial estimate.
if keyword_set( iterate ) then begin
counter = 0
repeat begin
    counter = counter + 1

    w = where( abs(xc) gt 0.6, nbadx )
    if nbadx gt 0 then begin
      dx = round( xc(w) )
       xl(w) = xl(w) + dx
       xh(w) = xh(w) + dx
       x(w) = x(w) + dx
    endif
    w = where( abs(yc) gt 0.6, nbady )
    if nbady gt 0 then begin
      dy = round( yc(w) )
       yl(w) = yl(w) + dy
       yh(w) = yh(w) + dy
       y(w) = y(w) + dy
    endif

    w = where( (abs(xc) gt 0.6) or (abs(yc) gt 0.6), nbad )
    if nbad gt 0 then begin     ; recalculate the centroids for the guys we're iterating

       for i=0,nbad-1 do m(w(i)) = total( a(xl(w(i)):xh(w(i)),$
         yl(w(i)):yh(w(i))) * mask )
       for i=0,nbad-1 do begin
         xc(w(i)) = total( a(xl(w(i)):xh(w(i)),yl(w(i)):yh(w(i))) * xmask )
         yc(w(i)) = total( a(xl(w(i)):xh(w(i)),yl(w(i)):yh(w(i))) * ymask )
       endfor

       xc(w) = xc(w) / m(w) - ((float(extent)+1.)/2.)
       yc(w) = ( yc(w) / m(w) - ((float(extent)+1.)/2.) ) / yscale

    endif
endrep until (nbad eq 0) or (counter eq 10)
endif

;   Update the positions and correct for the width of the 'border'
x = x + xc - extent/2
y = ( y + yc - extent/2 ) * yscale

;   Construct the subarray
for i=0L,nmax-1 do suba(*,*,i) = fracshift( a(xl(i):xh(i),yl(i):yh(i)), -xc(i) , -yc(i) )

;   Calculate the 'mass'
for i=0L,nmax-1 do m(i) = total( suba(*,*,i) * mask )

;   Calculate radii of gyration squared
for i=0L,nmax-1 do rg(i) = total( suba(*,*,i) * mask3 ) / m(i)

;   Calculate the 'eccentricity'
for i=0L,nmax-1 do e(i) = sqrt(( total( suba(*,*,i) * cmask )^2 ) +$
    ( total( suba(*,*,i) * smask )^2 )) / (m(i)-suba(cen,ycen,i)+1e-6)

params = [transpose(x),transpose(y),transpose(m),transpose(rg),transpose(e)]
return,params
end



