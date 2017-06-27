; plot_hist
;
; see http://glinda.lrsm.upenn.edu/~weeks/idl
;    for more information and software updates
;
;	A user-friendly way to plot roughly normal distributed histograms
;		easily with optional gaussian fits.
;	The relevant data can be returned with the optional parameter 'data'
;	and coff.  set number of terms in fit (must be 3 or more) using nterms
;
; original version by John C. Crocker
; slightly revised, Feb '99, Eric R. Weeks; /log added 2-23-99
;
pro plot_hist,d,data,coff,nterms=nterms,fit=fit,xrange=xrange,binsize=binsize,$
	noplot=noplot,oplot=oplot,jitter = jitter,center=center,log=log

if keyword_set(fit) then center=1;         ERW 2-22-99
if not keyword_set(jitter) then jitter = 0
if keyword_set(nterms) then begin
	if (nterms lt 3 or nterms gt 6) then begin
		print,'Resetting nterms to 6.  nterms must be 3, 4, 5, or 6!  See gaussfit.'
		nterms = 6
	endif
endif
if not keyword_set(nterms) then nterms = 3

; don't know why, just gotta do it
data = float(d)

; get a little info about the distribution
n=n_elements(data)
logn=alog10(n)
min = min(data,max=max)
stdev = stdev(data,mean)
min = min([min,mean-(6*stdev)])
max = max([max,mean+(4*stdev)])
if keyword_set(xrange) then begin
	min = xrange(0)
	max = xrange(1)
	w=where(data ge min and data le max,ndata)
	logn = alog10(ndata)
	stdev=stdev(data(w))	
endif

; calculate a nice binsize if the user doesn't give us one
if not keyword_set(binsize) then begin
	binsize=stdev/logn
;	reduce the binsize slightly so that its either 1,2 or 5 times a power of ten
	logbin=alog10(binsize)
	fp = (logbin-fix(logbin)) 
	ip = fix(logbin)
	if (fp lt 0) then begin
		fp = fp+1 & ip = ip-1 
	endif	
	coeff = 10^(fp)
	if (coeff lt 2) then fp = 0
	if (coeff ge 2) AND (coeff lt 5) then fp = alog10(2)
	if (coeff ge 5)	then fp = alog10(5)
	binsize = 10^(float(ip + fp))
endif	

; adjust the min value down so that 0 is at the center of a partition
minbin = long(min/binsize)
if minbin lt 0 then minbin = minbin-1
min = minbin * binsize
if (keyword_set(center) and (not keyword_set(xrange))) then xrange=[min,max]

; oh yeah, calculate the histogram
hist = histogram(data,binsize=binsize,min=min-(binsize*jitter),max=max+binsize*jitter) 

; pad out the hist with a zero so the plot doesn't hang in the air!
hist=[hist,[0]]
np = n_elements(hist)

; make an 'x' vector for the plot and the fit
x = (findgen(np)*binsize) + min - (binsize*jitter)

if (not keyword_set(center)) then begin
	w=where(hist gt 0,nw)
	minw=w(0) > 1
	maxw=w(nw-1) < (n_elements(hist)-2)
	hist=hist(minw-1:maxw+1)
	x=x(minw-1:maxw+1)
endif

; plot the histogram
if not keyword_set(noplot) then begin
	if keyword_set(oplot) then begin
		oplot,x,hist,psym=10
	endif else begin
		w=where(hist gt 0)
		if (keyword_set(xrange)) then begin 
			if (keyword_set(log)) then begin
				plot,x(w),hist(w),xrange=xrange,psym=4,/ylog
			endif else begin
				plot,x,hist,xrange=xrange,psym=10 
			endelse
		endif else begin
			if (keyword_set(log)) then begin
				plot,x(w),hist(w),psym=4,/ylog
			endif else begin
				plot,x,hist,psym=10 
			endelse
		endelse
	endelse 
endif

data = [transpose(x),transpose(hist)]

; do the fit, if desired
if keyword_set(fit) then begin
	ft = gaussfit(x,hist,coff,nterms=nterms)
	data = [transpose(x),transpose(hist),transpose(ft)]

	if not keyword_set(noplot) then begin
		oplot,x,ft,linestyle=3
		dy = !y.crange(1)-!y.crange(0)
		dx = !x.crange(1)-!x.crange(0)
		yy = !y.crange(0)
		xx = !x.crange(0)
		if (keyword_set(log)) then begin
			xyouts, xx + (dx*0.15), 10^(yy + (dy*.90)), "Height = "+$
				strcompress(string(coff(0),format='(g9.4)')) ,alignment=0.5
			xyouts, xx + (dx*0.15), 10^(yy + (dy*.85)), "Offset = "+$
				strcompress(string(coff(1),format='(g9.4)')) ,alignment=0.5
			xyouts, xx + (dx*0.15), 10^(yy + (dy*.80)), "Sigma = "+$
				strcompress(string(coff(2),format='(g9.4)')) ,alignment=0.5
		endif else begin
			xyouts, xx + (dx*0.15), yy + (dy*.90), "Height = "+$
				strcompress(string(coff(0),format='(g9.4)')) ,alignment=0.5
			xyouts, xx + (dx*0.15), yy + (dy*.85), "Offset = "+$
				strcompress(string(coff(1),format='(g9.4)')) ,alignment=0.5
			xyouts, xx + (dx*0.15), yy + (dy*.80), "Sigma = "+$
				strcompress(string(coff(2),format='(g9.4)')) ,alignment=0.5
		endelse
		if (nterms gt 3) then begin
			if (keyword_set(log)) then begin
		xyouts, xx + (dx*0.15), 10^(yy + (dy*.75)), "Polynomial:" ,alignment=0.5
		xyouts, xx + (dx*0.15), 10^(yy + (dy*.70)), $
			strcompress(string(coff(3),format='(g10.4)')) ,alignment=0.5
			endif else begin
		xyouts, xx + (dx*0.15), yy + (dy*.75), "Polynomial:" ,alignment=0.5
		xyouts, xx + (dx*0.15), yy + (dy*.70), $
			strcompress(string(coff(3),format='(g10.4)')) ,alignment=0.5
			endelse
		
		if (nterms gt 4) then begin
			if (keyword_set(log)) then begin
		xyouts, xx + (dx*0.15), 10^(yy + (dy*.65)), "+ "+$
			strcompress(string(coff(4),format='(g9.4)'))+" x" ,alignment=0.5
			endif else begin
		xyouts, xx + (dx*0.15), yy + (dy*.65), "+ "+$
			strcompress(string(coff(4),format='(g9.4)'))+" x" ,alignment=0.5
			endelse
		if (nterms gt 5) then begin
			if (keyword_set(log)) then begin
		xyouts, xx + (dx*0.15), 10^(yy + (dy*.60)), "+ "+$
			strcompress(string(coff(5),format='(g10.4)'))+" x^2" ,alignment=0.5
			endif else begin
		xyouts, xx + (dx*0.15), yy + (dy*.60), "+ "+$
			strcompress(string(coff(5),format='(g10.4)'))+" x^2" ,alignment=0.5
			endelse
		endif ; five
		endif ; four
		endif ; three
	endif
endif


end
