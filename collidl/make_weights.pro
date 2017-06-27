; makes the weights for pair correlations on a 256 square matrix


pro main, dummy

	W=fltarr(100)
	nw=100*100L-1

	for aa=1L,nw do begin
		ya=aa/100
		xa=aa-100*ya	
		print,xa,ya
		for bb=0,aa do begin	
			yb=bb/100
			xb=bb-100*yb
			d=floor(sqrt((xa-xb)*(xa-xb)+(ya-yb)*(ya-yb)))
			if (d<100) then begin
				W[d]=W[d]+1
			endif				
		endfor
	endfor

	window,1
	print,'W[3]=',W[3]
	plot,W(0:99),psym=0,max_value=1000,min_value=0

	openw,u,'weights.dat',/get_lun
	for Index=0, 99 do begin
		printf,u,W[Index]
	endfor
	free_lun,u	

end
