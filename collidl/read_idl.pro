close,/all
firstline=''
filename='sna222b.dat'
openr,11,filename
readf,11,firstline
readf,11,firstline



count=0

while (not EOF(11)) do begin
	coords=fltarr(2)
	readf,11,coords
	b=intarr(3,coords[0])
	readf,11,b
	count=count+1
	openw,12,strmid(filename,0,strlen(filename)-4)+string(count)+'.dat'
	printf,12,coords[0]
	printf,12,b
	close,12
end



close,11


end
