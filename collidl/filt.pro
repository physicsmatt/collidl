fs=dialog_pickfile(get_path=ps,/multiple_files)
if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif' then readlist,fs(0),fs,path=ps else fs=[fs]

for i=0,n_elements(fs)-1 do begin
print,'Now doing: ',fs(i)
data=read_tiff(fs(i))
;data=data(48:48+511,80:80+511)  probably test code.
n=size(data) & n=n(1:n(0))
print,n
;window,0,xsize=n(0)+140,ysize=n(1)+140
;plot,[0],[0],/nodata,xrange=[0,n(0)],yrange=[0,n(1)],xstyle=1,ystyle=1,position=[100,100,100+n(0),100+n(1)],/device
;tvscl,data,100,100

f=shift(fft(data),n(0)/2,n(1)/2)
;window,4,xsize=n(0)+140,ysize=n(1)+140
;tvscl,alog(abs(f)),100,100 & tv,bytscl(rebin(abs(f),n(0)/8,n(1)/8),max=2),100,100
ns=max(n)/2 & nphi=floor(2*!pi*ns)
ds=1./ns
s=ds*findgen(ns)#replicate(1,nphi)
phi=replicate(1.,ns)#(findgen(nphi)*2*!pi/nphi)
x=s*cos(phi)*n(0)+n(0)/2
y=s*sin(phi)*n(1)+n(1)/2
sf=bilinear(f,x,y) & sff=sf
sf=total(abs(sf)^2,2)/nphi & s=s(*,0)
;window,8
;plot,2*!pi*s,s*sf,psym=-4,symsiz=0.3
tmp=max(s*sf,w)
r=1./s(w)
print,'Radius (max):        ',r,' pixels'
w=where((s*sf) gt tmp/2.)
r=1./(total(s(w))/n_elements(w))
print,'Radius (halfheight): ',r,' pixels'
;oplot,[1/r],[tmp/2.],psym=4


x=(findgen(n(0))-n(0)/2.)#replicate(1.,n(1))
y=replicate(1.,n(0))#(findgen(n(1))-n(1)/2.)
q=sqrt((x/n(0))^2+(y/n(1))^2)
q0=1./r
perc=0.35
msk=(q gt (1-perc)*q0) and (q lt (1+perc)*q0)
msk=(q gt (1-perc)*q0) and (q lt q0/(1-perc))
;wset,4 & wshow,4
;tvscl,abs(f)*msk,100,100

filtdata=float(fft(shift(msk*f,-n(0)/2,-n(1)/2),/inverse))
;wset,0 & wshow,0
;tvscl,filtdata,100,100
smpltiff,strmid(fs(i),0,strlen(fs(i))-4)+'fil.tif',bytscl(filtdata)
;writeorg,strmid(fs(i),0,strlen(fs(i))-4)+'fft.dat',[[2*!pi*s],[sf],[s*sf]]


if 1 eq 1 then begin
rr=floor(r)+1
x=findgen(2*rr+1)#replicate(1.,2*rr+1)-rr
y=replicate(1.,2*rr+1)#findgen(2*rr+1)-rr
msk=sqrt(x^2+y^2) lt r
cmsk=sqrt(x^2+y^2) lt (r/2)
data=bytscl(convol(data,cmsk,total(cmsk),/edge_wrap))
mn=erode(data,msk,/gray) eq data
mx=dilate(data,msk,/gray) eq data
w=where(mx)
x=w mod n(0) & y=w/n(0)
endif

endfor

end
