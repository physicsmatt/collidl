; Function to pick out a single line from the Nanoscope file header:
function afminfo,info,desc,str=str
w=where(strmid(info,0,strlen(desc)+2) eq '\'+desc+':')
s=info(w(0))
s=strmid(s,strlen(desc)+3,strlen(s)-strlen(desc)-3)
if keyword_set(str) then return,s else return,long(s)
end


print,'AFM batch file processing.'
print,'set nofiducialmarks=1 to avoid fitting the bright spots'
;Ask for batch file (or single file).
if not keyword_set(noload) then begin
fs=dialog_pickfile(get_path=ps,/multiple_files)
if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) eq 'txt' then readlist,fs(0),fs,path=ps else fs=[fs]
end


for fi=0,n_elements(fs)-1 do begin
print,'Doing file: ',fs(fi)

; Read header:
openr,u,fs(fi),/get_lun
s=strarr(10000)
on_ioerror,go_on
readf,u,s
go_on:
on_ioerror,null
s=transpose(s)
w=where(STRMID(s,0,8) eq '\Lowpass')
s=transpose(s(0:w(1)))

if keyword_set(debug) then print,s
; Split into images:
we=where((strmid(s,0,2) eq '\*') or (strmid(s,0,8) eq '\Lowpass'))
w=where(strmid(s(we),0,18) eq '\*NCAFM image list')
help,w
print,s(we(w(0))+1:we(w(0)+1)-1)
print,s(we(w(1))+1:we(w(1)+1)-1)
for i=0,n_elements(w)-1 do begin
  ss=s(we(w(i))+1:we(w(i)+1)-1)
  ;print,ss
  print,'Image ',i+1,': ',afminfo(ss,'Data type',/str),', '+afminfo(ss,'Image data',/str)
;  stop
  OFFSET=0L+afminfo(ss,'Data offset')
  point_lun,u,offset
  data=intarr(afminfo(ss,'Samps/line'),afminfo(ss,'Number of lines'))
  ;if afminfo(ss,'Bytes/pixel') ne 2 then message,'Can do just integers.'
  readu,u,data
endfor
free_lun,u

n=size(data) & n=n(1:n(0))
;window,0,xsize=n(0),ysize=n(1)
;tv,bytscl(data,min=-2000,max=6000)

; Subtract polynomial fits:
if not keyword_set(withedges) then begin
fdata=0.*data
x=findgen(n_elements(data(*,0)))
for i=0,n_elements(data(0,*))-1 do begin
  res=poly_fit(x,data(*,i),3,yfit)
  fdata(*,i)=yfit
endfor
rng=3*sqrt(total((data-fdata)^2)/n_elements(data))
;window,2,xsize=n(0),ysize=n(1)
img=bytscl(data-fdata,min=-rng,max=+rng)
;tv,img
end

if keyword_set(withedge) then begin
; Find the edge
xgrad=float(convol(data,[-1,0,1],2))
ygrad=float(convol(data,transpose([-1,0,1]),2))
grad=sqrt(xgrad^2+ygrad^2)
gradrng=sqrt(total(grad^2)/n_elements(grad))
edge=grad gt gradrng*3
;tv,img*(1-edge)+255*edge,0,0,1
;tv,img*(1-edge),0,0,2
;tv,img*(1-edge),0,0,3
f=replicate(1,5,5)
b=label_region(dilate(edge,f))
b=b*edge
h=histogram(b,reverse_indices=idx)
tmp=max(h(1:*),w) & w=w+1
w=idx(idx(w):idx(w+1)-1)
;res=poly_fit(w/n(0),w mod n(0),2,yfit)
res=svdfit(w/n(0),w mod n(0),2,yfit=yfit)
;plots,yfit,w/n(0),/device,color=255*256L
print,res
;Define left and right parts:
x=findgen(n(1))
y=fltarr(n(1))
for i=0,n_elements(res)-1 do y=y+res[i]*(x^i)
plots,y,x,/device,color=255*256L
tmp=findgen(n(0))#replicate(1,n(1))
okl=tmp lt replicate(1,n(0))#(y-5)
okr=tmp gt replicate(1,n(0))#(y+5)
fitl=tmp lt replicate(1,n(0))#y
fitr=tmp gt replicate(1,n(0))#y

fdata=0.*data
x=findgen(n_elements(data(*,0)))
for i=0,n_elements(data(0,*))-1 do begin
  w=where(okl(*,i),tmp) & if tmp gt 0 then begin
  res=poly_fit(x(w),data(w,i),2,yfit)
  w=where(fitl(*,i))
  yfit=fltarr(n_elements(w)) & for j=0,2 do yfit=yfit+res[j]*(x(w)^j)
  fdata(w,i)=yfit & end
  w=where(okr(*,i),tmp) & if tmp gt 0 then begin
  res=poly_fit(x(w),data(w,i),2,yfit)
  ;res=svdfit(x(w),data(w,i),3,yfit=yfit)
  w=where(fitr(*,i))
  yfit=fltarr(n_elements(w)) & for j=0,2 do yfit=yfit+res[j]*(x(w)^j)
  fdata(w,i)=yfit & end
endfor
w=where(okl or okr)
rng=3*sqrt(total((data(w)-fdata(w))^2)/n_elements(data(w)))
tv,bytscl(data-fdata,min=-rng,max=+rng)

if keyword_set(nofiducialmarks) then begin
;window,1,xsize=n(0),ysize=n(1)
okl=okl and ((data-fdata)/rng lt 1.5)
okr=okr and ((data-fdata)/rng lt 1.5)
;tvscl,okl,0,0,1
;tvscl,okr,0,0,2
for i=0,n_elements(data(0,*))-1 do begin
  w=where(okl(*,i),tmp) & if tmp gt 0 then begin
  res=poly_fit(x(w),data(w,i),2,yfit)
  w=where(fitl(*,i))
  yfit=fltarr(n_elements(w)) & for j=0,2 do yfit=yfit+res[j]*(x(w)^j)
  fdata(w,i)=yfit & end
  w=where(okr(*,i),tmp) & if tmp gt 0 then begin
  res=poly_fit(x(w),data(w,i),2,yfit)
  w=where(fitr(*,i))
  yfit=fltarr(n_elements(w)) & for j=0,2 do yfit=yfit+res[j]*(x(w)^j)
  fdata(w,i)=yfit & end
endfor
end
wset,2
tv,bytscl(data-fdata,min=-rng,max=+rng)
end else $
$
; Fit again, avoiding fiducial marks:
if keyword_set(nofiducialmarks) then begin
ok=(data-fdata)/rng lt 1.5
fdata=0.*data
x=findgen(n_elements(data(*,0)))
for i=0,n_elements(data(0,*))-1 do begin
  w=where(ok(*,i))
  res=poly_fit(x(w),data(w,i),3,yfit)
  fdata(w,i)=yfit
endfor
;rng=3*sqrt(total((data-fdata)^2)/n_elements(data))
;tv,bytscl(data-fdata,min=-rng,max=+rng)
endif

; Write to file:
newfs=strmid(fs(fi),0,strlen(fs(fi))-4)+strmid(fs(fi),strlen(fs(fi))-2,2)+'.tif'
;smpltiff,newfs,bytscl(data-fdata,min=-rng,max=+rng)
write_tiff,newfs,bytscl(data-fdata,min=-rng,max=+rng)
if get_kbrd(0) eq 's' then stop
endfor

end
