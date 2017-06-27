pro readlist,fs,s,path=path
openr,u,fs,/get_lun
s=strarr(5000)
on_ioerror,go_on
readf,u,s
go_on: free_lun,u
s=transpose(s(where(s ne '')))
if keyword_set(path) then cd,path
get_lun,u
on_ioerror,error
for i=0,n_elements(s)-1 do begin
  openr,u,s(i)
  close,u
endfor
free_lun,u
return
error: message,'Cannot open file '+string(i)+': '+s(i)
end

print,"readlist,fs,list,path=  ; To read a list of Christopher's filenames"
end.run ; nothing
