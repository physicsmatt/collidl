pro writeorg,filename,data
openw,u,filename,/get_lun
n=size(data) & n=n(1:n(0))
for i=0L,n(0)-1 do printf,u,format='('+string(n(1))+'(E15.6," "))',data(i,*)
free_lun,u
end

print,'writeorg,filename,data'
print,''
print,'Examples:'
print," writeorg,'d:\trsh.dat',[[x],[y]]"
print," writeorg,'d:\trsh.dat',[[x],[y1],[y2],[y3]]"
print," writeorg,'d:\trsh.dat',2dimg"
print," writeorg,'d:\trsh.dat',[[findgen(48)*10],[vx(25,*)]]"
end
