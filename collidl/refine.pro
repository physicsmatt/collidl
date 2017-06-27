

function refine, image, location_data, distance_from_center

values=fltarr(distance_from_center*10*2)
numvalues=fltarr(distance_from_center*10*2)
data=fltarr(distance_from_center*10*2)

values[*]=0
numvalues[*]=0
data[*]=0

locsize=size(location_data,/dimensions)
pix=size(image,/dimensions)

;newcenter=fltarr(

print,"I'm the Juggernaut Bitch!"
for i = 0L, locsize[1]-1 do begin
    x=location_data[0,i]
    y=location_data[1,i]
    ;x=16
    ;y=15
    for j = -distance_from_center + round(x), distance_from_center + round(x) do begin;indecies for pixels in area around previously found center
        for k = -distance_from_center + round(y), distance_from_center + round(y) do begin
          if j GE 0 and j LT pix[1] then begin
            if k GE 0 and k LT pix[1] then begin
            dist=round(10*sqrt((x-j)^2 + (y-k)^2));distance from pixel in question to center of sphere
            height=image[j,pix[1]-k-1]
            ;print,x,y,j,k,dist,height
            values[dist]=values[dist]+height;puts height in array
            numvalues[dist]++;records number of values put in array, to calculate average

            endif
           endif
        endfor
    endfor
endfor

for l = 0, distance_from_center*10*2-1 do begin
    print,l,values[l],numvalues[l],values[l]/numvalues[l]
    data[l]=values[l]/numvalues[l]
endfor

myplot = OBJ_NEW('IDLgrPlot', data)

mywindow = OBJ_NEW('IDLgrWindow')
myview = OBJ_NEW('IDLgrView', VIEWPLANE_RECT=[-10,-10,120,120])
mymodel = OBJ_NEW('IDLgrModel')

mymodel->Add, myplot

mywindow->Draw, myview

;tv, image
print, potatoes
return, data

end
