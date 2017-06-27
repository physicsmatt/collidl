;I may have swapped x and y at one point in the logic, I don't think so.  it should be pretty blatantly obvious if so.
;other than that, this should work like a charm.
;the only major kludge is the A coefficient for the C+Asin((2pi/w)*r)

function sintest,x,c
 outbound=fltarr(5); return goodie box!
 y1=FLOAT(x)
 x1=IMAGINARY(x)
 r=sqrt(((y1-c[1])^2)+((x1-c[0])^2))
 ;does the function, followed by the partial derivatives.
 outbound[0]=c[3]+(c[4]*(cos(((!PI)*r*2)/c[2])))
 outbound[1]=((4*(!PI)*(c[4]))*sqrt((x1-c[0]))*(sin((2*(!PI)*sqrt(((x1-c[0])^2)-(((y1-c[1]))^2))/c[2]))))/c[2]
 outbound[2]=((4*(!PI)*(c[4]))*sqrt((y1-c[1]))*(sin((2*(!PI)*sqrt(((x1-c[0])^2)-(((y1-c[1]))^2))/c[2]))))/c[2]
 outbound[3]=((2*(!PI)*sqrt(((x1-c[0])^2)-(((y1-c[1]))^2)))*(sin((2*(!PI)*sqrt(((x1-c[0])^2)-(((y1-c[1]))^2))/c[2]))))/((c[2])^2)
 outbound[4]= sin((2*(!PI)*sqrt(((x1 - c[0])^2) - (((y1 - c[1]))^2))/c[2]))
 return, [outbound[0], outbound[1], outbound[2], outbound[3], [1 + X*0], outbound[4] ]
end


function kludgegen ; this is used to produce a nice little set of points to fake an image.
;it will help us determine how unstable the sort is and weed out issues with the diffs.  I hope.
a=fltarr(5)
x=0
y=0
b=fltarr(6)
temp=''
READ, temp, Prompt='Array Boundary in x '
x=Fix(temp)
READ, temp, Prompt='Array Boundary in y '
y=Fix(temp)
READ, temp, Prompt='Imagined Center for test (x)?'
a[0]=Fix(temp)
READ, temp, Prompt='Imagined Center for test (y)?'
a[1]=Fix(temp)
READ, temp, Prompt='Width of the sinusoidal for test (w)?   (recommend that this be kept within the boundaries selected above) '
a[2]=Fix(temp)
READ, temp, Prompt='Offset of the sinusoidal for test (c)? (recommend about 80 or it will overshoot the 255 max, I think)'
a[3]=Fix(temp)
READ, temp, Prompt='Amplitude of the sinusoidal for test?'
a[4]=Fix(temp)
image=fltarr(x,y)
for j = 0, x-1 do begin;indicies for pixels in area around previously found center
        for k=0,y-1 do begin
        m=DCOMPLEX(j,k)
          b=sintest(m,a)
               image[j,k]=b[0]
       endfor
endfor
return, image
end

function refineB2, image, location_data, distance_from_center
C = COMPLEXARR((distance_from_center*2)*(distance_from_center*2)) ;coordinate superstructure
a=fltarr(5); "coefficient" box
Brightnesses = fltarr((distance_from_center*2)*(distance_from_center*2))

coordc1 = 0
coordc2 = 0

image=kludgegen()
temp = ''
READ, temp, Prompt='Guess Center in x '
READS, temp, x
READ, temp, Prompt='Guess Center in y '
READS, temp, y


;locsize=size(location_data,/dimensions) ; temporarily commented
pix=size(image,/dimensions)
;Chis=fltarr(locsize[1]) ; temporarily commented
;for i = 0L, locsize[1]-1 do begin ; temporarily commented
for i = 0L, 1 do begin
    ;x=location_data[0,i]
    ;y=location_data[1,i]
    for j = -distance_from_center + round(x), distance_from_center + round(x) do begin;indecies for pixels in area around previously found center
        for k = -distance_from_center + round(y), distance_from_center + round(y) do begin
        if j GE 0 and j LT pix[0] then begin
            if k GE 0 and k LT pix[1] then begin;populates an array for the brightnesses.  This should be good.
             Brightnesses[(coordc1*distance_from_center*2)+coordc2]=image[j,k];does the dirty work of it.
             C[(coordc1*distance_from_center*2)+coordc2] =DCOMPLEX(j,k)
            endif
         endif
         coordc2+=1
        endfor
    coordc1+=1
    endfor
    a[0]=x
    a[1]=y
    a[2]=distance_from_center*2 ; sets up the w for the sinusoidal
    a[3]=MEAN(Brightnesses) ;sets up the C for the sinusoidal.
    a[4]=(Max(Brightnesses)-Min(Brightnesses))/a[3] ; just a kludge for the amplitude coefficient! no idea what else to do.

    Brightnesses=LMFIT(C, Brightnesses, a, chisq=Chis[i],/Double,FUNCTION_NAME = 'sintest')
    ;my feeling is that the interesting stuff is actually the "coefficient" x and y aught. correct me if I'm wrong

    location_data[0,i]=a[0]
    location_data[1,i]=a[1]
endfor

return, location_data

end


pro refineB2, image, location_data, distance_from_center
C = COMPLEXARR((distance_from_center*2)*(distance_from_center*2)) ;coordinate superstructure
a=fltarr(5); "coefficient" box
Brightnesses = fltarr((distance_from_center*2)*(distance_from_center*2))

coordc1 = 0
coordc2 = 0

image=kludgegen()
temp = ''
READ, temp, Prompt='Guess Center in x '
READS, temp, x
READ, temp, Prompt='Guess Center in y '
READS, temp, y


;locsize=size(location_data,/dimensions) ; temporarily commented
pix=size(image,/dimensions)
;Chis=fltarr(locsize[1]) ; temporarily commented
Chis=fltarr(10)
;for i = 0L, locsize[1]-1 do begin ; temporarily commented
for i = 0L, 1 do begin
    ;x=location_data[0,i]
    ;y=location_data[1,i]
    for j = -distance_from_center + round(x), distance_from_center + round(x) do begin;indecies for pixels in area around previously found center
        for k = -distance_from_center + round(y), distance_from_center + round(y) do begin
        if j GE 0 and j LT pix[0] then begin
            if k GE 0 and k LT pix[1] then begin;populates an array for the brightnesses.  This should be good.
             Brightnesses[(coordc1*distance_from_center*2)+coordc2]=image[j,k];does the dirty work of it.
             C[(coordc1*distance_from_center*2)+coordc2] =DCOMPLEX(j,k)
            endif
         endif
         coordc2+=1
        endfor
    coordc2= 0
    coordc1+=1
    endfor

    a[0]=x
    a[1]=y
    a[2]=distance_from_center*2 ; sets up the w for the sinusoidal
    a[3]=MEAN(Brightnesses) ;sets up the C for the sinusoidal.
    a[4]=(Max(Brightnesses)-Min(Brightnesses))/a[3] ; just a kludge for the amplitude coefficient! no idea what else to do.

    Brightnesses=LMFIT(C, Brightnesses, a, chisq=Chis[i],/Double,FUNCTION_NAME = 'sintest')
    ;my feeling is that the interesting stuff is actually the "coefficient" x and y aught. correct me if I'm wrong

    ;location_data[0,i]=a[0]
    ;location_data[1,i]=a[1]
    coordc1 = 0

endfor

;return, location_data

end

