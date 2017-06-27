;I may have swapped x and y at one point in the logic, I don't think so.  it should be pretty blatantly obvious if so.
;other than that, this should work like a charm.
;the only major kludge is the A coefficient for the C+Asin((2pi/w)*r)

function sigmoid_1d,r,params
  outbound=fltarr(4); return goodie box!
  p= 2.0
  w=params[0]
  A=params[1]
  C=params[2]
;print,w,A,C
  iw = 1/w
  exp_mrwp= exp(-(iw*r)^p)
  Aexp_mrwp= A*exp_mrwp
  prwpm1=p*(iw*r)^(p-1)
;  outbound[0] = A*exp(-(r/w)^p) + C                          ;function
;  outbound[1] = (A*exp(-(r/w)^p)*p*r     *(r/w)^(p-1))/w^2   ;d/dw
;  outbound[2] = exp(-(r/w)^p)                                ;d/dA

  ;this is same as above, but optimized for speed.
  outbound[0] = Aexp_mrwp + C                          ;function
  outbound[1] = Aexp_mrwp * prwpm1 * r *iw^2       ;d/dw
  outbound[2] = exp_mrwp                                ;d/dA
  outbound[3] = 1.0                                          ;d/dC
  return, outbound
end

function sigmoid_2d,indep,params
  common shared_xypairs, xy_pairs
  outbound=complexarr(6); return goodie box!
  index = fix(indep)
  x=REAL_PART(xy_pairs[index])
  y=IMAGINARY(xy_pairs[index])
;print,"index, x,y are",index,x,y
  p= 2.0
  x0=params[0]
  y0=params[1]
  w=params[2]
  A=params[3]
  C=params[4]
;print,x0,y0,w,A,C
  dx = x-x0
  dy = y-y0
  iw = 1/w
  r=sqrt((dy^2)+(dx^2))
  exp_mrwp= exp(-(iw*r)^p)
  Aexp_mrwp= A*exp_mrwp
  prwpm1=p*(iw*r)^(p-1)
;  outbound[0] = A*exp(-(r/w)^p) + C                          ;function
;  outbound[1] = A*exp(-(r/w)^p)*p*(x-x0)*(r/w)^(p-1)         ;d/dx0
;  outbound[2] = A*exp(-(r/w)^p)*p*(y-y0)*(r/w)^(p-1)         ;d/dy0
;  outbound[3] = (A*exp(-(r/w)^p)*p*r     *(r/w)^(p-1))/w^2   ;d/dw
;  outbound[4] = exp(-(r/w)^p)                                ;d/dA

  ;this is same as above, but optimized for speed.
  outbound[0] = Aexp_mrwp + C                          ;function
  outbound[1] = Aexp_mrwp * prwpm1 * dx       ;d/dx0
  outbound[2] = Aexp_mrwp * prwpm1 * dy       ;d/dy0
  outbound[3] = Aexp_mrwp * prwpm1 * r *iw^2       ;d/dw
  outbound[4] = exp_mrwp                                ;d/dA
  outbound[5] = 1.0                                          ;d/dC
  return, outbound
end

function sintest,indep,params
  common shared_xypairs, xy_pairs
  outbound=complexarr(6); return goodie box!
  index = fix(indep)
  x=REAL_PART(xy_pairs[index])
  y=IMAGINARY(xy_pairs[index])
;print,"index, x,y are",index,x,y
  x0=params[0]
  y0=params[1]
  w=params[2]
  A=params[3]
  C=params[4]
;print,x0,y0,w,A,C
  r=sqrt(((y-y0)^2)+((x-x0)^2))
  PI2w = 2*!PI/w
  outbound[0] = A*cos(PI2w*r)+C  ;function
  outbound[1] = A*sin(PI2w*r)*(PI2w/r)*(x-x0)   ;d/dx0
  outbound[2] = A*sin(PI2w*r)*(PI2w/r)*(y-y0)   ;d/dy0
  outbound[3] = A*sin(PI2w*r)*(PI2w*r)/w       ;d/dw
  outbound[4] = cos(PI2w*r)                  ;d/dA
  outbound[5] = 1.0                            ;d/dC
  return, outbound
end



function make_test_image ;
arr_size = 256
base_period = 8.0
amplitude = 80
offset = 128

int_im = indgen(arr_size,arr_size,/LONG)
com_im = complex((int_im mod arr_size),(int_im / arr_size))

;k1 = complex(1.0,0.0)*(2*!PI)/period
;k2 = complex(cos(!PI/3),sin(!PI/3))*(2*!PI)/period
;k3 = complex(cos(!PI/3),-sin(!PI/3))*(2*!PI)/period

;a=[0.0,  1,  0,  -0.333333,  0,  0.200000,  0,  -0.142857,  0,  0.111111,  0,  -0.0909091,  0]
a=[0.0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0.,  0]

w1=fltarr(arr_size,arr_size)
w2=fltarr(arr_size,arr_size)
w3=fltarr(arr_size,arr_size)

for h = 1, 11 do begin
  period = base_period / h
  k1 = complex(1.0,0.0)*(2*!PI)/period
  k2 = complex(cos(!PI/3),sin(!PI/3))*(2*!PI)/period
  k3 = complex(cos(!PI/3),-sin(!PI/3))*(2*!PI)/period
  w1 += a[h] * (cos(real_part(k1*conj(com_im)))+1) / 2
  w2 += a[h] * (cos(real_part(k2*conj(com_im)))+1) / 2
  w3 += a[h] * (cos(real_part(k3*conj(com_im)))+1) / 2
endfor

im = w1 * w2 * w3

;im = fix(((im*2-1)*amplitude)+offset)
im = fix(((im)*amplitude)+offset)
im += fix(randomu(1,arr_size,arr_size)*20)-10
return,im
end


pro refineB3, image, distance_from_center, x,y,sphere_locations
common shared_xypairs, xy_pairs

xy_pairs = COMPLEXARR((distance_from_center*2)*(distance_from_center*2)) ;coordinate superstructure
a=dblarr(5); "coefficient" box
Brightnesses = fltarr((distance_from_center*2)*(distance_from_center*2))

;locsize=size(location_data,/dimensions) ; temporarily commented
pix=size(image,/dimensions)
;Chis=fltarr(locsize[1]) ; temporarily commented
Chis=fltarr(10)
;for i = 0L, locsize[1]-1 do begin ; temporarily commented
distance_from_center_squared = distance_from_center*distance_from_center
for i = 0L, 0 do begin
count=0
    ;x=location_data[0,i]
    ;y=location_data[1,i]
    for j = -distance_from_center + round(x), distance_from_center + round(x) do begin;indecies for pixels in area around previously found center

        for k = -distance_from_center + round(y), distance_from_center + round(y) do begin

        if j GE 0 and j LT pix[0] then begin
            if k GE 0 and k LT pix[1] then begin;populates an array for the brightnesses.  This should be good.
              dx = (x-j)
              dy = (y-k)
              dr = dx*dx+dy*dy
              if dr LE distance_from_center_squared then begin
;print,j,k
                xy_pairs[count] = DCOMPLEX(j,k)
                Brightnesses[count]=image[j,k]
                count +=1
                endif
              endif
            endif
         endfor
    endfor
            xy_pairs = xy_pairs[0:count-1]
            Brightnesses = Brightnesses[0:count-1]

    a[0]=x+0.3
    a[1]=y-0.4
    ;a[2]=8.1;distance_from_center*2 ; sets up the w for the sinusoidal
    a[2]=6.1;distance_from_center*2 ; sets up the w for the sinusoidal
    a[3]=70;(Max(Brightnesses)-Min(Brightnesses))/2 ; just a kludge for the amplitude coefficient! no idea what else to do.
    a[4]=120;MEAN(Brightnesses) ;sets up the C for the sinusoidal.

;print,a
;help,xy_pairs
;print,size(xy_pairs)
indep_points=indgen((size(xy_pairs))[1])
;help,indep_points
;print,"indep points = ", indep_points
;help,Brightnesses

    Best_values = LMFIT(indep_points, Brightnesses, a, chisq=chisq,FITA = [1,1,1,1,1],FUNCTION_NAME = 'sigmoid_2d',iter=iter, itmin=20,convergence = convergence,itmax=100,/double);,/double,TOL=0.00001)

    ;location_data[0,i]=a[0]
    ;location_data[1,i]=a[1]
;print,a
;print,iter
;print,convergence
;print,best_values
;print,brightnesses
;print,best_values - brightnesses

if ((convergence eq 1) and (a[0] GT 0) and (a[0] LT 255) and (a[1] GT 0) and (a[1] LT 255)) then begin
  print, convergence,iter,chisq,x,y, a
  sphere_locations[a[0],a[1]]=255
  tv,sphere_locations
endif

endfor

;return, location_data

end


pro looptest,image

sphere_locations = bytarr(256,256)
    for i = 16,64 do begin
      for j = 16,64 do begin
        refineB3,image,3,i,j,sphere_locations
      endfor
    endfor

end

pro find_average_sphere_profile

;step through spheres
;for i=0,...
;x0
;use refineB3 as a guide for this function.
;Bin the points into a 10x larger grid space and average them.
;use a rectangular block; no need for a circle

end

function fourier_bpass,data


;data=data(48:48+511,80:80+511)  probably test code.
n=size(data) & n=n(1:n(0))
print,n
window,0,xsize=n(0)+140,ysize=n(1)+140
plot,[0],[0],/nodata,xrange=[0,n(0)],yrange=[0,n(1)],xstyle=1,ystyle=1,position=[100,100,100+n(0),100+n(1)],/device
tvscl,data,100,100


f=shift(fft(data),n(0)/2,n(1)/2)
window,4,xsize=n(0)+140,ysize=n(1)+140
tvscl,alog(abs(f)),100,100 & tv,bytscl(rebin(abs(f),n(0)/8,n(1)/8),max=2),100,100


ns=max(n)/2 & nphi=floor(2*!pi*ns)
ds=1./ns
s=ds*findgen(ns)#replicate(1,nphi)
phi=replicate(1.,ns)#(findgen(nphi)*2*!pi/nphi)
x=s*cos(phi)*n(0)+n(0)/2
y=s*sin(phi)*n(1)+n(1)/2
sf=bilinear(f,x,y) & sff=sf
sf=total(abs(sf)^2,2)/nphi & s=s(*,0)
window,8
plot,2*!pi*s,s*sf,psym=-4,symsiz=0.3

tmp=max(s*sf,w)
r=1./s(w)
print,'Radius (max):        ',r,' pixels'
w=where((s*sf) gt tmp/2.)
r=1./(total(s(w))/n_elements(w))
print,'Radius (halfheight): ',r,' pixels'
oplot,[1/r],[tmp/2.],psym=4

x=(findgen(n(0))-n(0)/2.)#replicate(1.,n(1))
y=replicate(1.,n(0))#(findgen(n(1))-n(1)/2.)
q=sqrt((x/n(0))^2+(y/n(1))^2)
q0=1./r
perc=0.35
msk=(q gt (1-perc)*q0) and (q lt (1+perc)*q0)
msk=(q gt (1-perc)*q0) and (q lt q0/(1-perc))
wset,4 & wshow,4
tvscl,abs(f)*msk,100,100

filtdata=float(fft(shift(msk*f,-n(0)/2,-n(1)/2),/inverse))
window,9
wset,9 & wshow,9
tvscl,filtdata,100,100
;print,filtdata[0:10,0:10]
return,filtdata
end

function get_initial_xy, image
filtered_image=fourier_bpass(image)
tv,filtered_image
print,filtered_image[0:8,0:8]

localmax = (filtered_image ge shift(filtered_image,1,0)) and  $
      (filtered_image ge shift(filtered_image,-1,0)) and $
      (filtered_image ge shift(filtered_image,0,1)) and $
      (filtered_image ge shift(filtered_image,0,-1)) and $
      (filtered_image ge shift(filtered_image,-1,-1)) and $
      (filtered_image ge shift(filtered_image,-1,1)) and $
      (filtered_image ge shift(filtered_image,1,-1)) and $
      (filtered_image ge shift(filtered_image,1,1))
tv,bytscl(localmax)
where_max = where(localmax)
;return,where_max
return,[[where_max MOD 256],[where_max / 256]]
end

function build_average_profile,image,xy

    scale_factor = 3

    x_size = (size(image))[1]
    y_size = (size(image))[2]
    num_features = (size(xy))[1]
    area_per_feature = x_size*y_size / num_features
    profile_size = sqrt(area_per_feature)
    profile=fltarr(2*profile_size*scale_factor)
    count=lon64arr(2*profile_size*scale_factor)
    r_array = indgen(2*profile_size*scale_factor,/float) / scale_factor

    for n = 0, (num_features) - 1 do begin
;    for n = 138,138 do begin
       x = xy[n,0]
       y = xy[n,1]
;print,n,x,y,image[x,y]
       for i = round(x - profile_size), round(x + profile_size) do begin
         for  j = round(y - profile_size), round(y + profile_size) do begin
            if (i ge 0) and (i lt x_size) and (j ge 0) and (j lt y_size) then begin
                r = sqrt((x-i)^2+(y-j)^2)
                index = round(r*scale_factor)
;print,n,i,j,r,index,image[i,j]
                profile[index] += image[i,j]
                count[index] += 1
            endif
         endfor
       endfor
    endfor
    profile /= count
    profile = profile[where(count gt 0)]

    r_array = r_array[where(count gt 0)]

    sigma = 1.0 / sqrt(count[where(count gt 0)])

    print,'count = ', count
    print,'profile = ',profile
    print,'r_array = ', r_array
    print,'sigma = ',sigma

    local_mins = where((profile le shift(profile,-1)) and (profile le shift(profile,1)))
    first_local_min = local_mins[0]
    return_value = [[r_array],[profile],[sigma]]
    return_value = return_value[0:first_local_min,*]
    return,return_value
    plot,profile

end


pro find_features, image, feature_size

    initial_xy = get_initial_xy(image)
    average_profile = build_average_profile(image,initial_xy)
    r_values = average_profile[*,0]
    z_values = average_profile[*,1]
    errors = average_profile[*,2]

    a = fltarr(3)
    profile_size = (size(z_values))[1]
    a[0] = r_values[profile_size / 3]
    a[2] = z_values[profile_size-1]
    a[1] = z_values[0] - a[2]

    z_fit_values = LMFIT(r_values, z_values, a, measure_errors = errors,chisq=chisq,FITA = [1,1,1],FUNCTION_NAME = 'sigmoid_1d',iter=iter, itmin=20,convergence = convergence,itmax=100,/double);,/double,TOL=0.00001)

plot,r_values,z_values,psym=6
oplot,r_values,z_fit_values,COLOR=128
print,a



end