
function fourier_bpass,data,dominant_radius

;data=data(48:48+511,80:80+511)  probably test code.
n=size(data) & n=n(1:n(0))
;print,n
window,0,xsize=n(0)+140,ysize=n(1)+140
plot,[0],[0],/nodata,xrange=[0,n(0)],yrange=[0,n(1)],xstyle=1,ystyle=1,position=[100,100,100+n(0),100+n(1)],/device
tvscl,data,100,100


f=shift(fft(data),n(0)/2,n(1)/2)
;window,4,xsize=n(0)+140,ysize=n(1)+140
;tvscl,alog(abs(f)),100,100 & tv,bytscl(congrid(abs(f),n(0)/8,n(1)/8),max=2),100,100

ns=max(n)/2 & nphi=floor(2*!pi*ns)
;ns=max(n) & nphi=floor(2*!pi*ns)
ds=1./ns
s=ds*findgen(ns)#replicate(1,nphi)
phi=replicate(1.,ns)#(findgen(nphi)*2*!pi/nphi)
x=s*cos(phi)*n(0)+n(0)/2
y=s*sin(phi)*n(1)+n(1)/2
phi = 1
fa = abs(f)
;sf=bilinear(f,x,y) ;& sff=sf
sf=bilinear(fa,x,y) ;& sff=sf
sf=total(sf^2,2,/double)/nphi & s=s(*,0)
window,8
;s[1] = 0

plot,2*!pi*s,s*sf,psym=-4,symsiz=0.3

tmp=max(s*sf,w)
r=1./s(w)
print,'Radius (max):        ',r,' pixels'
dominant_radius = r
;These next four lines appear to be trying to get the width at half height of the peak.
;Doesn't appear to be working. At any rate, I'll use r for the filter anyway.
;w=where((s*sf) gt tmp/2.)
;rh=1./(total(s(w))/n_elements(w))
;print,'Radius (halfheight): ',r,' pixels'
;oplot,[1/rh],[tmp/2.],psym=4

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
window,9
wset,9 & wshow,9
tvscl,filtdata,100,100
;print,filtdata[0:10,0:10]
return,filtdata
end

pro sigmoid_1d_pro,r,params,f,dfda
  dfda=FLTARR(N_ELEMENTS(r),3); return goodie box!
  p= 2.0
  w=params[0]
  A=params[1]
  C=params[2]
;print,w,A,C
  iw = 1/w
  exp_mrwp= exp(-(iw*r)^p)
  Aexp_mrwp= A*exp_mrwp
  prwpm1=p*(iw*r)^(p-1)
;  dfda[0] = A*exp(-(r/w)^p) + C                          ;function
;  dfda[1] = (A*exp(-(r/w)^p)*p*r     *(r/w)^(p-1))/w^2   ;d/dw
;  dfda[2] = exp(-(r/w)^p)                                ;d/dA

  ;this is same as above, but optimized for speed.
  f = Aexp_mrwp + C                          ;function
  dfda[0,0] = Aexp_mrwp * prwpm1 * r *iw^2       ;d/dw
  dfda[0,1] = exp_mrwp                                ;d/dA
  dfda[*,2] = 1.0                                          ;d/dC
  return
end


pro sigmoid_2d_pro,indep,params,F,dfda  ;written according to how it's required for "curvefit"
  common shared_xypoints, xpoints, ypoints
;  outbound=fltarr(6); return goodie box!
  dfda=dblARR(N_ELEMENTS(indep),5)
  x=xpoints[indep]
  y=ypoints[indep]
;print,"index, x,y are",index,x,y
;print,"index, x,y are"
;print,indep
;print,x
;print,y
  p= 2
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
  Aexp_mrwp_prwpm1 = Aexp_mrwp * prwpm1
;  dfda[0] = A*exp(-(r/w)^p) + C                          ;function
;  dfda[1] = A*exp(-(r/w)^p)*p*(x-x0)*(r/w)^(p-1)         ;d/dx0
;  dfda[2] = A*exp(-(r/w)^p)*p*(y-y0)*(r/w)^(p-1)         ;d/dy0
;  dfda[3] = (A*exp(-(r/w)^p)*p*r     *(r/w)^(p-1))/w^2   ;d/dw
;  dfda[4] = exp(-(r/w)^p)                                ;d/dA

  ;this is same as above, but optimized for speed.
  f = Aexp_mrwp + C                          ;function
  dfda[0,0] = Aexp_mrwp_prwpm1 * dx       ;d/dx0
  dfda[0,1] = Aexp_mrwp_prwpm1 * dy       ;d/dy0
  dfda[0,2] = Aexp_mrwp_prwpm1 * r *iw^2       ;d/dw
  dfda[0,3] = exp_mrwp                                ;d/dA
  dfda[*,4] = 1.0                                          ;d/dC
  return
end



function make_test_image ;
xarr_size = 2048
yarr_size = 2048
base_period = 5.3
amplitude = 80
offset = 128
noise_amplitude_multiplier = 0.15
angle = 0.7
angle_radians = angle * 2*!PI/360

int_im = indgen(xarr_size,yarr_size,/LONG)
com_im = complex((int_im mod xarr_size),(int_im / xarr_size))

;k1 = complex(1.0,0.0)*(2*!PI)/period
;k2 = complex(cos(!PI/3),sin(!PI/3))*(2*!PI)/period
;k3 = complex(cos(!PI/3),-sin(!PI/3))*(2*!PI)/period

;a=[0.0,  1,  0,  -0.333333,  0,  0.200000,  0,  -0.142857,  0,  0.111111,  0,  -0.0909091,  0]
a=[0.0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0.,  0]

w1=fltarr(xarr_size,yarr_size)
w2=fltarr(xarr_size,yarr_size)
w3=fltarr(xarr_size,yarr_size)

t1 = angle_radians
t2 = !PI/3 + angle_radians
t3 = -!PI/3 + angle_radians

for h = 1, 1 do begin
  period = base_period / h
  k1 = complex(1.0,0.0)*(2*!PI)/period
  k2 = complex(cos(!PI/3),sin(!PI/3))*(2*!PI)/period
  k3 = complex(cos(!PI/3),-sin(!PI/3))*(2*!PI)/period
  k1 = complex(cos(t1),sin(t1))*(2*!PI)/period
  k2 = complex(cos(t2),sin(t2))*(2*!PI)/period
  k3 = complex(cos(t3),sin(t3))*(2*!PI)/period
  w1 += a[h] * (cos(real_part(k1*conj(com_im)))+1) / 2
  w2 += a[h] * (cos(real_part(k2*conj(com_im)))+1) / 2
  w3 += a[h] * (cos(real_part(k3*conj(com_im)))+1) / 2
endfor

im = w1 * w2 * w3

;im = fix(((im*2-1)*amplitude)+offset)
im = ((im)*amplitude)+offset
im += (randomu(1,xarr_size,yarr_size) - 0.5 ) * 2 * amplitude * noise_amplitude_multiplier
return,fix(im)
end



function get_initial_xy, image,radius
    filtered_image=fourier_bpass(image,radius)
;    filtered_image=image
    tv,filtered_image
    ;print,filtered_image[0:8,0:8]

    localmax = ((filtered_image ge shift(filtered_image,1,0)) and  $
              (filtered_image ge shift(filtered_image,-1, 0)) and $
              (filtered_image ge shift(filtered_image, 0, 1)) and $
              (filtered_image ge shift(filtered_image, 0,-1)) and $
              (filtered_image ge shift(filtered_image,-1,-1)) and $
              (filtered_image ge shift(filtered_image,-1, 1)) and $
              (filtered_image ge shift(filtered_image, 1,-1)) and $
              (filtered_image ge shift(filtered_image, 1, 1)))
    tv,bytscl(localmax)
    where_max = where(localmax)
    ;return,where_max
    x_size = (size(image))[1]
    return_value = transpose(float([[where_max MOD x_size],[where_max / x_size]]))
    return,return_value
end

function build_average_profile,image,xy,dominant_length

    scale_factor = 4

    x_size = (size(image))[1]
    y_size = (size(image))[2]
    num_features = (size(xy))[2]
;    area_per_feature = x_size*y_size / num_features
;    profile_size = sqrt(area_per_feature)
    profile_size = dominant_length

    profile=fltarr(2*profile_size*scale_factor)
    count=lon64arr(2*profile_size*scale_factor)
    r_array = indgen(2*profile_size*scale_factor,/float) / scale_factor

    for n = 0L, (num_features) - 1 do begin
;    for n = 138,138 do begin
       x = xy[0,n]
       y = xy[1,n]
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

;    sigma = 1.0 / sqrt(count[where(count gt 0)])  ;use this definition for weights in lmfit
    sigma = float(count[where(count gt 0)]) ;use this definition if using curvefit

   local_mins = where((profile le shift(profile,-1)) and (profile le shift(profile,1)))
    tmp = min( abs(r_array[local_mins] - dominant_length/2.0), nearest_min_idx)
    nearest_min = local_mins[nearest_min_idx]

    first_local_min = local_mins[0]
    first_local_min = max([first_local_min,2])

    full_return_value = [[r_array],[profile],[sigma]]
    return_value = full_return_value[0:nearest_min,*]

    ;plot,profile
    return,return_value
end

pro refine5, image, feature_data, distance_from_center
common shared_xypairs, xy_pairs

    a=dblarr(5); "coefficient" box

    image_size=size(image,/dimensions)
    distance_from_center_squared = distance_from_center*distance_from_center
    max_points = ((distance_from_center+1)*2)^2

    for n= 0L, (size(feature_data))[2]-1 do begin  ;for each prospective sphere
;    for n= 200, 300 do begin  ;for each prospective sphere
        x=feature_data[0,n]
        y=feature_data[1,n]
        xy_pairs = COMPLEXARR(max_points) ;coordinate superstructure
        Brightnesses = fltarr(max_points)

        count=0
        for j = round(-distance_from_center + x), round(distance_from_center + x) do begin;indecies for pixels in area around previously found center
            for k = round(-distance_from_center + y), round(distance_from_center + y) do begin
                if ((j GE 0) and (j LT image_size[0]) and (k GE 0) and (k LT image_size[1])) then begin
                    dx = (x-j)
                    dy = (y-k)
                    dr2 = dx*dx+dy*dy
                    if dr2 LE distance_from_center_squared then begin
                        xy_pairs[count] = DCOMPLEX(j,k)
                        Brightnesses[count]=image[j,k]
                        count +=1
                    endif
                endif
            endfor
        endfor
        xy_pairs = xy_pairs[0:count-1]
        Brightnesses = Brightnesses[0:count-1]

       a[0] = double(feature_data[0:4,n])

    ;print,a
    indep_points=indgen((size(xy_pairs))[1])
    ;help,indep_points
    ;print,"indep points = ", indep_points
    ;help,Brightnesses
    if (count ge 6) then begin
 ;      Best_values = LMFIT(indep_points, Brightnesses, a, chisq=chisq,FITA = [1,1,1,1,1],FUNCTION_NAME = 'sigmoid_2d',iter=iter, $
 ;                           itmin=10,convergence = convergence,itmax=500,TOL=0.001)
       Best_values = CURVEFIT(indep_points, Brightnesses, weights, a, chisq=chisq,FITA = [1,1,0,0,0],FUNCTION_NAME = 'sigmoid_2d_pro',iter=iter, $
                            status = status,itmax=100,/double)
       convergence = 1 - status
    endif else begin
       convergence = 0
       iter=0
       chisq = 0
    endelse

;print,n,count, convergence,iter,chisq,x,y, a

    feature_data[0,n]=a  ;this fills in valus 0 through 4
    feature_data[5,n]=chisq
    feature_data[6,n]=convergence

    if (((a[0] - x)^2 + (a[1] - y)^2) gt 4*distance_from_center_squared) then begin
       feature_data[6,n] = 0.0
       ;print,'wiped out.'
       endif

    endfor  ;for n
end

pro refine9_old, image, feature_data, distance_from_center
common shared_xypoints, xpoints, ypoints

    arr_size = ceil(distance_from_center) * 2 + 1
    xpoints = fltarr(arr_size,arr_size)
    ypoints = fltarr(arr_size,arr_size)
    weights = fltarr(arr_size,arr_size)
    xbase = (findgen(arr_size) - ceil(distance_from_center) + 1)#replicate(1.0,arr_size)
    ybase = replicate(1.0,arr_size) #(findgen(arr_size) - ceil(distance_from_center) + 1)

    distance = sqrt(xbase^2 + ybase^2)
    ;points = where(distance le distance_from_center)
    points=indgen(arr_size*arr_size)
    float_image = float(image)

    a=dblarr(5); "coefficient" box

    image_size=size(image,/dimensions)
    distance_from_center_squared = distance_from_center*distance_from_center
    max_points = ((distance_from_center+1)*2)^2

    minxy = ceil(distance_from_center) +2
    maxx =  image_size[0] - minxy
    maxy =  image_size[1] - minxy

    for n= 0L, (size(feature_data))[2]-1 do begin  ;for each prospective sphere
;    for n= 139, 139 do begin
        x=feature_data[0,n]
        y=feature_data[1,n]
        if ((x GE minxy) and (y ge minxy) and (x le maxx) and (y le maxy)) then begin
            xpoints = xbase + round(x)
            ypoints = ybase + round(y)
            ;full_brightnesses = bilinear(float_image,xpoints,ypoints)
            full_brightnesses = float_image[xpoints[0]:xpoints[0]+arr_size-1,ypoints[0]:ypoints[0]+arr_size-1]
            ;print,full_brightnesses
            weights=fltarr(arr_size,arr_size)
            circle_areas,x-floor(x)+ceil(distance_from_center),y-floor(y)+ceil(distance_from_center),distance_from_center,weights
            ;print,"weights = "
            ;print,weights
            use_points = where(weights gt 0.0)
            full_brightnesses=full_brightnesses[use_points]
            weights=weights[use_points]
            xpoints=xpoints[use_points]
            ypoints=ypoints[use_points]
            points = indgen((size(use_points))[1])
            ;points=use_points
;            Brightnesses = (bilinear(float_image,xpoints,ypoints))[points]
            ;Brightnesses = (interpolate(float_image,xpoints[*,0],reform(ypoints[0,*]),/grid,cubic=-0.5))[points]
            a[0] = double(feature_data[0:4,n])

            Best_values = CURVEFIT(points, full_brightnesses, weights, a, chisq=chisq,FITA = [1,1,0,0,0],FUNCTION_NAME = 'sigmoid_2d_pro',iter=iter, $
                                status = status,itmax=100,/double)
            convergence = 1 - status
            feature_data[0,n]=a  ;this fills in valus 0 through 4
            feature_data[5,n]=chisq
            feature_data[6,n]=convergence
            if (((a[0] - x)^2 + (a[1] - y)^2) gt 4*distance_from_center_squared) then begin
                feature_data[6,n] = 0.0
                ;print,'wiped out.'
                endif
;print,n,convergence,iter,chisq,x,y, a

            endif


    endfor  ;for n
end

pro refine9, image, feature_data, distance_from_center
common shared_xypoints, xpoints, ypoints

t0 = systime(/seconds)
    arr_size = ceil(distance_from_center) * 2 + 1
    xpoints = fltarr(arr_size,arr_size)
    ypoints = fltarr(arr_size,arr_size)
    weights = fltarr(arr_size,arr_size)
    xbase = (findgen(arr_size) - ceil(distance_from_center) + 0)#replicate(1.0,arr_size)
    ybase = replicate(1.0,arr_size) #(findgen(arr_size) - ceil(distance_from_center) + 0)

    distance = sqrt(xbase^2 + ybase^2)
    ;points = where(distance le distance_from_center)
    points=indgen(arr_size*arr_size)
    float_image = float(image)

    a=dblarr(5); "coefficient" box

    image_size=size(image,/dimensions)
    distance_from_center_squared = distance_from_center*distance_from_center
    max_points = ((distance_from_center+1)*2)^2

    minxy = ceil(distance_from_center) +2
    maxx =  image_size[0] - minxy
    maxy =  image_size[1] - minxy
    ;feature_data[0,228]=11.6
    ;feature_data[1,228]=6.1


    for n= 0L, (size(feature_data))[2]-1 do begin  ;for each prospective sphere
;    for n= 139, 139 do begin
        x=feature_data[0,n]
        y=feature_data[1,n]
        if ((x GE minxy) and (y ge minxy) and (x le maxx) and (y le maxy)) then begin
            xpoints = xbase + round(x)
            ypoints = ybase + round(y)
            ;full_brightnesses = bilinear(float_image,xpoints,ypoints)
            full_brightnesses = float_image[xpoints[0]:xpoints[0]+arr_size-1,ypoints[0]:ypoints[0]+arr_size-1]
            ;print,full_brightnesses

            ;in this version, no circle areas.  Instead, a really rough calculation
            ;circle_areas,x-floor(x)+ceil(distance_from_center),y-floor(y)+ceil(distance_from_center),distance_from_center,weights

            distance = sqrt((xbase+(round(x)-x))^2 + (ybase+(round(y)-y))^2)
            weights[where(distance lt (distance_from_center-0.5))] = 1.0
            weights[where(distance gt (distance_from_center+0.5))] = 0.0
            w5 = where((distance ge (distance_from_center-0.5)) and (distance le (distance_from_center+0.5)))
            weights[w5] = distance_from_center - distance[w5] + 0.5



            ;print,"weights = "
            ;print,weights
            ;use_points = where(weights gt 0.0)
            ;full_brightnesses=full_brightnesses[use_points]
            ;weights=weights[use_points]
            ;xpoints=xpoints[use_points]
            ;ypoints=ypoints[use_points]
            ;points = indgen((size(use_points))[1])
            ;points=use_points
;            Brightnesses = (bilinear(float_image,xpoints,ypoints))[points]
            ;Brightnesses = (interpolate(float_image,xpoints[*,0],reform(ypoints[0,*]),/grid,cubic=-0.5))[points]
            a[0] = double(feature_data[0:4,n])

    ;weights = weights[points]
    ;full_brightnesses = full_brightnesses[points]
    weights = reform(weights,arr_size*arr_size,/overwrite)
    full_brightnesses = reform(full_brightnesses,arr_size*arr_size,/overwrite)

            Best_values = CURVEFIT(points, full_brightnesses, weights, a, chisq=chisq,FITA = [1,1,1,1,1],FUNCTION_NAME = 'sigmoid_2d_pro',iter=iter, $
                                status = status,itmax=100,/double)
            convergence = 1 - status
            feature_data[0,n]=a  ;this fills in valus 0 through 4
            feature_data[5,n]=chisq
            feature_data[6,n]=convergence
            if (((a[0] - x)^2 + (a[1] - y)^2) gt 4*distance_from_center_squared) then begin
                feature_data[6,n] = 0.0
                ;print,'wiped out.'
                endif
;print,n,convergence,iter,chisq,x,y, a

            endif


    endfor  ;for n
t1 = systime(/seconds)
print,"refine complete time: ",t1-t0
end

pro weed_out_bad_points, image,feature_data

    limits = [[0.0                            ,(size(image))[1]                 ],$  ;x coordinate
              [0.0                            ,(size(image))[2]                 ],$  ;y coordinate
              [0.5 * median(feature_data[2,*]),  3.0 * median(feature_data[2,*])],$  ;width
              [0.2 * median(feature_data[3,*]), 10.0 * median(feature_data[3,*])],$  ;amplitude parameter
              [0.2 * median(feature_data[4,*]), 10.0 * median(feature_data[4,*])],$  ;offset parameter
              [0.0 * median(feature_data[5,*]), 10.0 * median(feature_data[5,*])]]   ;chi squared parameter

    for i = 0,5 do begin
        w1 = where(feature_data[i,*] lt limits[0,i],w1_count)
        w2 = where(feature_data[i,*] gt limits[1,i],w2_count)
;       print,'deleting: lt',i
       if (w1_count gt 0) then begin
            feature_data[6,w1]=0
 ;           print,feature_data[*,w1])
            endif
;       print,'deleting: gt',i
       if (w2_count gt 0) then begin
            feature_data[6,w2]=0
 ;           print,feature_data[*,w2])
            endif
        endfor
    w3 = where(feature_data[6,*] gt 0.5, w3_count)
    if (w3_count gt 0) then begin
;        print,'deleted all of the following'
 ;       print,feature_data[*,w3])
        feature_data=feature_data[*,w3]
        endif
;    print,'kept all of the following'
;    print,feature_data

end

pro remove_duplicates,f,dominant_length
    tolerance = 0.2 * dominant_length
    ;first, sort the array, by y, which it already mostly is.
    f[0,0] = f[*,sort(f[1,*])]

    N = (size(f))[2]
    for i = 0L, N-1 do begin
       j_limit = f[1,i] + tolerance
       for j = i+1,N-1 do begin
         ;print,i,j
         if (f[1,j] gt j_limit) then break ; if j coordinate is 2 away, no need to go any further.
         r2 = (f[0,i]-f[0,j])^2 + (f[1,i]-f[1,j])^2
         if r2 lt tolerance then begin
            if f[5,i] gt f[5,j] then begin
               f[6,j] = 0.0
               ;print,'removing ',j
            endif else begin
               f[6,i]=0.0
               ;print,'removing ',i
               break
            endelse
         endif
         endfor ;j
       endfor ;i
    w3 = where(f[6,*] gt 0.5, w3_count)
    if (w3_count gt 0) then begin
;        print,'deleted all of the following'
 ;       print,transpose(feature_data[*,w3])
        f=f[*,w3]
        endif
;    print,'kept all of the following'
;    print,f
end

pro show_locations_old,image,feature_data
    xsize = (size(image))[1]
    ysize = (size(image))[2]
    location_image = bytarr(xsize,ysize)
    for n= 0L, (size(feature_data))[2]-1 do begin  ;for each prospective sphere
        x = round(feature_data[0,n])
        y = round(feature_data[1,n])
        if (x ge 0) and (x lt xsize) and (y ge 0) and (y lt ysize) then begin
            location_image[x,y]=255
            endif
        endfor
 write_tiff,'big_out.tif',location_image
   ;tv,congrid(location_image,512,512)

    return
end

pro show_locations,image,feature_data
    xsize = (size(image))[1]
    ysize = (size(image))[2]
    scale_factor = 512.0 / max([xsize,ysize])
    x_values = reform(feature_data[0,*]) * scale_factor
    y_values = reform(feature_data[1,*]) * scale_factor
    ;plot,x_values,y_values,COLOR=128,psym=6
    window,5,xsize=512+140,ysize=512+140

    plot,[0],[0],/nodata,xrange=[0,512],yrange=[0,512],xstyle=1,ystyle=1,position=[100,100,100+512,100+512],/device
    tv,congrid(image,512,512),100,100

    oplot,x_values,y_values,COLOR=128,psym=3,thick=2

    return
end

pro find_features, image
    xy_data = get_initial_xy(image,dominant_length)

    feature_data = fltarr(7,(size(xy_data))[2])
    feature_data[0,0]=xy_data
    for i=0,3 do begin
print,'building profile'
        average_profile = build_average_profile(image,feature_data,dominant_length)
        r_values = average_profile[*,0]
        z_values = average_profile[*,1]
        errors = average_profile[*,2]

        a = fltarr(3)
        profile_size = (size(z_values))[1]
        a[0] = r_values[profile_size / 3]
        a[2] = z_values[profile_size-1]
        a[1] = z_values[0] - a[2]

;        z_fit_values = LMFIT(r_values, z_values, a, measure_errors = errors,chisq=chisq,FITA = [1,1,1],FUNCTION_NAME = 'sigmoid_1d',iter=iter, itmin=20,convergence = convergence,itmax=100,/double);
        z_fit_values = CURVEFIT(r_values, z_values, errors, a, chisq=chisq,FITA = [1,1,1],FUNCTION_NAME = 'sigmoid_1d_pro',iter=iter, itmax=100,/double);

;    plot,r_values,z_values,psym=6
;    oplot,r_values,z_fit_values,COLOR=128
;    print,a

;       ;****little piece of test code
;       nn0 = 201
;       nn=nn0
;       for xx = 0,10 do begin
;           for yy = 0,10 do begin
;              nn+=1
;              feature_data[0,nn] =  feature_data[0,nn0] + (xx*.01)
;              feature_data[1,nn] =  feature_data[1,nn0] + (yy*.01)
;              endfor
;           endfor
;       ;********* end of little piece of test code

       feature_data[2,*]=a[0]
       feature_data[3,*]=a[1]
       feature_data[4,*]=a[2]


show_locations,image,feature_data

print,'refining'
       refine9,image,feature_data,r_values[profile_size-1]
       ;refine9,image,feature_data,r_values[profile_size-1]
       ;refine5,image,feature_data,r_values[profile_size-1]+1
print,'done refining'


show_locations,image,feature_data
;       weed_out_bad_points,image,feature_data
;show_locations,image,feature_data

;take away the parts too close to the edges
minx = min(feature_data[0,*]) + 10
miny = min(feature_data[1,*]) + 10
maxx = max(feature_data[0,*]) - 10
maxy = max(feature_data[1,*]) - 10
w4 = where((feature_data[0,*] gt minx) and (feature_data[1,*] gt miny) and (feature_data[0,*] lt maxx) and (feature_data[1,*] lt maxy),w4_count)
    if (w4_count gt 0) then begin
        feature_data_hist=feature_data[*,w4]
        endif


hist_data = reform(feature_data_hist[0,*]-floor(feature_data_hist[0,*]))
plot,histogram(hist_data,min=0,binsize=0.02)
oplot,histogram(feature_data_hist[1,*]-floor(feature_data_hist[1,*]),min=0,binsize=0.02),color=128
    endfor

    remove_duplicates,feature_data,dominant_length
;show_locations,image,feature_data
;show_locations_old,image,feature_data

;    print,'kept all of the following'
;    print,feature_data[*,200:600]

print, 'program "find_features" has located: ',(size(feature_data))[2], ' points.'

end

pro circle_areas,x0_in,y0_in,r,temp_out_area
;this procedure calculates the fraction of each pixel covered by a circle of radius r centered at (x,y)
x0 = x0_in ;+ 0.5
y0 = y0_in ;+ 0.5
do_graphs = 0
;given a circle, return the region centered around it with fractional areas covered.
;first, find all y intersection points
out_area_size = 2*r+1
out_area  = fltarr(out_area_size,out_area_size)
y1 = findgen(floor(y0+r) - ceil(y0-r)+1) + ceil(y0-r)
r2my2 = r^2 - (y1-y0)^2
w_r2my2 = where(r2my2 lt 0.0,wcount)
if wcount ne 0 then r2my2[w_r2my2]=0.0  ;this is necessary because rounding error sometimes makes r^2 - x^2 < 0.0
x1 = x0+sqrt(r2my2)
x2 = x0-sqrt(r2my2)

;now find all x intersection points
x3 = findgen(floor(x0+r) - ceil(x0-r)+1) + ceil(x0-r)
r2mx2 = r^2 - (x3-x0)^2
w_r2mx2 = where(r2mx2 lt 0.0,wcount)
if wcount ne 0 then r2mx2[w_r2mx2]=0.0
y3 = y0+sqrt(r2mx2)
y4 = y0-sqrt(r2mx2)

x = [x1,x2,x3,x3,x0-r,x0+r]
y = [y1,y1,y3,y4,y0,y0]
theta = atan(y-y0,x-x0)
order = sort(-theta)
theta = theta[order]
x = x[order]
y = y[order]
x = [x,x[0]] ; make a copy of the first element at the end.
y = [y,y[0]]
theta = [theta,theta[0]]

r2mx2 = r^2-(x-x0)^2
w_r2mx2 = where(r2mx2 lt 0.0,wcount)
if wcount ne 0 then r2mx2[w_r2mx2]=0.0  ;this is necessary because rounding error sometimes makes r^2 - x^2 < 0.0
;asinxmx0r = asin((x-x0)/r)
asinxmx0r = -abs(theta) + (!PI / 2.0)  ;this is an interesting fix to a bug. asin(x-x0/r) can fail when rounding
;error makes the argument of asin greater than 1.  Since I have the angle anyway (from x and y), this is a better way to do it.

;print,asin((x-x0)/r),asinxmx0r
integral = 0.5*((x-x0)*sqrt(r2mx2) + r^2 * asinxmx0r)
;integral = 0.5*((x-x0)*sqrt(r^2-(x-x0)^2) );+ r^2 * asin((x-x0)/r))
area = shift(integral,-1) - integral
dx = shift(x,-1) - x
miny = y
wmin = where(shift(y,-1) lt y)
miny[wmin] = (shift(y,-1))[wmin]
;print,x[0:14],y[0:14],integral[0:14],area[0:14],dx[0:14],theta
;print,x,y,integral,area,dx,theta

out_indices_x = floor(x)-floor(x0-r)
out_indices_y = floor(miny)-floor(y0-r)

;print,out_indices_x,out_indices_y
if (do_graphs gt 0) then begin
  plot,x,y,psym=7,xticklen=1,yticklen=1,xrange=[0,10],yrange=[0,10],xTICKINTERVAL= 1,yTICKINTERVAL= 1
  endif

;temp_out_area = fltarr(10,10)
;for i=0,(size(x))[1]-2 do begin
;for i=0,9 do begin
i=0
while(theta[i] gt 0) do begin
  xi = floor(x[i])
  top_yi = ceil(max([y[i],y[i+1]]))-1
  bott_yi = floor(y0)

  ;bottom part
  if (top_yi gt bott_yi) then begin
     ;temp_out_area[xi,bott_yi] += dx[i]*(ceil(y0)-y0)
     temp_out_area[xi,bott_yi] += dx[i]*(floor(y0)+1-y0)
;     print,"bottom: ", i,x[i],y[i],xi,bott_yi
     endif

  ;middle part
  for yi = bott_yi + 1,top_yi -1 do begin
    temp_out_area[xi,yi] += dx[i]
;    print,"middle: ",i,x[i],y[i],xi,yi
    endfor ;yi

  ;top part:
  base_height = max([(top_yi - y0),0])
  temp_out_area[xi,top_yi] += area[i] - dx[i]*base_height
;  print,"top:    ",i,x[i],y[i],xi,top_yi," base_height =",base_height,temp_out_area[xi,top_yi]
  ;print," "
  i+=1
  endwhile
;print,"area calculated so far: ", total(temp_out_area)
;print,"what I should have calculated: ", !PI*0.5*r^2

;Next, do the bottom part of the circle.
;I'm keeping the variable names "top" and "bottom" the same here, even though "top" is below bottom now.
;print,"NOW DOING THE BOTTOM"

while(i lt (size(x))[1] - 1) do begin
;while(1000 lt (size(x))[1] - 1) do begin
  xi = floor(x[i+1])
  top_yi = floor(min([y[i],y[i+1]]))
  bott_yi = floor(y0)

  ;bottom part
  if (top_yi lt bott_yi) then begin
     temp_out_area[xi,bott_yi] += dx[i]*(floor(y0)-y0)
;     print,"bottom: ", i,x[i],y[i],xi,bott_yi,dx[i],temp_out_area[xi,bott_yi]
     endif

  ;middle part
  for yi = bott_yi - 1,top_yi +1,-1 do begin
    temp_out_area[xi,yi] += - dx[i]
;    print,"middle: ",i,x[i],y[i],xi,yi,dx[i]
    endfor ;yi

  ;top part:
  base_height = max([(y0-top_yi-1),0])
;  print,"top:    ",i,x[i],y[i],xi,top_yi," base_height =",base_height
  temp_out_area[xi,top_yi] += - area[i] + dx[i]*base_height
;  print," "
  i+=1
  endwhile
  ;endfor ;i

;print,"final area calculated : ", total(temp_out_area)
;print,"what I should have calculated: ", !PI*r^2

delta = total(temp_out_area) / (!PI*r^2) - 1.0

;print,delta
if not(abs(delta) lt .001) then begin
  print,"WARNING: PROBLEM IN CIRCLE AREAS"
  print, x0,y0,r,total(temp_out_area),!PI*r^2,delta
  endif

;for i= 0,15 do begin
;  xi = out_indices_x[i]
;  yi = out_indices_y[i]
;  print,xi,yi
;  out_area[out_indices_x[i],out_indices_y[i]] += area[i] - dx[i]*(miny[i] - y0)
;  for j = yi-1, floor(y0)-floor(y0-r)+1,-1 do begin
;    out_area[xi,j] += dx[i]
;  endfor
;  if out_indices_y[i] gt floor(y0)-floor(y0-r) then begin
;    out_area[xi,floor(y0)-floor(y0-r)] += dx[i] * (1. - y0 + floor(y0))
;  endif



graph_size = (size(temp_out_area))[1]
if (do_graphs gt 0) then begin
    for ii=0,graph_size-1 do begin
      for jj = 0,graph_size-1 do begin
        xyouts,ii+0.15,jj+0.5,string(temp_out_area[ii,jj],format='(F6.4)')
        endfor
      endfor
    endif
;for ii=0,out_area_size-1 do begin
;  for jj = 0,out_area_size-1 do begin
;    xyouts,1+ii+0.15,2+jj+0.5,string(out_area[ii,jj],format='(F6.4)')
;    endfor
;  endfor



  ;for jy = out_indices_y[i], y0, -1 do begin  ; proper endpoint for this?
  ;  out_area[out_area[out_indices_x[i],jy]  += dx[i]  ; check indices!
  ;  endfor
  ; fix up the last bit
  ; add dx[i] * 1-y0 to the right square


  ;endfor

;area_out[, one down] += dx
;for each pair, find area under curve (better put endpoints at 0, pi)
;area = integral(x1 to x2)
;y_just_below = floor(min(y,nexty))
;block area = (xnext-x)*(y_just_below-y0)
;out_area[,] +=
;out_area[,-1] += dx
;actual area is total under curve minus total for next y area down.
;figure out which are pixel gets the area
;print,reverse(out_area,2)
;plot,x,y,psym=7,xticklen=1,yticklen=1,xrange=[0,10],yrange=[0,10],xTICKINTERVAL= 1,yTICKINTERVAL= 1
;for ii=0,out_area_size-1 do begin
;  for jj = 0,out_area_size-1 do begin
;    xyouts,1+ii+0.15,2+jj+0.5,string(out_area[ii,jj],format='(F6.4)')
;    endfor
;  endfor


end

pro test_circle_areas
for i=3.1, 4.2,0.1 do begin
  for j = 4.1, 5.2, 0.1 do begin
    for r = 0.50, 2.9, 0.01 do begin
      circle_areas,i,j,r
      endfor
    endfor
  endfor
print,"done with test"
end

pro test_circle_areas2
for x=5.1, 44.2,0.1 do begin
  for y = 2.9, 3.2, 0.1 do begin
    for distance_from_center = 2.750, 2.9, 0.1 do begin
    arr_size = ceil(distance_from_center) * 2 + 1
      area = fltarr(arr_size,arr_size)

      circle_areas,x-floor(x)+ceil(distance_from_center),y-floor(y)+ceil(distance_from_center),distance_from_center,area
      endfor
    endfor
  endfor
print,"done with test"
end