function g,x

return,exp(-(x/3.0)^4)
end


x = indgen(40) - 20
y = g(x)
fit_vals = gaussfit(x,y,a,nterms=3)
x_all = (findgen(800) -400) / 20

z = (x_all-a[1]) / a[2]
y_all = a[0]*exp(-z^2 / 2)
window,4 & wset,4 & plot,x_all,g(x_all),xrange=[-20,20],yrange =[0.0,1.5] & oplot,x,y,psym=7 & oplot, x_all,y_all
window,6 & wset,6 & plot,x_all,g(x_all),xrange=[-2,2],yrange =[0.9,1.1] & oplot,x,y,psym=7 & oplot, x_all,y_all


x_shift = x + 0.3
y_shift = g(x_shift)
fit_vals_shift = gaussfit(x_shift,y_shift,a_shift,nterms=3)

z = (x_all-a_shift[1]) / a_shift[2]
y_all_shift = a_shift[0]*exp(-z^2 / 2)
window,5 & wset,5 & plot, x_all,g(x_all),xrange=[-20,20],yrange =[0,1.5] & oplot,x_shift,y_shift,psym=7 & oplot, x_all,y_all_shift
window,7 & wset,7 & plot, x_all,g(x_all),xrange=[-2,2],yrange =[0.9,1.1] & oplot,x_shift,y_shift,psym=7 & oplot, x_all,y_all_shift

print,"a:", a
print,"a_shift:", a_shift
end
