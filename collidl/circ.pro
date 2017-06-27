;
;	sets the usersymbol (8) to be an open circle
;	this function returns the number 8 so you can
;	just say psym = circle( ... ) in the plot command	
;
function circ,radius = radius, thick = thick, fill = fill,$
	 dx = dx, dy = dy, left = left, right = right

if not keyword_set( radius ) then radius = 1.
if not keyword_set( thick ) then thick = 1.
if not keyword_set( dx ) then dx = 0.
if not keyword_set( dy ) then dy = 0.

t=findgen(37) * (!pi*2/36)

if keyword_set( right ) then t = t(0:18)
if keyword_set( left ) then t = [t(18:*),t(0)]

x = sin(t)*radius + dx
y = cos(t)*radius + dy

if keyword_set( fill ) then $
	usersym,x,y,/fill $
else	usersym,x,y,thick=thick

return,8	
end
