pro showimage, image, nlayers, base

; Displays an image in a resizable window, nlayers=1 for b/w and 3 for full color. image[3,x,y]

dims = size(image, /dimensions)
;if n_elements(dims) eq 2 then begin
  nx = dims[0]
  ny = dims[1]
;endif

;else begin
; nx = dims[1]
; ny = dims[2]
;endelse

;-------------------------------------------------------------------------------
;- CREATE A WINDOW
;-------------------------------------------------------------------------------

  scroll = 0
  xsize = nx
  ysize = ny
  draw_xsize = nx
  draw_ysize = ny

  ;- Adjust the window size if the image is too large

  device, get_screen_size=screen
  if nx gt (0.75 * screen[0]) then begin
    xsize = 0.75 * screen[0]
    scroll = 1
  endif
  if ny gt (0.75 * screen[1]) then begin
    ysize = 0.75 * screen[1]
    scroll = 1
  endif

  ;- Create the draw widget

  base = widget_base(title=file)
  draw = widget_draw(base, scroll=scroll)
  widget_control, draw, xsize=xsize, ysize=ysize, $
  draw_xsize=draw_xsize, draw_ysize=draw_ysize, sensitive=1




widget_control, base, /realize

;- Save current decomposed mode and display order

device, get_decomposed=current_decomposed
current_order = !order

;- Set image to display from bottom up

!order = 0

;- Display the image

if nlayers eq 1 then begin

  device, decomposed=0
  tv, image

endif else begin

  device, decomposed=1
  tv, image, true=3

endelse

;- Restore decomposed mode and display order

device, decomposed=current_decomposed
!order = current_order


END