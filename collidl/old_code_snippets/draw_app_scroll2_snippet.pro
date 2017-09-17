
;*********************************************************************************
; This code is a workaround for the limitations placed on virtual memory, modified
; from code given by RSI.


; File: draw_app_scroll2.pro
; Syntax: DRAW_APP_SCROLL2, my_large_image_array
; Purpose: Demonstrate how a very large image can be displayed most
; efficiently in IDL. The algorithm uses a scrollable WIDGET_DRAW,
; implemented with the /APP_SCROLL keyword, and displaying just the
; tile of the image array that is needed to fill up the whole viewport
; of the WIDGET_DRAW. Notice how the TVRD() in the event handler reads
; only the displayed subset of the full image.
; Instructions: I tested a dummy random-striped dataset with the
; following code:
; IDL> img = bytarr(8192, 8192)
; IDL> for i = 0, 255 do img[*, i*32:i*32+31] = byte(randomu(seed) * 255)
; IDL> draw_app_scroll2, img

; Event-handler routine. Does nothing in this example.
PRO draw_app_scroll2_event, ev
  COMMON GLOBAL_IMAGE, IMAGE, CURRENT_IMAGE_SUBSET

  ; In this simple app we assume that 'ev.id' must be the WIDGET_DRAW.
  widget_control, ev.id, GET_VALUE=winID
  wset, winID

  ; Check the event type. If the event is a viewport event
  ; (type 3), redraw the image in the viewport using the
  ; new X and Y coordinates contained in the event structure.
  ; Note that we can use this simple check for the value of
  ; the TYPE field because there are no other widgets in this
  ; example to generate events; in a more complex widget
  ; application, a more sophisticated check would be necessary.
  if (ev.TYPE eq 3) then tv, IMAGE[ev.x:ev.x+511, ev.Y:ev.y+511]

  ; Demonstrate TVRD, using CURRENT_IMAGE_SUBSET. Only the
  ; 400x400 block displayed in the scroll window is captured by
  ; TVRD. CURRENT_IMAGE_SUBSET is a global variable that holds
  ; the latest display.
  ;CURRENT_IMAGE_SUBSET = tvrd()
  ;help, CURRENT_IMAGE_SUBSET
  ;window, XSIZE=400, YSIZE=400
  ;tv, CURRENT_IMAGE_SUBSET
END

; Widget creation routine.
PRO draw_app_scroll2, img, nlayers, base    ;this is to replicate the functionality
  ;of the original showimage.pro
  COMMON GLOBAL_IMAGE, IMAGE, CURRENT_IMAGE_SUBSET
  IMAGE = img
  n=size(img)

  base = widget_base()
  ; Create the draw widget. The size of the viewport is set to
  ; 400x400 pixels, but the size of the "virtual" drawable area is
  ; set equal to the dimensions of the image array using the
  ; XSIZE and YSIZE keywords. "/APP_SCROLL" makes sure that your
  ; application does not actually have to find full duplicated
  ; extra storage for the graphics display of the 'image' array.
  wDraw = widget_draw(base, X_SCROLL_SIZE=512, Y_SCROLL_SIZE=512, $
    XSIZE=n[1], YSIZE=n[2], /APP_SCROLL, retain=2)

  widget_control, base, /REALIZE
  widget_control, wDraw, GET_VALUE=drawID
  wset, drawID
  ;device, get_decomposed=current_decomposed
  ;current_order = !order
  ;
  ;- Set image to display from bottom up
  ;
  ;!order = 0
  ;
  ;- Display the image
  ;
  ;if nlayers eq 1 then begin
  ;
  ;  device, decomposed=0
  ;  tv, IMAGE[0:511,0:511,0:2]
  ;
  ;endif else begin
  ;
  ;  device, decomposed=1
  ;  tv, IMAGE[0:511,0:511], true=3
  ;
  ;endelse
  ;
  ;- Restore decomposed mode and display order
  ;
  ;device, decomposed=current_decomposed
  ;!order = current_order

  tv, image[0:512,0:512]

  xmanager, 'draw_app_scroll2', base, /NO_BLOCK

END


;This program is a script to extract the image from an app_scroll widget
;it works by grabbing portions of the window by TVRD


