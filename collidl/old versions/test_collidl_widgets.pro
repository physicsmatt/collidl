pro toggle_spheres, w, state
  w['SPHERES'].hide = 1 - state
end
  
function collidl_keyboard, w, IsASCII, Character, KeyValue, X, Y, Press, Release, KeyMods
  if Release then return, 1
  if not isascii then return, 1
  if character ge 65 and character le 90 then character += 32B ;converts uppercase ascii to lowercase ascii 
  print,"key hit is ", character
  inputstr = string(character)
  WIDGET_CONTROL, w.uvalue.annotationwidget, get_value = ann_state
  if inputstr eq 's' then begin
    ann_state[0] = 1 - ann_state[0]
    toggle_spheres, w, ann_state[0]
  endif
  WIDGET_CONTROL, w.uvalue.annotationwidget, set_value = ann_state
end

function collidl_mouse_down, w, X, Y, Button, KeyMods, Clicks
  *w.uvalue.movehomemousexy = [x,y]
  *w.uvalue.movehomecenterxy = *w.uvalue.centerxy
  *w.uvalue.mousedown = 1
end

function collidl_mouse_up, w, X, Y, Button
  *w.uvalue.mousedown = 0
end

function collidl_mouse_motion, w, X, Y, KeyMods
;  print,"Button down: ",button
;  print,x,y
  if *w.uvalue.mousedown eq 1 then begin
    img = w['RAW_IMG']
    moveby = [x,y] - *w.uvalue.movehomemousexy
    movedby = *w.uvalue.centerxy - *w.uvalue.movehomecenterxy
    yet_to_move = moveby - movedby
    kludgefact = (1/*w.uvalue.xyzscale)
    img.translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device  
    *w.uvalue.centerxy += yet_to_move
  endif
end

function collidl_mouse_wheel, w, x, y, d, keymods
  ;print,"wheel: ", d
  ;print,x,y
  ;regular scroll should scroll image, eventually.  Not emplemented.
  ;control scroll zooms image;
  img = w['RAW_IMG']
  if (keymods eq 2) then begin ;actually it's a bit mask
    centerxy = *w.uvalue.centerxy
    if d gt 0 then scalefact = 1.5 ;zoom in
    if d lt 0 then scalefact = .6667 ;zoom out
    ;cursor position would be shifted by this much:
    shiftedby = ([x,y] - centerxy) * (scalefact - 1)
    ;print,shiftedby
    img.scale, scalefact, scalefact, scalefact
    *w.uvalue.xyzscale *= scalefact
    kludgefact = (1/*w.uvalue.xyzscale)
    img.translate, -shiftedby[0] * kludgefact, -shiftedby[1] * kludgefact, /device ;make correction, so area under cursor does not move with scale change
    *w.uvalue.centerxy -= shiftedby
  endif
  return,0 ; skip default handling; no idea if this is really needed
end

function collidl_selection_change, w, X, Y, Button, KeyMods, Clicks
  print,"The selection change handler got called!"
end

function img_button_event_handler, event
  wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
  WIDGET_CONTROL, wDraw, GET_VALUE = w
  if event.VALUE eq 'RAW' then p=w['RAW_IMG']
  if event.VALUE eq 'FILTERED' then p=w['FILTERED']
  p.hide= 1 - event.SELECT
end

function annotation_button_event_handler, event
  ;print,"annotation event"
  ;print,event
  ;print, TAG_NAMES(event)
  ;print, TAG_NAMES(event, /STRUCTURE_NAME)
  wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
  WIDGET_CONTROL, wDraw, GET_VALUE = w
  if event.value eq 'SPHERES' then toggle_spheres, w, event.select

;  wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
;  WIDGET_CONTROL, wDraw, GET_VALUE = w
;  if event.VALUE eq 'RAW' then p=w['RAW_IMG']
;  if event.VALUE eq 'FILTERED' then p=w['FILTERED']
;  p.hide= 1 - event.SELECT
end

PRO collidl_widget_EVENT, event
  print,"single button event"
print,event
print, TAG_NAMES(event)
print, TAG_NAMES(event, /STRUCTURE_NAME)


  CASE TAG_NAMES(event, /STRUCTURE_NAME) OF
    'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, event.id, GET_UVALUE = event_UV

      ; Retrieve the Widget Window
      wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
      WIDGET_CONTROL, wDraw, GET_VALUE = w

      ; Retrieve the plot with the NAME
      ; provided on plot creation
      p = w['RAW_IMG']
      CASE event_UV OF
        'DONE': WIDGET_CONTROL, event.top, /DESTROY
        'RED': p.color='red'
        'BLUE': p.color='blue'
        ELSE: ; do nothing
      ENDCASE
    END
    'CW_BGROUP': BEGIN
      print,"button group hit"
    end
    
    'WIDGET_BASE': begin
      ; Handle base resize events. Retrieve our cached padding,
      ; and our new size.
      WIDGET_CONTROL, event.id, GET_UVALUE=pad, TLB_GET_SIZE=newSize
      wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME='DRAW')
      ; Change the draw widget to match the new size, minus padding.
      xy = newSize - pad
      WIDGET_CONTROL, wDraw, $
        DRAW_XSIZE=xy[0], DRAW_YSIZE=xy[1], $
        SCR_XSIZE=xy[0], SCR_YSIZE=xy[1]
    end

    ELSE: ; do nothing
  ENDCASE
END

PRO test_collidl_widgets
  base1 = WIDGET_BASE(/COLUMN, TITLE='Test Collidl', $
    /TLB_SIZE_EVENTS)

  ; Create the base for the button:
  base2 = WIDGET_BASE(base1, /ROW, /ALIGN_CENTER)

  ; Create the action buttons.
  bgroup_images = CW_BGROUP(base2, ['Raw','Filtered'], /COLUMN, /EXCLUSIVE, LABEL_TOP='Images', /FRAME, set_value=0, $
    BUTTON_UVALUE=['RAW','FILTERED'], event_funct='img_button_event_handler')
  bgroup_annotations = CW_BGROUP(base2, ['Spheres','Triangulation','Defects','Orientation'], /ROW, /NONEXCLUSIVE, LABEL_TOP='Annotations', /FRAME, $
    BUTTON_UVALUE=['SPHERES','TRIANGULATION','DEFECTS','ORIENTATION'], event_funct='annotation_button_event_handler')
  done = WIDGET_BUTTON(base2, VALUE = 'Done', UVALUE = 'DONE')

  wDraw = WIDGET_WINDOW(base1, UVALUE='draw', UNAME='DRAW', xsize=800, ysize=800)


  ; Realize the widget (i.e., display it on screen).
  WIDGET_CONTROL, base1, /REALIZE

  ; Register the widget with the XMANAGER, leaving the IDL command
  ; line active.
  XMANAGER, 'collidl_widget', base1, /NO_BLOCK

  ; Cache the padding between the base and the draw
  WIDGET_CONTROL, base1, TLB_GET_SIZE=basesize
  xpad = basesize[0] - 640
  ypad = basesize[1] - 512
  WIDGET_CONTROL, base1, SET_UVALUE=[xpad,ypad]
  WIDGET_CONTROL, bgroup_annotations, set_value=[1,0,1,0]
  WIDGET_CONTROL, bgroup_images, set_value=0
  ;consider widget_control send_event?

  ; Retrieve the newly-created Window object.
  WIDGET_CONTROL, wDraw, GET_VALUE = w

  w.SELECT
  w.MOUSE_DOWN_HANDLER='collidl_mouse_down'
  w.MOUSE_UP_HANDLER='collidl_mouse_up'
  w.MOUSE_MOTION_HANDLER='collidl_mouse_motion'
  w.MOUSE_WHEEL_HANDLER='collidl_mouse_wheel'
  w.KEYBOARD_HANDLER='collidl_keyboard'
  w.SELECTION_CHANGE_HANDLER='collidl_selection_change' ;I don't understand when this function would be called.
  w.uvalue={ $
    basewidget:base1, $
    annotationwidget:bgroup_annotations, $
    mousedown:ptr_new(0), $
    xyzscale:ptr_new(1.0), $
    centerxy:ptr_new([400, 400]), $
    movehomemousexy:ptr_new([0,0]), $
    movehomecenterxy:ptr_new([0,0]) $
    }  

  data = read_tiff('double.tif')
  imglayer=image(data, /current, NAME = 'RAW_IMG', margin=0);, /widgets)
  data_inv = 255-data
  imglayer2 = image(data_inv, /current, NAME = 'FILTERED_IMG', margin=0);, /widgets)
  imglayer2.hide = 1
  spheres = ellipse(100,200, major=10, /current, NAME = 'SPHERES', color='blue', /data)
END
