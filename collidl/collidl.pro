compile_opt strictarr

;This program is supposed to read an image file, determine the sphere positions,
;do the triangulation and generate the orientation field

; MAIN PROGRAM

; Search for current work area to find code changes in progress
; USAGE notes (1/31/2006, by Matt Trawick)
; 1.  Start IDL
; 2. At IDL prompt, type: cd,'C:\Research Data\Collidl'
; 3. File -> Open... collidlv1.48.pro, or whatever the latest version is
; 4. Run -> Compile All
; 5. Run -> Compile All  (Again!)
; 6. At IDL prompt, type: collidl, spheresize=6, /stay (or whatever switches you want).

;v1.49 notes
;   Excised the correlation function, using collidl largely as a data prep mechanism
;   Function now exists as an external applet due to memory usage issues,
;
;
;v1.48 notes (as of 8/09/05):
;   By Matt Trawick
;   Adding a new internal switch, "postscript_defects", which generates a postscript file output of
;   disclinations and dislocations, similar to the old C version.  (Advantage: Postscript has arbitrarily high
;   resolution, so image has potential to look much nicer.  Disadvantage: slow, need to convert manually to .tif
;   file eventually.
;
;v1.45 notes (as of 6/23/05):
;   By Brian Salmons
;
;   This version seeks to eliminate some of the annoying problems inherent in the program and
;   in v1.4.  There are many changes, including:
;     -Now checks whether to close the windows automatically when there are multiple images run through
;     -Now checks to see if any images were actually selected, preventing a crash
;     -Global workaround.  At the cost of slight speed (about two seconds at 4096x4096 pixels),
;      the IDL global variables have been worked around and are passed.  This prevents the need to
;      constantly reset the IDE and allows multiple images to be run in a single pass as originally
;      designed.
;     -Better memory clearing.  Memory clears that were in if-loops have now been moved to exterior locations
;      or cleared in multiple areas.  Some variables include testing to determine whether they are used,
;      indicating whether they should be created or not.
;     -Better memory management.  Some variables used to be created at the beginning that were not used until
;      the end.  Many of these have been fixed and most variables are created when needed and cleared after.
;     -New Switch: /filtered added.  This indicates that the image has already been filtered externally before
;      loading into this application, and thus additional filtering is not needed or may be harmfull to
;      location data.  This tag simply skips the filter process within the code.  Also helps on extremely
;      large images in which there is not enough memory for the fft code in the band pass filter.
;     -Now allows image "trains" (multiple images within one run) to be analyzed (including large images).
;
;;      -Windows only version.  Relies on Virtual Memory, rather than IDL Pixmap for greater range
;
;
;   Feasible maximum image size in this version is 4096x4096, seems regardless of scale.
;
;
;
;
;V1.4 notes (as of 6/22/05):
;   By Brian Salmons
;
;   This version of the program has limitations placed on it by the interface with windows
;   in how it displays large images.  On our test machine, images of 2048x2048 pixels
;   could be displayed in all four windows at once (original, bonds, angle, and all).
;   However, at the next largest power of two (4096x4096 pixels), Windows ran out of virtual
;   memory of the backbuffer.  A backbuffer is needed, due to the save files being generated
;   from the displayed images; without the backbuffer, only what is on screen is saved and
;   the rest is lost.  Similarly, the pixmap buffers also ran out of memory, and thus had
;   to be switched to remain=1 (windows virtual memory), eliminating the functionality on X-windows
;   systems at this size and larger.  To solve this, only two images can be displayed at a time using
;   windows virtual memory (worth checking with IDL pixmap for X-windows functionality, though pixmap overflow
;   forced us to comment out the copy code in the saveimage() routine.  In its current state, it is simply
;   TVRD().  Therefore, it may not be possible to similarly use pixmaps for displaying)).  Therefore,
;   an automatic check is in place to close windows if the open image is larger than 2048x2048.
;
;   In future versions, there will also be a similar switch on display method in the displaying of images,
;   so that small image function can be restored to X-windows machines, while allowing Windows to operate
;   on larger images than pixmap memory allows.  Furthermore, try blocks will be in place to allow
;   backwards compatibility for machines to default from both pixmap and to closing images
;   if memory is not available.
;
;   ALL of these conditions to the operation of this program as found above ARE machine specific.  Different
;   machines are capable of different values, and in this version, code modification is the only way to change
;   for individual machines.  *To be fixed in future versions.
;
;   V1.4 changes:
;      -Now allows any size image, with a feasible maximum of around 4096x4096 pixels, subject to
;          individual machines capabilities.
;      -Memory allocations are not universal.  This allows for drastically faster smaller image
;          analysis and a theoretically infinite top end.  *Requires reseting of IDE to operate on different
;        sized images!*
;      -Forces closing of first two windows (original image and bonds) at image sizes over 2048x2048
;          pixels to free virtual memory.
;      -Closing of non-relevant arrays and some deletions.  Some arrays were unneeded or only temporarily
;          usefull.  These now close after finished to free memory.
;      -New switches:
;             -/Scale: scales the image to 1024.   -or-
;             -Scale=(number): scales the image by a factor of (number).
;             -spheresize=(number): sets the spheresize to (number), rather than arbitray values.
;      -Obsolete switches:
;             -/big          (no longer available)
;             -/simu        (replaced by spheresize)
;             -/twomicron     (replaced by spheresize)
;             -/onemicron     (replaced by spheresize)
;      -Windows only version.  Relies on Virtual Memory, rather than IDL Pixmap for greater range
;
;

pro toggle_spheres, w, state
  w['SPHERES'].hide = 1 - state
end

pro toggle_triangulation, w, state
  w['TRIANGULATION'].hide = 1 - state
end

pro toggle_defects, w, state
  w.refresh, /disable
  foreach defplot, w.uvalue.defect_plot_list do defplot.hide=1-state
  *w.uvalue.hide_defects = 1 - state
  w.refresh
end

pro toggle_orientation, w, state
  w['ORIENT_IMG'].hide = 1 - state
end

function collidl_keyboard, w, IsASCII, Character, KeyValue, X, Y, Press, Release, KeyMods
  if Release then return, 1
  if not isascii then return, 1
  if character ge 65 and character le 90 then character += 32B ;converts uppercase ascii to lowercase ascii
  ;print,"key hit is ", character
  inputstr = string(character)
  if strpos('stdo', inputstr) ge 0 then begin ;character for changing annotations
    ;print,"Hit a key for annotation selection...."
    bgroup_annotations = WIDGET_INFO(w.uvalue.basewidget, find_by_uname='ANN_BUTTONS')
    WIDGET_CONTROL, bgroup_annotations, get_value = ann_state
    ;print, "current state of widget is:", ann_state
    if inputstr eq 's' then begin
      ann_state[0] = 1 - ann_state[0]
      toggle_spheres, w, ann_state[0]
    endif
    if inputstr eq 't' then begin
      ann_state[1] = 1 - ann_state[1]
      toggle_triangulation, w, ann_state[1]
    endif
    if inputstr eq 'd' then begin
      ann_state[2] = 1 - ann_state[2]
      toggle_defects, w, ann_state[2]
    endif
    if inputstr eq 'o' then begin
      ann_state[3] = 1 - ann_state[3]
      toggle_orientation, w, ann_state[3]
    endif
    WIDGET_CONTROL, bgroup_annotations, set_value = ann_state
  endif
  if strpos('rfn', inputstr) ge 0 then begin
    ;print,"Hit a key for img selection...."
    bgroup_images = WIDGET_INFO(w.uvalue.basewidget, find_by_uname='IMG_BUTTONS')
    ;WIDGET_CONTROL, bgroup_images, get_value = img_state
    ;print, "current state of widget is:", img_state
    if inputstr eq 'r' then begin
      WIDGET_CONTROL, bgroup_images, set_value = 0
      w['RAW_IMG'].hide = 0
      w['FILTERED_IMG'].hide = 1
    endif
    if inputstr eq 'f' then begin
      WIDGET_CONTROL, bgroup_images, set_value = 1
      w['RAW_IMG'].hide = 1
      w['FILTERED_IMG'].hide = 0
    endif
    if inputstr eq 'n' then begin
      WIDGET_CONTROL, bgroup_images, set_value = 2
      w['RAW_IMG'].hide = 1
      w['FILTERED_IMG'].hide = 1
    endif
  endif
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
    ;img = w['RAW_IMG']
    moveby = [x,y] - *w.uvalue.movehomemousexy
    movedby = *w.uvalue.centerxy - *w.uvalue.movehomecenterxy
    yet_to_move = moveby - movedby
    kludgefact = (1/*w.uvalue.xyzscale)
    w['RAW_IMG'].translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device
    ;w['SPHERES'].translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device
    ;w['ORIENT_IMG'].translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device
    *w.uvalue.centerxy += yet_to_move
  endif
end

pro rescale_annotations, w
  ;There are many annotation elements (triangulation, defects, circled spheres, etc.) for which the symbol or line size
  ;does not scale automatically with the .scale method.  To make these values scale naturally (when using the scroll wheel,
  ;for example), I keep track of the current xyzscale, and increase their values manually here.
  ;Each time a plot is made with a property that needs to be scaled this way, I add an element to w.uvalue.rescale_list
  ;of the following form: list('sym_size',spheres, 2.5).  (That is, the rescale_list is a list of lists, each with 3 elements.)
  ;element[0] is the property name, element[1] is the plot or graphic object on which it occurs, and element[3] is the correct
  ;value of the property when the scale factor is just 1.
  ;This procedure goes through the list and rescales the properties accordingly.
  foreach element, w.uvalue.rescale_list do begin
    case element[0] of
      'sym_size':   element[1].sym_size =  element[2] * *w.uvalue.xyzscale
      'sym_thick':  element[1].sym_thick = element[2] * *w.uvalue.xyzscale
      'thick':      element[1].thick =     element[2] * *w.uvalue.xyzscale
    endcase
  endforeach
end

function collidl_mouse_wheel, w, x, y, d, keymods
  ;print,"wheel: ", d
  ;print,x,y
  ;regular scroll should scroll image, eventually.  Not emplemented.
  ;control scroll zooms image;
  ;if (keymods eq 2) then begin ;This is a bit mask: 2 corresponds to holding down CTRL.
  centerxy = *w.uvalue.centerxy
  w.refresh, /disable
  if d gt 0 then scalefact = 1.5 ;zoom in
  if d lt 0 then scalefact = .6667 ;zoom out
  *w.uvalue.xyzscale *= scalefact
  ;cursor position would be shifted by this much:
  shiftedby = ([x,y] - centerxy) * (scalefact - 1)
  ;print,shiftedby
  w['RAW_IMG'].scale, scalefact, scalefact, 1
  kludgefact = (1.0/*w.uvalue.xyzscale)
  w['RAW_IMG'].translate, -shiftedby[0] * kludgefact, -shiftedby[1] * kludgefact, /device ;make correction, so area under cursor does not move with scale change
  *w.uvalue.centerxy -= shiftedby
  rescale_annotations,w
  w.refresh
  ;endif
  return,0 ; skip default handling; no idea if this is really needed
end

function collidl_selection_change, w, X, Y, Button, KeyMods, Clicks
  print,"The selection change handler got called!"
end

function img_button_event_handler, event
  wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
  WIDGET_CONTROL, wDraw, GET_VALUE = w
  if event.select eq 0 then return,0
  if event.VALUE eq 'RAW' then begin
    w['RAW_IMG'].hide = 0
    w['FILTERED_IMG'].hide = 1
  endif
  if event.VALUE eq 'FILTERED' then begin
    w['RAW_IMG'].hide = 1
    w['FILTERED_IMG'].hide = 0
  endif
  if event.VALUE eq 'NONE' then begin
    w['RAW_IMG'].hide = 1
    w['FILTERED_IMG'].hide = 1
  endif
end

function annotation_button_event_handler, event
  ;print,"annotation event"
  ;print,event
  ;print, TAG_NAMES(event)
  ;print, TAG_NAMES(event, /STRUCTURE_NAME)
  wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
  WIDGET_CONTROL, wDraw, GET_VALUE = w
  if event.value eq 'SPHERES' then toggle_spheres, w, event.select
  if event.value eq 'TRIANGULATION' then toggle_triangulation, w, event.select
  if event.value eq 'ORIENTATION' then toggle_orientation, w, event.select
  if event.value eq 'DEFECTS' then toggle_defects, w, event.select

  ;  wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
  ;  WIDGET_CONTROL, wDraw, GET_VALUE = w
  ;  if event.VALUE eq 'RAW' then p=w['RAW_IMG']
  ;  if event.VALUE eq 'FILTERED' then p=w['FILTERED']
  ;  p.hide= 1 - event.SELECT
end

PRO collidl_widget_EVENT, event
  ;print,"single button event"
  ;print,event
  ;print, TAG_NAMES(event)
  ;print, TAG_NAMES(event, /STRUCTURE_NAME)


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
      WIDGET_CONTROL, wDraw, GET_VALUE = w
      ; Change the draw widget to match the new size, minus padding.
      xy = newSize - pad
      WIDGET_CONTROL, wDraw, $
        DRAW_XSIZE=xy[0], DRAW_YSIZE=xy[1], $
        SCR_XSIZE=xy[0], SCR_YSIZE=xy[1]
      ;print,"new window size", xy[0], xy[1]
      ;print,"old window size", *w.uvalue.windowsize
      *w.uvalue.centerxy += (xy - *w.uvalue.windowsize) * 0.5
      *w.uvalue.windowsize = xy
    end

    ELSE: ; do nothing
  ENDCASE
END

function create_collidl_widgets
  base1 = WIDGET_BASE(/COLUMN, TITLE='Test Collidl', /TLB_SIZE_EVENTS)

  ; Create the base for the button:
  base2 = WIDGET_BASE(base1, /ROW, /ALIGN_CENTER)

  ; Create the action buttons.
  bgroup_images = CW_BGROUP(base2, ['Raw','Filtered','None'], /ROW, /EXCLUSIVE, LABEL_TOP='Images', /FRAME, set_value=0, $
    BUTTON_UVALUE=['RAW','FILTERED','NONE'], event_funct='img_button_event_handler', UNAME='IMG_BUTTONS')
  WIDGET_CONTROL, bgroup_images, set_value=0
  bgroup_annotations = CW_BGROUP(base2, ['Spheres','Triangulation','Defects','Orientation'], /ROW, /NONEXCLUSIVE, LABEL_TOP='Annotations', /FRAME, $
    BUTTON_UVALUE=['SPHERES','TRIANGULATION','DEFECTS','ORIENTATION'], event_funct='annotation_button_event_handler', UNAME='ANN_BUTTONS')
  WIDGET_CONTROL, bgroup_annotations, set_value=[1,1,1,1]
  done = WIDGET_BUTTON(base2, VALUE = 'Done', UVALUE = 'DONE')

  wDraw = WIDGET_WINDOW(base1, UVALUE='draw', UNAME='DRAW', xsize=800, ysize=800)

  ; Realize the widget (i.e., display it on screen).
  WIDGET_CONTROL, base1, /REALIZE
  WIDGET_CONTROL, wDraw, GET_VALUE = w

  ; Register the widget with the XMANAGER, leaving the IDL command
  ; line active.
  XMANAGER, 'collidl_widget', base1, /NO_BLOCK

  ; Cache the padding between the base and the draw
  WIDGET_CONTROL, base1, TLB_GET_SIZE=basesize
  xpad = basesize[0] - 800;640
  ypad = basesize[1] - 800;512
  WIDGET_CONTROL, base1, SET_UVALUE=[xpad,ypad]
  ;consider widget_control send_event?

  ; Retrieve the newly-created Window object.

  w.SELECT
  w.MOUSE_DOWN_HANDLER='collidl_mouse_down'
  w.MOUSE_UP_HANDLER='collidl_mouse_up'
  w.MOUSE_MOTION_HANDLER='collidl_mouse_motion'
  w.MOUSE_WHEEL_HANDLER='collidl_mouse_wheel'
  w.KEYBOARD_HANDLER='collidl_keyboard'
  w.SELECTION_CHANGE_HANDLER='collidl_selection_change' ;I don't understand when this function would be called.
  w.uvalue={ $
    basewidget:base1, $
    mousedown:ptr_new(0), $
    xyzscale:ptr_new(1.0), $
    centerxy:ptr_new([400, 400]), $
    windowsize:ptr_new([800, 800]), $
    movehomemousexy:ptr_new([0,0]), $
    movehomecenterxy:ptr_new([0,0]), $
    defect_plot_list:list(), $ ;a list of plots of dislocations and disclinations
    hide_defects:ptr_new(0), $
    rescale_list:list() $  ;see note at procedure "rescale_annotations"
  }

  ;  data = read_tiff('double.tif')
  ;  imglayer=image(data, /current, NAME = 'RAW_IMG', margin=0);, /widgets)
  ;  data_inv = 255-data
  ;  imglayer2 = image(data_inv, /current, NAME = 'FILTERED_IMG', margin=0);, /widgets)
  ;  imglayer2.hide = 1
  ;  spheres = ellipse(100,200, major=10, /current, NAME = 'SPHERES', color='blue', /device)
  return,base1
END


pro collidl,saveloc=saveloc,invert=invert,scale=scale,spheresize=sphere_diameter,stay=stay,wait=wait
  !EXCEPT=1

  ; Safe Switches :
  ;     /filtered : tells IDL that the image is already filtered, and does not need the additional
  ;        bandpass filter.  This causes it to skip the call to bpass() and can help with large
  ;        images that occupy too much memory for internal filtering to be accomplished along side
  ;        analysis.  In this case, external pre-filtering is a better option.
  ;     /invert: tells IDL to invert the image colors before analyzing it.  May become standard if
  ;       our procedure has this being used every run
  ;     /scale : if set using /scale, will automatically scale the image to 1024x1024.  If set using
  ;        scale=(number), then the number becomes the scale factor
  ;     /spheresize : set using the spheresize=(number), it sets the sphere_diameter variable,
  ;        making /twomicron,/simu,/onemicron obsolete
  ;     /stay : in order to keep the windows on the screen. Useful
  ;        when working with only one file, a pain otherwise
  ;     /wait : waits for 5 secs after each file processed
  ;
  ;

  ; These are various switches used to control the behaviour of the
  ; program
  ; 1 = do it, 0 = don't

  use_debug_mode_filename = 1
  debug_mode_filename = "../../collidl_test_images/2017tests/double.tif"
  do_angle_histogram = 1  ;whether to output angle histogram file
  save_filtered_image =1 ; whether to save bandpass filtered version of input image
  save_the_all_image_file = 1 ; whether to save image with original image, orientation field, and defects.  Somehow this takes a long time.
  save_the_spheres_image_file = 1;
  show_computation_times = 0
  ; this is the latest trick, works great
  Ccorr=1   ;Preps the data for the external c-program to do the
  ; the correlation calculations fast. Best way so far. By far.
  ;********************************************************

  ; create gaussian weights used for smoothing the angular field
  ; howmuch (pixels out of 256) controls the smoothing of the angular correlation file

  DEFSYSV, '!recursion_level',0L
  DEFSYSV, '!max_recursion_level', 5000L

  howmuch=16

  ;     cd, "E:\matt\research\idl_trawick\simsph\w8.25_eta.4\smaller\trials1-9"
  ;     cd, "/home/angelscu/temp/081202/1.25"
  ;     cd, "/usr/temp/HardSpheres/fromthanos" ; make this your favorite data directory
  ; read files either as a list (.txt file with all names) or by selecting them by hand

  if use_debug_mode_filename then begin
    fs = debug_mode_filename
  endif else begin
    fs=dialog_pickfile(get_path=ps,/multiple_files)
  endelse

  if ((n_elements(fs) gt 0) AND (strcmp(fs[0],'') eq 0)) then begin
    if ((strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif') and (strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'dat'))then readlist,fs(0),fs,path=ps else fs=[fs]
    ; this is where all the info is dumped, then written to the summary file
    Summary_of_Data=fltarr(10, n_elements(fs))
    print,fs

    if (do_angle_histogram eq 1) then begin
      angle_histogram = lonarr(60,n_elements(fs))
    endif


    base1 = create_collidl_widgets()
    wDraw = WIDGET_INFO(base1, FIND_BY_UNAME = 'DRAW')
    WIDGET_CONTROL, wDraw, GET_VALUE = widg_win
    widg_win.select
    print,'number of files is',n_elements(fs)

    ; THE MASTER LOOP------------------------------------------------------------------------------------------
    time0=systime(1)
    for i=0,n_elements(fs)-1 do begin
      close,/all
      print,'Now working on : ',fs(i)


      data=byte(bytscl(read_tiff(fs(i))));
      ;      data=reverse(data,2)
      imagesize=size(data)
      if (keyword_set(invert)) then begin
        data=255-temporary(data)
      endif
      xs=imagesize[1]
      ys=imagesize[2]

      if (keyword_set(scale)) then begin
        ; scale to 1024X1024
        if (scale eq 1) then begin
          data=Interpolate(data,findgen((1024/(xs*1.0))*xs)/(1024/(xs*1.0)),findgen((1024/(ys*1.0))*ys)/(1024/(ys*1.0)),/grid)
        endif else begin
          ; scale by scale(variable)
          data=Interpolate(data,findgen(scale*imagesize[1])/scale,findgen(scale*imagesize[2])/scale,/grid)
        endelse
        ;reset the image size if scaled
        imagesize=size(data)
      endif

      if (keyword_set(sphere_diameter)) then begin
      endif else begin
        sphere_diameter = 6
      endelse
      print,'using sphere size',sphere_diameter
      ; parameters for the sphere locator (obsolete)

      ; locate the centers of the spheres
      ; these are (C) John Crocker http://glinda.lrsm.upenn.edu/~weeks/idl/
      ;data2=bpass(data,1,sphere_diameter)


      data_filtered=bpass(data,1,sphere_diameter)
      data1=feature(data_filtered,sphere_diameter)
      if (save_filtered_image eq 1) then write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'filtered.tif',bytscl(data_filtered)

      DEFSYSV, '!xss',imagesize[1]-1   ;is this needed?
      DEFSYSV, '!yss',imagesize[2]-1   ;is this needed?


      ; used to hold the cooordinates of the particles
      goodsize = size(data1)
      goodx=fltarr(goodsize[2]-1)   ;probably exist until end, previously global
      goody=fltarr(goodsize[2]-1)   ;probably exist until end, previously global
      goodsize=!NULL

      print, "X,Y image size : ", !xss+1, !yss+1


      raw_img=image(data, /current, NAME = 'RAW_IMG', margin=0, zvalue=-.03, axis_style=0)
      raw_img.rotate, /reset
      filtered_img=image(data_filtered, /overplot, NAME = 'FILTERED_IMG', margin=0, zvalue=-.02)
      filtered_img.rotate, /reset
      data_filtered=!NULL ;reallocate memory
      filtered_img.hide=1


      nvertices=n_elements(data1[0,1:*])

      ;Matt was fiddling with these next few lines to try to avoid an apparent vertex at (0,0)
      goodx[0:nvertices-1]=data1[0,1:*]
      goody[0:nvertices-1]=data1[1,1:*]

      if (keyword_set(saveloc)) then begin
        openw,locationfile,strmid(fs[0],0,strlen(fs[0])-4)+'location_data.dat',/get_lun
        for LongIndex=0L, nvertices do begin
          printf,locationfile,data1[0,LongIndex],data1[1,LongIndex]
        endfor
        free_lun,locationfile
      endif
      data1=!NULL

      ;In this new section, we will generate an image JUST showing where the spheres were found.
      if save_the_spheres_image_file then begin
        time_filespheres0=systime(1)
        sf = 2; scale factor for the whole image
        w=window(/buffer, dimensions=[sf*(!xss+1),sf*(!yss+1)],margin=[0.0,0.0,0.0,0.0])
        p1 = image(rebin(data,sf*(!xss+1),sf*(!yss+1)), /overplot, IMAGE_DIMENSIONS=[sf*(!xss+1),sf*(!yss+1)],margin=[0.0,0.0,0.0,0.0])
        ;add annotations as below
        pcircle_size = float(sphere_diameter)/!yss * 1024 / 6 * 0.5 ;at spheresize=6 on a 1024x1024 image, 0.5 was about right.
        spheres=plot(sf*goodx,sf*goody, /overplot, NAME = 'SPHERES', antialias=0,symbol="o",sym_color=[180,255,100], $
          sym_size=pcircle_size, sym_thick = 1.712 * sf*pcircle_size, linestyle='none', /data, axis_style=0)
        ;spheres.rotate, /reset
        img_circled_spheres = spheres.CopyWindow(border=0, height=sf*(!yss+1))
        write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_spheres.tif', reverse(img_circled_spheres,2), compression=1
        img_circled_spheres=!NULL
        w.close
        time_filespheres1=systime(1)
        if show_computation_times then print,'done with writing spheres file ; elapsed time = ',time_filespheres1 - time_filespheres0
      endif ;save_the_spheres_image_file

      ;now plot spheres on the diplay widget window
      widg_win.select
      pcircle_size = float(sphere_diameter)/!yss * 1024 / 6 * 0.5 ;at spheresize=6 on a 1024x1024 image, 0.5 was about right.
      spheres=plot(goodx,goody, /overplot, NAME = 'SPHERES', antialias=0,symbol="o",sym_color=[180,255,100], $
        sym_size=pcircle_size,linestyle='none', /data, axis_style=0)
      spheres.rotate, /reset
      widg_win.uvalue.rescale_list.add, list('sym_size', spheres, pcircle_size)
      widg_win.uvalue.rescale_list.add, list('sym_thick', spheres, 1.712 * pcircle_size)

      ; at the exit, edges will have a weird format, read idl help
      ; on "triangulate" - this is the main source of errors in code
      triangulate, goodx, goody, triangles, outermost, CONNECTIVITY = edges

      area_per_vertex=1.0*!xss*!yss/nvertices
      area_per_triangle = 0.5 * area_per_vertex
      bondlength=sqrt(area_per_triangle*4.0/sqrt(3.0))
      print, "Average bond length by area method = ",bondlength

      disc=fltarr(nvertices)
      inbounds=intarr(nvertices)
      ;ButtInOnAble=replicate(1,nvertices,2)   not used until later!
      ;ButtInOnAble=intarr(nvertices,2)
      ;ButtInOnAble=1 ;initially, all vertices can be butted in on.
      MAX_BOND_NUMBER=nvertices*3.25
      bondsx=fltarr(MAX_BOND_NUMBER)
      bondsy=fltarr(MAX_BOND_NUMBER)
      bondsangle=fltarr(MAX_BOND_NUMBER)
      ;bondsenergy=fltarr(MAX_BOND_NUMBER)
      bondsl=fltarr(MAX_BOND_NUMBER)
      bondcount=0L;

      discx=fltarr(MAX_BOND_NUMBER)
      discy=fltarr(MAX_BOND_NUMBER)
      discangle=fltarr(MAX_BOND_NUMBER)
      disccount=0L;

      nedges=n_elements(edges)

      ;Below are two methods to find spheres "near the edge", which may affect their number of nearest neighbors.
      ;(I also tried just finding the nearest neighbors of all boundary points of the triangulation, but that wasn't sufficient.)
      
      ;First way is to simply use distance from the edge of the picture:
      ;inboundsmult=1.5
      ;inbounds[where((goodx gt inboundsmult*bondlength) and (goodx lt !xss-inboundsmult*bondlength) and (goody gt inboundsmult*bondlength) and (goody lt !yss-inboundsmult*bondlength))]=1;
      
      ;Second way is to use the actual distance to the perimeter of the triangulation 
      inbounds = find_vertices_not_near_perimeter(goodx,goody,connectivity,outermost,bondlength)

      ;color_selected_points_in_window, widg_win, 1, goodx, goody, where(inbounds eq 0), sphere_diameter, [0,0,0]

      for i1=0L,nvertices-1 do begin
        disc[i1]=edges[i1+1]-edges[i1] ; # neighbors
        for j1=edges[i1],edges[i1+1]-1 do begin
          ; collect angle data from the bond
          if (i1 lt edges[j1]) then begin
            ;            plots,[goodx[edges[j1]]/!xss,goodx[i1]/!xss],[1-goody[edges[j1]]/!yss,1-goody[i1]/!yss],/normal,color=!colorbond,thick=1.5
            bondsx[bondcount]=(goodx[i1]+goodx[edges[j1]])/2
            bondsy[bondcount]=(goody[i1]+goody[edges[j1]])/2
            bondsl[bondcount]=sqrt((goodx[i1]-goodx[edges[j1]])^2.0+(goody[i1]-goody[edges[j1]])^2.0)
            ang1=!pi+atan(goody[i1]-goody[edges[j1]],goodx[i1]-goodx[edges[j1]])
            ang=ang1-(!pi/3.)*floor(ang1*3/!pi)
            bondsangle[bondcount]=ang

            bondcount=bondcount+1
          endif
        endfor
      endfor

      bondslength=total(bondsl)/bondcount
      print, "Average Bondlength, by direct calculation = ", bondslength
      print, "Median Bondlength, by direct calculation = ", median(bondsl[0:bondcount-1])

      unbounddisc=disc-6; unbound disclinationality phew !
      bondsl=0

      ;size of a disclination square on the screen
      discsize=1

      disc5=0 ; # inbounds 5s, total
      disc7=0 ; # inbounds 7s, total



      ; runs Matt's code on finding dislocations
      ; at the return, bound will have the bonded neighbors, in the "edges" format
      ; read the idl help on "triangulate" for an explanation of the format
      ; bound[i] will be 1 if there is a bond or 0 otherwise
      bound=intarr(nedges)
      find_all_unbound_disc, nvertices, edges, unbounddisc, inbounds, bound 

      disc5=0; total # 5s
      disc7=0; total # 7s
      unbound5=0
      unbound7=0

      ;Now draw triangulation---------------------
      listedges=list()
      for i1=0L, nvertices-1 do begin
        for j1=edges[i1],edges[i1+1]-1 do begin
          ; collect angle data from the bond
          if (i1 lt edges[j1]) then begin
            listedges.add,i1
            listedges.add,edges[j1]
          endif
        endfor
      endfor
      numverts=replicate(2,listedges.count()/2)
      firstindex=indgen(listedges.count()/2, /long)*2
      secondindex=indgen(listedges.count()/2, /long)*2+1
      connections=transpose([[numverts],[firstindex],[secondindex]])
      connections=reform(connections,n_elements(connections))
      triangulation=polyline(goodx[listedges.toarray()],goody[listedges.toarray()], antialias = 0 ,connectivity=connections,/data, $
        /overplot, NAME = 'TRIANGULATION', color=[0,0,255],thick=2)
      widg_win.uvalue.rescale_list.add, list('thick', triangulation, 2)

      ;--------------------------------------------------------


      print, "Found total 5's and 7's : ", disc5, disc7
      print, "Found total unbound 5's and 7's : ", unbound5, unbound7


      Summary_of_Data[2,i]=unbound5
      Summary_of_Data[3,i]=unbound7

      ; Now we need to find the dislocations and calculate their correlations


      ; good trick :  total(bound) = 2*#dislocations
      Summary_of_Data[4,i]=total(bound)/2

      ;bound=0 ;never used again

      ;--------------------------------------------------------------
      discx=0
      discy=0
      discangle=0

      ;--------------------------------------------------------------


      bondsx=bondsx(0:bondcount-1)
      bondsy=bondsy(0:bondcount-1)
      bondsangle=bondsangle(0:bondcount-1)
      print,'Found ', bondcount,' Delauney bonds'


      triangulate, bondsx, bondsy, btriangles, boutermost, CONNECTIVITY = bedges   ;bedges, btriangles gets created here!
      bcutoff=3*howmuch
      ;get an interpolated image of the bonds angles
      bcosangle=trigrid(bondsx,bondsy,cos(6.0*bondsangle),btriangles, NX=(!xss+1),NY=(!yss+1))
      bsinangle=trigrid(bondsx,bondsy,sin(6.0*bondsangle),btriangles, NX=(!xss+1),NY=(!yss+1))

      ; Do gaussian smoothing on the bond angle file
      smoothbcosangle=fltarr(!xss+1,!yss+1)
      smoothbsinangle=fltarr(!xss+1,!yss+1)
      time1=systime(1)
      if show_computation_times then print,'elapsed time so far: ', time1-time0
      if show_computation_times then print,'starting doing the smoothing...'
      ;      smooth,bcosangle, smoothbcosangle, weights, howmuch
      ;      smooth,bsinangle, smoothbsinangle, weights, howmuch
      smoothbcosangle = fftsmooth(bcosangle, long(bondlength))
      smoothbsinangle = fftsmooth(bsinangle, long(bondlength))

      ;this inverse tangent always throws a floating point underflow error, but it seems harmless.
      !EXCEPT=0
      smoothbangle = -((float(atan(smoothbsinangle, smoothbcosangle))) * 180.0 / !pi ) + 180.0
      junk_variable = check_math()
      !EXCEPT=1
      
      time2=systime(1)
      if show_computation_times then print,'done doing the smoothing...','elapsed time for smoothing = ',time2 - time1
      bcosangle=!NULL
      bsinangle=!NULL
      smoothbsinangle=!NULL
      smoothbcosangle=!NULL

      HSV_array = replicate(1.0,3,!xss+1,!yss+1)
      HSV_array[0,*,*]=smoothbangle
      rgb_angle_image = replicate(0B,3,!xss+1,!yss+1)
      color_convert, HSV_array, rgb_angle_image, /HSV_RGB

      write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_angle.tif', rgb_angle_image, compression=1

      widg_win.select
      orient_img=image(rgb_angle_image, /overplot, NAME = 'ORIENT_IMG', margin=0, transparency=50, zvalue=-.01, axis_style=0)
      orient_img.rotate, /reset


      if (do_angle_histogram eq 1) then begin
        single_angle_histogram = histogram(smoothbangle,min =0,binsize = !PI/(3 * 60), nbins = 61)
        angle_histogram[*,i] = single_angle_histogram[0:59]
        angle_histogram[0,i] = temporary(angle_histogram[0,i]) + single_angle_histogram[60]
      endif
      single_angle_histogram=!NULL



      ;      Now we have origimg, bondsimg, rgb_angle_image to work with
      ;      newimg=origimg*.5+rgb_angle_image*.5
      rgb_orig = bytarr(3,!xss+1,!yss+1)
      rgb_orig[0,*,*]=data
      rgb_orig[1,*,*]=data
      rgb_orig[2,*,*]=data
      newimg=rgb_orig*.5+rgb_angle_image*.5

      rgb_angle_image=!NULL


      ;In this section, we draw defects over a large image.
      ;first, open a window as a buffer, and draw things there.
      if save_the_all_image_file then begin
        time_defects0=systime(1)
        if show_computation_times then print,'about to draw defects....'
        sf = 2; scale factor for the whole image
        w=window(/buffer,dimensions=[sf*(!xss+1),sf*(!yss+1)])
        w.uvalue={defect_plot_list:list(), rescale_list:list()}
        p1 = image(rebin(newimg,3,sf*(!xss+1),sf*(!yss+1)), /overplot, IMAGE_DIMENSIONS=[sf*(!xss+1),sf*(!yss+1)],margin=[0.0,0.0,0.0,0.0])
        ;add annotations as below
        add_disclinations_to_window, w, sf, goodx, goody, disc, unbounddisc, sphere_diameter
        add_dislocations_to_window, w, sf, goodx, goody, edges, bound, nvertices
        img_new_all = p1.CopyWindow(border=0,height=sf*(!yss+1))
        time_fileall0=systime(1)
        write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_all.tif', reverse(img_new_all,2), compression=1
        time_fileall1=systime(1)
        if show_computation_times then print,'done with writing file defects; elapsed time = ',time_fileall1 - time_fileall0
        w.close
        ;The next line displays the file just written:
        ;im_ang = image(img_new_all)
        img_new_all = !NULL
        time_defects1=systime(1)
        if show_computation_times then print,'done with file defects; elapsed time = ',time_defects1 - time_defects0
     endif ;save_the_all_image_file

      ;Now do the same thing on the main window.
      widg_win.select
      
      add_disclinations_to_window, widg_win, 1, goodx, goody, disc, unbounddisc, sphere_diameter
      add_dislocations_to_window, widg_win, 1, goodx, goody, edges, bound, nvertices



      bedges=0 ; bedges gets created in do_force and do_bonds conditionals only!
      btriangles=0 ; btriangles gets created in do_force and do_bonds conditionals only!
      ;--------------------------------------------------------------


      imagesize=0

      ;--------------------------------------------------------------
      image1=0
      btriangles=0
      newimg=0
      bondsimg=0
      newred=0
      newgreen=0
      newblue=0
      newbred=0
      newbgreen=0
      newbblue=0
      whatbondstokeep=0
      boutermost=0
      origimg=0

      ;ccorr
      ;********************
      time_ccorr0=systime(1)
      if show_computation_times then print,'about to do Ccorr....'
      if (Ccorr eq 1) then write_output_for_correlation_function,fs,bondsx,bondsy,bondsangle,bondcount
;      bondsx=0
;      bondsy=0
      time_ccorr1=systime(1)
      if show_computation_times then print,'done with ccorr; elapsed time = ',time_ccorr1 - time_ccorr0



      data=0



      x=0
      smoothbangle=0
      ;direct

      print, 'All done with ',fs[i],'.'

      if (keyword_set(wait)) then begin
        ; to give you time to examine the images
        wait,5
      endif


      if (keyword_set(stay) eq 0) then begin
        ; destroy all the windows created
      end


    endfor ; main loop that reads each input file
    ; MASTER LOOP ENDS

    print, 'DONE WITH BIG LOOP'
    if (do_angle_histogram eq 1) then begin
      openw,angle_histogram_unit,strmid(fs[0],0,strlen(fs[0])-4)+'angle_histogram.dat',/get_lun
      printf,format='(60I)',angle_histogram_unit,angle_histogram
      free_lun,angle_histogram_unit
    endif
    angle_histogram=0




    ; output all data into the summary file
    openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'summary.dat',/get_lun
    for LongIndex=0L, n_elements(fs)-1 do begin
      printf,u,Summary_of_Data[0,LongIndex],Summary_of_Data[1,LongIndex],Summary_of_Data[2,LongIndex],Summary_of_Data[3,LongIndex],Summary_of_Data[4,LongIndex],Summary_of_Data[5,LongIndex]," ",fs[LongIndex]
    endfor
    free_lun,u

    print,'total time:',systime(1)-time0

  endif else begin
    print, 'no opened images'
  endelse


end






; **************************************************************************************
;
;
;  Begin Functions
;
;
; **************************************************************************************

pro write_output_for_correlation_function, fs,bondsx,bondsy,bondsangle,bondcount

  openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'bonds.dat',/get_lun
  sampling=2; The decimation rate in getting the bonds

  printf,u,sampling

  printf,u,bondcount

  for iii=0L, bondcount-1 do begin
    printf,u,bondsx[iii],bondsy[iii],bondsangle[iii]
  endfor

  free_lun,u
end

function fftsmooth, input, howmuch

  ;print, 'from fftsmooth: howmuch = ',howmuch

  x=(size(input))[1]
  y=(size(input))[2]
  bigweights=input*0
  norm=0
  for aa=0, howmuch do begin
    for bb=0, howmuch do begin
      bigweights[aa,bb]=exp(-(aa*aa+bb*bb)/(howmuch*howmuch))
      bigweights[x-aa-1,bb]=bigweights[aa,bb]
      bigweights[x-aa-1,y-bb-1]=bigweights[aa,bb]
      bigweights[aa,y-bb-1]=bigweights[aa,bb]
      norm=norm+4*bigweights[aa,bb]
    endfor
  endfor
  bigweights=bigweights/norm
  output=fft(fft(input)*fft(bigweights),/inverse)
  return,output
end

pro smooth, input, output, weights, howmuch

  for aa=-howmuch, howmuch do begin
    for bb=-howmuch, howmuch do begin
      output=output+weights[aa+howmuch, bb+howmuch]*shift(input,aa,bb)
    endfor
  endfor
  ;for greater speed, but not quite as fast as fftsmooth, consider:
  ;output=convol(input,weights)
end


function sign_of, x

  if (x lt 0) then return, -1
  if (x gt 0) then return,  1
  return,0

end



pro make_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound

  nn1=edges[vertex1+1]-edges[vertex1]
  nn2=edges[vertex2+1]-edges[vertex2]

  if ((nn1 eq 6) or (nn2 eq 6)) then stop

  var1=edges[vertex1]+where(edges[edges[vertex1]:edges[vertex1+1]-1] eq vertex2)
  bound[var1]=bound[var1]+1
  var1=edges[vertex2]+where(edges[edges[vertex2]:edges[vertex2+1]-1] eq vertex1)
  bound[var1]=bound[var1]+1
  var2=unbounddisc[vertex1]
  if (var2 eq 0)  then begin
    var21=0
  end else begin; Not quite sure if this is right, need to check matt's code
    var21=var2/abs(var2)
  end
  unbounddisc[vertex1]=var2-var21

  var2=unbounddisc[vertex2]
  if (var2 eq 0)  then begin
    var21=0
  end else begin; Not quite sure if this is right, need to check matt's code
    var21=var2/abs(var2)
  end
  unbounddisc[vertex2]=var2-var21

  ;   print, "making bond between ",vertex1,vertex2
end



pro break_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound

  var1=edges[vertex1]+where(edges[edges[vertex1]:edges[vertex1+1]-1] eq vertex2)
  bound[var1]=bound[var1]-1


  var1=edges[vertex2]+where(edges[edges[vertex2]:edges[vertex2+1]-1] eq vertex1)
  bound[var1]=bound[var1]-1

  nn1=edges[vertex1+1]-edges[vertex1]-6
  nn2=edges[vertex2+1]-edges[vertex2]-6

  unbounddisc[vertex1]=unbounddisc[vertex1]+sign_of(nn1)
  unbounddisc[vertex2]=unbounddisc[vertex2]+sign_of(nn2)

  ;   print, "breaking bond between ",vertex1,vertex2
  ;erase old bond
end




function it_would_help_to_bond_nicely ,vertex1,vertex2,edges, unbounddisc, inbounds, inprocess,bound,outofboundsok
  nvertices=n_elements(inbounds)
  if ((sign_of(unbounddisc[vertex1]) eq -sign_of(unbounddisc[vertex2])) and (inprocess[vertex2] eq 0) ) then begin
    ;  if (((unbounddisc[vertex1]*unbounddisc[vertex2] lt 0)) and (inprocess(vertex2) eq 0) ) then begin
    if ((inbounds(vertex2) eq 1) or (outofboundsok eq 1)) then begin
      return,1
    end
  end else begin
    return,0
  end
end





function try_to_find_a_mate_nicely_for, vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound,outofboundsok
  ;   print, "try_to_find_mates_nicely_for",vertex
  for n=edges[vertex], edges[vertex+1]-1 do begin
    if(it_would_help_to_bond_nicely(vertex,edges[n],edges, unbounddisc,inbounds, inprocess,bound,outofboundsok) eq 1) then begin
      make_single_bond_between,vertex, edges[n],nvertices,edges,unbounddisc, inbounds, inprocess,bound
      return,1
    end
  endfor
  return,0
end


; There is a problem with sign of...

function it_would_help_me_to_butt_in_on, vertex1, vertex2,edges,unbounddisc, inbounds, inprocess,bound,outofboundsok
  nvertices=n_elements(inbounds)
  nn1=edges[vertex1+1]-edges[vertex1]-6
  nn2=edges[vertex2+1]-edges[vertex2]-6
  if (sign_of(nn1) eq -sign_of(nn2)) then begin
    ;   if ((unbounddisc[vertex1]*unbounddisc[vertex2] lt 0)) then begin
    if (inprocess[vertex2] eq 0) then begin
      return,1
    end
  end else begin
    return,0
  end
end

function can_retract_from, vertex1, vertex2, edges, unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok
  ;   print, "called the motherfucker"
  inprocess[vertex1]=1
  break_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound
  ;   print, !recursion_level


  if (!recursion_level gt !max_recursion_level) then goto,can_retract_end
  if ((inbounds[vertex2] ne 1)) then begin
    inprocess[vertex1]=0
    return,1
  end

  !recursion_level=!recursion_level+1

  forward_function try_to_find_a_mate_rudely_for
  if ((try_to_find_a_mate_nicely_for(vertex2,nvertices,edges,unbounddisc, inbounds, inprocess,bound,outofboundsok) eq 1)) then begin
    inprocess[vertex1]=0
    !recursion_level=!recursion_level-1
    return,1
  end

  if (try_to_find_a_mate_rudely_for(vertex2,nvertices,edges,unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok) eq 1) then begin
    inprocess[vertex1]=0
    !recursion_level=!recursion_level-1
    return,1
  end

  !recursion_level=!recursion_level-1

  can_retract_end :
  make_single_bond_between,vertex1, vertex2,nvertices,edges,unbounddisc, inbounds, inprocess, bound
  inprocess[vertex1]=0
  return,0
end

function can_butt_in_on, vertex1, vertex2,edges, unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok
  nvertices=n_elements(inbounds)

  if ((inbounds[vertex2] eq 0) and (outofboundsok eq 0)) then begin
    return,0
  end
  if not(ButtInOnAble[vertex2,outofboundsok]) then begin
    return,0
  end
  inprocess[vertex1]=1

  for i=edges[vertex2], edges[vertex2+1]-1 do begin
    if (bound[i] gt 0) and ((inbounds[edges[i]] eq 1) or (outofboundsok eq 1)) then begin
      if (can_retract_from(vertex2, edges[i],edges, unbounddisc, inbounds, inprocess,bound,ButtInOnAble,outofboundsok) eq 1) then begin
        inprocess[vertex1]=0
        return,1
      end
    end
  endfor
  inprocess[vertex1]=0
  ButtInOnAble[vertex2,outofboundsok] = 0
  return,0
end


function try_to_find_a_mate_rudely_for, vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble,outofboundsok

  for n=edges[vertex], edges[vertex+1]-1 do begin
    if (it_would_help_me_to_butt_in_on(vertex, edges[n],edges,unbounddisc, inbounds, inprocess,bound,outofboundsok) eq 1 ) then begin
      if (can_butt_in_on(vertex, edges[n],edges, unbounddisc,inbounds, inprocess,bound, ButtInOnAble,outofboundsok) eq 1) then begin
        make_single_bond_between,vertex, edges[n],nvertices,edges,unbounddisc, inbounds, inprocess,bound
        return,1
      end
    end
  endfor

  return,0
end

pro try_to_find_mates_for,vertex,nvertices,edges,unbounddisc, inbounds, inprocess, bound, ButtInOnAble
  if (inbounds[vertex] eq 1) then begin
    inprocess[vertex]=1

    while (unbounddisc[vertex] ne 0) do begin
      if (try_to_find_a_mate_nicely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound,0) eq 0) then break
    endwhile

    while (unbounddisc[vertex] ne 0) do begin
      if (try_to_find_a_mate_rudely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble,0) eq 0) then break
    endwhile

    while (unbounddisc[vertex] ne 0) do begin
      if (try_to_find_a_mate_nicely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, 1) eq 0) then break
    endwhile

    while (unbounddisc[vertex] ne 0) do begin
      if (try_to_find_a_mate_rudely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble, 1) eq 0) then break
    endwhile

    inprocess[vertex]=0
  endif
end

pro find_all_unbound_disc, nvertices, edges, unbounddisc, inbounds, bound
  inprocess=intarr(nvertices)
  ButtInOnAble=replicate(1,nvertices,2)
  for i=long(0), nvertices-1 do begin
    ;     print, "vertex ", i, unbounddisc[i]
    try_to_find_mates_for,i,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble
  end
end


function find_vertices_not_near_perimeter, x,y,conn,outermost, bondlength
  ;The function "Triangulate" returns a list of "boundary" vertices on the outside of the triangulation.
  ;But these "boundary" points are often far apart.  This routine returns a boolean list [0, 1, 0, 0, 1...] 
  ;of which points are within a certain length of the triangulated boundary.

  ;time0=systime(1)
;  print,"starting to find edge vertices"
  is_near_perimeter = intarr((size(x))[1])
  num_perimeter_verts = (size(outermost))[1]
  ;for efficiency, prefill an array with perimeter points:
  L=fltarr(2,num_perimeter_verts + 1)
  for perim_idx = 0, num_perimeter_verts-1 do begin
    L[*,perim_idx] = [x[outermost[perim_idx]],y[outermost[perim_idx]]]
  endfor
  L[*,num_perimeter_verts]= L[*,0]
  for v = 0, (size(x))[1] -1 do begin
    min_dist = bondlength*100
    P = [x[v],y[v]]
    for perim_idx = 0, num_perimeter_verts-1 do begin
      dist_to_edge = pnt_line(P, L[*,perim_idx], L[*,perim_idx+1], /interval)
      if dist_to_edge lt min_dist then min_dist = dist_to_edge 
    endfor
    if min_dist lt bondlength*1.732*0.5 then is_near_perimeter[v] = 1
  endfor
  ;print,"total time for finding perimeter points:",systime(1)-time0
  is_far_from_perimeter = (is_near_perimeter*0+1) - is_near_perimeter
  return,is_far_from_perimeter
end


function create_neighbor_array, idx, connect
  neighbs = list()
  foreach i, idx do begin
    neighbs.add, connect[connect[i] : connect[i+1] - 1] ,/extract
  endforeach
  return, neighbs.toarray()
  both_combined = list(idx) 
  both_combined.add, neighbs, /extract
;  return, both_combined.toarray()
end


pro color_selected_points_in_window, w, sf, goodx, goody, selected_points, sphere_diameter, rgb_color_to_use
;used for diagnostic purposes
  psym_size= float(sphere_diameter)/!yss * 1024 / 6 * 0.4 ;at spheresize=6 on a 1024x1024 image, 0.4 was about right.
  pcircle_size = float(sphere_diameter)/!yss * 1024 / 6 * 0.75
  disc_thick=3.0 * sf
  circle_thick=1.0 * sf
  props = {antialias:0, symbol:"o", linestyle:'none'}

  p=plot(sf*goodx[selected_points],sf*(goody[selected_points]),/data,/overplot, sym_color=rgb_color_to_use,sym_filled=1,sym_size=psym_size, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, psym_size)
  w.uvalue.defect_plot_list.add, p
end

pro add_disclinations_to_window, w, sf, goodx, goody, disc, unbounddisc, sphere_diameter

  psym_size= float(sphere_diameter)/!yss * 1024 / 6 * 0.4 ;at spheresize=6 on a 1024x1024 image, 0.4 was about right.
  pcircle_size = float(sphere_diameter)/!yss * 1024 / 6 * 0.75
  disc_thick=3.0 * sf
  circle_thick=1.0 * sf
  props = {antialias:0, symbol:"o", linestyle:'none'}

  fours=where(disc lt 5)
  p=plot(sf*goodx[fours],sf*(goody[fours]),/data,/overplot, sym_color=[255,0,255],sym_filled=1,sym_size=psym_size, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, psym_size)
  w.uvalue.defect_plot_list.add, p

  fives=where(disc eq 5)
  p=plot(sf*goodx[fives],sf*(goody[fives]),/data,/overplot, sym_color=[255,0,0],sym_filled=1,sym_size=psym_size, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, psym_size)
  w.uvalue.defect_plot_list.add, p

  sevens=where(disc eq 7)
  p=plot(sf*goodx[sevens],sf*(goody[sevens]),/data,/overplot, sym_color=[0,255,0],sym_filled=1,sym_size=psym_size, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, psym_size)
  w.uvalue.defect_plot_list.add, p

  eights=where(disc gt 7)
  p=plot(sf*goodx[eights],sf*(goody[eights]),/data,/overplot, sym_color=[0,255,255],sym_filled=1,sym_size=psym_size, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, psym_size)
  w.uvalue.defect_plot_list.add, p

  unb_fours=where(unbounddisc eq -2)
  p=plot(sf*goodx[unb_fours],sf*(goody[unb_fours]),/data,/overplot, sym_color=[255,0,255],sym_size=pcircle_size,sym_thick=circle_thick, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, pcircle_size)
  w.uvalue.rescale_list.add, list('sym_thick',p, circle_thick)
  w.uvalue.defect_plot_list.add, p

  unb_fives=where(unbounddisc eq -1)
  p=plot(sf*goodx[unb_fives],sf*(goody[unb_fives]),/data,/overplot, sym_color=[255,0,0],sym_size=pcircle_size, sym_thick=circle_thick, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, pcircle_size)
  w.uvalue.rescale_list.add, list('sym_thick',p, circle_thick)
  w.uvalue.defect_plot_list.add, p

  unb_sevens=where(unbounddisc eq 1)
  p=plot(sf*goodx[unb_sevens],sf*(goody[unb_sevens]),/data,/overplot, sym_color=[0,255,0],sym_size=pcircle_size, sym_thick=circle_thick, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, pcircle_size)
  w.uvalue.rescale_list.add, list('sym_thick',p, circle_thick)
  w.uvalue.defect_plot_list.add, p

  unb_eights=where(unbounddisc eq 2)
  p=plot(sf*goodx[unb_eights],sf*(goody[unb_eights]),/data,/overplot, sym_color=[0,255,255],sym_size=pcircle_size, sym_thick=circle_thick, _extra=props)
  w.uvalue.rescale_list.add, list('sym_size',p, pcircle_size)
  w.uvalue.rescale_list.add, list('sym_thick',p, circle_thick)
  w.uvalue.defect_plot_list.add, p
end

pro add_dislocations_to_window, w, sf, goodx, goody, edges, bound, nvertices
  disc_thick=3.0 * sf
  listedges=list()
  for i1=0L, nvertices-1 do begin
    for j1=edges[i1],edges[i1+1]-1 do begin
      ; collect angle data from the bond
      if ((bound[j1]) and (i1 lt edges[j1])) then begin
        listedges.add,i1
        listedges.add,edges[j1]
      endif
    endfor
  endfor
  numverts=replicate(2L,listedges.count()/2)
  firstindex=indgen(listedges.count()/2, /long)*2
  secondindex=indgen(listedges.count()/2, /long)*2+1
  connections=transpose([[numverts],[firstindex],[secondindex]])
  connections=reform(connections,n_elements(connections))
  ;      p=polyline(sf*goodx[listedges.toarray()],sf*(!yss-goody[listedges.toarray()]), antialias = 0 ,connectivity=connections,/data,/overplot, color=[255,255,0],thick=disc_thick)
  p=polyline(sf*goodx[listedges.toarray()],sf*(goody[listedges.toarray()]), antialias = 0 ,connectivity=connections,/data, color=[255,255,0],thick=disc_thick)
  w.uvalue.rescale_list.add, list('thick', p, disc_thick)
  w.uvalue.defect_plot_list.add, p
end