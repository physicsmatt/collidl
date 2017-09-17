
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

pro toggle_orientation, w, state
  w['ORIENT_IMG'].hide = 1 - state
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
  if inputstr eq 't' then begin
    ann_state[1] = 1 - ann_state[1]
    toggle_triangulation, w, ann_state[1]
  endif
  if inputstr eq 'o' then begin
    ann_state[3] = 1 - ann_state[3]
    toggle_orientation, w, ann_state[3]
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
    ;img = w['RAW_IMG']
    moveby = [x,y] - *w.uvalue.movehomemousexy
    movedby = *w.uvalue.centerxy - *w.uvalue.movehomecenterxy
    yet_to_move = moveby - movedby
    kludgefact = (1/*w.uvalue.xyzscale)
    w['RAW_IMG'].translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device
    w['SPHERES'].translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device
    w['ORIENT_IMG'].translate, yet_to_move[0] * kludgefact, yet_to_move[1] * kludgefact, /device
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
    w['RAW_IMG'].scale, scalefact, scalefact, scalefact
    w['SPHERES'].scale, scalefact, scalefact, scalefact
    w['ORIENT_IMG'].scale, scalefact, scalefact, scalefact
    kludgefact = (1/*w.uvalue.xyzscale)
    w['RAW_IMG'].translate, -shiftedby[0] * kludgefact, -shiftedby[1] * kludgefact, /device ;make correction, so area under cursor does not move with scale change
    w['SPHERES'].translate, -shiftedby[0] * kludgefact, -shiftedby[1] * kludgefact, /device ;make correction, so area under cursor does not move with scale change
    w['ORIENT_IMG'].translate, -shiftedby[0] * kludgefact, -shiftedby[1] * kludgefact, /device ;make correction, so area under cursor does not move with scale change
    *w.uvalue.xyzscale *= scalefact
    *w.uvalue.centerxy -= shiftedby
    w['SPHERES'].sym_size *= scalefact
    w['SPHERES'].sym_thick = 1.712 * w['SPHERES'].sym_size
    w['TRIANGULATION'].thick *= scalefact
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
  if event.value eq 'TRIANGULATION' then toggle_triangulation, w, event.select
  if event.value eq 'ORIENTATION' then toggle_orientation, w, event.select

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

function create_collidl_widgets
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
  WIDGET_CONTROL, bgroup_annotations, set_value=[1,1,0,1]
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

  ;  data = read_tiff('double.tif')
  ;  imglayer=image(data, /current, NAME = 'RAW_IMG', margin=0);, /widgets)
  ;  data_inv = 255-data
  ;  imglayer2 = image(data_inv, /current, NAME = 'FILTERED_IMG', margin=0);, /widgets)
  ;  imglayer2.hide = 1
  ;  spheres = ellipse(100,200, major=10, /current, NAME = 'SPHERES', color='blue', /device)
  return,base1
END


pro collidl,saveloc=saveloc,invert=invert,scale=scale,spheresize=sphere_diameter,stay=stay,wait=wait

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

  ; all of these are old ways of doing correlations, either not
  ; reliabe or way too slow
  directcorr=0 ; calculate correlations directly

  ; this is the latest trick, works great
  Ccorr=1   ;Preps the data for the external c-program to do the
  ; the correlation calculations fast. Best way so far. By far.



  Gcorr=0 ; spawn an external c-program (called Grobcor) which does
  ; the translational correlation calculations fast.
  ; Best way so far. By far.


  showdiscbefore=1; whether to display the
  ; disclinations before drawing dislocations
  do_angle_histogram = 1  ;whether to output angle histogram file
  do_postscript_defects = 1 ;whether to output postscript file for disclinations, dislocations.
  save_bw_angle_tif = 0; whether to save b&w tif showing orientation (separate from the color tif file)
  save_filtered_image =1 ; whether to save bandpass filtered version of input image
  ;********************************************************

  ; in order to redraw windows
  device,retain=1

  ; create gaussian weights used for smoothing the angular field
  ; howmuch (pixels out of 256) controls the smoothing of the angular correlation file

  howmuch=16
  weights=fltarr(2*howmuch+1,2*howmuch+1)
  norm=0
  for aa=-howmuch, howmuch do begin
    for bb=-howmuch, howmuch do begin
      weights[aa+howmuch, bb+howmuch]=exp(-(aa*aa+bb*bb)/(howmuch*howmuch))
      norm=norm+weights[aa+howmuch, bb+howmuch]
    endfor
  endfor
  weights=weights/norm
  W=fltarr(1000)
  W[*]=1

  DEFSYSV, '!draw_recursively', 1
  DEFSYSV, '!recursion_level',0L
  DEFSYSV, '!max_recursion_level', 5000L
  DEFSYSV, '!colordisc7' ,  rgbcolor(0,255,0) ; green
  DEFSYSV, '!colordisc5' ,  rgbcolor(255,0,0); red
  DEFSYSV, '!colordisc4' , rgbcolor(255,0,255) ; magenta
  DEFSYSV, '!colordisc8' ,  rgbcolor(0,255,255) ; cyan
  DEFSYSV, '!colordisloc' , rgbcolor(255,255,0) ; yellow
  DEFSYSV, '!colorbond' , rgbcolor(0,0,255); blue
  DEFSYSV, '!colorwhite' , rgbcolor(255,255,255) ; white


  ;     cd, "E:\matt\research\idl_trawick\simsph\w8.25_eta.4\smaller\trials1-9"
  ;     cd, "/home/angelscu/temp/081202/1.25"
  ;     cd, "/usr/temp/HardSpheres/fromthanos" ; make this your favorite data directory
  ; read files either as a list (.txt file with all names) or by selecting them by hand
  ;       fs=dialog_pickfile(get_path=ps,/multiple_files)
  ;       print,fs
  fs="../../collidl_test_images/2017tests/double.tif"
  if ((n_elements(fs) gt 0) AND (strcmp(fs[0],'') eq 0)) then begin
    if ((strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'tif') and (strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'dat'))then readlist,fs(0),fs,path=ps else fs=[fs]
    ; this is where all the info is dumped, then written to the summary file
    Summary_of_Data=fltarr(10, n_elements(fs))
    print,fs

    if (do_angle_histogram eq 1) then begin
      angle_histogram = lonarr(60,n_elements(fs))
    endif

    ; THE MASTER LOOP

    base1 = create_collidl_widgets()
    wDraw = WIDGET_INFO(base1, FIND_BY_UNAME = 'DRAW')
    WIDGET_CONTROL, wDraw, GET_VALUE = widg_win
    widg_win.select

    print,'number of files is',n_elements(fs)
    time0=systime(1)
    for i=0,n_elements(fs)-1 do begin
      close,/all
      print,'Now working on : ',fs(i)
      windowsclosed=0


      data=byte(bytscl(read_tiff(fs(i))));
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
        sphere_diameter =6
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

      ; previously global, then passed, now in a common block to avoid passing.
      Common globalgood, goodx, goody      ;this is very slightly slower, but it IS cleaner than passing 392039238 times

      goodsize = size(data1)
      goodx=fltarr(goodsize[2]-1)   ;probably exist until end, previously global
      goody=fltarr(goodsize[2]-1)   ;probably exist until end, previously global
      goodsize=!NULL

      print, "X,Y image size : ", !xss+1, !yss+1


      data=reverse(data,2)
      raw_img=image(data, /current, NAME = 'RAW_IMG', margin=0, zvalue=-.03, axis_style=0)
      raw_img.rotate, /reset
      filtered_img=image(data_filtered, /current, NAME = 'FILTERED_IMG', margin=0, zvalue=-.02)
      filtered_img.rotate, /reset
      data_filtered=!NULL ;reallocate memory
      filtered_img.hide=1

      ;origimg=readimage(0)  ;only used in do_force

;      if (imagesize[1]*imagesize[2] gt 4194304) then begin ;4194304 is 2048x2048
;        widget_control,wimage,/destroy
;        windowsclosed=1
;      endif

      nvertices=n_elements(data1[0,1:*])
      print, nvertices

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
      widg_win.select
      pcircle_size = float(sphere_diameter)/!yss * 1024 / 6 * 0.5 ;at spheresize=6 on a 1024x1024 image, 0.5 was about right.
      spheres=plot(goodx,!yss-goody, /current, /overplot, NAME = 'SPHERES', antialias=0,symbol="o",sym_color=[0,255,0], $
        sym_size=pcircle_size,linestyle='none', /data, axis_style=0)
      spheres.rotate, /reset
      ;      spheres2 = ellipse(100,200, major=10, /current, NAME = 'SPHERES', color='blue', /data)

      img_circled_spheres = widg_win.CopyWindow(border=0,height=!yss+1)
      write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_spheres.tif', reverse(img_circled_spheres,2), compression=1

      img_circled_spheres=!NULL


      ; at the exit, edges will have a weird format, read idl help
      ; on "triangulate" - this is the main source of errors in code
      triangulate, goodx, goody, triangles, outermost, CONNECTIVITY = edges

      areavertex=1.0*!xss*!yss/nvertices
      bondlength=sqrt(areavertex*4/sqrt(3))
      print, "Average bond length a=",bondlength

      disc=fltarr(nvertices)
      inbounds=intarr(nvertices)
      inprocess=intarr(nvertices)
      ;ButtInOnAble=replicate(1,nvertices,2)   not used until later!
      ;ButtInOnAble=intarr(nvertices,2)
      ;ButtInOnAble=1 ;initially, all vertices can be butted in on.
      ;fcorr=fltarr(floor(sqrt(1.0*!xss*!xss+1.0*!yss*!yss)))  made later, as needed
      ;ncorr=fltarr(floor(sqrt(1.0*!xss*!xss+1.0*!yss*!yss)))  made later, as needed
      print, nvertices
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

      inboundsmult=1.5
      nedges=n_elements(edges)


      inbounds[where((goodx gt inboundsmult*bondlength) and (goodx lt !xss-inboundsmult*bondlength) and (goody gt inboundsmult*bondlength) and (goody lt !yss-inboundsmult*bondlength))]=1;



      for i1=long(0),nvertices-1 do begin
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
      print, "The Bondlength = ", bondslength

      ;This only allows bondsenergy to be created if it is going to be used.
      if (keyword_set(do_force) eq 1) then begin
        bondsenergy=fltarr(MAX_BOND_NUMBER)  ;unsure if this line is needed.  Is this
        ;just a remnant of c?
        bondsenergy=(bondsl-bondslength)^2.0   ;only is used later if do_force is 0
      endif

      unbounddisc=disc-6; unbound disclinationality phew !
      bondsl=0

      ;size of a disclination square on the screen
      discsize=1

      disc5=0 ; # inbounds 5s, total
      disc7=0 ; # inbounds 7s, total



      ; runs Matt's code on finding dislocations
      ; at the return, bound will have the bonded nighbours, in the "edges" format
      ; read the idl help on "triangulate" for an explanation of the format
      ; bound[i] will be 1 if there is a bond or 0 otherwise
      bound=intarr(nedges)
      ButtInOnAble=replicate(1,nvertices,2)
      find_unbound_disc,nvertices,edges,unbounddisc, inbounds,inprocess, bound, ButtInOnAble
      inprocess=0
      ButtInOnAble=0

      disc5=0; total # 5s
      disc7=0; total # 7s
      unbound5=0
      unbound7=0



      ; now will draw the dislocations
      for i1=long(0),nvertices-1 do begin
        for j1=edges[i1],edges[i1+1]-1 do begin
          ; collect angle data from the bond
          if ((bound[j1]) and (i1 lt edges[j1])) then begin
;            plots,[goodx[i1]/!xss,goodx[edges[j1]]/!xss],[1-goody[i1]/!yss,1-goody[edges[j1]]/!yss],/normal,color=!colordisloc,thick=2
            discx[disccount]=(goodx[i1]+goodx[edges[j1]])/2
            discy[disccount]=(goody[i1]+goody[edges[j1]])/2
            discmult=1.0
            if (disc[i1] gt disc[edges[j1]]) then discmult=-1.0
            ang1=!pi+atan(discmult*(goody[i1]-goody[edges[j1]]),discmult*(goodx[i1]-goodx[edges[j1]]))
            discangle[disccount]=ang1
            disccount=disccount+1
          endif
        endfor
      endfor

      for i1=long(0),nvertices-1 do begin
        ;the sizes for bound and unbound disclinations
        discsize=1
        if (unbounddisc[i1] ne 0) then discsize=2
        if ((unbounddisc[i1] eq -1) and (disc[i1] eq 5) and (inbounds[i1])) then begin
          unbound5=unbound5+1
        end
        if (disc[i1] lt 5) then begin
          for discind=-discsize,discsize do begin
;            plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc4
          endfor
        end

        if (disc[i1] eq 5) then begin
          for discind=-discsize,discsize do begin
;            plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc5
          endfor
          if ((goodx[i1] gt inboundsmult*bondlength) and (goodx[i1] lt !xss-inboundsmult*bondlength) and (goody[i1] lt !yss-inboundsmult*bondlength) and (goody[i1] gt inboundsmult*bondlength)) then begin
            disc5=disc5+1

          end
        end


        if ((unbounddisc[i1] eq 1) and (disc[i1] eq 7) and (inbounds[i1])) then begin
          unbound7=unbound7+1
        end

        if (disc[i1] gt 7) then begin
          for discind=-discsize,discsize do begin
;            plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc8
          endfor
        end

        if (disc[i1] eq 7) then begin
          for discind=-discsize,discsize do begin
;            plots,[(goodx[i1]-discsize)/!xss,(goodx[i1]+discsize)/!xss],[1-(goody[i1]+discind)/!yss,1-(goody[i1]+discind)/!yss],/normal,color=!colordisc7
          endfor
          if ((goodx[i1] gt inboundsmult*bondlength) and (goodx[i1] lt !xss-inboundsmult*bondlength) and (goody[i1] lt !yss-inboundsmult*bondlength) and (goody[i1] gt inboundsmult*bondlength)) then begin
            disc7=disc7+1
          end
        end

      endfor

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
      triangulation=polyline(goodx[listedges.toarray()],(!yss-goody[listedges.toarray()]), antialias = 0 ,connectivity=connections,/data, $
        /current, /overplot, NAME = 'TRIANGULATION', color=[0,0,255],thick=disc_thick)

 ;     img_new_all = p1.CopyWindow(border=0,height=sf*(!yss+1))
 ;     write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_all.tif', reverse(img_new_all,2), compression=1
;      w.close




      ;--------------------------------------------------------
      if (do_postscript_defects eq 1) then begin

        openw,postscript_defects_unit,strmid(fs[0],0,strlen(fs[0])-4)+'postscript_defects.ps',/get_lun
        printf,postscript_defects_unit,'%!PS'
        printf,postscript_defects_unit,'%%BoundingBox: ',0,0,imagesize[1],imagesize[2]
        printf,postscript_defects_unit,'1.00 1.00 setlinewidth'
        disc_circle_radius = 3
        unbound_disc_circle_radius = 5
        dislocation_linewidth = 3


        for i1=long(0),nvertices-1 do begin
          if (disc[i1] ne 6) then begin
            ;print out all of the disclinations
            if (disc[i1] lt 5) then printf,postscript_defects_unit,'1 0 1 setrgbcolor'
            if (disc[i1] eq 5) then printf,postscript_defects_unit,'1 0 0 setrgbcolor'
            if (disc[i1] eq 7) then printf,postscript_defects_unit,'0 1 0 setrgbcolor'
            if (disc[i1] gt 7) then printf,postscript_defects_unit,'0 1 1 setrgbcolor'
            printf,postscript_defects_unit,'newpath'
            printf,postscript_defects_unit,goodx[i1],imagesize[2]-goody[i1],disc_circle_radius,' 0 360 arc'
            printf,postscript_defects_unit,'closepath fill'
            if (unbounddisc[i1] ne 0) then begin
              ;now circle the unbound disclinations...
              if (unbounddisc[i1] eq -2) then printf,postscript_defects_unit,'1 0 1 setrgbcolor'
              if (unbounddisc[i1] eq -1) then printf,postscript_defects_unit,'1 0 0 setrgbcolor'
              if (unbounddisc[i1] eq 1) then printf,postscript_defects_unit,'0 1 0 setrgbcolor'
              if (unbounddisc[i1] eq 2) then printf,postscript_defects_unit,'0 1 1 setrgbcolor'
              printf,postscript_defects_unit,'newpath'
              printf,postscript_defects_unit,goodx[i1],imagesize[2]-goody[i1],unbound_disc_circle_radius,' 0 360 arc'
              printf,postscript_defects_unit,'closepath stroke'
            endif
          endif
        endfor ;i1
        ; now draw the dislocations
        printf,postscript_defects_unit,dislocation_linewidth,dislocation_linewidth,' setlinewidth'
        printf,postscript_defects_unit,'1 1 0 setrgbcolor'

        for i1=long(0),nvertices-1 do begin
          for j1=edges[i1],edges[i1+1]-1 do begin
            ; collect angle data from the bond
            if ((bound[j1]) and (i1 lt edges[j1])) then begin
              printf,postscript_defects_unit,'newpath'
              printf,postscript_defects_unit, goodx[i1],imagesize[2]-goody[i1], ' moveto'
              printf,postscript_defects_unit, goodx[edges[j1]],imagesize[2]-goody[edges[j1]], ' lineto'
              printf,postscript_defects_unit,'closepath stroke'
            endif
          endfor ;j1
        endfor ;i1
        free_lun,postscript_defects_unit
      endif ;do_postscript_defects
      ;-------------------------------------------------------------


      print, "Found total 5's and 7's : ", disc5, disc7
      print, "Found total unbound 5's and 7's : ", unbound5, unbound7


      Summary_of_Data[2,i]=unbound5
      Summary_of_Data[3,i]=unbound7

      ; Now we need to find the dislocations and calculate their correlations


      ;output the bond image in the TIFF format.
;      saveimage, strmid(fs[i],0,strlen(fs[i])-4)+'bonds.tif',/tiff
      ;will use this for the overlaying of the images
;      bondsimg=readimage(0)

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
      print,'starting doing the smoothing...'
      t0=systime(1)
      ;      smooth,bcosangle, smoothbcosangle, weights, howmuch
      ;      smooth,bsinangle, smoothbsinangle, weights, howmuch
      fftsmooth,bcosangle, smoothbcosangle, weights, long(bondlength);howmuch
      fftsmooth,bsinangle, smoothbsinangle, weights, long(bondlength);howmuch
      print,'done doing the smoothing...','elapsed time = ',systime(1)-t0
      bcosangle=!NULL
      bsinangle=!NULL

      smoothbangle=-((float(atan(smoothbsinangle, smoothbcosangle))) * 180.0 / !pi ) +180
      smoothbsinangle=!NULL
      smoothbcosangle=!NULL
      HSV_array = replicate(1.0,3,!xss+1,!yss+1)
      HSV_array[0,*,*]=smoothbangle
      rgb_angle_image = replicate(0B,3,!xss+1,!yss+1)
      color_convert, HSV_array, rgb_angle_image, /HSV_RGB

      write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_angle.tif', rgb_angle_image, compression=1

;      rgb_angle_image=reverse(rgb_angle_image,3)
;      wBase = WIDGET_BASE(/COLUMN)
;      wDraw = WIDGET_WINDOW(wBase, X_SCROLL_SIZE=900, Y_SCROLL_SIZE=900, XSIZE=!xss+1, YSIZE=!yss+1,/APP_SCROLL,retain=2)
;      WIDGET_CONTROL, wBase, /REALIZE
;      im_ang = image(rgb_angle_image, /current, IMAGE_DIMENSIONS=[!xss+1,!yss+1],margin=[0.0,0.0,0.0,0.0])
;      im_ang = image(rgb_angle_image, /current, IMAGE_DIMENSIONS=[!xss+1,!yss+1],margin=[0.0,0.0,0.0,0.0])
widg_win.select
      orient_img=image(rgb_angle_image, /current, NAME = 'ORIENT_IMG', margin=0, transparency=50, zvalue=-.01, axis_style=0)
      orient_img.rotate, /reset
      

      if (do_angle_histogram eq 1) then begin
        single_angle_histogram = histogram(smoothbangle,min =0,binsize = !PI/(3 * 60), nbins = 61)
        angle_histogram[*,i] = single_angle_histogram[0:59]
        angle_histogram[0,i] = temporary(angle_histogram[0,i]) + single_angle_histogram[60]
      endif
      single_angle_histogram=0



      ;      Now we have origimg, bondsimg, rgb_angle_image to work with
;      newimg=origimg*.5+rgb_angle_image*.5
      rgb_orig = bytarr(3,!xss+1,!yss+1)
      rgb_orig[0,*,*]=data
      rgb_orig[1,*,*]=data
      rgb_orig[2,*,*]=data
      newimg=rgb_orig*.5+rgb_angle_image*.5

      rgb_angle_image=!NULL

      print,'test point'



      ;      print,'Writing grayscale angle TIFF file'
      if (save_bw_angle_tif eq 1) then write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'smooth.tif',bytscl(smoothbangle,MAX= !pi/3,MIN=0)

      ;In this section, we draw defects over a large image.
      ;first, open a window as a buffer, and draw things there.
      sf = 2; scale factor for the whole image
      w=window(/buffer,dimensions=[sf*(!xss+1),sf*(!yss+1)])
      p1 = image(rebin(newimg,3,sf*(!xss+1),sf*(!yss+1)), /overplot, IMAGE_DIMENSIONS=[sf*(!xss+1),sf*(!yss+1)],margin=[0.0,0.0,0.0,0.0])
      ;add annotations as below


      psym_size= float(sphere_diameter)/!yss * 1024 / 6 * 0.4 ;at spheresize=6 on a 1024x1024 image, 0.4 was about right.
      pcircle_size = float(sphere_diameter)/!yss * 1024 / 6 * 0.75
      disc_thick=3.0 * sf
      circle_thick=1.0 * sf
      fours=where(disc lt 5)
      p=plot(sf*goodx[fours],sf*(!yss-goody[fours]),/data,/overplot, antialias=0,symbol="o",sym_color=[255,0,255],sym_filled=1,sym_size=psym_size,linestyle='none')
      fives=where(disc eq 5)
      p=plot(sf*goodx[fives],sf*(!yss-goody[fives]),/data,/overplot, antialias=0,symbol="o",sym_color=[255,0,0],sym_filled=1,sym_size=psym_size,linestyle='none')
      sevens=where(disc eq 7)
      p=plot(sf*goodx[sevens],sf*(!yss-goody[sevens]),/data,/overplot, antialias=0,symbol="o",sym_color=[0,255,0],sym_filled=1,sym_size=psym_size,linestyle='none')
      eights=where(disc gt 7)
      p=plot(sf*goodx[eights],sf*(!yss-goody[eights]),/data,/overplot, antialias=0,symbol="o",sym_color=[0,255,255],sym_filled=1,sym_size=psym_size,linestyle='none')

      unb_fours=where(unbounddisc eq -2)
      p=plot(sf*goodx[unb_fours],sf*(!yss-goody[unb_fours]),/data,/overplot, antialias=0,symbol="o",sym_color=[255,0,255],sym_size=pcircle_size,linestyle='none',sym_thick=circle_thick)
      unb_fives=where(unbounddisc eq -1)
      p=plot(sf*goodx[unb_fives],sf*(!yss-goody[unb_fives]),/data,/overplot, antialias=0,symbol="o",sym_color=[255,0,0],sym_size=pcircle_size,linestyle='none',sym_thick=circle_thick)
      unb_sevens=where(unbounddisc eq 1)
      p=plot(sf*goodx[unb_sevens],sf*(!yss-goody[unb_sevens]),/data,/overplot, antialias=0,symbol="o",sym_color=[0,255,0],sym_size=pcircle_size,linestyle='none',sym_thick=circle_thick)
      unb_eights=where(unbounddisc eq 2)
      p=plot(sf*goodx[unb_eights],sf*(!yss-goody[unb_eights]),/data,/overplot, antialias=0,symbol="o",sym_color=[0,255,255],sym_size=pcircle_size,linestyle='none',sym_thick=circle_thick)

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
      p=polyline(sf*goodx[listedges.toarray()],sf*(!yss-goody[listedges.toarray()]), antialias = 0 ,connectivity=connections,/data, color=[255,255,0],thick=disc_thick)

      img_new_all = p1.CopyWindow(border=0,height=sf*(!yss+1))
      write_tiff,strmid(fs[i],0,strlen(fs[i])-4)+'_all.tif', reverse(img_new_all,2), compression=1
      w.close

      wBase = WIDGET_BASE(/COLUMN)
      wDraw = WIDGET_WINDOW(wBase, X_SCROLL_SIZE=900, Y_SCROLL_SIZE=900, XSIZE=sf*(!xss+1), YSIZE=sf*(!yss+1),/APP_SCROLL,retain=2)
      WIDGET_CONTROL, wBase, /REALIZE
      im_ang = image(img_new_all, /current, IMAGE_DIMENSIONS=[sf*(!xss+1),sf*(!yss+1)],margin=[0.0,0.0,0.0,0.0])
      img_circled_spheres=!NULL



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
      weights=0



      ; ********   Calculating the bond-bond correlation function


      dimplot1=800


      ;fcorr=fltarr(floor(sqrt(!xss*!xss+!yss*!yss)))  can be moved into the if, as ccorr reassigns
      ;ncorr=fltarr(floor(sqrt(!xss*!xss+!yss*!yss)))  can be moved into the if, as ccorr reassigns



      ;--------------------------------------------------------------
      bondsd=0
      bondscorr=0
      ;--------------------------------------------------------------



      ;ccorr
      ;********************
      if (Ccorr eq 1) then begin

        ; Writes out and prompts for the desired file name

        openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'bonds.dat',/get_lun
        sampling=2; The decimation rate in getting the bonds

        printf,u,sampling

        printf,u,bondcount

        for iii=0L, bondcount-1 do begin
          printf,u,bondsx[iii],bondsy[iii],bondsangle[iii]
        endfor

        bondsangle=0
        bondsx=0
        bondsy=0

        free_lun,u

        ;excised and split off.  there was no reason for them to be run together, it just caused memory
        ;and buffer issues.  hopefully this resolves the problem.

        ;     spawn, './robcor'
        ;       spawn, 'robcor',/hide
        ;
        ;      robcor=read_ascii('robcor.dat');
        ;       dcorr=transpose(robcor.field1[0,*])
        ;       ncorr=transpose(robcor.field1[2,*])
        ;       fcorr=transpose(robcor.field1[1,*])
        ;
        ;       dimplot=600    ; current work area!  previous code was dimplot=300
        ;
        ;
        ;       window,1,xsize=dimplot+1,ysize=dimplot
        ;       plot, dcorr(0:dimplot), fcorr(0:dimplot), xtitle='pixels', ytitle='g6(r)',psym=0
        ;
        ;       CorrelationLenghTest=abs(fcorr(0:dimplot)-exp(-1.0))
        ;       CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
        ;       print,'The direct correlation length seed was ', Min_Subscript, ' .'
        ;
        ;       A=[.5,Min_Subscript]
        ;
        ;
        ;       dimfit=200;
        ;       minfit=20;
        ;       iter=0
        ;       chisq=0
        ;       yfit = curvefit(dcorr(minfit:dimfit), fcorr(minfit:dimfit), ncorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
        ;       X=indgen(dimfit)
        ;       ;plot out the determined function in a diffference color
        ;       ;F = (EXP(-X/A[0]))
        ;       F = A[0]*(EXP(-X/A[1]))
        ;       oplot,F,psym=0, color=254
        ;       ;blue equals 256*127
        ;
        ;
        ;       print,'The C correlation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
        ;       ;print,'The imagetranslate correlation length was ', A[0],' and its coefficient was', ' N/A'
        ;       print,'It required ', iter , ' iterations, chisq=',chisq
        ;
        ;       print, 'Writing robcor file...'
        ;       openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'robcor.dat',/get_lun
        ;
        ;       for LongIndex=0L,n_elements(ncorr)-1 do begin
        ;         printf,u,dcorr[LongIndex],fcorr[LongIndex], ncorr[LongIndex]
        ;       endfor
        ;       free_lun,u
        ;       close,u
        ;
        ;       Summary_of_Data[1,i]=A[1]
      endif ;(Ccorr eq 1)
      bondsx=0
      bondsy=0
      dcorr=0
      fcorr=0
      ncorr=0
      ;ccorr
      ;--------------------------------------------------------------



      ;Gcorr
      ;********************
      if (Gcorr eq 1) then begin




        Gn=size(data) & Gn=Gn(1:Gn(0))
        Gf=abs(shift(fft(data),Gn(0)/2,Gn(1)/2))
        Gf(Gn(0)/2, Gn(1)/2)=0
        bGf=Gf[Gn[0]/2-70:Gn[0]/2+70,Gn[1]/2-70:Gn[1]/2+70]
        data=0
        showimage,bytscl(bGf),1,wGfft

        ;HEY!  THIS ISN'T USED
        tmp=max(Gf,Gw)

        Gx=(Gw mod Gn(0))-Gn(0)/2
        Gy=Gw/Gn(0)-Gn(1)/2
        G=fltarr(6,2)
        bGf=0
        tmp

        for Gk=0,5 do begin
          Gtheta=Gk*!pi/3.
          Grot=[[cos(Gtheta),sin(Gtheta)],[-sin(Gtheta),cos(Gtheta)]]
          GR=[Gx,Gy]#Grot
          G[Gk,0]=GR[0]
          G[Gk,1]=GR[1]
          Gff=Gf[Gn[0]/2+GR[0]-20:Gn[0]/2+GR[0]+20, Gn[1]/2+GR[1]-20:Gn[1]/2+GR[1]+20]
          Gmax=max(Gff,Gw)
          G[Gk,0]=((Gw mod 41)+GR[0]-20)
          G[Gk,1]=(Gw/41+GR[1]-20)
          print,"Max value =", Gff[(Gw mod 41),Gw/41]
          print, "Neighbouring values = "
          for Gj=-5,5 do begin
            for Gi=-5,5 do begin
              if(Gf[(Gw mod 41)+Gi+Gn[0]/2+GR[0]-20,Gw/41+Gn[1]/2+Gj+GR[1]-20] gt Gf[Gn[0]/2+(Gw mod 41)+GR[0]-20,Gn[1]/2+Gw/41+GR[1]-20]) then print, "Alarm,neighboring value higher ! "
            endfor
          endfor
          gr=0
          gf=0
          Grot=0

          ;print,"The radius of the Fourier radius for peak ",Gk," is ", sqrt(G[Gk,0]^2+G[Gk,1]^2)

          plots,G[Gk,0]+70, G[Gk,1]+70,color=1000L,/device,psym=3,thick=1,symsize=2


        endfor


        Gfactors=1+(indgen(100)-50.)/250.0
        Gdifs=fltarr(100)

        for Gkk=0,99 do begin
          Gdifs[Gkk]=Gscale(G,Gfactors[Gkk])
        endfor
        Gmin=min(Gdifs,Gbestfactor)
        Gfact=Gfactors[Gbestfactor]
        print, "Gfact=",Gfact
        ;stop
        Gfactors=0
        Gdifs=0

        ;Gfact=1

        Gxreal=Gx*2.0*!pi/Gn(0)
        Gyreal=Gy*2.0*!pi*Gfact/Gn(1)
        print, "Greal =  ", Gxreal, Gyreal
        Gn=0
        Gx=0
        Gy=0


        ;showimage,data,1,datanew
        ;plots,[500,500+300./Gxreal], [500,500-300./Gyreal],color=!colorbond, thick=2,/device


        ;stop

        openw,u,'vertices.dat',/get_lun
        sampling=5; The decimation rate in getting the bonds

        printf,u,sampling
        printf,u,nvertices
        printf,u,1*Gxreal,-1*Gyreal

        for iii=0L, nvertices-1 do begin
          printf,u,goodx[iii],goody[iii]/Gfact
        endfor
        free_lun,u

        spawn, './Grobcor'

        Grobcor=read_ascii('Grobcor.dat');
        dGcorr=transpose(Grobcor.field1[0,*])
        nGcorr=transpose(Grobcor.field1[2,*])
        fGcorr=transpose(Grobcor.field1[1,*])

        dimplot=300


        window,1,xsize=dimplot+1,ysize=dimplot
        plot, dGcorr(0:dimplot), fGcorr(0:dimplot), xtitle='pixels', ytitle='gG(r)',psym=0
        oplot, dGcorr(0:dimplot), bytscl(nGcorr(0:dimplot))/255.,psym=0, color=rgbcolor(0,255,0)
        GCorrelationLenghTest=abs(fGcorr(0:dimplot)-exp(-1.0))
        GCorrelationLengthSeed=min(GCorrelationLenghTest,GMin_Subscript)
        print,'The direct correlation length seed was ', GMin_Subscript, ' .'


        A=[1,GMin_Subscript]

        ;A=[GMin_Subscript]


        dimfit=200;
        minfit=20;
        iter=0
        chisq=0
        yfit = curvefit(dGcorr(minfit:dimfit), fGcorr(minfit:dimfit), nGcorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
        ;yfit = curvefit(dGcorr(minfit:dimfit), fGcorr(minfit:dimfit), nGcorr(minfit:dimfit), A, SIGMA_A, FUNCTION_NAME = 'funct1', ITMAX=100, ITER=iter, chisq=chisq)
        X=indgen(dimfit)
        ;plot out the determined function in a diffference color
        ;F = (EXP(-X/A[0]))
        F = A[0]*(EXP(-X/A[1]))
        oplot,F,psym=0, color=254

        ;blue equals 256*127


        print,'The C  Gcorrelation length was ', A[1],'+/-',Sigma_a[1],' and its coefficient was', A[0],'+/-',Sigma_a[0]
        ;print,'The imagetranslate correlation length was ', A[0],' and its coefficient was', ' N/A'
        ;print,'The C  Gcorrelation length was ', A[0],'+/-',Sigma_a[0]
        print,'It required ', iter , ' iterations, chisq=',chisq

        print, 'Writing robcor file...'
        openw,u,strmid(fs[i],0,strlen(fs[i])-4)+'Grobcor.dat',/get_lun

        for LongIndex=0L,n_elements(nGcorr)-1 do begin
          printf,u,dGcorr[LongIndex],fGcorr[LongIndex], nGcorr[LongIndex]
        endfor
        free_lun,u
        close,u

        Summary_of_Data[5,i]=A[1]
        ;Summary_of_Data[5,i]=A[0]


      endif ;(Gcorr eq 1)
      data=0
      ;Gcorr
      ;--------------------------------------------------------------




      ; From here on the code is taken from Anglecorr.pro, and the point is to do the direct
      ; correlation on the image itself rather than on the fourier transform


      ;direct
      if (directcorr eq 1) then begin
        pi=3.141592
        dimplot=256
        sampling=8 ; the size of the sampling square for direct correlations
        dimfit=64 ; the size of the picture over which correlation function is fitted
        fitcutoff=4

        n=size(smoothbangle);
        dim1=n(1)
        dim2=n(2)
        n=0
        dim=ceil(dim1*sqrt(2))
        corr=fltarr(dim)
        pairs=fltarr(dim)
        ;help, corr

        samplingdim1=dim1/sampling
        samplingdim2=dim2/sampling
        yvar1=0
        yvar2=0

        for j=1L,samplingdim1*samplingdim2-1 do begin
          xvar1=sampling*(j mod samplingdim1)
          yvar2=yvar1
          yvar1=sampling*(j/samplingdim1)
          ;if yvar2 ne yvar1 then begin
          ;    print, 'Line # = ',yvar1
          ;endif
          for k=0L,j do begin

            xvar2=sampling*(k mod samplingdim1)
            yvar2=sampling*(k/samplingdim1)
            d=round(sqrt((xvar1-xvar2)*(xvar1-xvar2)+(yvar1-yvar2)*(yvar1-yvar2)))
            ;print, 'd=',d
            corr[d]=temporary(corr[d])+cos(6*(smoothbangle[xvar1,yvar1]-smoothbangle[xvar2,yvar2]))
            pairs[d]=temporary(pairs[d])+1;
          endfor
        endfor
        smoothbangle=0

        for j=0L,dim-1 do begin
          if pairs[j] ne 0 then begin
            corr[j]=temporary(corr[j])/pairs[j]
          endif
        endfor

        CorrelationLenghTest=abs(corr-exp(-1.0))
        CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
        print,'The direct correlation length seed was ', Min_Subscript, ' .'
        CorrelationLenghTest=0
        CorrelationLengthSeed=0

        A=[1,Min_Subscript]
        ;A=[Min_Subscript]



        ;window,0,xsize=dim1/2+50,ysize=dim2/2+50
        ;tvscl,angle,25,25

        index=where(pairs)
        ;restrict=where(index lt dimplot)
        index=index[where(index lt dimplot)]


        window,1,xsize=dimplot+1,ysize=dimplot+1
        plot, index, corr[index], xtitle='pixels', ytitle='g6(r)',psym=0

        ;restrict2=where((index lt dimfit) and (index gt fitcutoff))
        index=index[where((index lt dimfit) and (index gt fitcutoff))]
        yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct')

        yfit = curvefit(index, corr[index], pairs[index], A, SIGMA_A, FUNCTION_NAME = 'funct', ITMAX=100, ITER=iter, chisq=chisq)
        index=0
        corr=0
        pairs=0


        X=indgen(dimplot)
        ;plot out the determined function in a diffference color
        ;F = (EXP(-X/A[0]))
        F = A[0]*(EXP(-X/A[1]))
        oplot,F,psym=0, color=254
        ;blue equals 256*127

        print,'The direct correlation length was ', A[1],' and its coefficient was', A[0],' iter=',iter,' chisq=',chisq
        ;print,'The direct correlation length was ', A[0],' and its coefficient was', ' N/A'
        Summary_of_Data[2,i]=A[1]

      endif
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
        if (do_force eq 1) then begin
          widget_control,wbenergy,/destroy
          widget_control,wcombinedenerg,/destroy
          ;    widget_control,wbonds,/destroy
          ;     widget_control,wimage,/destroy



        end else begin
          if (Gcorr eq 1) then begin
            ;    widget_control,wbonds,/destroy
            widget_control,wimage,/destroy
            ;    widget_control,wGfft,/destroy
          end else begin
            ;    widget_control,wbonds,/destroy
            ;    widget_control,wimage,/destroy
          end
        end
      end
      if ((Ccorr eq 1) or (disccorr eq 1)) then wdelete,1



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

    print,'total time:',systime(1)-time0,'elapsed seconds = '

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






pro try, newimg

  newred=reform(newimg[0,*,*])
  newgreen=reform(newimg[1,*,*])
  newblue=reform(newimg[2,*,*])
  showimage,[[[newred]],[[newgreen]],[[newblue]]],3,wcombined

end


function readimage, dummy
  current_window = !d.window
  xsize = !d.x_size
  ysize = !d.y_size
  ;window, /pixmap,/free, xsize=xsize, ysize=ysize, retain=1
  ;device, copy=[0, 0, xsize, ysize, 0, 0, current_window]
  image = tvrd(0,0,xsize,ysize,order=0,true=1)
  ;wdelete, !d.window
  wset, current_window
  return,image
end


pro fftsmooth, input, output, weights, howmuch

  print, 'from fftsmooth: howmuch = ',howmuch

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



;********************************************************
;USED FOR FITTING THE EXPONENTIAL DECAY
;********************************************************
PRO funct, X, A, F, PDER

  F = A[0] * (EXP(-X/A[1]))
  ;If the function is called with four parameters,
  ;calculate the partial derivatives:


  IF N_PARAMS() GE 4 THEN BEGIN
    ;PDER's column dimension is equal to the number of elements
    ;in xi and its row dimension is equal to the number of
    ;parameters in the function F:
    pder = FLTARR(N_ELEMENTS(X), 2)
    ;Compute the partial derivatives with respect to a0 and
    ;place in the first row of PDER.
    pder[*, 0] =  EXP(-X/A[1])
    ;Compute the partial derivatives with respect to a1 and
    ;place in the second row of PDER.

    pder[*, 1] = (A[0]*X/(A[1]*A[1]))*EXP(-X/A[1])
  ENDIF
END



PRO funct1, X, A, F, PDER

  F =  (EXP(-X/A[0]))
  ;If the function is called with four parameters,
  ;calculate the partial derivatives:


  IF N_PARAMS() GE 4 THEN BEGIN
    ;PDER's column dimension is equal to the number of elements
    ;in xi and its row dimension is equal to the number of
    ;parameters in the function F:
    pder = FLTARR(N_ELEMENTS(X), 1)
    ;Compute the partial derivatives with respect to a0 and
    ;place in the first row of PDER.
    pder[*, 0] = (X/(A[0]*A[0]))* EXP(-X/A[0])
    ;Compute the partial derivatives with respect to a1 and
    ;place in the second row of PDER.

  ENDIF
END





function colorred,index
  return,255*(1+sin(index*2*!pi/255))/2
end

function colorgreen,index
  return,255*(1+sin(index*2*!pi/255-2*!pi/3))/2
end

function colorblue,index
  return,255*(1+sin(index*2*!pi/255+2*!pi/3))/2
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

  if (!draw_recursively eq 0) then begin
    Common globalgood, goodx, goody
    plots,[!goodx[vertex1]/!xss,!goodx[vertex2]/!xss],[1-!goody[vertex1]/!yss,1-!goody[vertex2]/!yss],/normal,color=!colordisloc,thick=2
  end
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

  if (!draw_recursively eq 0) then begin
    Common globalgood, goodx, goody
    plots,[!goodx[vertex1]/!xss,!goodx[vertex2]/!xss],[1-!goody[vertex1]/!yss,1-!goody[vertex2]/!yss],/normal,color=!colorwhite,thick=2
    plots,[!goodx[vertex1]/!xss,!goodx[vertex2]/!xss],[1-!goody[vertex1]/!yss,1-!goody[vertex2]/!yss],/normal,color=!colorbond,thick=1.5
  end
end




function it_would_help_to_bond_nicely ,vertex1,vertex2,edges, unbounddisc, inbounds, inprocess,bound,outofboundsok
  nvertices=n_elements(inbounds)
  if ((sign_of(unbounddisc[vertex1]) eq -sign_of(unbounddisc[vertex2])) and (inprocess(vertex2) eq 0) ) then begin
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
    if (inprocess(vertex2) eq 0) then begin
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
        inprocess(vertex1)=0
        return,1
      end
    end
  endfor
  inprocess(vertex1)=0
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
    inprocess(vertex)=1
    jump0:   if (unbounddisc[vertex] eq 0) then goto,jump1
    if (try_to_find_a_mate_nicely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound,0) eq 0) then goto,jump1
    goto,jump0

    jump1:   if (unbounddisc[vertex] eq 0) then goto,jump2
    if (try_to_find_a_mate_rudely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble,0) eq 0) then goto,jump2
    goto,jump1

    jump2:   if (unbounddisc[vertex] eq 0) then goto,jump3
    if (try_to_find_a_mate_nicely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, 1) eq 0) then goto,jump3
    goto,jump2

    jump3:   if (unbounddisc[vertex] eq 0) then goto,jump4
    if (try_to_find_a_mate_rudely_for(vertex,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble, 1) eq 0) then goto,jump4
    goto,jump3

    jump4:   inprocess(vertex)=0
  end
end


pro find_unbound_disc,nvertices,edges,unbounddisc, inbounds,inprocess,bound, ButtInOnAble
  for i=long(0), nvertices-1 do begin
    ;     print, "vertex ", i, unbounddisc[i]
    try_to_find_mates_for,i,nvertices,edges,unbounddisc, inbounds, inprocess,bound, ButtInOnAble
  end
end


function rgbcolor,R,G,B

  return,R+256L*(G+256L*B)

end


function Gscale,G,mult
  Gcopy=G
  Gcopy[*,1]=G[*,1]*mult
  Gradius=sqrt(Gcopy[*,1]^2.+Gcopy[*,0]^2.)
  Grad=total(Gradius)/6
  ;print, "Grad for mult=",mult," is ", Grad
  result=0
  Gdif=sqrt(total((Gradius-Grad)^2.)/6.)
  ;print, "Gdif for mult=",mult," is ", Gdif
  return,Gdif

end

