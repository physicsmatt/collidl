;This is an example written by Matt Trawick in July 2017 that shows how to use new graphics (v 8.0+) and widgets to handle big files.

;Here is a test image to save and display
sf=64L  ;scale this from 4 to 64, for instance
rightedge = sf*128-1
testdata = bytarr(3,128*sf,128*sf)
testdata[2,*,*]=255
testdata[1,64*sf,*]=255
testdata[1,*,64*sf]=255

;First, put data in a window, but without the display, using /buffer switch.
w=window(/buffer,dimensions=[128L*sf,128L*sf])
p1 = image(testdata, /current, IMAGE_DIMENSIONS=[128L*sf,128L*sf],margin=[0.0,0.0,0.0,0.0])
;add annotations as below
e1 = ellipse(64L*sf,64L*sf,/data,major=32L*sf,eccentricity=0.0,target=p1,fill_color=[255,0,0])
l1 = POLYLINE([0,0], [0,50], /DATA, TARGET=p1, COLOR=[0,255,0], THICK=0,antialias=0)
l2 = POLYLINE([rightedge,rightedge], [0,50], /Data, TARGET=p1, COLOR=[0,255,0], THICK=0,antialias=0)
;p1.save, "matttestdata.tif", compression=1 ;DOES NOT ALLOW LZW compression
savedata = p1.CopyWindow(border=0,height=128L*sf);,width=1024)
cd,'C:\Users\mtrawick\Desktop\github\collidl\collidl'
write_tiff,'matttestdata.tif', savedata, compression=1
help,savedata
w.close

;Now, if we want to display the annotated data, but are worried about memory, use a widget with a scroll bar. 
wBase = WIDGET_BASE(/COLUMN)
wDraw = WIDGET_WINDOW(wBase, X_SCROLL_SIZE=512, Y_SCROLL_SIZE=512, XSIZE=sf*128, YSIZE=sf*128,/APP_SCROLL,retain=2);,graphics_level=2)
WIDGET_CONTROL, wBase, /REALIZE
; Retrieve the newly-created Window object.
;WIDGET_CONTROL, wDraw, GET_VALUE=oWin
; Make sure this is the current window
p1 = image(savedata, /current, IMAGE_DIMENSIONS=[128L*sf,128L*sf],margin=[0.0,0.0,0.0,0.0]);,scale_factor=1),/widgets)

;p = PLOT(/TEST, /CURRENT, /FILL_BACKGROUND)

end
