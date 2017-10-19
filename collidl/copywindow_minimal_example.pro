
  ;This code shows an apparent bug (or my misunderstanding?) in the copywindow method for windows made with widget_window().
  base1 = WIDGET_BASE()
  wDraw = WIDGET_WINDOW(base1)
  WIDGET_CONTROL, base1, /REALIZE
  WIDGET_CONTROL, wDraw, GET_VALUE = w
  ;The following REDRAW and UPDATE commands had no effect
  WIDGET_CONTROL, wDraw, REDRAW=0
  WIDGET_CONTROL, wDraw, REDRAW=1
  WIDGET_CONTROL, wDraw, UPDATE=1
  w.select
  plot1=plot([50,100],[50,100], /current)
  print,"TOGGLE BREAKPOINT ON THIS LINE"
  ;The CopyWindow method below fails, but works if a breakpoint is added to the print statement above
  img = w.CopyWindow()
  w2=image(img)

  ;As subsequent lines show, CopyWindow works as expected if graphc object is created by window function instead of widget_window function
  w3 = window()
  plot3=plot([100,50],[50,100], /current)
  img4 = w3.CopyWindow()
  w4=image(img4)
end