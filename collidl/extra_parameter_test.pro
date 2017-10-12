y1 = FINDGEN(10)
y2 = y1 * 5
y3 = -2 * y1 + 40

y4 = 0.5 * y1^2
; Put four plots in the same window
; and use the same color, linestyle,
; symbol and yrange.
props = {current:1, color:'blue', $
  linestyle:'dashed', symbol:'triangle', $
  yrange:[0,50]}

myWindow = WINDOW( $
  WINDOW_TITLE='Four plots with common properties')
myPlotOne = PLOT(y1, LAYOUT=[2,2,1], _REF_EXTRA=props)
myPlotToo = PLOT(y2, LAYOUT=[2,2,2], _REF_EXTRA=props)
myPlotThree = PLOT(y3, LAYOUT=[2,2,3], _REF_EXTRA=props)
myPlotFour = PLOT(y4, LAYOUT=[2,2,4], _REF_EXTRA=props)
end
