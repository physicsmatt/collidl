;This file is a "minimal working example" of a problem I'm having.
;When I create a window in a widget, the window doesn't update until the program is done.

base1 = WIDGET_BASE(/COLUMN, /TLB_SIZE_EVENTS)
winwidg = WIDGET_WINDOW(base1, xsize=800, ysize=800)

WIDGET_CONTROL, base1, /REALIZE
WIDGET_CONTROL, winwidg, GET_VALUE = w


w.select
p1=plot([2,3],[4,5], /overplot, color='red')
print,"Why don't I see the red plot yet?

;These attempts below don't help.
;WIDGET_CONTROL, winwidg, /REALIZE
;WIDGET_CONTROL, winwidg, /input_focus
;w.refresh

wait,5
p2 = plot([2,3],[5,4], /overplot, color='blue')
end
