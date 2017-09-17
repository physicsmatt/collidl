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
