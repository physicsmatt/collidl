

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

