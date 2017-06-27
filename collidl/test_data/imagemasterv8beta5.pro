
;This program is designed to correct for drift exhibited by microscopy images when the coefficients of shift
;are not known.

; MAIN PROGRAM

; Search for current work area to find code changes in progress
; USAGE notes (4/04/06, by Brian Salmons)
; 1.  Start IDL
; 2. At IDL prompt, type: imagemaster+(#version), (switches)' or press F5 in current builds.


;V.8 notes (as of 5/21/06)
;   By Brian Salmons
;   Currently: implementing blocks, unfinished.
;

;V.7 notes (as of 5/21/06)
;   By Brian Salmons
;   This version of the code is an implementation of the sliver algorithm.  The sliver algorithm does not account for sliver
;drift, but rather is a faster method than the earlier versions.  It precomputes all the possible shifts for each "strip" in
;the sliver so that they are not recomputed several thousand times later on.  The second portion of the program then focuses
;on computing where the strips would be.  After it computes this, it totals all the differences for all the strips.  Largely,
;this build focuses on the new faster method.


;V.6 notes (as of 4/11/06)
;   By Brian Salmons
;
;   This version of the code eliminates the hard coding and makes the program based on movement percentages.
;Each cooefficent has a percentage allowed and the program determines the range allowed for the coefficients
;Furthermore, there is an added flag called /precision, which is the precision of the algorithm along the top line.
;Since the fit is projected from the bottom, we have control over how the top line is affected, and the number of pixels
;to fit too.
;
;   *Unfinished version, some bugs may exist that yield improper results.




;V.5 notes (as of 4/04/06):
;   By Brian Salmons
;
;   This version of the program still has limits hard coded.  In this version, the maximum amount of shift is based
;on the size, such as 10% of the image size.  This has been replaced in more recent versions with a method based on
;the amount of shift that it is probable we'll see in images.  For instance, it's quite possible that there may be a shift
;of 10%, but we are unlikely to see all of this from the quadratic term.  This is the most recent version of the program
;still implementing the old max shift based code.  In this code, A2 and B2 terms are not implemented, both algorithms
;work correctly.



    ;*****************************************************

       ; Safe Switches :
       ;     /testdiff : uses a difference calculation rather than cross correlation to compute the best image.
       ;       This is usefull if the image is heavy centered around bright or dark images, as the difference ignores
       ;       the actual color values and only looks at the difference between them.  If there are many points of
       ;       the same color, the differences between two cross correlations is reduced and eventually may lead to
       ;       inaccurate results.
       ;
       ; Parameters:
       ;   precision=(1,2,....):  a number that represents how accurate the program should be.  This specifically
       ;   references how accurate it should be at the TOP line.  This also allows sub-pixel accuracy, for
       ;   better mid-image control.  A value of two would mean that pixels along the top row would shift by 2
       ;   through the coefficients, etc.

    ;*****************************************************


Pro imagemasterv8beta5,testdiff=testdiff,precision=precision,blocksize=blocksize,output=output,displaydiff=displaydiff,testind=testind

print,''

debug=1 ;(1=on)This is a software level flag.  This is simply for convenience to the coder to allow F5 simple runs with full detail and precision.
       ;Furthermore, all the flags can be set in the below block so if other settings are desired, change the below block.
if (debug eq 1) then begin
    precision=1
    blocksize=8
    output=1
    displaydiff=0
    testind=1
endif


; This control variable sets the precision to which the algorithm should check.  This is a pixel value,
;and represents the furthest from the base of the image (where the image and sliver should be close).
;As such, this variable controls how accurate the algorithm computes at the top of the image.
;--------------------------------------------------
;Introduced: V.6
;Fixed for A2,B2 : V.8
;Defaults to 1 (most accurate), if no precision is specified, or the program is run via F5
;--------------------------------------------------
if (keyword_set(precision)) Then Begin
Endif Else Begin
    precision=1  ;Default
EndElse


; This portion handles the image selection, and unfortunately starts at the selected program path, and so if
;the two images are in the same folder, the user will have to navigate to that folder twice if the program path
;is not specified to be that folder.  Currently, only one image may be run at a time.  Also, the images are being
;rotated around, though I AM NOT SURE THAT IDL ISN'T DOING THIS ALREADY.  This flip, in theory is so that "0" in the y
;axis is at the "bottom," where the two images line up.  I shall make a test image to test this.
Basefile=dialog_pickfile(get_path=ps,title="Please select a base image to transform",filter=['*.tif'])
;Baseimage=reverse(Read_Tiff(Basefile),2)
Baseimage=Read_Tiff(Basefile)
;Baseimage=rot(Read_Tiff(dialog_pickfile(get_path=ps,title="Please select a base image to transform",filter=['*.tif'])),180)
Testfile=dialog_pickfile(get_path=ps,title="Please select a sliver to perform comparisons",filter=['*.tif'])
;testsliver=reverse(Read_Tiff(Testfile),2)
testsliver=Read_Tiff(Testfile)
;testsliver=rot(Read_Tiff(dialog_pickfile(get_path=ps,title="Please select a sliver to perform comparisons",filter=['*.tif'])),180)
intcopy=FIX(baseimage) ;Int so that diffs doesn't explode later on when it tries to subtract and square.
time0=systime(1)   ;Just a timing control

;---------Testing code here--------------
;TVSCL, Baseimage
;wait,2
;----------------------------------------

;records both sizes, which are important in finding the maximum area that the drift could cover.
imagesize=Size(baseimage,/Dimensions)
sliversize=Size(testsliver,/Dimensions)


;Percentages of shifts that are allowed.  These are arbitrary in this version, and will be code driven
;until the implementation of a user interface.
A0percent=.05
A1percent=.04
A2percent=.04
B0percent=.01
B1percent=.04
B2percent=.04

;These record in pixels how far the allowed shift is.  Dynamically computed.
MaxA0drift=Floor(A0percent*imagesize[1])
MaxA1drift=Floor(A1percent*imagesize[1])
MaxA2drift=Floor(A2percent*imagesize[1])
MaxB0drift=Floor(B0percent*imagesize[1])
MaxB1drift=Floor(B1percent*imagesize[1])
MaxB2drift=Floor(B2percent*imagesize[1])

;this takes a smaller chunk of the base image, so that less is slug around in memory.  In general, the amount of drift is
;less than the image size, and so this takes the maximum amount of area covered at the most extreme set.
print,imagesize[0]/2-sliversize[0]/2-MaxA0drift-MaxA1drift-MaxA2drift,imagesize[0]/2-sliversize[0]/2+MaxA0drift+MaxA1drift+MaxA2drift+sliversize[0]+(-1),0,imagesize[1]-1
maximage=intcopy[imagesize[0]/2-sliversize[0]/2-MaxA0drift-MaxA1drift-MaxA2drift:imagesize[0]/2-sliversize[0]/2+MaxA0drift+MaxA1drift+MaxA2drift+sliversize[0]+(-1),0:imagesize[1]-1]
subsize=Size(maximage,/Dimensions)

;These are the computations to yeild the maximum coefficient settings in terms of the matrix shift.
;These are the actual numbers that the program works with.
MaxA0=MaxA0drift
MaxA1=MaxA1drift/(imagesize[1]*1.0)
MaxA2=MaxA2drift/(imagesize[1]*1.0)^2
MaxB0=MaxB0drift
MaxB1=MaxB1drift/(imagesize[1]*1.0)
MaxB2=MaxB2drift/(imagesize[1]*1.0)^2

;Storage for the "best fit."  Different variables are used by the two algorithms so that they can be tested
;against each other at the same time, if need be.
bestdiffA0=5
bestdiffA1=5
bestdiffA2=5
bestdiffB0=5
bestdiffB1=5
bestdiffB2=5
bestdiff=uint(-1)


;obsolete?
A2=0
B2=0
A1=0
B1=0

;This set of varialbes record how many pixels of drift in either axis that will be encountered by the program.
;This is for matrix offsets.
totaldriftA=MaxA0drift+MaxA1drift+MaxA2drift
totaldriftB=MaxB0drift+MaxB1drift+MaxB2drift

counter=long(0)



;************************
;   Block Calculations
;************************
;   Block size is determined by the maximum drift that may occur at a maximum.  This may be changed
;to become more dynamic in later versions, however, as of now it is separate from the loop,
;and so worst case must be assumed.
if (keyword_set(blocksize)) Then Begin
endif else begin
    maxslope=(MaxA1+2*MaxA2*(subsize[1]-1) > MaxB1+2*MaxB2*(subsize[1]-1))  ;< or > ???
    blocksize=fix(1/maxslope)
endelse
numblocks=subsize[1]/blocksize+((subsize[1] mod blocksize) gt 0) ;in case we have leftover
remainderexists=((subsize[1] mod blocksize) gt 0)
remainderarea=subsize[1] mod blocksize


;************************
;Begin Strip Calculations
;************************
time1=systime(1)
;These two are  simply how many points along the top there are that are hit in a pass.  NEEDS TO BE FIXED!
;CURRENTLY SEMI-BROKEN!  It doesn't take precision into account.
Apoints=totaldriftA*2+1
Bpoints=totaldriftB*2+1


diffs=ulindgen(numblocks,Apoints,Bpoints)
diffs[*,*,*]=0
areas=intarr(numblocks,Bpoints)+sliversize[0]
areaarr=indgen(numblocks)
areas[*,*]=0

;The limits of to the location of the sliver center on the smaller chunk of the base image
smin=subsize[0]/2-sliversize[0]/2
smax=subsize[0]/2-sliversize[0]/2+sliversize[0]-1
width=smax-smin+1
sminy=0
smaxy=sliversize[1]-1

;********************
;   Diffs Creation
;Cycle through all possible shifts for each block and compute diffs
;********************
for j=-totaldriftB,totaldriftB do begin
    for i=-totaldriftA,totaldriftA do begin
        for block=0+((j/blocksize)*(j gt 0)),numblocks-1+((j+remainderarea)/(blocksize)*(j lt 0))+((j/remainderarea)*(j lt 0 and remainderarea gt 0)),1 do Begin
        diffs[block,i+totaldriftA,j+totaldriftB]=total((maximage[smin+i:smax+i,(((block)*blocksize-j)>(-j*(j lt 0))):(((block+1)*blocksize-1-j)<(smaxy-j*(j gt 0)))]$
            - Testsliver[*,((block)*blocksize>(j*(j gt 0))):(block+1)*blocksize-1<(smaxy+j*(j lt 0))]*1.0)^2)
         if (block eq numblocks-1) then Begin
          endif
            counter=counter+1
        endfor
    endfor ;i
    for block=0+((j/blocksize)*(j gt 0)),numblocks-1+((j+remainderarea)/(blocksize)*(j lt 0))+((j/remainderarea)*(j lt 0 and remainderarea gt 0)),1 do Begin
        ;areas[block,j+totaldriftB]=(width)*(((block*blocksize + j*(j lt 0)) < (smaxy + j*(j lt 0))) - (((block - 1)*blocksize - (j*(j gt 0)) > (j*(j gt 0)))) + (blocksize + j*(j lt 0)*width)*(block eq 0))
       areas[block,j+totaldriftB]=(width)*((block+1)*blocksize<(smaxy+j*(j lt 0)+1)-((block)*blocksize>(j*(j gt 0))))
         ;areas[block,j+totaldriftB]=(smax-smin+1)*((block)*blocksize<(smaxy+j*(j lt 0))-((block-1)*blocksize>(j*(j gt 0)))) + (blocksize + j*(j lt 0)*width)*(block eq 0)
        ;areas[block,j+totaldriftB]=(smax-smin+1)*((block eq 0 or block eq 0+(j/blocksize))+(block)*blocksize<(smaxy+j*(j lt 0))-((block-1)*blocksize>(j*(j gt 0))))
    endfor
    if(remainderexists) then BEGIN
        areas[numblocks-1,j+totaldriftB]=(smax-smin+1)*((remainderarea+j*(j lt 0))*((smax-smin+1)*(remainderarea+j) gt 0))
    endif
endfor ;j
 time1=systime(1)-time1

print, "time for setup and completion of step 1",time1
print, "PHASE 1.  loop cycles",counter
;diffs[] should now represent all the possible shifts, in strip form.  It is in [y,deltax, deltay] format.

yarrgen = indgen(numblocks,/ulong)
;yarrgen = indgen(numblocks-60,/ulong)+30
;yarrgen2 = indgen(numblocks-1)-1+remainderarea  ;Possibly obsolete with the new error prevention code.
xarr = ulonarr(numblocks)
yarr = ulonarr(numblocks)

yarrgensize=size(yarrgen,/dimensions)  ;can probably be replaced by numblocks

if (keyword_set(displaydiff)) Then Begin
    print,"Handling testing display parameters.
    For block=0,numblocks-1,1 do begin
       tv,bytscl(diffs[block,*,*]),xsize=256,ysize=256
       wait,5
    endfor
Endif Else Begin

EndElse

if (keyword_set(output)) Then Begin
stringarr=STRSPLIT(Basefile,'.',/extract)
openw,1,stringarr[0]+"_data.txt"
diffsize=size(diffs,/dimensions)
;formatstring="("+string(diffsize[2])+"I)"
;formatstring="("+string(diffsize[1])+"I)"
formatstring="("+string(diffsize[1])+"I7)"
;blockformattemp=strtrim(""+string(diffsize[0])+"I)")
;blockformatstring='("Block: "'+","+strtrim(""+string(diffsize[0])+"I)")
;blockformattwo=
For block=0,numblocks-1,1 do begin
    ;printf,1,format=blockformatstring,string(""+Block+"("+string((block+1)*blocksize-1<(smaxy+j*(j lt 0)))+"-"+string(((block)*blocksize>(j*(j gt 0))))+")")     ;Only works on square images!
    printf,1,+block
    ;For X=0,diffsize[1]-1,1 do begin
       ;printf,1,format=formatstring,diffs[block,X,*]
       printf,1,format=formatstring,diffs[block,*]
       printf,1," "
    ;EndFor
    printf,1,""
EndFor
close,1
Endif Else Begin
    print,"Skipping Output  (use /output to produce output files)"
EndElse

counter=long(0)

;Compute the step values based on pixel precision.  These should now be correct
A0increase=precision
A1increase=((1.0)/((subsize[1])*1.0)*precision)
A2increase=((1.0)/((subsize[1]^2)*1.0)*precision)
B0increase=precision
B1increase=((1.0)/((subsize[1])*1.0)*precision)
B2increase=((1.0)/((subsize[1]^2)*1.0)*precision)

;Create a sequence of block counts and find the movement of each of those points using
;Matt's trick.  Then reference the diffs of those.  Loops stay unchanged.

time2=systime(1)
FOR A0=-MaxA0,MaxA0,A0increase DO Begin
    FOR B0=-MaxB0,MaxB0,B0increase DO Begin
       FOR A1=-MaxA1,MaxA1,A1increase DO Begin
         FOR B1=1-MaxB1,1+MaxB1,B1increase DO Begin
            xarr_1=A1*((yarrgen)*blocksize)+A0;+totaldriftA
            yarr_1=(B1-1)*((yarrgen)*blocksize)+B0;+totaldriftB
            FOR A2=-MaxA2,MaxA2,A2increase DO Begin
                FOR B2=-MaxB2,MaxB2,B2increase DO Begin
                 xarr=round(xarr_1 + A2*((yarrgen)*blocksize)*((yarrgen)*blocksize)+totaldriftA)
                 yarr=round(yarr_1 + B2*((yarrgen)*blocksize)*((yarrgen)*blocksize)+totaldriftB)
                 ind=yarrgen + yarrgensize[0]*xarr + yarrgensize[0]*Apoints*yarr
                 perarea=yarrgen + yarrgensize[0]*yarr
                 totalsum=total(diffs[ind],/integer)/(total(areas[perarea],/integer)*1.0)
                 counter=counter+1
;				if (abs(A1-.0195313) lt .0001 and A0 eq 0 and B0 eq 0 and A2 lt .000001 and B2 lt .00001 and B1 eq 1) then begin
;                	if (testind eq 1) then begin
;						tempind=ind
;						temploc=Array_Indices(diffs,tempind)
;						Print, A0,A1,A2,B0,B1,B2
;					For block=0,numblocks-1,1 do begin
;						;Print, temploc[0,block],temploc[1,block]-totaldriftA,temploc[2,block]-totaldriftB,"              unmodded:",temploc[1,block],temploc[2,block]
;						Print, temploc[0,block],temploc[1,block]-totaldriftA,"              unmodded:",temploc[1,block]
;					;	print,"avast! It's the rogue penguin!"
;					EndFor
;					print, totalsum
;					print,""
;;					EndFor
;				endif
;				endif
;                    print,A0,A1,A2,B0,B1,B2
;                    print,xarr,yarr,ind,totalsum,perarea
;                    print,""
;          if ((A0 eq 0 and A1 eq 0 and ((A2 - .0000305176) lt .001) and B0 eq 0 and B1 eq 1 and B2 eq 0)) then begin
;            print, "help"
;          endif
                 if (totalsum lt bestdiff) Then Begin
                    bestdiff=totalsum
                    bestdiffA0=A0
                    bestdiffA1=A1
                    bestdiffA2=A2
                    bestdiffB0=B0
                    bestdiffB1=B1
                    bestdiffB2=B2
                    if (totalsum lt 0) then BEGIN
                     print, "booooooooooo we have a negative being saved here!"
                    endif
                 endif
             EndFor
            EndFor
         EndFor
       EndFor
    EndFor
EndFor



time2=systime(1)-time2
print, "Time for setup and completion of stage 2",time2


print, "AVAST YE SCURVY DOG"

Print, 'Coefficients of drift were'
print,"Totaldiff",bestdiff
print,"A0   ",bestdiffA0
print,"B0   ",bestdiffB0
print,"A1   ",bestdiffA1,bestdiffA1*imagesize[1]
print,"B1   ",bestdiffB1,bestdiffB1*imagesize[1]
print,"A2   ",bestdiffA2,bestdiffA2*imagesize[1]*imagesize[1]
print,"B2   ",bestdiffB2,bestdiffB2*imagesize[1]*imagesize[1]
		;spawn_inverts(A0,A1,A2,0,B0,B1,B2,0)
		;IDL Function run internally
		;spawn_inverts

;				Spawn Inverse SETUP
		b0=bestdiffB0
		b1=bestdiffB1
		b2=bestdiffB2
		b3=0.0
		a0=bestdiffA0
		a1=bestdiffA1
		a2=bestdiffA2
		a3=0.0
				NewB0=-(b0/b1) - (b0^2*b2)/b1^3 - (2*b0^3*b2^2)/b1^5 + (b0^3*b3)/b1^4
				NewB1=1/b1 + (2*b0*b2)/b1^3 + (6*b0^2*b2^2)/b1^5 - (3*b0^2*b3)/b1^4
				NewB2=-(b2/b1^3) - (6*b0*b2^2)/b1^5 + (3*b0*b3)/b1^4
				NewB3=(2*b2^2)/b1^5 - b3/b1^4
				NewA0=-a0 + (a3*b0^3)/b1^3 - (a2*b0^2)/b1^2 + (a1*b0)/b1 + (3*a3*b0^4*b2)/b1^5 - (2*a2*b0^3*b2)/b1^4 + (a1*b0^2*b2)/b1^3 + (9*a3*b0^5*b2^2)/b1^7 -$
    				  (5*a2*b0^4*b2^2)/b1^6 + (2*a1*b0^3*b2^2)/b1^5 + (13*a3*b0^6*b2^3)/b1^9 - (4*a2*b0^5*b2^3)/b1^8 + (18*a3*b0^7*b2^4)/b1^11 - $
     				  (4*a2*b0^6*b2^4)/b1^10 + (12*a3*b0^8*b2^5)/b1^13 + (8*a3*b0^9*b2^6)/b1^15 - (3*a3*b0^5*b3)/b1^6 + (2*a2*b0^4*b3)/b1^5 - (a1*b0^3*b3)/b1^4 -$
     				  (6*a3*b0^6*b2*b3)/b1^8 + (2*a2*b0^5*b2*b3)/b1^7 - (15*a3*b0^7*b2^2*b3)/b1^10 + (4*a2*b0^6*b2^2*b3)/b1^9 - (12*a3*b0^8*b2^3*b3)/b1^12 - $
     				  (12*a3*b0^9*b2^4*b3)/b1^14 + (3*a3*b0^7*b3^2)/b1^9 - (a2*b0^6*b3^2)/b1^8 + (3*a3*b0^8*b2*b3^2)/b1^11 + (6*a3*b0^9*b2^2*b3^2)/b1^13 - $
     				  (a3*b0^9*b3^3)/b1^12
				NewA1=(-3*a3*b0^2)/b1^3 + (2*a2*b0)/b1^2 - a1/b1 - (12*a3*b0^3*b2)/b1^5 + (6*a2*b0^2*b2)/b1^4 - (2*a1*b0*b2)/b1^3 - (45*a3*b0^4*b2^2)/b1^7 +$
     				  (20*a2*b0^3*b2^2)/b1^6 - (6*a1*b0^2*b2^2)/b1^5 - (78*a3*b0^5*b2^3)/b1^9 + (20*a2*b0^4*b2^3)/b1^8 - (126*a3*b0^6*b2^4)/b1^11 + $
     				  (24*a2*b0^5*b2^4)/b1^10 - (96*a3*b0^7*b2^5)/b1^13 - (72*a3*b0^8*b2^6)/b1^15 + (15*a3*b0^4*b3)/b1^6 - (8*a2*b0^3*b3)/b1^5 + $
     				  (3*a1*b0^2*b3)/b1^4 + (36*a3*b0^5*b2*b3)/b1^8 - (10*a2*b0^4*b2*b3)/b1^7 + (105*a3*b0^6*b2^2*b3)/b1^10 - (24*a2*b0^5*b2^2*b3)/b1^9 +$
     				  (96*a3*b0^7*b2^3*b3)/b1^12 + (108*a3*b0^8*b2^4*b3)/b1^14 - (21*a3*b0^6*b3^2)/b1^9 + (6*a2*b0^5*b3^2)/b1^8 - (24*a3*b0^7*b2*b3^2)/b1^11 -$
     				  (54*a3*b0^8*b2^2*b3^2)/b1^13 + (9*a3*b0^8*b3^3)/b1^12
				NewA2=(3*a3*b0)/b1^3 - a2/b1^2 + (18*a3*b0^2*b2)/b1^5 - (6*a2*b0*b2)/b1^4 + (a1*b2)/b1^3 + (90*a3*b0^3*b2^2)/b1^7 - (30*a2*b0^2*b2^2)/b1^6 +$
     				  (6*a1*b0*b2^2)/b1^5 + (195*a3*b0^4*b2^3)/b1^9 - (40*a2*b0^3*b2^3)/b1^8 + (378*a3*b0^5*b2^4)/b1^11 - (60*a2*b0^4*b2^4)/b1^10 + $
     				  (336*a3*b0^6*b2^5)/b1^13 + (288*a3*b0^7*b2^6)/b1^15 - (30*a3*b0^3*b3)/b1^6 + (12*a2*b0^2*b3)/b1^5 - (3*a1*b0*b3)/b1^4 - $
     				  (90*a3*b0^4*b2*b3)/b1^8 + (20*a2*b0^3*b2*b3)/b1^7 - (315*a3*b0^5*b2^2*b3)/b1^10 + (60*a2*b0^4*b2^2*b3)/b1^9 - (336*a3*b0^6*b2^3*b3)/b1^12 -$
     				  (432*a3*b0^7*b2^4*b3)/b1^14 + (63*a3*b0^5*b3^2)/b1^9 - (15*a2*b0^4*b3^2)/b1^8 + (84*a3*b0^6*b2*b3^2)/b1^11 + (216*a3*b0^7*b2^2*b3^2)/b1^13 -$
     				  (36*a3*b0^7*b3^3)/b1^12
				NewA3=-(a3/b1^3) - (12*a3*b0*b2)/b1^5 + (2*a2*b2)/b1^4 - (90*a3*b0^2*b2^2)/b1^7 + (20*a2*b0*b2^2)/b1^6 - (2*a1*b2^2)/b1^5 - (260*a3*b0^3*b2^3)/b1^9 +$
     				  (40*a2*b0^2*b2^3)/b1^8 - (630*a3*b0^4*b2^4)/b1^11 + (80*a2*b0^3*b2^4)/b1^10 - (672*a3*b0^5*b2^5)/b1^13 - (672*a3*b0^6*b2^6)/b1^15 + $
     				  (30*a3*b0^2*b3)/b1^6 - (8*a2*b0*b3)/b1^5 + (a1*b3)/b1^4 + (120*a3*b0^3*b2*b3)/b1^8 - (20*a2*b0^2*b2*b3)/b1^7 + (525*a3*b0^4*b2^2*b3)/b1^10 -$
     				  (80*a2*b0^3*b2^2*b3)/b1^9 + (672*a3*b0^5*b2^3*b3)/b1^12 + (1008*a3*b0^6*b2^4*b3)/b1^14 - (105*a3*b0^4*b3^2)/b1^9 + (20*a2*b0^3*b3^2)/b1^8 -$
     				  (168*a3*b0^5*b2*b3^2)/b1^11 - (504*a3*b0^6*b2^2*b3^2)/b1^13 + (84*a3*b0^6*b3^3)/b1^12
print,''
print,'Inverse coefficients are:'
print,"A0   ",NewA0
print,"B0   ",NewB0
print,"A1   ",NewA1,NewA1*imagesize[1]
print,"B1   ",NewB1,NewB1*imagesize[1]
print,"A2   ",NewA2,NewA2*imagesize[1]*imagesize[1]
print,"B2   ",NewB2,NewB2*imagesize[1]*imagesize[1]
print,"A3   ",NewA3,NewA3*imagesize[1]*imagesize[1]*imagesize[1]
print,"B3   ",NewB3,NewB3*imagesize[1]*imagesize[1]*imagesize[1]
stringarr=STRSPLIT(Basefile,'.',/extract)
Fixedimage = POLY_2D(Baseimage,[[NewA0, NewA1,NewA2,NewA3],[1,0,0,0],[0,0,0,0],[0,0,0,0]],[[NewB0,NewB1,NewB2,NewB3],[0,0,0,0],[0,0,0,0],[0,0,0,0]],2,CUBIC=-.5)
Write_Tiff,strjoin(stringarr,"_result."),reverse(Fixedimage,2)

tv,fixedimage
print,'total time:',systime(1)-time0,'  elapsed seconds'
print,"PHASE 2.  loop cycles ", counter

if (keyword_set(output)) Then Begin
openw,2,stringarr[0]+"_diffdata.txt"
;formatstring="("+string(diffsize[2])+"I)"
;formatstring="("+string(diffsize[])+"I)"
;blockformattemp=strtrim(""+string(diffsize[0])+"I)")
;blockformatstring='("Block: "'+","+strtrim(""+string(diffsize[0])+"I)")
;blockformattwo=
A0=BestdiffA0
A1=BestdiffA1
;A1=-0.0195313
A2=BestdiffA2
B0=BestdiffB0
B1=BestdiffB1
B2=BestdiffB2
xarr_1=A1*((yarrgen)*blocksize)+A0;+totaldriftA
yarr_1=(B1-1)*((yarrgen)*blocksize)+B0;+totaldriftB
xarr=round(xarr_1 + A2*((yarrgen)*blocksize)*((yarrgen)*blocksize)+totaldriftA)
yarr=round(yarr_1 + B2*((yarrgen)*blocksize)*((yarrgen)*blocksize)+totaldriftB)
ind=yarrgen + yarrgensize[0]*xarr + yarrgensize[0]*Apoints*yarr
perarea=yarrgen + yarrgensize[0]*yarr
totalsum=total(diffs[ind],/integer)/(total(areas[perarea],/integer)*1.0)
print, diffs[ind]
;xgen=indgen(
For block=0,numblocks-1,1 do begin
	;diffstringformat=
    ;printf,1,format=blockformatstring,string(""+Block+"("+string((block+1)*blocksize-1<(smaxy+j*(j lt 0)))+"-"+string(((block)*blocksize>(j*(j gt 0))))+")")     ;Only works on square images!
    printf,2,block
    ;For X=0,diffsize[1]-1,1 do begin
       ;printf,1,format=formatstring,diffs[block,X,*]
       printf,2,format=formatstring,diffs[ind[block]]
       printf,2,array_indices(diffs,ind[block])-totaldriftA
       printf,2," "
    ;EndFor
    printf,2,""
EndFor
close,2
Endif Else Begin
    print,"Skipping Output  (use /output to produce output files)"
EndElse

print, "------End of Program------"


 print, potatoes

END