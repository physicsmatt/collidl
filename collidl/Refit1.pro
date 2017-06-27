;  September 1, 2001
;  Takes a list of filenames which refer to correlation functions
;  Analyzes these correlation functions by fitting a few different
;  functional forms.  Spits out parameters in a *summary.dat file.
;
;



;********************************************************
;USED FOR FITTING THE POWER LAW DECAY WITH ONE PARAMETER
;********************************************************
PRO One_Parameter_Power_Law, X, A, F, PDER
 
  F = (X^(-A[0]))
;If the function is called with four parameters,
;calculate the partial derivatives:
  IF N_PARAMS() GE 4 THEN BEGIN
;PDER's column dimension is equal to the number of elements
;in xi and its row dimension is equal to the number of
;parameters in the function F:
    pder = FLTARR(N_ELEMENTS(X), 1)
;Compute the partial derivatives with respect to a0 and
;place in the first row of PDER.
;alog is natural logarithm
    pder[*, 0] =  - (X^(-A[0])) *alog(x)
  ENDIF
 


  END
;********************************************************
;USED FOR FITTING THE POWER LAW DECAY WITH TWO PARAMETERS
;********************************************************
PRO Two_Parameter_Power_Law, X, A, F, PDER
 
  F = A[0] * (X^(-A[1]))
;If the function is called with four parameters,
;calculate the partial derivatives:
 

  IF N_PARAMS() GE 4 THEN BEGIN
;PDER's column dimension is equal to the number of elements
;in xi and its row dimension is equal to the number of
;parameters in the function F:
    pder = FLTARR(N_ELEMENTS(X), 2)
;Compute the partial derivatives with respect to a0 and
;place in the first row of PDER.
   pder[*, 0] =  (X^(-A[1]))
;Compute the partial derivatives with respect to a1 and
;place in the second row of PDER.
;alog is natural logarithm
    pder[*, 1] =  - (A[0])*(X^(-A[1])) * (alog(X))
  ENDIF
END
 
;********************************************************
;USED FOR FITTING THE EXPONENTIAL DECAY WITH ONE PARAMETER
;********************************************************
PRO One_Parameter_Exponential, X, A, F, PDER
 
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
 
;********************************************************
;USED FOR FITTING THE EXPONENTIAL DECAY WITH TWO PARAMETERS
;********************************************************
PRO Two_Parameter_Exponential, X, A, F, PDER
 
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
;*************************************************************
;*************************************************************
;WE START THE PROGRAM HERE
;*************************************************************
;*************************************************************
pro main,data
 
;Here we set the constants used in the program downstream
;*************************************************************

RepeatSpacing=4
ChosenFunction='One_Parameter_Exponential'
 
print, 'Repeat spacing specified as ', RepeatSpacing,' pixels.'
 

; One_Parameter_Power_Law means two parameter power law
; Two_Parameter_Power_Law means one parameter power law
; One_Parameter_Exponential means one parameter exponential
; Two_Parameter_Exponential means two parameter exponential
;*************************************************************
 
	fs=dialog_pickfile(get_path=ps,/multiple_files)
 

		if strmid(strlowcase(fs(0)),strlen(fs(0))-3,3) ne 'dat' then readlist,fs(0),fs,path=ps else fs=[fs]
		Summary_of_Data=fltarr(10, n_elements(fs))
 
		for FileNumber=0,n_elements(fs)-1 do begin
			print,'Now working on : ',fs(FileNumber)
 

			FileName=fs(FileNumber)
			data=read_ascii(FileName, count=count)
			mfa=data.field1
			help,mfa
			dim=size(mfa)			
			dimx=dim[2]
			dimy=dimx
			

;note that the # of x pixels is xs+1, # of y pixels is ys+1
 


 

;next we perform a curvefit,
 


 

if ((ChosenFunction eq 'One_Parameter_Power_Law') OR (ChosenFunction eq 'Two_Parameter_Power_Law'))  then begin
		Y=fltarr(DimY-RepeatSpacing-150)
		Y(*)=mfa(1,RepeatSpacing:DimY-1-150)
 
		X=fltarr(DimY-RepeatSpacing-150)
		X(*)=mfa(0,RepeatSpacing:DimY-1-150)/RepeatSpacing
 

		W=fltarr(DimY-RepeatSpacing-150)
		W(*)=mfa(2,RepeatSpacing:DimY-1-150)
		W(*)=1
		window,1,xsize=DimY,ysize=200
		plot,X,Y,title='log plots', /XLOG, /YLOG, xtitle='rpt. spaci.', ytitle='g(r)',psym=0
 
ENDIF
 
if ((ChosenFunction eq 'One_Parameter_Exponential') OR (ChosenFunction eq 'Two_Parameter_Exponential'))  then begin
		Y=fltarr(DimY)
		Y(*)=mfa(1,*)
 
		X=fltarr(DimY)
		X(*)=mfa(0,*)
 

		W=fltarr(DimY)
		W(*)=mfa(2,*)
		W(*)=1
 
		window,1,xsize=DimY,ysize=200
		plot,X,Y,title='lin plots', xtitle='rpt. spaci.', ytitle='g(r)',psym=0
 

ENDIF
 


 

		;below to weight the points of smaller distance
	;	for Dan=0,DimY-1 do begin
 
	;	W(Dan)=W(Dan)/(Dan+1)
 
	;	endfor
 

CorrelationLenghTest=abs(Y-exp(-1.0)) ; an array
CorrelationLengthSeed=min(CorrelationLenghTest,Min_Subscript)
print,'The correlation length seed was ', Min_Subscript, ' .'
 

if ChosenFunction eq 'Two_Parameter_Exponential' then A=[1,Min_Subscript]
if ChosenFunction eq 'One_Parameter_Exponential' then A=[Min_Subscript]
if ChosenFunction eq 'Two_Parameter_Power_Law' then A=[1,0.25]
if ChosenFunction eq 'One_Parameter_Power_Law' then A=[0.25]
 


 
;OK, now call it
NumberofIterateractionsPerformed=1
yfit = CURVEFIT(X, Y,W, TOL=0.001, A,SIGMA_A,FUNCTION_NAME = ChosenFunction)
print,'Performed ',NumberofIterateractionsPerformed,' iterations.'
;plot out the determined function in a diffference color
 

if ChosenFunction eq 'Two_Parameter_Exponential' then F=(A[0])*exp(-X/A[1])
if ChosenFunction eq 'One_Parameter_Exponential' then F=exp(-X/A[0])
if ChosenFunction eq 'Two_Parameter_Power_Law' then F = (A[0]*X^(-A[1]))
if ChosenFunction eq 'One_Parameter_Power_Law' then F = ((X)^(-A[0]))
 


 


 
oplot,X, F, psym=0, color=254
;blue equals 256*127
 

if ChosenFunction eq 'Two_Parameter_Exponential' then 	print,'The coefficient was ', A[0],' and the exponent was ',A[1],'.'
if ChosenFunction eq 'One_Parameter_Exponential' then print, 'The correlation length was ', A[0]
if ChosenFunction eq 'Two_Parameter_Power_Law' then print,'The coefficient was ', A[0],' and the exponent was ',A[1],'.'
if ChosenFunction eq 'One_Parameter_Power_Law' then print,'The exponent  was ', A[0]
 


 

	;record the data in the array for summary
	;Summary_of_Data[0,FileNumber]=A[1]
	;Summary_of_Data[1,FileNumber]=A[0]
 
	Summary_of_Data[0,FileNumber]=A[0]
	Summary_of_Data[1,FileNumber]=A[0]
 


 


 
print, 'All done with ',fs[FileNumber],'.'
 


if (1 eq 0 ) then begin
 
;WDELETE,1
 
endif
 


;bbb=''
;read,bbb
 


 
		endfor ;THIS END FOR IS THE MASTER LOOP FOR READING ALL THE FILES
;here we want to output the Summary_of_Data information.
 

openw,u,strmid(fs[0],0,strlen(fs[0])-4)+'refitsummary.dat',/get_lun
for LongIndex=0L, n_elements(fs)-1 do begin
 printf,u,Summary_of_Data[0,LongIndex],'     ',Summary_of_Data[1,LongIndex],'     ',fs[LongIndex]
endfor
free_lun,u
close,u
 

print,'All done with every file.'
print,'The summary file is ', strmid(fs[0],0,strlen(fs[0])-4)+'refitsummary.dat'
 wait,3

end
 
