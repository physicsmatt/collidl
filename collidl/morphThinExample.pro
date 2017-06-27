FUNCTION morphThinExample,img

; Prepare the display device and load grayscale color table.
DEVICE, DECOMPOSED = 0, RETAIN = 2
LOADCT, 0

if not keyword_set(img) then begin
; Load an image.
file = FILEPATH('pollens.jpg', $
  SUBDIRECTORY=['examples','demo','demodata'])
READ_JPEG, file, img, /GRAYSCALE
endif

; Get the image size, prepare a display window and
; display the image.
dims = SIZE(img, /DIMENSIONS)
;WINDOW, 0, XSIZE=2*dims[0], YSIZE=2*dims[1], $
 ; TITLE='Original, Binary and Thinned Images'
;TVSCL, img, 0

; Generate a thresholded binary image.
binaryImg = img GE 128B
help, binaryimg
;TVSCL, binaryImg, 1

; Prepare hit and miss structures for thinning.
    h0 = [[0b,0,0], [0,1,0], [1,1,1]]

    m0 = [[1b,1,1], [0,0,0], [0,0,0]]

    h1 = [[0b,0,0], [1,1,0], [1,1,0]]

    m1 = [[0b,1,1], [0,0,1], [0,0,0]]

    h2 = [[1b,0,0], [1,1,0], [1,0,0]]

    m2 = [[0b,0,1], [0,0,1], [0,0,1]]

    h3 = [[1b,1,0], [1,1,0], [0,0,0]]

    m3 = [[0b,0,0], [0,0,1], [0,1,1]]

    h4 = [[1b,1,1], [0,1,0], [0,0,0]]

    m4 = [[0b,0,0], [0,0,0], [1,1,1]]

    h5 = [[0b,1,1], [0,1,1], [0,0,0]]

    m5 = [[0b,0,0], [1,0,0], [1,1,0]]

    h6 = [[0b,0,1], [0,1,1], [0,0,1]]

    m6 = [[1b,0,0], [1,0,0], [1,0,0]]

    h7 = [[0b,0,0], [0,1,1], [0,1,1]]

    m7 = [[1b,1,0], [1,0,0], [0,0,0]]

; Iterate until the thinned image is identical to
; the input image for a given iteration.
bCont = 1b
iIter = 1
thinImg = binaryImg

WHILE bCont eq 1b do begin
  PRINT,'Iteration: ', iIter
    inputImg = thinImg

    ; Perform the thinning using the first pair
    ;  of structure elements.
    thinImg = MORPH_THIN(inputImg, h0, m0)

    ; Perform the thinning operation using the
    ; remaining structural element pairs.
    thinImg = MORPH_THIN(thinImg, h1, m1)
    thinImg = MORPH_THIN(thinImg, h2, m2)
    thinImg = MORPH_THIN(thinImg, h3, m3)
    thinImg = MORPH_THIN(thinImg, h4, m4)
    thinImg = MORPH_THIN(thinImg, h5, m5)
    thinImg = MORPH_THIN(thinImg, h6, m6)
    thinImg = MORPH_THIN(thinImg, h7, m7)

  ; Display the results of thinning and wait a second for
    ; display purposes.
    ;TVSCL, thinImg, 2
;    WAIT, 1

  ; Test the condition and increment the loop.
    bCont = MAX(inputImg - thinImg)
    iIter = iIter + 1

ENDWHILE

; Show inverse of final result.
;TVSCL, 1 - thinImg, 3

RETURN,thinImg

END