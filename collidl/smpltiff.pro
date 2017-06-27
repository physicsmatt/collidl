pro smpltiff,fs,data
n=size(data) & n=n(1:n(0))
openw,u,fs,/get_lun
; Fixed header:
w='4949'XU & writeu,u,w
w='002A'XU & writeu,u,w
writeu,u,8L
; IFD - image file directory:
writeu,u,13US    ; # of fields
writeu,u,'00FE'XU,4S,1L,0L         ;NewSubFileType
writeu,u,'0100'XU,3S,1L,long(n(0)) ;ImageWidth
writeu,u,'0101'XU,3S,1L,long(n(1)) ;ImageLength
writeu,u,'0102'XU,3S,1L,8L         ;BitsPerSample
writeu,u,'0103'XU,3S,1L,1L         ;Compression
writeu,u,'0106'XU,3S,1L,1L         ;PhotometricInterpretation - blackiszero (1)
writeu,u,'0111'XU,4S,1L,186L       ;StripOffsets
writeu,u,'0115'XU,3S,1L,1L         ;SamplesPerPixel
writeu,u,'0116'XU,3S,1L,long(n(1)) ;RowsPerStrip
writeu,u,'0117'XU,4S,1L,long(n(0)*n(1));StripByteCounts
writeu,u,'011A'XU,5S,1L,170L       ;XResolution
writeu,u,'011B'XU,5S,1L,178L       ;YResolution
writeu,u,'0128'XU,3S,1L,1L         ;ResolutionUnit - no absolute unit (1S)
writeu,u,0L
; Resolution longints (at 170 and 178):
writeu,u,1L,1L,1L,1L
; Strip with data (at 186):
writeu,u,byte(data)
free_lun,u
end

print,'smpltiff,fs,data  ; Writes a very basic gray scale TIFF file.'
print,'                  ; data should be a twodimensional byte array.'
end
