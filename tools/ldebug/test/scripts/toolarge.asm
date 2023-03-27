
%if 0

Invalid MZ executable
 by C. Masloch, 2022

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

	org 0
start:
	db "MZ"		; exeSignature
	dw 0		; exeExtraBytes
	dw (1024 * 1024 - 1) / 512	; exePages
	dw 0		; exeRelocItems
	dw 0		; exeHeaderSize
	dw 0		; exeMinAlloc
	dw 0		; exeMaxAlloc
	dw -1		; exeInitSS
	dw -1		; exeInitSP
	dw 0		; exeChecksum
	dw -1, -1	; exeInitCSIP
	dw 0		; exeRelocTable
	times 32 - ($ - start) db 0
