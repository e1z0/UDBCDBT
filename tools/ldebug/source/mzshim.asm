
%if 0

"Big .COM" executable MZ header shim
 by C. Masloch, 2019

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

%include "lmacros2.mac"

	defaulting

	strdef FILE, ""
%ifidn _FILE,""
 %fatal Has to specify a file!
%endif

	numdef IMAGE_EXE,	0
	numdef IMAGE_EXE_CS,	-16	; relative-segment for CS
	numdef IMAGE_EXE_IP,	256 +64	; value for IP
		; The next two are only used if _IMAGE_EXE_AUTO_STACK is 0.
	numdef IMAGE_EXE_SS,	-16	; relative-segment for SS
	numdef IMAGE_EXE_SP,	0FFFEh	; value for SP (0 underflows)
	numdef IMAGE_EXE_AUTO_STACK,	2048, 2048
					; allocate stack behind image
	numdef IMAGE_EXE_MIN,	65536	; how much to allocate for the process
%ifndef _IMAGE_EXE_MIN_CALC
 %define _IMAGE_EXE_MIN_CALC	\
		(((_IMAGE_EXE_MIN \
		- (payload.actual_end - payload) \
		- 256 \
		+ _IMAGE_EXE_AUTO_STACK) + 15) & ~15)
%endif
	numdef IMAGE_EXE_MAX, -1


	org 0
start:
	db "MZ"		; exeSignature
	dw (payload.end - $$) % 512		; exeExtraBytes

		; Carries a .COM-like executable.
		; Note: With _IMAGE_EXE_AUTO_STACK, the
		;	 stack segment will be behind the image.
	dw (payload.end - $$ + 511) / 512	; exePages
	dw 0		; exeRelocItems
	dw (payload -$$+0) >> 4	; exeHeaderSize
	dw (_IMAGE_EXE_MIN_CALC + 15) >> 4	; exeMinAlloc
%if _IMAGE_EXE_MAX
	dw _IMAGE_EXE_MAX	; exeMaxAlloc
%else
	dw (_IMAGE_EXE_MIN_CALC + 15) >> 4	; exeMaxAlloc
%endif
%if _IMAGE_EXE_AUTO_STACK
	dw ((payload.actual_end - payload) \
		+ _IMAGE_EXE_MIN_CALC \
		- _IMAGE_EXE_AUTO_STACK + 15) >> 4	; exeInitSS
		; ss: payload size minus 512 (conservative, assume DOS
		;  treats bogus exeExtraBytes as below 512 bytes.)
		; + exeMinAlloc
		; - auto stack size
	dw _IMAGE_EXE_AUTO_STACK		; exeInitSP
		; sp = auto stack size (eg 800h)
%else
	dw _IMAGE_EXE_SS	; exeInitSS
	dw _IMAGE_EXE_SP	; exeInitSP
%endif
	dw 0		; exeChecksum
	dw _IMAGE_EXE_IP, _IMAGE_EXE_CS	; exeInitCSIP
	dw 0		; exeRelocTable

	align 16, db 38
payload:
	incbin _FILE
	align 16, db 38
.actual_end:
.end:
