
%if 0

Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

;--- defines procedures
;--- PowerOf10
;--- FloatToBCD
;--- FloatToStr

; These are bits in the FP status word.

FP_LESSTHAN	equ 01h
FP_EQUALTO	equ 40h

	align 8, db 0
ten:	dq 10.0
ten16:	dq 1.0e16
;rounder:dq 5.0e10

ten_1:	dt 1.0e1		; 10.0
	dt 1.0e2		; 100.0
	dt 1.0e3		; 1,000.0
	dt 1.0e4		; 10,000.0
	dt 1.0e5		; 100,000.0
	dt 1.0e6		; 1,000,000.0
	dt 1.0e7		; 10,000,000.0
	dt 1.0e8		; 100,000,000.0
	dt 1.0e9		; 1,000,000,000.0
	dt 1.0e10		; 10,000,000,000.0
	dt 1.0e11		; 100,000,000,000.0
	dt 1.0e12		; 1,000,000,000,000.0
	dt 1.0e13		; 10,000,000,000,000.0
	dt 1.0e14		; 100,000,000,000,000.0
	dt 1.0e15		; 1,000,000,000,000,000.0

ten_16:	dt 1.0e16
	dt 1.0e32
	dt 1.0e48
	dt 1.0e64
	dt 1.0e80
	dt 1.0e96
	dt 1.0e112
	dt 1.0e128
	dt 1.0e144
	dt 1.0e160
	dt 1.0e176
	dt 1.0e192
	dt 1.0e208
	dt 1.0e224
	dt 1.0e240

ten_256:dt 1.0e256

; The remaining exponents are only necessary for 10-byte doubles.

	dt 1.0e512
	dt 1.0e768
	dt 1.0e1024
	dt 1.0e1280
	dt 1.0e1536
	dt 1.0e1792
	dt 1.0e2048
	dt 1.0e2304
	dt 1.0e2560
	dt 1.0e2816
	dt 1.0e3072
	dt 1.0e3328
	dt 1.0e3584
	dt 1.0e4096
	dt 1.0e4352
	dt 1.0e4608
	dt 1.0e4864

%if _DUALCODE
	usesection lDEBUG_CODE2
%else
	usesection lDEBUG_CODE
%endif

		; Divide or multiply st0 to normalize it
		;
		; INP:	ax = exponent word
		; CHG:	bx, dx, cl
PowerOf10:
	push si
	push ax
	test ax, ax
	jns .notnegative
	neg ax
.notnegative:
	fld1
	mov bl, al
	and bl, 0Fh		; bits 0..3
	je .0..3zero
	mov si, ten_1
	call mul10
.0..3zero:
	mov bl, al
	mov cl, 4
	shr bl, cl
	and bl, 0Fh		; bits 4..7
	je .4..7zero
	mov si, ten_16
	call mul10
.4..7zero:
	mov bl, ah
	and bl, 1Fh		; bits 8..12
	jz .8..12zero
	mov si, ten_256
	call mul10
.8..12zero:
	pop ax
	test ax, ax
	jns .notnegative2
	fdivp st1
	pop si
	retn

.notnegative2:
	fmulp st1
	pop si
	retn

mul10:
	dec bl
	mov bh, 0
	push ax
	mov ax, bx
	add ax, ax
	add ax, ax		; *4
	add bx, ax		; *5
	add bx, bx		; *10
	pop ax
	fld tword [bx + si]
	fmulp st1
	retn


		; Convert a floating point register to ASCII. For internal use.
		; The result always has exactly 18 digits, with zero padding
		; on the left if required.
		;
		; INP:	st0 = number to convert, 0 <= st0 < 1.0E19
		;	di-> 18-character output buffer
		; CHG:	si, di, cx, ax
FloatToBCD:
	push bp
	mov bp, sp
	sub sp, 12

		; The fbstp instruction converts the top of the stack to
		; a packed BCD form in ten bytes, with two digits per
		; byte. The top byte has the sign, which we ignore.
	fbstp [ bp-12 ]

		; Now we need to unpack the BCD to ASCII.
	lea si, [ bp-4 ]
	mov cx, 9
.nextdigits:
	mov al, byte [ si ]	; xxxx xxxx AAAA BBBB
	dec si
%if 1
	mov ah, 0		; 0000 0000 AAAA BBBB
	push cx
	mov cl, 4
	ror ax, cl		; BBBB xxxx xxxx AAAA
	rol ah, cl		; xxxx BBBB xxxx AAAA
	pop cx
	;and ax, 0F0Fh		; 0000 BBBB 0000 AAAA
%else
	aam 16			; 0000 AAAA 0000 BBBB
	xchg al, ah		; 0000 BBBB 0000 AAAA
%endif
	add ax, "00"
	stosw
	loop .nextdigits
	mov sp, bp
	pop bp
	retn


; Convert a double precision number to a string.
;
; Entry:	dword [far pfpin] -> 8-byte double to convert, non-zero
;		es = ss : word [pszDbl] -> character buffer
;
; Exit:		es = ss : word [pszDbl] -> converted value
;
; CHG:		ax, bx, cx, dx
;
; The buffer at pszDbl should be at least 19 bytes long.
; The buffer needs to be initialized with blanks.

;FloatToStr PROC stdcall public USES si di pfpin: ptr TBYTE, pszDbl: PTR BYTE
FloatToStr: section_of_function
	lframe dualdistance
	lpar word, ??pszDbl	; pszDbl: PTR BYTE, near
	lpar dword, ??pfpin	; pfpin: ptr TBYTE, but far
	lvar word, ??iExp	; LOCAL iExp: WORD
	lvar word, ??mystat	; LOCAL mystat: WORD
	lvar 10, ??fpin		; LOCAL fpin: TBYTE
	lvar 22, ??szTemp	; LOCAL szTemp[22]: BYTE
	lenter

%define iExp	bp+???%+%?
%define mystat	bp+???%+%?
%define fpin	bp+???%+%?
%define szTemp	bp+???%+%?
%define pfpin	bp+???%+%?
%define pszDbl	bp+???%+%?

	push ds
	push si			; USES si
	push es
	push di			; USES di

; Special case zero has been filtered already. (fxtract fails for zero.)
	lds si, [pfpin]
	 push ss
	 pop es
	lea di, [fpin]
	mov cx, 5
	rep movsw		; store number in local buffer
	 push ss
	 pop ds

	mov di, [pszDbl]	; -> output buffer

; Check for a negative number.
	test byte [fpin+9], 80h
	jz .ispositive
	and byte [fpin+9], ~80h	; change to positive
	mov al, '-'        	; store a minus sign
	stosb
.ispositive:

; Load our value onto the stack two times.
	fld tword [fpin]
	fld st0

; Compute the closest power of 10 below the number.  We can't get an
; exact value because of rounding.  We could get close by adding in
; log10(mantissa), but it still wouldn't be exact.  Since we'll have to
; check the result anyway, it's silly to waste cycles worrying about
; the mantissa.
;
; The exponent is basically log2(fpin).  Those of you who remember
; algebra realize that log2(fpin) x log10(2) = log10(fpin), which is
; what we want.

	fxtract			; ST = mantissa, exponent, fpin
	fstp st0		; discard the mantissa
	fldlg2			; push log10(2)
	fmulp st1, st0		; ST = log10(fpin), fpin
	fistp word [iExp]	; ST = fpin

; An 8-byte double can carry almost 16 digits of precision.  Actually, it's
; 15.9 digits, so some numbers close to 1E17 will be wrong in the bottom
; digit. If this is a concern, change the 18 or 16 to a 15.
;
; A 10-byte double can carry almost 19 digits, but fbstp only stores the
; guaranteed 18. Since we're doing 10-byte doubles, we check for 18 instead of 16.

	cmp word [iExp], 18
	jae .notbelow18
	fld st0			; ST = fpin, fpin
	frndint			; ST = int(fpin), fpin
	fcomp st1		; ST = fpin, status set
	fstsw word [mystat]
	mov ax, word [mystat]
	sahf
	jne .notequal

; We have an integer!  Lucky day.  Go convert it into a temp buffer.

	push di
	lea di, [szTemp]
	call FloatToBCD
	pop di

	mov ax, 16+1
	mov cx, word [iExp]
	sub ax, cx
	inc cx
	lea si, [szTemp]
	add si, ax

; The off-by-one order of magnitude problem below can hit us here.
; We just trim off the possible leading zero.

	cmp byte [si], '0'
	jnz .not0digit
	inc si
	dec cx
.not0digit:

; Copy the rest of the converted BCD value to our buffer.

	rep movsb
	jmp .ftsExit

.notequal:
.notbelow18:

; Have fbstp round to 17 places.

	mov ax, 16		; experiment
	sub ax, word [iExp]	; adjust exponent to 17
	call PowerOf10

; Either we have exactly 17 digits, or we have exactly 16 digits.  We can
; detect that condition and adjust now.

	fcom qword [ten16]
; x0xxxx00 means top of stack > ten16
; x0xxxx01 means top of stack < ten16
; x1xxxx00 means top of stack = ten16
	fstsw word [mystat]
	mov ax, word [mystat]
	test ah, 1
	jz .notset1
	fmul qword [ten]
	dec word [iExp]
.notset1:

; Go convert to BCD.

	push di
	lea di, [szTemp]
	call FloatToBCD
	pop di

	lea si, [szTemp+1]	; point to converted buffer

; If the exponent is between -15 and 16, we should express this as a number
; without scientific notation.

	mov cx, word [iExp]
	push cx
	add cx, 15
	cmp cx, 15+16
	pop cx
	ja .fts6

; If the exponent is less than zero, we insert '0.', then -cx
; leading zeros, then 16 digits of mantissa. If the exponent is
; positive, we copy cx+1 digits, then a decimal point (maybe), then
; the remaining 16-cx digits.

	inc cx
	cmp cx, byte 0
	jg .ispos1
	mov ax, "0."
	stosw
	neg cx
	mov al, '0'
	rep stosb
	mov cx, 16
	jmp short .fts3
.ispos1:
	rep movsb
	mov al, '.'
	stosb
	mov cx, 16
	sub cx, word [iExp]
.fts3:
	rep movsb

; Trim off trailing zeros.

.nextitem2:
	cmp byte [di-1], '0'
	jne .fts1
	dec di
	jmp short .nextitem2
.fts1:

; If we cleared out all the decimal digits, kill the decimal point, too.

	cmp byte [di-1], '.'
	jnz .fts2
	dec di
.fts2:

; That's it.

	jmp short .ftsExit
.fts6:

; Now convert this to a standard, usable format.  If needed, a minus
; sign is already present in the outgoing buffer, and di already points
; past it.

	movsb			; copy the first digit
	mov al, '.'
	stosb			; plop in a decimal point
	movsw
	movsw
	movsw			; copy six more digits

%if 0

; The printf %g specified trims off trailing zeros here. I dislike
; this, so I've disabled it. Comment out the %if 0 and %endif if you
; want this.

.fts61:
	cmp byte [di-1], '0'
	je .fts62
	dec di
	jmp short .fts61
.fts62:
%endif

; Shove in the exponent. If you support 10-byte reals, remember to
; allow 4 digits for the exponent.

	mov ax, "e+"
	mov dx, word [iExp]
	test dx, dx
	jns .fts7
	neg dx
	mov ah, '-'
.fts7:
	stosw

	xchg ax, dx
	mov si, 10
	mov cx, 4
.fts8:
	xor dx, dx
	div si
	push dx
	loop .fts8
	mov cl, 4
.fts9:
	pop ax
	add al, '0'
	stosb
	loop .fts9

%if 0
	add di, byte 4		; -> terminator
%endif

; Clean up and go home.

.ftsExit:
;	fldcw [stat]		; restore control word
;	fwait
%if 0
	mov ax, di
	sub ax, word [pszDbl]
	mov cx, 21
	sub cx, ax
	mov al, 32
	rep stosb
%endif
	pop di
	pop es
	pop si
	pop ds

	lleave
	dualreturn
	lret

%undef pfpin
%undef pszDbl
%undef iExp
%undef mystat
%undef fpin
%undef szTemp
