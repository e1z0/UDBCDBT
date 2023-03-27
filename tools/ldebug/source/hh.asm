
%if 0

lDebug H commands (hexadecimal calculation)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

%if _EXPRESSIONS

		; H command - hex computation
hh:
	call skipcomm0
	dec si
	mov dx, msg.base
	call isstring?
	jne .normal
	call skipequals
	call getexpression
	test bx, bx
@@:
	jnz error
	cmp dx, 36
	ja @B
	cmp dx, 2
	jb @B
	push dx

	call skipcomm0
	dec si
	mov dx, msg.group
	call isstring?
	mov dx, 0
	jne .gotgroup
	call skipequals
	call getexpression
	call skipcomm0
	test bx, bx
	jnz @B
	cmp dx, 32
	ja @B
	dec si
.gotgroup:
	push dx
	mov dx, msg.width
	call isstring?
	mov bx, 0
	jne .gotwidth
	call skipequals
	call getexpression
	call skipcomm0
	test bx, bx
	jnz @B
	cmp dx, 32
	ja @B
	mov bx, dx
	db __TEST_IMM8		; (skip lodsb)
.gotwidth:
	lodsb
	push bx
	call .compute
	pop ax
	pop si
	pop cx
	mov di, line_out + 66

	call .storeresult

	xchg bx, ax		; ax:dx = number, bx = width
	xchg ax, dx		; dx:ax = number
	lframe
	lenter
	lvar dword,	dividend
	 push dx
	 push ax
	dec bx
	lvar word,	minwidth
	 push bx
	lvar word,	group
	 push si
	lvar word,	groupcounter
	 push si

	mov bx, di
	std			; _AMD_ERRATUM_109_WORKAROUND does not apply

		; dword [bp + ?dividend] = number to display
		; cx = base
.loop_write:

	xor dx, dx
	push di
	mov di, 4
.loop_divide:
	mov ax, [bp + ?dividend - 2 + di]
	div cx
	mov word [bp + ?dividend - 2 + di], ax
	dec di
	dec di
	jnz .loop_divide
				; dx = last remainder
	pop di
	xchg ax, dx		; ax = remainder (next digit)
				; dword [bp + ?dividend] = result of div
	add al, '0'
	cmp al, '9'
	jbe @F
	add al, -('9'+1)+'A'
@@:
	stosb

	dec word [bp + ?groupcounter]
	jnz @F
	push word [bp + ?group]
	pop word [bp + ?groupcounter]
	mov al, '_'
	stosb
@@:

	dec word [bp + ?minwidth]
	jns .loop_write

	cmp word [bp + ?dividend + 2], 0
	jnz .loop_write
	cmp word [bp + ?dividend], 0
				; any more ?
	jnz .loop_write		; loop -->

	cld

	sub bx, di
	mov cx, bx
	mov si, di
	inc si

	mov di, line_out

	cmp byte [si], '_'
	jne @F
	inc si
	dec cx
		; never need to loop because next digit is always a digit

@@:
	rep movsb		; overlapping!

	lleave
	jmp short .putsline_crlf


.normal:
	lodsb
	push si
	push ax
	or byte [hhflag], 1	; set flag so no operator means add
	call .compute
	pop ax
	pop si
	call .storeresult
	test byte [hhflag], 4	; any two-fold operation ?
	jz .single		; no -->
	mov ah, byte [options2 + 1]
	and ah, opt2_hh_compat >> 8
	call .store2		; display "FFFFFFFF (-0001)"
	push ax
	mov ax, 32<<8|32
	stosw
	pop ax
	push ax
	or byte [hhflag], 2	; set flag so no operator means sub
	call .compute
	pop ax
	call .store2		; display "FFFFFFFF (-0001)"
	jmp short .putsline_crlf

.single:
	mov ah, 0
	 push bx
	 push dx
	call .store2

	mov ax, (32 << 8) | 32
	stosw
	mov ax, "de"
	stosw
	mov ax, "ci"
	stosw
	mov ax, "ma"
	stosw
	mov ax, "l:"
	stosw
	mov al, 32
	stosb
	 pop ax
	 pop dx
	call decdword
	test dx, dx		; result negative ?
	jns @F			; no -->
	 push ax
	mov ax, " ("
	stosw
	mov al, "-"
	stosb
	 pop ax
	neg dx
	neg ax
	sbb dx, byte 0		; neg bx:dx
	call decdword
	mov al, ")"
	stosb
@@:
.putsline_crlf:
	jmp putsline_crlf


.compute:
	call getdword
	call chkeol		; expect end of line here
.comp_ret:
	retn

		; INP:	bx:dx = result
		;	ah = flag, nonzero if to stay 86-DOS Debug compatible
		; OUT:	displayed
		; CHG:	di, bx, dx
.store:
	push ax
	test ah, ah
	jnz .store_nothigh
	test bx, bx
	jz .store_nothigh	; no need to display 32-bit value
	mov ax, bx
	call hexword
.store_nothigh:
	mov ax, dx
	call hexword
	pop ax
	retn

.store2:
	call .store
	push ax
	test ah, ah
	jnz .store2_ret
	test bx, bx		; result negative ?
	jns .store2_ret		; no -->
	mov ax, " ("
	stosw
	mov al, "-"
	stosb
	neg bx
	neg dx
	sbb bx, byte 0		; neg bx:dx
	pop ax
	push ax
	call .store
	mov al, ")"
	stosb
.store2_ret:
	pop ax
	retn


.storeresult:
	mov word [hhresult], dx
	mov word [hhresult + 2], bx
	retn
%else
		; H command - hex addition and subtraction.
hh:
	call getdword
	push bx
	push dx
	call skipcomm0
	call getdword
	call chkeol		; expect end of line here
	pop cx
	pop ax			; first value in AX:CX, second in BX:DX
	mov si, ax
	mov bp, cx		; first value in SI:BP now
	mov ax, cx
	add ax, dx
	push ax
	mov ax, si
	adc ax, bx
	jz .nothigh1		; no need to display 32-bit value
	call hexword
.nothigh1:
	pop ax
	call hexword
	mov ax, 2020h
	stosw
	mov ax, bp
	sub ax, dx
	push ax
	mov ax, si
	sbb ax, bx
	jz .nothigh2		; no need to display 32-bit value
	or si, bx
	jz .nothigh2		; both were zero, non-zero result only by carry -->
	call hexword
.nothigh2:
	pop ax
	call hexword
	call putsline_crlf
	retn
%endif


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
hhresult:	dd 0
