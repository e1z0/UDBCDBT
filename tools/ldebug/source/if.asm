
%if 0

lDebug IF commands (conditional control flow)

Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
if_exists_check:
	push ss
	pop es
	mov di, word [if_exists_si]
	mov cx, word [if_exists_length]
	mov si, word [if_exists_sp]
	rep movsb
	mov cx, word [if_exists_length]
	inc cx
	and cl, ~1

	mov si, word [if_exists_then_address]
	test si, si
	jz .error
	dec si
	mov dx, msg.then
	call isstring?
	jne .error
	retn

.error:
	mov ax, 107h
	call setrc
	jmp error

if_exists_not_found:
	call if_exists_check
	testopt [internalflags3], dif3_if_not
	jnz if_exists_condition_met
if_exists_condition_not_met:
	jmp cmd3

if_exists_found_open:
	push ss
	pop es
	call getline_close_file

if_exists_found_closed:
	call if_exists_check
	testopt [internalflags3], dif3_if_not
	jnz if_exists_condition_not_met
if_exists_condition_met:
	mov sp, word [if_exists_sp]
	add sp, cx
	call skipwhite
	pop dx				; discard near return address
	clropt [internalflags3], dif3_in_if | dif3_auxbuff_guarded_1
	jmp cmd3_notblank
%endif


		; IF command -- conditional
ii:
	mov dx, si
	push ax
	mov ax, [si - 2]
	and ax, TOUPPER_W
	cmp ax, "IF"
	pop ax
	jne .not_if

	call skipwhite
	call isoperator?
	jne .if
	mov bx, cx
	add bx, bx			; bh = 0 !
	push ax
	call near [operatordispatchers+bx]
	pop ax
	test bx, bx
	jnz .not_if
	call skipwhite
.if:
	clropt [internalflags3], dif3_if_not
	dec si
	mov dx, msg.not
	call isstring?
	lodsb
	jne @F
	call skipwh0
	setopt [internalflags3], dif3_if_not

@@:
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	dec si
	mov dx, msg.exists
	call isstring?
	lodsb
	jne .if_numeric
	call skipwh0
	dec si
	mov dx, msg.r
	call isstring?
	je .is_variable
	mov dx, msg.y
	call isstring?
	jne error
	call skipwhite

	dec si
	mov word [if_exists_si], si
	mov bx, si
@@:
	lodsb
	call iseol?.notsemicolon
	jne @B
	mov cx, si		; -> after EOL byte
	sub cx, bx		; = length including EOL
	mov word [if_exists_length], cx
	inc cx			; round up
	and cl, ~1		; make even
	sub sp, cx
	mov word [if_exists_sp], sp
	mov di, sp
	mov si, bx
	shr cx, 1
	rep movsw

	mov si, bx
	lodsb
	and word [if_exists_then_address], 0
	setopt [internalflags3], dif3_in_if
	call yy
	jmp error
%endif

.if_numeric:
	call getexpression
	call toboolean
	mov bx, dx
.if_bx:
 	mov dx, msg.then
	dec si
	call isstring?
	jne error
	call skipwhite
	testopt [internalflags3], dif3_if_not
	jz @F
	xor bl, 1
@@:
	test bx, bx
	jz .if_false
	pop bx			; discard near return address to cmd3
	jmp cmd3_notblank	; execute tail

.if_false:
	jmp resetrc


.is_variable:
	call skipcomma
	call isvariable?
	mov bx, 1
	jnc .if_bx_comma
@@:
	lodsb
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, ','
	je @F
	call iseol?
	je @F
	cmp al, '('
	jne @B
	call skipwhite
	call getexpression
	cmp al, ')'
	jne error
	jmp @B

@@:
	xor bx, bx
.if_bx_comma:
	dec si
	call skipcomma
	jmp .if_bx
