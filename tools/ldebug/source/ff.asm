
%if 0

lDebug F command (find)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

		; F command - fill memory
ff:
	xor cx, cx		; get address range (no default length)
	mov bx, word [reg_ds]
	call getrange		; get address range into bx:(e)dx
	_386_PM_o32	; sub ecx, edx
	sub cx, dx
	_386_PM_o32	; inc ecx
	inc cx			; (e)cx = number of bytes
	push bx
	_386_PM_o32	; push ecx
	push cx			; save it
	_386_PM_o32	; push edx
	push dx			; save start address

	dec si
	mov dx, msg.range
	call isstring?
	lodsb
	jne .notrange

	mov bx, word [reg_ds]	; get search range
	setopt [internalflags3], dif3_accept_getrange_0
	call getrangeX.ecx_and_0_valid
				; try to get second range
	call chkeol		; and insure end-of-line
				; successful if it returned
	_386_PM_o32		; mov esi, edx
	mov si, dx		; bx:esi-> source string
	_386_PM_o32		; sub ecx, edx
	sub cx, dx		; ecx = count - 1
	jmp short @F

.notrange:
	call skipcomm0
	call getstr		; get string of bytes
	mov cx, di
	sub cx, line_out
	jz error
	dec cx
_386_PM	movzx ecx, cx
	mov bx, ds
_386_PM	xor esi, esi
	mov si, line_out

@@:
	_386_PM_o32	; pop edi
	pop di
	_386_PM_o32	; pop eax
	pop ax
	pop es
	mov ds, bx
%if _PM
	cmp byte [ss:bAddr32], 0
	jz ff16
ff32:
[cpu 386]
	inc ecx
	jz error
	cmp ecx, byte 1
	je .onebytesource
	xor edx, edx		; edx:eax = size
	div ecx
	test eax, eax
	jz .partial
.loop:
	push esi
	push ecx
	a32 movsb
	dec ecx
	a32 rep movsb
	pop ecx
	pop esi
	dec eax
	jnz .loop
.partial:
	mov ecx, edx		; get remainder (number of bytes in partial copy)
	jecxz ffret		; if no partial copy -->
	a32 rep movsb
	jmp short ffret		; done -->
.onebytesource:
	mov ecx, eax		; size
	mov al, byte [esi]
	a32 rep stosb
	jmp short ffret
__CPU__
ff16:
%endif
	xor dx, dx		; dx:ax = size
	cmp ax, byte 1
	adc dx, byte 0		; convert 0000:0000 to 0001:0000 (0 = 64 KiB)
	inc cx
	jnz @F
; dx:ax = 1_0000h, remainder = 0, quotient = 1
; dx:ax = 1, remainder = 1, quotient = 0
; dx:ax = 1234h, remainder = 1234h, quotient = 0
	xchg ax, dx
	jmp @FF

@@:
	cmp cx, byte 1
	je .onebytesource	; a common optimization
	div cx			; compute number of whole repetitions
@@:
	test ax, ax
	jz .partial		; if less than one whole rep
.loop:
	push si
	push cx
	movsb
	dec cx
	rep movsb
	pop cx
	pop si
	dec ax
	jnz .loop		; if more to go
.partial:
	mov cx, dx		; get remainder (number of bytes in partial copy)
	jcxz ffret		; if no partial copy -->
	rep movsb
	jmp short ffret		; done -->
.onebytesource:
	mov cx, ax		; size
	mov al, byte [si]
	stosb			; cx=0 -> 64 kB
	dec cx
	rep stosb
ffret:
	push ss			; restore ds
	pop ds
	push ss			; restore es
	pop es
	retn
