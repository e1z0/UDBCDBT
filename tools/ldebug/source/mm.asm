
%if 0

lDebug M commands (move, machine type)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

		; M command - move from place to place.
		;
		; First check for machine-related M commands.
		; Those are: M, MNC, M?, MC, MC2, MC3, M [one expression]
		; Move M command has more than one expression.
mm:
	mov cx, si		; - 1 -> input
	push si
	call iseol?
	je mc			; no argument, CPU-related M command
	mov ah, byte [ si ]
	push ax
	and ax, ~(2020h)
	cmp ax, "NC"
	pop ax
	jne @F
	mov cx, msg.c0 + 1	; - 1 -> C0 string
	inc si			; skip 'N'
	jmp .checkend

@@:
	cmp al, '?'
	jne @F
	mov cx, msg.cr + 1	; - 1 -> empty string
.checkend:
	call skipwhite		; skip '?' or 'C' (in "NC")
	call iseol?
	je mc
	pop si
	push si
	dec si
	call skipwhite
@@:
	push si
	call prephack
	mov bx, word [reg_ds]	; get source range
	call getaddrX		; just parse an address first
		; Note that valid MC commands allow at most C3h
		;  in the expression read here. This must be
		;  allowed by this getaddrX call regardless the
		;  D/B bit and limit of the ds segment.
		; By using getaddrX here instead of getexpression
		;  as previously, we get support for all special
		;  cases of address parameters for free. This now
		;  includes the single or double dollar sign prefix
		;  and also the taken keywords (not previously
		;  allowed here).
	pop cx			; - 1 -> input
	call iseol?
	je mc			; one argument, CPU-related

		; bx:(e)dx, si, al are already initialised here.
.mm:
	pop di			; discard si on stack

			; It is a normal M command (Move)
%if _PM
	mov di, getaddr		; second parameter must be writable
%endif
	call parsecm_have_address
				; parse arguments (DS:ESI, ES:EDI, ECX)
	push cx
%if _PM
	call ispm
	jnz .rm
	mov ax, ds
	mov cx, es
	cmp ax, cx
	je .pmsimple		; same selector, simple -->

	mov ax, 0006h
	mov bx, ds
	int 31h			; get selector's base
	jc error
	push cx
	push dx
	mov ax, 0006h
	mov bx, es
	int 31h			; get selector's base
	jc error		; throw
	cmp byte [ss:bAddr32], 0
	je .pm16
[cpu 386]
	pop eax
	push cx
	push dx
	pop edx			; mov edx, cxdx
	add eax, esi		; add offset to source selector's base
	jc error
	add edx, edi		; add offset to destination selector's base
	jc error		; if overflow (> 4 GiB) -->
	cmp eax, edx		; compare linear source to linear destination
	jmp short m3		; and decide whether to move up or down -->
__CPU__

.rm:
	mov ax, ds
	mov bx, ds
	mov dx, es
	mov cl, 12
	shr bx, cl
	shr dx, cl
	push dx
	mov dx, es
	mov cl, 4
	shl ax, cl
	shl dx, cl
	pop cx
	db __TEST_IMM16		; (skip 2 pop instructions)

.pm16:
	pop ax
	pop bx
	add ax, si
	adc bx, byte 0		; add offset to source selector's base
	jc error
	add dx, di
	adc cx, byte 0		; add offset to destination selector's base
	jc error		; if overflow (> 4 GiB) -->
	cmp bx, cx		; compare linear source to linear destination
	jne m3
	cmp ax, dx
	jmp short m3		; and decide whether to move up or down -->

.pmsimple:
	_386_o32	; cmp esi, edi
	cmp si, di
%else
	mov dx, di
	mov bx, es
	mov cl, 4
	shr dx, cl
	add dx, bx		; upper 16 bits of destination
	mov ax, si
	shr ax, cl
	mov bx, ds
	add ax, bx
	cmp ax, dx
	jne m3			; if we know which is larger
	mov ax, si
	and al, 0Fh
	mov bx, di
	and bl, 0Fh
	cmp al, bl
%endif
m3:	pop cx
	lahf
	push ds
	push es
	push ss			; ds := cs
	pop ds
	call dohack		; do the interrupt pointer hack
	pop es
	pop ds
	sahf
	jae .forward		; if forward copy is OK
	_386_PM_o32
	add si, cx
	_386_PM_o32
	add di, cx		; point both behind data
	std			; _AMD_ERRATUM_109_WORKAROUND as below


	numdef AMD_ERRATUM_109_WORKAROUND, 1
		; Refer to comment in init.asm init_movp.

%if _AMD_ERRATUM_109_WORKAROUND
	_386_PM_a32
	jcxz @FF
	_386_PM_o32
	cmp cx, strict byte 20
	ja @FF
@@:
	_386_PM_a32
	movsb
	_386_PM_a32
	loop @B
@@:
%endif
.forward:
	_386_PM_a32
	rep movsb		; do the move
	_386_PM_a32
	movsb			; one more byte (length of zero means 64 KiB. or 4 GiB..)
.was32:
	cld			; restore flag
	jmp ee0a		; restore segments and undo the interrupt pointer hack


		; Other M command: set machine type.
		;
		; INP:	cx -> numeric input (expression 0..6, C, C0, C2, C3)
		;	or cx -> EOL
		;	word [ss:sp] = to discard
mc:
	mov si, cx
	pop dx			; discard
	dec si
	call skipwhite		; reload
	call iseol?
	je mquery		; if just an M or M? (query machine type) -->
	call getbyte		; get numeric input
	call chkeol		; insure valid
	xchg ax, dx
	cmp al, 6
	ja mc_fpu

mc_cpu:
	mov byte [machine], al	; set machine type
	mov byte [mach_87], al	; coprocessor type, too

mc_encode:
	cmp byte [has_87], 0
	mov al, 0C0h
	je .done
	cmp byte [machine], 3
	mov al, 0Ch
	jne .done
	cmp byte [mach_87], 2
	jne .done
	mov al, 0C2h
.done:
	mov byte [encodedmach87], al
	retn

mc_fpu:
	mov ah, byte [machine]
	cmp al, 0Ch		; MC command ?
	je mcc_ah
	cmp al, 0C0h		; MC0 command or MNC command ?
	je mnc
	cmp ah, 3		; MC2 or MC3 only valid for machine 386
	jne .error
	cmp al, 0C2h		; MC2 command ?
	je mcc_2
	cmp al, 0C3h		; MC3 command ?
	je mcc_3		; (ah = 3)
.error:				; invalid input
	jmp error

mnc:
	mov byte [has_87], 0	; clear coprocessor flag
	jmp mc_encode		; done

mcc_2:
	mov ah, 2		; set type to 287
mcc_3:				; (if jumping here ah = 3) set type to 387
mcc_ah:
	mov byte [has_87], 1	; set coprocessor flag
	mov byte [mach_87], ah	; set coprocessor type
	jmp mc_encode		; done


		; Display machine type.
mquery:
	mov si, msg8088
	mov al, byte [machine]
	cmp al, 0
	je .88or86		; if 8088
	mov si, msgx86
	add al, '0'
	mov byte [si], al
.88or86:
	call showstring
	mov si, no_copr
	cmp byte [has_87], 0
	je .m12			; if no coprocessor
	mov si, has_copr
	mov al, byte [mach_87]
	cmp al, byte [machine]
	je .m12			; if has coprocessor same as processor
	mov si, has_287
.m12:
	call showstring		; show string
	jmp putsline_crlf	; call puts and quit
