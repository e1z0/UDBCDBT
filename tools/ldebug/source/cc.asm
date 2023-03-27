
%if 0

lDebug C command (compare)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

		; C command - compare bytes.
cc:
%if _PM
	mov di, getaddrX	; second parameter needn't be writable
%endif
	call parsecm		; parse arguments

		; To make the 16-bit 64 KiB compare hack below work, the
		; full ecx mustn't be increased here for 16-bit segments.
		; The passed ecx isn't higher than FFFFh for 16-bit segments,
		; and a value of 0001_0000h needs to be passed as zero to
		; the hack anyway.
%if _PM
	cmp byte [ss:bAddr32], 0
	je .16
[cpu 386]
	inc ecx
	jnz cc1
	jmp error
__CPU__
.16:
%endif
	inc cx
cc1:
	push ds
	push es
	push ss
	pop ds			; ds := cs
	call dohack		; do the interrupt pointer hack
	pop es
	pop ds
%if _PM
	cmp byte [ss:bAddr32], 0
	jz .cmp16
	a32 repe cmpsb
	mov dl, byte [esi-1]
	mov dh, byte [es:edi-1]
	jmp short .cmpdone
.cmp16:
%endif
		; The following 3 instructions make a hack to support 64 KiB
		; compare. The only time we get here with cx = 0 is the first
		; iteration for a 64 KiB compare. In that case, dec cx results
		; in FFFFh making repe cmpsb work. The single cmpsb will either
		; jump the repe cmpsb (if it found a mismatch) or not jump it.
		; The repe cmpsb might be executed with cx = 0, but will then
		; not change anything including the flags so it works.
	dec cx
	cmpsb
	jne .skip
	repe cmpsb		; start comparing
.skip:
	mov dl, byte [si-1]	; save the possibly errant characters
	mov dh, byte [es:di-1]
.cmpdone:
	lahf
	push ds
	push es
	push ss
	pop ds
	call unhack		; undo the interrupt pointer hack
	pop es
	pop ds
	sahf
	je cc2			; if we're done
	push es
	push ss
	pop es
	_386_PM_o32	; mov ebx, edi
	mov bx, di
	mov	di, line_out
	mov	ax, ds
	call hexword
	mov	al, ':'
	stosb
	_386_PM_o32	; mov eax, esi
	mov ax, si
	_386_PM_o32	; dec eax
	dec ax
%if _PM
	cmp byte [ss:bAddr32], 0
	jz .16si
	call hexword_high
.16si:
%endif
	call hexword
	mov ax, 32<<8|32
	stosw
	mov al, dl
	call hexbyte
	mov ax, 32<<8|32
	stosw
	mov al, dh
	call hexbyte
	mov ax, 32<<8|32
	stosw
	pop	ax
	push	ax
	call hexword
	mov al, ':'
	stosb
	_386_PM_o32	; mov eax, ebx
	mov ax, bx
	_386_PM_o32	; dec eax
	dec ax
%if _PM
	cmp byte [ss:bAddr32], 0
	jz .16bx
	call hexword_high
.16bx:
%endif
	call hexword
	push ds
	push ss
	pop ds
	push bx
	push cx
	call putsline_crlf
	pop cx
	pop	di
	pop	ds
	pop	es
%if _PM
	cmp byte [ss:bAddr32],0
	jz cc1_6
[cpu 386]
	jecxz cc2
__CPU__
 cc1_j1:
	jmp cc1
cc1_6:
%else
 cc1_j1 equ cc1
%endif
%if 0
	_386_PM_o32	; inc ecx
	inc cx
	_386_PM_a32	; loopd cc1
	loop cc1		; if not done yet
%else
	_386_PM_a32	; jecxz cc2
	jcxz cc2
	jmp cc1			; if not done yet
%endif
cc2:
	push ss			; restore segment registers
	pop ds
	push ss
	pop es
	retn
