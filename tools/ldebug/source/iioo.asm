
%if 0

lDebug I and O commands (port input/output)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

		; O command - output to I/O port.
oo:
	mov ah, 'O'
	mov bx, .tab
	jmp ii.common


	align 2, db 0
.tab:
	dw .byte, .word, .dword


.byte:
	call getbyte		; read value from command line
	call chkeol		; expect end of line here
	xchg ax, dx		; al = value
	pop dx			; recover port number
	out dx, al		; send
	retn

.word:
	call getword
	call chkeol
	xchg ax, dx		; ax = value
	pop dx
	out dx, ax
	retn

.dword:
[cpu 386]
	call getdword
	call chkeol		; expect end of line here
	push bx
	push dx
	pop eax			; eax = value
	pop dx
	out dx, eax
	retn
__CPU__


%include "if.asm"		; (jumps to .not_if for port Input commands)
		; I command - input from I/O port.
.not_if:
	mov si, dx
	dec si
	dec si
	mov dx, msg.install
	call isstring?
	lodsb
	je install
	lodsb

	mov ah, 'I'
	mov bx, .tab

		; bx = jump table for byte, word, dword handler
		; ah = letter of the command
		; si, al etc.
.common:
	push ax
	call uppercase
	cmp al, 'W'
	jne .notw
.incbx2:
	inc bx
	inc bx			; use word handler
	call skipwhite		; skip the 'W' til next character
	jmp short .sizeset

.notw:
	cmp al, 'D'
	jne .sizeset
%if 1
	xor ah, byte [si-2]
	jz .d			; "Id" or "Od" --> (uppercase command)
	xor ah, 32
	jnz .sizeset		; no space is allowed between the command and 'D' -->
			; "id" or "od" here (lowercase command)
.d:
%endif
_386	inc bx
_386	inc bx			; use dword handler
	_386_jmps .incbx2	; bx += 2 and skip the 'D'
			; no 386 here. try with D as part of port number
.sizeset:
	call getword		; get port
	pop cx			; restore letter if necessary
	cmp ch, 'I'		; check whether I or O
	jne .o			; O -->
	call chkeol		; expect end of line here for I commands
	db __TEST_IMM8		; (skip push)
.o:
	push dx			; save port number for O commands
	jmp near [cs:bx]


	align 2, db 0
.tab:
	dw .byte, .word, .dword

.byte:
	in al, dx
	call hexbyte
	jmp short .done

.word:
	in ax, dx
.doneword:
	call hexword
.done:
	jmp putsline_crlf

.dword:
[cpu 386]
	in eax, dx
	call hexword_high
__CPU__
	jmp short .doneword


errorj5:jmp	error
