
%if 0

lDebugX PM entrypoints

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2021 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

%if _CATCHPMINT214C
pmint21:
	cmp ah, 4Ch
	jne .next_no_pop
	push ds
	mov ds, word [cs:dssel]
	call entry_to_code_sel
	dw pmint21_4C_code

.next:
	pop ds
.next_no_pop:
%ifn _ONLYNON386
..@patch_no386_nop_DATA_ENTRY:
	o32
%endif
	jmp far [cs:pmintsave.int21]


	usesection lDEBUG_CODE

	code_insure_low_byte_not_0CCh
pmint21_4C_code:
	push word [cssel]
	push word pmint21.next

	_386_o32
	pusha
	push es

		; Some code (particularly d4message) may expect us to
		;  run on the debugger's stack, to access the data
		;  segment. Therefore, switch stacks.
	mov dx, ds			; dx = ds = debugger data selector
	mov ax, ss
	_386_o32		; mov ebx, esp
	mov bx, sp			; ax:(e)bx = stack to restore

	cmp dx, ax			; special case: already on our stack ?
	je @F				; yes, avoid stack switch -->

	_386_o32		; mov ecx, dword [run_sp]
	mov cx, word [run_sp]		; dx:(e)cx = our stack
	mov ss, dx
%ifn _ONLYNON386
..@patch_no386_ds_2:			; (as for the other case in run.asm
					;  insure to set sp directly after ss)
	o32			; mov esp, ecx
%endif
	mov sp, cx			; switch to our stack

@@:
	push ax
	_386_o32		; push ebx
	push bx				; save original stack, far pointer

	setopt [internalflags], protectedmode


		; remember that we cannot access Protected Mode any longer
		; (also clear modeswitched flag so resetmode is a no-op)
	clropt [internalflags], canswitchmode | switchbuffer | modeswitched

	mov cx, word [auxbuff_switchbuffer_size]
	jcxz .no_switchbuffer_size_change
	mov es, word [auxbuff_segorsel + soaSelector]
	xor di, di			; es:di -> auxbuff switchbuffer
	mov al, 32
	rep stosb			; fill with blanks (for WHILE)
.no_switchbuffer_size_change:
	and word [auxbuff_switchbuffer_size], 0

	push ss
	pop es

	call pm_reset_handlers


	_386_o32
	pop bx
	pop ax				; ax:(e)bx = original stack
	mov ss, ax
%ifn _ONLYNON386
..@patch_no386_ds_3:			; (as for the other case above
					;  insure to set sp directly after ss)
	o32			; mov esp, ebx
%endif
	mov sp, bx			; return to user stack

	pop es
	_386_o32
	popa

	retf				; jump to cssel:pmint21.next
%endif


	usesection lDEBUG_DATA_ENTRY

%if _CATCHPMINT41
pmint41:
	cmp ax, 4Fh
	je .is4F
%ifn _ONLYNON386
..@patch_no386_nop_2_DATA_ENTRY:
	o32
%endif
	jmp far [cs:pmintsave.int41]

.is4F:
	mov ax, 0F386h
	cmp byte [cs:dpmi32], 0
	je .iret16
	o32
.iret16:
	iret
%endif


	struc exceptionframe16
		resw 8			; pusha
fr16_ds:	resw 1			; push ds
		resw 2			; 16-bit return address to DPMI host
		resw 1			; error code
fr16_ip:	resw 1
fr16_cs:	resw 1
fr16_fl:	resw 1
fr16_sp:	resw 1
fr16_ss:	resw 1
	endstruc

	struc exceptionframe32
		resd 8			; pushad
		resw 1			; stack alignment
fr32_ds:	resw 1			; push ds
		resd 2			; 32-bit return address to DPMI host
		resd 1			; error code
fr32_eip:	resd 1
fr32_cs:	resd 1
fr32_efl:	resd 1
fr32_esp:	resd 1
fr32_ss:	resd 1
	endstruc

%assign EXC_ENTRY_FIRST 1
	%macro exc_entry 2.nolist
%ifn EXC_ENTRY_FIRST
	jmp strict short exc
%endif
exc %+ %1 %+ :
	push ds
	push %2
%if EXC_ENTRY_FIRST
 exc_second: equ $ + 2			; + 2 for short jump
%endif
%assign EXC_ENTRY_FIRST 0
	%endmacro

	align 2, db 0
		; Exception handlers.
		; These are the entry into the debugger in protected mode.
		; The address difference between exc_first and exc_second
		; is assumed to be the same for all the entries. This is
		; above, in installdpmi, as well as in run.asm run. These
		; are where the exception handlers are installed.

exc_first:
%if _CATCHEXC00
	exc_entry 00, int0msg
%endif
%if _CATCHEXC01
	exc_entry 01, int1msg
%endif
%if _CATCHEXC03
	exc_entry 03, int3msg
%endif
%if _CATCHEXC06
	exc_entry 06, exc6msg
%endif
%if _CATCHEXC0C
	exc_entry 0C, excCmsg
%endif
%if _CATCHEXC0D
	exc_entry 0D, excDmsg
%endif
%if _CATCHEXC0E
	exc_entry 0E, excEmsg
%endif
exc:
	mov ds, word [cs:dssel]
	pop word [run_int]
	times 1 - (($ - $$) & 1) nop	; align in-code parameter
	call entry_to_code_sel
	dw exc_code


	usesection lDEBUG_CODE

	code_insure_low_byte_not_0CCh
exc_code:
	cmp byte [dpmi32], 0
	jz exc16

[cpu 386]
exc32:
	push ax				; stack alignment
	pushad
	mov ebp, esp
	mov eax, dword [ ebp + fr32_eip ]
	mov bx, word [ ebp + fr32_cs ]
	mov ecx, dword [ ebp + fr32_efl ]
	mov edx, dword [ ebp + fr32_esp ]
	mov si, word [ ebp + fr32_ss ]
	mov word [ ebp + fr32_cs ], cs
	mov word [ ebp + fr32_ss ], ds
	cmp byte [ bInDbg ], 0		; did the exception occur inside DEBUG?
	je @F				; no -->

		; inside debugger
%if _EXCCSIP || _AREAS
	mov word [exception_csip], ax
	mov word [exception_csip + 2], bx
					; render CS:IP if internal GPF
 %if _AREAS
	push es
	 push ds
	 pop es
	cld
	xchg si, dx			; si -> stack
	mov di, exception_stack
	mov cx, 8
	rep movsb			; preserve 4 words
	pop es
 %endif
%endif
	mov dword [ ebp + fr32_eip ], debuggerexception
	movzx eax, word [ savesp ]
	mov dword [ ebp + fr32_esp ], eax
	clropt [ ebp + fr32_efl ], 100h	; reset TF
	jmp short @FF

@@:		; inside debuggee
	setopt [internalflags], protectedmode
	mov dword [ ebp + fr32_eip ], intrtn2_code
	clropt [ ebp + fr32_efl ], 300h	; reset IF + TF
	mov dword [ ebp + fr32_esp ], reg_ss
	mov dword [ reg_eip ], eax
	mov word [ reg_cs ], bx
	; mov dword [ reg_efl ], ecx	; (eflh is saved in intrtn2_code)
	mov word [ reg_efl ], cx
	mov dword [ reg_esp ], edx
	mov word [ reg_ss ], si
	push word [ ebp + fr32_ds ]
	pop word [ reg_ds ]

@@:
	popad
	pop ax				; stack alignment
	pop ds
	o32 retf

__CPU__
exc16:
	pusha
	mov bp, sp
	mov ax, word [ bp + fr16_ip ]
	mov bx, word [ bp + fr16_cs ]
	mov cx, word [ bp + fr16_fl ]
	mov dx, word [ bp + fr16_sp ]
	mov si, word [ bp + fr16_ss ]
	mov word [ bp + fr16_cs ], cs
	mov word [ bp + fr16_ss ], ds
	cmp byte [ bInDbg ], 0		; did the exception occur inside DEBUG?
	je isdebuggee16
%if _EXCCSIP || _AREAS
	mov word [exception_csip], ax
	mov word [exception_csip + 2], bx
					; render CS:IP if internal GPF
 %if _AREAS
	push es
	 push ds
	 pop es
	cld
	xchg si, dx			; si -> stack
	mov di, exception_stack
	mov cx, 8
	rep movsb			; preserve 4 words
	pop es
 %endif
%endif
	mov word [ bp + fr16_ip ], debuggerexception
	mov ax, word [ savesp ]
	mov word [ bp + fr16_sp ], ax
	clropt [ bp + fr16_fl ], 100h	; reset TF
	jmp short isdebugger16
isdebuggee16:
	setopt [internalflags], protectedmode
	mov word [ bp + fr16_ip ], intrtn2_code
	clropt [ bp + fr16_fl ], 300h	; reset IF + TF
	mov word [ bp + fr16_sp ], reg_ss
	mov word [ reg_eip ], ax
	mov word [ reg_cs ], bx
	mov word [ reg_efl ], cx
	mov word [ reg_esp ], dx
	mov word [ reg_ss ], si
	push word [ bp + fr16_ds ]
	pop word [ reg_ds ]
isdebugger16:
	popa
	pop ds
	retf


	usesection lDEBUG_DATA_ENTRY

i23pm:
%if 1
	push ds
	mov ds, word [cs:dssel]
	setopt [serial_flags], sf_ctrl_c
	pop ds
%endif
	cmp byte [ cs:dpmi32 ], 0	; ! always NC
	je .retfw_2
[cpu 386]
	retfd 4				; (NC)
__CPU__
.retfw_2:
	retfw 2				; (NC)

i24pm:
	mov al, 03h			; fail
	cmp byte [ cs:dpmi32 ], 0
	je .iret16
	o32				; iretd
.iret16:
	iret
