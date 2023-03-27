%if 0

lDebug assembler/immediate execution handler

Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

immasm:
%if _DUALCODE
section_of aa_imm_enrry

	usesection lDEBUG_CODE2
section_of immasm_relocated

	usesection lDEBUG_CODE
	 dualcall immasm_relocated

	usesection lDEBUG_CODE2

dualfunction
immasm_relocated: section_of_function
%endif
	nearcall skipwhite
	nearcall iseol?
	je .ret
	mov byte [ ia_exec ], 0
	cmp al, '.'			; second dot ?
	jne .nodot			; no -->
	inc byte [ ia_exec ]		; note it
	nearcall skipwhite		; skip it
.nodot:
	cmp al, ','			; comma ?
	jne .nocomma			; no -->
	or byte [ ia_exec ], 80h	; note it
	nearcall skipwhite		; skip it
.nocomma:
	nearcall iseol?
	je .ret_e			; empty -->

%if _IMMASM_AUXBUFF
	nearcall guard_auxbuff
%endif

	push ax
	push si

	mov si, a_addr
	mov di, ia_savedaddr
	movsw
	movsw
	movsw				; save A command address
	mov word [ ia_restore ], ia_restore_a_addr

	push es
	call getimmselseg
	xor di, di
	mov word [ a_addr+4 ], bx
_386_PM	and word [ a_addr+2 ], byte 0
	mov word [ a_addr+0 ], di	; initialize address

%if _PM
	push bx
	nearcall verifysegm_or_error
%endif
	mov es, bx
	mov cx, immasm_length
	mov al, 0CCh
	rep stosb			; initialize buffer
%if _PM
	pop bx
%endif
	pop es

	pop si
	pop ax
	mov word [ aa_ret ], .aa_ret	; make assembler return here
	mov word [ ia_sp ], sp		; (possibly unnecessary)

%if _PM
	mov byte [ bCSAttr ], 0
	nearcall test_d_b_bit
	jz .16
	mov byte [ bCSAttr ], 40h
.16:
%endif

	setopt [internalflags6], dif6_immasm
	clropt [internalflags6], dif6_immasm_rel8
%if _DUALCODE
	dualcall aa_imm_entry

	section_of immasm_relocated.aa_ret_relocated

	usesection lDEBUG_CODE
.aa_ret:
	dualcall immasm_relocated.aa_ret_relocated

	usesection lDEBUG_CODE2
.aa_ret_relocated:
%else
	jmp aa_imm_entry

.aa_ret:
%endif
	mov sp, word [ ia_sp ]
	push ss
	pop ds
	push ss
	pop es

		; dwo [a_addr] - dwo [ia_savedaddr] = len of opcode

	nearcall ia_restore_a_addr

		; for now, disassemble the result
	mov si, u_addr
	mov di, ia_savedaddr
	movsw
	movsw
	movsw				; save U command address
	mov word [ ia_restore ], ia_restore_u_addr

	call getimmselseg
	mov word [ u_addr+4 ], bx
_386_PM	and word [ u_addr+2 ], byte 0
	and word [ u_addr+0 ], byte 0	; initialize address

	testopt [options6], opt6_immasm_display_uu
	jnz @F
	setopt [internalflags6], dif6_immasm_no_output
@@:

	mov word [disflags], DIS_F_SHOW
	nearcall disasm
	clropt [internalflags6], dif6_immasm_no_output

	push ss
	pop ds
	push ss
	pop es

	nearcall ia_restore_u_addr

	testopt [options6], opt6_immasm_flag
	jz @F
	test byte [ ia_exec ], 1	; test execution ?
.ret_e:
	jz .ret				; no -->
@@:

	mov si, reg_eip
	mov di, ia_savedcseip
	movsw
_386_PM	movsw
	mov si, reg_cs
	movsw				; save CS:(E)IP
	mov ax, word [reg_ds]
	mov word [immasm_save_ds], ax	; unconditionally store ds

	and word [immasm_temp], 0
	cmp byte [segmnt], 1
	jne .not_cs_prefix
	inc word [immasm_temp]		; flag used by .lds
	mov byte [segmnt], 3
	mov word [ ia_restore ], ia_restore_ds
	mov ax, word [reg_cs]
	mov word [reg_ds], ax
	call getimmselseg
	mov es, bx
	xor di, di
	mov al, 2Eh
	mov cx, immasm_length
	repne scasb
	jne error_mirror
%if _PM
	nearcall verifysegm_or_error
	mov es, bx
%endif
	mov byte [es:di - 1], 3Eh
	push ss
	pop es

.not_cs_prefix:
	mov ax, word [index]
	mov di, immasm_special_indices
	mov cx, immasm_special_indices.amount
	repne scasw
	jne .notspecial
	testopt [options6], opt6_immasm_debug
	jz @F
	mov dx, word [di - (immasm_special_indices. + 2) + immasm_special_indices.names]
	nearcall putsz
	mov dx, crlf
	nearcall putsz
@@:
	mov dx, word [di - (immasm_special_indices. + 2) + immasm_special_indices.functions]
	mov di, line_out

	mov ax, word [reg_efl]
	mov word [immasm_efl], ax
	mov ax, word [reg_efl + 2]
	mov word [immasm_efl + 2], ax
	mov ax, word [immasm_save_ds]
	mov word [immasm_ds], ax
	mov word [immasm_callback], .retn

		; OUT:	cx:dx:ax = cs:eip
		;	dword [immasm_efl] = efl
		;	word [immasm_ds] = ds
		;	word [immasm_callback] = to call
		;	(e)bx = (e)sp
	call dx
	testopt [options6], opt6_immasm_debug
	jz @F
	nearcall putsline_crlf
@@:
	testopt [options6], opt6_immasm_nobranch
	jnz .ret_j
	test dx, dx
	jz @F
%if _PM
	nearcall ispm
	jnz .error_eip
	push bx
	mov bx, cx
	nearcall test_d_b_bit
	pop bx
	jnz @F
%endif
.error_eip:
	mov dx, msg.immasm_error_eip
	nearcall putsz
	jmp .ret_j

.retn:
	retn

@@:
	mov word [reg_eip], ax
	mov word [reg_eip + 2], dx
	mov word [reg_cs], cx
	_386_PM_o32
	mov word [reg_esp], bx
	call near [immasm_callback]
	mov ax, word [immasm_efl]
	mov word [reg_efl], ax
	mov ax, word [immasm_efl + 2]
	mov word [reg_efl + 2], ax
	mov ax, word [immasm_ds]
	mov word [reg_ds], ax
.ret_j:
	jmp .ret

.notspecial:
..@immasm_not_special:
	testopt [internalflags6], dif6_immasm_rel8
	jz .exec

	mov si, word [condmsg]
	test si, si
	jz .jumping			; this is an unconditional jump -->
	cmp si, msg.condjump		; jumping ?
	jne .exec_if_loop		; no, just decrement (e)cx

.jumping:
	_386_PM_o32
	mov ax, word [immasm_rel8_target]
	_386_PM_o32
	mov word [reg_eip], ax		; update (e)ip with branch target

.exec_if_loop:
	mov ax, word [index]
	cmp ax, 0E0h
	jb .ret
	cmp ax, 0E2h
	ja .ret
					; decrement (e)cx but keep (e)ip

.exec:
	mov ax, ia_restore_cseip_and_u_addr
	cmp word [ ia_restore ], ia_restore_ds
	jne @F
	mov ax, ia_restore_cseip_and_u_addr_and_ds
@@:
	cmp word [ ia_restore ], ia_restore_es
	jne @F
	mov ax, ia_restore_cseip_and_u_addr_and_es
@@:
	mov word [ ia_restore ], ax

	call getimmselseg
	mov word [ reg_cs ], bx
_386_PM	and word [ reg_eip+2 ], byte 0
	and word [ reg_eip+0 ], byte 0	; initialize address


%if _PM
	nearcall resetmode
%endif
	mov dx, 15		; DL = number of bytes to go; DH = prefix flags.
	mov bx, word [reg_cs]
	_386_PM_o32	; mov esi, dword [reg_eip]
	mov si, word [reg_eip]
.pp2:
	nearcall pp16		; get next instruction byte into AL
	mov di, ppbytes
	mov cx, PPLEN_ONLY_STRING
%if _SYMBOLIC
	mov byte [pp_instruction], al
%endif
	repne scasb
	jne .not_p		; if not one of these -->
	mov al,byte [di+PPLEN-1]; get corresponding byte in ppinfo
	test al, PP_PREFIX	; prefix ?
	jz .pp3			; no -->
	or dh, al		; set the OSIZE or ASIZE flags if either of these
			; Note:	Multiple OSIZE in a 16-bit cs do not toggle
			;	between decoding as O32 and O16, they're always
			;	decoded as O32. The same is true for A32, and
			;	in a 32-bit cs for O16 and A16.
	dec dl
	jnz .pp2		; if not out of bytes -->
	mov dx, msg.warnprefix
	nearcall putsz
	jmp .not_p

		; A repeatable string instruction is to be decoded.
		; Finish the decoding and skip the appropriate number
		; of opcode bytes.
.pp3:
_386_PM	nearcall pp_fix32bitflags
	test al, PP_VARSIZ | PP_SIZ_MASK
	jnz error_mirror
%if 0
	test al, PP_VARSIZ	; different opcode length depends on OSIZE ?
	jz .ignoreosize		; no -->
	and dh, 2
	add al, dh
.ignoreosize:
	and ax, PP_SIZ_MASK
_386_PM	movzx eax, ax		; clear high word (in case it counts)
	_386_PM_o32	; add esi, eax
	add si, ax
%endif
; pp10:
%if _SYMBOLIC && 0
	nearcall pp3_check_symhints
	jc .not_p		; trace -->
%endif
	; jmp short pp11	; we have a skippable instruction here
; pp11:
_386_PM	nearcall resetmode_and_test_d_b_bit
_386_PM	jnz .32			; full 32-bit offset valid -->
_386_PM	movzx esi, si		; clear high word here
.32:
	nearcall proceedbreakpoint	; run until the breakpoint is hit
		; This call might return modeswitched.
	jmp short @F

.not_p:
	nearcall traceone	; call common code
@@:
%if _PM
	nearcall resetmode
%endif
	jc .unexpected			; --> (returns to cmd3)
	testopt [options6], opt6_immasm_debug
	jz .ret
	nearcall dumpregs_no_disasm	; dump registers (only)
.ret:
%if _DUALCODE
	jmp cmd3_mirror

.unexpected:
	dualcall unexpectedinterrupt
%else
	jmp cmd3

.unexpected:
	jmp unexpectedinterrupt
%endif



immasm_run_near:
	mov bx, word [reg_cs]
	_386_PM_o32
	mov si, word [reg_eip]
	mov dl, 15		; number of bytes to go
.loop:
	nearcall pp16
	cmp al, 0CBh		; retf ?
	je .trace
	cmp al, 0CAh		; retf imm16 ?
	je .trace
	cmp al, 0CFh		; iret ?
	je .trace
	mov di, ppbytes
	mov cx, PPLEN_ONLY_PREFIXES
	repne scasb
	jne .done
				; if one of the prefixes
	dec dl
	jnz .loop		; if not out of bytes -->

				; fall through: do not trace
.done:

immasm_run_far:
	nearcall ia_restore_ds

	mov si, ia_savedcseip
	_386_PM_o32
	lodsw				; (e)ax = (e)ip
	_386_PM_o32
	push ax
	lodsw				; ax = cs
	xchg bx, ax			; bx = cs
	_386_PM_o32
	pop si				; (e)si = (e)ip
	nearcall proceedbreakpoint
immasm_run_near.trace:
	jmp @B


getimmselseg:
	mov bx, word [immseg]
%if _PM
	nearcall ispm
	jnz .ret
	push cx
	push ax
subcpu 286
	mov bx, word [reg_cs]
	nearcall test_d_b_bit
	mov bx, word [immsel]
	jz .16
.32:
	lar cx, bx
	shr cx, 8
	mov ch, 40h
	jmp .common

.16:
	lar cx, bx
	shr cx, 8
.common:
	mov ax, 0009h
	int 31h
	pop ax
	pop cx
subcpureset
%endif
.ret:
	retn


	usesection lDEBUG_CODE
ia_restore_a_addr:
	mov di, a_addr
.common_u:
	mov si, ia_savedaddr
	movsw
	movsw
	movsw				; restore address
ia_restore_reset:
	mov word [ ia_restore ], ia_restore_none
ia_restore_none:
	retn

ia_restore_cseip_and_u_addr_and_es:
	mov ax, word [immasm_save_es]
	mov word [reg_es], ax
	jmp ia_restore_cseip_and_u_addr

ia_restore_cseip_and_u_addr_and_ds:
	mov ax, word [immasm_save_ds]
	mov word [reg_ds], ax
ia_restore_cseip_and_u_addr:
	mov di, reg_eip
	mov si, ia_savedcseip
	movsw
_386_PM	movsw
	mov di, reg_cs
	movsw

ia_restore_u_addr:
	mov di, u_addr
	jmp short ia_restore_a_addr.common_u

ia_restore_ds:
	mov ax, word [immasm_save_ds]
	mov word [reg_ds], ax
	jmp ia_restore_reset

ia_restore_es:
	mov ax, word [immasm_save_es]
	mov word [reg_es], ax
	jmp ia_restore_reset

	usesection lDEBUG_DATA_ENTRY

%if _IMMASM_AUXBUFF
immasm_length: equ _AUXBUFFSIZE
%else
	align 16, db 0
immasm_length: equ 32
immasm_buffer:		times immasm_length db 0
%endif
	align 2, db 0
ia_savedaddr:	dw 0,0,0		; a_addr or u_addr
ia_savedcseip:	dw 0,0,0		; CS:(E)IP
immasm_rel8_target:
immasm_rel1632_target:		; (not used at the same time)
		dd 0
immasm_far_target:
		dw 0,0,0
immasm_imm16:	dw 0
		dw 0		; allow loading as a dword
immasm_efl:	dd 0
immasm_ds:	dw 0
immasm_save_es:			; (not used at the same time)
immasm_save_ds:	dw 0
immasm_temp:	dw 0
immasm_callback:dw 0
ia_restore:	dw ia_restore_none	; called by cmd3
ia_sp:		dw 0
ia_exec:	db 0

	align 2, db 0
immasm_special_indices:
.:
	dw GROUP7 + 4		; jmp near r/m
	dw GROUP7 + 5		; jmp far r/m
	dw GROUP7 + 2		; call near r/m
	dw GROUP7 + 3		; call far r/m
	dw 00E8h		; call near imm
	dw 009Ah		; call far imm
	; dw 00EBh		; not needed: jmp short imm
	; dw 00E9h		; not needed: jmp near imm
	dw 00EAh		; jmp far imm
	dw 00C3h		; retn
	dw 00C2h		; retn imm
	dw 00CBh		; retf
	dw 00CAh		; retf imm
	dw 00CFh		; iret
	dw 008Eh		; mov to ds
	dw 008Ch		; mov from ds, cs
	dw 000Eh		; push cs
	dw 001Eh		; push ds
	dw 001Fh		; pop ds
	dw 00C5h		; lds
.amount equ ($ - .) / 2

%define IMMASMSPECIALMESSAGES db ""
%define IMMASMSPECIALFUNCTIONS db ""

%imacro immasm_special 2
 %xdefine IMMASMSPECIALFUNCTIONS IMMASMSPECIALFUNCTIONS, dw %1
 %xdefine IMMASMSPECIALMESSAGES IMMASMSPECIALMESSAGES, %%message:, {asciz %2}
	dw %%message
%endmacro
%imacro immasm_special_dump 1-*.nolist
 %rep %0
	%1
  %rotate 1
 %endrep
%endmacro
.names:
immasm_special .jnm, "jmp near m"
immasm_special .jfm, "jmp far m"
immasm_special .cnm, "call near m"
immasm_special .cfm, "call far m"
immasm_special .cni, "call near i"
immasm_special .cfi, "call far i"
immasm_special .jfi, "jmp far i"
immasm_special .rn, "retn"
immasm_special .rni, "retn i"
immasm_special .rf, "retf"
immasm_special .rfi, "retf i"
immasm_special .ir, "iret"
immasm_special .movto, "mov to ds"
immasm_special .movfrom, "mov from ds, cs"
immasm_special .pushcs, "push cs"
immasm_special .pushds, "push ds"
immasm_special .popds, "pop ds"
immasm_special .lds, "lds"
.names_amount: equ ($ - .names) / 2

%if .names_amount != .amount
 %error Table mismatch
%endif

.functions:
immasm_special_dump IMMASMSPECIALFUNCTIONS
.functions_amount: equ ($ - .functions) / 2

%if .functions_amount != .amount
 %error Table mismatch
%endif

immasm_special_dump IMMASMSPECIALMESSAGES


%if _DUALCODE
	usesection lDEBUG_CODE2
%else
	usesection lDEBUG_CODE
%endif
.jnm:
	mov al, byte [regmem]
	cmp al, 1100_0000b
	jb .jnm_m
	and ax, 7
	mov bx, ax
	add bx, bx
	mov bx, word [reg32addr + bx]
	mov dx, [bx + 2]
	mov ax, [bx]
.jnm_dxax:
	mov cx, word [reg_cs]
	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
	jmp @FF
@@:
	xor dx, dx
@@:
	nearcall hexword
	_386_PM_o32
	mov bx, word [reg_esp]
	retn

.jnm_m:
	testopt [disflags], DIS_I_SHOW
	jz error_mirror

	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	mov bx, [segrgaddr + bx]; get address of value
	mov es, [bx]

	_386_o32
	mov bx, word [addrr]
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
_386	mov dh, [es:ebx + 3]
_386	mov dl, [es:ebx + 2]
_386	mov ah, [es:ebx + 1]
_386	mov al, [es:ebx]
_386	jmp @FF

@@:
	mov dh, [es:bx + 3]
	mov dl, [es:bx + 2]
	mov ah, [es:bx + 1]
	mov al, [es:bx]
@@:
	push ss
	pop es
	jmp .jnm_dxax


.jfm:
	mov al, byte [regmem]
	cmp al, 1100_0000b
	jae error_mirror

.jfm_m:
	testopt [disflags], DIS_I_SHOW
	jz error_mirror

	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	mov bx, [segrgaddr + bx]; get address of value
	mov es, [bx]

	_386_o32
	mov bx, word [addrr]
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
_386	test byte [presizeflags], PRE32D
_386	jz .jfm_m_nocx_32
_386	mov ch, [es:ebx + 5]
_386	mov cl, [es:ebx + 4]
.jfm_m_nocx_32:
_386	mov dh, [es:ebx + 3]
_386	mov dl, [es:ebx + 2]
_386	mov ah, [es:ebx + 1]
_386	mov al, [es:ebx]
_386	jmp @FF

@@:
	test byte [presizeflags], PRE32D
	jz .jfm_m_nocx_16
	mov ch, [es:bx + 5]
	mov cl, [es:bx + 4]
.jfm_m_nocx_16:
	mov dh, [es:bx + 3]
	mov dl, [es:bx + 2]
	mov ah, [es:bx + 1]
	mov al, [es:bx]
@@:
	push ss
	pop es

	test byte [presizeflags], PRE32D
	jz @F
	push ax
	mov ax, cx
	nearcall hexword
	mov al, ':'
	stosb
	pop ax
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
	jmp @FF
@@:
	mov cx, dx
	push ax
	mov ax, cx
	nearcall hexword
	mov al, ':'
	stosb
	pop ax
	xor dx, dx
@@:
	nearcall hexword
	_386_PM_o32
	mov bx, word [reg_esp]
	retn


.cnm:
	call .jnm
.cni_common:
	push dx
	push cx
	push ax
	mov bx, word [reg_ss]
	mov es, bx
	mov dx, word [reg_eip + 2]
	mov ax, word [reg_eip]
%if _PM
	nearcall test_d_b_bit
	_386_o32
	mov bx, word [reg_esp]
	jz .cnm_16bitstack
subcpu 386
	test byte [presizeflags], PRE32D
	jz @F
	sub ebx, 2
	mov word [es:ebx], dx

@@:
	sub ebx, 2
	mov word [es:ebx], ax
	jmp .cnm_commonstack
subcpureset

.cnm_16bitstack:
%else
	mov bx, word [reg_esp]
%endif
	test byte [presizeflags], PRE32D
	jz @F
	sub bx, 2
	mov word [es:bx], dx

@@:
	sub bx, 2
	mov word [es:bx], ax
	jmp .cnm_commonstack

.cnm_commonstack:
	; _386_o32
	; mov word [reg_esp], bx

	push ss
	pop es

	xchg ax, si
	mov al, '='
	stosb
	xchg ax, si
	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
@@:
	nearcall hexword

	xchg ax, si
	mov al, '@'
	stosb
	xchg ax, si

	xchg ax, bx
	nearcall hexword
	xchg ax, bx

	pop ax
	pop cx
	pop dx

	testopt [ia_exec], 80h
	jz .cnm_retn
	mov word [immasm_callback], immasm_run_near
.cnm_retn:
	retn


.cfm:
	call .jfm
.cfi_common:
	push dx
	push cx
	push ax
	mov bx, word [reg_ss]
	mov es, bx
	mov bp, word [reg_cs]
	mov dx, word [reg_eip + 2]
	mov ax, word [reg_eip]
%if _PM
	nearcall test_d_b_bit
	_386_o32
	mov bx, word [reg_esp]
	jz .cfm_16bitstack
subcpu 386
	test byte [presizeflags], PRE32D
	jz @F
	sub ebx, 2
	and word [es:ebx], 0
@@:
	sub ebx, 2
	mov word [es:ebx], bp

	test byte [presizeflags], PRE32D
	jz @F
	sub ebx, 2
	mov word [es:ebx], dx
@@:
	sub ebx, 2
	mov word [es:ebx], ax
	jmp .cfm_commonstack
subcpureset

.cfm_16bitstack:
%else
	mov bx, word [reg_esp]
%endif
	test byte [presizeflags], PRE32D
	jz @F
	sub bx, 2
	and word [es:bx], 0
@@:
	sub bx, 2
	mov word [es:bx], bp

	test byte [presizeflags], PRE32D
	jz @F
	sub bx, 2
	mov word [es:bx], dx
@@:
	sub bx, 2
	mov word [es:bx], ax
	jmp .cfm_commonstack

.cfm_commonstack:
	; _386_o32
	; mov word [reg_esp], bx

	push ss
	pop es

	xchg ax, si
	mov al, '='
	stosb
	xchg ax, si
	xchg ax, bp
	nearcall hexword
	xchg ax, bp
	xchg ax, si
	mov al, ':'
	stosb
	xchg ax, si

	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
@@:
	nearcall hexword

	xchg ax, si
	mov al, '@'
	stosb
	xchg ax, si

	xchg ax, bx
	nearcall hexword
	xchg ax, bx

	pop ax
	pop cx
	pop dx
	testopt [ia_exec], 80h
	jz .cfm_retn
	mov word [immasm_callback], immasm_run_far
.cfm_retn:
	retn

.cni:
	mov dx, [immasm_rel1632_target + 2]
	mov ax, [immasm_rel1632_target]
	mov cx, word [reg_cs]
	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
	jmp @FF
@@:
	xor dx, dx
@@:
	nearcall hexword
	jmp .cni_common

.cfi:
	call .jfi
	jmp .cfi_common

.jfi:
	mov dx, [immasm_far_target + 2]
	mov ax, [immasm_far_target]
	mov cx, word [immasm_far_target + 4]

	push ax
	mov ax, cx
	nearcall hexword
	mov al, ':'
	stosb
	pop ax
	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
	jmp @FF
@@:
	xor dx, dx
@@:
	nearcall hexword
	_386_PM_o32
	mov bx, word [reg_esp]
	retn


.rn:
	mov cx, word [reg_cs]
	mov bx, word [reg_ss]
	mov es, bx
%if _PM
	nearcall test_d_b_bit
	_386_o32
	mov bx, word [reg_esp]
	jz .rn_16bitstack
subcpu 386
	mov ax, word [es:ebx]
	add ebx, 2
	xor dx, dx
	test byte [presizeflags], PRE32D
	jz @F
	mov dx, word [es:ebx]
	add ebx, 2
@@:
	jmp .rn_commonstack
subcpureset

.rn_16bitstack:
%else
	mov bx, word [reg_esp]
%endif
	mov ax, word [es:bx]
	add bx, 2
	xor dx, dx
	test byte [presizeflags], PRE32D
	jz @F
	mov dx, word [es:bx]
	add bx, 2
@@:
	jmp .rn_commonstack

.rn_commonstack:
	; _386_o32
	; mov word [reg_esp], bx

	push ss
	pop es

	xchg ax, si
	mov al, '='
	stosb
	xchg ax, si
	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
@@:
	nearcall hexword

	xchg ax, si
	mov al, '@'
	stosb
	xchg ax, si

	xchg ax, bx
	nearcall hexword
	xchg ax, bx

	retn


.rni:
	call .rn
.rfi_common:
%if _PM
	push bx
	mov bx, word [reg_ss]
	nearcall test_d_b_bit
	pop bx
	jz .rni_16
	_386_o32
%endif
.rni_16:
	add bx, word [immasm_imm16]
	; _386_o32
	; mov word [reg_esp], bx
	push ax
	mov al, ','
	stosb
	pop ax
	xchg ax, bx
	nearcall hexword
	xchg ax, bx
	retn

.rf:
	mov bx, word [reg_ss]
	mov es, bx
%if _PM
	nearcall test_d_b_bit
	_386_o32
	mov bx, word [reg_esp]
	jz .rf_16bitstack
subcpu 386
	mov ax, word [es:ebx]
	add ebx, 2
	xor dx, dx
	test byte [presizeflags], PRE32D
	jz @F
	mov dx, word [es:ebx]
	add ebx, 2
@@:
	mov cx, word [es:ebx]
	add ebx, 2
	test byte [presizeflags], PRE32D
	jz @F
	add ebx, 2
@@:
	jmp .rf_commonstack
subcpureset

.rf_16bitstack:
%else
	mov bx, word [reg_esp]
%endif
	mov ax, word [es:bx]
	add bx, 2
	xor dx, dx
	test byte [presizeflags], PRE32D
	jz @F
	mov dx, word [es:bx]
	add bx, 2
@@:
	mov cx, word [es:bx]
	add bx, 2
	test byte [presizeflags], PRE32D
	jz @F
	add bx, 2
@@:
	jmp .rf_commonstack

.rf_commonstack:
	; _386_o32
	; mov word [reg_esp], bx

	push ss
	pop es

	xchg ax, si
	mov al, '='
	stosb
	xchg ax, si
	xchg ax, cx
	nearcall hexword
	xchg ax, cx
	xchg ax, si
	mov al, ':'
	stosb
	xchg ax, si
	test byte [presizeflags], PRE32D
	jz @F
	xchg ax, dx
	nearcall hexword
	xchg ax, dx
@@:
	nearcall hexword

	xchg ax, si
	mov al, '@'
	stosb
	xchg ax, si

	xchg ax, bx
	nearcall hexword
	xchg ax, bx

	retn

.rfi:
	call .rf
	jmp .rfi_common


.ir:
	call .rf
	push ax
	push bx
	mov bx, word [reg_ss]
	mov es, bx
%if _PM
	nearcall test_d_b_bit
	pop bx
	jz .ir_16bitstack
subcpu 386
	mov ax, word [es:ebx]
	mov word [immasm_efl], ax
	add ebx, 2
	test byte [presizeflags], PRE32D
	jz @F
	mov ax, word [es:ebx]
	mov word [immasm_efl + 2], ax
	add ebx, 2
@@:
	jmp .ir_commonstack
subcpureset

.ir_16bitstack:
%else
	pop bx
%endif
	mov ax, word [es:bx]
	mov word [immasm_efl], ax
	add bx, 2
	test byte [presizeflags], PRE32D
	jz @F
	mov ax, word [es:bx]
	mov word [immasm_efl + 2], ax
	add bx, 2
@@:
	jmp .ir_commonstack

.ir_commonstack:
	; _386_o32
	; mov word [reg_esp], bx

	push ss
	pop es

	mov al, '^'
	stosb
	test byte [presizeflags], PRE32D
	jz @F
	mov ax, word [immasm_efl + 2]
	nearcall hexword
	mov al, '_'
	stosb
@@:
	mov ax, word [immasm_efl]
	nearcall hexword

	mov al, ','
	stosb
	pop ax
	xchg ax, bx
	nearcall hexword
	xchg ax, bx
	retn


.movto:
	mov al, byte [regmem]
	and al, 00_111_000b
	mov cl, 3
	shr al, cl
	cmp al, 3		; to ds ?
	jne .notspecial

	mov al, byte [regmem]
	cmp al, 1100_0000b
	jb .movto_mem
.movto_reg:
	and ax, 7
	mov bx, ax
	add bx, bx
	mov bx, word [reg32addr + bx]
	mov ax, [bx]
.movto_common:
	mov word [immasm_ds], ax
	nearcall ia_restore_ds
	jmp .noop

.movto_mem:
	testopt [disflags], DIS_I_SHOW
	jz error_mirror

	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	mov bx, [segrgaddr + bx]; get address of value
	mov es, [bx]

	_386_o32
	mov bx, word [addrr]
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
_386	mov ah, [es:ebx + 1]
_386	mov al, [es:ebx]
_386	jmp @FF

@@:
	mov ah, [es:bx + 1]
	mov al, [es:bx]
@@:
	push ss
	pop es
	jmp .movto_common

.notspecial:
	pop ax
	jmp ..@immasm_not_special


.movfrom:
	mov al, byte [regmem]
	and al, 00_111_000b
	mov cl, 3
	shr al, cl
	cmp al, 3		; to ds ?
	je .movfrom_ds
	cmp al, 1
	jne .notspecial
.movfrom_cs:
	mov cx, word [reg_cs]
	jmp @F

.movfrom_ds:
	mov cx, word [immasm_save_ds]
@@:
	mov word [immasm_temp], cx
	mov word [immasm_callback], .movfrom_callback
	jmp .noop

.movfrom_callback:
	mov al, byte [regmem]
	cmp al, 1100_0000b
	jb .movfrom_mem
.movfrom_reg:
	and ax, 7
	mov bx, ax
	add bx, bx
	mov bx, word [reg32addr + bx]
	mov ax, word [immasm_temp]
	mov [bx], ax
	test byte [presizeflags], PRE32D
	jz @F
	and word [bx + 2], 0
@@:
	retn

.movfrom_mem:
	testopt [disflags], DIS_I_SHOW
	jz error_mirror

	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	mov bx, [segrgaddr + bx]; get address of value
	mov es, [bx]

	_386_o32
	mov bx, word [addrr]
	mov ax, word [immasm_temp]
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
_386	mov [es:ebx + 1], ah
_386	mov [es:ebx], al
_386	jmp @FF

@@:
	mov [es:bx + 1], ah
	mov [es:bx], al
@@:
	push ss
	pop es
	retn


.pushcs:
	mov ax, word [reg_cs]
.push_common:
	mov bx, word [reg_ss]
	mov es, bx
%if _PM
	nearcall test_d_b_bit
	_386_o32
	mov bx, word [reg_esp]
	jz .push_16bitstack
subcpu 386
	test byte [presizeflags], PRE32D
	jz @F
	sub ebx, 2
	and word [es:ebx], 0

@@:
	sub ebx, 2
	mov word [es:ebx], ax
	jmp .push_commonstack
subcpureset

.push_16bitstack:
%else
	mov bx, word [reg_esp]
%endif
	test byte [presizeflags], PRE32D
	jz @F
	sub bx, 2
	and word [es:bx], 0

@@:
	sub bx, 2
	mov word [es:bx], ax
	jmp .push_commonstack

.push_commonstack:
	; _386_o32
	; mov word [reg_esp], bx

	push ss
	pop es
	jmp .noop_except_esp


.pushds:
	mov ax, word [immasm_save_ds]
	jmp .push_common


.popds:
	nearcall ia_restore_ds
	jmp .notspecial

.lds:
	cmp word [immasm_temp], 0
	je .notspecial

	nearcall ia_restore_ds
	mov byte [segmnt], 0
	mov ax, word [reg_es]
	mov word [immasm_save_es], ax
	mov word [ ia_restore ], ia_restore_es
	mov ax, word [reg_cs]
	mov word [reg_es], ax
	call getimmselseg
	mov es, bx
	push di
	xor di, di
	mov al, 3Eh
	mov cx, immasm_length
	repne scasb
	jne error_mirror
%if _PM
	nearcall verifysegm_or_error
	mov es, bx
%endif
	mov byte [es:di - 1], 26h
	pop di
	push ss
	pop es
	jmp .notspecial


.noop:
	_386_PM_o32
	mov bx, word [reg_esp]
.noop_except_esp:
	mov cx, word [reg_cs]
	mov dx, word [reg_eip + 2]
	mov ax, word [reg_eip]
	retn
