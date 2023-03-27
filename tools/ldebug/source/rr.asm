
%if 0

lDebug R commands - Register access

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

..@rr_access_start:
		; R command - dump and manipulate registers.
rr:
	call iseol?
	je dumpregs		; if no parameters -->

	call uppercase
	cmp al, 'E'
	jne @F

	push ax
	push si
	lodsb
	cmp al, '.'
	je re_cmd
	call iseol?
	pop si
	pop ax
	je dumpregs_extended
@@:

	cmp al, 'C'
	jne @F

	push ax
	push si
	lodsb
	cmp al, '.'
	je rc_cmd
	call iseol?
	pop si
	pop ax
	je rc_run
@@:

%if !_ONLYNON386 || (_OPTIONS || _VARIABLES) || _MMXSUPP || _RN
	dec si
	dec si
	mov dx, msg.rvv
	call isstring?
	je dumpallvars
	mov dx, msg.rvm
	call isstring?
	je dumpmemory
	mov dx, msg.rvp
	call isstring?
	je dumpprocess
	mov dx, msg.rvd
	call isstring?
	je dumpdevice
%if _MMXSUPP
	cmp byte [has_mmx], 0
	je .notrm
	mov dx, msg.rm
	call isstring?
	jne .notrm
	call skipcomma
	jmp dumpregsMMX
.notrm:
%endif
	inc si
	lodsb
	call uppercase

	push ax
	push si
	call skipwhite
	call iseol?		; line ends after single character ?
	pop si
	pop ax
	jne short rr1		; no, not other kinds of dumps -->

	cmp al, 'F'		; only valid input to a later check
	je rr2.writeprompt	; so go there -->
				; (note that byte [si-1] must != '.')
%if _OPTIONS || _VARIABLES
	cmp al, 'V'
	je dumpvars
%endif
_386	cmp al, 'X'
_386	je short rrx
%if _RN
	cmp al, 'N'
	jne .notrn
	cmp byte [has_87], 0
	je .notrn
	jmp dumpregsFPU
.notrn:
%endif
	jmp error		; all other single characters are invalid
%endif

%ifn _ONLYNON386
rrx:
	lodsb
	call chkeol
	xoropt [options], dispregs32
	mov dx, msg.regs386
	call putsz
	mov dx, msg.regs386_on
	testopt [options], dispregs32
	jnz .on
	mov dx, msg.regs386_off
.on:
	jmp putsz
%endif

rr1:
	lframe
	lvar dword, offset
	lvar word, segsel
	lvar word, memsizestring
	lvar word, ismem_high_size_low
	lequ ?ismem_high_size_low, size
	lequ ?ismem_high_size_low + 1, ismem
	lvar word, hhtype_high_rrtype_low
	lequ ?hhtype_high_rrtype_low, replace_rrtype
%define rrtype bp + ?replace_rrtype
	lequ ?hhtype_high_rrtype_low + 1, hhtype
	lvar word, replace_rrmask
%define rrmask bp + ?replace_rrmask
	lvar word, hhoffset
%if _MMXSUPP
	lvar word, mmx_isvar_type
%endif
	lequ 16, namebufferlength
	lvar ?namebufferlength, namebuffer
	lenter
%if _MMXSUPP
	and word [bp + ?mmx_isvar_type], 0
%endif
	mov di, sp		; -> ?namebuffer
	call isvariable?.return_name
	jnc rr1_variable

	dec si
	mov dx, msg.byte
	mov bx, 101h
	push dx
	call isstring?
	je rr1_memory
	pop dx
	mov dx, msg.word
	inc bx			; = 102h
	push dx
	call isstring?
	je rr1_memory
	pop dx
	mov dx, msg.3byte
	inc bx			; = 103h
	push dx
	call isstring?
	je rr1_memory
	pop dx
	mov dx, msg.dword
	inc bx			; = 104h
	push dx
	call isstring?
	je rr1_memory
	; pop dx
	lleave code
	lodsb
	jmp rr2

rr1_memory:
	pop word [bp + ?memsizestring]
	mov word [bp + ?ismem_high_size_low], bx

	xor ax, ax
	mov al, bl
	neg ax
	add ax, masks + 4	; 4 - size = offset into masks
	mov word [rrmask], ax	; -> mask
	call skipcomma
	cmp al, '['
	jne error
	lodsb
	mov bx, word [reg_ds]
	call getaddrX
	mov word [bp + ?segsel], bx
	_386_PM_o32
	mov word [bp + ?offset], dx
	call skipcomm0
	cmp al, ']'
	jne error

	call skipcomma
	cmp al, '.'		; special ?
	je short .writeprompt	; yes -->
	call iseol?
	jne short .noprompt
.writeprompt:
				; si -> behind dot if any
	mov di, line_out
	push si
	mov si, word [bp + ?memsizestring]
				; si -> size string (ASCIZ)
	db __TEST_IMM8		; (skip stosb)
@@:
	stosb			; store next byte
	lodsb			; load next byte
	test al, al		; is zero ?
	jnz @B			; not yet, loop -->
	pop si			; (preserve si)
	mov ax, " ["
	stosw
	mov ax, word [bp + ?segsel]
	mov bx, ax
	call hexword
	mov al, ':'
	stosb
	_386_PM_o32
	mov ax, word [bp + ?offset]
%if _PM
	call test_high_limit	; 32-bit segment ?
	jz .16
	call hexword_high
.16:
%endif
	call hexword
	mov ax, "] "
	stosw

	call prephack
	call rr1_read_mem_dxax

	cmp cl, 4
	jb .pnohigh
	xchg ax, dx
	call hexword		; display high word
	xchg ax, dx
	jmp @F
.pnohigh:
	cmp cl, 3
	jb @F
	xchg al, dl
	call hexbyte
	xchg al, dl
@@:
	cmp cl, 2
	jb .pnobyte
	xchg al, ah
	call hexbyte		; display high byte
	xchg al, ah
.pnobyte:
	call hexbyte		; display low byte

	call dot_prompt
	je .return
.noprompt:
	xor cx, cx
	mov cl, byte [bp + ?size]
	jmp rr1_common


..@rr_variable_read_access_start:
		; OUT:	cl = ?size
		;	dx:ax = value read from memory
		; CHG:	bx
rr1_read_mem_dxax:
	call dohack
	xor ax, ax
	xor dx, dx
	mov cl, byte [bp + ?size]
%if _PM
	mov bx, word [bp + ?segsel]
	call test_high_limit	; 32-bit segment ?
	mov ds, bx
%else
	mov ds, word [bp + ?segsel]
%endif
	_386_PM_o32
	mov bx, word [bp + ?offset]
%if _PM
	jz .16

[cpu 386]
.32:
	cmp cl, 2
	jb .32_1
	je .32_2
	cmp cl, 4
	jb .32_3
.32_4:
	mov dh, byte [ebx + 3]
.32_3:
	mov dl, byte [ebx + 2]
.32_2:
	mov ah, byte [ebx + 1]
.32_1:
	mov al, byte [ebx]
	jmp .ret
__CPU__
%endif

.16:
	cmp cl, 2
	jb .16_1
	je .16_2
	cmp cl, 4
	jb .16_3
.16_4:
	mov dh, byte [bx + 3]
.16_3:
	mov dl, byte [bx + 2]
.16_2:
	mov ah, byte [bx + 1]
.16_1:
	mov al, byte [bx]
.ret:
	push ss
	pop ds
	jmp unhack
..@rr_variable_read_access_end:


rr1_variable:
	mov word [rrmask], di	; -> mask of read-only bits
	mov al, cl
	mov cl, 0
	dec si			; (to reload al)
	 push ax		; h = variable's field type, l = its size
	mov ah, 0
	mov word [bp + ?ismem_high_size_low], ax
	xchg cl, ch		; cx = variable's name's length

	call skipcomma
	cmp al, '.'		; special ?
	je short .writeprompt	; yes -->
	call iseol?
	jne short .noprompt
.writeprompt:
	 push si		; -> behind dot if any
	lea si, [bp + ?namebuffer]
				; -> name
	mov di, line_out
	rep movsb
	 pop si			; -> behind dot if any
	mov al, 32
	stosb
	 pop cx 		; h = variable's field type, l = its size
	xchg bx, dx
	mov ax, word [bx]
	xchg bx, dx
	 push cx
	cmp cl, 4
	jb .pnohigh
	call hexword		; display high word
	jmp @F
.pnohigh:
	cmp cl, 3
	jb @F
	call hexbyte
@@:
	mov ax, word [bx]
	cmp cl, 2
	jb .pnobyte
	xchg al, ah
	call hexbyte		; display high byte
	xchg al, ah
.pnobyte:
	call hexbyte		; display low byte
	call dot_prompt
	 pop cx			; h = variable's field type, l = its size
	je .return
	db __TEST_IMM8		; (skip pop)
.noprompt:
	 pop cx			; h = variable's field type, l = its size
%if _MMXSUPP
	mov word [bp + ?mmx_isvar_type], cx
%endif
	test ch, ch
	jnz rr1_readonly_or_mmx
rr1_common_do_mmx:
	 push bx
	 push dx
rr1_common:
	mov byte [rrtype], cl
	push cx
	push ax
	push si
	call isoperator?
	jne .nooperator
	mov bx, cx
	add bx, bx		; bh = 0 !
	push ax
	call near [operatordispatchers+bx]
	pop ax
	test bx, bx
	jnz .gotoperator
.nooperator:
	mov bx, OPERATOR_RIGHTOP; set default computation function
	db __TEST_IMM8
.gotoperator:
	lodsb

	call isassignmentoperator?
	jnc .assign_op_done	; found an assignment operator -->
	cmp bx, OPERATOR_RIGHTOP; dummy (no operator specified) ?
	je .assign_op_done	; yes, assignment operator not required -->
	pop cx
	pop ax			; restore al, si in front of operator
	push ax
	push cx
	xchg si, cx
	push cx
	call isunaryoperator?	; is this a valid unary operator too ?
	pop cx
	je .nooperator		; yes -->
	xchg si, cx
errorj9: equ $
	jmp error		; error where the assignment operator needs to be
.assign_op_done:
	pop cx
	pop cx
	pop cx
	 push bx
	cmp bl, OPERATOR_COND
	je error
	call skipcomm0

	call getexpression	; bx:dx = expression result
	mov byte [bp + ?hhtype], ah
	mov word [bp + ?hhoffset], si
	call chkeol
	pop cx			; operator computation function

	cmp byte [bp + ?ismem], 0
	je .var_hhvar

.mem_hhvar:
	mov di, ax
	mov si, dx
	push bx
	push cx
	call rr1_read_mem_dxax
	pop cx
	pop bx
	push dx
	push ax
	mov ax, di
	mov dx, si
	mov di, mask_4byte	; di -> zero
	mov si, mask_4byte + 2	; si -> zero

	jmp .common_hhvar

.var_hhvar:
	pop si			; si-> high word
	pop di			; di-> low word
	push word [si]
	push word [di]

.common_hhvar:
	pop word [hhvar]
	pop word [hhvar+2]	; save variable's current value (as left operand)

	mov byte [hhtype], 0	; type info signed=0 pointer=0
	cmp byte [rrtype], 4
	jae .cleardword
	cmp byte [rrtype], 2
	ja .clearthreebyte
	je .clearword
.clearbyte:
	mov byte [hhvar + 1], 0	; clear second byte
.clearword:
	mov byte [hhvar + 2], 0	; clear third byte
.clearthreebyte:
	mov byte [hhvar + 3], 0	; clear fourth byte
.cleardword:
	xchg cx, bx
	add bx, bx
	mov bx, word [bx + operatorfunctions]
	xchg cx, bx		; cx = operator function
	mov ah, byte [bp + ?hhtype]
	call cx			; compute

	call getexpression.countsignificantbits
	push bx
	push si
	xor bx, bx
	mov bl, byte [rrtype]
	add bx, bx
	mov si, word [bp + ?hhoffset]
	call near word [checksignificantbits_table + bx]
	pop si
	pop bx

		xchg ax, di	; ax -> low word of variable
	mov di, word [rrmask]	; di -> mask dword
	 push bx		; use bx as a scratch space
	mov cx, word [di]	; = low word of mask
	  push cx		; preserve low word of mask
		xchg ax, di	; di -> low word of variable
	and cx, word [di]	; cx = low word value to preserve
	  pop bx		; = low word of mask
	not bx			; = bitmask of bits to use from result
	and dx, bx		; dx = bits to use from result low word
	or dx, cx		; dx = low word compound
	 pop bx			; restore high word after scratch use
		xchg ax, di	; di -> mask dword
	 push dx		; use dx as a scratch space
	mov cx, word [di + 2]	; = high word of mask
	  push cx		; preserve high word of mask
	and cx, word [si]	; cx = high word value to preserve
	  pop dx		; = high word of mask
	not dx			; = bitmask of bits to use from result
	and bx, dx		; bx = bits to use from result high word
	or bx, cx
	 pop dx			; restore low word after scratch use
		xchg ax, di	; di -> low word of variable

%if _MMXSUPP
	cmp byte [bp + ?mmx_isvar_type + 1], 0
	jne rr1_mmx_set
%endif
	cmp byte [bp + ?ismem], 0
	jne rr1_mem_set

.var_set:
	cmp byte [rrtype], 2
	jb .setbyte
	je .setword
	cmp byte [rrtype], 4
	jb .setthreebyte
.setdword:
	mov byte [si + 1], bh	; set fourth byte
.setthreebyte:
	mov byte [si], bl	; set third byte
.setword:
	mov byte [di + 1], dh	; set second byte
.setbyte:
	mov byte [di], dl	; set first byte
.return:
rr1_memory.return:
rr1_variable.return:
	lleave code
rr1b:
	retn


rr1_readonly_or_mmx:
%if _MMXSUPP
	cmp ch, 1
	je rr1_readonly
	cmp byte [has_mmx], 0
	jne rr1_common_do_mmx
	mov dx, msg.internal_error_no_mmx
	jmp rr1_readonly.error
%endif

rr1_readonly:
	mov dx, msg.readonly
.error:
	call putsz_error
	jmp rr1_variable.return


..@rr_variable_write_access_start:
rr1_mem_set:
	mov ax, dx
	mov dx, bx

	call dohack
	mov cl, byte [bp + ?size]
%if _PM
	mov bx, word [bp + ?segsel]
	call verifysegm
	jc .ro
	call test_high_limit	; 32-bit segment ?
	mov ds, bx
%else
	mov ds, word [bp + ?segsel]
%endif
	_386_PM_o32
	mov bx, word [bp + ?offset]
%if _PM
	jz .16

[cpu 386]
.32:
	cmp cl, 2
	jb .32_1
	je .32_2
	cmp cl, 4
	jb .32_3
.32_4:
	mov byte [ebx + 3], dh
.32_3:
	mov byte [ebx + 2], dl
.32_2:
	mov byte [ebx + 1], ah
.32_1:
	mov byte [ebx], al

.32_check:
	cmp cl, 2
	jb .32_check_1
	je .32_check_2
	cmp cl, 4
	jb .32_check_3
.32_check_4:
	cmp dh, byte [ebx + 3]
	jne .ro
.32_check_3:
	cmp dl, byte [ebx + 2]
	jne .ro
.32_check_2:
	cmp ah, byte [ebx + 1]
	jne .ro
.32_check_1:
	cmp al, byte [ebx]
	jne .ro
	jmp .ret
__CPU__
%endif

.16:
	cmp cl, 2
	jb .16_1
	je .16_2
	cmp cl, 4
	jb .16_3
.16_4:
	mov byte [bx + 3], dh
.16_3:
	mov byte [bx + 2], dl
.16_2:
	mov byte [bx + 1], ah
.16_1:
	mov byte [bx], al

.16_check:
	cmp cl, 2
	jb .16_check_1
	je .16_check_2
	cmp cl, 4
	jb .16_check_3
.16_check_4:
	cmp dh, byte [bx + 3]
	jne .ro
.16_check_3:
	cmp dl, byte [bx + 2]
	jne .ro
.16_check_2:
	cmp ah, byte [bx + 1]
	jne .ro
.16_check_1:
	cmp al, byte [bx]
	jne .ro
.ret:
	call ee0a
rr1_memory.return_j1: equ $
	jmp rr1_memory.return

.ro:
	call ee0a
	mov dx, msg.readonly_mem
	call putsz_error
	jmp short rr1_memory.return_j1
..@rr_variable_write_access_end:

%if _MMXSUPP
		; INP:	bx:dx = number
		;	byte [bp + ?mmx_isvar_type] = type/register
rr1_mmx_set:
subcpu 586
	mov al, byte [bp + ?mmx_isvar_type + 1]
	dec ax
	dec ax			; undo encoding for isvar type return
	mov cx, bx		; cx:dx = number
	mov bl, al
	and bl, 7		; = register number
	and ax, 11_000b		; = type of write
	shr ax, 2		; = index into table

 %if _PM
	push dx
	mov dx, word [code_seg]
	call setes2dx		; es => lDEBUG_CODE (writable)
	pop dx
%else
	mov es, word [code_seg]	; es => lDEBUG_CODE
%endif
	shl bl, 3		; shift into reg field
	or bl, 07h		; code to get our ModR/M byte (r/m = [bx])
	mov byte [es:.getmmx_modrm], bl
				; SMC in section lDEBUG_CODE
	mov byte [es:.setmmx_modrm], bl
				; SMC in section lDEBUG_CODE
	jmp @F			; try to invalidate prefetch
@@:
	push ss
	pop es
	sub sp, 8
	mov bx, sp
.getmmx_modrm: equ $+2	; (opcode adjusted for the right MMX reg)
	movq qword [bx], mm0

	xchg ax, di
	xor ax, ax		; = 0
	call near [mmx_set_table + di]

.setmmx_modrm: equ $+2	; (opcode adjusted for the right MMX reg)
	movq mm0, qword [bx]
	add sp, 8
	jmp short rr1_memory.return_j1

.signextend:
	test cx, cx
	jns .zeroextend
	dec ax			; = -1

.zeroextend:
	mov word [bx + 4], ax
	mov word [bx + 6], ax
	;; jmp short .lowonly
	; fall through

.lowonly:
	mov word [bx], dx
	mov word [bx + 2], cx
	retn

.highonly:
	mov word [bx + 4], dx
	mov word [bx + 6], cx
	retn

subcpureset
%endif


	lleave ctx


	usesection lDEBUG_DATA_ENTRY
	align 2, db 0
checksignificantbits_table:
	dw error
	dw getbyte.checksignificantbits
	dw getword.checksignificantbits
	dw get3byte.checksignificantbits
	dw dmycmd

%if _MMXSUPP
	align 2, db 0
mmx_set_table:
		;	  0 zero extension from 32 bits to write all 64 bits
		;	  1 sign extension from 32 bits to write all 64 bits
		;	  2 writes only low 32 bits
		;	  3 writes only high 32 bits
	dw rr1_mmx_set.zeroextend
	dw rr1_mmx_set.signextend
	dw rr1_mmx_set.lowonly
	dw rr1_mmx_set.highonly
%endif

	usesection lDEBUG_CODE


		; Change flag register with mnemonics - F
rr2:
	call uppercase
	cmp al, 'F'
	jne rr3			; if not 'F' -->
	push ax
	mov al, byte [si]
	call isseparator?	; valid separator ?
	pop ax
	jne rr3			; no -->
.ef:
	call skipcomma
	cmp al, '.'		; special ?
	je .writeprompt		; yes -->
	call iseol?		; end of line ?
	jne .noprompt		; no -->
.writeprompt:
	mov di, line_out
%if _REGSHIGHLIGHT
	setopt [internalflags3], dif3_do_not_highlight
%endif
	push si
	call dmpflags
	pop si			; -> behind dot if any
	call dot_prompt
	je rr1b			; if no change
.noprompt:

	call isassignmentoperator?
	push si
	jnc .noteol		; at least one value is required -->
.check_loop:
	call skipcomm0
	call iseol?
	je .really		; return if done
.noteol:
	call uppercase
	xchg al, ah
	lodsb
	call uppercase
	xchg al, ah		; ax = mnemonic
	mov di, flagson
	mov cx, 16
	repne scasw
	jne short .errordec	; if no match
	lodsb
	call isseparator?
	je .check_loop
.errordec2:
	dec si
.errordec:
errordec: equ $
	dec si			; back up one before flagging an error
	jmp error

.really:
	pop si
	dec si
	lodsb
.loop:
	call iseol?
	je rr1b			; return if done

	call uppercase
	xchg al, ah
	lodsb
	call uppercase
	xchg al, ah		; ax = mnemonic

	mov di, flagson
	mov cx, 16
	repne scasw
	jne short .errordec	; if no match
	cmp di, flagsoff
	ja .clear		; if we're clearing
	mov ax, word [di-(flagson-flagbits)-2]
	or word [reg_efl], ax	; set the bit
	jmp short .common
.clear:
	mov ax, word [di-(flagsoff-flagbits)-2]
	not ax
	and word [reg_efl], ax	; clear the bit
.common:
	lodsb
	call isseparator?
	jne short .errordec2
	call skipcomm0
	jmp short .loop

		; Change flag register with mnemonics - EF
rr3:
	xchg al, ah
	lodsb
	call uppercase
	xchg al, ah		; ax = next two characters
_386	cmp ax, "EF"
_386	jne rr4			; if not "EF" -->
_386	push ax
_386	mov al, byte [si]
_386	call isseparator?	; valid separator ?
_386	pop ax
_386	je rr2.ef

		; Change a single flag with mnemonic
rr4:
	mov di, flagnames
	mov cx, 8
	repne scasw
	jne short rr2.errordec
	mov dx, ax
	lodsb
	call isseparator?
	jne short rr2.errordec2
	push word [di-(flagnames-flagbits)-2]
	call skipcomm0
	cmp al, '.'		; special ?
	je .writeprompt		; yes -->
	call iseol?
	jne .noprompt
.writeprompt:
	mov di, line_out
	mov ax, dx
	stosw
	pop ax
	push ax
	test word [reg_efl], ax	; is it off ?
	mov ax, " 0"		; assume so
	jz .off			; it is off -->
	inc ah			; is on, set to '1'
.off:
	stosw
	call dot_prompt
	je .ret_pop		; if no change -->
.noprompt:
	call iseol?		; end of line ?
	je .ret_pop		; yes, no change requested -->
	push cx
	push ax
	push si
	call isoperator?
	jne .nooperator
	mov bx, cx
	add bx, bx		; bh = 0 !
	push ax
	call near [operatordispatchers+bx]
	pop ax
	test bx, bx
	jz .nooperator
	cmp bl, OPERATOR_BOOL_AND
	ja .nooperator
	cmp bl, OPERATOR_BOOL_OR
	jae .gotoperator
	add bl, OPERATOR_BOOL_OR - OPERATOR_BIT_OR
	cmp bl, OPERATOR_BOOL_OR
	jae .gotoperator
.nooperator:
	mov bx, OPERATOR_RIGHTOP; set default computation function
	db __TEST_IMM8
.gotoperator:
	lodsb

	call isassignmentoperator?
	jnc .assign_op_done	; found an assignment operator -->
	cmp bx, OPERATOR_RIGHTOP; dummy (no operator specified) ?
%if 1	; since | ^ & are never unary operators
	jne error
%else
	je .assign_op_done	; yes, assignment operator not required -->
	pop cx
	pop ax			; restore al, si in front of operator
	push ax
	push cx
	xchg si, cx
	push cx
	call isunaryoperator?	; is this a valid unary operator too ?
	pop cx
	je .nooperator		; yes -->
	xchg si, cx
	jmp error		; error where the assignment operator needs to be
%endif
.assign_op_done:
	pop cx
	pop cx
	pop cx
	 push bx
	call getexpression
	call chkeol
	call toboolean
	 pop cx			; operator index
	xor ax, ax
	mov byte [hhtype], al
	mov word [hhvar+2], ax
	 pop si
	 push si		; flag
	test word [reg_efl], si
	jz .notset
	inc ax
.notset:
	mov word [hhvar], ax
	xchg cx, bx
	add bx, bx
	mov bx, word [bx+operatorfunctions]
	xchg cx, bx		; cx = operator function
	call cx			; compute
	pop ax
	test dx, dx
	jz .clear
	or word [reg_efl], ax	; set the bit
	retn
.clear:
	not ax
	and word [reg_efl], ax	; clear the bit
	retn

.ret_pop:
	pop ax
	retn


		; INP:	di-> behind prompt to display (in line_out)
		;	Iff byte[si-1] == '.',
		;	 only display without actual prompting
		;	 si-> line to check for EOL
		; OUT:	NZ iff actually prompted and got non-empty line,
		;	 al = first character
		;	 si-> next character
		;	 dx, bx preserved
		; CHG:	ax, cx, si, di, dx, bx
dot_prompt:
	cmp byte [si-1], '.'	; syntax for display without prompt ?
	je .onlydisplay		; yes -->
	push bx
	push dx
	call getline0
	pop dx
	pop bx
	call iseol?		; no change requested ?
	je .ret			; yes --> (ZR)
	cmp al, '.'		; other syntax for no change ?
	jne .ret		; no --> (NZ)
.chkeol1:
	lodsb
	jmp chkeol		; (ZR)

.onlydisplay:
	call .chkeol1
	call putsline_crlf
	cmp al, al		; ZR
.ret:
	retn


		; INP:	al = first character
		;	si-> remaining string
		; OUT:	CY if no assignment operator was found
		;	NC if an assignment operator was found,
		;	 al = first character behind it (skipcomma called)
		;	 si-> remaining string behind character al
isassignmentoperator?:
	cmp al, ':'
	jne .checksingleequality
	lodsb
	cmp al, '='
	je .skip		; long form assignment operator -->
		; A single colon. Report "no assignment operator" here.
	dec si
	mov al, ':'		; restore si, al
.return_cy:
	stc
	retn

.checksingleequality:
	cmp al, '='
	jne .return_cy		; no assignment operator -->
.skip:
	call skipcomma
	clc
	retn


rc_cmd:
	mov di, cmdline_buffer
	mov bp, cmdline_buffer.end
	jmp @F

		; INP:	al = '.'
		;	si -> next character
re_cmd:
	mov di, re_buffer
	mov bp, re_buffer.end
@@:
	pop dx
	pop dx			; discard si and ax on stack
	mov dx, msg.list
	call isstring?
	je .list

	cmp di, re_buffer
	je @F
	call guard_rc
	jmp @FF

@@:
	call guard_re
@@:

	mov dx, msg.append
	call isstring?
	je .append
	mov dx, msg.replace
	call isstring?
	je .common		; di -> at first byte (where to append)
	mov ax, 0104h
	jmp .error_common

.append:
	xor al, al
	mov cx, -1
	repne scasb		; di -> after zero byte
	dec di			; -> at zero byte

.common:
	call skipwhite
	call guard_auxbuff

	xor bx, bx

	push ax
	mov al, 13
	cmp byte [di - 1], al	; is there an EOL in front of us?
	mov es, word [auxbuff_segorsel]
	xchg bx, di		; es:di -> auxbuff, bx -> at zero byte
	je @F			; yes -->

	stosb			; first store a CR
@@:
	pop ax
.loop:
	cmp al, '\'
	jne .literal

	lodsb			; load escaped character
	call iseol?.notsemicolon; EOL ?
	je .error_escaped_cr	; yes, error -->
	stosb			; store escaped literal
	lodsb			; load next
	jmp .loop

.literal:
	call iseol?.notsemicolon; EOL ?
	je .end			; got all -->
	cmp al, ';'		; semicolon ?
	jne @F
	mov al, 13
	stosb			; store linebreak
	call skipwhite		; skip leading blanks
	jmp .loop

@@:
	stosb			; store character
	lodsb
	jmp .loop

.end:
		; di -> behind last character
	xor ax, ax
	stosb
	mov ax, bx
	add ax, di
	jc .error_too_much
	cmp ax, bp
	ja .error_too_much
	xor si, si
	push es
	pop ds
	push ss
	pop es
	mov cx, di
	mov di, bx
	rep movsb
	push ss
	pop ds
	retn

.error_escaped_cr:
	mov ax, 0105h
	jmp .error_common

.error_too_much:
	mov ax, 0106h

.error_common:
	call setrc
	jmp error

re_cmd.list:
	lodsb
	call chkeol

	mov si, di
	xor ax, ax		; ah = 0 (flag to escape blanks)
%if _40COLUMNS
	usesection lDEBUG_DATA_ENTRY
	align 2, db 0
.lastfragmentlength:
	dw 0

	usesection lDEBUG_CODE
	mov word [.lastfragmentlength], ax
				; init to zero
%endif
	mov di, line_out	; write to line_out
		; Note that we cannot depend on line_out being
		;  large enough for every command because we
		;  want to escape initial blanks and all quote
		;  marks as well as backslashes. However, the
		;  quote marks can be entered without escapes
		;  in Rx.APPEND/.REPLACE so line_out may not be
		;  large enough to hold a whole command.
		; Previously we called putsline in the .cr branch
		;  but this is not needed if several short commands
		;  are displayed. The necessary check in .put does
		;  suffice to handle a filled buffer.
.loop:
	lodsb
	test al, al
	jz .end
	cmp al, 13		; (intentionally not iseol?)
	je .cr
	cmp al, 32
	je .escapeif
	cmp al, 9
	je .escapeif
	mov ah, 1		; ah = 1
	cmp al, ';'
	je .escape
	cmp al, '"'
	je .escape
	cmp al, "'"
	je .escape
	cmp al, '\'
	je .escape
.put:
		; Check that we are within available buffer space.
		;  Is it enough for 1 more codepoint plus an escape
		;  backslash for the .escape branch?
		;  Is it also enough for 1 more codepoint plus the
		;  semicolon for the .cr branch?
		; The 4 is an exaggeration.
	cmp di, line_out_end - 4
	jb @F			; yes -->
%if _40COLUMNS
	push ax
	mov ax, word [.lastfragmentlength]
	call putsline_break_line
	mov word [.lastfragmentlength], ax
		; We cheat: At this point we know that we always
		;  want to display more so we can call the _more
		;  function unconditionally here.
	call puts_break_line_more
	pop ax
%else
	call putsline
%endif
	mov di, line_out
@@:
	stosb
	jmp .loop

.escapeif:
	test ah, ah
	jnz .put
	mov ah, 1		; ah = 1
.escape:
	push ax
	mov al, '\'
	stosb			; (always have space)
	pop ax
	jmp .put		; checks for buffer space

.cr:
	lodsb
	test al, al
	jz .end
	dec si
	mov al, ';'
	stosb			; (always have space)
	mov ax, 32		; ah = 0
	jmp .put		; checks for buffer space

.end:
%if _40COLUMNS
	mov ax, word [.lastfragmentlength]
	call putsline_break_line
%else
	call putsline
%endif
	mov dx, crlf
	jmp putsz


rc_run:
	call guard_rc
	call guard_re		; do not allow RE command to run RC
				;  (the RE buffer would drain first)

	call yy_reset_buf

	mov word [cmdline_buffer.position], cmdline_buffer
	and word [rc_count], 0
	and word [rc_count + 2], 0

	setopt [internalflags3], dif3_input_cmdline

	retn


dumpregs_extended:
	call guard_re
	cmp word [re_buffer], "@R"
	je @F
	cmp word [re_buffer], "@r"
	jne .complex
@@:
	cmp byte [re_buffer + 2], 0
	je .just_dumpregs

.complex:
	testopt [internalflags3], dif3_auxbuff_guarded_2
	jz @F

	mov ax, 0103h
	call setrc
	mov dx, msg.unexpected_auxbuff_guard
	jmp putsz

@@:
%if _SYMBOLIC
	testopt [internalflags3], dif3_nosymbols_2
	jz @F

	mov ax, 0103h
	call setrc
	mov dx, msg.unexpected_nosymbols
	jmp putsz

@@:
%endif
	call yy_reset_buf

	mov word [re_buffer.position], re_buffer
	and word [re_count], 0
	and word [re_count + 2], 0

	push word [rc]
	push word [savesp]
	push word [throwsp]
	push word [lastcmd]
	push bp
	mov word [savesp], sp
	mov word [throwsp], sp
	mov word [re_sp], sp

	setopt [internalflags3], dif3_input_re

	testopt [internalflags3], dif3_auxbuff_guarded_1
	jz @F
	xoropt [internalflags3], dif3_auxbuff_guarded_1 | dif3_auxbuff_guarded_2
@@:
%if _SYMBOLIC
	testopt [internalflags3], dif3_nosymbols_1
	jz @F
	xoropt [internalflags3], dif3_nosymbols_1 | dif3_nosymbols_2
@@:
%endif
.cmd3:
	jmp cmd3

.exit:
	mov sp, word [re_sp]
	pop bp
	pop word [lastcmd]
	pop word [throwsp]
	pop word [savesp]
	pop ax
	test ax, ax
	jz @F
	mov word [rc], ax
@@:
	clropt [internalflags3], dif3_input_re

	testopt [internalflags3], dif3_auxbuff_guarded_2
	jz @F
	xoropt [internalflags3], dif3_auxbuff_guarded_1 | dif3_auxbuff_guarded_2
@@:
%if _SYMBOLIC
	testopt [internalflags3], dif3_nosymbols_2
	jz @F
	xoropt [internalflags3], dif3_nosymbols_1 | dif3_nosymbols_2
@@:
%endif

	testopt [options2], opt2_re_cancel_tpg
	jnz @F

	retn

@@:
	call terminate_silent_dump.if_nonnull
	jmp .cmd3


.just_dumpregs:
		; DUMPREGS - Dump registers.
		;
		; 16 bit: 8 regs, line break, first 4 segment regs,
		;	IP, flags
		; 32 bit: 6 regs, line break, 2 regs, flags, line break,
		;	6 segment regs, EIP
		; 16 bit / 40-column mode: 5 regs, line break, 4 segment regs,
		;	SP, line break, IP, SI, DI, shorter flags display
		; CHG:	ax, bx, cx, dx, di, si
dumpregs:
%if _IMMASM
	call dumpregs_no_disasm
	jmp dumpregs_disasm


dumpregs_no_disasm:
%endif
%if _REGSHIGHLIGHT
	testopt [options3], opt3_r_highlight_eip
	jnz @F
	mov ax, word [reg_eip]
	mov word [reg_eip - regs + regs_prior], ax
%if _PM
	mov ax, word [reg_eip + 2]
	mov word [reg_eip + 2 - regs + regs_prior], ax
%endif
@@:
%endif

	mov si, reg16names
	mov di, line_out
	mov cx, 8			; display all 8 standard regs (16-bit)
	testopt [options], dispregs32
	jz .firstrow16
	mov cl, 6			; room for 6 standard regs (32-bit) only
%if _40COLUMNS
	jmp .firstrow_not40
%endif
.firstrow16:
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz .firstrow_not40
	mov cl, 4
	push di
	call dmpr1			; ax, bx, cx, dx
	inc si
	inc si				; skip sp
	inc cx				; = 1
	call dmpr1			; bp
	call trimputs
	pop di
	push di
	mov si, reg16names + 11 * 2	; cs
	mov cx, 1
	call dmpr1
	mov si, reg16names + 8 * 2	; ds, es, ss
	mov cl, 3
	call dmpr1
	mov si, reg16names + 4 * 2	; sp
	inc cx
	call dmpr1
	call trimputs
	pop di
	mov si, reg16names + 14 * 2	; ip
	mov cx, 1
	call dmpr1
	mov si, reg16names + 6 * 2	; si, di
	mov cl, 2
	call dmpr1
	call dmpshortflags
	call dmpflags.40
	jmp .lastrowdone

%endif
.firstrow_not40:
	pushf
	push di
	call dmpr1			; display first row
	call trimputs
	pop di				; (reset di)
	popf				; (reset ZF)
	jnz .secondrow32
	mov cl, 4			; display 4 segment regs
	call dmpr1
	add si, byte 2*2		; skip FS+GS
	inc cx				; (= 1)
	call dmpr1			; display IP
	call dmpflags			; display flags in 16-bit display
	jmp short .lastrowdone
.secondrow32:
	push di
	mov cl, 2			; display rest of 32-bit standard regs
	call dmpr1
	push si
	call dmpflags			; display flags in 32-bit display
	call putsline_crlf
	pop si
	pop di				; (reset di)
	mov cl, 6			; display all segment registers
	call dmpr1
	inc cx				; (= 1)
	call dmpr1			; display EIP
.lastrowdone:
	call trimputs

%if _REGSHIGHLIGHT
	mov si, regs
	mov di, regs_prior
	mov cx, words(regs_prior.size)
	rep movsw			; update prior regs save area
%endif
%if _IMMASM
	retn


dumpregs_disasm:
%endif
		; Set U address to CS:(E)IP.
	mov si, reg_eip
	mov di, u_addr
	movsw				; first word of saOffset
%if saSegSel == 4
	movsw				; second word of saOffset
%endif
	mov ax, word [reg_cs]
	stosw				; saSegSel
%if _PM
	call ispm
	jnz .86m
.pm:
	scasw				; skip saSegment, sto to saSelector
.86m:
	stosw				; (if jumped to .86m) saSegment
@@:
%endif

	mov ax, DIS_F_REPT | DIS_F_SHOW
	testopt [options], rr_disasm_no_rept
	jz @F
	and al, ~ DIS_F_REPT
@@:
	testopt [options], rr_disasm_no_show
	jz @F
	and al, ~ DIS_F_SHOW
@@:
	mov word [disflags], ax
	call disasm

		; Set ABO to address after the dumpregs disassembly.
	mov di, behind_r_u_addr
	mov si, u_addr
	movsw				; first word of saOffset
%if saSegSel == 4
	movsw				; second word of saOffset
%endif
	movsw				; saSegSel
%if _PM
	movsw				; saSegment
	movsw				; saSelector
%endif
%if (behind_r_u_addr + SEGADR_size) != u_addr
 %error Expected u_addr behind behind_r_u_addr
	; mov di, u_addr
%endif
		; Reset U offset to (E)IP.
	mov si, reg_eip
	movsw				; first word of saOffset
_386_PM	movsw				; second word of saOffset


%if _ACCESS_VARIABLES_AMOUNT
dumpregs_set_access_variables:
	xor ax, ax
	mov di, reading_access_variables
	mov cx, words(_ACCESS_VARIABLES_AMOUNT * 8 * 2)
	rep stosw

	mov cx, [memrefs.free]
	jcxz .none
	xor si, si
.loop:
	mov bx, si
	call get_memref_index_bx
	mov ax, word [memrefs + bx + mrFlags]
	test al, mrfBranchDirect
	jnz .next
	xor dx, dx			; 0 = reading
	test al, mrfStringSource
	jnz .gotmsg
	inc dx				; 1 = writing
	test al, mrfStringDest
	jnz .gotmsg
	mov dl, al
	and dl, mrfMemSource | mrfMemDest
	cmp dl, mrfMemSource | mrfMemDest
	mov dl, 2			; 2 = r/w
	je .gotmsg
	xor dx, dx			; 0 = reading
	test al, mrfMemSource
	jnz .gotmsg
	inc dx				; 1 = writing
	test al, mrfMemDest
	; jnz .gotmsg
	jz .next
.gotmsg:

	test dl, dl
	jz .read

.write:
	mov di, writing_access_variables
	call add_access_variable
	cmp dl, 1
	je .next

.read:
	mov di, reading_access_variables
	call add_access_variable

.next:
	inc si
	loop .loop
.none:
%endif
	retn


%if _ACCESS_VARIABLES_AMOUNT
add_access_variable:
	xchg ax, bx
	xor bx, bx
.loop:
	cmp word [di + bx + 4], 0
	jne .next
	cmp word [di + bx + 4 + 2], 0
	jne .next
	xchg ax, bx
	 push word [memrefs + bx + mrLinear + 2]
	 push word [memrefs + bx + mrLinear]
	 push word [memrefs + bx + mrLength + 2]
	 push word [memrefs + bx + mrLength]
	xchg ax, bx
	 pop word [di + bx + 4]
	 pop word [di + bx + 4 + 2]
	 pop word [di + bx]
	 pop word [di + bx + 2]
	xchg ax, bx
	retn

.next:
	add bx, 8
	cmp bx, _ACCESS_VARIABLES_AMOUNT * 8
	jb .loop
	xchg ax, bx
	retn


	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
reading_access_variables:
	times _ACCESS_VARIABLES_AMOUNT * 8 db 0

writing_access_variables:
	times _ACCESS_VARIABLES_AMOUNT * 8 db 0

	usesection lDEBUG_CODE
%endif


		; Function to display multiple register entries.
		;
		; INP:	[options]&dispregs32 = whether to display 32-bit registers,
		;				except segment registers which are always 16-bit
		;	si-> 2-byte register name in table
		;	cx = number of registers to display
		; OUT:	si-> register name in table after the last one displayed
		;	cx = 0
		; CHG:	bx, ax, dx
dmpr1:
.:
	lea bx, [si-(reg16names+DATASECTIONFIXUP)]
	add bx, bx			; index * 4
	cmp byte [si+1], 'S'		; segment register ?
	je .no_e			; always 16-bit --> (ZR)
	testopt [options], dispregs32	; display 32-bit register ?
	jz .no_e			; no --> (ZR)
	mov al, 'E'
	stosb				; store E for Exx register name
.no_e:
	movsw				; store register name, increase pointer
	mov al, '='
	stosb				; store equality sign
	jz .no_high			; (ZF left from before)

%if _REGSHIGHLIGHT
	clropt [internalflags3], dif3_highlighting
	mov ax, word [regs + bx + 2]
	mov dx, word [regs_prior + bx + 2]
	testopt [options3], opt3_r_highlight_full
	jz @F
	cmp dx, ax
	jne .highlighthigh
	push dx
	push ax
	mov ax, word [regs + bx]
	mov dx, word [regs_prior + bx]
	cmp dx, ax
	pop ax
	pop dx
	je @F
.highlighthigh:
	call highlight
@@:
	call hexword_diff		; store high word (only if 32-bit register)
.no_high:
	mov ax, word [regs + bx]
	mov dx, word [regs_prior + bx]
	testopt [options3], opt3_r_highlight_full
	jz @F
	cmp dx, ax
	je @F
	call highlight
@@:
	call hexword_diff		; store low word

	call unhighlight
%else
	mov ax, word [regs + bx + 2]
	call hexword			; store high word (only if 32-bit register)
.no_high:
	mov ax, word [regs + bx]
	call hexword			; store low word
%endif

	mov al, 32
	stosb				; store space
	loop .
	retn
			; Note:	This code doesn't use 386+ registers to display our internal
			;	variables for these. Currently, setting the RX bit of options
			;	will display the 32-bit variables even on non-386 machines.
			;	Changing this code to require EAX would require changes to our
			;	check too.
			;	32-bit code probably wouldn't be much shorter than the current
			;	implementation as well.

%if _REGSHIGHLIGHT
highlight:
	testopt [internalflags3], dif3_highlighting
	jnz @F
	setopt [internalflags3], dif3_highlighting
	push si
	push cx
	mov si, msg.highlight
	call copy_single_counted_string
	pop cx
	pop si
@@:
	retn

unhighlight:
	testopt [internalflags3], dif3_highlighting
	jz @F
	clropt [internalflags3], dif3_highlighting
	push si
	push cx
	mov si, msg.unhighlight
	call copy_single_counted_string
	pop cx
	pop si
@@:
	retn

hexword_diff:
	testopt [options3], opt3_r_highlight_diff
	jz @F
	testopt [options3], opt3_r_highlight_full
	jz @FF
@@:
	jmp hexword

@@:
.hexword:
	xchg al, ah
	xchg dl, dh
	call .hexbyte
	xchg al, ah
	xchg dl, dh

.hexbyte:
	push cx
	mov cl, 4
	rol al, cl
	rol dl, cl
	call .hexnyb
	rol al, cl
	rol dl, cl
	pop cx

.hexnyb:
	push ax
	mov ah, dl
	and ax, 0F0Fh
	cmp al, ah
	je .unhighlight
.highlight:
	call highlight
	jmp .common

.unhighlight:
	call unhighlight
.common:
		; INP:	original ax on stack, then return near address
		;	al = nybble value to display, 0..15
		;	es:di -> where to store
		; OUT:	es:di incremented
		;	ax restored
		;	return to near address that was on stack
	jmp hexnyb.common
%endif


%if _RN
		; The layout for FSAVE/FRSTOR depends on mode and 16-/32-bit.

%if 0
	struc FPENV16
.cw:	resw 1	; 00h
.sw:	resw 1	; 02h
.tw:	resw 1	; 04h
.fip:	resw 1	; 06h IP offset
.opc:		; 08h RM: opcode (0-10), IP 16-19 in high bits
.fcs:	resw 1	; 08h PM: IP selector
.fop:	resw 1	; 0Ah operand pointer offset
.foph:		; 0Ch RM: operand pointer 16-19 in high bits
.fos:	resw 1	; 0Ch PM: operand pointer selector
	endstruc; 0Eh

	struc FPENV32
.cw:	resd 1	; 00h
.sw:	resd 1	; 04h
.tw:	resd 1	; 08h
.fip:	resd 1	; 0Ch ip offset (RM: bits 0-15 only)
.fopcr:		; 10h (dword) RM: opcode (0-10), ip (12-27)
.fcs:	resw 1	; 10h PM: ip selector
.fopcp:	resw 1	; 12h PM: opcode (bits 0-10)
.foo:	resd 1	; 14h operand pointer offset (RM: bits 0-15 only)
.fooh:		; 18h (dword) RM: operand pointer (12-27)
.fos:	resw 1	; 18h PM: operand pointer selector
	resw 1	; 1Ah PM: not used
	endstruc; 1Ch
%endif


	usesection lDEBUG_DATA_ENTRY

		; dumpregsFPU - Dump Floating Point Registers
fregnames:
	db "CW", "SW", "TW"
	db "OPC=", "IP=", "DP="
msg.empty:	db "empty"
	endarea msg.empty
msg.nan:	db "NaN"
	endarea msg.nan


	usesection lDEBUG_CODE

dumpregsFPU:
	call guard_auxbuff
	mov es, word [auxbuff_segorsel]
			; => auxbuff
	xor di, di	; -> auxbuff
	mov cx, 128
	xor ax, ax
	rep stosw	; initialise auxbuff
%if _AUXBUFFSIZE < (128 * 2)
 %error auxbuff not large enough for dumpregsFPU
%endif
	mov di, line_out
	mov si, fregnames
	xor bx, bx	; es:bx -> auxbuff
	_386_o32
	fnsave [es:bx]

		; display CW, SW and TW
	push ss
	pop es		; es:di -> line_out
	mov cx, 3
.nextfpr:
	movsw
	mov al, '='
	stosb
	xchg si, bx
	 mov ds, word [auxbuff_segorsel]
			; ds:si -> auxbuff entry
	_386_o32	; lodsd
	lodsw
	 push ss
	 pop ds		; ds:si -> fregnames entry
	xchg si, bx
	push ax
	call hexword
	mov al, 32
	stosb
	loop .nextfpr

		; display OPC
		; in 16-bit PM, there's no OPC
		; in 32-bit PM, there's one, but the location differs from RM
	push bx
%if _PM
	call ispm
	jz .notpm_opc
	add bx, byte 2		; location of OPC in PM differs from RM
_no386	add si, byte 4		; no OPC in 16-bit PM
_no386	jmp short .no_opc
.notpm_opc:
%endif
	movsw
	movsw
	xchg si, bx
	 mov ds, word [auxbuff_segorsel]
				; ds:si -> auxbuff entry
	_386_o32	; lodsd
	lodsw			; skip word/dword
	lodsw
	 push ss
	 pop ds			; ds:si -> fregnames entry
	xchg si, bx
	and ax, 07FFh		; bits 0-10 only
	call hexword
	mov al, 32
	stosb
.no_opc:
	pop bx

		; display IP and DP
	mov cl, 2
.nextfp:
	push cx
	 push ss
	 pop ds			; ds:si -> fregnames entry
	movsw
	movsb
	xchg si, bx
	 mov ds, word [auxbuff_segorsel]
				; ds:si -> auxbuff entry
	_386_o32	; lodsd
	lodsw
	_386_o32	; mov edx, eax
	mov dx, ax
	_386_o32	; lodsd
	lodsw
	xchg si, bx
	 push ss
	 pop ds			; ds:si -> fregnames entry
%if _PM
	call ispm
	jz .notpm_ipdp
	call hexword
	mov al, ':'
	stosb
	jmp short .fppm
.notpm_ipdp:
%endif
	mov cl, 12
	_386_o32	; shr eax, cl
	shr ax, cl
_386	call hexword
_386	jmp short .fppm
	call hexnyb
.fppm:
	_386_PM_o32	; mov eax, edx
	mov ax, dx
_386_PM	call ispm
_386_PM	jz .notpm_fppm
_386_PM	call hexword_high
.notpm_fppm:
	call hexword
	mov al, 32
	stosb
	pop cx
	loop .nextfp

	xchg si, bx
	 push ss
	 pop ds			; ds = es = ss
	call trimputs

		; display ST0..7
	pop bp			; TW
	pop ax			; SW
	pop dx			; CW (discarded here)

	mov cl, 10
	shr ax, cl		; move TOP to bits 1..3
	and al, 1110b		; separate TOP
	mov cl, al
	ror bp, cl		; adjust TW

	mov cl, '0'
.nextst:
	mov di, line_out
	push cx
	mov ax, "ST"
	stosw
	mov al, cl
	mov ah, '='
	stosw
	push di
	test al, 1
	mov al, 32
	mov cx, 22
	rep stosb
	jz .oddst
	mov ax, 10<<8|13
	stosw
.oddst:
	mov al, 0
	stosb			; make it an ASCIZ string
	pop di

	mov ax, bp
	ror bp, 1
	ror bp, 1
	and al, 3		; 00b = valid, 01b = zero, 10b = NaN, 11b = empty
	jz .isvalid
	push si
	 push ss
	 pop ds			; ds = es = ss
	mov si, msg.empty
	mov cl, msg.empty_size
	cmp al, 3
	je .gotst
	mov si, msg.nan
	mov cl, msg.nan_size
	cmp al, 2
	je .gotst
	mov al, '0'
	stosb
	xor cx, cx
.gotst:
	rep movsb
	pop si
	jmp short .regoutdone

.isvalid:
	 mov ds, word [auxbuff_segorsel]
				; ds:si -> auxbuff entry
	testopt [ss:options], hexrn
	jnz .hex
	push di			; -> buffer (first parameter; in es = ss)
	push ds
	push si			; -> auxbuff entry (second parameter)
	dualcall FloatToStr
	jmp short .regoutdone

.hex:
	mov ax, word [si+8]
	call hexword
	mov al, '.'
	stosb
	mov ax, word [si+6]
	call hexword
	mov ax, word [si+4]
	call hexword
	mov ax, word [si+2]
	call hexword
	mov ax, word [si+0]
	call hexword

.regoutdone:
	mov dx, line_out
	 push ss
	 pop ds			; ds = es = ss
	call putsz
	pop cx

	add si, byte 10		; -> next ST
	inc cl
	cmp cl, '8'
	jne .nextst
	 mov es, word [auxbuff_segorsel]
				; es => auxbuff
	_386_o32
	frstor [es:0]
	retn
%endif


		; DMPFLAGS - Dump flags output.
dmpflags:
%if _40COLUMNS
.80:
	push bp
	mov bp, flagbits_for_80 << flagbits_for_shl
	jmp @F
.40:
	push bp
	mov bp, flagbits_for_40 << flagbits_for_shl
@@:
%endif
%if _REGSHIGHLIGHT
	push dx
	push bx
%endif
	mov si, flagbits
	mov cx, flagbits.amount
.loop:	lodsw
%if _40COLUMNS
	shl bp, 1
	jnc .next
%endif
%if _REGSHIGHLIGHT
	mov dx, word [reg_efl - regs + regs_prior]
	and dx, ax
	mov bx, word [reg_efl]
	and bx, ax
	cmp dx, bx
	je @F
	testopt [options3], opt3_r_highlight_diff | opt3_r_highlight_full
	jz @F
	testopt [internalflags3], dif3_do_not_highlight
	jnz @F
	call highlight
@@:
	test bx, bx
%else
	test ax, word [reg_efl]
%endif
	mov ax, word [si+(flagsoff-flagbits)-2]
	jz .off			; if not set
	mov ax, word [si+(flagson-flagbits)-2]
.off:	stosw
%if _REGSHIGHLIGHT
	call unhighlight
%endif
	mov al, 32
	stosb
.next:
	loop .loop
	dec di			; -> last (unnecessary) blank
%if _REGSHIGHLIGHT
	pop bx
	pop dx
%endif
%if _40COLUMNS
	pop bp
%endif
	retn


%if _40COLUMNS
dmpshortflags:
%if _REGSHIGHLIGHT
	push dx
	push bx
%endif
	mov si, shortflagbits
	mov cx, shortflagbits.amount
.loop:	lodsw
%if _REGSHIGHLIGHT
	mov dx, word [reg_efl - regs + regs_prior]
	and dx, ax
	mov bx, word [reg_efl]
	and bx, ax
	cmp dx, bx
	je @F
	testopt [options3], opt3_r_highlight_diff | opt3_r_highlight_full
	jz @F
	testopt [internalflags3], dif3_do_not_highlight
	jnz @F
	call highlight
@@:
	test bx, bx
%else
	test ax, word [reg_efl]
%endif
	mov ax, word [si+(shortflagsoff-shortflagbits)-2]
	jz .off			; if not set
	mov ax, word [si+(shortflagson-shortflagbits)-2]
.off:	stosb
%if _REGSHIGHLIGHT
	call unhighlight
%endif
	mov al, 32
	stosb
.next:
	loop .loop
%if _REGSHIGHLIGHT
	pop bx
	pop dx
%endif
	retn
%endif


%if _OPTIONS || _VARIABLES
dumpvars:
%if _VARIABLES
	mov si, vregs
%endif
	xor bx, bx
.loop:
	mov di, line_out
	xor dx, dx
%if _VARIABLES
	mov cx, 4
	call .dump		; display four variables
	inc bx			; (would be one off here)
	push si
%else
	add bx, byte 4		; (no motivation to optimize that)
%endif
%if _OPTIONS
 %if _VARIABLES
	mov ax, 32<<8|32
	stosw			; more blanks inbetween
 %endif
	cmp bl, 16
	je .3
	cmp bl, 8
	ja .2
	je .1

		; First line, display DCO and DCS
.0:
	mov ax, "CO"
	mov si, options
	call .dump_option
	mov ax, "CS"
	mov si, startoptions
	jmp short .next

		; Second line, DAO and DAS
.1:
	mov ax, "AO"
	mov si, asm_options
	call .dump_option
	mov ax, "AS"
	; asm_startoptions follows directly behind asm_options
	jmp short .next

		; Third line, DIF and DPI
.2:
	mov ax, "IF"
	mov si, internalflags
	call .dump_option
	mov ax, "PI"
	mov si, psp22
	inc dx
	inc dx
	jmp short .next

		; Fourth line, DPR, DPS (if _PM) and DPP
.3:
	inc dx
	mov ax, "PR"
	mov si, pspdbg
	call .dump_option
 %if _PM
	xor ax, ax
	call ispm
	jnz .3_rm
	push ds
	db __TEST_IMM8		; (skip push)
.3_rm:
	push ax
	mov ax, "PS"
	mov si, sp
	call .dump_options
	pop ax
 %else
	mov ax, 32<<8|32
	stosw
	stosw
 %endif
	mov ax, "PP"
	mov si, parent

.next:
	call .dump_options
%endif
	push bx
	call putsline_crlf	; display line
	pop bx			; (retain counter)
%if _VARIABLES
	pop si			; (retain pointer to next variable)
%endif
	cmp bl, 16		; was end ?
	jne .loop		; no, loop -->

				; done
.mode:
	mov dx, msg.rv_mode.before
	call putsz
%if _PM
	call ispm
	jnz .mode_86m
	mov dx, msg.rv_mode_dpmi_16
	mov bx, word [reg_cs]
	call test_d_b_bit
	jz @F
	mov dx, msg.rv_mode_dpmi_32
	jmp @F

.mode_86m:
%endif
	mov dx, msg.rv_mode_r86m
		; (only 386+ has the V86M so even though smsw ax is a
		;  286 level instruction, so could be used without a 386,
		;  we only really need it on a 386+.)
_386	smsw ax
_386	test al, 1
_386	jz @F
_386	mov dx, msg.rv_mode_v86m
@@:
	jmp putsz


		; INP:	ax = 2-byte option name ('N' will precede this)
		;	d[si] = value
		; OUT:	si-> behind value
		;	cx = 0
		; CHG:	ax
.dump_options:
%if _VARIABLES
.dump_option:
	mov word [di], " D"
	scasw
%else
	mov byte [di], ' '
	inc di
.dump_option:
	mov byte [di], 'D'
	inc di
%endif
	stosw
%if _VARIABLES		; falls through otherwise, always count 1
	mov cx, 1
	jmp short .dump_one
%endif

%if 0
PM && OPTIONS && VARIABLES
V0=00000000 V1=00000000 V2=00000000 V3=00000000   DCO=00000000 DCS=00000000
V4=00000000 V5=00000000 V6=00000000 V7=00000000   DAO=00000000 DAS=00000000
V8=00000000 V9=00000000 VA=00000000 VB=00000000   DIF=0000840D DPI=0616:01DE
VC=00000000 VD=00000000 VE=00000000 VF=00000000   DPR=0984 DPS=0000 DPP=0616

!PM && OPTIONS && VARIABLES
V0=00000000 V1=00000000 V2=00000000 V3=00000000   DCO=00000000 DCS=00000000
V4=00000000 V5=00000000 V6=00000000 V7=00000000   DAO=00000000 DAS=00000000
V8=00000000 V9=00000000 VA=00000000 VB=00000000   DIF=0000840D DPI=0616:01DE
VC=00000000 VD=00000000 VE=00000000 VF=00000000   DPR=0984     DPP=0616

!OPTIONS && VARIABLES
V0=00000000 V1=00000000 V2=00000000 V3=00000000
V4=00000000 V5=00000000 V6=00000000 V7=00000000
V8=00000000 V9=00000000 VA=00000000 VB=00000000
VC=00000000 VD=00000000 VE=00000000 VF=00000000

!PM && OPTIONS && !VARIABLES
DCO=00000000 DCS=00000000
DAO=00000000 DAS=00000000
DIF=0000840D DPI=0616:01DE
DPR=0984     DPP=0616

PM && OPTIONS && !VARIABLES
DCO=00000000 DCS=00000000
DAO=00000000 DAS=00000000
DIF=0000840D DPI=0616:01DE
DPR=0984 DPS=0000 DPP=0616

!OPTIONS && !VARIABLES
%endif
%if 0
DCO Debugger Common Options
DCS Debugger Common Startup options
DIF Debugger Internal Flags
DPR Debugger Process (Real-mode segment)
DPS Debugger Process Selector, or zero
DPP Debugger Parent Process
DPI Debugger Parent Interrupt 22h
DAO Debugger Assembler/disassembler Options
DAS Debugger Assembler/disassembler Startup options
%endif

%if _VARIABLES
.dump_loop:
	inc bx
	mov al, 32
	stosb
.dump:
	mov al, 'V'
	stosb
	mov al, bl
	call hexnyb
%endif
.dump_one:
	mov al, '='
	stosb
	lodsw
	cmp dl, 1
	je .dumpw
	push ax
	lodsw
	pushf
	call hexword
	popf				; CF
	jb .nocolon
	mov al, ':'
	stosb
.nocolon:
	pop ax
.dumpw:
	call hexword
%if _VARIABLES
	loop .dump_loop
%endif
	retn
%endif


dumpallvars:
	lodsb
	call chkeol
	mov si, vregs
	xor bx, bx
.loop:
	mov di, line_out
	mov cx, 4
	xor dx, dx
	call .dump		; display four variables
	inc bx			; (would be one off here)
	test dx, dx
	jz @F
	push si
	push bx
	call putsline_crlf	; display line
	pop bx			; (retain counter)
	pop si			; (retain pointer to next variable)
@@:
	test bl, bl		; was end ?
	jnz .loop		; no, loop -->
	retn

.dump_loop:
	inc bx
	mov al, 32
	stosb
.dump:
	mov al, 'V'
	stosb
	mov al, bl
	call hexbyte
.dump_one:
	mov al, '='
	stosb
	lodsw
	or dx, ax
	push ax
	lodsw
	or dx, ax
	call hexword
	pop ax
	call hexword
	loop .dump_loop
	retn


dumpmemory:
	lodsb
	call chkeol

	mov ax, word [code_seg]
%if _PM
	mov dx, word [code_sel]
%endif
	mov si, msg.vm_codeseg
	call .line

%if _DUALCODE
	mov ax, word [code2_seg]
 %if _PM
	mov dx, word [code2_sel]
 %endif
	mov si, msg.vm_code2seg
	call .line
%endif

%if _PM
	mov ax, word [pspdbg]
	mov dx, ss
%else
	mov ax, ss
%endif
	mov si, msg.vm_dataseg
	call .line

%if _PM
	mov ax, word [pspdbg]
	mov dx, word [cssel]
%else
	mov ax, ss
%endif
	mov si, msg.vm_entryseg
	call .line

%if _PM
	mov ax, word [auxbuff_segorsel + soaSegment]
	mov dx, word [auxbuff_segorsel + soaSelector]
%else
	mov ax, word [auxbuff_segorsel]
%endif
	mov si, msg.vm_auxseg
%if _HISTORY_SEPARATE_FIXED && _HISTORY
	call .line

 %if _PM
	mov ax, word [history.segorsel + soaSegment]
	mov dx, word [history.segorsel + soaSelector]
 %else
	mov ax, word [history.segorsel]
 %endif
	mov si, msg.vm_hisseg
%endif

.line:
	mov di, line_out
	call copy_single_counted_string
	call hexword
%if _PM
	call ispm
	jnz @F
	mov si, msg.vm_selector
	call copy_single_counted_string
	xchg ax, dx
	call hexword
@@:
%endif
	jmp putsline_crlf


dumpprocess:
	lodsb
	call chkeol

%if _PM
	call var_psps_setup
%endif
	call var_ppr_setup
	call var_ppi_setup

%if _BOOTLDR
	mov dx, msg.rvp_boot
	testopt [internalflags], nodosloaded
	jnz @F
%endif
%if _DEVICE
	mov dx, msg.rvp_device
	testopt [internalflags6], dif6_device_mode
	jnz @F
%endif
%if _TSR
	mov dx, msg.rvp_tsr
	testopt [internalflags], tsrmode
	jnz @F
%endif
	mov dx, msg.rvp_application
@@:
	call putsz

	mov di, line_out

	mov ax, word [pspdbe]
	mov si, msg.vp_pspsegment
	call .line

	mov ax, word [psp_parent]
	mov si, msg.vp_parent
	call .line

	mov ax, word [psp_pra + 2]
	mov si, msg.vp_pra
	call .line
	mov al, ':'
	stosb
	mov ax, word [psp_pra]
	call hexword

%if _PM
	mov ax, word [psp_selector]
	mov si, msg.vp_pspsel
	call .line
%endif
	call putsline_crlf

	mov di, line_out

	mov ax, word [pspdbg]
	mov si, msg.vp_dpspsegment
	call .line

	mov ax, word [parent]
	mov si, msg.vp_dparent
	call .line

	mov ax, word [psp22 + 2]
	mov si, msg.vp_dpra
	call .line
	mov al, ':'
	stosb
	mov ax, word [psp22]
	call hexword

%if _PM
	mov ax, ss
	mov si, msg.vp_dpspsel
	call .line
%endif
	jmp putsline_crlf

.line:
	call copy_single_counted_string
	jmp hexword


dumpdevice:
	lodsb
	call chkeol

%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz @F
%endif
	mov dx, msg.rvd_not_device
	call putsz
	retn

%if _DEVICE
@@:
	mov ax, word [device_header_address + 2]
	mov si, msg.rvd_deviceheader
	call .line
	mov al, ':'
	stosb
	mov ax, word [device_header_address]
	call hexword

	mov ax, word [device_mcb_paragraphs]
	mov si, msg.rvd_size
	call .line
	jmp putsline_crlf

.line:
	call copy_single_counted_string
	jmp hexword
%endif


%if _MMXSUPP
subcpu 586
dumpregsMMX:
	dec si
	call get_length_keyword
	lodsb
	cmp cl, 4
	je error
	mov dh, 1
	shl dh, cl
	call chkeol

	call guard_auxbuff
	mov ds, word [auxbuff_segorsel]	; => auxbuff
	o32
	fnsave [0]
	mov si, 7*4
	mov cl, '0'
	mov di, line_out
.nextreg:
	mov ds, word [ss:auxbuff_segorsel]
					; => auxbuff
	mov ax, "MM"
	stosw
	mov al, cl
	mov ah, '='
	stosw
	push cx
	mov dl, 8
	mov bh, 0
	mov bl, dh		; = how many bytes per item
.nextitem:
	add si, bx		; -> behind item of data
	db __TEST_IMM8		; (skip dec dx)
.nextbyte:
	dec dx			; (if branched here) dl always nonzero after this
	dec si			; -> next byte (less significant than prior)
	mov al, byte [si]
	call hexbyte		; write this byte
	cmp bl, 5		; wrote 4 bytes of qword yet ?
	mov al, ':'
	je .oddbyte		; yes, add a colon
	cmp bl, 1		; within item ?
	ja .initem		; yes, no more separators -->
	mov al, 32
	test dl, 1
	jz .oddbyte
	mov al, '-'
.oddbyte:
	stosb
.initem:
	dec bx			; count down in item index
	jnz .nextbyte		; not yet done with item -->
	mov bl, dh		; reset bx = number of bytes per item
	add si, bx		; -> behind item
	dec dl			; qword done ?
	jnz .nextitem		; not yet -->

	dec di			; -> at last separator
	mov ax, 32<<8|32
	stosw
	add si, byte 2
	pop cx
	test cl, 1
	jz .oddreg
	push cx
	push dx
	 push ss
	 pop ds				; ds = es = ss
	call trimputs
	pop dx
	pop cx
	mov di, line_out
.oddreg:
	inc cl
	cmp cl, '8'
	jne .nextreg
	mov ds, word [ss:auxbuff_segorsel]
					; => auxbuff
	o32
	fldenv [0]
	 push ss
	 pop ds				; ds = es = ss
	retn
subcpureset
%endif
..@rr_access_end:
