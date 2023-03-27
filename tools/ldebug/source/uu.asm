
%if 0

lDebug U command - Disassembler

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

..@uu_access_start:
		; U command - disassemble.
uu:
	dec si
	dec si
	mov dx, msg.uninstall
	call isstring?
	lodsb
	je uninstall
	lodsb

	mov word [lastcmd], lastuu
	call iseol?
	jne uu1			; if an address was given

lastuu:
_386_PM	xor eax, eax
	mov ax, word [uu_default_lines]
				; default length in lines, if nonzero
	test ax, ax
	jz @F
	js short .error
	mov word [getrange_lines], ax
	xor ax, ax
	jmp @FF

.error:
	jmp error

@@:
	mov word [getrange_lines], 8000h
	mov ax, word [uu_default_length]
	test ax, ax
	jz short .error
	dec ax
@@:
	_386_PM_o32	; mov ecx, dword [u_addr]
	mov cx, word [u_addr]
	_386_PM_o32	; mov edx, ecx
	mov dx, cx
	_386_PM_o32	; add ecx, byte 1Fh
	add cx, ax
	jnc .no_overflow	; if no overflow -->
	_386_PM_o32	; or ecx, byte -1
	or cx, byte -1		; til end of segment
.no_overflow:
	jmp short uu2

uu1:
	mov cx, word [uu_default_length]
				; default length
	mov di, word [uu_default_lines]
				; default length in lines, if nonzero
	mov bx, word [reg_cs]
	mov word [getrange_lines], 8000h
	call getrangeX.lines	; get address range into bx:(e)dx
	call chkeol		; expect end of line here
	mov word [u_addr + saSegSel], bx
%if _PM
	call ispm
	jnz .86m
.pm:
	mov word [u_addr + saSelector], bx
	jmp @F
.86m:
	mov word [u_addr + saSegment], bx
@@:
%endif
	_386_PM_o32		; mov dword [u_addr], edx
	mov word [u_addr], dx

		; (d)word [u_addr] = offset start address
		; word [u_addr + saSegSel] = segment start address
		; (e)cx = end address
		; (e)dx = start adddress (same as [u_addr])
uu2:
	_386_PM_o32	; inc ecx
	inc cx
uu3:
	call handle_serial_flags_ctrl_c
	_386_PM_o32	; push ecx
	push cx
	_386_PM_o32	; push edx
	push dx
	and word [disflags], 0
	call disasm		; do it
	_386_PM_o32	; pop ebx
	pop bx
	_386_PM_o32	; pop ecx
	pop cx
	_386_PM_o32	; mov eax, dword [u_addr]
	mov ax, word [u_addr]
	_386_PM_o32	; mov edx, eax
	mov dx, ax
	_386_PM_o32	; sub eax, ecx
	sub ax, cx		; current position - end
	_386_PM_o32	; sub ebx, ecx
	sub bx, cx		; previous position - end

	testopt [getrange_lines], 8000h
	jz @F
	_386_PM_o32	; cmp eax, ebx
	cmp ax, bx
	jnb uu3			; if we haven't reached the goal
	retn

@@:
	dec word [getrange_lines]
	jnz uu3
	retn


	usesection lDEBUG_DATA_ENTRY

		; Jump table for displaying sized operands.
		;  Entries are defined in the debug.asm opsizeditem list.
	align 2, db 0
disjmp2:
	dw DISJMP2_OPSIZEDITEMS
.end:

		; Jump table for displaying operands.
		;  Entries are defined in the debug.asm opitem list.
	align 2, db 0
optab:
	dw da_internal_error	; entry for OP_END
	dw OPTAB_OPITEMS


%if _MEMREF_AMOUNT
	align 16, db 0
memrefs:	times MEMREF_size * _MEMREF_AMOUNT db 0
.free:		dw 0

	align 4, db 0
string_memref_counter:
		dd 0
%endif


;	DISASM - Disassemble.

%if _SYMBOLIC
	align 4, db 0
u_lin_start:	dd 0
%endif
	align 2, db 0
%if _40COLUMNS
mnemonofs:	dw 0
trailofs:	dw 0
%endif
dis_n:		dw 0		; number of bytes in instruction so far
		dw 0
preflags:	db 0		; flags for prefixes found so far (includes OSIZE,ASIZE)
preused:	db 0		; flags for prefixes used so far
%if _PM
presizeflags:	db 0		; O32,A32 flags = (OSIZE,ASIZE) XOR (32-bit cs?)
%else
presizeflags:	equ preflags	; O32,A32 flags are always equal to OSIZE,ASIZE
%endif

PRESEG	equ 1			; segment prefix
PREREP	equ 2			; rep prefixes
PREREPZ	equ 4			; F3h, not F2h
PRELOCK	equ 8			; lock prefix
PREOSIZE equ 10h		; flag for OSIZE prefix
PREASIZE equ 20h		; flag for ASIZE prefix
PRE32D	equ 10h			; flag for 32-bit data
PRE32A	equ 20h			; flag for 32-bit addressing
PREWAIT	equ 40h			; prefix wait (not really a prefix)
GOTREGM	equ 80h			; set if we have the reg/mem part

instru:	db 0			; the main instruction byte
disp8:	db 0
	align 2, db 0
index:	dw 0			; index of the instruction (unsqueezed)
obsinst:dw SFPGROUP3, SFPGROUP3+1, SFPGROUP3+4
	dw SPARSE_BASE+24h, SPARSE_BASE+26h ; obsolete-instruction values
	; This is used to search for obsolete instructions:
	; DBE0h:	feni
	; DBE1h:	fdisi
	; DBE4h:	fsetpm
	; 124h:		mov trX, reg
	; 126h:		mov reg, trX

rmsize:	db 0			; <0 or 0 or >0 means mod r/m is 8 or 16 or 32
segmnt:	db 0			; segment determined by prefix (or otherwise)
	align 4, db 0
addrr:	dd 0			; address in mod r/m byte
savesp2:dw 0			; save the stack pointer here (used in disasm)

	align 2, db 0
disflags:	dw 0		; flags for the disassembler

;--- equates for disflags:

DIS_F_REPT	equ 1		; repeat after pop ss, etc.
DIS_F_SHOW	equ 2		; show memory contents
DIS_I_SHOW	equ 4		; there are memory contents to show
DIS_I_UNUSED	equ 8		; (internal) print " (unused)"
DIS_I_SHOWSIZ	equ 10h		; (internal) always show the operand size
DIS_I_KNOWSIZ	equ 20h		; (internal) we know the operand size of instr.
DIS_I_MOV_SS	equ 40h		; (internal) note for repetition: mov ss
DIS_I_DONTSHOW	equ 80h		; do not show memory contents override
DIS_I_NOSIZ	equ 100h	; do not show size
DIS_I_FAR_M	equ 200h	; far memory reference, show segment word contents
DIS_I_DOUBLE_M	equ 400h	; double memory reference, show second item contents
DIS_I_SHOW_A32	equ 800h	; memory contents to show are 32-bit addressed
DIS_I_M_SRC    equ 1000h	; memory operand is source
DIS_I_M_DST    equ 2000h	; memory operand is destination
				; both of the above may be set
DIS_I_M_ALWAYS_16 equ 4000h	; mov from or to segreg, display no size
				;  and treat as m16 and ignore (but allow) osize

disflags2:	db 0		; another copy of DIS_I_KNOWSIZ
disrepeatcount:	db 0

	align 2, db 0
sizeloc:	dw 0		; address of size words in output line
%if _COND
condmsg:	dw 0		; -> conditionals message to display (if R and no mem)
%endif

		; Jump table for a certain place.
		; The size of this table matches OPTYPES_BASE.
	align 2, db 0
disjmp:
	dw disbad		; illegal instruction
	dw da_twobyte		; two-byte instruction
	dw da_insgrp		; instruction group
	dw da_fpuins		; coprocessor instruction
	dw da_fpugrp		; coprocessor instruction group
	dw da_insprf		; instruction prefix (including 66h/67h)
%if ($ - disjmp) != OPTYPES_BASE
 %error Wrong length of table disjmp
%endif

		; Table for 16-bit mod r/m addressing.  8 = BX, 4 = BP, 2 = SI, 1 = DI.
rmtab:		db 8+2, 8+1, 4+2, 4+1, 2, 1, 4, 8


%macro defgpr 1.nolist
REG_ %+ %1: equ ($ - rgnam816) / 2
%defstr %%string %1
		db %%string
%endmacro
%macro defsr 1.nolist
REG_ %+ %1: equ REG_NO_GPR + ($ - segrgnam) / 2
%defstr %%string %1
		db %%string
%endmacro

REG_NO_GPR	equ 24	; 16..23 are registers eax..edi

	align 2, db 0
		; Tables of register names.
		; rgnam816/rgnam16/segrgnam/xregnam must be consecutive.
rgnam816:
	defgpr AL
	defgpr CL
	defgpr DL
	defgpr BL
	defgpr AH
	defgpr CH
	defgpr DH
	defgpr BH	; 0..7
rgnam16:
	defgpr AX
	defgpr CX
	defgpr DX
	defgpr BX
	defgpr SP
	defgpr BP
	defgpr SI
	defgpr DI	; 8..15 (16-bit), 16..23 (32-bit)
N_REGS16	equ ($ - rgnam16) >> 1
segrgnam:
	defsr ES
	defsr CS
	defsr SS
	defsr DS
	defsr FS
	defsr GS	; 24..29
N_SEGREGS	equ ($ - segrgnam) >> 1
xregnam:
	defsr ST
	defsr MM
	defsr CR
	defsr DR
	defsr TR	; 30..34
N_ALLREGS	equ ($ - rgnam816) >> 1


	align 2, db 0
reg32addr:	dw reg_eax, reg_ecx, reg_edx, reg_ebx
		dw reg_esp, reg_ebp, reg_esi, reg_edi

	align 2, db 0
segrgaddr:	dw reg_es,reg_cs,reg_ss,reg_ds,reg_fs,reg_gs

;	Tables for handling of named prefixes.

prefixlist:	db 26h,2Eh,36h,3Eh,64h,65h	; segment prefixes (in order)
		db 9Bh,0F0h,0F2h,0F3h		; WAIT,LOCK,REPNE,REPE
		db 066h,067h			; OSIZE, ASIZE
N_PREFIX:	equ $ - prefixlist

	align 2, db 0
prefixmnem:	dw MN_WAIT,MN_LOCK,MN_REPNE,MN_REPE
		dw MN_O32,MN_A32	; in 16-bit CS, OSIZE is O32 and ASIZE is A32
%if _PM
		dw MN_O16,MN_A16	; in 32-bit CS, OSIZE is O16 and ASIZE is A16
%endif

%if _COND
	align 2, db 0
cond_table:
	dw 800h			; OF
	dw 1			; CF
	dw 40h			; ZF
	dw 41h			; CF | ZF
	dw 80h			; SF
	dw 4			; PF
;	dw cond_L_GE	; handled specifically
;	dw cond_LE_G	; handled specifically
%endif

single_byte_opcodes_repeat_disassembly:
.:
	db 0E6h		; out imm8, al
	db 0E7h		; out imm8, (e)ax
	db 0EEh		; out dx, al
	db 0EFh		; out dx, (e)ax
	db 06Eh		; outsb
	db 06Fh		; outsw/outsd
	db 0E4h		; in al, imm8
	db 0E5h		; in (e)ax, imm8
	db 0ECh		; in al, dx
	db 0EDh		; in (e)ax, dx
	db 06Ch		; insb
	db 06Dh		; insw/insd
	db 0F4h		; hlt
	db 0FBh		; sti
	db 17h		; pop ss
.length: equ $ - .
	db 1Fh		; pop ds
	db 07h		; pop es
.length_nec: equ $ - .


	usesection lDEBUG_CODE

disasm:
	mov byte [disrepeatcount], 0	; number of repeated disassembly lines

%if _MEMREF_AMOUNT
	mov cx, _MEMREF_AMOUNT
	mov di, memrefs
@@:
	call init_one_memref
	loop @B
	stosw				; memrefs.free
%endif

.preserve_disrepeatcount:
%if _COND
	and word [condmsg], 0		; initialize conditions message
%endif
.preserve_condmsg_and_disrepeatcount:
	mov word [savesp2], sp
	_386_PM_o32		; xor eax, eax
	xor ax, ax
	_386_PM_o32		; mov dword [dis_n], eax
	mov word [dis_n], ax
	mov byte [disp8], al
	mov word [preflags], ax		; clear preflags and preused
	mov	byte [segmnt], 3	; initially use DS segment
	mov	byte [rmsize], 80h	; don't display any memory
	mov	word [dismach], ax	; no special machine needed, so far
	mov bx, word [u_addr + saSegSel]
%if _PM
	mov byte [bCSAttr], al
	call test_d_b_bit
	jz .16
	mov byte [bCSAttr], 40h
.16:
%endif
%if _40COLUMNS
	mov ax, MNEMONOFS
	mov dx, 79
	testopt [asm_options], disasm_40_columns
	jz .not40
	mov ax, MNEMONOFS_40
	mov dx, 39
.not40:
	mov word [mnemonofs], ax
	mov word [trailofs], dx
%endif

%if _SYMBOLIC
	nearcall zz_detect_xms		; re-detect XMS if used after run

	testopt [internalflags3], dif3_nosymbols_1 | dif3_nosymbols_2
	jnz .no_symbol

	_386_PM_o32
	mov dx, word [u_addr]
	call getlinear_32bit
	jc .no_symbol

	mov word [u_lin_start], ax
	mov word [u_lin_start + 2], dx

	mov cx, dx
	mov bx, ax
	nearcall binsearchmain		; search for matching symbol
	jcxz .no_symbol
.loop_symbol:
	 push bx
	dualcall displaystring
	 push bx
	 push ax
	dualcall getfarpointer.main
	 pop di
	 pop es
	xor dx, dx
_386_PM	test byte [bCSAttr], 40h
_386_PM	jz @F
_386_PM	mov dx, word [u_addr + 2]
@@:
	mov ax, word [u_addr]
	cmp dx, word [es:di + smOffset + 2]
	mov dx, msg.uu_after_symbol.non_wrt
	jne .wrt_symbol
	cmp ax, word [es:di + smOffset]
	je .non_wrt_symbol
.wrt_symbol:
	mov dx, msg.uu_between_symbol.wrt
	call disp_message
	push ss
	pop es
	mov di, line_out
	mov ax, word [u_addr + 4]
	call hexword
	push bx
	push cx
	call putsline
	pop cx
	pop bx
	mov dx, msg.uu_after_symbol.wrt
.non_wrt_symbol:
	call disp_message
.next_symbol:
	inc bx
	loop .loop_symbol

.no_symbol:
	push ss
	pop es
	push ss
	pop ds
%endif

	call disgetbyte			; get a byte of the instruction
	cmp	al, 9Bh			; wait instruction (must be the first prefix)
	jne	da2			; if not -->

;	The wait instruction is actually a separate instruction as far as
;	the x86 is concerned, but we treat it as a prefix since there are
;	some mnemonics that incorporate it.  But it has to be treated specially
;	since you can't do, e.g., seg cs wait ... but must do wait seg cs ...
;	instead.  We'll catch it later if the wait instruction is not going to
;	be part of a shared mnemonic.

	or byte [preflags], PREWAIT

;	If we've found a prefix, we return here for the actual instruction
;	(or another prefix).

da1:
	call disgetbyte
da2:
	mov	[instru],al	; save away the instruction
	mov	ah,0

;	Now we have the sequence number of the instruction in AX.  Look it up.

da3:
	mov	bx,ax
	mov	[index],ax	; save the compressed index
	cmp	ax,SPARSE_BASE
	jb	da4		; if it's not from the squeezed part of the table
	mov	bl,[sqztab+bx-SPARSE_BASE]
	mov	bh,0
	add	bx,SPARSE_BASE	; bx = compressed index

da4:
	mov	cl, [optypes+bx]; cx = opcode type
	mov	ch, 0
	shl	bx, 1
	mov	bx, [opinfo+bx]	; bx = other info (usually the mnemonic)
	mov	si, cx
	mov	ax, bx
	mov	cl, 12
	shr	ax, cl
	cmp	al, [dismach]
	jb	da5		; if a higher machine is already required
	mov	[dismach], al	; set machine type
da5:
	and	bh, 0Fh		; remove the machine field
	cmp	si, OPTYPES_BASE
	jae	da13_unp	; if this is an actual instruction
	test si, 1		; check whether valid displacement
	jnz da_internal_error	; if not -->
	call [disjmp+si]	; otherwise, do more specific processing
	jmp s	da3		; back for more

;	Two-byte instruction.

da_twobyte:
	call disgetbyte
	mov	[instru],al
	mov	ah,0
	add	ax,SPARSE_BASE
	ret

;	Instruction group.

da_insgrp:
	call getregmem_r	; get the middle 3 bits of the R/M byte
	cbw
	add	ax,bx		; offset
	ret

;	Coprocessor instruction.

da_fpuins:
	or	byte [disflags],DIS_I_SHOWSIZ
	or	byte [dmflags],DM_COPR
	call getregmem
	cmp	al,0c0h
	jb	da_insgrp	;range 00-bfh is same as an instruction group
	mov	cl,3
	shr	al,cl		;C0h --> 18h
	sub	al,18h-8	;18h --> 8
	cbw
	add	ax,bx		;offset
	ret

;	Coprocessor instruction group.

da_fpugrp:
	mov	al,[regmem]
	and	al,7
	cbw
	add	ax,bx
	ret

;	Instruction prefix.  At this point, bl = prefix bits; bh = segment

da_insprf:
	test bl,[preflags]
	jnz	da12		; if there are duplicates
	or [preflags],bl
	test bl,PRESEG
	jz	da11		; if not a segment
	mov	[segmnt],bh	; save the segment
da11:
	pop	ax		; discard return address
	jmp	da1

da12:
	jmp	disbad		; we don't allow duplicate prefixes

	; si = index into table opindex, + OPTYPES_BASE
da13_unp:
	sub si, OPTYPES_BASE	; = offset into opindex
	xor ax, ax
	mov al, [si + opindex]	; ax = adjustment (from opindex)
	add si, si		; take offset twice
	dec si			; subtract one
	add si, ax		; add in the adjustment

;	OK.  Here we go.  This is an actual instruction.
;	bx = offset of mnemonic in mnlist
;	si = offset of operand list in oplists
;	First print the op mnemonic.

da13:
%if _PM
	mov al, byte [preflags]
	and al, PREOSIZE | PREASIZE	; get OSIZE,ASIZE status (= O32,A32 in 16-bit cs)
_386	test byte [bCSAttr], 40h	; in a 32-bit segment?
_386	jz .16				; no -->
_386	xor al, PRE32D | PRE32A		; OSIZE,ASIZE present means O16,A16
.16:
	mov byte [presizeflags], al	; set O32,A32 status
%endif
	push si
	lea	si,[mnlist+bx]	; offset of mnemonic
	cmp	si,mnlist+MN_BSWAP
	jne	da13a		; if not BSWAP
	call dischk32d
	jz	da12		; if no operand-size prefix -->
da13a:
	call showop		; print out the op code (at line_out+28)
	and word [sizeloc], 0	; clear out this flag
	pop si			; recover list of operands (offset in oplists)
	add si, oplists		; -> actual oplist entry

da14_check_end:
	cmp byte [si], OP_END
	je da_op_end_e		; if we're done -->

;	Loop over operands.  si-> next operand type.
;	Fortunately the operands appear in the instruction in the same
;	order as they appear in the disassembly output.

da14:
	mov byte [disflags2], 0	; clear out size-related flags
	lodsb			; get the operand type
	cmp	al,OP_SIZE
	jb	da18		; if it's not size dependent
	mov	byte [disflags2],DIS_I_KNOWSIZ	;indicate variable size
	cmp	al,OP_1632_DEFAULT
	jae	da15_default
	cmp	al,OP_8
	jae	da16		; if the size is fixed (8, 16, 32, 64)
	cmp	al,OP_1632
	jae	da15		; if word or dword (or segreg mov)
		; OP_ALL here. This has a width of 2.
		;  If the low bit is clear, this means
		;  8 bit, else 16/32 bits.
	mov	ah,-1
	test byte [instru],1
	jz	da17		; if byte -->
	jmp da15

da15_default:
	test byte [preflags], PREOSIZE
	jnz da15
	setopt [disflags], DIS_I_NOSIZ
da15:
	or byte [preused],PRE32D; mark this flag as used
.no_use_osize:
	mov	ah,[presizeflags]
	and	ah,PRE32D	;this will be 10h for dword, 00h for word
	jmp s	da17		;done

da16:
	mov	ah,al		;OP_8, OP_16, OP_32 or OP_64 (we know which)
	and	ah,0f0h		;this converts ah to <0 for byte, =0 for word,
	sub	ah,OP_16	;and >0 for dword (byte=F0h, word=00h, dword=10h, qword=20h)

;	Now we know the size (in ah); branch off to do the operand itself.

da17:
	mov bl, al
	and bx, 0Fh		; 8 entries (IMM, RM, M, R_MOD, M_OFFS, R, R_ADD, AX)
	shl bx, 1
	cmp bx, disjmp2.end - disjmp2
	jae @F
	call [disjmp2 + bx]	; print out the operand
	jmp short da20		; done with operand

@@:
da_internal_error:
	mov dx, msg.uu_internal_error
	call putsz
	jmp cmd3


;	Sizeless operands.

da18:
	cbw
	xchg ax, bx		; bx = index
	shl bx, 1
	mov ax, [optab + bx]
	cmp bx, OP_FIRST_STRING << 1
	jb	da19		; if it's not a string
	cmp bx, OP_AFTER_LAST << 1
	jae @B
	call	dis_stosw_lowercase
	test	ah, ah
	jnz	da20		; if it's two characters
	dec	di
	jmp s	da20		; done with operand

da19:
	call ax			; otherwise, do something else

		; Operand done, check if there's another one.
da20:
	cmp	byte [si], OP_END
da_op_end_e:
	je da_op_end		; if we're done -->
	mov	al,','
	stosb
	testopt [asm_options], disasm_commablank
	jz .nospace
	mov al, 32
	stosb
.nospace:
	jmp	da14		;another operand


		; All operands done. Now handle prefixes:
		; OPSIZE (66h), ADDRSIZE (67h), WAIT, segment, REP, LOCK
da_op_end:

		; Now check whether O32 or A32 modifies the opcode.
da_modify_opcode_osize_asize:
	mov	cx, N_LTABO
	mov	bx, ltabo1
	mov	dx, 2*N_LTABO-2
	mov	ah, PRE32D
	call da23_osize_asize

	mov	cx, N_LTABA
	mov	bx, ltaba1
	mov	dx, 2*N_LTABA-2
	mov	ah, PRE32A
	call da23_osize_asize

		; Now check and loop for unused prefixes.
da21:
	mov	al, [preused]	; = flags that are used
	not	al		; = flags that are not used
	and	al, [preflags]	; = flags that are not used but present
	jz	da28		; if all present flags were used -->

	mov	cx, N_WTAB
	mov	bx, wtab1
	mov	dx, 2*N_WTAB-2
	mov	ah, PREWAIT
	test	al, ah
	jz @F
				; if there's a WAIT prefix hanging
	call da23_wait
	jne disbad2
	jmp da21

@@:
	mov	ah, PREASIZE
	test	al, ah
	jnz	da21_asize	; if it is a 67h prefix -->

	mov ah, PREOSIZE
	test al, ah
	jz da24			; if not osize -->
da21_osize:
		; check whether OSIZE applies (segreg push/pop)
	push di
	push ax
	mov ax, [index]
%if 0
	mov di, .msg_number
	call hexword
	push dx
	mov dx, .msg
	call putsz
	pop dx
	jmp .skipmsg
	usesection lDEBUG_DATA_ENTRY
.msg:	db "Index="
.msg_number:
	asciz "----h, base=",_4digitshex(SPARSE_BASE),"h.",13,10
	usesection lDEBUG_CODE
.skipmsg:
%endif
	mov di, o32prfxtab
	mov cx, O32P_AMOUNT
	repne scasw
	jmp da21_a_o_common

da21_asize:
		; check whether ASIZE applies to an implicit operand
	push	di
	push	ax
	mov	ax, [index]
	test	ah, ah
	jnz	da21_nota32prfx_nz
				; opcode index > FF, not in this list -->
	mov	di, a32prfxtab
	scasb			; xlatb ?
	je	@F		; yes --> (ZR)
	and	al, ~1		; clear the low bit (MOVSW -> MOVSB)
	mov	cx, A32P_LEN - 1
	repne	scasb		; scan table (low bit cleared)
@@:
da21_a_o_common:
da21_nota32prfx_nz:
	pop	ax
	pop	di
	jne	da21_notprfx	; not in the list -->

	or	[preused], ah	; mark it as used
	mov	cl, 4		; (ch = 0 because A32P_LEN / O32P_AMOUNT < 256)
%if A32P_LEN > 255 || O32P_AMOUNT > 255
 %error Remove optimisation
%endif
	call	moveover	; make space for "A32 " (or a16, osize)
_386_PM	push ax
	cmp ah, PREOSIZE	; is it OSIZE ?
	mov	ax, "A3"	;  start of "A32"
	jne @F			; no -->
	mov al, 'O'		; make it OSIZE (O32/O16)
@@:
	call	dis_lowercase	; al = "a"/"o" if lowercase option specified
%if _40COLUMNS
	mov si, word [mnemonofs]
	mov	word [line_out + si], ax
	mov	word [line_out + si + 2], "2 "
				; trail of A32/O32
_386_PM	pop ax
_386_PM	test byte [presizeflags], ah
_386_PM				; do we have 32-bit ASIZE/OSIZE ?
_386_PM	jnz da21_j0		; yes, keep a32/o32 (in 16-bit cs) -->
_386_PM	mov word [line_out + si + 1], "16"
				; no, make it a16/o16 (in 32-bit cs)
%else
	mov	word [line_out+MNEMONOFS], ax
	mov	word [line_out+MNEMONOFS+2], "2 "
				; trail of A32/O32
_386_PM	pop ax
_386_PM	test byte [presizeflags], ah
_386_PM				; do we have 32-bit ASIZE/OSIZE ?
_386_PM	jnz da21_j0		; yes, keep a32/o32 (in 16-bit cs) -->
_386_PM	mov word [line_out + MNEMONOFS + 1], "16"
				; no, make it a16/o16 (in 32-bit cs)
%endif
da21_j0:
	jmp da21

da21_notprfx: equ disbad2


da23_osize_asize: equ da23

da23_wait:
%if _PM
	or byte [presizeflags], ah
		; Fake that this flag is set for da23's check.
		;  If not _PM then presizeflags == preflags and
		;  PREWAIT is already set in that variable.
%endif

		; INP:	bx -> array of words to match against word [index]
		;	cx = number of array entries
		;	ah = preused flag to mark as used if match
		;	ah = presizeflags flag to check if in use
		;	dx = offset to add to di -> behind matched word
		; OUT:	ZR if one of the array entries matched,
		;	 byte [preused] |= INP:ah
		;	 if also flag in byte [presizeflags] was set,
		;	  mnemonic replaced
		;	NZ if no entry matched
		; CHG:	ax, cx, si
		;
		; Note:	If this is called with a 32-bit CS then the
		;	 OSIZE/ASIZE value in the byte [preflags] is
		;	 inverted to that in the byte [presizeflags].
		;	As we check the latter, for O16/A16 we will
		;	 not modify the mnemonics (keeping the 16-bit
		;	 default mnemonics) but will mark the prefix
		;	 as used by setting the flag in byte [preused].
		;	If there is no OSIZE/ASIZE in 32-bit CS then
		;	 the preused flag is also set (which does no
		;	 harm) and the 32-bit mnemonic is selected.
		;	Operation in 16-bit CS has the same result as
		;	 the prior handling, though it will always
		;	 scan for the affected opcodes, and set the
		;	 preused flag even if no prefix is used.
da23:
	push di
	mov di, bx
	 push ax
	mov ax, [index]
	repne scasw
	 pop ax
	jne .ret		; if not found in the list --> (NZ)
	or byte [preused], ah	; mark this (OSIZE, ASIZE or WAIT) prefix as used
	test byte [presizeflags], ah
				; is it O32, A32, or is WAIT present ?
	jz .ret			; no, do not modify mnemonic -->
	add di, dx		; replace the mnemonic with the modified name
	mov si, [di]
	add si, mnlist
	call showop		; copy instruction mnemonic
	cmp al, al		; ZR
.ret:
	pop di
	retn


da21_j1: equ da21_j0

disbad2:
	jmp	disbad

da24:
	test	al, PRESEG
	jz	da25		; if not because of a segment prefix -->
	mov	ax, [index]
	test	ah, ah
	jnz	disbad2		; if index > 256, it's none of these -->
	push	di
	mov	cx, SEGP_LEN
	mov	di, segprfxtab
	repne	scasb
	pop	di
	jne	disbad2		; if it's not on the list -->
	mov	cx, 3
	call	moveover
	push	di
%if _40COLUMNS
	mov di, line_out
	add di, word [mnemonofs]
%else
	mov	di, line_out+MNEMONOFS
%endif
	call	showseg		; show segment register
	mov	al, ':'
	testopt [asm_options], disasm_nasm
	jz	.notnasm
	mov	al, 32
.notnasm:
	stosb
	pop	di
	or	byte [preused], PRESEG	; mark it as used
da21_j2:
	jmp s	da21_j1

da25:
	test	al, PREREP
	jz	da26		; if not a REP prefix
	and	al, PREREP|PREREPZ
	or	[preused], al
	mov	ax, [index]
	test	ah, ah
	jnz	disbad3		; if not in the first 256 bytes
	and	al, ~1		; clear the low bit (MOVSW -> MOVSB)
	push	di
	mov	di, replist
	mov	cx, REP_SAME_LEN; scan those for REP first
	repne	scasb
	mov	si, mnlist+MN_REP
	je	da27		; if one of the REP instructions -->
	mov	cl, REP_DIFF_LEN; (ch = 0)
	repne	scasb
	jne	disbad3		; if not one of the REPE/REPNE instructions
	mov	si, mnlist+MN_REPE
	test	byte [preused], PREREPZ
	jnz	da27		; if REPE
	mov	si, mnlist+MN_REPNE
	jmp s	da27		; it's REPNE

disbad3:
	jmp	disbad

da26:
	test	al, PRELOCK
	jz	disbad3		; if not a lock prefix, either -->
	push	di
	mov	ax, [index]
	mov	di, locktab
	mov	cx, N_LOCK
	repne	scasw
	jne	disbad3		; if not in the approved list -->
	test	byte [preused], PRESEG
	jz	disbad3		; if memory was not accessed -->
	mov	si, mnlist+MN_LOCK
	or	byte [preused], PRELOCK

;	Slip in another mnemonic (REP or LOCK).
;	SI = offset of mnemonic, what should be
;	DI is on the stack.

da27:
	pop	di
	mov	cx, 8
%if _40COLUMNS
		; We cheat: Instead of moving things back after showop,
		;  which obtains the final true length of the mnemonic,
		;  we get the length here separately. This only works because
		;  the REP and LOCK prefixes are never suffixed by a W or D,
		;  so the length found here matches what showop will get.
	testopt [asm_options], disasm_no_indent
	jz @F
	mov cx, word [si - 2]	; get mnemonic length in low 4 bits
	and cx, 15		; extract the length
	inc cx			; allow for one blank
@@:
%endif
	push	si
	call	moveover
	pop	si
	push	di
	call	showop
	pop	di
	jmp s	da21_j2

;	Done with instruction.  Erase the size indicator, if appropriate.

da28:
	mov	cx, [sizeloc]	; -> size keyword, or zero if none
	jcxz	da28b		; if there was no size given -->
	mov	al, [disflags]
	test al, DIS_I_SHOWSIZ
	jnz	da28b		; if we need to show the size -->
	test al, DIS_I_KNOWSIZ
	jz	da28b		; if the size is not known already -->
	xchg cx, di		; di -> size keyword, cx -> behind disassembly
	mov	si,di		; si -> size keyword
	mov	al, 32		; scan for next blank
da28a:
	scasb			; skip size name
	jne	da28a		; if not done yet -->
		; (The above is the same as repne scasb, but
		;  has no effect on cx.)
	testopt [asm_options], disasm_nasm
	jnz .nasm
	add	di, byte 4	; skip 'PTR '
.nasm:
	xchg si, di		; di -> size keyword, si -> after size keyword
	sub	cx, si		; behind disassembly - after size keyword
				;  = length of disassembly part after keyword
	rep	movsb		; move the remainder of the line
				; di -> behind disassembly (size keyword deleted)

;	Now we're really done.  Print out the bytes on the left.

		; di -> behind disassembly
da28b:
	push di
	mov	di, line_out	; print start of disassembly line
	mov	ax, [u_addr + saSegSel]
				; print address
	call hexword		; segment/selector
	mov	al, ':'
	stosb			; colon
	_386_PM_o32	; mov eax, dword [u_addr]
	mov ax, word [u_addr]
%if _PM
	test byte [bCSAttr], 40h
	jz .16
	call hexword_high
.16:
%endif
	call hexword		; offset
	mov	al, 32
	stosb			; one blank
	mov	bx, [dis_n]
da28c:
%if _40COLUMNS
	mov si, line_out - 1
	add si, word [mnemonofs]
%else
	mov si,line_out+MNEMONOFS - 1
%endif
	sub si, di		; how many columns available for machine code
	shr si, 1		; how many bytes can be dumped
	cmp bx, si
	jbe da29		; if it's a short instruction which fits in one line
	sub bx, si		; bx = how many bytes to defer to next line(s)
	push bx
	mov bx, si		; bx = how many bytes fit in this line
	push di
	call disshowbytes	; dump bytes on this line
	call putsline_crlf	; append CR LF and display line
		; Note that MNEMONOFS is always even, and so is the length
		;  of the prefix (segment + colon + offset 4 or 8 + blank).
		; We determined how many bytes to dump by using line_out
		;  plus mnemonofs minus 1. Therefore we will always actually
		;  have two bytes left before the disassembly here. This is
		;  crucial for this call as it appends two bytes to the line.
		; We do not want the CR or LF to overwrite the disassembly
		;  text in the line_out buffer so we need two bytes of space.
	pop cx			; -> at column after segmented address
	pop bx			; = how many bytes left for subsequent line(s)
	mov di, line_out	; reset di
	sub cx, di		; = how many columns to skip
	mov al, 32
	rep stosb		; fill with blanks
	jmp short da28c		; loop for subsequent line(s) -->
da29:
	call disshowbytes	; dunp remaining bytes
da30:
	mov	al, 32		; pad to op code
%if _40COLUMNS
	mov cx, line_out
	add cx, word [mnemonofs]
%else
	mov	cx,line_out+MNEMONOFS
%endif
	sub	cx,di
	jc  da30_1		; (shouldn't happen)
	rep	stosb
da30_1:
	pop	di		; -> behind disassembly
	test byte [disflags],DIS_I_UNUSED
	jz	da32		; if we don't print ` (unused)'
	mov	si,unused
	cmp	byte [di-1], 32
	jne	da31		; if there's already a space here
	inc	si
da31:
	call showstring

;	Print info on minimal processor needed.

da32:
	mov al, [dismach]
	cmp al, [maxmachinetype]
	jbe @F
	mov [maxmachinetype], al
@@:
	push di
	mov di, obsinst
	mov cx, word [index]
	call showmach	;show the machine type, if needed
	pop di
	jcxz da32f_j1		; if no message -->

;	Print a message on the far right.

da32_tabto:
%if _40COLUMNS
	mov ax, line_out
	add ax, word [trailofs]
%else
	mov	ax, line_out+79
%endif
	sub	ax, cx
	push	cx
	call	tab_to		; tab out to the location
	pop	cx
	rep	movsb		; copy the string
da32z_j1:
	jmp	da32z		; done

da32f_j1:
	jmp da32f

%if _COND
		; Try dumping a condition status.
da32_cond:
 %if _COND_RDUMP_ONLY
	test al, DIS_F_SHOW	; (! DIS_F_SHOW|DIS_I_SHOW is negated here)
	jnz da32z_j1		; not showing conditionals message -->
 %endif
	mov si, word [condmsg]
	test si, si		; stored a message here ?
	jz da32z_j1		; no -->
	push di
	mov di, si
	mov cx, -1
	xor ax, ax
	repne scasb
	not cx
	dec cx			; get string length
	pop di
	jmp short da32_tabto
%else
da32_cond: equ da32z_j1
%endif

da32_xlatb:
	test al, DIS_F_SHOW	; (! DIS_F_SHOW|DIS_I_SHOW is negated here)
	jnz da32_cond
_no386	cmp byte [segmnt], 3
_no386	ja da32_cond		; if FS or GS on non-386 --> (invalid)
_no386	test byte [presizeflags], PRE32A
_no386	jnz da32_cond
	cmp byte [segmnt], 5
	ja da32_cond		; if invalid segment -->
	cmp word [index], 0D7h	; xlatb ?
	jne da32_cond
	mov ax, word [reg_ebx]
	mov word [addrr], ax
	mov ax, word [reg_ebx + 2]
	mov word [addrr + 2], ax; get address in (e)bx
	mov al, byte [reg_eax]
	add byte [addrr], al
	adc byte [addrr + 1], 0
	adc word [addrr + 2], 0	; add al
	mov byte [rmsize], 80h	; byte size
%if _MEMREF_AMOUNT
	setopt [disflags], DIS_I_M_SRC
%endif
	test byte [presizeflags], PRE32A
	jz @F
	setopt [disflags], DIS_I_SHOW_A32
				; set flag if 32-bit
	jmp @F


;	Dump referenced memory location.

da32f:
	mov	al,[disflags]
	xor	al, DIS_F_SHOW | DIS_I_SHOW
	test	al, DIS_F_SHOW | DIS_I_SHOW | DIS_I_DONTSHOW
		; (NZ if either _SHOW is clear, or _DONTSHOW is set)
	jnz	da32_xlatb	; if there is no memory location to show -->
_no386	cmp byte [segmnt], 3
_no386	ja da32_xlatb		; if FS or GS on non-386 --> (invalid)
_no386	testopt [disflags], DIS_I_SHOW_A32
_no386	jnz da32_xlatb
	cmp byte [segmnt], 5
	ja da32_xlatb		; if invalid segment -->
@@:
%if _PM
	push ax
%endif
	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	mov bx, [segrgaddr + bx]; get address of value
%if _PM
	pop ax
	call ispm
	jnz @F			; if 86 Mode, all segments readable -->
[cpu 286]
	verr word [bx]
	jnz da32_cond		; (NOT to da32_xlatb)
__CPU__
@@:
%endif
	push bx
%if _MEMREF_AMOUNT
	push word [bx]		; = segment/selector value
	call get_free_memref	; memrefs + bx -> the memref structure
	pop word [memrefs + bx + mrSegmentSelector]
	or byte [memrefs + bx + mrFlags], mrfMem
	testopt [disflags], DIS_I_M_SRC
	jz @F
	or byte [memrefs + bx + mrFlags], mrfMemSource
@@:
	testopt [disflags], DIS_I_M_DST
	jz @F
	or byte [memrefs + bx + mrFlags], mrfMemDest
@@:
%endif

%if _40COLUMNS
	mov ax, line_out - 8
	add ax, word [trailofs]
%else
	mov ax, line_out + 79 - 8
%endif
		; 8 is composed of:
		;	3 segreg name + colon
		;	4 low word offset
		;	1 equals sign
		; 79 means we write to the very right of an 80-columns
		;  display, up to the second-to-last column.
	mov cx, 2		; if byte then content uses 2 digits
%if _MEMREF_AMOUNT
	mov byte [memrefs + bx + mrLength], 1
				; if byte then length = 1
%endif
	cmp	byte [rmsize],0
	jl	da32h		; if byte
	jz	da32g		; if word
%if _MEMREF_AMOUNT
	add byte [memrefs + bx + mrLength], 2
				; add 2 bytes to length for high half of dword
%endif
	add cx, byte 4		; add 4 digits for high half of dword
da32g:
%if _MEMREF_AMOUNT
	inc byte [memrefs + bx + mrLength]
				; add 1 byte to length for the high byte
%endif
	inc cx
	inc cx			; add 2 digits for the high byte
da32h:
	testopt [disflags], DIS_I_DOUBLE_M
	jz @F
%if _MEMREF_AMOUNT
	mov dl, byte [memrefs + bx + mrLength]
	add byte [memrefs + bx + mrLength], dl
				; double the amount of bytes
%endif
	add cx, cx		; double the amount of digits
	inc cx			; plus one slot for the comma
@@:
	testopt [disflags], DIS_I_FAR_M
	jz @F
%if _MEMREF_AMOUNT
	add byte [memrefs + bx + mrLength], 2
				; add 2 bytes to the length for the segment
%endif
	add cx, 5		; 4 digits for segment plus 1 colon
@@:
%if _MEMREF_AMOUNT
	push word [addrr]
	pop word [memrefs + bx + mrOffset]
%endif
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
%if _MEMREF_AMOUNT
	push word [addrr + 2]
	pop word [memrefs + bx + mrOffset + 2]
	or byte [memrefs + bx + mrFlags], mrfA32
%endif
	add cx, 4		; add 4 digits for high word offset
@@:
%if _MEMREF_AMOUNT
	call calc_linear_memref_and_mark_nonfree
%endif
	sub ax, cx
	call tab_to		; CHG: ax, bx, cx, dx, di
	call showseg_uppercase_ax
				; ax = segment register name
	call dis_lowercase_refmem_w
	stosw
	mov al, ':'
	stosb
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
	mov ax, [addrr + 2]
	call hexword		; show high word of offset
@@:
	mov ax, [addrr]
	call hexword		; show offset

	testopt [disflags], DIS_I_SHOW_A32
	jz .isa16option		; if 16-bit addressing -->
	testopt [asm_options], disasm_a32_memref
	jmp .commonoption
.isa16option:
	testopt [asm_options], disasm_a16_memref
.commonoption:
	jnz @F
	pop bx
	jmp da32z
@@:

	mov	al,'='
	stosb
	call putsline
	mov di, line_out
..@uu_referenced_memory_access_start:
	pop bx
	push es
	mov es, [bx]
	_386_o32
	mov bx, [addrr]

	testopt [disflags], DIS_I_FAR_M
	jz .no_far_m
	mov si, 1
	cmp byte [rmsize], 0
	jl @F
	mov si, 2
	jz @F
	mov si, 4
@@:
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
[cpu 386]
	movzx esi, si
	mov al, byte [es:ebx + esi]
	mov ah, byte [es:ebx + esi + 1]
__CPU__
	jmp @FF
@@:
	mov al, byte [es:bx + si]
	mov ah, byte [es:bx + si + 1]
@@:
	mov si, es
	pop es
	push es
	call hexword
	mov al, ':'
	stosb
	mov es, si
.no_far_m:

	mov si, es
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
[cpu 386]
	mov al, [es:ebx]	; avoid a "mov ax,[-1]"
	cmp byte [rmsize], 0
	jl .displaybyte		; if byte -->
	mov ah, [es:ebx + 1]
	jz .displayword		; if word -->
	mov dl, [es:ebx + 2]	; avoid a "mov dx,[-1]"
	mov dh, [es:ebx + 3]
__CPU__
	jmp .displaydword	; is dword -->
@@:
	mov al, [es:bx]		; avoid a "mov ax,[-1]"
	cmp byte [rmsize], 0
	jl .displaybyte		; if byte
	mov ah, [es:bx + 1]
	jz .displayword		; if word
	mov dl, [es:bx + 2]	; avoid a "mov dx,[-1]"
	mov dh, [es:bx + 3]
.displaydword:
	pop es
	xchg ax, dx
	call hexword
	xchg ax, dx
	db __TEST_IMM8		; (skip pop)
.displayword:
	pop es
	call hexword
	jmp short .displayed	; done
.displaybyte:
	pop es
	call hexbyte		; display byte
.displayed:

	testopt [disflags], DIS_I_DOUBLE_M
	jz .no_double_m

	mov al, ','
	stosb

	push es
	mov es, si

	mov si, 1
	cmp byte [rmsize], 0
	jl @F
	mov si, 2
	jz @F
	mov si, 4
@@:
	testopt [disflags], DIS_I_SHOW_A32
	jz @F
[cpu 386]
	movzx esi, si
	mov al, [es:ebx + esi]	; avoid a "mov ax,[-1]"
	cmp byte [rmsize], 0
	jl .double_displaybyte	; if byte -->
	mov ah, [es:ebx + esi + 1]
	jz .double_displayword	; if word -->
	mov dl, [es:ebx + esi + 2]
				; avoid a "mov dx,[-1]"
	mov dh, [es:ebx + esi + 3]
__CPU__
	jmp .double_displaydword
@@:
	mov al, [es:bx + si]	; avoid a "mov ax,[-1]"
	cmp byte [rmsize], 0
	jl .double_displaybyte	; if byte -->
	mov ah, [es:bx + si + 1]
	jz .double_displayword	; if word -->
	mov dl, [es:bx + si + 2]; avoid a "mov dx,[-1]"
	mov dh, [es:bx + si + 3]
.double_displaydword:
	pop es
	xchg ax, dx
	call hexword
	xchg ax, dx
	db __TEST_IMM8		; (skip pop)
.double_displayword:
	pop es
	call hexword
	jmp short .double_displayed
.double_displaybyte:
	pop es
	call hexbyte		; display byte
.double_displayed:
.no_double_m:
..@uu_referenced_memory_access_end:

da32z:
	call	trimputs	; done with operand list

%if _MEMREF_AMOUNT
set_string_memref:
	test byte [disflags], DIS_F_SHOW
	jnz @F
.none_j:
	jmp .none
@@:

	mov ax, [index]
	test ah, ah
	jnz .none_j
	mov di, ppbytes.string	; list of string opcodes
	mov cx, ppbytes.string_amount
	repne scasb
	jne .none_j		; if not one of these -->
		; last entries are: 0A6h,0A7h,0AEh,0AFh
		;  corresponding to cmpsb, cmpsw/d, scasb, scasw/d
		; so cx = 0 means scasw, = 1 scasb, = 2 cmpsw, = 3 cmpsb
	mov al,byte [di+PPLEN-1]; get corresponding byte in ppinfo

	xor dx, dx
	push word [reg_ecx]
	pop word [string_memref_counter]
	test byte [presizeflags], PRE32A
	jz @F			; if 16-bit addressing -->
	mov dx, [reg_ecx + 2]	; = ecxh value
@@:
	mov word [string_memref_counter + 2], dx

	cmp cl, 4		; repetition differs for memory content ?
	jae @F			; no -->
	test byte [preflags], PREREP
	jz @F			; if not repeated -->

	test byte [presizeflags], PRE32A
	jz .isa16option		; if 16-bit addressing -->
	testopt [asm_options], disasm_a32_string
	jmp .commonoption
.isa16option:
	testopt [asm_options], disasm_a16_string
.commonoption:
	jnz .optiondone
	mov word [string_memref_counter], 1
	and word [string_memref_counter + 2], 0
	jmp @F

.optiondone:

	add cx, cx		; 0 = scasd, 2 = scasb, 4 = cmpsd, 6 = cmpsb
	test byte [presizeflags], PRE32D
	jnz .iso32
	inc cx			; point to word entry, not dword
.iso32:

	add cx, cx		; table entries are words, so 0..14

	test byte [preflags], PREREPZ
	jz .isrepne
	add cx, simulate_rep_sca_cmp.table_repe_offset
				; from 0..14 to 16..30
.isrepne:

	mov bx, cx		; = index into table
	mov dx, word [bx + simulate_rep_sca_cmp.table]
				; function to call
	test byte [presizeflags], PRE32A
	jz .isa16		; if 16-bit addressing -->
	dec dx			; a32 prefix is 1 byte lower than table entry
.isa16:

	_386_o32
	mov si, word [reg_esi]
	_386_o32
	mov di, word [reg_edi]
	_386_o32
	mov cx, word [reg_ecx]
	mov es, word [reg_es]
	push ax
	test al, PP_STRSRC	; cmps ?
	jz .issca		; no, no need to load ds -->

	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	mov bx, word [segrgaddr + bx]
				; si = address of segment/selector reg
	mov ds, [bx]

.issca:
	_386_o32
	mov ax, word [ss:reg_eax]

	testopt [ss:reg_efl], 400h
				; DF set ?
	jz .up
	std
.up:
	call dx			; simulate the repeated string op
	cld
	 push ss
	 pop ds
	 push ss
	 pop es

_386	push ecx
_386	pop cx
_386	pop dx

_386	test byte [presizeflags], PRE32A
_386	jnz .count32		; if 32-bit addressing -->
	xor dx, dx
.count32:
	sub word [string_memref_counter], cx
	sbb word [string_memref_counter + 2], dx

	pop ax
@@:

	test al, PP_STRSRC
	jz .no_src

_no386	cmp byte [segmnt], 3
_no386	ja .no_src		; if FS or GS on non-386 --> (invalid)
	cmp byte [segmnt], 5
	ja .no_src		; if invalid segment -->

	push ax
	mov al, [segmnt]	; segment number
	cbw
	shl ax, 1
	xchg ax, bx		; mov bx, ax
	pop ax
	mov si, word [segrgaddr + bx]
				; si = address of segment/selector reg
	mov di, reg_esi		; di = address of offset reg
	call init_string_memref
@@:			; entry: set memref to string source and mark
			;  (jump to .none afterwards if al is zero)
	or byte [memrefs + bx + mrFlags], mrfString | mrfStringSource
	call calc_linear_memref_and_mark_nonfree

.no_src:
	test al, PP_STRDEST | PP_STRSRC2
	jz .none
	mov si, reg_es		; si = address of segment/selector reg
	mov di, reg_edi		; di = address of offset reg
	call init_string_memref
	test al, PP_STRSRC2	; alternative string source ?
	mov al, 0		; (cause conditional branch to jump)
	jnz @B			; yes, set as string source -->
	or byte [memrefs + bx + mrFlags], mrfString | mrfStringDest
	call calc_linear_memref_and_mark_nonfree

.none:

%if _SYMBOLIC
display_symbol_memrefs:
	testopt [internalflags3], dif3_nosymbols_1 | dif3_nosymbols_2
	jnz .none_memref

	mov cx, [memrefs.free]
	test cx, cx
	jz .none_memref

	xor si, si
.loop_memref:
	push cx
	push si

	mov bx, si
	call get_memref_index_bx
	mov ax, word [memrefs + bx + mrFlags]
	mov dx, msg.memrefs_branchdirect
	test al, mrfBranchDirect
	jnz .gotmsg

	test byte [disflags], DIS_F_SHOW
	jz .next_memref

	mov dx, msg.memrefs_stringsource
	test al, mrfStringSource
	jnz .gotmsg
	mov dx, msg.memrefs_stringdest
	test al, mrfStringDest
	jnz .gotmsg
	mov dl, al
	and dl, mrfMemSource | mrfMemDest
	cmp dl, mrfMemSource | mrfMemDest
	mov dx, msg.memrefs_memsourcedest
	je .gotmsg
	mov dx, msg.memrefs_memsource
	test al, mrfMemSource
	jnz .gotmsg
	mov dx, msg.memrefs_memdest
	test al, mrfMemDest
	jnz .gotmsg
	mov dx, msg.memrefs_mem_unknown
	test al, mrfMem
	jnz .gotmsg
	mov dx, msg.memrefs_unknown
.gotmsg:

	; We do not call zz_detect_xms here because that is
	;  already done unconditionally during the initialisation
	;  of the function disasm.

	 push dx
	mov ax, [memrefs + bx + mrLinear]
	mov dx, [memrefs + bx + mrLinear + 2]

	mov cx, dx
	mov bx, ax
	nearcall binsearchmain		; search for matching symbol
	 pop dx
	jcxz .no_symbol
.loop_symbol:
	call putsz
	 push bx
	dualcall displaystring
	 push bx
	 push ax
	dualcall getfarpointer.main
	 pop di
	 pop es

	pop si
	push si

	push dx

	 push bx
	mov bx, si
	call get_memref_index_bx
	mov dx, word [memrefs + bx + mrOffset + 2]
	mov ax, word [memrefs + bx + mrOffset]

	cmp dx, word [es:di + smOffset + 2]
	mov dx, msg.memref_after_symbol.non_wrt
	jne .wrt_symbol
	cmp ax, word [es:di + smOffset]
	je .non_wrt_symbol
.wrt_symbol:
	mov dx, msg.memref_between_symbol.wrt
	call disp_message
	push ss
	pop es
	mov di, line_out
	mov ax, word [memrefs + bx + mrSegmentSelector]
	call hexword
	push bx
	push cx
	call putsline
	pop cx
	pop bx
	mov dx, msg.memref_after_symbol.wrt
.non_wrt_symbol:
	call disp_message

	 pop bx
	pop dx
.next_symbol:
	inc bx
	loop .loop_symbol

.no_symbol:
	push ss
	pop es
	push ss
	pop ds

.next_memref:
	pop si
	pop cx
	inc si
	loop .loop_memref_j
	jmp .none_memref

.loop_memref_j:
	jmp .loop_memref

.none_memref:
%endif

%if _DEBUG2
display_memrefs:
	mov cx, [memrefs.free]
	test cx, cx
	jz .none
	xor si, si
.loop:
	mov bx, si
	call get_memref_index_bx
	mov ax, word [memrefs + bx + mrFlags]
	mov dx, msg.memrefs_branchdirect
	test al, mrfBranchDirect
	jnz .gotmsg
	mov dx, msg.memrefs_stringsource
	test al, mrfStringSource
	jnz .gotmsg
	mov dx, msg.memrefs_stringdest
	test al, mrfStringDest
	jnz .gotmsg
	mov dl, al
	and dl, mrfMemSource | mrfMemDest
	cmp dl, mrfMemSource | mrfMemDest
	mov dx, msg.memrefs_memsourcedest
	je .gotmsg
	mov dx, msg.memrefs_memsource
	test al, mrfMemSource
	jnz .gotmsg
	mov dx, msg.memrefs_memdest
	test al, mrfMemDest
	jnz .gotmsg
	mov dx, msg.memrefs_mem_unknown
	test al, mrfMem
	jnz .gotmsg
	mov dx, msg.memrefs_unknown
.gotmsg:
	call putsz

	mov di, line_out
	mov ax, word [memrefs + bx + mrSegmentSelector]
	call hexword
	push word [memrefs + bx + mrOffset]
	mov al, ':'
	stosb
%if 1 || _PM
	mov ax, word [memrefs + bx + mrOffset + 2]
	test byte [memrefs + bx + mrFlags], mrfA32
	jz .16
	call hexword
.16:
%endif
	pop ax
	call hexword
	push si
	push cx
	mov si, msg.memrefs_length
	call copy_single_counted_string
	mov ax, word [memrefs + bx + mrLength + 2]
	test ax, ax
	jz @F
	call hexword
@@:
	mov ax, word [memrefs + bx + mrLength]
	call hexword
	call putsline_crlf
	pop cx
	pop si

	inc si
	dec cx
	jnz .loop
.none:
%endif	; _DEBUG2
%endif	; _MEMREF_AMOUNT


da_repeat:
%if _SYMBOLIC
	testopt [internalflags3], dif3_nosymbols_1 | dif3_nosymbols_2
	jnz .no_sym_between

	mov bx, word [u_addr + 4]
	_386_PM_o32
	mov dx, word [u_addr]
	call getlinear_32bit
	jc .no_sym_between

	; push dx
	push ax			; linear after instruction

	xchg dx, cx
	xchg ax, bx

	sub bx, 1
	sbb cx, 0		; cx:bx = end of range

	mov dx, word [u_lin_start + 2]
	mov ax, word [u_lin_start]
	add ax, 1
	adc dx, 0		; dx:ax = start of range

	nearcall binsearchmain
	pop ax
	; pop dx		; dx:ax = linear after instruction
	jcxz .no_sym_between

.loop_sym_between:
	; push dx
	push ax
	 push bx
	dualcall displaystring
	pop ax
	; pop dx

	 push bx
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

	; push dx
	push ax
	sub ax, word [es:di + smLinear]
	; sbb dx, word [es:di + smLinear + 2]

	; push dx
	push ax
	xor dx, dx
	add ax, word [es:di + smOffset]
	adc dx, word [es:di + smOffset + 2]
	cmp word [u_addr + 2], dx
	mov dx, msg.uu_after_symbol_between_1.non_wrt
	jne .sym_between.wrt
	cmp word [u_addr], ax
	je .sym_between.not_wrt
.sym_between.wrt:
	mov dx, msg.uu_after_symbol_between_1.wrt
	call disp_message

	push ss
	pop es
	mov di, line_out
	mov ax, word [u_addr + 4]
	call hexword
	push bx
	push cx
	call putsline
	pop cx
	pop bx

	mov dx, msg.uu_after_symbol_between_2.wrt
.sym_between.not_wrt:
	call disp_message
	pop ax
	; pop dx

	push ss
	pop es
	push di
	push bx
	push cx
	mov di, line_out
	call decword
	call putsline
	pop cx
	pop bx
	pop di
	pop ax
	; pop dx

	; push dx
	mov dx, msg.uu_after_symbol_between_3
	call disp_message
	; pop dx

	inc bx
	loop .loop_sym_between
.no_sym_between:
	push ss
	pop es
%endif

	mov	al, [disflags]
%if _IMMASM
	testopt [internalflags6], dif6_immasm
	jz .notimmasm
	test	al, DIS_I_UNUSED
	jnz	@F		; if " (unused)" was displayed -->
.notimmasm:
%endif
	test	al, DIS_F_REPT
	jz	@FF		; if not repeating -->
	test	al, DIS_I_UNUSED
	jnz	@F		; if " (unused)" was displayed -->
	test	al, DIS_I_MOV_SS
	mov	ax, [index]
	jz	.not_mov_to_ss	; not mov to ss -->

	testopt [asm_options], disasm_nec
	jnz @F			; allow mov from or to any segreg
		; DIS_I_MOV_SS is set, check for wo[index] == 8Eh;
		;  as we only want to match move *to* ss, not from (8Ch).
	cmp	ax, 8Eh		; move to seg reg?
	je	@F		; yes, it is mov to ss -->
.not_mov_to_ss:

	cmp ax, SGROUP4 + 0	; pop r/m ?
	jne .not_pop_rm
	cmp byte [regmem], 1100_0000b
				; long-form pop reg ?
	jae @F			; yes -->
.not_pop_rm:

	test ah, ah
	jnz .not_single_byte_opcode

	mov di, single_byte_opcodes_repeat_disassembly
	mov cx, single_byte_opcodes_repeat_disassembly.length
	testopt [asm_options], disasm_nec
	jz .not_nec
%if single_byte_opcodes_repeat_disassembly.length_nec \
     == single_byte_opcodes_repeat_disassembly.length + 2
	inc cx
	inc cx
%else
	mov cx, single_byte_opcodes_repeat_disassembly.length_nec
%endif
.not_nec:
	repne scasb
	jne	@FFF
@@:
	and	word [disflags], DIS_F_REPT|DIS_F_SHOW
	inc	byte [disrepeatcount]
	cmp	byte [disrepeatcount], 16
	jb	disasm.preserve_disrepeatcount

	mov dx, msg.uu_too_many_repeat
	call putsz
@@:
	retn

@@:
.not_single_byte_opcode:

	mov	bh, byte [disp8]
	cmp	ax, 0EBh	; unconditional short jump ?
	je	@BB		; yes, return -->
	_386_PM_o32	; and dword [dis_n], byte 0
	and	word [dis_n], byte 0
	call	disgetbyte
	mov	bl, 2		; displacement to skip a jmp short
	cmp	al, 0EBh	; jmp short ?
	je	@F		; yes -->
	mov	bl, 3		; displacement to skip a 16-bit jmp near
	cmp	al, 0E9h	; jmp near ?
	jne	@BB		; no, return -->
%if _PM
	test	byte [bCSAttr], 40h	; 32-bit code segment ?
	jz	@F		; no, 16-bit, use displacement 3 -->
	mov	bl, 5		; displacement to skip a 32-bit jmp near
%endif
@@:
	cmp	bh, bl		; right displacement ?
	jne	@BBB		; no -->

	and	word [disflags], DIS_F_REPT|DIS_F_SHOW
	xor	word [condmsg], \
	    (msg.condnotjump + DATASECTIONFIXUP) ^ (msg.condjump + DATASECTIONFIXUP)
	jmp	disasm.preserve_condmsg_and_disrepeatcount


%if _MEMREF_AMOUNT
simulate_rep_sca_cmp:

..@uu_simulate_scas_start:
	_386_a32
.repne_scasd:
	_386_o32
	repne
	scasw
	retn

	_386_a32
.repne_scasw:
	repne scasw
	retn

	_386_a32
.repne_scasb:
	repne scasb
	retn

	_386_a32
.repe_scasd:
	_386_o32
	repe
	scasw
	retn

	_386_a32
.repe_scasw:
	repe scasw
	retn

	_386_a32
.repe_scasb:
	repe scasb
	retn
..@uu_simulate_scas_end:

..@uu_simulate_cmps_start:
	_386_a32
.repne_cmpsd:
	_386_o32
	repne
	cmpsw
	retn

	_386_a32
.repne_cmpsw:
	repne cmpsw
	retn

	_386_a32
.repne_cmpsb:
	repne cmpsb
	retn

	_386_a32
.repe_cmpsd:
	_386_o32
	repe
	cmpsw
	retn

	_386_a32
.repe_cmpsw:
	repe cmpsw
	retn

	_386_a32
.repe_cmpsb:
	repe cmpsb
	retn
..@uu_simulate_cmps_end:


	usesection lDEBUG_DATA_ENTRY
	align 2, db 0
.table:
	dw .repne_scasd
	dw .repne_scasw
	dw .repne_scasb
	dw .repne_scasb
	dw .repne_cmpsd
	dw .repne_cmpsw
	dw .repne_cmpsb
	dw .repne_cmpsb
.table_repe_offset: equ $ - .table
	dw .repe_scasd
	dw .repe_scasw
	dw .repe_scasb
	dw .repe_scasb
	dw .repe_cmpsd
	dw .repe_cmpsw
	dw .repe_cmpsb
	dw .repe_cmpsb

	usesection lDEBUG_CODE
%endif


;	Here are the routines for printing out the operands themselves.
;	Immediate data (OP_IMM)

dop_imm:
	cmp	ah, 0
	jl	dop03		; if just a byte -->
	pushf
	test	byte [disflags], DIS_I_SHOWSIZ
	jz	.nosize		; if we don't need to show the size -->
	call	showsize
.nosize:
	call	disgetword
%if _IMMASM
	mov word [immasm_imm16], ax
%endif
	popf			; ZF
	je	hexword		; if just a word
	jmp	disp32.ax

dop03:
	call	disgetbyte	; print immediate byte
	jmp	hexbyte


;	Memory offset reference (OP_MOFFS)

dop_moffs:
	mov	al, 5
	test	byte [presizeflags], PRE32A
	jnz	.32		; if 32-bit addressing -->
	inc	ax
.32:
	mov	[regmem], al
	jmp s	dop05


;	MOD R/M (OP_RM)

dop_rm:
	call getregmem
	cmp	al,0c0h
	jae	dop33		; if pure register reference -->

	testopt [disflags], DIS_I_M_ALWAYS_16
	jz @F
	mov ah, 0		; word size (m16)
@@:

dop05:			; <--- used by OP_M, OP_M64, OP_M80, OP_MOFFS
	mov byte [rmsize], ah	; save r/m size
	testopt [disflags], DIS_I_NOSIZ
	jnz @F
	call showsize		; print out size
	call showptr		; append "PTR " (if not NASM syntax)
@@:
dop06:			; <--- used by OP_MXX, OP_MFLOAT, OP_MDOUBLE
	or	byte [preused],PRESEG	; needed even if there's no segment override
					; because handling of LOCK prefix relies on it
	mov	al, '['
	call	stosb_nasm

	test byte [preflags],PRESEG
	jz	dop07			;if no segment override
	call showseg		;print segment name
	mov	al,':'
	stosb
dop07:
	mov	al,[regmem]
	and	al,0c7h
	or	byte [preused],PREASIZE
	test	byte [presizeflags],PRE32A
	jnz	dop18		;if 32-bit addressing
	or	byte [disflags],DIS_I_SHOW	;we'd like to show this address
	and word [addrr], 0	; zero out the address initially
	xchg	ax,bx		;mov bx,ax
	call	store_opensqubracket
	cmp	bl,6
	je	dop16		;if [xxxx]
	and	bx,7
	mov	bl,[rmtab+bx]
	test	bl,8
	jnz	dop09		;if BX
	test	bl,4
	jz	dop11		;if not BP
	mov	ax,'BP'
	mov	cx,[reg_ebp]
	call da_set_default_ss
	jmp dop10


		; INP:	al = 2 * register number
		;	[preflags] & PRESEG set if segment overridden
		;	else,
		;	 byte [segmnt] = initialised to 3 (ds)
		; OUT:	no action if register number not for esp or ebp
		;	no action if segment overridden
		;	otherwise,
		;	 byte [segmnt] -= 1, resulting in 2 (ss)
da_set_default_ss_if_esp_ebp:
	cmp al, 2 * 4
	je @F
	cmp al, 2 * 5
	jne @FF
@@:
da_set_default_ss:
	test byte [preflags], PRESEG
	jnz @F			; if segment override -->
	dec byte [segmnt]	; default is now SS
@@:
	retn


dop09:
	mov	ax,'BX'		;BX
	mov	cx,[reg_ebx]

dop10:
	mov	[addrr],cx	;print it out, etc.
	call	dis_stosw_lowercase
	test	bl,2+1
	jz	dop13		;if done
	mov	al,'+'
	stosb
dop11:
	mov	ax,'SI'
	mov	cx,[reg_esi]
	test	bl,1
	jz	dop12		;if SI
	mov	al,'D'		;DI
	mov	cx,[reg_edi]

dop12:
	add	[addrr], cx	; print it out, etc.
	call	dis_stosw_lowercase
dop13:
	test	byte [regmem], 0C0h
	jz s	dop17		; if no displacement -->
	test	byte [regmem], 80h
	jnz	dop15		; if word displacement -->
	call	disgetbyte
	cbw
	add	[addrr], ax
	cmp	al, 0
	mov	ah, '+'
	jge	dop14		; if not negative -->
	mov	ah, '-'
	neg	al
dop14:
	mov	[di], ah
	inc	di
	call	hexbyte		; print the byte displacement
	jmp s	dop17		; done -->

dop15:
	mov	al, '+'
	stosb
dop16:
	call	disgetword
	add	[addrr], ax
	call	hexword		; print word displacement

dop17:
	mov	al, ']'
	stosb
	retn

;	32-bit MOD REG R/M addressing.

dop18:
	or word [disflags], DIS_I_SHOW | DIS_I_SHOW_A32
	and word [addrr], 0
	and word [addrr + 2], 0	; zero out the address initially
	cmp al, 5		; mod=0 and r/m=5 ?
	je dop19		; yes, just a disp32 address -->
	push	ax
	and	al, 7
	cmp	al, 4
	jne	dop20		; if no SIB -->
	call	disgetbyte	; get and save it
	mov	[sibbyte], al
dop20:
	pop	ax
	test	al, 80h
	jnz	dop22		; if disp32 -->
	test	al, 40h
	jz	dop23		; if no disp8 -->
	call	disgetbyte

	cbw
	cwd
	add word [addrr], ax
	adc word [addrr + 2], dx

	cmp	al, 0
	jge	dop21		; if not negative -->
	neg	al
	mov	byte [di], '-'
	inc	di
dop21:
	call	hexbyte
	jmp s	dop22a		; done -->

disp32_add_to_addrr:
	call disgetword
	add word [addrr], ax
	push ax
	pushf
	call disgetword
	popf
	adc word [addrr + 2], ax
	call hexword
	pop ax
	jmp hexword

dop22:
	call	disp32_add_to_addrr
				; print disp32

dop22a:
	call	store_plus

dop23:
	mov	al,[regmem]
	and	al,7
	cmp	al,4
	jne	dop28		;if no SIB
	mov al, [sibbyte]
	mov ah, al
	and ax, 00_111_000_00_000_111b
				; ah = index << 3, al = base
	cmp ah, 4 << 3		; index encodes esp ?
	je dop_sib_index_4	; yes, use base only -->
		; When this branch is taken, the scale is ignored.
		; This is typically used only for encoding [esp]
		;  and [esp + x] but is a valid encoding even for
		;  other base registers. So better support it.
		; (This used to be a special case for SIB == 24h
		;  only because it doesn't usually occur else.
		;  The other cases were rejected in dop25.)

	call dop_is_mod_0_and_base_5
	jnz dop24		; if not mod=0 base=5 -->
	call disp32_add_to_addrr; show 32-bit displacement instead of [EBP]
	jmp dop25		; and handle the scale and index -->


		; INP:	al = low 3 bits of SIB byte (= base)
		;	byte [regmem] = ModR/M byte
		; OUT:	NZ if not mod=0 base=5 special case
		;	ZR else
		; REM:	base=5 usually encodes [ebp], and mod=0
		;	 usually encodes no displacement field.
		;	 If both are true however, then the
		;	 special case is true: there is no base
		;	 register and a 32-bit displacement.
dop_is_mod_0_and_base_5:
	cmp al, 5
	jne @F			; NZ if not base=5 -->
	test byte [regmem], 1100_0000b
				; NZ if not mod=0
@@:
	retn


dop_sib_index_4:
		; The mod=0 base=5 special case and the
		;  index=4 special case can occur both
		;  together. That is a SIB encoding of
		;  a 32-bit displacement without any
		;  index or base registers.
		;
		; This was noted on stackoverflow.com by
		;  Peter Cordes: "x86-32 has 2 redundant
		;  ways to encode [0x123456], i.e. no-base
		;  + disp32: with or without a SIB byte,
		;  because SIB has an encoding for no-base
		;  and no-index." (There is a use for this
		;  distinction and thus the SIB form only
		;  in 64-bit mode but it is accepted as a
		;  valid alternative encoding even for us.)
		;
		; - https://stackoverflow.com/questions/48124293/can-rip-be-used-with-another-register-with-rip-relative-addressing/48125453#48125453
	call dop_is_mod_0_and_base_5
	jnz dop28		; if not mod=0 base=5 -->
		; fall through to dop19

dop19:
	call	store_opensqubracket
	call	disp32_add_to_addrr
				; display 32-bit offset
dop17_j1:
	jmp dop17

add_reg32_to_addrr:
	mov cx, 1
add_reg32_times_cx_to_addrr:
	push ax
	push bx
	xchg ax, bx
	mov bx, word [reg32addr + bx]
@@:
	mov ax, word [bx]
	add word [addrr], ax
	mov ax, word [bx + 2]
	adc word [addrr + 2], ax
	loop @B
	pop bx
	pop ax
	retn

dop24:
	call store_opensqubracket_e
	call showreg16		; show 16-bit register name (number in AL)
	call da_set_default_ss_if_esp_ebp
	call add_reg32_to_addrr
	mov	al, ']'
	call	stosb_notnasm
dop25:
	call	store_plus

	mov al, [sibbyte]
	call da_get_bits_3_to_5	; al = index
		; (In dop23 we already checked this is not = 4
		;  which is a special escaping encoding.)

	call store_opensqubracket_e

	mov ah, [sibbyte]
	mov cx, 1
	test ah, 0C0h
	jz @F
	inc cx
	test ah, 80h
	jz @F
	mov cl, 4
	test ah, 40h
	jz @F
	mov cl, 8
@@:

	call showreg16
	push cx
	call add_reg32_times_cx_to_addrr
	pop cx
	dec cx			; = 0-based scale
	jz dop27		; if scale == 1 (S=00b) -->
	mov al, '*'
	stosb
	mov al, cl
	add al, '1'		; from 0-based to '1'-based
dop26:
	stosb
dop27:
dop17_j2:
	jmp s dop17_j1

;	32-bit addressing without SIB

dop28:
	call store_opensqubracket_e
	call showreg16
	call da_set_default_ss_if_esp_ebp
	call add_reg32_to_addrr
	jmp short dop27

		; Store '[' if not NASM syntax,
		;  then (regardless of syntax) store 'E'
		; INP:	di-> buffer
		; OUT:	di-> behind "[E" or 'E'
		; CHG:	-
		;
		; The 'E' is lowercased if that option is selected.
store_opensqubracket_e:
	push ax
	call store_opensqubracket
	mov al, 'E'
	call dis_lowercase
	stosb
	pop ax
	retn

		; Store '[' if not NASM syntax
		; INP:	di-> buffer
		; OUT:	di-> behind '[' if not NASM syntax
		; CHG:	al
store_opensqubracket:
	mov	al, '['

		; Store al if not NASM syntax
		; INP:	di-> buffer
		; OUT:	di-> behind stored byte if not NASM syntax
		; CHG:	-
stosb_notnasm:
	testopt [asm_options], disasm_nasm
	jnz	.ret
	stosb
.ret:	retn

		; Store '+' if NASM syntax
		; INP:	di-> buffer
		; OUT:	di-> behind '+' if NASM syntax
		; CHG:	al
store_plus:
	mov	al, '+'

		; Store al if NASM syntax
		; INP:	di-> buffer
		; OUT:	di-> behind stored byte if NASM syntax
		; CHG:	-
stosb_nasm:
	testopt [asm_options], disasm_nasm
	jz	.ret
	stosb
.ret:	retn


;	Memory-only reference (OP_M)

dop_m:
	call	getregmem
	cmp	al, 0C0h
	jb	dop05		; if it's what we expect -->

		; it's a register reference
disbad1:jmp	disbad		; this is not supposed to happen -->

;	Register reference from MOD R/M part (OP_R_MOD)

dop_r_mod:
	call getregmem
	cmp	al,0c0h
	jb	disbad1		;if it's a memory reference
	jmp s	dop33

;	Pure register reference (OP_R)

dop_r:
	call getregmem_r

dop33:				; <--- used by OP_RM, OP_R_MOD and OP_R_ADD
	and	al,7			;entry point for regs from MOD R/M, and others
	mov	cl,[disflags2]
	or	[disflags],cl	;if it was variable size operand, the size
						;should now be marked as known.
	cmp	ah,0
	jl	dop35			;if byte register
	jz	dop34			;if word register
dop33a:
	cmp ah, 20h		; qword register (MMX) ?
	je dop35_1		; -->
	push ax
	mov al, 'E'
	call dis_lowercase
	stosb
	pop ax
	;mov	byte [di],'E'	;enter here from OP_ECX
	;inc	di
dop34:
	add	al,8
dop35:
	cbw
	shl	ax,1
	xchg ax,bx			;mov bx,ax
	mov	ax,[rgnam816+bx];get the register name
	jmp	dis_stosw_lowercase

dop35_1:
	push ax
	mov ax, "MM"
	call dis_stosw_lowercase
	pop ax
	add al, '0'
	stosb
	retn

;	Register number embedded in the instruction (OP_R_ADD)

dop_r_add:
	mov	al,[instru]
	jmp s	dop33

;	AL or AX or EAX (OP_AX)

dop_ax:
	mov	al,0
	jmp s	dop33

		; QWORD mem (OP_M64)
		; This operand type is used by CMPXCHG8B, FILD and FISTP.
dop_m64:
	;mov ax, 'Q'		; print "QWORD"
	mov ah, 20h		; size QWORD
	jmp s dop40

		; FLOAT (=REAL4) mem (OP_MFLOAT)
dop_mfloat:
	mov	ax, "FL"
	call	dis_stosw_lowercase
	mov	ax, "OA"
	call	dis_stosw_lowercase
	mov	ax, "T "
	jmp short dop38c

;	DOUBLE (=REAL8) mem (OP_MDOUBLE).

dop_mdouble:
	mov	ax, "DO"
	call	dis_stosw_lowercase
	mov	ax, "UB"
	call	dis_stosw_lowercase
	mov	al, 'L'
	call	dis_lowercase
	stosb
	mov	ax, "E "
 dop38c:
	call	dis_stosw_lowercase
	call	showptr
	jmp s	dop42a

;	TBYTE (=REAL10) mem (OP_M80).

dop_m80:
	mov	ax,0FF00h+'T'	;print 'T' + "BYTE"
	call dis_lowercase
	stosb
dop40:
	call getregmem
	cmp	al,0c0h
	jae	disbad5		; if it's a register reference
	or	byte [disflags], DIS_I_DONTSHOW
				; don't show this
	jmp	dop05

%if 0
	; Far memory (OP_FARMEM).
	; This is either a FAR16 (DWORD) or FAR32 (FWORD) pointer.
dop_farmem:
	call	dischk32d
	jz	dop41a		; if not dword far
	call	showdword
dop41a:
	mov	ax, "FA"	; store "FAR "
	call	dis_stosw_lowercase
	mov	ax, "R "
	call	dis_stosw_lowercase
%endif

;	mem (OP_MXX).

dop_mxx:
	or	byte [disflags], DIS_I_DONTSHOW
				; don't show this
dop42a:
	call	getregmem
	cmp	al,0c0h
	jb	dop06		; mem ref, don't show size -->
disbad5:
	jmp	disbad

	; Far immediate (OP_FARP). Either FAR16 or FAR32.
dop_farimm:
	call disgetword
	push ax
	call dischk32d
	jz dop44_word		; if not 32-bit address

dop44_dword:
%if _PM
	test byte [bCSAttr],40h	; for 16-bit code segments
	jnz @F			; no need to display "WORD "
%endif
	call showdword
@@:

	call disgetword
	push ax
	jmp dop44_common

dop44_word:
%if _PM
	test byte [bCSAttr],40h	; for 32-bit code segments
	jz @F			; no need to display "DWORD "
	call showword
@@:
%endif

dop44_common:
	call disgetword
%if _MEMREF_AMOUNT
	call get_free_memref
	mov word [memrefs + bx + mrSegmentSelector], ax
%endif
	call hexword
%if _IMMASM
	mov word [immasm_far_target + 4], ax
	xor ax, ax
	mov word [immasm_far_target + 2], ax
%endif
	mov	al,':'
	stosb
	call dischk32d
	jz	dop45		;if not 32-bit address
	pop	ax
%if _IMMASM
	mov word [immasm_far_target + 2], ax
%endif
%if _MEMREF_AMOUNT
	mov word [memrefs + bx + mrOffset + 2], ax
	or byte [memrefs + bx + mrFlags], mrfA32
%endif
	call hexword
dop45:
	pop	ax
%if _IMMASM
	mov word [immasm_far_target], ax
%endif
%if _MEMREF_AMOUNT
	mov word [memrefs + bx + mrOffset], ax
	or byte [memrefs + bx + mrFlags], mrfBranchDirect
	call calc_linear_memref_and_mark_nonfree
%endif
	jmp hexword


%if _COND
		; INP:	[presizeflags] & PRE32A, d[reg_ecx]
		; OUT:	dx:ax = (e)cx
cond_get_ecx:
	mov ax, word [reg_ecx]
	test byte [presizeflags], PRE32A	; A32 ?
	mov dx, word [reg_ecx+2]
	jnz .ecx
	xor dx, dx
.ecx:
	retn

		; INP:	ax = 0..15 condition code, else invalid
		; OUT:	w[condmsg] set as appropriate
cond_handle:
	cmp ax, 15
	ja .return
	mov cx, word [reg_efl]	; get flags
	mov bx, ax
	and bl, ~1		; make even
	and al, 1		; 1 if negated condition
	cmp bl, 12		; L/GE or LE/G?
	jae .specific		; yes -->

	test cx, [cond_table+bx]; flag(s) set ?
	jmp short .jump_ZF	; NZ if (normal) condition true -->

.specific:
	cmp bl, 14
	jb .L_GE

		; Handle LE/NG and G/NLE conditions.
		; The former says ZF | (OF ^ SF).
.LE_G:
	test cl, 40h		; ZF | ..
	jnz .jump_true

		; Handle L/NGE and GE/NL conditions.
		; The former says OF ^ SF.
.L_GE:
	and cx, 880h		; OF ^ SF
	jz .jump_false		; both clear -->
	xor cx, 880h
.jump_ZF:
	jz .jump_false		; both set --> (or ZR: (normal) condition false)
.jump_true:
	xor al, 1		; (negating ^ raw truth) = cooked truth
.jump_false:
	test al, al		; true ?
	jnz .msg_jumping	; yes -->

.msg_notjumping:
	mov word [condmsg], msg.condnotjump
.return:
	retn

.msg_jumping:
	mov word [condmsg], msg.condjump
	retn
%endif


;	8-bit relative jump (OP_REL8)

dop_rel8:
%if _COND
	mov ax, word [index]
	cmp ax, 0E3h
	ja .cond_done		; no conditional jump -->
	jb .cond_noncx		; not jcxz, check for other -->

	call cond_get_ecx
	or ax, dx
	jz .cond_msg_jumping
.cond_msg_notjumping:
	call cond_handle.msg_notjumping
	jmp short .cond_done

.cond_msg_jumping:
	call cond_handle.msg_jumping
	jmp short .cond_done

.cond_noncx:
	cmp al, 0E0h
	jb .cond_nonloop	; not loop, check for other -->

	push ax
	call cond_get_ecx
	dec ax			; = 0 if cx is 1
	or ax, dx		; = 0 if cx is 1 and ecx is cx
	pop ax
	jz .cond_msg_notjumping	; if (e)cx is 1 -->
	cmp al, 0E2h
	je .cond_msg_jumping	; loop without additional condition -->
	xor al, 0E0h^75h	; E0h (loopnz) to 75h (jnz),
				;  E1h (loopz) to 74h (jz)

.cond_nonloop:
	sub al, 70h		; (ah = 0)
	call cond_handle	; call common code (checks for ax < 16)
.cond_done:
%endif
	call disgetbyte
	cbw
	mov byte [disp8], al
	jmp dop48

;	16/32-bit relative jump (OP_REL1632)

dop_rel1632:
%if _COND
	mov ax, word [index]
	sub ax, SPARSE_BASE+80h
	call cond_handle
%endif
	call disgetword
	call dischk32d
	jz dop48_near		; if not 32-bit offset
	xchg ax, dx	; mov dx, ax
	call disgetword

	cmp word [index], 00E8h
	je .not_show_keyword	; no need to distinguish NEAR call -->
		; ax:dx between FFFFh:FF80h (-128) .. 0000h:007Fh (127):
		; == show "NEAR" keyword
		;
		; Note:	This is not entirely correct. If a jump short is
		;	 used, the actual opcode is shorter, thus the
		;	 exact distance that can be reached by the jump short
		;	 differs from what the jump near can reach with
		;	 a rel16/32 displacement between -128..127.
	cmp ax, -1
	je .checkminus
	test ax, ax
	jnz .not_show_keyword
.checkplus:
	cmp dx, byte 127
	jg .not_show_keyword
	cmp dx, 0
	jl .not_show_keyword
	jmp .show_keyword

.checkminus:
	cmp dx, byte -128
	jl .not_show_keyword
	cmp dx, 0
	jge .not_show_keyword

.show_keyword:
	testopt [asm_options], disasm_show_near
	jnz .not_show_keyword
	call dop_show_near
.not_show_keyword:

%if _PM
	test byte [bCSAttr],40h	; for 32-bit code segments
	jnz @F			; no need to display "DWORD "
%endif
	push ax
	call showdword
	pop ax
@@:

	mov	bx,[u_addr+0]
	add	bx,[dis_n]
	adc	ax,[dis_n + 2]
	add	dx,bx
%if _PM
	adc	ax,[u_addr+2]
%else
	adc	ax, 0
%endif
%if _MEMREF_AMOUNT
	call get_free_memref
	mov word [memrefs + bx + mrOffset + 2], ax
	or byte [memrefs + bx + mrFlags], mrfA32
%endif
	call hexword
	xchg ax,dx
	jmp s	dop_branch_word


dop48_near:
	cmp word [index], 00E8h
	je @F			; no need to distinguish NEAR call -->
		; ax between FF80h (-128) .. 007Fh (127):
		; == show "NEAR" keyword
		;
		; Note:	This is not entirely correct. If a jump short is
		;	 used, the actual opcode is shorter, thus the
		;	 exact distance that can be reached by the jump short
		;	 differs from what the jump near can reach with
		;	 a rel16/32 displacement between -128..127.
	cmp ax, byte -128
	jl @F
	cmp ax, byte 127
	jg @F
	testopt [asm_options], disasm_show_near
	jnz @F
	call dop_show_near
@@:

%if _PM
	test byte [bCSAttr],40h	; for 16-bit code segments
	jz @F			; no need to display "WORD "
	push ax
	call showword
	pop ax
@@:
%endif

dop48:
	cwd
	add ax, word [u_addr]
%if _PM
	adc dx, word [u_addr + 2]
%else
	adc dx, 0
%endif
	add ax, word [dis_n]
	adc dx, word [dis_n + 2]
	call dischk32d		; 32-bit opsize ?
	jz .16			; no -->
	xchg ax, dx
	call hexword		; yes, display high word
	xchg ax, dx
%if _MEMREF_AMOUNT
	call get_free_memref
	mov word [memrefs + bx + mrOffset + 2], dx
	or byte [memrefs + bx + mrFlags], mrfA32
%endif
.16:

dop_branch_word:
%if _MEMREF_AMOUNT
	call get_free_memref
	mov word [memrefs + bx + mrOffset], ax
	push word [u_addr + saSegSel]
	pop word [memrefs + bx + mrSegmentSelector]
	or byte [memrefs + bx + mrFlags], mrfBranchDirect
	call calc_linear_memref_and_mark_nonfree
%endif
	jmp hexword		; call hexword and return


%if _MEMREF_AMOUNT
		; INP:	word [memrefs.free]
		; OUT:	bx = byte index into memref array
		; CHG:	-
		; STT:	ss = ds = es
get_free_memref:
	mov bx, [memrefs.free]
get_memref_index_bx:
	cmp bx, _MEMREF_AMOUNT
	jae @F
	add bx, bx
	add bx, bx
	add bx, bx
	add bx, bx
%if MEMREF_size != 16
 %error Adjust multiplication
%endif
	retn

@@:
	mov dx, msg.memrefs_invalid_internal
	call putsz
	jmp cmd3


		; INP:	word [memrefs.free]
		;	[memrefs] array entry
		; OUT:	NC if valid segmented address (getlinear succeeded),
		;	 dword [memrefs + x + mrLinear] filled
		;	 word [memrefs.free] incremented
		;	CY if invalid address,
		;	 [memrefs + x] re-initialised
		;	 word [memrefs.free] left unmodified
		; CHG:	-
		; STT:	ss = ds = es
calc_linear_memref_and_mark_nonfree:
	push ax
	push bx
	_386_PM_o32
	push dx
	call get_free_memref
	push bx
	_386_PM_o32
	mov dx, word [memrefs + bx + mrOffset]
	mov bx, word [memrefs + bx + mrSegmentSelector]
	call getlinear_32bit
	pop bx
	jc .error

	mov word [memrefs + bx + mrLinear], ax
	mov word [memrefs + bx + mrLinear + 2], dx
	inc word [memrefs.free]

	; clc
.return:
	_386_PM_o32
	pop dx
	pop bx
	pop ax
	retn

.error:
	push di
	lea di, [memrefs + bx]
	call init_one_memref
	pop di
	stc
	jmp .return


		; INP:	di -> memref to initialise
		; OUT:	ax = 0
		;	di -> past initialised memref
		; CHG:	-
		; STT:	ss = ds = es
init_one_memref:
	xor ax, ax

		; INP:	di -> memref to initialise
		;	ax = 0
		; OUT:	di -> past initialised memref
		; CHG:	-
		; STT:	ss = ds = es
.ax_already_zero:
	stosw				; zero-initialise all memrefs
	stosw				; mrLinear
	stosw
	stosw				; mrOffset
	stosw				; mrSegmentSelector
	stosw				; mrFlags
	inc ax
	stosw
	dec ax
	stosw				; mrLength = 1
	retn


		; INP:	si = address of segreg (reg_cs, reg_ds, etc)
		;	di = address of index reg (reg_esi or reg_edi)
		;	[memrefs]
		;	[presizeflags]
		;	byte [index] = which opcode
		;	[reg_ecx]
		;	[reg_efl] & 400h = Direction Flag
		; OUT:	memrefs + bx -> current memref (partially filled)
		; CHG:	bx, si, di, cx, dx
init_string_memref:
	call get_free_memref
	push word [si]		; get segment/selector
	pop word [memrefs + bx + mrSegmentSelector]
				; store segment/selector
	push word [di]
	pop word [memrefs + bx + mrOffset]
				; store low word of offset
	mov cx, word [string_memref_counter + 2]
	mov dx, word [string_memref_counter]

	test byte [presizeflags], PRE32A
	jz @F			; if 16-bit addressing -->
	push word [di + 2]
	pop word [memrefs + bx + mrOffset + 2]
				; store high word of offset
	or byte [memrefs + bx + mrFlags], mrfA32
				; remember that it is a32
@@:

	test byte [preflags], PREREP
	jnz @F			; if to take (e)cx repetitions -->
	mov dx, 1
	xor cx, cx		; cx:dx = 1, just one element
@@:

	push ax
	mov ax, 1
	test byte [index], 1	; element size is byte ?
	jz @F			; yes, ax = 1
	inc ax			; = 2
	test byte [presizeflags], PRE32D
				; element size is word ?
	jz @F			; yes, ax = 2
	add ax, ax		; = 4
@@:

	cmp al, 1		; byte size ?
	je @F			; yes, do not multiply -->
	add dx, dx
	adc cx, cx		; * 2, word to byte
	jc .carry_counter
	cmp al, 2		; word size ?
	je @F			; yes, done multiplying -->
	add dx, dx
	adc cx, cx		; * 4, dword to byte
	jnc @F
.carry_counter:
	mov dx, -1
	mov cx, dx		; in case the counter is large
		; A carry out of an a16 address is not yet handled.
		;  Note that eg cx=8000h a16 movsw may be valid to
		;  copy exactly 64 KiB. Likewise, technically a
		;  size of exactly 4 GiB is valid; however, our
		;  memref format cannot store that.
@@:
	mov word [memrefs + bx + mrLength], dx
	mov word [memrefs + bx + mrLength + 2], cx

	testopt [reg_efl], 400h	; DF set ?
	jz @F

	add word [memrefs + bx + mrOffset], ax
	adc word [memrefs + bx + mrOffset + 2], 0
	sub word [memrefs + bx + mrOffset], dx
	sbb word [memrefs + bx + mrOffset + 2], cx

@@:
	pop ax
	retn
%endif


;	Check for ST(1) (OP_1CHK).

dop49:
	pop	ax		;discard return address
	mov	al,[regmem]
	and	al,7
	cmp	al,1
	je	dop50		;if it's ST(1)
	jmp	da14		;another operand (but no comma)

dop50:
	jmp	da_op_end	; end of list -->

;	ST(I) (OP_STI).

dop_sti:
	mov al, byte [regmem]
	and al, 7
	xchg ax, bx		;mov bx,ax
	mov ax, 'ST'
	call dis_stosw_lowercase; store ST(bl)
	mov al, '('
	stosb
	mov ax, '0)'
	or al, bl
	stosw
	retn

;	CRx (OP_CR).

dop_cr:
	mov	bx,'CR'
	call getregmem_r
	cmp	al,4
	ja	disbad4		;if too large
	jne	dop52a
	mov	byte [dismach],5	;CR4 is new to the 586
dop52a:
	cmp	word [index],SPARSE_BASE+22h
	jne	dop55		;if not MOV CRx,xx
	cmp	al,1
	jne	dop55		;if not CR1

disbad4:jmp	disbad		;can't MOV CR1,xx

;	DRx (OP_DR).

dop_dr:
	call getregmem_r
	mov	bx,'DR'
	mov	cx,-1		;no max or illegal value
	jmp s	dop55

;	TRx (OP_TR).

dop_tr:
	call getregmem_r
	cmp	al,3
	jb	disbad		;if too small
	cmp	al,6
	jae	dop54a		;if TR6-7
	mov	byte [dismach],4	;TR3-5 are new to the 486
dop54a:
	mov	bx,'TR'

dop55:
	xchg ax, bx
	call dis_stosw_lowercase; store XX
	xchg ax, bx
	or al, '0'
	stosb
	retn

;	Segment register (OP_SEGREG).

dop_segreg:
	call getregmem_r
	cmp	al,6
	jae	disbad		; if not a segment register -->
	testopt [asm_options], disasm_nec
	jnz	.nec		; allow mov from or to any segreg
	cmp	al,2
	jne	@F		; if not SS -->
.nec:
	or	byte [disflags], DIS_I_MOV_SS	; note this
@@:
	cmp	al,4
	jb dop57a		;if not FS or GS
	mov	byte [dismach],3;(no new 486-686 instructions involve seg regs)
dop57a:
	add	al,16
	jmp	dop35		;go print it out

;	Sign-extended immediate byte (OP_IMMS8). "push xx"

dop_imms8:
	call disgetbyte
	cmp	al,0
	xchg ax,bx		;mov bl,al
	mov	al,'+'
	jge	dop58a		;if >= 0
	neg	bl
	mov	al,'-'
dop58a:
	stosb
	xchg ax,bx		;mov al,bl
	jmp s	dop59a		;call hexbyte and return


;	Immediate byte (OP_IMM8).

dop_imm8:
	call disgetbyte
dop59a:
	jmp	hexbyte		;call hexbyte and return


dop_imm8_int:
	call disgetbyte
	cmp al, 3
	jne dop59a
	push ax
	call showsize.byte
	pop ax
	jmp dop59a

dop_imm8_optional:
	call disgetbyte
	cmp al, 10
	jne dop59a
	pop ax
	jmp da_op_end


	; Show MMx reg (OP_MMX; previously was "Show ECX if 32-bit LOOPxx").
dop_mmx:
	mov bx, "MM"
	call getregmem_r
	jmp short dop55

	; MMX register (in ModR/M part)
dop_mmx_mod:
	mov bx, "MM"
	call getregmem
	cmp al, 0C0h
	jb disbad		; needs to be encoded as register -->
	and al, 7
	jmp short dop55


%if _MEMREF_AMOUNT
dop_stack_push:
	mov ax, 2
.special:
	test byte [presizeflags], PRE32D
.iso16_if_ZR:
	jz .iso16
	add ax, ax
.iso16:
	call get_free_memref	; memrefs + bx -> the memref structure

	mov word [memrefs + bx + mrLength], ax
	mov cx, word [reg_esp + 2]
	mov dx, word [reg_esp]
	sub dx, ax
	sbb cx, 0

	mov word [memrefs + bx + mrOffset], dx

	mov dl, mrfMemDest | mrfMem

	jmp dop_stack_common

%if _PM
.special_int:
	call ispm
	jnz .iso16
	cmp byte [dpmi32], 0
	jmp .iso16_if_ZR
%else
.special_int: equ .iso16
%endif

dop_stack_pop:
	mov ax, 2
.special:
	mov cx, word [reg_esp + 2]
	mov dx, word [reg_esp]
.leave:
	test byte [presizeflags], PRE32D
	jz .iso16
	add ax, ax
.iso16:
	call get_free_memref	; memrefs + bx -> the memref structure

	mov word [memrefs + bx + mrLength], ax

	mov word [memrefs + bx + mrOffset], dx

	mov dl, mrfMemSource | mrfMem

dop_stack_common:
	mov ax, word [reg_ss]	; get ss selector into ax
	mov word [memrefs + bx + mrSegmentSelector], ax

_386_PM	xchg ax, bx		; selector in bx
_386_PM	call test_d_b_bit	; check whether a 32-bit ss
_386_PM	xchg ax, bx
_386_PM	jz .isstack16
_386_PM	or dl, mrfA32
_386_PM	mov word [memrefs + bx + mrOffset + 2], cx
.isstack16:

	or byte [memrefs + bx + mrFlags], dl
	call calc_linear_memref_and_mark_nonfree
	jmp dop_continue_maybe_end


dop_stack_special:
	mov ax, [index]
	cmp ax, GROUP7 + 3	; call far [mem] ?
	je .callfar
	test ah, ah
	jnz .error
	push di
	mov di, .bytes		; list of opcodes
	mov cx, .bytes_amount
	repne scasb
	mov al, [di + .bytes_amount - 1]
	pop di
	jne .error		; if not one of these -->

	db __TEST_IMM16		; (skip mov)
.callfar:
	mov al, 40h + 4

.haveinfo:
	mov bx, ax
	and ax, 1Fh
	cmp bl, 80h
	jae dop_stack_pop.special
	cmp bl, 40h
	jae dop_stack_push.special
	cmp bl, 20h
	jae dop_stack_push.special_int
	test al, al
	jz .leave
	cmp al, 1
	jne .error
.into:
	testopt [reg_efl], 800h
	jz .none
	mov al, 20h + 6
	jmp .haveinfo

.leave:
	mov ax, 2		; pop size = 1 word or 1 dword
	mov cx, word [reg_ebp + 2]
	mov dx, word [reg_ebp]	; pop from address in (e)bp, not (e)sp
	jmp dop_stack_pop.leave

.error:
	jmp da_internal_error

	usesection lDEBUG_DATA_ENTRY

.bytes:
	db 0CDh, 0CEh		; int, into
	db 0CCh, 0F1h		; int3, int1
	db 9Ah			; call far imm:imm
	db 0CFh, 0CAh, 0CBh	; iret, retf imm, retf
	db 60h			; pusha
	db 61h			; popa
	db 0C9h			; leave
.bytes_amount: equ $ - .bytes

		; In this table the bits have the following meaning:
		;
		; flag 80h: special pop
		; flag 40h: special push
		; flag 20h: special push for int
		; if neither flag set: unusual, for leave and into
		; else: masked with 1Fh gives amount of bytes to push/pop
		;	 in o16, which needs to be doubled to get the amount
		;	 to push/pop in o32 instead.
.info:
	db 20h + 6	; int
	db 1		; into
	db 20h + 6	; int3
	db 20h + 6	; int1
	db 40h + 4	; call far imm:imm
	db 80h + 6	; iret
	db 80h + 4	; retf imm
	db 80h + 4	; retf
	db 40h + 16	; pusha
	db 80h + 16	; popa
	db 0		; leave

	usesection lDEBUG_CODE

.none:
%else
dop_stack_push:
dop_stack_pop:
dop_stack_special:
%endif
dop_continue_maybe_end:
	pop	ax		; discard return address
	jmp	da14_check_end	; next -->


dop_m_always_16:
	setopt [disflags], DIS_I_M_ALWAYS_16 | DIS_I_NOSIZ
	jmp dop_continue_maybe_end


	; Set flag to always show size (OP_SHOSIZ).
dop_shosiz:
	or	byte [disflags],DIS_I_SHOWSIZ
dop60a:
	pop	ax		; discard return address
	jmp	da14		; next...

dop_far_m:
	setopt [disflags], DIS_I_FAR_M
	jmp dop60a

dop_double_m:
	setopt [disflags], DIS_I_DOUBLE_M
	jmp dop60a


%if _MEMREF_AMOUNT
dop_m_src:
	setopt [disflags], DIS_I_M_SRC
	jmp dop60a

dop_m_src_dst:
	setopt [disflags], DIS_I_M_SRC
dop_m_dst:
	setopt [disflags], DIS_I_M_DST
	jmp dop60a
%else
dop_m_src: equ dop60a
dop_m_src_dst: equ dop60a
dop_m_dst: equ dop60a
%endif


dop_short:
	testopt [asm_options], disasm_show_short
	jz dop60a
	mov ax, "SH"
	call dis_stosw_lowercase
	mov ax, "OR"
	call dis_stosw_lowercase
	mov ax, "T "
	call dis_stosw_lowercase
dop60a_1:
	jmp dop60a

dop_near:
	testopt [asm_options], disasm_show_near
	jz dop60a_1
	call dop_show_near
dop60a_2:
	jmp dop60a_1

dop_far:
	testopt [asm_options], disasm_show_far
	jz dop60a_2
dop_far_required:
	mov	ax, "FA"	; store "FAR "
	call	dis_stosw_lowercase
	mov	ax, "R "
	call	dis_stosw_lowercase
	jmp	dop60a_2

dop_show_near:
	push ax
	mov ax, "NE"
	call dis_stosw_lowercase
	mov ax, "AR"
	call dis_stosw_lowercase
	mov al, " "
	stosb
	pop ax
	retn


disbad:
	mov	sp,[savesp2]	;pop junk off stack
	mov	ax, da13
	push	ax
	_386_PM_o32	; xor eax, eax
	xor	ax, ax
	_386_PM_o32	; mov dword [dis_n], eax
	mov	word [dis_n], ax
	mov	word [preflags], ax	; clear preflags and preused
%if _COND
	mov	word [condmsg], ax	; initialize conditions message
%endif
	mov	byte [rmsize], 80h	; don't display any memory
	mov	word [dismach], ax	; forget about the machine type
	and	byte [disflags],~DIS_I_SHOW	;and flags
	call disgetbyte
	mov	di,prefixlist
	mov	cx,N_PREFIX
	repne scasb
	je	.namedprefix	;if it's a named prefix
	_386_PM_o32	; dec dword [dis_n]
	dec	word [dis_n]
	mov	bx,MN_DB	;offset of 'DB' mnemonic
	mov si, fake_oplist.op_imm8 - oplists
	retn

.namedprefix:
	or	byte [disflags],DIS_I_UNUSED	;print special flag
	mov	bx,N_PREFIX - 1
	sub	bx,cx
	shl	bx,1
	cmp	bx, byte 6 *2
	jb	.segprefix	; if SEG directive -->
%if _PM
	cmp	bx, byte 10 *2
	jb	.non16prefix	; if not OSIZE or ASIZE -->
	test byte [bCSAttr], 40h; 32-bit code segment ?
	jz .non16prefix		; no, O32 or A32 -->
	add	bx, byte 4	; yes, change to O16 or A16
.non16prefix:
%endif
	mov	bx,[prefixmnem+bx-6*2]
	xor si, si		; no operand (empty oplist)
	retn

.segprefix:
	lea si, [bx + fake_oplist.op_segments - oplists]
				; -> fake OPLIST for segments
	mov	bx,MN_SEG
	retn


usesection ASMTABLE1
fake_oplist:
.op_imm8:
	db OP_IMM8, 0
.op_segments:
	db OP_ES, 0
	db OP_CS, 0
	db OP_SS, 0
	db OP_DS, 0
	db OP_FS, 0
	db OP_GS, 0

usesection lDEBUG_CODE


;	GETREGMEM_R - Get the reg part of the reg/mem part of the instruction
;	Uses	CL

getregmem_r:
	call	getregmem

		; INP:	al = bits 3 to 5 has value to extract
		; OUT:	al = (INP:al >> 3) & 7
		; CHG:	cl
da_get_bits_3_to_5:
	mov	cl,3
	shr	al,cl
	and	al,7
	ret

;	GETREGMEM - Get the reg/mem part of the instruction

getregmem:
	test byte [preused],GOTREGM
	jnz	grm1		;if we have it already
	or	byte [preused],GOTREGM
	call disgetbyte	;get the byte
	mov	[regmem],al	;save it away

grm1:	mov	al,[regmem]
	ret

dis_lowercase_w:
	xchg al, ah
	call dis_lowercase
	xchg al, ah
dis_lowercase:
	cmp al, 'A'
	jb .not
	cmp al, 'Z'
	ja .not
	testopt [asm_options], disasm_lowercase
	jz .not
	or al, 20h
.not:
	retn


dis_lowercase_refmem_w:
	xchg al, ah
	call dis_lowercase_refmem
	xchg al, ah
dis_lowercase_refmem:
	cmp al, 'A'
	jb .not
	cmp al, 'Z'
	ja .not
	testopt [asm_options], disasm_lowercase_refmem
	jz .not
	or al, 20h
.not:
	retn


		; Show the opcode mnemonic
		;
		; INP:	si-> Opcode mnemonic string of an mnlist entry.
		;	w[si-2] & 0Fh = Length of that string.
		; OUT:	di-> next available byte in output line
		;	     (>= line_out + 32 due to padding)
		; CHG:	ax, cx, si
showop:
%if _40COLUMNS
	mov di, line_out
	add di, word [mnemonofs]
%else
	mov di, line_out+MNEMONOFS
%endif
	push si

	mov cx, [si-2]
	and cx, 0Fh
.loop:
	lodsb
	call dis_lowercase
	stosb
	loop .loop

	pop ax				; ax-> mnemonic
	cmp ax, mnlist_a_suffix_allowed	; non-suffixed mnemonic ?
	jb .nosuffix			; yes -->
	cmp ax, mnlist_o_suffix_allowed	; optional address size suffix ?
	jb .a_suffix_allowed		; yes -->
	cmp ax, mnlist_o_suffix_required; optional operand size suffix ?
	 mov ah, PREOSIZE		; (OSIZE: check OSIZE/O32)
	jae .suffix_decide		; no, it's required -->
	db __TEST_IMM16			; (skip mov)
.a_suffix_allowed:
	 mov ah, PREASIZE		; optional ASIZE: check ASIZE/A32

.suffix_decide_optional:	; check whether the suffix is necessary
	test byte [preflags], ah	; check if such a prefix occured (ZR if not)
	jz .suffix_invisible		; no, is default form --> (hide suffix)

.suffix_decide:			; suffix will be displayed, now only decide which
	mov al, 'W'
	test byte [presizeflags], ah	; 32-bit form ?
	jz .got_suffix			; no -->
	mov al, 'D'
.got_suffix:
	call dis_lowercase
	stosb				; store suffix

.suffix_invisible:		; notional suffix either displayed or left hidden,
	or byte [preused], ah		; in any case, mark opcode prefix as used

.nosuffix:

		; Store blanks to pad to 8 characters, but at least one
%if _40COLUMNS
	mov si, line_out + 8
	add si, word [mnemonofs]
%endif
	mov al, 32

%if _40COLUMNS
	testopt [asm_options], disasm_no_indent
	jnz @F
%endif
.pad:
	stosb			; store a blank
%if _40COLUMNS
	cmp di, si		; past 8 columns already ?
%else
	cmp di, line_out+MNEMONOFS+8
%endif
	jb .pad			; not yet, loop -->

	retn

%if _40COLUMNS
@@:
	stosb			; store a single blank
	retn
%endif


		; INP:	byte [segmnt] = number of segment register
		;	[segrgnam] = uppercase segment register names
		; CHG:	bx, ax
		; OUT:	ax = uppercase segment register name
showseg_uppercase_ax:
	mov	al,[segmnt]	;segment number
	cbw
	shl	ax,1
	xchg	ax,bx		;mov bx,ax
	mov	ax,[segrgnam+bx] ;get register name
	retn

;	SHOWSEG - Show the segment descriptor in SEGMNT
;	Entry	DI	Where to put it
;	Exit	DI	Updated
;	Uses	AX, BX

showseg:
	call showseg_uppercase_ax
dis_stosw_lowercase:
	call	dis_lowercase_w
	stosw
	retn


		; Write a size specifier to the buffer
		;  and set some flags
		; INP:	ah = r/m size value,
		;		F0h byte (less than zero)
		;		00h word (equal to zero)
		;		10h dword (greater than zero, but != 20h)
		;		20h qword (greater than zero, == 20h)
		;	di-> buffer
		; OUT:	di-> behind size specifier in buffer
		;	by[rmsize] set
		;	wo[sizeloc]-> size specifier in buffer
		; CHG:	ax
		;
		; Size specifiers are BYTE, WORD, DWORD, and QWORD. One
		; blank is appended to the size specifier.
		;
		; Size specifiers are lowercased if that option is selected.
showsize:
	mov	[rmsize], ah	; save r/m size
	mov	[sizeloc], di	; save where we're putting this
	mov	al, 'Q'
	cmp	ah, 20h		; QWORD ?
	je	.qword		; yes -->
	cmp	ah, 0
	jge	.notbyte	; if word or dword -->
.byte:
	mov	ax, "TE"
	push	ax
	mov	ax, "BY"
	jmp s	.common

.notbyte:
	je	.word		; if word
.dword:
	mov	al, 'D'
.qword:
	call	dis_lowercase
	stosb
.word:
	mov	ax, "RD"
	push	ax
	mov	ax, "WO"
.common:
	call	dis_stosw_lowercase
	pop	ax
	call	dis_stosw_lowercase
	mov	al, 32
	stosb
 showptr.ret:
	retn

		; Write "PTR " to a buffer if NASM syntax is not selected.
		; INP:	di-> buffer
		; OUT:	di-> behind written string "PTR " (or unchanged)
		; CHG:	ax
		;
		; The string is lowercased if that option is selected.
showptr:
	testopt [asm_options], disasm_nasm
	jnz	.ret
	mov	ax, "PT"
	call	dis_stosw_lowercase
	mov	ax, "R "
	jmp s	dis_stosw_lowercase

		; Write "DWORD " to a buffer
		; INP:	di-> buffer
		; OUT:	di-> behind written string "DWORD "
		; CHG:	ax
		;
		; The string is lowercased if that option is selected.
showdword: equ showsize.dword
showword: equ showsize.word

;	DISP32 - Print 32-bit displacement for addressing modes.
;	Entry	None
;	Exit	None
;	Uses	AX

disp32:
	call disgetword
.ax:
	push ax
	call disgetword
	call hexword
	pop ax
	jmp hexword

		; SHOWREG16 - Show 16-bit register name.
		;
		; INP:	al = register number, 0 to 7
		;	di -> buffer
		; OUT:	register name stored to buffer
		;	ax = INP:al * 2
showreg16:
	cbw
	shl	ax,1
	xchg ax,bx
	push ax
	mov	ax,[rgnam16+bx]
	call	dis_stosw_lowercase
	pop	ax
	xchg ax,bx
	ret


		; DISCHK32D - Check for O32 (32-bit operand size).
dischk32d:
	or byte [preused], PREOSIZE
	test byte [presizeflags], PRE32D
	retn


;	SHOWMACH - Return string "[needs math coprocessor]", etc.
;	Entry   di -> table of 5 words
;		cx = instruction
;	Exit	si	Address of string
;		cx	Length of string, or 0 if not needed
;	Uses	al, di

showmach:
	mov	si,needsmsg	; candidate message
	test byte [dmflags],DM_COPR
	jz	sm1		; if not a coprocessor instruction
	mov byte [si + needsmsg.digit_6_ofs], '7'
				; change message text
	mov	al,[mach_87]
	cmp	byte [has_87],0
	jnz	sm2		; if it has a coprocessor
	mov	al,[machine]
	cmp	al,[dismach]
	jb	sm3		; if we display the message
	mov	si,needsmath	; print this message instead
	mov	cx,needsmath_L
	retn

sm1:
	mov byte [si + needsmsg.digit_6_ofs], '6'
				; reset message text
	mov	al,[machine]
sm2:
	cmp	al,[dismach]
	jae	sm4		; if no message (so far)
sm3:
	mov	al,[dismach]
	add	al,'0'
	mov byte [si + needsmsg.digit_x_ofs], al
	mov	cx,needsmsg_L	; length of the message
	retn

		; Check for obsolete instruction.
sm4:
	mov	si, obsolete	; candidate message
	mov	ax, cx		; get info on this instruction
	mov	cx, 5
	repne scasw
	jne	sm6		; if no matches
	mov	di, obsmach + 5 - 1
	sub	di, cx
	xor	cx, cx		; clear CX: no message
	mov	al, byte [mach_87]
	cmp	al, byte [di]
	jle	sm5		; if this machine is OK
	mov	cx, obsolete_L
sm5:
	retn

sm6: equ sm5


;	DISGETBYTE - Get byte for disassembler.
;	Entry	None
;	Exit	AL	Next byte in instruction stream
;	Uses	None

disgetbyte:
	push ds
	_386_PM_o32		; push esi
	push si				; save ds, (e)si
	_386_PM_o32		; mov esi, dword [u_addr]
	mov si, word [u_addr]
	_386_PM_o32		; add esi, dword [dis_n]
	add si, word [dis_n]		; index to the right byte
	mov ds, word [u_addr + saSegSel]
	_386_PM_a32
	lodsb 				; get the byte
	_386_PM_o32		; pop esi
	pop si
	pop ds				; restore regs
	_386_PM_o32		; inc dword [dis_n]
	inc word [dis_n]		; indicate that we've gotten this byte
	retn


;	DISGETWORD - Get word for disassembler.
;	Entry	None
;	Exit	AX	Next word
;	Uses	None

disgetword:
	push ds
	_386_PM_o32		; push esi
	push si				; save ds, (e)si
	_386_PM_o32		; mov esi, dword [u_addr]
	mov si, word [u_addr]
	_386_PM_o32		; add esi, dword [dis_n]
	add si, word [dis_n]		; index to the right byte
	mov ds, word [u_addr + saSegSel]
	_386_PM_a32
	lodsw 				; get the word
	_386_PM_o32		; pop esi
	pop si
	pop ds				; restore regs
	_386_PM_o32		; add dword [dis_n], byte 2
	add word [dis_n], byte 2	; indicate that we've gotten this word
	retn


;	DISSHOWBYTES - Show bytes for the disassembler.
;	Entry	BX	Number of bytes (must be > 0)
;		di-> output line
;	Exit		u_addr updated
;	Uses	BX, (E)SI.

disshowbytes:
	_386_PM_o32		; mov esi, dword [u_addr]
	mov si, word [u_addr]
	mov ds, word [u_addr + saSegSel]
.loop:
	_386_PM_a32
	lodsb
	call hexbyte
	dec bx
	jnz .loop
	push ss
	pop ds
	_386_PM_o32		; mov dword [u_addr], esi
	mov word [u_addr], si
	retn

		; MOVEOVER - Move the line to the right.
		; Entry	DI	Last address + 1 of line so far
		;	CX	Number of bytes to move
		; Exit	DI	Updated
		; Uses	SI
moveover:
	cmp word [sizeloc], byte 0
	je mo1			; if sizeloc not saved
	add word [sizeloc], cx

mo1:
	mov si, di
	add di, cx
	mov cx, di
%if _40COLUMNS
	sub cx, line_out
	sub cx, word [mnemonofs]
%else
	sub cx, line_out+MNEMONOFS
%endif
	push di
	std			; _AMD_ERRATUM_109_WORKAROUND as below
	dec si
	dec di


	numdef AMD_ERRATUM_109_WORKAROUND, 1
		; Refer to comment in init.asm init_movp.

%if _AMD_ERRATUM_109_WORKAROUND
	jcxz @FF
	cmp cx, 20
	ja @FF
@@:
	movsb
	loop @B
@@:
%endif
	rep movsb
	pop di
	cld
	retn

..@uu_access_end:
