
%if 0

lDebug A command - Assembler

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

	align 2, db 0
aa13a_mnemposition:	dw 0		; -> mnemonic, to display error
aa_mnemsuffix:		db 0		; 0 = normal, 1 = 'W' suffix, 2 = 'D' suffix

asm_mn_flags:	db 0			; flags for the mnemonic
AMF_D32		equ 1			; 32-bit opcode/data operand
AMF_WAIT	equ 2
AMF_A32		equ 4			; address operand is 32-bit
AMF_SIB		equ 8			; there's a SIB in the arguments
AMF_MSEG	equ 10h			; if a seg prefix was given before mnemonic
AMF_FSGS	equ 20h			; if FS or GS was encountered
AMF_D16		equ 40h			; 16-bit opcode/data operand
AMF_ADDR	equ 80h 		; address operand is given (write address size prefix)

aa_saved_prefix:db 0			; WAIT or REP... prefix
	; aa_saved_prefix and aa_seg_pre must be consecutive.
aa_seg_pre:	db 0			; segment prefix

	align 2, db 0
mneminfo:	dw 0			; address associated with the mnemonic

	; The following 7 words (including alloweddist) must all be consecutive.
rmaddr:		dw 0			; address of operand giving the R/M byte
	; regmem and sibbyte must be consecutive
regmem:		db 0			; mod reg r/m part of instruction
sibbyte:	db 0			; SIB byte
immaddr:	dw 0			; address of operand giving the immed stf
xxaddr:		dw 0			; address of additional stuff
	; dismach and dmflags must be consecutive
dismach:	db 0			; type of processor needed
dmflags:	db 0			; flags for extra processor features

DM_COPR		equ 1			; math coprocessor
DM_MMX		equ 2			; MMX extensions

opcode_or:	db 0			; extra bits in the op code
opsize:		db 0			; size of this operation (2 or 4)
varflags:	db 0			; flags for this variant

VAR_LOCKABLE	equ 1			; variant is lockable
VAR_MODRM	equ 2			; if there's a MOD R/M here
VAR_SIZ_GIVN	equ 4			; if a size was given
VAR_SIZ_FORCD	equ 8			; if only one size is permitted
VAR_SIZ_NEED	equ 10h			; if we need the size
VAR_D16		equ 20h			; if operand size is WORD
VAR_D32		equ 40h			; if operand size is DWORD
VAR_M_ALWAYS_16	equ 80h
alloweddist:	db 0

a_reqsize:	db 0			; size that this arg should be
	align 2, db 0
a_opcode:	dw 0			; op code info for this variant

	align 2, db 0
a_opcode2:	dw 0			; copy of a_opcode for obs-instruction
a_obstab:	dw 0DBE0h,0DBE1h,0DBE4h,124h,126h	; obs. instruction codes
obsmach:	db 1,1,2,4,4		; max permissible machine for the above
	; This is used to search for obsolete instructions:
	; DBE0h:	feni
	; DBE1h:	fdisi
	; DBE4h:	fsetpm
	; 124h:		mov trX, reg
	; 126h:		mov reg, trX

aadbsiz:	db 0,4,2,1		; table for max size of db operand
	align 2, db 0
aadbsto:	dw 0,aa28,aa29,aa30	; table for routine to store a number

	align 2, db 0		; (modrmtab really is an array of words)
modrmtab:	db REG_BX,0,REG_BP,0		; [bx], [bp]
		db REG_DI,0,REG_SI,0		; [di], [si]
		db REG_DI,REG_BP,REG_SI,REG_BP	; [bp+di],[bp+si]
		db REG_DI,REG_BX,REG_SI,REG_BX	; [bx+di],[bx+si]

%if _IMMASM
aa_ret:		dw aa01			; used by immasm
%endif
aam_args:	db 'a',13

		; Equates for parsed arguments, stored in OPRND.flags
ARG_DEREF	equ 1			; non-immediate memory reference
ARG_MODRM	equ 2			; if we've computed the MOD R/M byte
ARG_JUSTREG	equ 4			; a solo register
ARG_WEIRDREG	equ 8			; if it's a segment register or CR, etc.
ARG_IMMED	equ 10h			; if it's just a number
ARG_FARADDR	equ 20h			; if it's of the form xxxx:yyyyyyyy
ARG_ECX_SPECIAL equ 80h			; have to overflow loop displacement

		; For each operand type in the following table, the value
		;  is the bits at least one of which must be present.
		;  For each entry in bittab, there's an entry in asmjmp.
		;  Entries are defined in the debug.asm opsizeditem list.
bittab:
		db BITTAB_OPSIZEDITEMS
		times 16 - ($ - bittab) db 0	; unused OP_SIZE combined types

		; OP_END does not have a table entry. Subsequent
		;  entries are defined in the debug.asm opitem list.
		db BITTAB_OPITEMS
%if ($ - bittab) != OP_AMOUNT_TABLE
 %error bittab has wrong size
%endif

		; Jump table for operand types.
		;  Entries are defined in the debug.asm opsizeditem list.
	align 2, db 0
asmjmp:
		dw ASMJMP_OPSIZEDITEMS
		times 16 - (($ - asmjmp) / 2) dw ao50	; unused size-combined types (reject)

		; OP_END does not have a table entry. Subsequent
		;  entries are defined in the debug.asm opitem list.
		dw ASMJMP_OPITEMS
%if ($ - asmjmp) / 2 != OP_AMOUNT_TABLE
 %error asmjmp has wrong size
%endif

; special ops DX, CL, ST, CS, DS, ES, FS, GS, SS
; entry required if ao48 is used in the opitem list
; order has to match opitem order
; refer to aagetreg comment for the number assignments
asm_regnum:	db REG_DX, REG_CL, REG_ST
		db REG_CS, REG_DS, REG_ES, REG_FS, REG_GS, REG_SS

; sizes for OP_M64, OP_MFLOAT, OP_MDOUBLE, OP_M80, OP_MXX
; entry required if ao17 is used in the opitem list
; order has to match opitem order
asm_siznum:	db SIZ_QWORD, SIZ_FLOAT, SIZ_DOUBLE, SIZ_TBYTE
		db -1		; none

; size qualifier
;  1 BY = BYTE
;  2 WO = WORD
;  3 unused
;  4 DW = DWORD
;  5 QW = QWORD
;  6 FL = FLOAT (REAL4)
;  7 DO = DOUBLE (REAL8)
;  8 TB = TBYTE (REAL10)

SIZ_NONE	equ 0
SIZ_BYTE	equ 1
SIZ_WORD	equ 2
SIZ_DWORD	equ 4
SIZ_QWORD	equ 5
SIZ_FLOAT	equ 6
SIZ_DOUBLE	equ 7
SIZ_TBYTE	equ 8

	align 2, db 0
sizetcnam:	db "BY","WO","WO","DW","QW","FL","DO","TB"
	endarea sizetcnam

	align 2, db 0
distnam:	db "SH","NE","FA"
	endarea distnam


	usesection lDEBUG_CODE

..@aa_access_start:

aa_cmd3_check:
	lodsb
	call chkeol
aa_cmd3:
	jmp cmd3			; exit assembler mode -->


aa:
	call guard_re
	mov bx, word [ reg_cs ]		; default segment to use
aa00a:
	call iseol?
	je aa01				; if end of line -->
	call getaddrX			; get address into bx:(e)dx
					;  (no scratchsel)
	call chkeol			; expect end of line here
	_386_PM_o32		; mov dword [ a_addr ], edx
	mov word [ a_addr ], dx		; save the address
	mov word [a_addr + saSegSel], bx
%if _PM
	call ispm
	jnz .86m
.pm:
	mov word [a_addr + saSelector], bx
	jmp @F
.86m:
	mov word [a_addr + saSegment], bx
@@:
%endif

		; Begin loop over input lines.
aa01:
%if _IMMASM
	mov word [ aa_ret ], aa01
%endif
	mov word [ errret ], aa01
	mov sp, word [ savesp ]		; restore the stack (this implies no "retn")
	mov di, line_out
	mov ax, word [a_addr + saSegSel]
%if _PM
	mov bx, ax			; this is the original selector,
					;  not the scratchsel
%endif
	call hexword
	mov al, ':'
	stosb
%if _PM
	mov byte [ bCSAttr ], 0
	call test_d_b_bit
	jz .16
	mov byte [ bCSAttr ], 40h	; set 32-bit attrib for later checks here
	mov ax, word [ a_addr+2 ]
	call hexword
.16:
%endif
	mov ax, word [ a_addr+0 ]
	call hexword
	mov al, 32
	stosb
	call getline00
	cmp al, '.'
	je aa_cmd3_check
	cmp al, ';'
	je aa01				; if comment
	call iseol?.notsemicolon
	je aa_cmd3			; if done, return to command line -->
aa_imm_entry:
	mov byte [ asm_mn_flags ], 0
	mov word [ aa_saved_prefix ], 0	; clear aa_saved_prefix and aa_seg_pre

		; Get mnemonic and look it up.
		; (At this point, it has been determined that it is not empty.)
		;
		; INP:	al = first character
		;	si-> remaining string (al isn't EOL)
aa02:
	mov di, line_out		; -> buffer
	xor cx, cx			; = 0
	mov [ aa13a_mnemposition ], si

	db __TEST_IMM16			; skip stosb,lodsb initially
@@:
	stosb
	lodsb
	inc cx				; count length
	call uppercase
	call iseol?			; end of mnemonic ?
	je @F
	cmp al, ':'
	je @F				; (for prefixes, else will be an error later)
	cmp al, 32
	je @F
	cmp al, 9
	jne @B				; not yet -->
@@:

	dec cx				; = length of input
	call skipwh0			; skip to next field
	dec si

	mov al, [di-1]			; get last stored character
	cmp al, 'W'			; possible suffix?
	je @F
	cmp al, 'D'
	je @F				; yes -->
	xor al, al
@@:
	mov [aa_mnemsuffix], al		; store 'D', 'W', or 0

	push si				; save position in input line

	mov si, mnlist			; -> first area: no or optional suffix
	mov dx, mnlist_o_suffix_required; -> end of first area

		; [line_out] = name to search
		; cx = length of name to search
		; si-> next mnlist entry
		; dx-> behind last mnlist entry of this area
		; w[ss:sp]-> next field in input line (operand or EOL)
aa_mnemlistloop:
	lodsw				; load combined word, si-> name
	and ax, 0Fh			; separate mnemonic length
	cmp ax, cx			; length matches ?
	je .length_match		; yes, check name -->
.cmps_mismatch:
	add si, ax			; -> behind entry
	cmp si, dx			; at end of this list area ?
	jb aa_mnemlistloop		; not yet, check next entry -->

	cmp dx, mnlist_o_suffix_required; was first or second area ?
	jne .mnem_invalid		; second, not found -->

	mov si, mnlist_a_suffix_allowed	; -> second area: optional or required suffix
	mov dx, end_mnlist		; -> end of second area

	dec cx				; prepare for second look-up
	cmp [aa_mnemsuffix], ah		; is there a valid suffix ? (ah still 0)
	jne aa_mnemlistloop		; yes, check for suffixed instruction -->

.mnem_invalid:
	pop ax				; (discard)
	jmp aa13a			; complain -->

.length_match:			; found a name of correct length
	mov di, line_out		; -> all-capitals input
	mov bx, si			; -> name
	repe cmpsb			; compare names
	mov si, bx
	mov cx, ax			; restore length
	jne .cmps_mismatch		; not this, continue -->

		; We found the mnemonic.
		; (bx=si)-> entry's mnemonic
		; dx-> behind last mnlist entry of this area
		; w[ss:sp]-> next field in input line (operand or EOL)
	mov ax, si			; -> mnemonic's name

	cmp dx, end_mnlist		; was first or second area ?
	je .handlesuffix		; second, there's a suffix to handle -->

	cmp ax, mnlist_a_suffix_allowed	; optional suffix that was not specified?
	jb aa_mnemonic_found		; no, done with the suffixes already -->

%if _PM
	mov dl, byte [ bCSAttr ]	; dl = whether a 32-bit CS
%else
	xor dl, dl			; 86 Mode is always 16-bit
%endif
	jmp sho .suffix_decide

.handlesuffix:

	mov dl, byte [ aa_mnemsuffix ]	; dl = 'W' or 'D'. 'W' is odd, 'D' is even
	not dl				; make 'W' an even value, 'D' an odd one
	and dl, 1			; 'W' results in 0, 'D' in 1

.suffix_decide:
	cmp ax, mnlist_o_suffix_allowed	; address size suffix ?
	jb .a_suffix			; yes -->

		; Operand size suffix.
	test dl, dl			; which ?
	jz .o_suffix_w
	or byte [asm_mn_flags], AMF_D32	; o32
	jmp sho aa_mnemonic_found

.o_suffix_w:
	or byte [asm_mn_flags], AMF_D16	; o16
	jmp sho aa_mnemonic_found

		; Address size suffix.
.a_suffix:
	test dl, dl			; which ?
	jz .a_suffix_w
	or byte [asm_mn_flags], AMF_ADDR|AMF_A32; a32
.a_suffix_w:
	or byte [asm_mn_flags], AMF_ADDR	; a16 (AMF_A32 still clear)

aa_mnemonic_found:
	mov cl, 4
	mov si, [si-2]			; get the combined word
	shr si, cl			; extract offset into asmtab
	add si, asmtab			; -> asmtab sequence

		; bx-> name of matching mnlist entry
		; If this mnemonic is suffixable/suffixed,
		;  AMF_D32,AMF_D16,AMF_ADDR,AMF_A32 show suffix status
		; si-> associated asmtab sequence
		; w[ss:sp]-> next field in input line (operand or EOL)

%if 0

Now si points to the spot in asmtab corresponding to this mnemonic.
The format of the assembler table is as follows.
First, there is optionally one of the following bytes:

ASM_SPECIAL	This is a special mnemonic (directive or AAx).
ASM_WAIT	The instruction is prefixed by a WAIT.
ASM_D32		This is a 32-bit instruction variant.
ASM_D16		This is a 16-bit instruction variant.

Then, except for non-AAx ASM_SPECIAL, this is followed by one or
more of the following sequences, indicating an instruction variant.

ASM_LOCKABLE	Indicates that this instruction can follow a LOCK prefix.
ASM_MACHx	Indicates the CPU this instruction requires, 1..6 (186..686).
ASM_ESCAPE	Escapes a large following word. The assembler table contains
		 as many escapes as necessary; each escape means to add the
		 value of ASM_ESCAPE to the following high byte of the info
		 word. This will easily overflow the word, so a dword is
		 required to process the info word. ASM_ESCAPE currently
		 needs to be equal to ASM_FIRST (ie. the lowest assembler
		 table prefix byte) because otherwise some values would have
		 no valid encoding. _ASM_ESCAPE_USED is a preprocessor
		 variable which will be 0 in case there are no ASM_ESCAPE
		 bytes to be found in the table.
[word]		This is a 16-bit integer, most significant byte first, giving
		 ASMMOD * a + b, where b is an index into the array opindex
		 (indicating the operand list), and a is as follows (hex):
		0..FF	  The (one-byte) instruction.
		100..1FF  The lower 8 bits give the second byte of
			  a two-byte instruction beginning with 0Fh.
		200..23F  Bits 2-0 say which floating point instruction
			  this is (D8h-DFh), and 5-3 give the /r field.
		240..1247 (a-240h)/8 is the index in the array agroups
			  (which gives the real value of a), and the
			  low-order 3 bits gives the /r field.
[byte]		This gives the second byte of a floating point
		 instruction if 0D8h <= a <= 0DFh.

Following these is an ASM_END byte. (ASM_SPECIAL has the same value as
ASM_END, but the context allows to decide which one is meant.)

Exceptions:
	ASM_SPECIAL are not followed by this opcode information (except AAx).
	ASM_SPECIAL segment, LOCK and REP prefixes are followed by the literal
	 prefix byte.
	ASM_SPECIAL for all mnemonics except AAx and the prefixes are not
	followed by anything at all.

The ASM_ symbols are defined where debugtbl.inc is included in debug.asm.

%endif

	; To do: BITS, USE16, USE32, USEAUTO, CPU
	lodsb				; get a possible prefix
.checkprefix:
%if 1
	cmp al, ASM_SPECIAL		; a special mnemonic ?
	jne .notspecial			; no -->

		; Dispatch based on mnemonic.
	xor ax, ax
	cmp bx, mnlist+MN_O16
	je aa_sizeprefix		; o16 (ax = 0) -->
	inc ax
	cmp bx, mnlist+MN_A16
	je aa_sizeprefix		; a16 (ax = 1) -->
	mov ah, 2
	cmp bx, mnlist+MN_A32
	je aa_sizeprefix		; a32 (ax = 201h) -->
	dec ax
	cmp bx, mnlist+MN_O32
	je aa_sizeprefix		; o32 (ax = 200h) -->
	cmp bx, mnlist+MN_LOCK
	je aa18				; lock -->
	cmp bx, mnlist+MN_REP
	jb .notreplock
	cmp bx, mnlist+MN_REPNE
	jbe aa18			; rep, repe, repne -->
.notreplock:
	cmp bx, mnlist+MN_ES
	jb .notseg
	cmp bx, mnlist+MN_GS
	jbe aa17			; single segment prefix -->
.notseg:
	cmp bx, mnlist+MN_AAD
	je aa_aax			; aad -->
	cmp bx, mnlist+MN_AAM
	je aa_aax			; aam -->
	cmp word [ aa_saved_prefix ], byte 0
	jne aa13a			; if there was a prefix or a segment, error -->
	pop si				; get position in input line
	;cmp bx, mnlist+MN_SEG
	;je aa_seg			; SEG mnemonic, process -->
	cmp bx, mnlist+MN_ORG
	je aa_org
	mov ax, 1
	cmp bx, mnlist+MN_DD
	je aa20m			; dd (ax = 1) -->
	inc ax
	cmp bx, mnlist+MN_DW
	je aa20m			; dw (ax = 2) -->
	inc ax
	cmp bx, mnlist+MN_DB
	je aa20m			; db (ax = 3) -->
	jmp short aa13a			; unhandled special mnemonic -->

.notspecial:
	sub al, ASM_D16			; mnemonic has a prefix ?
	jb .normal			; no -->
	je .d16				; it is a 16-bit mnemonic form -->
%else
	cmp al, ASM_O16PREF
	jb .normal			; no special mnemonic -->
	cmp al, ASM_A32PREF
	jbe aa_sizeprefix		; 386 address/operand size prefix -->

	sub al, ASM_LOCKREP		; check for mnemonic flag byte,
					; and convert it to 0..9 if one
	jb .normal			; if none -->
	je aa18				; if LOCK/REP -->
	cbw
	dec ax
	jz aa17				; if segment prefix (ASM_SEG) -->
	dec ax
	jz aa_aax			; if aad or aam (ASM_AAX) -->
	dec ax
	jz .d16				; if ASM_D16 -->
	cmp al, 3
	jae aa20			; if ASM_ORG or ASM_DD or ASM_DW or ASM_DB -->
%endif
	or [ asm_mn_flags ], al		; save AMF_D32 or AMF_WAIT (1 or 2)
	db __TEST_IMM8			; (skip dec)
.normal:
	dec si				; -> first byte of mnemonic info
.ab01:
	jmp ab01			; now process the arguments
.d16:
	or byte [ asm_mn_flags ], AMF_D16
	inc si				; skip the ASM_D32 byte
	jmp short .ab01			; now process the arguments

aa_sizeprefix:
%if 0
	sub al, ASM_O16PREF		; 0 = o16, 1 = a16, 2 = o32, 3 = a32
	mov ah, al
	and ax, (2<<8)|1		; ah = 2 if 32-bit prefix, al = 1 if ASIZE
%endif
%if _PM
	or ah, byte [ bCSAttr ]
	jz .nobyte			; 16-bit CS and 16-bit prefix, no output -->
	cmp ah, 40h| 2
	je .nobyte			; 32-bit CS and 32-bit prefix, no output -->
%else
	test ah, ah
	jz .nobyte			; 16-bit CS and 16-bit prefix -->
%endif

		; CS differs from the prefix's type.
		; Output a prefix byte.
	add al, 66h			; 66h if OSIZE, 67h if ASIZE
	mov di, line_out
	stosb
	call aa_copymem
.nobyte:
aa_handleprefixes:
	pop si
	lodsb				; get character
	cmp al, ':'
	jne .nocolon
	call skipwhite			; skip a colon
.nocolon:
	call iseol?			; end of line?
	jne aa02			; no, process instruction -->

		; No instruction follows.
		; Write out saved LOCK/REP and/or segment prefix.
	mov al, byte [ aa_seg_pre ]
	test al, al
	jz .noseg
	mov di, line_out
	stosb
	push si
	call aa_copymem
	pop si
.noseg:
	mov al, byte [ aa_saved_prefix ]
	test al, al
	jz .noreplock
	mov di, line_out
	stosb
	push si
	call aa_copymem
	pop si
.noreplock:
 aa01_j1:
%if _IMMASM
	jmp near [ aa_ret ]		; return to prompt
%else
	jmp aa01
%endif


%if 0
		; SEG directive (segment prefix follows)
aa_seg:
	call skipwhite
	mov ah, byte [si]
	and ax, ~2020h
	mov di, segrgnam
	mov cx, 6
	repne scasw
	jne aa24		; if not found
	push si			; save si in case there's no colon
	inc si			; skip "?s"
	call skipwhite
	pop si
	call chkeol
	mov bx, prefixlist + 5
	sub bx, cx
	mov al, byte [ bx ]	; look up the prefix byte
	mov di, line_out
	stosb
	push si
	call aa_copymem
	pop si
	jmp short aa01_j1
%endif

		; segment prefix
aa17:
	lodsb				; get prefix value
	mov byte [ aa_seg_pre ], al
	or byte [ asm_mn_flags ], AMF_MSEG
	jmp short aa_handleprefixes

		; LOCK or REP prefix
aa18:
	lodsb				; get prefix value
	xchg al, byte [ aa_saved_prefix ]
	test al, al
	jz aa_handleprefixes
		; if there already was a saved prefix:
aa13a:
	mov si, [ aa13a_mnemposition ]
	jmp error

%if 0
		; Pseudo ops (org or db/dw/dd).
aa20:
	cmp word [ aa_saved_prefix ], byte 0
	jne aa13a			; if there was a prefix or a segment, error -->
	pop si				; get position in input line
	sub al, 3			; AX=0 if org, 1 if dd, 2 if dw, 3 if db.
	jne aa20m			; if not ORG
%endif

aa_org:
		; Process ORG pseudo op.
	lodsb
	mov bx, word [a_addr + saSegSel]; default segment
	jmp aa00a			; go to top, set address if any given

		; Data instructions (DB/DW/DD).
aa20m:
%if _IMMASM
	testopt [internalflags6], dif6_immasm
	jnz aa24
%endif
	mov di, line_out		; put the bytes here when we get them
	xchg ax, bx			; mov bx,ax
	mov al, byte [ aadbsiz+bx ]	; move maximum size
	mov byte [ aadbsiz ], al
	shl bx, 1
	mov ax, word [ aadbsto+bx ]	; move address of storage routine
	mov word [ aadbsto ],ax
	call skipwhite
	call iseol?
	je aa27				; if end of line

aa21:
	cmp al, '"'
	je aa22				; if string
	cmp al, "'"
	je aa22				; if string
	call aageti			; get a numerical value into dx:bx, size into cl
	cmp cl, byte [ aadbsiz ]
	ja aa24				; if overflow
	xchg ax, bx
	call near [ aadbsto ]		; store the value
	cmp di, line_out_end
	ja aa24				; if output line overflow
	xchg ax, bx
	jmp short aa26			; done with this one

aa22:
	mov ah, al
aa23:
	lodsb
	call iseol?.notsemicolon
	je aa24				; if end of line (closing quote missing) -->
	cmp al, ah
	je aa25				; if end of string
	stosb
	cmp di, line_out_end
	jbe aa23			; if output line not overflowing
aa24:
	jmp error			; error
aa25:
	lodsb
aa26:
	call skipcomm0
	call iseol?
	jne aa21			; if not end of line

		; End of line. Copy it to debuggee's memory.
aa27:
%if _IMMASM
	push word [ aa_ret ]
%else
	mov ax, aa01
	push ax
%endif

		; INP:	di-> behind memory to copy (starts at line_out)
		;	[a_addr]-> destination
		;	word [ss:sp] -> in input line behind this prefix
		; OUT:	memory copied
		;	a_addr offset updated
		;	es set to ss
		; CHG:	(e)si, (e)cx, (e)di
aa_copymem:
	mov si, line_out		; ds:si-> data
	mov cx, di			; -> behind data
	sub cx, si			; = size of data
%if _PM
	push bx
	mov bx, word [a_addr + saSegSel]
	call verifysegm_or_error
	mov es, bx
	pop bx
%else
	mov es, word [a_addr + saSegSel]
%endif
	_386_PM_o32		; mov edi, dword [ a_addr+0 ]
	mov di, word [ a_addr+0 ]	; es:(e)di-> destination
_386_PM	movzx ecx, cx
_386_PM	movzx esi, si			; fix high words
	_386_PM_a32
	rep movsb			; copy it
	_386_PM_o32		; mov dword [ a_addr+0 ], edi
	mov word [ a_addr+0 ], di	; save new address
	push ss
	pop es
%if _IMMASM && (!_IMMASM_AUXBUFF || _AUXBUFFSIZE < (128 + 16))
	testopt [internalflags6], dif6_immasm
	jz @F
	cmp di, immasm_length - 16
	jbe @F
	pop si				; discard near return address
	pop si				; get offset in input line
	jmp error

@@:
%endif
	retn

		; Routines to store a byte/word/dword,
		; into a buffer in our memory.
aa28:
	stosw				; store a dword value
	xchg ax, dx
aa29:
	stosw				; store a word value
	retn

aa30:
	stosb				; store a byte value
	retn

%if _PM && 0
aa_use16:
	cmp word [ aa_saved_prefix ], byte 0
	jne aa13a
	pop si
	lodsb
	call chkeol
	mov dl, 16
	jmp short aa_bits.parse

aa_use32:
	cmp word [ aa_saved_prefix ], byte 0
	jne aa13a
	pop si
	lodsb
	call chkeol
	mov dl, 32
	jmp short aa_bits.parse

aa_useauto:
	cmp word [ aa_saved_prefix ], byte 0
	jne aa13a
	pop si
	lodsb
	call chkeol
	jmp short aa_bits.auto

aa_bits:
	cmp word [ aa_saved_prefix ], byte 0
	jne aa13a			; if there was a prefix or a segment, error -->
	pop si				; get position in input line

		; Check whether "AUTO" requested.
	push si
	lodsw
	and ax, ~2020h
	cmp ax, "AU"
	jne .notauto
	lodsw
	and ax, ~2020h
	cmp ax, "TO"
	jne .notauto
	lodsb
	call iseol?
	jne .notauto
	pop ax
.auto:
	xor ax, ax
	mov al, byte [ bCSAttr+1 ]
	test al, al			; any saved ?
	jz aa01_j1			; no -->
	mov word [ bCSAttr ], ax	; restore
	jmp short aa01_j1

.notauto:
	pop si
	lodsb
	push si
	call getbyte
	call chkeol
	pop si
.parse:
	mov ax, word [ bCSAttr ]
	or al, 1
	cmp dl, 16
	je .16
	cmp dl, 16h
	je .16
	cmp dl, 32
	je .32
	cmp dl, 32h
	jne aa24
.32:
_no386	jmp aa24
	test ah, ah
	mov ah, al
	mov al, 40h
	jmp short .save

.16:
	test ah, ah
	mov ah, al
	mov al, 0
.save:
	jnz .saved
	mov byte [ bCSAttr+1 ], ah
.saved:
	mov byte [ bCSAttr ], al
	jmp short aa01_j1
%endif

		; Here we process the AAD and AAM instructions.  They are special
		; in that they may take a one-byte argument, or none (in which case
		; the argument defaults to 0Ah = ten).
aa_aax:
	mov word [ mneminfo ], si	; save this address
	pop si
	lodsb
	call iseol?
	jne ab01b			; if not end of line -->
	mov si, aam_args		; fake a 0Ah argument if none given
	jmp short ab01a


		; Process normal instructions.

		; First we parse each argument into the following structure,
		; stored consecutively at line_out, line_out+OPRND_size, etc.
		;
		; For arguments of the form xxxx:yyyyyyyy, xxxx is stored in
		; OPRND.num2, and yyyyyyyy in OPRND.num. The number of bytes
		; in yyyyyyyy is stored in opaddr, 2 is stored in OPRND.numadd,
		; and di is stored in xxaddr.
	struc OPRND
.flags:		resb 1	; 0 flags (ARG_DEREF, etc)
.distflags:	resb 1	; 1 distance flags
	; (short = 1, near = 2, far = 4)
.sizearg:	resb 1	; 2 size argument, if any
	; (1=byte, 2=word, 4=dword, 5=qword, 6=float, 7=double, 8=tbyte)
	; (refer to SIZ_ equs, sizetcnam, and asm_siznum)
.sizedis:	resb 1	; 3 size of ModR/M displacement
.reg1:			; 4 ModR/M byte or first register (byte)
.numadd:	resb 1	; 4 number of additional bytes at num2 (up to 4)
.num2:			; 5 second number (word)
.reg2:		resb 1	; 5 index register, second register or SIB byte
		; reg2 needs to follow reg1 immediately
.index:		resb 1	; 6 index factor
.orednum:	resb 1	; 7 sizes of numbers are ORed here
.num:		resd 1	; 8 number
	endstruc 1

odfShort:	equ 1
odfNear:	equ 2
odfFar:		equ 4


ab01:
	mov word [ mneminfo ], si	; save this address
	pop si			; get position in line
ab01a:
	lodsb
ab01b:
	mov di, line_out

		; Loop over operands.
ab02:
	call iseol?
	jne ab04		; if not end of line
 ab99_j1:
	jmp ab99		; to next phase

ab04:
	push di			; clear out the next storage area
	mov cx, OPRND_size_w
	xor ax, ax
	rep stosw
	pop di

		; Small loop over "BYTE PTR" and segment prefixes.
ab05:
	dec si
	mov ax, word [ si ]
	and ax, TOUPPER_W
.checksize:
	cmp byte [ di+OPRND.sizearg ], SIZ_NONE
	jne .notsize		; if already have a size qualifier ("BYTE PTR",...)
	push di
	mov di, sizetcnam
	mov cx, sizetcnam_size_w
	repne scasw
	pop di
	je .size		; if found -->
.notsize:
.checkdist:
	test byte [ di + OPRND.distflags ], -1
	jnz .notdist
	push di
	mov di, distnam
	mov cx, distnam_size_w
	repne scasw
	pop di
	jne .notdist
	test cx, cx
	jnz .dist		; if not "FA"
	mov al, byte [ si+2 ]
	and al, TOUPPER
	cmp al, 'R'
	jne .notdist		; if not "FAR" (could be hexadecimal) -->
.dist:
		; 0 = far, 1 = near, 2 = short
	sub cl, distnam_size_w - 1
		; -2 = far, -1 = near, -0 = short
	neg cl
		; 2 = far, 1 = near, 0 = short
	mov ch, 1
	shl ch, cl
		; 4 = far, 2 = near, 1 = short
		;
		; This matches odfFar = 4, odfNear = 2, odfShort = 1
	mov byte [ di + OPRND.distflags ], ch
	jmp .skipptr

.size:
	sub cl, sizetcnam_size_w
	neg cl			; convert to 1..8
	mov byte [ di+OPRND.sizearg ], cl
.skipptr:
	call skipalpha		; go to next token
	mov ah, byte [si]
	and ax, TOUPPER_W
	cmp ax, "PT"
	jne ab05_j1		; if not "PTR"
	call skipalpha		; go to next token (ignore "PTR")
ab05_j1: equ $
	jmp ab05

.notdist:
ab07:
	call ab08
	jne ab09		; not a segment prefix -->
	jmp short ab05_j1	; if it was a segment prefix -->

		; Test for and process segment prefix
		;
		; INP:	b[aa_seg_pre]
		;	si-> string
		;	ax = w[si] (uppercased)
		; OUT:	NZ if no segment prefix,
		;	 si unchanged
		;	ZR if segment prefix,
		;	 si-> behind prefix + 1
		;	 al = character behind prefix
		; CHG:	ax, bx, cx
ab08:
	cmp byte [ aa_seg_pre ], 0
	jne .ret		; if we already have a segment prefix
	push di
	mov di, segrgnam
	mov cx, N_SEGREGS
	repne scasw
	pop di
	jne .ret		; if not found
	push si			; save si in case there's no colon
	lodsw			; skip "?s"
	call skipwhite
	cmp al, ':'
	jne .retpopsi		; if not followed by ':'
	pop ax			; discard saved si
	call skipwhite		; skip it
	mov bx, prefixlist + 5
	sub bx, cx
	mov al, byte [ bx ]	; look up the prefix byte
	mov byte [ aa_seg_pre ], al	; save it away
	cmp al, al		; ZR, valid segment prefix
.ret:
	retn

.retpopsi:
	pop si
	retn

		; Begin parsing main part of argument.

		; First check registers.
ab09:
	push di			; check for solo registers
	mov	di, rgnam816
	mov	cx, N_ALLREGS	; 27
	call aagetreg
	pop	di
	jc	ab14		; if not a register
	or	byte [di+OPRND.flags], ARG_JUSTREG
	mov	byte [di+OPRND.reg1], bl	; save register number
	cmp	bl, 24		; 0..23 = AL..DH, AX..DI, EAX..EDI
	jae	ab09a		; if it's not a normal register
	xchg ax, bx		; mov al, bl
	mov	cl, 3
	shr	al, cl		; al = size:  0 -> byte, 1 -> word, 2 -> dword
	add	al, -2
	adc	al, 3		; convert to 1, 2, 4 (respectively)
				;  matching SIZ_BYTE, SIZ_WORD, SIZ_DWORD
	jmp	short ab13

ab09a:
	xor	byte [di+OPRND.flags], ARG_JUSTREG|ARG_WEIRDREG
	mov	al, SIZ_WORD	; register size
	cmp	bl, REG_ST	; 24..29 = segment registers
	ja	ab11		; if it's MM, CR, DR, or TR -->
	je	ab09b		; if it's ST -->
	cmp	bl, 28
	jb	ab13		; if it's a non-386 segment register -->
	or	byte [asm_mn_flags], AMF_FSGS	; else flag it
	jmp	short ab13

		; ST registers
ab09b:
	lodsb
		; Check for NASM FPU register notation: ST0..ST7
	cmp al, '0'		; digit following ?
	jb .par
	cmp al, '7'
	ja .par			; no -->
	sub al, '0'
	mov byte [di+OPRND.reg2], al	; save number
	jmp short ab12		; -->

.par:
		; Check for MASM FPU register notation: ST(0)..ST(7)
	cmp al, '('		; parenthesis following ?
	je .ispar		; yes -->
		; Plain ST (= ST0)
	dec si
	jmp short ab12

.ispar:
	lodsb			; get digit
	sub al, '0'
	cmp al, 7
	ja ab10			; if not 0..7
	mov byte [di+OPRND.reg2], al	; save the number
	lodsb
	cmp al, ')'		; validate that there's a closing parenthesis
	je ab12			; okay -->
ab10:
 aa24_j2:
	jmp	aa24		; error

		; other registers: 31..34 (MM, CR, DR, TR)
ab11:
	lodsb
	sub	al, '0'		; get digit
	cmp	al, 7
	ja	ab10		; if error -->
	mov	byte [di+OPRND.reg2], al	; save the number
	mov	al, SIZ_DWORD	; register size
	cmp	bl, REG_MM
	jne	ab13		; if not MM register
	or	byte [di+OPRND.flags], ARG_JUSTREG
	mov	al, SIZ_QWORD
	db	__TEST_IMM16	; (skip mov)
ab12:
	mov	al, 0		; size for ST registers
ab13:
	cmp	al, byte [di+OPRND.sizearg]	; compare with stated size
	je	ab13a		; if same
	xchg	al, byte [di+OPRND.sizearg]
	test	al, al		; SIZ_NONE ?
	jnz	ab10		; if wrong size given, error -->
ab13a:
	jmp	ab44		; done with this operand

		; It's not a register reference.  Try for a number.
ab14:
	lodsb
	call aaifnum
	jc	ab17		; it's not a number
ab14a:
	call aageti		; get the number
	mov byte [di+OPRND.orednum], cl
	mov word [di+OPRND.num+0], bx
	mov word [di+OPRND.num+2], dx
	call skipwh0
	cmp	cl, 2
	jg	ab17		; if we can't have a colon here
	cmp	al, ':'
	jne	ab17		; if not xxxx:yyyy
	call skipwhite
	call aageti
	mov cx, word [di+OPRND.num+0]
	mov word [di+OPRND.num2], cx
	mov word [di+OPRND.num+0], bx
	mov word [di+OPRND.num+2], dx
	or byte [di+OPRND.flags], ARG_FARADDR
	jmp ab43		; done with this operand

ab15:
	jmp ab30		; do post-processing

		; Check for [...].
ab16:
	call skipwhite
ab17:
	cmp	al, '['		; begin loop over sets of []
	jne	ab15		; if not [
	or	byte [di+OPRND.flags], ARG_DEREF	; set the flag

		; Process NASM segment prefix inside brackets if any
	call skipwhite
	mov ah, byte [si]
	and ax, TOUPPER_W
	dec si			; set up for ab08
	call ab08
	jz ab19			; if segment prefix (called skipwhite)
ab18:
	call skipwhite
ab19:
	cmp	al, ']'		; begin loop within []
	je	ab16		; if done

		; Check for a register (within []).
	dec si
	push di
	mov	di, rgnam16
	mov	cx, N_REGS16
	call aagetreg
	pop	di
	jc	ab25		; if not a register
	cmp	bl, 16
	jae	ab20		; if 32-bit register
	add	bl, 8		; adjust 0..7 to 8..15
	jmp	short ab21
ab20:
	cmp	byte [di+OPRND.reg2], 0
	jnz	ab21		; if we already have an index
	call skipwhite
	dec	si
	cmp	al, '*'
	jne	ab21		; if not followed by '*'
	inc	si
	mov	byte [di+OPRND.reg2], bl	; save index register
	call skipwhite
	call aageti
	call aaconvindex
	jmp	short ab28	; ready for next part

ab21:
	cmp	byte [di+OPRND.reg1], 0
	jne	ab22		; if there's already a register
	mov	byte [di+OPRND.reg1], bl
	jmp	ab23
ab22:
	cmp	byte [di+OPRND.reg2], 0
	jne	ab24		; if too many registers
	mov	byte [di+OPRND.reg2], bl
ab23:
	call skipwhite
	jmp	short ab28	; ready for next part

ab24:
 aa24_j3:
	jmp	aa24		; error

		; Try for a number (within []).
ab25:
	lodsb
ab26:
	call aageti		; get a number (or flag an error)
	call skipwh0
	cmp	al, '*'
	je	ab27		; if it's an index factor
	or byte [di+OPRND.orednum], cl
	add word [di+OPRND.num+0], bx
	adc word [di+OPRND.num+2], dx
	jmp short ab28		; next part ...

ab27:
	call aaconvindex
	call skipwhite
	dec	si
	push di
	mov	di, rgnam16
	xor	cx, cx
	call aagetreg
	pop	di
	jc	ab24		; if error
	cmp	byte [di+OPRND.reg2], 0
	jne	ab24		; if there is already a register
	mov	byte [di+OPRND.reg2], bl
	call skipwhite

		; Ready for the next term within [].
ab28:
	cmp	al, '-'
	je	ab26		; if a (negative) number is next
	cmp	al, '+'
	jne	ab29		; if no next term (presumably)
	jmp ab18
ab29:
	jmp ab19		; back for more

		; Post-processing for complicated arguments.
ab30:
	cmp	word [di+OPRND.reg1], 0
	jne	ab32		; if registers were given ( ==> create MOD R/M)
	cmp	byte [di+OPRND.orednum], 0
	je	ab31		; if nothing was given ( ==> error)
	cmp	byte [di+OPRND.flags], 0
	jne	ab30b		; if it was not immediate
	or	byte [di+OPRND.flags], ARG_IMMED
ab30a:
	jmp	ab43		; done with this argument
ab30b:
	or	byte [asm_mn_flags], AMF_ADDR
	mov	al, 2		; size of the displacement
	test byte [di+OPRND.orednum], 4
	jz	ab30c		; if no 32-bit displacement -->
	inc	ax
	inc	ax		; al = 4
	or	byte [asm_mn_flags], AMF_A32	; 32-bit addressing
ab30c:
	mov	byte [di+OPRND.sizedis], al	; save displacement size
	jmp	short ab30a	; done with this argument
ab31:
	jmp	short aa24_j3	; flag an error

		; Create the MOD R/M byte.
		; (For disp-only or register, this will be done later as needed.)
ab32:
	or	byte [di+OPRND.flags], ARG_MODRM
	mov	al, byte [di+OPRND.reg1]
	or	al, byte [di+OPRND.reg2]
	test al, 10h
	jnz	ab34		; if 32-bit addressing
	test byte [di+OPRND.orednum], 4
	jnz	ab34		; if 32-bit addressing
	or byte [asm_mn_flags], AMF_ADDR
	mov	ax, word [di+OPRND.reg1]	; get reg1 and reg2
	cmp	al, ah
	ja	ab33		; make sure al >= ah
	xchg al, ah
ab33:
	push di
	mov	di, modrmtab
	mov	cx, 8
	repne scasw
	pop	di
	jne	ab31		; if not among the possibilities (error)
	mov	bx, 206h	; max disp = 2 bytes; 6 ==> (non-existent) [bp]
	jmp short ab39		; done (just about)

		; 32-bit addressing
ab34:
	or	byte [asm_mn_flags], AMF_A32 | AMF_ADDR	; 32-bit addressing
	mov	al, byte [di+OPRND.reg1]
	or	al, byte [di+OPRND.index]
	jnz	ab35		; if we can't optimize [Exx*1] to [Exx]
	mov	ax, word [di+OPRND.reg1]
	xchg al, ah
	mov	word [di+OPRND.reg1], ax
ab35:
	mov	bx, 405h	; max disp = 4 bytes; 5 ==> (non-existent) [bp]
	cmp	byte [di+OPRND.reg2], 0
	jne	ab36		; if there's a SIB
	mov	cl, byte [di+OPRND.reg1]
	cmp	cl, 16
	jl	ab31		; if wrong register type
	and	cl, 7
	cmp	cl, 4		; check for ESP
	jne	ab39		; if not, then we're done (otherwise do SIB)

ab36:
	or	byte [asm_mn_flags], AMF_SIB	; form SIB
	mov	ch, byte [di+OPRND.index]	; get SS bits
	mov	cl, 3
	shl	ch, cl		; shift them halfway into place
	mov	al, byte [di+OPRND.reg2]	; index register
	cmp	al, 20
	je	ab31		; if ESP ( ==> error)
	cmp	al, 0
	jne	ab37		; if not zero
	mov	al, 20		; set it for index byte 4
ab37:
	cmp	al, 16
	jl	ab31		; if wrong register type
	and	al, 7
	or	ch, al		; put it into the SIB
	shl	ch, cl		; shift it into place
	inc	cx		; R/M for SIB = 4
	mov	al, byte [di+OPRND.reg1]	; now get the low 3 bits
	test	al, al
	jnz	ab38		; if there was a first register
	or	ch, 5
	jmp short ab42		; MOD = 0, disp is 4 bytes

ab38:
	cmp	al, 16
	jl	ab45		; if wrong register type
	and	al, 7		; first register
	or	ch, al		; put it into the SIB
	cmp	al, 5
	je	ab40		; if it's EBP, then we don't recognize disp=0
				; otherwise bl will be set to 0

		; Find the size of the displacement.
ab39:
	cmp	cl, bl
	je	ab40		; if it's [(E)BP], then disp=0 is still 1 byte
	mov	bl, 0		; allow 0-byte disp

ab40:
	push cx
	mov	al, byte [di+OPRND.num+0]
	mov	cl, 7
	sar	al, cl
	pop	cx
	mov	ah, byte [di+OPRND.num+1]
	cmp	al, ah
	jne	ab41		; if it's bigger than 1 byte
	cmp	ax, word [di+OPRND.num+2]
	jne	ab41		; ditto
	mov	bh, 0		; no displacement
	or	bl, byte [di+OPRND.num+0]
	jz	ab42		; if disp = 0 and it's not (E)BP
	inc	bh		; disp = 1 byte
	or	cl, 40h		; set MOD = 1
	jmp short ab42		; done

ab41:
	or	cl, 80h		; set MOD = 2

ab42:
	mov byte [di+OPRND.sizedis], bh	; store displacement size
	mov word [di+OPRND.reg1], cx	; store MOD R/M and maybe SIB

		; Finish up with the operand.
ab43:
	dec	si
ab44:
	call	skipwhite
	add	di, byte OPRND_size
	call	iseol?
	je	ab99		; if end of line -->
	cmp	al, ','
	jne	ab45		; if not comma ( ==> error)
	cmp	di, line_out+3*OPRND_size
	jae	ab45		; if too many operands
	call skipwhite
	jmp	ab02

ab45:
	jmp	aa24		; error jump

ab99:
	mov	byte [di+OPRND.flags], -1	; end of parsing phase

%if 0
For the next phase, we match the parsed arguments with the set of
permissible argument lists for the opcode.  The first match wins.
Therefore the argument lists should be ordered such that the
cheaper ones come first.

There is a tricky issue regarding sizes of memory references.
Here are the rules:
   1.	If a memory reference is given with a size, then it's OK.
   2.	If a memory reference is given without a size, but some
	other argument is a register (which implies a size),
	then the memory reference inherits that size.
	Exceptions:	OP_CL does not imply a size (it's the shift counter)
			OP_SHOSIZ
   3.	If 1 and 2 do not apply, but this is the last possible argument
	list, and if the argument list requires a particular size, then
	that size is used.
   4.	In all other cases, flag an error.
%endif

ac01:
	xor	ax, ax
	mov	di, rmaddr
	mov	cx, 7
	rep	stosw		; clear variant-specific variables
				; cx = 0
	mov	si, word [mneminfo]
				; -> the next argument variant
%if _ASM_ESCAPE_USED
	xor	bx, bx		; cx:bx = counter of ASM_ESCAPEs
%endif
		; Parse the variant's assembler table entry
		; si-> next argument variant
		; variant-specific variables cleared
		; cx:bx = 0

ac02:
	lodsb
%if _ASM_ESCAPE_USED
	cmp	al, ASM_ESCAPE
	jne	.notescape
	add	bx, ASM_ESCAPE << 8
	adc	cx, byte 0
	jc	ac04		; if this branches, too many escapes -->
	jmp	short ac02
.notescape:
%endif
	sub	al, ASM_MACH1
	jb	ac05		; if no more special bytes
	cmp	al, ASM_LOCKABLE-ASM_MACH1
	je	ac03		; if ASM_LOCKABLE
	ja	ac04		; if ASM_END or another  (--> error)
	inc	ax
	mov	byte [dismach], al	; save machine type
	jmp	short ac02	; back for next byte
ac03:
	or	byte [varflags], VAR_LOCKABLE
	jmp	short ac02	; back for next byte

ac04:
	jmp	aa13a		; error

		; Get and unpack the word.
ac05:
	dec	si
	lodsw
	xchg	al, ah			; put into little-endian order
	xor	dx, dx
%if _ASM_ESCAPE_USED
	add	ax, bx			; add in the ASM_ESCAPE adjustment
	adc	dx, cx			; account for overflow (cx = 0)
%endif
	mov	bx, ASMMOD
	div	bx			; ax = a_opcode; dx = index into opindex
	mov	word [a_opcode], ax	; save ax
	mov	word [a_opcode2], ax	; save the second copy
	cmp	ax, 0DFh		; a coprocessor instruction ?
	ja	ac05a
	cmp	al, 0D8h
	jb	ac05a			; if no coprocessor instruction -->
	or	byte [dmflags], DM_COPR	; flag it as an x87 instruction
	mov	ah, al			; ah = low order byte of opcode
	lodsb				; get extra byte
	mov	byte [regmem], al	; save it in regmem
	mov	word [a_opcode2], ax	; save this for obsolete-instruction detection
	or byte [varflags], VAR_MODRM	; flag its presence
ac05a:
	mov	[mneminfo], si		; save si back again
	mov	si, dx
%if ASMMOD > 0FFh
	xor	bx, bx
%endif
	mov	bl, byte [opindex+si]
	add dx, dx
	dec dx
	add	bx, dx			; adjust to get correct index into oplists
	lea	si, [oplists+bx]	; si = the address of our operand list
	mov	di, line_out

		; Begin loop over operands.
		; [a_opcode] etc set for opcode
		; si-> operand list
		; di-> next parsed operand
		; [mneminfo]-> mnemonic's next variant in assembler table
ac06:
	lodsb			; get next operand byte
	cmp	al, OP_END
	je	ac10		; if end of list

		; The OP_STACK_* operand types don't really need another
		;  operand structure. So handle them before checking for
		;  a valid operand structure. This is required for the
		;  cases with no regular operands following the stack hint.
		; Because they aren't needed by the assembler anyway we
		;  just loop back to ac06 to load the next operand type.
	cmp al, OP_STACK_PUSH
	je ac06
	cmp al, OP_STACK_POP
	je ac06
	cmp al, OP_STACK_SPECIAL
	je ac06

		; Actual operand, or one of these always followed by one
		;  or more actual operands (OP_M_*, OP_SHORT|NEAR|FAR),
		;  so check for another valid operand structure.
	cmp	byte [di+OPRND.flags], -1
	je	ac01_j1		; if too few operands were given
	cmp	al, OP_SIZE
	jb	ac07		; if no size needed
%if 1
	mov	ah, 0
	mov	cl, 4
	shl	ax, cl		; move bits 4..7 (size) to ah (OP_1632=5, OP_8=6, OP_16=7, ...)
	shr	al, cl		; move bits 0..3 back
%else
	aam 16			; ax=00XY -> ax=0X0Y
%endif
	mov byte [a_reqsize], ah	; save it away
	jmp short ac08

ac07:				; al = OP_M64..
		; have al = 1..x
		; want al = 16..y
	add	al, 16-1	; adjust for the 16 start entries in asmjmp

ac08:
	cbw			; al = 0..7 or 16..y,
				; al < 128, thus ax = al
	xchg ax, bx		; now bx contains the index
	mov al, byte [bittab+bx]
	shl bx, 1		; = offset into word array
	mov cx, word [asmjmp + bx]
				; subroutine address
	shr bx, 1		; return to index
	cmp bx, OP_AMOUNT_TABLE
	jae ac09_internal_error	; internal error
	test al, byte [di+OPRND.flags]
	jz	ac09		; if no required bits are present
	mov ah, 0		; (insure ah = 0 for ao90)
	call cx			; call its specific routine
	mov al, [alloweddist]
	not al
	test byte [di + OPRND.distflags], al
	jnz	ac09		; if invalid distance specified -->
	cmp word [si-1], (OP_1632|OP_R)<<8|(OP_1632|OP_R_MOD)
	je	ac06_j1		; (hack) for IMUL instruction
	add	di, byte OPRND_size
				; -> next operand
 ac06_j1:
	jmp	ac06		; back for more

ac09_internal_error:
	mov dx, msg.aa_internal_error
	call putsz
ac09:
 ac01_j1:
	jmp	ac01		; back to next possibility

		; End of operand list.
ac10:
	cmp	byte [di+OPRND.flags], -1
	jne	ac09		; if too many operands were given

	test byte [varflags], VAR_MODRM
	jz @F			; if no ModR/M -->
	cmp byte [regmem], 0C0h
	jb @F			; if not both high bits set -->
				; both bits are set, ModR/M is for a register operand
	clropt [varflags], VAR_M_ALWAYS_16
				; do not special case mov with segreg and a GPR
@@:

		; Final check on sizes
	mov al, byte [varflags]
	test al, VAR_SIZ_NEED
	jz	ac12		; if no size needed
	test al, VAR_SIZ_GIVN
	jnz	ac12		; if a size was given
	test al, VAR_SIZ_FORCD
	jz	ac09		; if the size was not forced ( ==> reject)
	mov si, word [mneminfo]
	cmp byte [si], ASM_END
	je	ac12		; if this is the last one
ac11:
	jmp	aa13a		; it was not, error --> (not a retry)

ac12:
	test al, VAR_M_ALWAYS_16
	jz @F
	mov al, byte [opsize]
	cmp al, SIZ_NONE
	je @F
	cmp al, SIZ_WORD
	je @F
	jmp aa13a
@@:

		; Check other prefixes.
	mov	al, byte [aa_saved_prefix]
	cmp	al, 0
	je	ac14		; if no saved prefixes to check
	cmp	al, 0F0h
	jne	ac13		; if it's a rep prefix
	test byte [varflags], VAR_LOCKABLE
	jz	ac11		; if this variant is not lockable, error -->
	jmp short ac14		; done

ac13:
	mov ax, word [a_opcode]	; check if opcode is OK for rep{,z,nz}
	and	al, ~1		; clear low order bit (MOVSW -> MOVSB)

	cmp	ax, 0FFh
	ja	ac11		; if it's not a 1-byte instruction, error -->
	mov	di, replist	; list of instructions that go with rep
	mov	cx, REP_LEN	; scan all (REP + REPxx)
	repne	scasb
	jne	ac11		; if it's not among them, error -->

ac14:
	test byte [asm_mn_flags], AMF_MSEG
	jz	ac15		; if no segment prefix before mnemonic -->
	mov ax, word [a_opcode]	; check if opcode allows this
	cmp	ax, 0FFh
	ja	ac11		; if it's not a 1-byte instruction, error -->
	mov	di, segprfxtab
	mov	cx, SEGP_LEN
	repne	scasb
	jne	ac11		; if it's not in the list, error -->

ac15:
	mov bx, word [immaddr]
	or	bx, bx
	jz	ac16		; if no immediate data
	mov al, byte [opsize]
	neg	al
	shl	al, 1
	test al, byte [bx+7]
	jnz	ac11		; if the immediate data was too big, error -->

		; Put the instruction together
		; (maybe is this why they call it an assembler)

		; First, the prefixes (including preceding WAIT instruction)
ac16:
	_386_PM_o32	; mov edi, dword [a_addr]
	mov di, word [a_addr]
%if _PM
	mov bx, word [a_addr + saSegSel]
	call verifysegm_or_error
	mov es, bx
%else
	mov es, word [a_addr + saSegSel]
%endif
	test byte [asm_mn_flags], AMF_WAIT
	jz	.nowaitprefix	; if no wait instruction beforehand
	mov	al, 9Bh
	_386_PM_a32
	stosb
.nowaitprefix:

	mov	al,[aa_saved_prefix]
	test	al, al
	jz	.noprefix	; if no LOCK or REP prefix
	_386_PM_a32
	stosb
.noprefix:

;--- a 67h address size prefix is needed
;--- 1. for CS16: if AMF_ADDR=1 and AMF_A32=1
;--- 2. for CS32: if AMF_ADDR=1 and AMF_A32=0

	mov al, byte [asm_mn_flags]
	test al, AMF_ADDR
	jz .noaddressprefix
	and al, AMF_A32
%if _PM
	or al, byte [bCSAttr]
	jz .noaddressprefix	; if 16-bit CS and 16-bit addressing -->
	cmp al, AMF_A32| 40h
	jz .noaddressprefix	; if 32-bit CS and 32-bit addressing -->
%else
	jz .noaddressprefix	; 16-bit addressing in RM -->
%endif
		; Otherwise, the CS and addressing bitness mismatch. Write a prefix.
	mov al, 67h
	_386_PM_a32
	stosb			; store address size prefix
.noaddressprefix:

;--- a 66h data size prefix is needed
;--- for CS16: if VAR_D32 == 1 or AMF_D32 == 1
;--- for CS32: if VAR_D16 == 1 or AMF_D16 == 1

	testopt [varflags], VAR_M_ALWAYS_16
	jnz .nodataprefix	; mov segreg never emits an osize -->
	mov ah, byte [asm_mn_flags]
	mov al, byte [varflags]
%if _PM
	test byte [bCSAttr], 40h
	jz .dataprefix_rm
	test ax, VAR_D16|(AMF_D16<<8)
	jnz .dataprefix
	jmp short .nodataprefix
.dataprefix_rm:
%endif
	test ax, VAR_D32|(AMF_D32<<8)
	jz .nodataprefix
.dataprefix:
	mov	al, 66h
	_386_PM_a32
	stosb			; store operand size prefix
.nodataprefix:

	mov	al, [aa_seg_pre]
	cmp	al, 0
	je	ac22		; if no segment prefix
	_386_PM_a32
	stosb
	cmp	al, 64h
	jb	ac22		; if not 64 or 65 (FS or GS) -->
	or	byte [asm_mn_flags], AMF_FSGS	; flag it
ac22:

		; Now emit the instruction itself.
	mov	ax, word [a_opcode]
	mov	bx, ax
	sub	bx, 240h
	jae	ac23		; if 576.. (AGRP) -->
	cmp	ax, 200h
	jb	ac24		; if regular instruction -->
	or	byte [dmflags], DM_COPR	; flag it as an x87 instruction
	and	al, 38h		; get register part
	or	byte [regmem], al
	xchg	ax, bx		; mov ax, bx (the low bits of bx are good)
	and	al, 7
	or	al, 0D8h
	jmp	short ac25	; on to decoding the instruction

ac23:
	or byte [varflags], VAR_MODRM	; flag presence of ModR/M byte
	mov	cl, 3		; one instruction of a group
	shr	bx, cl		; separate AGRP()'s num part
	and	al, 7		; separate ModR/M register value
	shl	al, cl
	or	byte [regmem], al	; fix ModR/M byte
	shl	bx, 1
	mov	ax, word [agroups+bx]	; get actual opcode

ac24:
	test	ah, ah
	jz	ac25		; if no 0Fh first -->
	push	ax		; store a 0Fh
	mov	al, 0Fh
	_386_PM_a32
	stosb
	pop	ax
ac25:
	or al, byte [opcode_or]	; put additional bits into the opcode
	_386_PM_a32
	stosb			; store the op code itself

		; Now store the extra stuff that comes with the instruction.
	mov ax, word [regmem]
	test byte [varflags], VAR_MODRM
	jz ac26			; if no ModR/M -->
	_386_PM_a32
	stosb			; store the ModR/M byte
	test byte [asm_mn_flags], AMF_SIB
	jz ac26			; if no SIB -->
	mov al, ah
	_386_PM_a32
	stosb			; store the SIB byte, too
ac26:

	mov	bx, word [rmaddr]
	test	bx, bx
	jz ac27			; if no offset associated with the R/M -->
	_386_PM_o32	; xor ecx, ecx
	xor	cx, cx
	mov	cl, byte [bx+OPRND.sizedis]
	_386_PM_o32	; lea esi, [bx+OPRND.num]
	lea	si, [bx+OPRND.num]
	_386_PM_a32
	rep	movsb		; store the R/M offset (or memory offset)
ac27:

		; Now store immediate data
	mov	bx, word [immaddr]
	test	bx, bx
	jz	ac28		; if no immediate data -->
	_386_PM_o32	; xor ecx, ecx
	xor	cx, cx
	mov	cl, byte [opsize]
	_386_PM_o32	; lea esi, [bx+OPRND.num]
	lea	si, [bx+OPRND.num]
	_386_PM_a32
	rep movsb
ac28:

		; Now store additional bytes (needed for, e.g., enter instruction
		; and far memory address)
	mov	bx, word [xxaddr]
	test	bx, bx
	jz	ac29		; if no additional data -->
	_386_PM_o32	; lea esi, [bx+OPRND.numadd]
	lea	si, [bx+OPRND.numadd]
	_386_PM_o32	; xor eax, eax
	xor	ax, ax
	lodsb
	_386_PM_o32	; xchg eax, ecx
	xchg	ax, cx		; (mov cx, ax)
	_386_PM_a32
	rep	movsb
ac29:

		; Done emitting. Update assembler address offset.
	push ss
	pop es			; restore es
	_386_PM_o32	; mov dword [a_addr], edi
	mov word [a_addr], di

		; Compute machine type.
	cmp byte [dismach], 3
	jae ac31		; if we already know a 386 is needed
	test byte [asm_mn_flags], AMF_D32 | AMF_A32 | AMF_FSGS
	jnz ac30		; if 386 -->
	test byte [varflags], VAR_D32
	jz ac31			; if not 386 -->
ac30:
	mov byte [dismach], 3
ac31:
	mov di, a_obstab	; info on this instruction
	mov cx, word [a_opcode2]
	call showmach		; get machine message into si, length into cx
	jcxz ac33		; if no message

ac32:
	mov	di, line_out
	rep	movsb		; copy the line to line_out
	call putsline_crlf

ac33:
%if _IMMASM
	jmp	near [ aa_ret ]	; back for the next input line
%else
	jmp	aa01
%endif


%if 0
		; This is debugging code.  It assumes that the original value
		; of a_addr is on the top of the stack.
	pop	si		; get orig. a_addr
	mov	ax, word [a_addr + saSegSel]
	mov	word [u_addr], si
	mov	word [u_addr + saSegSel], ax	; (doesn't work with 32-bit CS)
%if _PM
	mov ax, word [a_addr + saSegment]
	mov word [u_addr + saSegment], ax
	mov ax, word [a_addr + saSelector]
	mov word [u_addr + saSelector], ax
%endif
	mov	bx, word [a_addr]
	sub	bx, si
	mov	di, line_out
	mov	cx, 10
	mov	al, ' '
	rep	stosb
	mov	ds, word [a_addr + saSegSel]

ax1:	lodsb
	call hexbyte		; display the generated bytes
	dec	bx
	jnz	ax1
	push	ss
	pop	ds
	call putsline_crlf
	and word [disflags], 0
	call disasm		; disassemble the new instruction
%if _IMMASM
	jmp	near [ aa_ret ]	; back to next input line
%else
	jmp	aa01
%endif
%endif

		; Routines to check for specific operand types.
		; Upon success, the routine returns.
		; Upon failure, it pops the return address and jumps to ac01.
		; The routines must preserve si and di.

		; OP_RM, OP_M, OP_R_MOD:  form MOD R/M byte.
aop_rm:
aop_m:
aop_r_mod:
	call ao90		; form reg/mem byte
	jmp short ao07		; go to the size check

		; OP_R:  register.
aop_r:
	mov al, byte [di+OPRND.reg1]	; register number
	and al, 7
	mov cl, 3
	shl al, cl		; shift it into place
	or byte [regmem], al	; put it into the reg/mem byte
	jmp short ao07		; go to the size check

		; OP_R_ADD:  register, added to the instruction.
aop_r_add:
	mov al, byte [di+OPRND.reg1]
	and al, 7
	mov byte [opcode_or], al	; put it there
	jmp short ao07		; go to the size check

		; OP_IMM:  immediate data.
aop_imm:
	mov word [immaddr], di	; save the location of this
	jmp short ao07		; go to the size check

		; OP_MOFFS:  just the memory offset
aop_moffs:
	test byte [di+OPRND.flags], ARG_MODRM
	jnz ao11		; if MOD R/M byte ( ==> reject)
	mov word [rmaddr], di	; save the operand pointer
	jmp short ao07		; go to the size check

		; OP_AX:  check for AL/AX/EAX
aop_ax:
	test byte [di+OPRND.reg1], 7
	jnz ao11		; if wrong register, reject -->
	; jmp short ao07	; go to the size check

		; Size check
ao07:
	or byte [varflags], VAR_SIZ_NEED
	mov al, byte [a_reqsize]
	sub al, 5		; OP_1632 >> 4
	jl ao12			; if OP_ALL
	je ao13			; if OP_1632
	cmp al, 5		; OP_1632_DEFAULT ?
	je ao_1632_default
		; OP_8 = 1, OP_16 = 2, OP_32 = 3, OP_64 = 4
	add al, -3
	adc al, 3		; convert 3 --> 4 and 4 --> 5
ao08:
	or byte [varflags], VAR_SIZ_FORCD| VAR_SIZ_NEED
ao08_1:
	mov bl, byte [di+OPRND.sizearg]
	test bl, bl		; SIZ_NONE ?
	jz ao09			; yes, if no size given -->
	or byte [varflags], VAR_SIZ_GIVN
	cmp al, bl
	jne ao11		; if sizes conflict, reject -->
ao09:
	cmp al, byte [opsize]
	je ao10			; if sizes agree -->
	cmp al, -1		; is it OP_MXX (for lea) ?
	je ao10
	xchg al, byte [opsize]
	cmp al, SIZ_NONE
	jne ao11		; if sizes disagree, reject -->
	or byte [varflags], VAR_SIZ_GIVN	; added in DEBUG/X 1.18
ao10:
	retn

ao11:
ao50_j1:
	jmp ao50		; reject

		; OP_ALL - Allow all sizes.
ao12:
	mov al, byte [di+OPRND.sizearg]
	cmp al, SIZ_BYTE
	je ao15			; if byte
	jb ao14			; if unknown (SIZ_NONE) -->
	or byte [opcode_or], 1	; set bit in instruction
	jmp short ao14		;  if size is 16 or 32

		; OP_1632_DEFAULT
ao_1632_default:
	mov al, byte [di+OPRND.sizearg]
	test al, al		; SIZ_NONE ?
	jnz @F			; no -->
	mov al, byte [opsize]	; (for push imm16/32)
	test al, al		; SIZ_NONE ?
	jnz @F			; no -->
	mov al, SIZ_WORD
%if _PM
	test byte [bCSAttr], 40h
	jz @F
	mov al, SIZ_DWORD
%endif
	jmp @F

		; OP_1632 - word or dword.
ao13:
	mov al, byte [di+OPRND.sizearg]
ao14:
	test al, al		; SIZ_NONE ?
	jz ao16			; yes, if still unknown -->
@@:
	cmp al, SIZ_WORD
	jne ao15_1		; if word
	or byte [varflags], VAR_D16
	jmp short ao15
ao15_1:
	cmp al, SIZ_DWORD
	jne ao11		; if not dword
	or byte [varflags], VAR_D32	; set flag
ao15:
	or byte [varflags], VAR_SIZ_GIVN
		; hack for pushd/pushw imm: check for match
	jmp ao09
ao16:
	retn

		; OP_M64 - 64-bit memory reference.
		; OP_MFLOAT - single-precision floating point memory reference.
		; OP_MDOUBLE - double-precision floating point memory reference.
		; OP_M80 - 80-bit memory reference.
		; OP_MXX - memory reference, size unknown.
		; INP:	(from ac08 calling this:)
		;	bx = index into bittab
ao17:
	call ao90		; form reg/mem byte
	mov al, byte [asm_siznum + bx - (OP_FIRST_ASM_SIZNUM + 16 - 1)]
	jmp ao08		; check size

		; OP_FARIMM - far address contained in instruction
ao21:
	mov al, byte [di+OPRND.sizearg]
	test al, al		; have a size ? (check for not SIZ_NONE)
	jnz @F			; yes -->
	mov al, SIZ_WORD	; default to word, assuming 16-bit CS
%if _PM
	test byte [bCSAttr], 40h; is it a 32-bit CS ?
	jz @F			; no -->
	mov al, SIZ_DWORD	; yes, default to dword
%endif

@@:
	cmp al, SIZ_WORD	; is it word ?
	jne .o32_check		; no, check for dword size -->
.o16:
	or byte [varflags], VAR_D16
				; mark flag for o16 prefix if needed

	cmp word [di+OPRND.num+2], byte 0
	jne ..@ao50_j_NZ	; if not a 16-bit offset -->

	jmp short .common

.o32_check:
	cmp al, SIZ_DWORD	; is it dword ?
..@ao50_j_NZ:
	jne ao50		; no, invalid -->
.o32:
	or byte [varflags], VAR_D32
				; mark flag for o32 prefix if needed

.common:
	or byte [varflags], VAR_SIZ_GIVN

	mov byte [di+OPRND.numadd], 2	; 2 additional bytes (segment part)
	mov word [immaddr], di
	mov byte [opsize], al	; size of offset, 2 or 4
ao22a:
	mov word [xxaddr], di
	retn

		; OP_REL8 - relative address
ao23:
	cmp byte [di+OPRND.sizearg], SIZ_NONE
	jne ..@ao50_j_NZ

	_386_PM_o32
	mov ax, word [a_addr]	; offset
	_386_PM_o32
	inc ax
	_386_PM_o32
	inc ax			; $
	mov cl, byte [asm_mn_flags]

	test cl, AMF_ADDR
	jnz ao23aa
		; JxCXZ, LOOPx, LOOPZx and LOOPNZx need a 67h, not a 66h prefix
	test cl, AMF_D32 | AMF_D16
	jz ao23b		; if not JxCXZ, LOOPx
	test cl, AMF_D32
	jz ao23a
	or cl, AMF_A32
ao23a:
	and cl, ~(AMF_D32 | AMF_D16)
	or cl, AMF_ADDR
	mov byte [asm_mn_flags], cl
ao23aa:
	and cl, AMF_A32
%if _PM
	or cl, byte [bCSAttr]
	jz ao23b		; 16-bit CS and addressing -->
	cmp cl, AMF_A32| 40h
	je ao23b		; 32-bit CS and addressing -->
%else
	jz ao23b		; RM CS and 16-bit addressing -->
%endif
	_386_PM_o32
	inc ax			; adjust $ for the prefix that will be used
ao23b:
	mov bx, ax
	xor cx, cx
_386_PM	push eax
_386_PM pop ax
_386_PM pop cx
	mov ax, word [di+OPRND.num+0]
	mov dx, word [di+OPRND.num+2]

%if _IMMASM
	testopt [internalflags6], dif6_immasm
	jz @F

	mov word [immasm_rel8_target+0], ax
	mov word [immasm_rel8_target+2], dx
	setopt [internalflags6], dif6_immasm_rel8

	mov ax, bx
	mov dx, cx
@@:
%endif

	sub ax, bx
	sbb dx, cx

	mov byte [di+OPRND.num2], al
	cmp ax, 80h		; just one byte beyond range ?
	jne @F
	test dx, dx
	jnz @F			; no -->
	cmp byte [si], OP_E_CX	; we're trying for loop with (e)cx explicit ?
	jne @F			; no -->
	setopt [di + OPRND.flags], ARG_ECX_SPECIAL
				; remember that we have to overflow
	jmp @FF
@@:
	mov cl, 7
	sar al, cl
	cmp al, ah
	jne ao24		; if too big, reject -->
	cmp ax, dx
	jne ao24		; if too big, reject -->
@@:
	mov byte [di+OPRND.numadd], 1	; save the length
	jmp ao22a		; save it away


		; OP_REL1632:  relative jump/call to a longer address.
		;
		; size of instruction is
		; a) CS 16-bit:
		;  3 (xx xxxx, near jmp/call E9/E8) or
		;  4 (0F xx xxxx, near jcc 0F 80+cc)
		;  6 (66 xx xxxxxxxx, near 32-bit jmp/call E9/E8)
		;  7 (66 0F xx xxxxxxxx, near 32-bit jcc 0F 80+cc)
		;
		; b) CS 32-bit:
		;  5 (xx xxxxxxxx, near jmp/call E9/E8) or
		;  6 (0F xx xxxxxxxx, near jcc 0F 80+cc)
		;  4 (66 xx xxxx, near 16-bit jmp/call E9/E8)
		;  5 (66 0F xx xxxx, near 16-bit jcc 0F 80+cc)
ao25:
	mov bx, word [a_addr+0]
%if _PM
	mov cx, word [a_addr+2]
%else
	xor cx, cx
%endif

	xor ax, ax
	mov al, byte [di+OPRND.sizearg]
	test al, al		; SIZ_NONE ?
	jnz @F			; no -->
	mov al, SIZ_WORD	; in 16-bit CS default to word (2)
%if _PM
	test byte [bCSAttr], 40h
	jz @F
	mov al, SIZ_DWORD	; in 32-bit CS default to dword (4)
%endif
@@:
	push ax			; preserve size

%if _PM
	test byte [bCSAttr], 40h
	jz .adjust_16bitcs
.adjust_32bitcs:
	cmp al, SIZ_DWORD	; default size ?
	jmp .adjust_common
%endif

.adjust_16bitcs:
	cmp al, SIZ_WORD	; default size ?
.adjust_common:
	je .adjust_done
	inc ax			; no, increment for 66h prefix (osize)
.adjust_done:

	inc ax			; add size of opcode (E8h/E9h/80h+cc)

	cmp word [a_opcode], 100h
				; is it a 0Fh-prefixed opcode ?
				;  (that is, a 0Fh 80h+cc conditional jump)
	jb @F
	inc ax			; add size of 0Fh prefix opcode
@@:
	add bx, ax
	adc cx, byte 0
	pop ax			; restore size (2 or 4)
	mov dx, word [di+OPRND.num+2]

	cmp al, SIZ_DWORD
	je ao27_1		; if the size given was "dword" -->

ao27:
	test dx, dx
	jnz ao24		; if operand is too big, reject -->
	or byte [varflags], VAR_D16
	jmp short ao28

ao27_1:
	or byte [varflags], VAR_D32

ao28:
	mov byte [di+OPRND.numadd], al	; store the size
	mov ax, word [di+OPRND.num+0]
%if _IMMASM
	mov word [immasm_rel1632_target+0], ax
	mov word [immasm_rel1632_target+2], dx
%endif
	xor cx, cx
	sub ax, bx
	sbb dx, cx		; compute DX:AX - CX:BX
	mov word [di+OPRND.num2+0], ax
	mov word [di+OPRND.num2+2], dx
	mov word [xxaddr], di
	retn

ao24:
ao50_j2:
	jmp ao50		; reject

		; OP_1CHK - The assembler can ignore this one.
ao29:
	pop ax			; discard return address
 ac06_j2:
	jmp ac06_j1		; next operand

		; OP_STI - ST(I).
aop_sti:
	mov al, REG_ST		; code for ST
	mov bl, byte [di+OPRND.reg2]
	jmp short ao38		; to common code -->

		; OP_MMX (previously was OP_ECX (used for LOOPx))
aop_mmx:
	mov al, REG_MM
	jmp short ao37		; to common code -->

		; OP_MMX_MOD
aop_mmx_mod:
	mov al, REG_MM
	mov bl, byte [di+OPRND.reg2]
	or bl, 0C0h
	jmp short ao38

		; OP_CR
aop_cr:
	mov al, byte [di+OPRND.reg2]	; get the index
	cmp al, 4
	ja ao24			; if too big, reject -->
	jne ao32		; if not CR4
	mov byte [dismach], 5	; CR4 is new to the 586
ao32:
	cmp al, 1
	jne ao33
	cmp byte [di+OPRND_size+OPRND.flags], -1
	jne ao24		; reject if no other arg (can't mov CR1,xx)
ao33:
	mov al, REG_CR		; code for CR
	jmp short ao37		; to common code

		; OP_DR
ao34:
	mov al, REG_DR		; code for DR
	jmp short ao37		; to common code

		; OP_TR
ao35:
	mov al, byte [di+OPRND.reg2]	; get the index
	cmp al, 3
	jb ao24			; if too small, reject -->
	cmp al, 6
	jae ao36
	mov byte [dismach], 4	; TR3-5 are new to the 486
ao36:
	mov al, REG_TR		; code for TR

		; Common code for these weird registers.
ao37:
	mov bl, byte [di+OPRND.reg2]
	mov cl, 3
	shl bl, cl
ao38:
	or byte [regmem], bl
	or byte [varflags], VAR_MODRM
	cmp al, byte [di+OPRND.reg1]	; check for the right numbered register
	je ao40			; if yes, then return
ao38a:
	jmp ao50		; reject -->

		; OP_SEGREG
ao39:
	mov al, byte [di+OPRND.reg1]
	sub al, 24
	cmp al, 6
	jae ao38a		; if not a segment register, reject -->
	mov cl, 3
	shl al, cl
	or byte [regmem], al
;--- v1.26: don't force size for MOV sreg, mxx / MOV mxx, sreg
	or byte [varflags], VAR_SIZ_GIVN
ao40:
	retn

		; OP_IMMS8 - Sign-extended immediate byte (PUSH xx)
ao41:
	and byte [varflags], ~VAR_SIZ_NEED	; added for v1.09. Ok?
ao41_extend:
	mov ax, word [di+OPRND.num+0]
	mov cl, 7
	sar al, cl
	jmp short ao43		; common code

		; OP_IMM8 - Immediate byte
ao42:
	mov ax, word [di+OPRND.num+0]
	mov al, 0
ao43:
	cmp al, ah
	jne ao50		; if too big, reject -->
	cmp ax, word [di+OPRND.num+2]
	jne ao50		; if too big, reject -->
	mov al, SIZ_BYTE
	call aasizchk		; check that size == 0 or 1
	mov ah, byte [di+OPRND.num+0]	; store the byte, length 1
	mov word [di+OPRND.numadd], ax	; store length (0/1) + the byte
	mov word [xxaddr], di
ao43r:
	retn


aop_e_cx:
	mov cx, AMF_ADDR | 0 | (~AMF_A32 << 8)
	cmp word [di + OPRND.reg1], REG_CX
	je .a16
.a32:
	mov cx, AMF_ADDR | AMF_A32 | (~0 << 8)
	call .common
	mov al, REG_CX + 8
	jmp ao48a

.a16:
.common:
	and byte [asm_mn_flags], ch
	or byte [asm_mn_flags], cl

	mov bx, word [xxaddr]
	mov cl, byte [asm_mn_flags]
	and cl, AMF_A32
%if _PM
	or cl, byte [bCSAttr]
	jz .noprefix		; 16-bit CS and addressing -->
	cmp cl, AMF_A32| 40h
	je .noprefix		; 32-bit CS and addressing -->
%else
	jz .noprefix		; 86 Mode CS and 16-bit addressing -->
%endif
	dec byte [bx + OPRND.num2]
	jno .ret		; (no need to check special flag)
.prefix_overflow:
	testopt [bx + OPRND.flags], ARG_ECX_SPECIAL
	jz .ao50		; not special, overflow -128 to +127
				; special, accept +128 to +127
.ret:
	retn

.noprefix:
	testopt [bx + OPRND.flags], ARG_ECX_SPECIAL
	jz .ret			; not special, no prefix
				; special and no prefix: reject
.ao50:
	jmp ao50


		; OP_SHOSIZ - force the user to declare the size of the next operand
ao44:
	test byte [varflags], VAR_SIZ_NEED
	jz ao45			; if no testing needs to be done
	test byte [varflags], VAR_SIZ_GIVN
	jz ao50			; if size was given ( ==> reject)
ao45:
	and byte [varflags], ~VAR_SIZ_GIVN	; clear the flag
	cmp byte [si], OP_IMM8
	je ao45a		; if OP_IMM8 is next, then don't set VAR_SIZ_NEED
	or byte [varflags], VAR_SIZ_NEED
ao45a:

		; hack for pushd/pushw imm (the only OP_SHOSIZ with suffix),
		;  set operand size to 2 for pushw, 4 for pushd.
		;  this is checked by ao15 later so as to match.
	mov al, 2
	mov ah, byte [aa_mnemsuffix]
				; 0 = normal, 'W' suffix, or 'D' suffix
	cmp ah, 'D'
	je @F			; if D, al = 2 -->
	dec ax			; al = 1
	cmp ah, 'W'
	je @F			; if W, al = 1 -->
	dec ax			; al = 0
@@:
	add al, al		; 0 = no suffix, 2 = 'W' suffix, 4 = 'D' suffix
				;  as 0 = SIZ_NONE, 2 = SIZ_WORD, 4 = SIZ_DWORD
	mov byte [opsize], al
ao_modifier_continue:
	pop ax			; discard return address
	jmp ac06_j2		; next operand


ao_m_always_16:
	setopt [varflags], VAR_M_ALWAYS_16
	jmp ao_modifier_continue


ao_short:
	mov al, odfShort
ao_distance:
	or byte [alloweddist], al
	jmp ao_modifier_continue

ao_near:
	mov al, odfNear
	jmp ao_distance

ao_far_required:
	test byte [di + OPRND.distflags], odfFar
	jz ao50			; if not far, reject -->
ao_far:
	mov al, odfFar
	jmp ao_distance


		; OP_1
ao46:
	cmp word [di+OPRND.orednum], 101h
		; check both size and value (OPRND.num)
	jmp short ao49		; test it later

		; OP_3
ao47:
	cmp byte [di + OPRND.sizearg], SIZ_NONE
	jne ao50		; if BYTE is specified, reject this -->
	cmp word [di+OPRND.orednum], 301h
		; check both size and value (OPRND.num)
	jmp short ao49		; test it later

		; OP_DX, OP_CL, OP_ST, OP_ES, ..., OP_GS
		; INP:	(from ac08 calling this:)
		;	bx = index into bittab
ao48:
	mov al, [asm_regnum + bx - (OP_FIRST_ASM_REGNUM + 16 - 1)]
ao48a:
	cbw
	cmp ax, word [di+OPRND.reg1]

ao49:
	je ao51

		; Reject this operand list.
ao50:
	pop ax			; discard return address
	jmp ac01		; go back to try the next alternative

ao51:
	retn

		; AASIZCHK - Check that the size given is 0 or AL.
aasizchk:
	cmp byte [di+OPRND.sizearg], SIZ_NONE
	je ao51
	cmp byte [di+OPRND.sizearg], al
	je ao51
	pop ax			; discard return address
	jmp short ao50		; reject this list -->

		; Do reg/mem processing.
		;
		; INP:	di-> OPRND structure
		; CHG:	ax
ao90:
	test byte [di+OPRND.flags], ARG_JUSTREG
	jnz ao92		; if just register
	test byte [di+OPRND.flags], ARG_MODRM
	jz ao91			; if no precomputed MOD R/M byte
	mov ax, word [di+OPRND.reg1]	; get the precomputed bytes
	jmp short ao93		; done

ao91:
	mov al, 6		; convert plain displacement to MOD R/M
	test byte [asm_mn_flags], AMF_A32
	jz ao93			; if 16-bit addressing
	dec ax
	jmp short ao93		; done

ao92:
	mov al, byte [di+OPRND.reg1]	; convert register to MOD R/M
%if 1
	cmp al, REG_MM
	jne .notmm
	mov al, byte [di+OPRND.reg2]
.notmm:
%endif
	and al, 7		; get low 3 bits
	or al, 0C0h

ao93:
	or word [regmem], ax	; store the MOD R/M and SIB
	or byte [varflags], VAR_MODRM	; flag its presence
	mov word [rmaddr], di	; save a pointer
	retn			; done

		; AAIFNUM - Determine if a number starts here
		;
		; INP:	al = first character
		;	si-> next character
		; OUT:	CY if no number starts there
		;	NC if a number starts there
		; CHG:	-
		;
		; Note:	Actually checks for a plus or minus sign that
		;	 is followed by a valid (hexadecimal) digit,
		;	 or just a digit without specified sign.
aaifnum:
	push	si
	 push	ax
	cmp	al, '-'		; minus or plus sign ?
	je	@F
	cmp	al, '+'
	jne	@FF		; no -->
@@:
	call	skipwhite	; skip sign, and (if any) blanks
@@:
	cmp	al, '('
	stc
	je	@F
	sub	al, '0'
	cmp	al, 10
@@:
	 pop	ax
	jb	@F		; if a decimal digit -->

	 push	ax
	and	al, TOUPPER
	sub	al, 'A'
	cmp	al, 6
	 pop	ax
@@:
	cmc			; carry clear <==> it's a number
	pop	si
	retn


		; AAGETI - Get a number from the input line.
		;
		; Entry	AL	First character of number
		;	SI	Address of next character of number
		; Exit	DX:BX	Resulting number
		;	CL	1 if it's a byte, 2 if a word, 4 if a dword
		;	AL	Next character not in number
		;	SI	Address of next character after that
		; Uses	AH, CH
aageti:
; Incorporate expression evaluator here.
	cmp	al, '-'
	je	aag1		; if negative number -->
	cmp	al, '+'		; (unnecessary) plus sign ?
	jne	@F		; no -->
	call	skipwhite	; skip it, plus blanks
@@:
	call aag4		; get the bare number
	mov	cx, 1		; set up cx
	or	dx, dx
	jnz	aag2		; if dword
	or	bh, bh
	jnz	aag3		; if word
	retn			; it's a byte

aag1:
	call	skipwhite
	call aag4		; get the bare number
	mov	cx, bx
	or	cx, dx
	mov	cx, 1
	jz	aag1a		; if -0
	not	dx		; negate the answer
	neg	bx
	cmc
	adc	dx, byte 0
	test dh, 80h
	jz	aag7		; if error
	cmp	dx, byte -1
	jne	aag2		; if dword
	test bh, 80h
	jz	aag2		; if dword
	cmp	bh, -1
	jne	aag3		; if word
	test bl, 80h
	jz	aag3		; if word
aag1a:
aag4.got_expr:
	retn			; it's a byte

aag2:
	inc	cx		;return: it's a dword
	inc	cx
aag3:
	inc	cx		;return: it's a word
	retn

aag4:
	cmp al, '('
	jne .not_expr
	lodsb				; skip opening paren
	call getexpression	; returns bx:dx = numerical value
	xchg bx, dx		; dx:bx = number
	call skipwh0
	cmp al, ')'
	lodsb
	je .got_expr
	dec si
	jmp aag7

.not_expr:
	xor	bx, bx		; get the basic integer
	xor	dx, dx
	call getnyb
	jc	aag7		; if not a hex digit
aag5:
	or	bl, al		; add it to the number
@@:
	lodsb
	cmp	al, '_'
	je	@B
	call getnyb
	jc	aag1a		; if done
	test dh, 0F0h
	jnz	aag7		; if overflow
	mov	cx, 4
aag6:
	shl	bx, 1		; shift it by 4
	rcl	dx, 1
	loop aag6
	jmp short aag5

aag7:
	jmp	error

		; AACONVINDEX - Convert results from AAGETI and store index value
		;
		; Entry	DX:BX,CL As in exit from AAGETI
		;	DI	Points to information record for this arg
		; Exit	SS bits stored in [di+OPRND.index]
		; Uses	DL
aaconvindex:
	cmp	cl, 1
	jne	aacv1		; if the number is too large
	cmp	bl, 1
	je	aacv2		; if 1
	inc	dx
	cmp	bl, 2
	je	aacv2		; if 2
	inc	dx
	cmp	bl, 4
	je	aacv2		; if 4
	inc	dx
	cmp	bl, 8
	je	aacv2		; if 8
aacv1:
	jmp short aag7		; error

aacv2:
	mov byte [di+OPRND.index], dl	; save the value
	retn

		; AAGETREG - Get register for the assembler.
		;
		; Entry	DI	Start of register table
		;	CX	Length of register table (or 0)
		;	SI	Address of first character in register name
		; Exit	NC if a register was found,
		;	 SI	Updated if a register was found
		;	 BX	Register number, defined as in the table below
		; Uses	AX, CX, DI
		;
		; Exit value of BX:
		;	DI = rgnam816, CX = 27	DI = rgnam16, CX = 8
		;	----------------------	--------------------
		;	0  ..  7:  AL .. BH	0  ..  7:  AX .. DI
		;	8  .. 15:  AX .. DI
		;	16 .. 23:  EAX..EDI	16 .. 23:  EAX..EDI
		;	24 .. 29:  ES .. GS
		;	30 .. 34:  ST .. TR
		; (This has to match the REG_ equs defined in uu.asm
		;  around rgnam816 and following.)
aagetreg:
	mov ax, word [si]
	and	ax, TOUPPER_W	; convert to upper case
	cmp	al, 'E'		; check for EAX, etc.
	jne	aagr1		; if not 'E' --> (NZ)
	push ax
	mov	al, ah
	mov ah, byte [si+2]
	and	ah, TOUPPER
	push di
	mov	di, rgnam16
	push cx
	mov	cx, N_REGS16
	repne scasw
	mov	bx, cx
	pop	cx
	pop	di
	pop	ax
	jne	aagr1		; if no match --> (NZ)
	inc	si
	not	bx
	add	bl, 8+16	; adjust BX
	jmp short aagr2		; finish up

aagr1:			; (entered with NZ)
	mov	bx, cx		; (if cx = 0, this is always reached with
	repne	scasw		;  ZF clear, that is, NZ)
	jne	aagr3		; if no match
	sub	bx, cx
	dec	bx
	cmp	bl, 16
	jb	aagr2		; if AL .. BH or AX .. DI
	add	bl, 8
aagr2:
	inc	si		; skip the register name
	inc	si
	clc
	retn
aagr3:
	stc			; not found
	retn

..@aa_access_end:
