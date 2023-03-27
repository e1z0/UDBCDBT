
%if 0

lDebug expression evaluator

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

;--- get a valid offset for segment in BX

		; INP:	bx = segment
		;	al = first character, si -> next character
		;	ah = 1 if might be a segment or pointer
		;	ah = 0 if must be an offset
		; OUT:	CY if pointer type return, bx:dx = pointer
		;	DebugX on a 386:
		;	 edx = offset (even if 16-bit PM/86M segment)
		;	 ah = 1 if a 32-bit segment, 0 if a 16-bit segment
		;	DebugX otherwise:
		;	 dx = offset
		;	 ah = 0
		;	Debug:
		;	 dx = offset
		;	al, si refer to next part of command line
		; CHG:	Debug: ah, DebugX: -
		; REM:	The byte [bAddr32] is no longer changed by this
		;	 function. The caller has to use the status that
		;	 is returned in ah, if desired.
getofsforbx:
	push cx			; preserve
	push bx			; preserve
	mov ch, ah		; ch = 0 if must be an offset
%if _PM
_386	xor edx, edx		; properly initialize high word
	call test_high_limit
	jz .16			; 16-bit segment -->
[cpu 386]
	call getdword
	 push bx
	 push dx
	 pop edx		; edx = 32-bit offset
	call checkpointer
	mov ah, 1		; return 32-bit offset flag
	jmp .ret_pop_NC
__CPU__
%endif

.16:
	call getexpression
	call checkpointer
	call getword.checksignificantbits
%if _PM
	mov ah, 0		; return 16-bit offset flag
%endif
.ret_pop_NC:
	pop bx
	pop cx			; restore
	clc
	retn

checkpointer:
	test ah, ah		; pointer type ?
	jns .ret		; no -->
	test ch, ch		; can be a pointer or segment ?
	jz errorj10		; no -->
	pop cx			; discard a near return address
	pop cx			; discard bx
	pop cx			; restore cx
%if _PM
_386	movzx edx, dx		; edx = dx
	mov ah, 0		; always treat as 16-bit offset
%endif
	stc			; return a pointer type
.ret:	retn


;	GETRANGE - Get address range from input line.
;    a range consists of either start and end address
;    or a start address, a 'L' and a length.
;	Entry	AL	First character of range
;		SI	Address of next character
;		BX	Default segment to use
;		CX	Default length to use (or 0 if not allowed)
;		di	Default length in lines if nonzero
;			 (only used for getrange.lines entrypoint
;			  and with word getrange_lines & 8000h set)
;			 must be <= 7FFFh
;	Exit	AL	First character beyond range
;		SI	Address of the character after that
;		BX:(E)DX	First address in range
;		BX:(E)CX	Last address in range
;	Uses	AH

%if _PM
getrangeX:
	and word [getrange_lines], 0
.lines:
	clropt [internalflags3], dif3_accept_getrange_0
_386	movzx ecx, cx
.ecx_and_0_valid:
	_386_PM_o32
	push cx
	call getaddrX
	jmp short getrange.common
%else
getrangeX: equ getrange
getrangeX.lines: equ getrange.lines
getrangeX.ecx_and_0_valid: equ getrange.ecx_and_0_valid
%endif

getrangeX_have_address_need_length:
	and word [getrange_lines], 0
	clropt [internalflags3], dif3_accept_getrange_0
	_386_PM_o32
	xor cx, cx
	_386_PM_o32
	push cx			; save the default length
	jmp getrange.common


errorj10: jmp error

getrange:
	and word [getrange_lines], 0
.lines:
	clropt [internalflags3], dif3_accept_getrange_0
_386_PM	movzx ecx, cx
.ecx_and_0_valid:
	_386_PM_o32
	push cx			; save the default length
	call getaddr		; get address into bx:(e)dx (sets bAddr32) (returns edx)
.common:
	push si
	call skipcomm0
	call iseol?
	jne gr2
	pop si			; restore si and cx
	_386_PM_o32
	pop cx
	dec si			; restore al
	lodsb

	testopt [getrange_lines], 8000h
	jz @F			; if lines length not supported -->
	test di, di		; default lines given ?
	jz @F			; no -->
	js short errorj10	; error if sign bit set -->
	mov word [getrange_lines], di
				; return lines (PL)
	mov cx, 1		; placeholder length
@@:

	testopt [internalflags3], dif3_accept_getrange_0
	jnz @F
	_386_PM_a32
	jcxz errorj10		; if a range is mandatory
@@:
	xor ah, ah
gr3.addcheck:
_386_PM	cmp byte [bAddr32], 0
_386_PM	je .16
_386_PM	dec ecx
_386_PM	add ecx, edx
_386_PM	jnc gr1			; if no wraparound
_386_PM	or ecx, byte -1		; go to end of segment
_386_PM	jmp short .checkgr3
.16:
_386_PM	cmp ecx, 0FFFFh		; was high ?
_386_PM	jbe @F			; no --> (ecxh = 0)
_386_PM	xor ecx, ecx		; ecxh = 0 and prepare so as to
				;  result in cx = FFFFh after dec
@@:
	dec cx
	add cx, dx
	jnc gr1			; if no wraparound
	mov cx, -1		; go to end of segment
.checkgr3:
	test ah, ah
	jnz short errorj10	; if specified length wrapped -->
gr1:
	retn

gr2:
_386_PM	add sp, byte 2		; for the ecxh on stack
	add sp, byte 4		; discard saved cx, si
	call uppercase
	cmp al, 'L'
	je gr3			; if a range is given
;	call skipwh0		; get next nonblank
	_386_PM_o32	; xchg ecx, edx
	xchg cx, dx
	mov ah, 0		; must be offset
	call getofsforbx_remember_bitness
				; (DebugX: returns edx no matter what)
	_386_PM_o32	; xchg ecx, edx
	xchg cx, dx
	_386_PM_o32	; cmp edx, ecx
	cmp dx, cx
	ja errorj2		; if empty range -->
	retn

gr3:
	dec si			; -> at 'L'
	push dx
	mov dx, msg.length
	call isstring?
	pop dx
	jne .notlength
	db __TEST_IMM8		; (skip inc, si -> terminator after "LENGTH")
.notlength:
	inc si			; -> behind 'L'
	call skipcomma		; discard the 'L'
;--- a range is entered with the L/ength argument
;--- get a valid length for segment in BX
	push dx
	push bx
_386_PM	call test_high_limit
_386_PM	pushf
	mov cx, word [getrange_lines]
	call get_length.lines
_386_PM	popf
_386_PM	jnz .not16_64kib	; don't check for <= 64 KiB
	cmp bx, byte 1
	jb .not16_64kib		; < 64 KiB in 16-bit segment -->
	jne short errorj2	; 16-bit segment, above 64 KiB -->
	test dx, dx
	jnz short errorj2	; 16-bit segment, above 64 KiB -->
.not16_64kib:
%if _PM
_386	push bx			; (only push high word on 386+)
	push dx
	_386_o32	; pop ecx	; mov ecx, bxdx
	pop cx			; mov cx, dx
%else
	mov cx, dx
%endif
	or bx, dx		; zero ?
	jz short errorj2	; yes, error -->
	pop bx
	pop dx
	mov ah, 1
	jmp .addcheck

%ifn _PM
errorj2:
	jmp	error
%endif


;	GETADDR - Get address from input line.
;	Entry	AL	First character of address
;		SI	Address of next character
;		BX	Default segment to use
;	Exit	AL	First character beyond address
;		SI	Address of the character after that
;		BX:(E)DX	Address found
;	Uses	AH,CX
;	REM:	mm expects that numeric expressions evaluating
;		 to numbers in the range 0 to C3h are always
;		 allowed here regardless the default segment's
;		 D/B bit and limit. This is needed for its mc
;		 command detection.

getaddr:
%if _PM
	call getaddrX
	jmp verifysegm_or_error	; make BX a writeable segment

; getaddrX differs from getaddr in that BX is not ensured
; to be writeable in PM.
;
; For DEBUG without PM support, getaddr is getaddrX. Both don't return CF.

getaddrX: section_of_function
	mov byte [bAddr32], 0
	xor cx, cx
	cmp al, '$'		; a real-mode segment?
	jne ga1_1
	lodsb
%if _DOUBLEDOLLAR
	cmp al, '$'
	jne @F
	lodsb
	dec cx			; indicate double dollar sign (0FFFFh)
@@:
%endif

_386	xor edx, edx		; edxh = 0
	call getexpression
	test ah, ah		; pointer type ?
	jns @F

	call ispm		; need to translate ?
	jnz .ret		; no -->
				; bx = segment
		; after falling through we get bx:(e)dx = segmented address

seg_bx_to_sel: equ $		; (no base for local labels)
	push ax
	mov ax, 0002h
	int 31h
	jc short errorj2
	mov bx, ax		; bx = segment
%if _DOUBLEDOLLAR
	 push dx
	push cx
	xor cx, cx
	mov dx, -1
	mov ax, 0008h
	int 31h			; set segment limit 0FFFFh
	pop cx
	jcxz .no_double_dollar
	mov dx, cx		; cx:dx = 0FFFFh
	mov ax, 0008h
	int 31h			; set segment limit 0FFFF_FFFFh
.no_double_dollar:
	 pop dx
%endif
	pop ax
.ret:
	retn

@@:
	call getword.checksignificantbits
	call ispm		; need to translate ?
	jnz .checkseg		; no -->
	mov bx, dx
	call seg_bx_to_sel
	mov dx, bx
.checkseg:
	push si
	call skipwh0
	cmp al, ':'		; was a segment at all?
	je ga2_2		; yes -->
errorj2:
	jmp error
%else
getaddrX: section_of_function
	cmp al, '$'
	jne ga1_1
	lodsb
%if _DOUBLEDOLLAR
	cmp al, '$'
	jne @F
	lodsb
@@:
%endif
	call getexpression
	test ah, ah		; pointer type ?
	js .ret			; return bx:dx = segmented address
	push ax			; (unused)
	call getword.checksignificantbits
	call skipwh0
	cmp al, ':'
	je ga2_2
	jmp error

.ret:
	retn
%endif
ga1_1:
	dec si
	mov dx, msg.t
	call isstring?
	je ga_taken
	mov dx, msg.nt
	call isstring?
	je ga_nottaken
	mov dx, msg.taken
	call isstring?
	je ga_taken
	mov dx, msg.nottaken
	call isstring?
	je ga_nottaken
	lodsb
	mov ah, 1		; might be a pointer type
	call getofsforbx
	jc .ret
	push si
	call skipwh0
	cmp al, ':'
	je ga2			; if this is a segment/selector -->
	pop si
	dec si
	lodsb
.ret:
%if _PM
	jmp short remember_bitness
				; remember 32-bitness (only if no segment)
%else
	retn
%endif

ga2:
_386_PM cmp edx, 0001_0000h	; segment/selector fits in word ?
_386_PM jae short errorj2	; no -->
ga2_2:
	pop ax			; throw away saved si
	mov bx, dx		; mov segment into BX
	call skipwhite		; skip to next word
	mov ah, 0		; must be an offset
%if _PM
getofsforbx_remember_bitness:
	call getofsforbx
remember_bitness:
	or byte [bAddr32], ah	; remember 32-bitness
	retn
%else
	jmp getofsforbx

getofsforbx_remember_bitness: equ getofsforbx
%endif


ga_nottaken:
	xor ax, ax		; 0 = not taken
	db __TEST_IMM16		; (skip mov)
ga_taken:
	mov al, 1		; 1 = taken
	push di
	_386_PM_o32	; (push esi)
	push si
	_386_PM_o32	; (push eax)
	push ax			; ! must be the LAST word in this stack frame

	mov dx, 15		; DL = number of bytes to go, DH = prefix flags
	mov bx, word [reg_cs]
	_386_PM_o32	; mov esi, dword [reg_eip]
	mov si, word [reg_eip]
.pp2:
	call pp16		; get next instruction byte into AL
	mov di, ppbytes
	mov cx, PPLEN_ONLY_PREFIXES
	repne scasb		; asize, osize, seg prefixes (ie, hints) ?
	jne @F			; no -->
	mov al,byte [di+PPLEN-1]; get corresponding byte in ppinfo
	; test al, PP_PREFIX	; prefix ?
	; jz pp3		; no -->
		; (Always set in ga_taken.)
	or dh, al		; set the OSIZE or ASIZE flags if either of these
			; Note:	Multiple OSIZE in a 16-bit cs do not toggle
			;	between decoding as O32 and O16, they're always
			;	decoded as O32. The same is true for A32, and
			;	in a 32-bit cs for O16 and A16.
.next:
	dec dl
	jnz .pp2		; if not out of bytes -->
.pp5:				; unknown, error out on the (NOT)TAKEN keyword
	_386_PM_o32	; (pop eax)
	pop ax
	_386_PM_o32	; (pop esi)
	pop si			; restore si so error will be behind keyword
	pop di
	inc si
	jmp errorj2a

@@:
	cmp al, 70h		; jcc rel8 ?
	jb @F			; no -->
	cmp al, 7Fh
	jbe .rel_8		; yes -->
@@:
	cmp al, 0E0h		; loop* / j(e)cxz rel8 ?
	jb @F			; no -->
	cmp al, 0E3h
	jbe .rel_8		; yes -->
@@:
	cmp al, 0Fh		; prefix byte ?
	jne .pp5		; no, no valid conditional branch (error) -->
	call pp16
	cmp al, 80h		; jcc rel16/rel32 ?
	jb .pp5
	cmp al, 8Fh
	ja .pp5			; no, error -->
.rel_16_32:
_386_PM	call pp_fix32bitflags
	call pp16
	xchg al, ah
	call pp16
	xchg al, ah		; ax = rel16
_386_PM	test dh, PP_OPSIZ
_386_PM	jz @F			; have 16-bit displacement -->
_386_PM	rol eax, 16		; preserve low 16 bits in high 16 bits
_386_PM	call pp16
_386_PM	ror eax, 8
_386_PM	call pp16
_386_PM	ror eax, 8		; magic swap to put high 16 bits where they belong
	jmp @FF			; have 32-bit displacement -->
		; (When _PM=0 build or no 386, this branch will do the
		;  same thing as taking it as a 16-bit displacement.)

.rel_8:
	call pp16		; get 8-bit displacement
	cbw			; 8 to 16 bits
@@:
_386_PM	cwde			; 16 to 32 bits
@@:
	pop dx			; get ax from stack
	push dx			; restore it to the stack
	test dl, dl		; 1 if ga_taken, 0 if ga_nottaken
	jz @F			; ga_nottaken, esi has eip value after inst -->
	_386_PM_o32	; (add esi, eax)
	add si, ax		; ga_taken: apply displacement to esi
@@:
	_386_PM_o32
	mov dx, si		; get into (e)dx, bx already has seg/sel
_386_PM	call test_d_b_bit
_386_PM	jnz @F			; if 32-bit cs -->
_386_PM	movzx edx, dx		; clear high 16 bits
@@:
	_386_PM_o32	; (pop eax)
	pop ax
	_386_PM_o32	; (pop esi)
	pop si
	pop di
_386_PM	mov ah, 0		; do not remember bitness (is 16 bit)
_386_PM	jz @F			; if 16-bit cs -->
_386_PM	inc ah			; remember bitness as 1 (is 32 bit)
@@:
_386_PM	call remember_bitness
	jmp skipwhite

errorj2a:
	jmp error


;	GETSTR - Get string of bytes.  Put the answer in line_out.
;		Entry	AL	first character
;			SI	address of next character
;		Exit	[line_out] first byte of string
;			DI	address of last+1 byte of string
;		Uses	AX,CL,DL,SI

getstr:
	mov di, line_out
	call iseol?
	je short errorj2a	; we don't allow empty byte strings
gs1:
	cmp al, "'"
	je gs2			; if string
	cmp al, '"'
	je gs2			; ditto
	call getbyte		; byte in DL
	mov byte [di], dl	; store the byte
	inc di
	jmp short gs6

gs2:
	mov ah, al		; save quote character
gs3:
	lodsb
	cmp al, ah
	je gs5			; if possible end of string
	call iseol?.notsemicolon
	je short errorj2a	; if end of line
gs4:
	stosb			; save character and continue
	jmp short gs3

gs5:
	lodsb
	cmp al, ah
	je gs4			; if doubled quote character
gs6:
	call skipcomm0		; go back for more
	call iseol?
	jne gs1			; if not done yet
.ret:
	retn

		; INP:	al =, si -> input
		;	cx = getrange lines flag (8000h if LINES allowed)
		; OUT:	al =, si -> after number / keyword
		;	If not LINES,
		;	 word [getrange_lines] unchanged
		;	 bx:dx = size
		;	If LINES,
		;	 word [getrange_lines] = number of lines
		;		(must be >= 1 and <= 7FFFh)
		;	 bx:dx = 1, as a placeholder value
		; CHG:	ah, cx
		; REM:	jumps to error on invalid input
get_length:
	xor cx, cx		; do not allow LINES
.lines:

	call getdword

	dec si
	 push dx

	test cx, cx		; LINES allowed ?
	jns @F			; no -->

	mov dx, msg.lines
	call isstring?		; LINES specified ?
	jne @F			; no -->

	pop dx
	test bx, bx		; lines must be <= 7FFFh
	jnz .error		; if > FFFFh -->
	test dx, dx
	jz .error		; lines mustn't be zero -->
	js .error		; if >= 8000h -->
	mov word [getrange_lines], dx
				; save lines specified
	mov dx, 1		; return a placeholder size in bx:dx
	jmp .done

.error:
	jmp error

@@:
	call get_length_keyword
				; si -> terminator after length unit
				;  (or -> after expression if no unit)
	 pop dx
	jcxz .noshift		; "BYTES" or no unit given -->
.shiftloop:
	shl dx, 1
	rcl bx, 1
	jc short .error		; too large -->
	loop .shiftloop		; loop shifting (if it was shift count 2 or 4)
.noshift:
.done:
	call skipwhite		; al = next character, si -> after that
	retn


		; INP:	si -> potential keyword
		; OUT:	NZ if no keyword,
		;	 si unchanged
		;	 cx = 0 (default length shift like BYTES)
		;	ZR if keyword found,
		;	 si -> separator behind keyword
		;	 cx = length shift
		; CHG:	dx, al
get_length_keyword:
	mov cx, 4		; shift count = 4 (do times 16)
	mov dx, msg.paragraphs
	call isstring?
	je .gotsize
	mov dx, msg.paras
	call isstring?
	je .gotsize
	dec cx			; shift count = 3 (do times 8)
	mov dx, msg.qwords
	call isstring?
	je .gotsize
	dec cx			; shift count = 2 (do times 4)
	mov dx, msg.dwords
	call isstring?
	je .gotsize
	dec cx			; shift count = 1
	mov dx, msg.words
	call isstring?
	je .gotsize
	dec cx			; shift count = 0
	mov dx, msg.bytes
	call isstring?
	; je .gotsize
.gotsize:
	retn


isbracketorunaryoperator?:
	call isunaryoperator?
	je .yes
%if _INDIRECTION
	cmp al, '['
	je .yes
%endif
	cmp al, '('
.yes:
	retn


		; Is al one of the simple unary operators?
		; OUT:	NZ if not
		;	ZR if so,
		;	 NC
		;	 cx = index into unaryoperators
isunaryoperator?:
	push di
	mov di, unaryoperators
	jmp short isoperator?.common

		; See previous description.
isoperator?:
	push di
	mov di, operators
.common:
	mov cx, word [di]
	push cx
	scasw
	repne scasb
	pop di
	jne .no
	neg cx
	add cx, di
	dec cx
	cmp al, al		; NC, ZR
.no:
	pop di
	retn


		; INP:	al = character
		; OUT:	al = capitalised character
		;	ZR, NC if a separator
		;	NZ if no separator
isseparator?:
.:
	call uppercase
	push cx
%if _EXPRESSIONS
	cmp al, 'A'
	jb @F
	cmp al, 'Z'
	jbe @FF
@@:
	call isoperator?	; normal operators are separators (also handles equality sign)
	je .yes			; if match --> (ZR, NC)
@@:
%endif
	push di
	mov di, separators
	mov cx, word [di]
	scasw
	repne scasb		; ZR, NC on match, NZ else
	pop di
.yes:
	pop cx
	retn

		; INP:	al = character
		; OUT:	al = capitalised character
		;	ZR, NC if a separator (not L or dot)
		;	NZ if no separator (including L or dot)
.except_L_or_dot:
	call uppercase
	cmp al, '.'
	je .ret_NZ
	cmp al, 'L'
	jne .
.ret_NZ:
	test al, al
	retn


		; Does one of the type operators start in input?
		;
		; INP:	al = first character
		;	si-> next character
		; OUT:	Iff NC,
		;	 bx>>1 = offset into typebitmasks and typehandlers tables
		;	 bx&1 = set iff signed type
		;	 di-> behind the type operator
		; CHG:	bx, cx, di
		;
		; Note:	Signed types are specified by an S prefix to
		;	 the type names. Only non-address types can
		;	 be signed (that is, offset, segment, and
		;	 pointer cannot be signed).
		;	Types can be specified with abbreviated names,
		;	 except where that would clash with numeric
		;	 input or a register name or ambiguity would
		;	 be caused. These cases are:
		;	SS, S (short, seg, signed, ss)
		;	B (byte, numeric 0Bh)
		;	C (char, numeric 0Ch)
		;	D (dword, numeric 0Dh)
		;	3 (3byte, numeric 3)
		;	3B (3byte, numeric 3Bh)
		;	CH (char, register ch)
istype?:
%if maxtypesize & 1
	mov cx, maxtypesize+1		; = maximum count + 1
%else
	mov cx, maxtypesize		; = maximum count
%endif
				; cx is even here!
	push dx
	push ax
	push si

	sub sp, cx			; allocate name buffer
	mov di, sp			; -> name buffer
	 push di
	xor bx, bx			; initialise count
%ifn maxtypesize & 1
	inc cx				; = maximum count + 1
%endif
				; The +1 does not represent an off-by-one
				;  because the below loop stores to the
				;  buffer at the beginning of subsequent
				;  iterations, after checking cx.

	db __TEST_IMM16			; (skip stosb and lodsb)
.storename:
	stosb				; store in name buffer
	lodsb				; get next character to check
	call uppercase
	push cx
	call isbracketorunaryoperator?	; terminator ?
	pop cx
	je .endname
	call iseol?
	je .endname
	cmp al, 32
	je .endname
	cmp al, 9
	je .endname			; yes -->
				; We don't check for digits here.
				;  Immediate values and variables
				;  must leave a space inbetween.
	inc bx				; count characters
	loop .storename			; count remaining buffer space
				; Here, the potential name was too
				;  long for a valid type name.
	stc
	jmp short .done			; -->

.endname:
	call skipwh0			; skip to next field
	dec si				; -> behind potential name
	 pop di				; -> name buffer
	mov cx, bx			; cx = length
	 push si			; save position in input line
	mov si, di			; si-> name buffer
	push bx
	push di
	clc				; indicate unsigned check
	call isunsignedtype?		; matches an unsigned type ?
	pop si
	pop cx
	jnc .done			; yes -->

	lodsb				; al = first, si-> second character
	dec cx				; cx = length less one
	cmp al, 'S'			; first character an "S" ?
	stc				; (indicate signed check, or: no type)
	jne .done			; no, not signed either -->
	call isunsignedtype?		; matches an unsigned type now ?
	inc bx				; if NC, set to indicate signed type
.done:
	lahf
	 pop di				; if NC, -> behind matched type name
	add sp, (maxtypesize+1) & ~1	; discard name buffer
	pop si
	sahf
	pop ax
	pop dx
	retn

		; Does one of the unsigned type operators start in buffer?
		;
		; INP:	si-> name buffer with capitalised potential name
		;	cx = length of potential name
		;	CY iff looking for signed type
		; OUT:	Iff NC,
		;	 bx>>1 = offset into typebitmasks and typehandlers tables
		;	 bx&1 = 0
		; CHG:	ax, bx, cx, dx, si, di
isunsignedtype?:
	mov di, types
	 sbb dx, dx			; 0FFFFh if signed check else 0
	jcxz .notype			; if zero characters -->
	cmp cx, 2
	jne @F
	cmp word [si], "CH"
	je .notype
	cmp word [si], "3B"
	je .notype
@@:
	loop .single_character_checked	; if not single character -->

	lodsb				; get that character
	cmp al, 'S'			; specified "S" or "SS" ?
	je .notype			; yes, not allowed -->
	or al, dl			; iff signed check, al |= 0FFh
	dec si				; (restore)
	cmp al, '3'
	je .notype
	cmp al, 'A'			; specified only a valid digit ?
	jb .single_character_checked
	cmp al, 'F'+1
	jb .notype			; yes, not allowed -->

.single_character_checked:
	inc cx				; (restore)
	 and dx, types.addresses-types.end	; = 0 iff unsigned check
	xor ax, ax			; initialise ah, and ax = 0 first
	xor bx, bx
	xchg di, si
	 add dx, types.end		; = .addresses for signed check,
	 				;  = .end for unsigned check

		; Before each iteration,
		;  si-> byte-counted next name to check
		;  di-> potential name (in name buffer)
		;  cx = cl = length of potential name
		;  (dx-1) = maximum value for si
		;  ah = 0
		; Before the first iteration additionally,
		;  bx&~3 = index to return for this name (if match)
		;  al = 0
		; Before subsequent iterations additionally,
		;  (bx+2)&~3 = index to return for this name (if match)
		;  al = offset to add to si first
	db __TEST_IMM16			; (skip two times inc bx)
.loop:
	inc bx
	inc bx				; increase index
	add si, ax			; -> next table entry
	lodsb				; ax = length of full name
	cmp si, dx			; checked all allowed names?
	jae .notype			; yes, done -->
	cmp ax, cx			; full name large enough ?
	jb .loop			; no -->
	push di
	push cx
	push si
	 repe cmpsb			; potential name matches ?
	pop si
	pop cx
	pop di
	jne .loop			; no -->

	and bl, ~3			; conflate alternative type names
	db __TEST_IMM8			; (NC, skip stc)
.notype:
	stc
	retn


	usesection lDEBUG_DATA_ENTRY

		; Table of bit masks and shift counts to determine
		;  how a type modifies the bit mask of required bytes.
		;
		; It would be possible to always retrieve a full dword
		;  from memory to process indirection in expressions,
		;  but this could fault if accessing inexistent data.
		;  Hence the debugger should minimise memory access.
		; For this reason, types allow the expression evaluator
		;  to keep track which of the term's bytes are actually
		;  going to be used. The bit mask of required bytes
		;  indicates which bytes are not discarded by any of a
		;  term's type operators.
		;
		; The second byte of each entry (applied to ch by the
		;  reader, ie high byte of cx) indicates a mask to
		;  apply to the bit mask of required bytes. Note that
		;  this mask is applied first, before the shift that's
		;  described next.
		; The first byte of each entry (loaded into cl by the
		;  reader, ie low byte of cx) indicates a shift left
		;  count to apply to the bit mask of required bytes.
		;  (Only the segment type doesn't have 0 currently.)
		;
		; Note that types are parsed forwards through the input
		;  (ie the specified command) but are actually applied
		;  to the numeric value they refer to backwards, that
		;  is, a type that is closer to the term in the input
		;  is applied to the term's result before a type that's
		;  farther from the term.
		; Misleadingly, this reversal isn't very apparent in
		;  most processing of the type and unary operators.
		; The segment type's shifting and masking reflects the
		;  reversal: while the actual operation is to shift
		;  right then restrict to the low word, the entry in
		;  this table indicates to restrict the bit mask to
		;  the low word then shift left.
	align 2, db 0
typebitmasks:
	db 0,    1b	; byte
	db 0,   11b	; word
	db 0,  111b	; 3byte
	db 0, 1111b	; dword
	db 0, 1111b	; pointer
	db 0,   11b	; offset
	db 2,   11b	; segment


		; Dispatch table for type conversion functions.
		;
		; INP:	bx:dx = dword input
		;	CF = signedness of type conversion
		;	ah from lahf with the same CF as current
		;	al = type (80h = pointer, 40h = signed)
		; OUT:	bx:dx = new value
		;	ah = type (80h = pointer, 40h = signed)
		; CHG:	ax
	align 2, db 0
typehandlers:
	dw handlebyte
	dw handleword
	dw handle3byte
	dw handledword
	dw handlepointer
	dw handleoffset
	dw handlesegment


	usesection lDEBUG_CODE

handlesegment:
	mov dx, bx
	xor ah, ah		; NC, ah = 0
	jmp short handleword
handleoffset equ handleword

handle3byte:
	mov bh, 0
	jnc .zero		; (iff unsigned type -->)
	test bl, bl		; signed ?
	jns .zero_f		; no -->
	dec bh
.zero_f:
	sahf			; restore CF
.zero:
	jmp handledword

handlebyte:
	mov dh, 0
	jnc .zero		; (iff unsigned type -->)
	test dl, dl		; signed ?
	jns .zero_f		; no -->
	dec dh
.zero_f:
	sahf			; restore CF
.zero:
handleword:
	mov bx, 0
	jnc .zero		; (iff unsigned type -->)
	test dx, dx		; signed ?
	jns .zero_f		; no -->
	dec bx
.zero_f:
	sahf			; restore CF
.zero:
handledword:
	jc .signed		; if signed -->
	xor ah, ah		; return pointer=0 signed=0
	retn

.signed:
	mov ah, 40h		; return pointer=0 signed=1
	retn

handlepointer:
	mov ah, 80h		; return pointer=1 signed=0
	retn


	usesection lDEBUG_DATA_ENTRY

		; List of binary and unary operators.
		; The order has to match that in the respective
		; operator handler dispatch table below.
	align 2, db 0
operators:
.:		countedw "+-*/%<>=!|&^oOaAxXcC?"
.amount: equ $ - . - 2
	align 2, db 0
unaryoperators:
.:		countedw "+-~!?"
.amount: equ $ - . - 2


		; Dispatch table for unary operators,
		;  used by getexpression.
		; Functions in this table are called with:
		;
		; INP:	bx:dx = number
		;	ah = type
		; OUT:	bx:dx = result
		;	ah = type
		; CHG:	-
	align 2, db 0
unaryoperatorhandlers:
.:
	dw uoh_plus		; +
	dw uoh_minus		; -
	dw uoh_not_bitwise	; ~
	dw uoh_not_boolean	; !
	dw uoh_abs		; ?
.amount: equ ($ - .) / 2
%if .amount != unaryoperators.amount
 %error String and table mismatch
%endif


	usesection lDEBUG_CODE

uoh_abs:
	and ah, ~ 40h		; make type signed=0
	test bh, 80h		; negative ?
	jz uoh_plus		; no -->
	jmp calculate_minus_bxdx
uoh_minus:
	or ah, 40h		; make type signed=1
calculate_minus_bxdx:
	neg bx
	neg dx
	sbb bx, byte 0		; neg bx:dx
uoh_plus:			; (nop)
	retn

uoh_not_bitwise:
	mov ah, 0		; make type pointer=0 signed=0
	not bx
	not dx
	retn

uoh_not_boolean:
	mov ah, 0		; make type pointer=0 signed=0
	call toboolean
	xor dl, 1		; toggle only bit 0
	retn


	usesection lDEBUG_DATA_ENTRY

		; Word table operatordispatchers: order as in string operators
		; Pointed functions dispatch depending on operator characters
		; Return: operator index, 0 = invalid

		; Operator index (byte):
		; 0 = invalid, no operator found
		; 1.. = 1-based index in byte table operatorprecedences
		;     = 1-based index in word table operatorfunctions
		;     = 1-based index in word table operatornames

		; Dispatch table for (binary) expression operators,
		;  used by getexpression.
		; Functions in this table are called with:
		; INP:	al = operator character (which is also implicit)
		;	si-> remaining line (directly) behind operator character
		; OUT:	bl != 0 if a valid operator,
		;	 bl = operator index
		;	 si-> behind the last character belonging to the operator
		;	bl = 0 if no valid operator
		; CHG:	al, bh, dx

	struc opprecs
OPPREC_INVALID:	resb 1
OPPREC_COND:	resb 1
OPPREC_BOOL_OR:	resb 1
OPPREC_BOOL_XOR:resb 1
OPPREC_BOOL_AND:resb 1
OPPREC_COMPARE:	resb 1
OPPREC_BIT_OR:	resb 1
OPPREC_BIT_XOR:	resb 1
OPPREC_BIT_AND:	resb 1
OPPREC_BIT_CLR:	resb 1
OPPREC_SHIFT:	resb 1
OPPREC_ADDSUB:	resb 1
OPPREC_MULDIV:	resb 1
OPPREC_POWER:	resb 1
OPPREC_RIGHTOP:		; (to process it first in getexpression)
	endstruc
		; The number of precedence levels indicates how many
		; intermediate results getexpression might have to save
		; on its stack. With thirteen levels of precedence, up to
		; twelve intermediate results are pushed by getexpression.
		; (With 6 bytes each, that gives a moderate 72 bytes.)
		; Key to this is that, in case of a low enough operator
		; behind the one that triggered the pushing, the pushed
		; value will be popped before proceeding. This way more
		; intermediate results may be pushed later but the stack
		; never holds intermediate results that don't need to be
		; on the stack.

		; This is the definition of operator index values. The tables
		; operatorprecedences and operatorfunctions are ordered by this.
		; The operator dispatchers return one of these.
	struc ops
OPERATOR_INVALID:		resb 1	; 0 - invalid
OPERATOR_PLUS:			resb 1	; +
OPERATOR_MINUS:			resb 1	; -
OPERATOR_MULTIPLY:		resb 1	; *
OPERATOR_DIVIDE:		resb 1	; /
OPERATOR_MODULO:		resb 1	; %
OPERATOR_POWER:			resb 1	; **
OPERATOR_CMP_BELOW:		resb 1	; <
OPERATOR_CMP_BELOW_EQUAL:	resb 1	; <=
OPERATOR_CMP_ABOVE:		resb 1	; >
OPERATOR_CMP_ABOVE_EQUAL:	resb 1	; >=
OPERATOR_CMP_EQUAL:		resb 1	; ==
OPERATOR_CMP_NOT_EQUAL:		resb 1	; !=
OPERATOR_SHIFT_LEFT:		resb 1	; <<
OPERATOR_SHIFT_RIGHT:		resb 1	; >>
OPERATOR_SHIFT_RIGHT_SIGNED:	resb 1	; >>>
OPERATOR_BIT_MIRROR:		resb 1	; ><
OPERATOR_BIT_CLR:		resb 1	; clr (bitwise AND with bitwise NOT)
OPERATOR_BIT_OR:		resb 1	; |
OPERATOR_BIT_XOR:		resb 1	; ^
OPERATOR_BIT_AND:		resb 1	; &
OPERATOR_BOOL_OR:		resb 1	; ||
OPERATOR_BOOL_XOR:		resb 1	; ^^
OPERATOR_BOOL_AND:		resb 1	; &&
OPERATOR_COND:			resb 1	; ?? :: construct (special)
OPERATOR_RIGHTOP:		resb 1	; (dummy right-operand operator)
	endstruc
		; Order of BIT_* needs to be the same as that of BOOL_*.
		; BOOL_* have to follow directly behind BIT_*.
		; "R CF op= expr" depends on that (rr4 in rr.asm).

	align 2, db 0
operatordispatchers:
.:
	dw od_plus		; +
	dw od_minus		; -
	dw od_multiply		; * **
	dw od_divide		; /
	dw od_modulo		; %
	dw od_below		; < <> <= <<
	dw od_above		; > >< >= >> >>>
	dw od_equal		; == =< =>
	dw od_not		; !=
	dw od_or		; | ||
	dw od_and		; & &&
	dw od_xor		; ^ ^^
	dw od_o			; o
	dw od_o			; O
	dw od_a			; a
	dw od_a			; A
	dw od_x			; x
	dw od_x			; X
	dw od_c			; c
	dw od_c			; C
	dw od_cond		; ?
.end: equ $ - .
.amount: equ ($ - .) / 2
%if .amount != operators.amount
 %error String and table mismatch
%endif
%if .amount > 100h
 %error Too many operator dispatchers
%endif


operatorprecedences:
.:
	db OPPREC_INVALID		; need this for some checks
	db OPPREC_ADDSUB		; +
	db OPPREC_ADDSUB		; -
	db OPPREC_MULDIV		; *
	db OPPREC_MULDIV		; /
	db OPPREC_MULDIV		; %
	db OPPREC_POWER			; **
	db OPPREC_COMPARE		; <
	db OPPREC_COMPARE		; <=
	db OPPREC_COMPARE		; >
	db OPPREC_COMPARE		; >=
	db OPPREC_COMPARE		; ==
	db OPPREC_COMPARE		; !=
	db OPPREC_SHIFT			; <<
	db OPPREC_SHIFT			; >>
	db OPPREC_SHIFT			; >>>
	db OPPREC_SHIFT			; ><
	db OPPREC_BIT_CLR		; clr
	db OPPREC_BIT_OR		; |
	db OPPREC_BIT_XOR		; ^
	db OPPREC_BIT_AND		; &
	db OPPREC_BOOL_OR		; ||
	db OPPREC_BOOL_XOR		; ^^
	db OPPREC_BOOL_AND		; &&
	db OPPREC_COND			; ?? :: construct
	db OPPREC_RIGHTOP		; getexpression: no number yet
.amount: equ $ - .
%if .amount != ops_size
 %error Operators table size mismatch
%endif


	align 2, db 0
operatorfunctions:
.:
	dw error			; should not be called
	dw of_plus			; +
	dw of_minus			; -
	dw of_multiply			; *
	dw of_divide			; /
	dw of_modulo			; %
	dw of_power			; **
	dw of_compare_below		; <
	dw of_compare_below_equal	; <=
	dw of_compare_above		; >
	dw of_compare_above_equal	; >=
	dw of_compare_equal		; ==
	dw of_compare_not_equal		; !=
	dw of_shift_left		; <<
	dw of_shift_right		; >>
	dw of_shift_right_signed	; >>>
	dw of_bit_mirror		; ><
	dw of_clr_bitwise		; clr
	dw of_or_bitwise		; |
	dw of_xor_bitwise		; ^
	dw of_and_bitwise		; &
	dw of_or_boolean		; ||
	dw of_xor_boolean		; ^^
	dw of_and_boolean		; &&
	dw of_cond			; ?? :: construct
	dw of_rightop			; set to right operand
.amount: equ ($ - .) / 2
%if .amount != ops_size
 %error Operators table size mismatch
%endif


%if _EXPRESSION_DEBUG

%define OPNAMES db ""
%macro opname 1.nolist
%defstr %%string %1
%substr %%prefix %%string 1,3
%ifidni %%prefix, "of_"
 %substr %%string %%string 4,-1
%endif
%xdefine OPNAMES OPNAMES,%%name:,{asciz %%string}
	dw %%name
%endmacro

%macro opnamesstrings 1-*
 %rep %0
%1
  %rotate 1
 %endrep
%endmacro

	align 2, db 0
operatornames:
.:
	opname invalid			; invalid
	opname of_plus			; +
	opname of_minus			; -
	opname of_multiply		; *
	opname of_divide		; /
	opname of_modulo		; %
	opname of_power			; **
	opname of_compare_below		; <
	opname of_compare_below_equal	; <=
	opname of_compare_above		; >
	opname of_compare_above_equal	; >=
	opname of_compare_equal		; ==
	opname of_compare_not_equal		; !=
	opname of_shift_left		; <<
	opname of_shift_right		; >>
	opname of_shift_right_signed	; >>>
	opname of_bit_mirror		; ><
	opname of_clr_bitwise		; clr
	opname of_or_bitwise		; |
	opname of_xor_bitwise		; ^
	opname of_and_bitwise		; &
	opname of_or_boolean		; ||
	opname of_xor_boolean		; ^^
	opname of_and_boolean		; &&
	opname of_cond			; ?? :: construct
	opname right			; set to right operand
.amount: equ ($ - .) / 2
%if .amount != ops_size
 %error Operators table size mismatch
%endif

	opnamesstrings OPNAMES
%endif

	usesection lDEBUG_CODE

od_minus:
	mov bl, OPERATOR_MINUS
	retn

od_plus:
	mov bl, OPERATOR_PLUS
	retn

od_multiply:
	mov bl, OPERATOR_MULTIPLY
	cmp byte [si], al
	jne .ret
	inc si
	mov bl, OPERATOR_POWER
.ret:
	retn

od_divide:
	mov bl, OPERATOR_DIVIDE
	retn

od_modulo:
	mov bl, OPERATOR_MODULO
	retn

od_above:
	cmp byte [si], al
	je .shr
	cmp byte [si], '<'
	je .mirror
	cmp byte [si], '='
	je .cmp_ae
	mov bl, OPERATOR_CMP_ABOVE
	retn
.cmp_ae:
	inc si
	mov bl, OPERATOR_CMP_ABOVE_EQUAL
	retn
.shr:
	inc si
	cmp byte [si], al
	je .sar
	mov bl, OPERATOR_SHIFT_RIGHT
	retn
.sar:
	inc si
	mov bl, OPERATOR_SHIFT_RIGHT_SIGNED
	retn
.mirror:
	inc si
	mov bl, OPERATOR_BIT_MIRROR
	retn

od_below:
	cmp byte [si], al
	je .shl
	cmp byte [si], '>'
	je .ncmp
	cmp byte [si], '='
	je .cmp_be
	mov bl, OPERATOR_CMP_BELOW
	retn
.cmp_be:
	inc si
	mov bl, OPERATOR_CMP_BELOW_EQUAL
	retn
.shl:
	inc si
	mov bl, OPERATOR_SHIFT_LEFT
	retn
.ncmp:
od_not.ncmp:
	inc si
	mov bl, OPERATOR_CMP_NOT_EQUAL
	retn

od_equal:
	cmp byte [si], '>'
	je od_above.cmp_ae
	cmp byte [si], '<'
	je od_below.cmp_be
	cmp byte [si], al
	jne .invalid		; no valid operator -->
.cmp:
	inc si
	mov bl, OPERATOR_CMP_EQUAL
	retn

od_not:
	cmp byte [si], '='
	je .ncmp
od_equal.invalid:
	mov bl, OPERATOR_INVALID
	retn

od_or:
	cmp byte [si], al
	je .boolean
	mov bl, OPERATOR_BIT_OR
	retn
.boolean:
	inc si
	mov bl, OPERATOR_BOOL_OR
	retn
od_and:
	cmp byte [si], al
	je .boolean
	mov bl, OPERATOR_BIT_AND
	retn
.boolean:
	inc si
	mov bl, OPERATOR_BOOL_AND
	retn

od_xor:
	cmp byte [si], al
	je .boolean
	mov bl, OPERATOR_BIT_XOR
	retn
.boolean:
	inc si
	mov bl, OPERATOR_BOOL_XOR
	retn


od_cond:
	mov bl, OPERATOR_COND
	mov dx, msg.questionmark
	jmp od_string_common

od_o:
	mov bl, OPERATOR_BIT_OR
	mov dx, msg.r
	jmp od_string_common

od_a:
	mov bl, OPERATOR_BIT_AND
	mov dx, msg.nd
	jmp od_string_common

od_x:
	mov bl, OPERATOR_BIT_XOR
	mov dx, msg.or
	jmp od_string_common

od_c:
	mov bl, OPERATOR_BIT_CLR
	mov dx, msg.lr
	; jmp od_string_common

od_string_common:
	call isstring?
	je .ret
.none:
	xor bx, bx
.ret:
	retn


		; (Binary) Expression operator functions,
		;  used by getexpression.
		; These functions are called with:
		; INP:	dword [hhvar] = previous number (left-hand operand)
		;	byte [hhtype] = previous type
		;	bx:dx = following number (right-hand operand)
		;	ah = following type
		; OUT:	bx:dx = result
		;	ah = type
		; CHG:	ax, cx
		; Note:	Errors (divisor zero) are currently simply handled
		;	 by jumping to "error".
		;	getexpression mustn't be called after until hhvar is
		;	 no longer used, as the call might overwrite hhvar.
		;
		; Type info (in ah and b[hhtype]) appears to be correctly
		; passed to here already. However, how should that be used?
		;
		; Quite simply, doing any arithmetic on two unsigned numbers
		; could return the smallest possible unsigned type (so that
		; if the result is <= 255 then the type is 1, if <= 65536
		; then the type is 2, else the type is 4).
		;
		; Handling two signed numbers here might be equally simple:
		; if the result is >= -128 && <= 127 then the type is 81h,
		; if the result is >= -32768 && <= 32767 then the type is
		; 82h, otherwise the type is 84h. Have to look into this.
		; (How does this interact with the unsignedness of the
		; actual computations?)
		;
		; It gets hairy when one operand is signed and the other
		; isn't; generally, two sub-cases of this exist: first, the
		; signed operand is positive, second, the signed operand is
		; negative. (Whether this distinction actually makes sense
		; for the implementation is still to be determined.)
		; Possible models:
		; * Result is always signed.
		; * Result is always unsigned(?!).
		; * Result is treated as signed, but if it's positive its
		;   type is changed to unsigned.
		;
		; It is also possible that operators could be handled
		; differently, for example, (some) bit and boolean
		; operators could imply unsigned operands in any case.
		; (>>> obviously implies a signed left operand already.)
		;
		; Note that (signed) negative bit shifting counts could
		; imply reversing the operation; << becomes >> and such.
		;
		; Note that for the addition of, for example, bit rotation,
		; it would be useful to retain the originally used type
		; inside getdword. At the end of getdword, the current bit
		; counting could be changed to use the "signed" bit of the
		; types and then determine which signed or unsigned type is
		; large enough to hold the value. (It might already work
		; mostly like that.) (Is this specifically about binary
		; operators? - No. In fact, binary operators are the most
		; likely to be (one of) the syntactic element(s) which
		; should change (and possible 'optimize') types. - Then
		; this might still apply to unary operators, and brackets
		; and parentheses. In particular, the later should call
		; a different entry or instruct getdword not to optimize
		; the type at the end so as to retain it. - Although in
		; cases where that matters, the parentheses are arguably
		; unnecessary, aren't they?)
		;
		; It has to be decided whether there should be implicit
		; dispatching based on the operands' types' signedness.
		; For example, currently (with all operands being implied
		; to be unsigned) there exist >> and >>>, and there could
		; exist > and S>. With implicit signedness dispatching, the
		; behaviour of >> would change: it would expose the current
		; >>'s behaviour with an unsigned (left) operand, and the
		; current >>>'s behaviour with a signed (left) operand.
		; (Either U(nsigned)>> and S(igned)>> operators could then
		; exist, which would imply an unsigned or signed left
		; operand respectively, or the affected operands' signedness
		; could be changed with the currently available prefix or
		; possible new postfix unary operators.
of_modulo:
	push word [hhvar+2]
	push word [hhvar]
	push bx
	push dx
	call of_divide		; bx:dx := prev / foll
	pop word [hhvar]
	pop word [hhvar+2]
	call of_multiply	; bx:dx := (prev / foll) * foll
	pop word [hhvar]
	pop word [hhvar+2]
;	jmp short of_minus	; bx:dx := prev - ((prev / foll) * foll)

of_minus:
	call calculate_minus_bxdx
of_plus:
	add dx, word [hhvar]
	adc bx, word [hhvar+2]
or_hhtype:
	or ah, byte [hhtype]
	retn

of_multiply:			; bx:dx := var * bx:dx
	or byte [hhtype], ah
	push si
	push di			; si:di is used as temporary storage
	mov ax, dx
	push ax
	mul word [hhvar]
	mov di, ax
	mov si, dx
	pop ax
	mul word [hhvar+2]
	add si, ax
	mov ax, bx
	mul word [hhvar]
	add si, ax
	; bx*[hhvar+2] not required, completely overflows
	mov dx, di
	mov bx, si
	pop di
	pop si			; restore those
set_hhtype:
	mov ah, byte [hhtype]
	retn

of_divide:			; bx:dx := var / bx:dx
	or byte [hhtype], ah
	 push bx
	or bx, dx		; divisor zero ?
	 pop bx
	jz short .error		; divisor zero !
	_386_jmps .32		; 386, use 32-bit code -->
	test bx, bx		; need only 16-bit divisor ?
	jnz .difficultdiv16	; nope -->
	mov cx, dx
	xor dx, dx
	mov ax, word [hhvar+2]	; dx:ax = high word of previous number
	div cx
	mov bx, ax
	mov ax, word [hhvar]	; ax = low word of previous number, dx = remainder
	div cx
	mov dx, ax		; bx:dx = result
	jmp set_hhtype

.difficultdiv16:		; code adapted from Art of Assembly chapter 9
				; refer to http://www.plantation-productions.com/Webster/www.artofasm.com/DOS/ch09/CH09-4.html#HEADING4-99
	mov cx, 32
	push bp
	push si
	push di
	mov ax, word [hhvar]
	mov bp, word [hhvar+2]	; bp:ax = previous number
	xor di, di
	xor si, si		; clear variable si:di
	xchg ax, dx
	xchg bp, bx		; bx:dx = previous number, bp:ax = divisor
.bitloop:
	shl dx, 1
	rcl bx, 1
	rcl di, 1
	rcl si, 1		; si:di:bx:dx << 1
	cmp si, bp		; does the divisor fit into si:di here ?
	jne @F
	cmp di, ax
@@:
	jb .trynext		; no -->
.goesinto:
	sub di, ax
	sbb si, bp		; subtract divisor
	inc dx			; set a bit of the result (bit was zero before, never carries)
.trynext:
	loop .bitloop		; loop for 32 bits
	pop di
	pop si
	pop bp
	jmp set_hhtype

.error:
	jmp error

%ifn _ONLYNON386
.32:
[cpu 386]
	push eax
	push ebx
	push edx		; to preserve the high words
	 push bx
	 push dx
	 pop ebx		; ebx = following number
	xor edx, edx
	mov eax, dword [hhvar]	; edx:eax = previous number
	div ebx
	pop edx
	pop ebx			; restore high words
	 push eax
	 pop dx
	 pop bx			; bx:dx = result
	pop eax			; restore high word of eax
__CPU__
	jmp set_hhtype
%endif


of_power:
	or byte [hhtype], ah
	mov cx, bx
	mov ax, dx		; get exponent
	or bx, dx		; exponent zero ?
	mov bx, 0
	mov dx, 1
	jz .ret			; yes, return with result as 1 -->
	cmp word [hhvar], dx	; optimization:
	jne .notone
	cmp word [hhvar+2], bx
	je .ret			; if base is one (and exponent not zero), result is 1 -->
.notone:
	push bp

.loop:				; cx:ax non-zero here
	shr cx, 1
	rcr ax, 1		; exponent /= 2
	push ax
	push cx
	jnc .even		; if exponent was even -->
	call of_multiply	; var *= base
.even:
		; In the last iteration, cx:ax might be zero here
		; making the next call unnecessary. Oh well.
	push bx
	push dx
	mov bx, word [hhvar+2]
	mov dx, word [hhvar]	; base
	call of_multiply	;  * base = base squared
	mov word [hhvar+2], bx
	mov word [hhvar], dx	; store as new base
	pop dx
	pop bx
	pop cx
	pop ax

	mov bp, bx		; optimization:
	or bp, dx		;  register now zero ?
	jz .ret_bp		; if so, return now --> (multiplying zero always results in zero)
	mov bp, cx
	or bp, ax		; exponent now zero ?
	jnz .loop		; no, loop -->

.ret_bp:
	pop bp
.ret:
	jmp set_hhtype

of_compare_below_equal:
	call of_helper_compare
	jbe of_helper_compare_true
	retn

of_compare_below:
	call of_helper_compare
	jb of_helper_compare_true
	retn

of_compare_not_equal:
	call of_helper_compare
	jne of_helper_compare_true
	retn

of_compare_equal:
	call of_helper_compare
	je of_helper_compare_true
	retn

of_compare_above_equal:
	call of_helper_compare
	jae of_helper_compare_true
	retn

of_compare_above:
	call of_helper_compare
	ja of_helper_compare_true
	retn


		; Called by operator functions to convert a number to boolean
		;
		; INP:	bx:dx
		; OUT:	dx = 0 or 1
		;	bx = 0
		;	ZF
toboolean:
	or bx, dx		; = 0 iff it was 0000_0000h
	cmp bx, byte 1		; CY iff it was 0000_0000h, else NC
	sbb dx, dx		; -1 iff it was 0000_0000h, else 0
	xor bx, bx

		; INP:	dx
		; OUT:	dx += 1
of_helper_compare_true:
	inc dx			; bx:dx = 0 iff it was 0000_0000h, else 1

		; Dummy operator computation function,
		;  used when setting a register without operator (rr)
		;  and to initialize the first getexpression loop iteration
		; INP:	dword [hhvar] = previous number (left-hand operand)
		;	byte [hhtype] = previous type
		;	bx:dx = following number (right-hand operand)
		;	ah = following type
		; OUT:	bx:dx = result (right-hand operand)
		;	ah = type
		; CHG:	ax, cx
of_rightop:
	retn


		; Called by operator functions to compare operands
		;
		; INP:	dword [hhvar], byte [hhtype]
		;	bx:dx, ah
		; OUT:	Flags as for "cmp d[hhvar], bxdx"
		;	bx:dx = 0
		;	ah = 0
of_helper_compare:
	xor ah, ah		; type signed=0 pointer=0
	cmp word [hhvar+2], bx
	jne .ret
	cmp word [hhvar], dx
.ret:
	mov bx, 0
	mov dx, bx		; set both to zero (without affecting flags)
	retn

of_shift_right:
	xor ah, ah
	call of_helper_getshiftdata
.loop:
	shr bx, 1
	rcr dx, 1
	loop .loop
	retn

of_shift_right_signed:
	mov ah, 40h
	call of_helper_getshiftdata
.loop:
	sar bx, 1
	rcr dx, 1
	loop .loop
	retn

of_shift_left:
	xor ah, ah
	call of_helper_getshiftdata
.loop:
	shl dx, 1
	rcl bx, 1
	loop .loop
	retn

		; Called by operator functions to get shift data
		;
		; This returns to the next caller with the unchanged input
		; operand if the shift count is zero. Otherwise, large shift
		; counts are normalized so the returned value in cx is not
		; zero and not higher than 32. This normalization is not just
		; an optimization, it's required for shift counts that don't
		; fit into a 16-bit counter.
		;
		; INP:	bx:dx = shift count
		; OUT:	bx:dx = input operand
		;	If shift count is >= 1 and <= 32,
		;	 cx = shift count
		;	If shift count is > 32,
		;	 cx = 32
		;	If shift count is zero,
		;	 discards one near return address before returning
		; CHG:	cx
of_helper_getshiftdata:
	mov cx, dx
	test bx, bx
	jnz .largeshift
	cmp dx, byte 32
	jb .normalshift
.largeshift:
	mov cx, 32		; fix at maximal shift count
.normalshift:
	mov dx, word [hhvar]
	mov bx, word [hhvar+2]
	jcxz .break		; shift count zero, return input -->
	retn

.break:
	pop cx			; discard near return address
	retn

of_bit_mirror:
	xor ah, ah
	mov cx, dx
	test bx, bx
	jnz .large
	cmp dx, byte 64
	jb .normal
.large:
	xor bx, bx		; mirror count 64 or higher:
	xor dx, dx		;  all 32 bits mirrored with (nonexistent) zero bits
	retn
.normal:
	mov dx, word [hhvar]
	mov bx, word [hhvar+2]
	cmp cl, 1
	jbe .ret		; mirror count one or zero, return input -->
	push si
	push di

	push cx
	mov di, -1
	mov si, di
.loopmask:
	shl di, 1
	rcl si, 1
	loop .loopmask		; create mask of bits not involved in mirroring
	and si, bx
	and di, dx		; get the uninvolved bits
	pop cx

	push si
	push di			; save them
	xor si, si
	xor di, di		; initialize mirrored register
.loop:
	shr bx, 1
	rcr dx, 1		; shift out of original register's current LSB
	rcl di, 1
	rcl si, 1		;  into other register's current LSB
	loop .loop
	pop dx
	pop bx			; restore uninvolved bits
	or bx, si
	or dx, di		; combine with mirrored bits

	pop di
	pop si
.ret:
	retn

of_or_bitwise:
	or dx, word [hhvar]
	or bx, word [hhvar+2]	; bitwise or
or_hhtype_1:
	jmp or_hhtype

of_or_boolean:
	call of_helper_getbool
	or dx, bx		; boolean or
	jmp short of_helper_retbool

of_clr_bitwise:
	not bx
	not dx

of_and_bitwise:
	and dx, word [hhvar]
	and bx, word [hhvar+2]	; bitwise and
	jmp or_hhtype_1

of_and_boolean:
	call of_helper_getbool
	and dx, bx		; boolean and
	jmp short of_helper_retbool

of_xor_bitwise:
	xor dx, word [hhvar]
	xor bx, word [hhvar+2]	; bitwise xor
	jmp or_hhtype_1

of_xor_boolean:
	call of_helper_getbool
	xor dx, bx		; boolean xor
of_helper_retbool:
	xor bx, bx		; high word always zero
	retn

		; Called by operator functions to convert operands to boolean
		;
		; INP:	bx:dx = next number, ah = type
		;	[hhvar] = previous number, [hhtype] = type
		; OUT:	bx = next number's boolean value
		;	dx = previous number's boolean value
		;	ah = 0
of_helper_getbool:
	xor ah, ah
	call toboolean
	push dx
	mov dx, word [hhvar]
	mov bx, word [hhvar+2]
	call toboolean
	pop bx
	retn


	usesection lDEBUG_DATA_ENTRY

isvariable_data:
reverselfsrtop:	db 1Fh

%if _DEBUG3
	align 8, db 0
kregs:	dd 0
	dd 1
	dd 0aa55aa55h
	dd -1
	times 12 dd 0
%endif

	align 4, db 0
lfsr:		dd 2
reverselfsr:	dd 1
lfsrtap:	dd 8020_0003h
%if _SLEEP_NEW
getc_repeat_idle:	dw 0
sleep_repeat_idle:	dw 0
sleep_highest_delta:	dw 0
sleep_delta_limit:	dw 5
%endif

	align 8, db 0
isvariable_struc_list:

%assign IVS_ONEBYTE 1
isvariable_struc_onebyte_list:
	; name, size, flags, address, array last index, array skip
isvariablestruc "V", 4, 0, vregs, 255
%if _DEBUG3
isvariablestruc "K", 4, 0, kregs, 15
%endif
isvariable_struc_onebyte_list_end:

%assign IVS_ONEBYTE 0
isvariable_struc_morebyte_list:
isvariablestruc "INT8CTRL", 2, 0, intr8_limit
isvariablestruc "LFSR", 4, ivfSpecialSetUp, var_lfsr_setup
isvariablestruc "RLFSR", 4, ivfSpecialSetUp, var_reverselfsr_setup
isvariablestruc "LFSRTAP", 4, 0, lfsrtap
isvariablestruc "RLFSRTOP", 1, 0, reverselfsrtop
%if _SLEEP_NEW
isvariablestruc "GREPIDLE", 1, 0, getc_repeat_idle
isvariablestruc "SREPIDLE", 1, 0, sleep_repeat_idle
isvariablestruc "SMAXDELTA", 2, 0, sleep_highest_delta
isvariablestruc "SDELTALIMIT", 2, 0, sleep_delta_limit
%endif
isvariablestruc "DEVICESIZE", 2, ivfReadOnly, device_mcb_paragraphs
isvariablestruc "DEVICEHEADER", 4, ivfReadOnly, device_header_address
isvariablestruc "MACHX86", 1, ivfReadOnly, machine
isvariablestruc "MACHX87", 1, ivfReadOnly, encodedmach87
isvariablestruc "MMT", 1, 0, maxmachinetype
isvariablestruc "DCO", 4, \
	ivfArrayOneBased | ivfArrayOptional, options, 6
isvariablestruc "DCS", 4, \
	ivfReadOnly | ivfArrayOneBased | ivfArrayOptional, startoptions, 6
isvariablestruc "DIF", 4, \
	ivfReadOnly | ivfArrayOneBased | ivfArrayOptional, internalflags, 6
isvariablestruc "DAO", 4, \
	0, asm_options
isvariablestruc "DAS", 4, \
	ivfReadOnly, asm_startoptions
isvariablestruc "DPI", 4, \
	ivfReadOnly, psp22
isvariablestruc "DPR", 2, \
	ivfReadOnly, pspdbg
isvariablestruc "DPP", 2, \
	ivfReadOnly, parent
isvariablestruc "DPRA", 4, \
	ivfReadOnly, psp22
isvariablestruc "DPSP", 2, \
	ivfReadOnly, pspdbg
isvariablestruc "DPARENT", 2, \
	ivfReadOnly, parent
%if _PM
isvariablestruc "DPS", 2, \
	ivfReadOnly | ivfSpecialSetUp, var_seldbg_setup
isvariablestruc "DPSPSEL", 2, \
	ivfReadOnly | ivfSpecialSetUp, var_dpspsel_setup
%else
isvariablestruc "DPSPSEL", 2, \
	ivfReadOnly, pspdbg
%endif

isvariablestruc "PPC", 4, 0, default_pp_count
isvariablestruc "TPC", 4, 0, default_tp_count
isvariablestruc "TTC", 4, 0, default_tt_count

isvariablestruc "RELIMIT", 4, 0, re_limit
isvariablestruc "RECOUNT", 4, 0, re_count
isvariablestruc "RCLIMIT", 4, 0, rc_limit
isvariablestruc "RCCOUNT", 4, 0, rc_count

isvariablestruc "RC", 2, 0, priorrc
isvariablestruc "ERC", 2, 0, erc
isvariablestruc "QQCODE", 1, 0, qqtermcode
isvariablestruc "TERMCODE", 2, 0, usertermcode

%if _PM
 %assign var_addr_offset 4
%else
 %assign var_addr_offset 2
%endif

isvariablestruc "AAO", var_addr_offset, 0, a_addr
isvariablestruc "AAS", 2, 0, a_addr + saSegSel
isvariablestruc "ADO", var_addr_offset, 0, d_addr
isvariablestruc "ADS", 2, 0, d_addr + saSegSel
isvariablestruc "ABO", var_addr_offset, 0, behind_r_u_addr
isvariablestruc "ABS", 2, 0, behind_r_u_addr + saSegSel
isvariablestruc "AUO", var_addr_offset, 0, u_addr
isvariablestruc "AUS", 2, 0, u_addr + saSegSel
isvariablestruc "AEO", var_addr_offset, 0, e_addr
isvariablestruc "AES", 2, 0, e_addr + saSegSel
%if _DSTRINGS
isvariablestruc "AZO", var_addr_offset, 0, dz_addr
isvariablestruc "AZS", 2, 0, dz_addr + saSegSel
isvariablestruc "ACO", var_addr_offset, 0, dcpm_addr
isvariablestruc "ACS", 2, 0, dcpm_addr + saSegSel
isvariablestruc "APO", var_addr_offset, 0, dcount_addr
isvariablestruc "APS", 2, 0, dcount_addr + saSegSel
isvariablestruc "AWO", var_addr_offset, 0, dwcount_addr
isvariablestruc "AWS", 2, 0, dwcount_addr + saSegSel
%endif
%if _PM
isvariablestruc "AXO", 4, 0, a_addr
%endif

isvariablestruc "DSR", 1, 0, serial_rows
isvariablestruc "DSC", 1, 0, serial_columns
isvariablestruc "DST", 1, 0, serial_keep_timeout
%if _USE_TX_FIFO
isvariablestruc "DSF", 1, 0, serial_fifo_size
%endif
isvariablestruc "DSPVI", 1, 0,		 serial_var_intnum
isvariablestruc "DSPUI", 1, ivfReadOnly, serial_use_intnum
isvariablestruc "DSPVS", 1, 0,		 serial_var_params
isvariablestruc "DSPUS", 1, ivfReadOnly, serial_use_params
isvariablestruc "DSPVF", 1, 0,		 serial_var_fifo
isvariablestruc "DSPUF", 1, ivfReadOnly, serial_use_fifo
isvariablestruc "DSPVP", 2, 0,		 serial_var_baseport
isvariablestruc "DSPUP", 2, ivfReadOnly, serial_use_baseport
isvariablestruc "DSPVD", 2, 0,		 serial_var_dl
isvariablestruc "DSPUD", 2, ivfReadOnly, serial_use_dl
isvariablestruc "DSPVM", 2, 0,		 serial_var_irqmask
isvariablestruc "DSPUM", 2, ivfReadOnly, serial_use_irqmask

%if _40COLUMNS
isvariablestruc "IOCLINE", 1, 0, io_columns_getline
	; IOCLINE must be before IOC as otherwise "IOCLINE" would get
	;  misdetected as "IOC" + "L" separator + junk
%endif
isvariablestruc "IOR", 1, 0, io_rows
isvariablestruc "IOC", 1, 0, io_columns
isvariablestruc "IOS", 2, 0, io_start_buffer
isvariablestruc "IOE", 2, 0, io_end_buffer
isvariablestruc "IOL", 2, 0, io_levels
isvariablestruc "IOF", 2, 0, io_flags
isvariablestruc "IOI", 2, ivfReadOnly | ivfSpecialSetUp, var_ioi_setup

%if _CATCHINT2D
isvariablestruc "AMISNUM", 1, ivfReadOnly, amis_multiplex_number
isvariablestruc "TRYAMISNUM", 1, 0, try_amis_multiplex_number
%endif

isvariablestruc "HHRESULT", 4, 0, hhresult
%if _PM
isvariablestruc "DARESULT", 2, 0, daresult
%endif
%if _EMS
isvariablestruc "XARESULT", 2, 0, xaresult
%endif

%if _DEBUG1
isvariablestruc "TRC", 2, ivfArrayOptional, \
	test_records_Readmem + 4, 15, 4
isvariablestruc "TRA", 4, ivfArrayOptional, \
	test_records_Readmem, 15, 2
isvariablestruc "TWC", 2, ivfArrayOptional, \
	test_records_Writemem + 4, 15, 4
isvariablestruc "TWA", 4, ivfArrayOptional, \
	test_records_Writemem, 15, 2
isvariablestruc "TLC", 2, ivfArrayOptional, \
	test_records_getLinear + 4, 15, 4
isvariablestruc "TLA", 4, ivfArrayOptional, \
	test_records_getLinear, 15, 2
isvariablestruc "TSC", 2, ivfArrayOptional, \
	test_records_getSegmented + 4, 15, 4
isvariablestruc "TSA", 4, ivfArrayOptional, \
	test_records_getSegmented, 15, 2
isvariablestruc "TRV", 1, 0, \
	test_readmem_value
%endif


%if _PM
isvariablestruc "SRO", 4, ivfArrayOptional | ivfReadOnly, \
	search_results, 15, 2
isvariablestruc "SRS", 2, ivfArrayOptional | ivfReadOnly, \
	search_results + 4, 15, 4
%else
isvariablestruc "SRO", 2, ivfArrayOptional | ivfReadOnly, \
	search_results, 15, 2
isvariablestruc "SRS", 2, ivfArrayOptional | ivfReadOnly, \
	search_results + 2, 15, 2
%endif
isvariablestruc "SRC", 4, ivfReadOnly, \
	sscounter

isvariablestruc "RI", 2, ivfSpecialSetUp | ivfSeparatorSpecial | ivfReadOnly, \
	var_ri_setup, 255

%if _DEBUG3
isvariablestruc "MT", 4, ivfSpecialSetUp, var_mt_setup, 1
%endif

%if _INPUT_FILE_BOOT || _INPUT_FILE_HANDLES
isvariablestruc "YSF", 2, ivfSpecialSetUp | ivfArrayOptional, \
	var_ysf_setup, 15
 %if _INPUT_FILE_HANDLES
isvariablestruc "YHF", 2, ivfSpecialSetUp | ivfArrayOptional, \
	var_ysf_setup, 15
 %endif
 %if _INPUT_FILE_BOOT
isvariablestruc "YBF", 2, ivfSpecialSetUp | ivfArrayOptional, \
	var_ysf_setup, 15
 %endif
%endif

%if _ACCESS_VARIABLES_AMOUNT
isvariablestruc "READADR", 4, ivfReadOnly | ivfArrayOptional, \
	reading_access_variables, _ACCESS_VARIABLES_AMOUNT - 1, 4
isvariablestruc "READLEN", 4, ivfReadOnly | ivfArrayOptional, \
	reading_access_variables + 4, _ACCESS_VARIABLES_AMOUNT - 1, 4
isvariablestruc "WRITADR", 4, ivfReadOnly | ivfArrayOptional, \
	writing_access_variables, _ACCESS_VARIABLES_AMOUNT - 1, 4
isvariablestruc "WRITLEN", 4, ivfReadOnly | ivfArrayOptional, \
	writing_access_variables + 4, _ACCESS_VARIABLES_AMOUNT - 1, 4
%endif

%if _PSPVARIABLES
 %if _PM
isvariablestruc "PSPS", 2, ivfReadOnly | ivfSpecialSetUp, var_psps_setup
isvariablestruc "PSPSEL", 2, ivfReadOnly | ivfSpecialSetUp, var_psps_setup
 %else
isvariablestruc "PSPS", 2, ivfReadOnly, pspdbe
isvariablestruc "PSPSEL", 2, ivfReadOnly, pspdbe
 %endif
isvariablestruc "PSP", 2, ivfReadOnly, pspdbe
isvariablestruc "PPR", 2, ivfReadOnly | ivfSpecialSetUp, var_ppr_setup
isvariablestruc "PPI", 4, ivfReadOnly | ivfSpecialSetUp, var_ppi_setup
isvariablestruc "PARENT", 2, ivfReadOnly | ivfSpecialSetUp, var_ppr_setup
isvariablestruc "PRA", 4, ivfReadOnly | ivfSpecialSetUp, var_ppi_setup
%endif

%if _BOOTLDR
isvariablestruc "BOOTUNITFL", 1, ivfSpecialSetUp, var_bootunitflags_setup, 255
isvariablestruc "BOOTLDPUNIT", 1, ivfReadOnly | ivfSpecialSetUp, var_bootldpunit_setup
isvariablestruc "BOOTSDPUNIT", 1, ivfReadOnly | ivfSpecialSetUp, var_bootsdpunit_setup
 %if _INPUT_FILE_BOOT
isvariablestruc "BOOTYDPUNIT", 1, ivfReadOnly | ivfSpecialSetUp, var_bootydpunit_setup
 %endif
isvariablestruc "BOOTLDPPART", 4, ivfReadOnly | ivfSpecialSetUp, var_bootldppart_setup
isvariablestruc "BOOTSDPPART", 4, ivfReadOnly | ivfSpecialSetUp, var_bootsdppart_setup
 %if _INPUT_FILE_BOOT
isvariablestruc "BOOTYDPPART", 4, ivfReadOnly | ivfSpecialSetUp, var_bootydppart_setup
 %endif
%endif

isvariablestruc "TRYDEBUGNUM", 1, 0, try_debugger_amis_multiplex_number
isvariablestruc "DEBUGFUNC", 2, ivfReadOnly, debuggerfunction
%if _MCLOPT
isvariablestruc "MASTERPICBASE", 1, 0, master_pic_base
%endif

isvariablestruc "DSTACKSEG", 2, ivfReadOnly, pspdbg
isvariablestruc "DENTRYSEG", 2, ivfReadOnly, pspdbg
isvariablestruc "DCODE1SEG", 2, ivfReadOnly, code_seg
%if _DUALCODE
isvariablestruc "DCODE2SEG", 2, ivfReadOnly, code2_seg
%endif
%if _PM
isvariablestruc "DAUXBUFSEG", 2, ivfReadOnly, auxbuff_segorsel + soaSegment
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
isvariablestruc "DHISBUFSEG", 2, ivfReadOnly, history.segorsel + soaSegment
 %endif
%else
isvariablestruc "DAUXBUFSEG", 2, ivfReadOnly, auxbuff_segorsel
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
isvariablestruc "DHISBUFSEG", 2, ivfReadOnly, history.segorsel
 %endif
%endif
 %if _IMMASM
isvariablestruc "DIMMSEG", 2, ivfReadOnly, immseg
 %endif

%if _PM
isvariablestruc "DSTACKSEL", 2, ivfReadOnly, dssel
isvariablestruc "DENTRYSEL", 2, ivfReadOnly, cssel
isvariablestruc "DCODE1SEL", 2, ivfReadOnly, code_sel
 %if _DUALCODE
isvariablestruc "DCODE2SEL", 2, ivfReadOnly, code2_sel
 %endif
isvariablestruc "DAUXBUFSEL", 2, ivfReadOnly, auxbuff_segorsel + soaSelector
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
isvariablestruc "DHISBUFSEL", 2, ivfReadOnly, history.segorsel + soaSelector
 %endif
isvariablestruc "DSCRATCHSEL", 2, ivfReadOnly, scratchsel
 %if _SYMBOLIC
isvariablestruc "DSYM1SEL", 2, ivfReadOnly, symsel1
isvariablestruc "DSYM2SEL", 2, ivfReadOnly, symsel2
 %endif
 %if _IMMASM
isvariablestruc "DIMMSEL", 2, ivfReadOnly, immsel
 %endif
%endif

isvariablestruc "DEFAULTDLEN", 2, 0, dd_default_length
isvariablestruc "DEFAULTDLINES", 2, 0, dd_default_lines
isvariablestruc "DEFAULTULEN", 2, 0, uu_default_length
isvariablestruc "DEFAULTULINES", 2, 0, uu_default_lines

%if _PM
isvariablestruc "CIP", 2, ivfSpecialSetUp, var_cip_setup
isvariablestruc "CSP", 2, ivfSpecialSetUp, var_csp_setup
%else
isvariablestruc "CIP", 2, 0, reg_eip
isvariablestruc "CSP", 2, 0, reg_esp
%endif

isvariable_struc_morebyte_list_end:
isvariable_struc_list_end:

	align 2, db 0
interrupt_var:	dd 0
	align 2, db 0
ioi_var:	dw 0
%if _PM
	align 2, db 0
seldbg:	dw 0
	align 2, db 0
dpspsel:	dw 0
%endif
%if _PSPVARIABLES
	align 2, db 0
 %if _PM
psp_selector:	dw 0
 %endif
psp_parent:	dw 0
psp_pra:	dd 0
%endif

	align 2, db 0
isvariable_morebyte_nameheaders:
	dw IVS_MOREBYTE_NAMEHEADERS
.end:
%if IVS_HAVE_ONEBYTE && ! IVS_SINGLE_ONEBYTE
isvariable_onebyte_names:
	db IVS_ONEBYTE_NAMES
.end:
%endif

isvariablestrings ISVARIABLESTRINGS


	usesection lDEBUG_CODE
isvariable_code:
		; INP:	ax = array index (0-based), di = 0
		;	cl = default size of variable (1..4)
		; CHG:	si, ax
		; OUT:	NC if valid,
		;	 bx -> var, di = 0 or di -> mask
		;	 cl = size of variable (1..4)

%if _PM
var_cip_setup:
	mov bx, word [reg_cs]
	call test_d_b_bit
	mov bx, reg_eip
@@:
	jz .ret
	inc cx
	inc cx			; = 4
.ret:
	clc			; (NC)
	retn

var_csp_setup:
	mov bx, word [reg_ss]
	call test_d_b_bit
	mov bx, reg_esp
	jmp @B
%endif


var_lfsr_setup:
	mov bx, lfsr
	shr word [bx + 2], 1
	rcr word [bx], 1
	jnc .ret		; if not to tap --> (NC)
	mov ax, word [lfsrtap]
	xor word [bx], ax
	mov ax, word [lfsrtap + 2]
	xor word [bx + 2], ax	; (NC)
.ret:
	retn


var_reverselfsr_setup:
	mov bx, reverselfsr
	xchg ax, cx		; preserve original cx
	mov cl, byte [reverselfsrtop]
	mov si, 1		; register for shift mask
	cmp cl, 16		; fits in low 16 bits ?
	jb .below_16		; yes -->
	sub cl, 16		; get bit within high word
	cmp cl, 16		; beyond maximum ?
	jb @F
	mov cl, 15		; yes, clamp to bit 31 (for now)
@@:
	shl si, cl		; shift to get a bitmask
	xchg cx, ax		; restore the original cx
	test word [bx + 2], si	; is top bit set ?
	lahf			; preserve ZF
	not si			; invert mask to allow clearing
	and word [bx + 2], si	; clear the bit if it was set
	mov di, -1		; si:di = mask what to clear in taps
	jmp @F

.below_16:
	shl si, cl		; shift to get a bitmask
	xchg cx, ax		; restore the original cx
	test word [bx], si	; is top bit set ?
	lahf			; preserve ZF
	not si			; invert mask to allow clearing
	and word [bx], si	; clear the bit if it was set
	mov di, si
	mov si, -1		; si:di = mask what to clear in taps
@@:
	sahf			; restore ZF
	xchg ax, di		; si:ax = mask what to clear in taps
	jz .justshift		; if not to tap, just shift --> (NC)
	call .justshift		; also shift, but return to us afterwards
	and ax, word [lfsrtap]
	and si, word [lfsrtap + 2]
				; get the taps (highest bit cleared)
	stc			; lowest bit will get set to 1
	rcl ax, 1
	rcl si, 1		; shift the taps to create reverse taps
	xor word [bx], ax
	xor word [bx + 2], si	; tap (NC)
	retn

.justshift:
	shl word [bx], 1
	rcl word [bx + 2], 1
	xor di, di		; restore di = 0
				; also: set NC if return from setup function
	retn


%if _PM
var_dpspsel_setup:
	mov bx, dpspsel
	mov word [bx], ss
	clc
	retn

var_seldbg_setup:
	mov bx, seldbg
	and word [bx], byte 0
	call ispm
	jnz @F
	mov word [bx], ds
@@:
	clc
	retn
%endif

%if _DEBUG3
var_mt_setup:
	mov bx, ax
	add bx, bx
	add bx, bx
	mov di, bx
	add bx, mtest_regs
	add di, masks_test
	clc
	retn
%endif

var_ioi_setup:
	call peekc
	mov bx, ioi_var
	mov word [bx], ax
	clc
	retn

%if _PSPVARIABLES
%if _PM
var_psps_setup:
	mov bx, psp_selector
	call var_get_psp_segment
	jc @F
	call ispm
	jnz @F
	push bx
	xchg ax, bx
	mov ax, 0002h
	int 31h
	pop bx
@@:
	mov word [bx], ax
	clc
	retn
%endif


var_ppr_setup:
 %if _PM
	 sub sp, 8
	call save_scratchsel
 %endif
	mov bx, psp_parent
 	xor ax, ax
	mov word [bx], ax
	call var_get_psp_selector
	jc @F
	mov ax, word [es:16h]
	mov word [bx], ax		; retrieve parent process
@@:
	push ss
	pop es
 %if _PM
	call restore_scratchsel
		; This is not really needed but does not hurt either.
 		;  getsegmented is used above, but only from PM.
 		;  This is assumed not to switch modes.
	call resetmode			; reset mode if we switched
 %endif
	clc
	retn


var_ppi_setup:
 %if _PM
	 sub sp, 8
	call save_scratchsel
 %endif
	mov bx, psp_pra
 	xor ax, ax
	mov word [bx], ax
	mov word [bx + 2], ax
	call var_get_psp_selector
	jc @B
	mov ax, word [es:TPIV]
	mov word [bx], ax
	mov ax, word [es:TPIV + 2]
	mov word [bx + 2], ax		; retrieve Int22 address
	jmp @B


var_get_psp_segment:
	mov ax, word [pspdbe]
	inc ax				; FFFFh ?
	jz short .pspvar_psp_invalid	; yes, invalid -->
	dec ax				; 0 ?
	jz short .pspvar_psp_invalid	; yes, invalid -->
	clc
	retn

.pspvar_psp_invalid:
	xor ax, ax
	stc
	retn


var_get_psp_selector:
	call var_get_psp_segment
	jc .pspvar_psp_invalid
 %if _PM
	call ispm
	jnz short .pspvar_rm
	push bx
	_386_o32		; push edx
	push dx
	push cx
	xor dx, dx
	mov cx, 4
.pspvar_shift:
	shl ax, 1
	rcl dx, 1
	loop .pspvar_shift		; dx:ax = PSP segment << 4
	call getsegmented
		; getsegmented is assumed not to switch modes (see below).
	jc short .pspvar_error		; (shouldn't happen)
	_386_o32
	test dx, dx		; test edx, edx
	jnz short .pspvar_error		; (assumed not to happen)
	call setrmlimit
	pop cx
	_386_o32
	pop dx			; pop edx
	xchg ax, bx			; ax = selector
	pop bx
.pspvar_rm:
 %endif
	mov es, ax			; es = segment/selector
	cmp word [es:0], 20CDh		; int 20h opcode ?
	jne short .pspvar_psp_invalid	; no, invalid -->
	clc
	retn

.pspvar_psp_invalid:
	stc
	retn

.pspvar_error:
	xor si, si
	jmp error
%endif


%if _BOOTLDR
var_bootldpunit_setup:
	mov bx, loaddata_loadedfrom - LOADDATA \
			+ bsBPB + ebpbNew + bpbnBootUnit
	jmp @F

var_bootsdpunit_setup:
	mov bx, load_data - LOADDATA2 \
			+ bsBPB + ebpbNew + bpbnBootUnit
	jmp @F

%if _INPUT_FILE_BOOT
var_bootydpunit_setup:
	testopt [internalflags2], dif2_input_file_boot
	jz .retc

	push dx
	mov ax, LOAD_INPUT_FILE_SIZE
	mul word [load_input_file.active]
	pop dx

	xchg ax, bx
	lea bx, [load_input_file + bx - LOADDATA3 \
			+ bsBPB + ebpbNew + bpbnBootUnit]
	jmp @F
%endif

var_bootldppart_setup:
	mov bx, loaddata_loadedfrom - LOADDATA \
			+ bsBPB + bpbHiddenSectors
	jmp @F

var_bootsdppart_setup:
	mov bx, load_data - LOADDATA2 \
			+ bsBPB + bpbHiddenSectors
	jmp @F

%if _INPUT_FILE_BOOT
var_bootydppart_setup:
	testopt [internalflags2], dif2_input_file_boot
	jz .retc

	push dx
	mov ax, LOAD_INPUT_FILE_SIZE
	mul word [load_input_file.active]
	pop dx

	xchg ax, bx
	lea bx, [load_input_file + bx - LOADDATA3 \
			+ bsBPB + bpbHiddenSectors]
	jmp @F
%endif

var_bootunitflags_setup:
	add ax, load_unit_flags
	mov bx, ax
	mov di, mask_bootunitflags
@@:
	testopt [internalflags], nodosloaded
	jnz @F				; (NC)
var_bootydpunit_setup.retc:
var_bootydppart_setup.retc:
	stc
@@:
	retn
%endif


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
masks:
	; size 4 want masks + 0
	; size 3 want masks + 1
	; size 2 want masks + 2
	; size 1 want masks + 3
	; size 0 want masks + 4
	; 4 - size = offset into masks
mask_4byte:		db 0
mask_3byte:		db 0
mask_2byte:		db 0
mask_1byte:		db 0
mask_0byte:		dd -1
mask_compoundwithzero:	db -1, -1, 0, 0
mask_ysf:		dd ~ (ifhfTestReserved1 | ifhfTestReserved2 \
				| ifhfQuietInput | ifhfQuietOutput)
%if _BOOTLDR
mask_bootunitflags:	dd ~ luf_mask_writable
%endif

%if _DEBUG3
masks_test:
mask_test_0:		dd 0AA55AA55h
mask_test_1:		dd 00FF00FFh
mtest_regs:
mtest_reg_0:		dd 0
mtest_reg_1:		dd 00110022h
%endif


	usesection lDEBUG_CODE

%if _SYMBOLIC
		; INP:	al = first character
		;	si -> next character
		; OUT:	CY if no symbol
		;	NC if symbol,
		;	 bx:dx = symbol (offset) value
		;	 al = next character behind symbol
		;	 si -> behind next character
		; CHG:	ah, bx, cx, dx
		; STT:	ss = ds = es, UP
		;
		; Note:	This invalidates the symbol table access slice.
issymbol?:
	nearcall zz_detect_xms	; re-detect XMS if used after run (eg WHILE)

	lframe near
	lvar word,	new_si
	lenter
	lvar word,	orig_si
	 push si
	lvar word,	orig_ax
	 push ax
	xor bx, bx
	lequ 1,		flag_has_nondigit
	lequ 2,		flag_has_symbol_prefix
	lequ 4,		flag_has_linear
	lequ 8,		flag_has_offset
	lequ 16,	flag_has_base
	lequ 32,	flag_is_86m_segment
	lvar word,	flags
	 push bx
	lvar word,	main_index	; used as parameter to zz_match_symbol
	 push ax

	dec si
%if 0
	cmp al, '.'
	jne @F
	inc si			; allow dot prefix to symbol name
	mov al, byte [si]
@@:
%endif
	cmp al, '0'
	jb @F
	cmp al, '9'
	jbe .not
@@:

	mov dx, msg.sl
	call isstring?
	jne @F
	lodsb
	cmp al, '.'
	jne short .error
	or byte [bp + ?flags], ?flag_has_symbol_prefix | ?flag_has_linear
	jmp .not_symbol_prefix
@@:

	mov dx, msg.symbol
	call isstring?
	jne .not_symbol_prefix
	lodsb
	cmp al, '.'
	jne short .error
	or byte [bp + ?flags], ?flag_has_symbol_prefix

	mov dx, msg.linear
	call isstring?
	jne @F
	lodsb
	cmp al, '.'
	jne short .error
	or byte [bp + ?flags], ?flag_has_linear
@@:

	mov dx, msg.offset
	call isstring?
	jne @F
	lodsb
	cmp al, '.'
	jne short .error
	test byte [bp + ?flags], ?flag_has_linear
	jnz short .error
	or byte [bp + ?flags], ?flag_has_offset
@@:

	mov dx, msg.base
	call isstring?
	jne @F
	lodsb
	cmp al, '.'
	jne short .error
	test byte [bp + ?flags], ?flag_has_linear | ?flag_has_offset
	jnz short .error
	or byte [bp + ?flags], ?flag_has_base
@@:

.not_symbol_prefix:

	xor bx, bx
	mov dx, si
.loopname:
	lodsb
	call isseparator?.except_L_or_dot
	je .endname
	call getexpression.lit_ishexdigit?
	jnc @F
	or byte [bp + ?flags], ?flag_has_nondigit
@@:
	inc bx
	jmp .loopname

.error:
	jmp error			; does not use es

.endname:
	mov word [bp + ?new_si], si
	mov cx, bx
	test cx, cx
	jz .not

	test byte [bp + ?flags], ?flag_has_nondigit | ?flag_has_symbol_prefix
	jz .not

	mov si, dx
	push cx
	nearcall zz_hash
	pop cx

.loop:
		; ?main_index used as parameter
	 push ss
	 push dx
	 push cx

	dualcall zz_match_symbol		; ! note that possibly es != ss
	jc .not

	testopt [es:di + smFlags], smfPoison
	jz @F
	setopt [internalflags2], dif2_poison
@@:

	mov dx, word [es:di + smLinear]
	mov bx, word [es:di + smLinear + 2]
	test byte [bp + ?flags], ?flag_has_linear
	jnz .got
	test byte [bp + ?flags], ?flag_has_base
	jz .offset
.base:
		; offset=100 linear=10100 base=10000
	sub dx, word [es:di + smOffset]
	sbb bx, word [es:di + smOffset + 2]
	jmp .got

.offset:
	mov si, word [bp + ?new_si]
	dec si
	call skipcomma			; does not use es
	dec si
	mov dx, msg.wrt
	push es
	 push ss
	 pop es
	call isstring?			; uses es
	pop es
	jne .offset_no_wrt

	call skipcomma			; does not use es

	cmp al, '$'
	jne @F

	or byte [bp + ?flags], ?flag_is_86m_segment
	call skipcomma			; does not use es
@@:

	push word [hh_depth_of_single_term]
	mov dx, word [hh_depth]
	inc dx
	mov word [hh_depth_of_single_term], dx
	 push ss
	 pop es
	call getexpression		; (recursive) uses es
	pop word [hh_depth_of_single_term]

		; The getexpression call may recurse into calling
		;  this function itself again. Therefore, it can
		;  invalidate the access slice. To make sure we
		;  can access the SYMMAIN entry again, reload it.
	 push word [bp + ?main_index]
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

	call skipcomm0			; does not use es
	mov word [bp + ?new_si], si

	test bx, bx
	jnz short .errorj1

	mov bx, dx
%if _PM
	test byte [bp + ?flags], ?flag_is_86m_segment
	jnz .wrt_rm
	call ispm			; does not use es
	jnz .wrt_rm

	mov ax, 0006h
	int 31h					; cx:dx = base
	jc short .errorj1
	xchg ax, cx				; ax:dx = base
	xchg dx, ax				; dx:ax = base
	jmp .offset_wrt

.wrt_rm:
%endif
	xor dx, dx
	mov ax, bx
	mov cx, 4
@@:
	shl ax, 1
	rcl dx, 1
	loop @B

.offset_wrt:
		; dx:ax = seg base

		; offset=100
		; linear=10100
		; seg base=8000
		; offset wrt seg=8100
	neg dx
	neg ax
	sbb dx, byte 0				; neg dx:ax
	add ax, word [es:di + smLinear]
	adc dx, word [es:di + smLinear + 2]	; linear - seg base
	xchg ax, dx				; ax:dx = value
	xchg ax, bx				; bx:dx = value
	jmp .got

.errorj1:
	jmp error			; does not use es


.offset_no_wrt:
	mov dx, word [es:di + smOffset]
	mov bx, word [es:di + smOffset + 2]

.got:
	testopt [es:di + smFlags], smfBase
	jz @F
	push word [bp + ?main_index]
	pop word [sym_storage.main.based.base]
		; if called from zz_add set up base
@@:

	mov si, word [bp + ?new_si]
	dec si
	lodsb
	clc
	jmp .ret
.not:
	stc

	mov si, word [bp + ?orig_si]
	mov ax, word [bp + ?orig_ax]
.ret:
	push ss
	pop es				; reset es
	lleave
	retn
%endif


		; INP:	al = first character
		;	si-> next character
		; OUT:	CY if no variable,
		;	 al, si = unchanged
		;	NC if variable,
		;	 bx-> low word
		;	 dx-> high word
		;		(if cl <= 2 then dx-> some word in our memory)
		;		(dx != bx+2 if compound register)
		;	 cl = size of variable (1, 2, 3, 4 bytes)
		;	 ch = size of variable's name (2..13 bytes)
		;		(this is limited to ivfNameLengthLimit)
		;	 INP:si - 1 -> variable's name
		;		(can be modified from original content)
		;	 ah = 0 if a writeable variable (ie simply memory)
		;	      1 if a read-only variable (ie simply memory)
		;	      2..33 if an MMX register, see note below
		;	 al = next character behind variable
		;	 si-> behind next character
		;	 dword [di] = mask of bits that are read-only
		; CHG:	ah, bx, dx, cx, di
		; STT:	ss = ds = es, UP
		;
		; Note: For read access to (half of) an MMX register, no
		;	 special handling is necessary at all, because cl,
		;	 bx, and dx are set up to access a buffer that
		;	 contains the current value. (The value should be
		;	 read at once though, as the buffer may be shared
		;	 or become outdated otherwise.)
		;	Write access to an MMX register must be handled
		;	 specifically, however. The returned field type
		;	 in ah indicates the register number (0..7) in the
		;	 lowest three bits. The two bits above those specify
		;	 the access type, which also specifies what was read
		;	 but need not be examined by readers. The access
		;	 type must be adhered to by writers. These are the
		;	 access types:
		;	  0 zero extension from 32 bits to write all 64 bits
		;	  1 sign extension from 32 bits to write all 64 bits
		;	  2 writes only low 32 bits
		;	  3 writes only high 32 bits
		;	 (Access type 3 is the only one for which the read
		;	 buffer is initialised with the high 32 bits.)
		;
		;	As dx points to 'some word in our memory' if it
		;	 doesn't serve any purpose, it is still valid to
		;	 read the word that it points to. Particularly dx
		;	 mustn't contain 0FFFFh then, but with the current
		;	 implementation, it can also be assumed that we do
		;	 actually 'own' the word (even with a PM segment
		;	 shorter than 64 KiB the read would be allowed).
isvariable?:
	db __TEST_IMM8		; (skip stc, NC)

		; As above but additionally:
		;
		; INP:	di -> buffer to receive variable name, 14 bytes
		; OUT:	if NC, buffer filled with all-caps ASCIZ name
.return_name:
	stc

	dec si
	lframe
	lenter
	lvar word, bit0_is_return_name
	 pushf
	lvar word, return_name_pointer
	 push di
%if ivfNameLengthLimit != 13
 %error Adjust code here to new ivfNameLengthLimit
%endif
	xor ax, ax
	lvar 16, namebuffer
	 push ax		; ax = 0 so that accidentally reading past
				;	  the actual buffer wouldn't match
	 push word [si+12]
	 push word [si+10]
	 push word [si+8]
	 push word [si+6]
	 push word [si+4]
	 push word [si+2]
	 push word [si]
	mov di, sp		; -> name buffer

	lvar word, fieldtype_high_flags_low
	lequ ?fieldtype_high_flags_low + 1, fieldtype
	lequ ?fieldtype_high_flags_low, flags
	push ax			; field type initialised to 0 (RW), flags too
_386	xor bx, bx		; (a flag for the 32-bit register name check)
	lvar word, length_to_add
	push ax			; = 0
	lvar word, startpointer
	push si			; -> name start
	mov si, di
	 push di

	mov cx, 16
.store:
	lodsb
	; call uppercase	; (isseparator? calls uppercase)
	call isseparator?
	clc
	jne short .not_separator
	stc
.not_separator:
	rcr dx, 1		; dl = flags indicating separators
	stosb
	loop .store

	 pop si

	test byte [bp + ?bit0_is_return_name], 1
	jz @F

	push si
	mov cl, 14 >> 1
	mov di, word [bp + ?return_name_pointer]
	rep movsw
	pop si

@@:
	test dl, 1<<2|1<<4
	lodsw
	jz .notreg16

	call .reg16names_match	; (iff no match, --> .notreg16)
				; bx-> regs entry of (first) match
	test dl, 1<<2
	lodsw
	jz @F
	test dl, 1<<5
	jz .reg16_j
	cmp word [si - 4], "FL"
	jne .reg16_j
	cmp al, '.'
	jne .reg16_j
	dec si
	lodsw
	mov di, flagnames
	mov cx, flagbits.amount
	repne scasw
	jne .reg16_j
	mov ax, [di - (flagnames + 2) + flagbits]
	mov bx, flagvaron
	test ax, word [reg_efl]
	jnz .flagon
	inc bx
.flagon:
	mov cx, (5 << 8) | 1	; 5-byte name, 1-byte variable
	inc byte [bp + ?fieldtype]
				; = 1 (RO)
	jmp .return_success_var32_set_dx

.reg16_j:
	jmp .reg16		; iff single match -->

@@:
	cmp ax, "00"
	je .compoundwithzero

		; Check for a second 16-bit register name
		;  (ie check for a compound register name)
	call .reg16names_match	; (iff no match, --> .notreg16)
				; bx-> regs entry of second match
	xchg dx, ax		; dx-> regs entry of first match
	mov cx, 4<<8|4
.return_success:		; cx, bx, dx, ?fieldtype set
	xor ax, ax
	mov al, cl
	neg ax
	add ax, masks + 4	; 4 - size = offset into masks
	xchg di, ax		; di -> mask
	cmp byte [bp + ?fieldtype], 1
	jne @F
	mov di, mask_0byte	; di -> mask
@@:
.return_success_di:
	xor ax, ax
	mov al, ch		; ax = length

	test byte [bp + ?bit0_is_return_name], 1
	jz @F

	mov si, word [bp + ?return_name_pointer]
	add si, ax
	mov byte [si], ah	; zero-terminate
@@:

	pop si			; ?startpointer
	testopt [bp + ?flags], 1
	jz @F
	pop ax			; get length to add to start
	push ax			; fill stack slot again
@@:
	add si, ax		; -> behind name (should NC)
	pop ax			; discard ?length_to_add
	clc			; (NC)
.return_ax_frame_lodsb:
	pop ax			; ah = ?type
	lleave code
	lodsb
.retn:
	retn


.compoundwithzero:
	mov dx, bx		; -> word to use as upper word
	mov di, mask_compoundwithzero
				; only allow writing upper word
	mov cx, 4 << 8 | 4	; 4-byte name, 4-byte variable
	lea bx, [di + 2]	; -> (constant) word that is zero, as lower word
		; This depends on the contents of mask_compoundwithzero.
	jmp .return_success_di


		; INP:	ax = capitalised candidate register name
		;	ch = 0
		;	dx, si, bx, etc set up for later checks
		; OUT:	Iff match,
		;	 bx-> associated regs entry (dword-aligned)
		;	 ax = INP:bx
		;	Else,
		;	 returns to .notreg16
		;	 bx left unchanged on 386 systems
		; CHG:	cl, di, bx, ax
		;
		; Note:	The 32-bit register name check depends on the
		;	 fact that the low two bits of bx are set to
		;	 zero on a match, which is true because regs
		;	 is dword-aligned.
		;	It also depends on bx being left unchanged on
		;	 a mismatch, which is the case unless the
		;	 non-386 additional FS,GS filtering occurs.
.reg16names_match:
	mov di, reg16names
	mov cl, 16
	repne scasw
	jne short .notreg16_pop	; no match -->
	add di, di
	xchg ax, bx		; (returned for compound register name match)
	lea bx, [di -2*(reg16names+DATASECTIONFIXUP+2) +regs]
				; -> regs entry
_386	retn
				; cx = number of remaining reg16names
_no386	shr cx, 1		; cx = number of remaining reg16names pairs
				;    = 1 iff exactly the IP,FL pair remaining
				;	  (ie matched one of FS,GS)
_no386	loop .retn		; iff cx != 1, return the match -->
			; on non-386 systems, FS,GS matches fall through here
.notreg16_pop:
	pop ax			; (discard near return address)
.notreg16:
		; Check for a 32-bit register name
_386	test dl, 1<<3
_386	jz short .notreg32

_386	lea si, [bp+?namebuffer]
_386	lodsb
_386	shr bl, 1		; CY iff second entry during same call
				;  (in that case, al contains 'E')
_386	sbb al, 'E'		; possibly an 'E' register ? (on first entry)
_386	lodsw
_386	jne short .notreg32	; no --> (or after second entry)
_386	inc bx			; prepare flag for second entry
				;  (this requires regs to be dword-aligned!)
_386	cmp ah, 'S'		; candidate segment register ?
_386	je short .notreg32	; yes, skip check (disallow match) -->

_386	call .reg16names_match	; (iff no match, --> .notreg16 (second entry))
				; bx-> regs entry of match
_386	mov cx, 3<<8|4
_386	jmp short .return_success_var32_set_dx

.notreg32:
		; Check for an 8-bit register name
	test dl, 1<<2
	jz short .notreg8

	lea si, [bp+?namebuffer]
	lodsw
	mov di, reg8names
	mov cl, 8
	repne scasw
	jne short .notreg8
				; cx = cl = number of remaining reg8names
	and cl, 1		; cx = cl = 1 iff an xL register, else 0
	lea bx, [di-reg8names-2+regs-1]
				; bx-> reg_eax-1 if AL, reg_eax+1 if AH, etc
	add bx, cx		; bx-> reg_eax   if AL, reg_eax+1 if AH, etc
	mov cl, 1

	db __TEST_IMM16		; (NC, skip mov)
.reg16:
	mov cl, 2
.got2bytename:
.got2bytename_var32_set_dx:
	mov ch, 2
.return_success_var32_set_dx:
	lea dx, [bx+2]		; (irrelevant to 8-/16-bit register return)
%define .return_success_j .return_success_j1
%[.return_success_j]:
	jmp .return_success

.notreg8:

	mov di, isvariable_morebyte_nameheaders

.loop:
	mov ax, word [bp + ?namebuffer]
	mov cx, isvariable_morebyte_nameheaders.end
	cmp di, cx
	jae @F
	sub cx, di
	shr cx, 1			; = how many headers remaining
	repne scasw
	je .check_morebyte
%if IVS_HAVE_ONEBYTE
	jmp .next_no_pop

@@:
 %if IVS_SINGLE_ONEBYTE
	inc cx
	cmp di, cx			; iteration after one byte var check ?
	jae @F				; yes, end -->
	inc di				; remember we did the one byte check
	cmp al, IVS_SINGLE_ONEBYTE_NAME
	je .check_onebyte
 %else
	mov cx, isvariable_onebyte_names.end
	sub cx, di			; = how many names remaining
	jbe @F				; if none -->
	repne scasb
	je .check_onebyte
 %endif
%endif
	db __TEST_IMM8			; (skip pop)
.next:
	pop di
.next_no_pop:
	jmp .loop

@@:
	jmp .end


%if IVS_HAVE_ONEBYTE
.check_onebyte:
	push di
 %if IVS_SINGLE_ONEBYTE
	mov bx, isvariable_struc_onebyte_list
 %else
	sub di, isvariable_onebyte_names + 1
					; = index
%if ISVARIABLESTRUC_size == 8
	add di, di			; = index * 2
	add di, di			; = index * 4
	add di, di			; = index * 8
%else
 %error Unexpected structure size
%endif
	lea bx, [di + isvariable_struc_onebyte_list]
 %endif
	lea si, [bp + ?namebuffer + 1]	; -> buffered string (allcaps)
	xor cx, cx			; = 0, gets incremented next
	jmp @F
%endif

.check_morebyte:
	push di
	sub di, isvariable_morebyte_nameheaders + 2
					; = index * 2
%if ISVARIABLESTRUC_size == 8
	add di, di			; = index * 4
	add di, di			; = index * 8
%else
 %error Unexpected structure size
%endif
	lea bx, [di + isvariable_struc_morebyte_list]
	mov cx, word [bx + ivFlags]
	and cx, ivfNameLengthMask	; cx = length
	mov di, word [bx + ivName]	; -> name
	lea si, [bp + ?namebuffer + 2]	; -> buffered string (allcaps)
	cmp al, al		; ZR (in case remaining length is zero)
	push cx
	repe cmpsb		; compare
	pop cx
	jne .next		; if not same -->
	inc cx			; increment twice
@@:
	inc cx			; increment once
	lodsb			; get next byte in buffer
	cmp byte [bx + ivArrayLast], 0
	je .notarray

.array:
	cmp al, '('
	jne .array_not_index_expression	; not index expression -->

.array_index_expression:
	mov si, word [bp + ?startpointer]
	add si, cx		; -> at opening parens
	inc si			; -> behind opening parens
	lodsb
	push dx
	push bx
	call getexpression
	push ax
	and ah, 3Fh
	cmp ah, 8		; from checksignificantbits
	pop ax
	mov dh, 0
	mov di, dx		; di = index
	pop bx
	pop dx
	ja short .next		; more than 8 significant bits -->
	call skipwh0
	cmp al, ')'		; insure closing parens
	jne short .error_j3

	push si
	sub si, word [bp + ?startpointer]
				; = how much to skip
	mov word [bp + ?length_to_add], si
	pop si
	setopt [bp + ?flags], 1

	test byte [bp + ?bit0_is_return_name], 1
	jz .array_no_return_name

	xchg ax, di
	mov di, word [bp + ?return_name_pointer]
	add di, cx		; -> behind name
	cmp byte [bx + ivArrayLast], 0Fh
	ja @F			; if >= 10h is a valid index -->
	call hexnyb		; expand to single digit
	jmp @FF
@@:
	call hexbyte		; expand to two digits
@@:
	xchg ax, di		; restore di = index

.array_no_return_name:
	inc cx			; make space for one digit
	cmp byte [bx + ivArrayLast], 0Fh
	jna @F			; if maximum index <= 0Fh -->
	inc cx			; make space for another digit
@@:
	jmp .array_index_expression_common

.array_not_index_expression:
	call isseparator?	; name ends ?
	jne @F			; no -->
	testopt [bx + ivFlags], ivfArrayOptional
				; is the index optional ?
	jnz .arrayfirst		; yes, handle as if first -->
.next_j1:
	jmp .next		; no, mismatch -->

.error_j3:
	jmp error

@@:
	call getnyb
	jc .next_j1
	cbw			; ax = first nybble of index
	xchg di, ax		; di = first nybble of index
	lodsb
	 testopt [bx + ivFlags], ivfSeparatorSpecial
	 jz @F
	cmp byte [bx + ivArrayLast], 0Fh
				; highest > 0Fh ?
	jna .gotseparatorspecial; no -->
	call getnyb
	jc .gotseparatorspecial	; not hexit, treat as separator -->
	jmp .have_second_digit	; have al == 0..0Fh

@@:
	call isseparator?	; single digit ?
	je @F			; yes -->
	cmp byte [bx + ivArrayLast], 0Fh
				; highest > 0Fh ?
	jna .next_j1		; no -->
.get_second_digit:
	call getnyb
	jc .next_j1
.have_second_digit:
	cbw			; ax = second nybble of index
	add di, di
	add di, di
	add di, di
	add di, di		; di = first digit times 16
	add di, ax		; di = full index
	lodsb
	 testopt [bx + ivFlags], ivfSeparatorSpecial
	 jnz @F
	call isseparator?
	jne .next_j1
	jmp .gotseparatorspecial
@@:		; make sure no hexit follows
	call getnyb
	jnc .next_j1
.gotseparatorspecial:

	neg si			; minus -> after separator
	lea cx, [bp + ?namebuffer + 1 + si]
		; -> buffered string plus 1 minus -> after separator
		; -> buffered string plus 1 minus (-> at separator plus 1)
		; -> buffered string minus -> at separator
	neg cx			; -> at separator minus -> buffered string
	mov si, word [bp + ?startpointer]
	add si, cx

.array_index_expression_common:
		; si -> behind index
	mov ax, di
	; test ah, ah
	; jnz short .error_j1	; (not possible)
	cmp al, byte [bx + ivArrayLast]
				; index > last ?
	ja short .next_j1	; then error -->

	testopt [bx + ivFlags], ivfArrayOneBased
	jz @F			; got index
	dec ax			; decrement index to get 0-based
	js short .next_j1	; if it was 0, invalid -->
@@:
	xchg ax, si		; si = index
	jmp .arraycommon	; continue


.notarray:
	call isseparator?	; name ends ?
	jne short .next_j1	; no, mismatch -->

.arrayfirst:
	xor si, si		; si = array index (0 here)

.arraycommon:
	mov ch, cl		; ch = name length
	mov ax, word [bx + ivFlags]
	and ax, ivfVarLengthMask; ax = shifted encoded var length
	mov cl, ivfVarLengthShift
	shr ax, cl		; ax = encoded var length
	inc ax			; decode var length
	mov cl, al		; cl = var length

	xchg ax, si		; ax = array index

	testopt [bx + ivFlags], ivfReadOnly
	jz @F
	inc byte [bp + ?fieldtype]
				; = 1 (RO)
@@:
	xor di, di
	mov si, word [bx + ivAddress]
	testopt [bx + ivFlags], ivfSpecialSetUp
	jz .notspecial
		; (NC)
	call si			; INP:	ax = array index (0-based), di = 0
				;	cl = default size of variable (1..4)
				; CHG:	si, ax
				; OUT:	NC if valid,
				;	 bx -> var, di = 0 or di -> mask
				;	 cl = size of variable (1..4)
	jnc @F
	jmp .next

.notspecial:
	mov bl, byte [bx + ivArrayBetween]
	mov bh, 0		; bx = array between offset
	add bl, cl
	adc bh, 0		; bx = array item size
	mul bx			; dx:ax = ax times bx
	test dx, dx
	jnz short .error_j2
	add ax, si		; ax -> variable
	jc short .error_j2
	xchg ax, bx		; bx -> variable
@@:
	pop dx			; (discard pointer into names/headers)
	lea dx, [bx+2]		; (irrelevant to 8-/16-bit register return)
	test di, di
	jz .return_success
	jmp .return_success_di

.error_j2:
	jmp error

.end:
%if _MMXSUPP
		;MMx  MMxf MM(x) MM(x)f
	test dl, 1<<3|1<<4| 1<<5|  1<<6
	jz short .notmmx_ZR

	lea si, [bp+?namebuffer]
	cmp byte [has_mmx], 0	; MMX supported ?
.notmmx_ZR:
	je short .notmmx	; no -->
[cpu 586]
	lodsw
	cmp ax, "MM"		; possibly an MMX register ?
	jne short .notmmx	; no -->
	lodsb
	call getstmmxdigit
	jc short .notmmx

 %if _PM
	 sub sp, 8
	call save_scratchsel
	mov dx, word [code_seg]
	call setes2dx		; es => lDEBUG_CODE (writable)
%else
	mov es, word [code_seg]	; es => lDEBUG_CODE
%endif
	mov ah, bl
	shl bl, 3		; shift into reg field
	 lea cx, [si-?namebuffer]
	or bl, 06h		; code to get our ModR/M byte (r/m = [ofs])
	 sub cx, bp		; length of name (if with suffix)
	mov byte [es:.getmmxlow_modrm], bl
				; SMC in section lDEBUG_CODE
	inc bx			; adjust the ModR/M byte (r/m = [bx])
	lodsb
	mov byte [es:.getmmxhigh_modrm], bl
				; SMC in section lDEBUG_CODE
	jmp @F			; try to invalidate prefetch
@@:
	push ss
	pop es
 %if _PM
	call restore_scratchsel
 %endif
	dec si
	dec si

	call isseparator?	; a separator after potential suffix ?
	lodsb
	mov bh, 0
	jne short .check_mmx_no_suffix	; no -->

	cmp al, 'Z'
	je short .getmmxlow	; 0 = ZX -->
	mov bh, 2<<3		; = 10h = low
	cmp al, 'L'
	je short .getmmxlow
	mov bh, 1<<3		; = 8h = SX
	cmp al, 'S'
	je short .getmmxlow
	cmp al, 'H'
	je short .getmmxhigh
	mov bh, 0		; 0 = ZX
.check_mmx_no_suffix:
	 dec cx			; length of name (if no suffix follows)
	call isseparator?	; immediately a separator (but no suffix) ?
	jne short .notmmx	; no -->
				; yes, (default to) zero-extending full reg
.getmmxlow:
 .getmmxlow_modrm: equ $+2	; (opcode adjusted for the right MMX reg)
	movd dword [mmxbuff], mm0
	or ah, bh

.mmxcommon:
	add ah, 2
	mov ch, 4
	xchg cl, ch
	mov bx, mmxbuff
	mov byte [bp+?fieldtype], ah
.return_success_var32_set_dx_j:
	jmp .return_success_var32_set_dx
__CPU__

.notmmx:
%endif

.return_failure:
	pop si			; ?startpointer
	pop ax			; ?length_to_add
	stc
	jmp .return_ax_frame_lodsb


%if _MMXSUPP
[cpu 586]
.getmmxhigh:
	sub sp, byte 8
	or ah, 3<<3		; = 18h = high
	mov bx, sp		; (ss = ds)
 .getmmxhigh_modrm: equ $+2	; (opcode adjusted for the right MMX reg)
	movq qword [bx], mm0
	pop bx
	pop bx			; discard low dword
	pop dword [mmxbuff]	; save high dword here
	jmp short .mmxcommon
__CPU__
%endif


var_ri_setup:
	lea si, [bp + ?namebuffer + 3]
	testopt [bp + ?flags], 1
	jz @F
	mov si, word [bp + ?startpointer]
	add si, word [bp + ?length_to_add]
	inc word [bp + ?length_to_add]
	push ax
	mov bl, 1		; set up 1 additional digit
	lodsb			; load what must be type letter
	jmp .expr

@@:
	push ax
	mov bl, -1
@@:
	lodsb			; al = candidate type letter, or hexit
	inc bx			; count number of hexits
	call getnyb
	jnc @B			; if was a hexit, loop -->
.expr:		; al = type letter
	call uppercase
	mov bh, al		; bh = letter
	cmp al, 'L'		; valid one ?
	je @F
	cmp al, 'O'
	je @F
	cmp al, 'S'
	je @F
	cmp al, 'P'
	jne .ret_CY_pop		; no -->
@@:				; is valid letter
	lodsb			; = candidate separator
	call isseparator?	; is it ?
	jne .ret_CY_pop		; no -->
	pop ax

	test byte [bp + ?bit0_is_return_name], 1
	jz .interrupt_no_return_name

	mov di, bx
	and di, 255		; get number of additional hexits
	add di, word [bp + ?return_name_pointer]
	add di, 3		; amount hexits + 3 + pointer
	mov byte [di], bh	; store the additional letter

.interrupt_no_return_name:
	inc ch			; increment name length
	xchg ax, bx

		; INP:	bx = interrupt number 0..255
		;	ah = 'S' for segment, 'O' for offset, 'L' for linear
 %if _PM
	 sub sp, 8
	call save_scratchsel
 %endif
	xor si, si
%if _PM
	push bx
	call setds2si		; ds => IVT
	pop bx
%else
	mov ds, si		; ds => IVT
%endif
	cmp ah, 'S'
	jne @F
	inc si
	inc si			; si = 2 (displacement to get segment)
@@:
	add bx, bx
	add bx, bx		; ds:bx -> IVT entry
	push word [bx + si + 2]
	push word [bx + si]	; get segment:offset or trash:segment
	 push ss
	 pop ds
	mov bx, interrupt_var
	pop word [bx]
	pop word [bx + 2]
				; store in variable
 %if _PM
	call restore_scratchsel
 %endif
	cmp ah, 'P'		; for pointer type ?
	je .var_size_4		; yes -->
	cmp ah, 'L'
	jne @FF
	 push cx
	xor ax, ax
	xchg ax, word [bx + 2]	; clear high word, get segment
	xor dx, dx		; dx:ax = segment
	mov cx, 4
@@:
	shl ax, 1
	rcl dx, 1
	loop @B			; shift up 4 bits
	add word [bx], ax
	adc word [bx + 2], dx	; add to dword
	 pop cx
	mov cl, 3		; 3byte variable (21 bits needed)
	db __TEST_IMM16		; (skip mov)
.var_size_4:
	mov cl, 4		; dword variable
@@:
	xor di, di		; (NC)
	retn

.ret_CY_pop:
	pop ax
.ret_CY:
	stc
	retn



%if _INPUT_FILE_BOOT || _INPUT_FILE_HANDLES
var_ysf_setup:
	xchg ax, bx			; bx = index
	mov al, byte [bp + ?namebuffer + 1]
	cmp al, 'S'
	jne @F
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	mov al, 'B'
	jnz @F
%endif
	mov al, 'H'
@@:
	cmp al, 'B'
	jne @F
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz @FF
%endif
.notysf:
	stc
	retn
@@:
%if _INPUT_FILE_HANDLES
	cmp al, 'H'
	jne short .notysf
	testopt [internalflags2], dif2_input_file
	jz .notysf
%else
	jmp .notysf
%endif
@@:
				; bx = index
%if _INPUT_FILE_HANDLES
	mov di, input_file_handles.active
				; -> active index for YHF
%endif
%if _INPUT_FILE_BOOT
 %if _INPUT_FILE_HANDLES
	cmp al, 'B'
	jne @F
 %endif
	mov di, load_input_file.active
				; -> active index for YBF
@@:
%endif
	neg bx			; -1 .. 0
	jz @F			; if 0, always valid -->
		; index was nonzero, meaning bx now holds a
		;  negative number. if [.active] is large enough,
		;  adding it to bx yields a positive number, which
		;  carries in the addition. check for that.
	add bx, word [di]
	jnc .notysf		; not valid index -->
	jmp @FF			; valid, use it
@@:
	add bx, word [di]
				; 0 means the topmost level
@@:
%if _INPUT_FILE_BOOT
 %if _INPUT_FILE_HANDLES
	cmp al, 'B'
	jne @F
 %endif

	mov ax, LOAD_INPUT_FILE_SIZE
	xchg ax, bx
	mul bx			; dx ignored, should be zero
	xchg ax, bx		; bx = offset into array
	add bx, load_input_file - LOADDATA3 + ldFATType
				; -> flag word
	jmp @FF

@@:
%endif
%if _INPUT_FILE_HANDLES
	shl bx, 1
	shl bx, 1
	shl bx, 1		; to qword array index
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
	add bx, input_file_handles + ifhFlags
				; -> flags word
%endif
@@:
	mov di, mask_ysf	; give the mask of read-only bits
	clc
	retn
%endif


	lleave ctx


		; INP:	al, si-> string
		; OUT:	CY if no valid digit 0..7
		;	NC if valid digit,
		;	 bl = 0..7
		;	 al, si-> behind digit specification
		; CHG:	bl
getstmmxdigit:
	cmp al, '('
	je .paropen
	call .isdigit?
	jc .ret
.retlodsb:
	lodsb
.ret:
	retn

.paropen:
	push ax
	push si
	lodsb
	call .isdigit?
	jc .retpop
	lodsb
	cmp al, ')'		; closing parenthesis ?
	stc
	jne .retpop		; no --> (CY)
	add sp, byte 4		; discard saved registers (NC)
	jmp short .retlodsb

.retpop:
	pop si
	pop ax
	retn

.isdigit?:
	mov bl, al
	sub bl, '0'
	cmp bl, 8		; valid digit 0..7 ? (CY if so)
	cmc			; NC if so
	retn


	usesection lDEBUG_CODE
isvariable_code_size equ $ - isvariable_code


	usesection lDEBUG_DATA_ENTRY
isvariable_data_size equ $ - isvariable_data

	numdef SHOW_ISVARIABLE_SIZE, _DEFAULTSHOWSIZE
%if _SHOW_ISVARIABLE_SIZE
 %assign CODESIZE isvariable_code_size
 %assign DATASIZE isvariable_data_size
 %warning isvariable size: CODESIZE code, DATASIZE data
%endif


	align 2, db 0
separators:	countedw 32,9,13,",L;]:)(=.",0

%ifn _EXPRESSIONS
 %error Building without the expression evaluator is not possible right now
%endif

%if _EXPRESSIONS

	align 4, db 0
hhvar:	dd 0	; left-hand operand for operator functions
	align 2, db 0
hh_depth:
	dw 0
hh_depth_of_single_term:
	dw 0
hhflag:	db 0	; &2: getdword called from hh, default to sub (precedence over 1)
		; &1: getdword called from hh, default to add
		; &4: getdword defaulted to add/sub for hh
		; (Note that during recursive getexpression calls (ie,
		;  bracket handling), hh_depth is incremented to 2 and
		;  higher. As these flags are only used with hh_depth == 1,
		;  this means inside brackets the hh defaulting is
		;  not in effect.)
hhtype:	db 0	; type info on left-hand operand for operator functions
%endif


	usesection lDEBUG_CODE

		; Get a numerical value from input line
		; INP:	al = first character
		;	si-> next character
		; OUT:	bx:dx = numerical value
		;	ah&80h = whether a pointer
		;	ah&40h = whether a signed type
		;	ah&3Fh = number of significant bits
		;	 one-based position of highest one bit if unsigned or signed but positive
		;	 one-based position of lowest one bit from the top if negative signed
		;
		; Note:	This invalidates the symbol table access slice.
getdword: section_of_function
getexpression: section_of_function
	inc word [hh_depth]
	call skipcomm0
	push cx
	push di
	lframe
	lenter

	xor cx, cx
	push cx
	lvar word, ??Count
%define lCount ???Count

		; The first number field's operator is initialized to
		; the dummy right-operand operator, which is set up with
		; the highest precedence. This means it'll be processed
		; immediately in the first iteration below.
	mov cl, OPERATOR_RIGHTOP
	push cx				; initialize dummy first number operator
	sub sp, byte 6+4
	lvar 6, ??A
%define lA ???A
	lvar 6, ??B
%define lB ???B

.loop:
		; Get next term of an expression. A term is one variable,
		; one immediate number, one expression contained within
		; round brackets or one expression used to access memory.
		; This code also parses any number of unary operators
		; (including type conversions) in front of the term.
		;
		; INP:	(si-1)-> first character
		; OUT:	(see label .term_done)
		;	bx:dx = numerical value of term
		;	ah&80h = whether a pointer
		;	ah&40h = whether a signed type
		;	al = first character behind term
		;	si-> line
		; CHG:	di, cx
		;
		; This part might be simplified by directly modifying 6byte[bp+lB]
		; instead of setting bx:dx and ah. (The current interface is one of
		; the holdovers of getexpressionterm as a separate function.) In
		; that case, bx:dx and ah presumably will then be simply added to
		; the CHG specification.
.term:
	dec si
	push si			; -> term

		; count unary operators and type specifiers,
		;  get the bit mask of required bytes
		;  and skip past the operators and specifiers
	call count_unary_operators

%if _INDIRECTION
	cmp al, '['
	je .indirection		; handle indirected value -->
%endif
	cmp al, '('
	je .parens		; handle term with precedence -->

	push dx			; remember the count+1
				; (only if no indirection or bracket)

	dec si
	mov dx, msg.value	; does a VALUE keyword for VALUE x IN y go here ?
	call isstring?
	je .value_in		; yes -->

	mov dx, msg.linear	; does a LINEAR keyword go here ?
	call isstring?
	je .linear		; yes -->

%if _PM
	mov dx, msg.desctype
	call isstring?
	je .desctype		; yes -->
%endif

	lodsb

	call isvariable?	; is it a variable ?
	jc .check_literal_or_symbol
				; no, may be an immediate value or symbol -->

.variable:
	xchg bx, dx		; bx-> high word
	mov ah, cl
	xor cx, cx
	cmp ah, 2
	jbe .variable_nohigh
	mov cx, word [bx]	; get high word
	cmp ah, 3
	ja @F
	mov ch, 0
@@:
.variable_nohigh:
	xchg bx, dx		; bx-> low word
	mov dx, word [bx]	; get low word
	mov bx, cx		; high word/zero
	cmp ah, 1
	ja .variable_notbyte
	mov dh, 0		; limit to byte
.variable_notbyte:
.symbol:
	mov ah, 0		; type pointer=0 signed=0
	jmp .term_end


.check_literal_or_symbol:
%if _SYMBOLIC
	call issymbol?		; is it a symbol ?
	jnc .symbol		; yes -->
				; no, must be an immediate value
%endif

.literal:
	xor bx, bx		; (in case of decimal base shortcut:
	mov dx, 10		;   set base: decimal)
	cmp al, '#'		; shortcut change to decimal base, or string literal?
	jne .lithex_common
	cmp byte [si], '"'
	je .lit_string
	cmp byte [si], "'"
	je .lit_string
	jmp .lit_base		; decimal base -->

.lit_string:
	xor dx, dx
	lodsb
	xchg al, ah

	call @F
	mov dl, al
	call @F
	mov dh, al
	call @F
	mov bl, al
	call @F
	mov bh, al
	lodsb
	cmp al, ah
	je @FFF
.err3:
	jmp .err2

@@:
	lodsb
	cmp al, ah
	je @F
	call iseol?.notsemicolon
	je .err3
.lit_string_retn:
	retn

@@:
	lodsb
	cmp al, ah
	je .lit_string_retn
	pop cx			; (discard near return address)
	dec si
@@:
	lodsb
	jmp .lit_end_string


.lithex_common:
	call .lit_ishexdigit?	; the first character must be a digit then
	jc .err2
	xor dl, dl		; initialize value
.lithex_loopdigit:
	cmp al, '_'
	je .lithex_skip
	call .lit_ishexdigit?	; was last character ?
	jc .lit_end		; yes -->
	test bh, 0F0h		; would shift bits out ?
	jnz .err2
	call uppercase
	sub al, '0'
	cmp al, 9		; was decimal digit ?
	jbe .lithex_decimaldigit; yes -->
	sub al, 'A'-('9'+1)	; else adjust for hexadecimal digit
.lithex_decimaldigit:
	mov cx, 4
.lithex_loopshift:
	shl dx, 1
	rcl bx, 1
	loop .lithex_loopshift	; *16
	or dl, al		; add in the new digit
.lithex_skip:
	lodsb
	jmp short .lithex_loopdigit

.lit_end:
	cmp al, '#'		; base change specification?
	je .lit_base		; yes -->
.lit_end_string:
	call isseparator?	; after the number, there must be a separator
	jne .err2		; none here -->
	mov ah, 0		; type pointer=0 signed=0
	jmp .term_end		; okay -->
.lit_base:
	test bx, bx		; insure base <= 36
	jnz .err2
	cmp dx, byte 36
	ja .err2
	cmp dx, byte 2		;  and >= 2
	jb .err2		; otherwise error -->

	lodsb
	cmp dl, 16		; hexadecimal ?
	je .lithex_common	; yes, use specific handling -->

	mov di, dx		; di = base
	mov cl, dl
	add cl, '0'-1
	cmp cl, '9'
	jbe .lit_basebelow11
	mov cl, '9'
.lit_basebelow11:		; cl = highest decimal digit for base ('1'..'9')
	mov ch, dl
	add ch, 'A'-10-1	; ch = highest letter for base ('A'-x..'Z')

	call .lit_isdigit?	; first character must be a digit
	jc .err2
	xor dx, dx		; initialize value
.lit_loopdigit:
	cmp al, '_'
	je .lit_skip
	call .lit_isdigit?	; was last character ?
	jc .lit_end		; yes -->
	call uppercase
	sub al, '0'
	cmp al, 9		; was decimal digit ?
	jbe .lit_decimaldigit	; yes -->
	sub al, 'A'-('9'+1)	; else adjust for hexadecimal digit
.lit_decimaldigit:
	push ax
	mov ax, dx
	push bx
	mul di			; multiply low word with base
	mov bx, dx
	mov dx, ax
	pop ax
	push dx
	mul di			; multiply high word with base
	test dx, dx
	pop dx
	jnz .err2		; overflow -->
	add bx, ax		; add them
	pop ax
	jc .err2		; overflow -->
	add dl, al		; add in the new digit
	adc dh, 0
	adc bx, byte 0
	jc .err2		; overflow -->
; The value 3#102002022201221111210 is exactly equal to ffffFFFFh
;  so 3#102002022201221111211 (or ...2) overflows 32 bits and
;  needs to be detected here when adding in the as yet least
;  significant digit. This is why this jump is needed.
; This also applies to decimal #4294967296.

.lit_skip:
	lodsb
	jmp short .lit_loopdigit

.err2:
	jmp error


%if _PM
.desctype:
	pop ax			; discard unary operators counter

%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_DESCTYPE_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_DESCTYPE_STACK_CHECK
	call stack_check	; abort if deep recursion
%endif
	test ax, msg.stack_overflow.desctype

	lodsb
	call ispm
	jnz short .err2
	call skipwh0
	call getword
_386	push edx
_386	pop dx
_386	lar edx, edx
_386	jz @F
_386	xor edx, edx
@@:
_386	shr edx, 8
_386	push dx
_386	pop edx
subcpu 286
_no386	lar dx, dx
_no386	jz @F
_no386	xor dx, dx
@@:
_no386	xchg dl, dh
subcpureset
	xor bx, bx
	call skipcomm0
	mov ah, 0		; type pointer=0 signed=0
	jmp .term_end_recount
%endif


.linear:
	pop ax			; discard unary operators counter

%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_LINEAR_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_LINEAR_STACK_CHECK
	call stack_check	; abort if deep recursion
%endif
	test ax, msg.stack_overflow.linear

	call skipwhite
_386_PM	push word [bAddr32]
_386_PM	push edx
_386_PM	pop dx
	mov bx, word [reg_ds]
	call getlinearaddr
_386_PM	push dx
_386_PM	pop edx
_386_PM	pop word [bAddr32]
	jc .err2

	_386_PM_o32	; or dword
	or word [bp_offset], strict byte -1
				; do not use a preferred offset

	call skipcomm0
	mov ah, 0		; type pointer=0 signed=0
	jmp .term_end_recount


.value_in:
	pop ax			; discard unary operators counter

%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_VALUE_IN_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_VALUE_IN_STACK_CHECK
	call stack_check	; abort if deep recursion
%endif
	test ax, msg.stack_overflow.value_in

	call skipwhite
	dec si
	mov dx, msg.executing
	call isstring?
	jne @F
	call skipwhite
	push si
	mov si, msg.executing_value_range
	call get_value_range
	pushf			; ! preserve CF
	call chkeol
	popf
	pop si
	dec si			; preserves CF
	lodsb
	jmp @FF

@@:
	call get_value_range	; OUT:	cx:di = from, bx:dx = to
@@:

		; If the VALUE is a not normal range (zero length
		;  or from > to) then we still have to parse the
		;  remaining input line to find the end of the
		;  VALUE x IN y construct. So we do this with a
		;  sort of hack: we take the invalid / unset
		;  range values, do all the (invalid) comparisons,
		;  but do not count any of the possible matches.
		; Look at .value_in.found for this hack.
	lframe none, nested
	lenter
	lvar word, bit0_do_not_match
	 pushf
	lvar dword, value_from
	 push cx
	 push di
	lvar dword, value_to
	 push bx
	 push dx
	xor cx, cx
	lvar dword, found
	 push cx
	 push cx

	push cx			; zero terminator of keyword remembered offsets
	dec si
	mov dx, msg.in
	call isstring?
.err3_NZ:
.err4_NZ:
	jne .err2

.value_in.loop:
	call skipwhite
	dec si
	mov bx, msgtable_value_range
				; -> table

@@:
	mov dx, word [bx]
	test dx, dx
	jz @F			; last checked, go get range -->
	call isstring?
	je .value_in.keyword	; found a keyword -->
	add bx, 4		; go to next table entry
	jmp @B

.value_in.keyword:
%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_VALUE_IN_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_VALUE_IN_STACK_CHECK
	call stack_check	; abort if deep recursion
%endif
	test ax, msg.stack_overflow.value_in

	call skipwhite		; skip past blanks
	push si			; remember offset after keyword
	mov si, word [bx + 2]	; -> keyword replacement text
	jmp .value_in.loop	; handle replacement -->

@@:
	call get_value_range	; OUT:	cx:di = from, bx:dx = to
	jc .value_in.next

		; ! (StartVal > EndMatch)
	cmp word [bp + ?value_from + 2], bx
	jne @F
	cmp word [bp + ?value_from], dx
@@:
	ja .value_in.next

		; ! (EndVal < StartMatch)
	cmp word [bp + ?value_to + 2], cx
	jne @F
	cmp word [bp + ?value_to], di
@@:
	jb .value_in.next

.value_in.found:
	mov cx, word [bp + ?bit0_do_not_match]
	not cx
	and cx, 1
	add word [bp + ?found], cx
	adc word [bp + ?found], 0

.value_in.next:
@@:
	call skipwh0
	cmp al, ','
	je .value_in.loop

	pop cx			; get next remembered offset
	jcxz @F			; if none -->
	mov si, cx		; -> behind keyword
	dec si
	lodsb			; reload al
	jmp @B			; continue after the keyword -->

@@:
	pop dx
	pop bx			; pop ?found counter variable
	lleave

	mov ah, 0		; type pointer=0 signed=0
	jmp .term_end_recount


%if _INDIRECTION
.indirection:
	call stack_check_indirection
				; abort if deep recursion
	test ax, msg.stack_overflow.indirection

_386_PM	push word [bAddr32]
_386_PM	push edx
_386_PM	pop dx

	call skipcomma		; also skips the '[' in al
	mov bx, word [reg_ds]	; default segment/selector
	push cx			; save previous bit mask (ch)
	call getaddrX		; (recursively calls getexpression:)
	pop cx
	mov cl, 0
	cmp al, ']'		; verify this is the closing bracket
.err5_NZ:
	jne .err4_NZ		;  if not -->
	lodsb			; get next character
		; bx:(e)dx-> data
		; ch = bit mask of required bytes
		; cl = 0

..@hh_indirection_memory_access_start:
	push ax
	push bp
	xor bp, bp
	push bp
	push bp
	mov bp, sp		; -> buffer

	call prephack
	call dohack
			; Regarding how this loop handles cx,
			;  remember that ch holds the flags for
			;  the required bytes.
			;  And cl is initialised to 0. In
			;  each iteration, cl is incremented.
			;  The loop instruction then decrements
			;  cl again, but the entire cx is only
			;  zero if no more bytes are required.
			; (This hack saves a single byte over
			;  the "test ch, ch \ jnz" alternative
			;  but it isn't very pretty. It
			;  also probably slows down a bit.)
.indirection_loop:
	shr ch, 1		; need to read this byte ?
	jnc .indirection_skip	; no -->
	call readmem		; else read byte
	mov byte [bp+0], al	; store byte
.indirection_skip:
	inc cx			; = 1 if no more to read
	inc bp			; increase buffer pointer
	_386_PM_o32		; inc edx
	inc dx			; increase offset
	loop .indirection_loop	; read next byte if any -->
	call unhack

	pop dx
	pop bx
	pop bp
	pop ax
..@hh_indirection_memory_access_end:

_386_PM	push dx
_386_PM	pop edx
_386_PM	pop word [bAddr32]
	mov ah, 0		; type pointer=0 signed=0
	jmp short .term_end_recount
%endif


.parens:
%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_PARENS_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_PARENS_STACK_CHECK
	call stack_check	; abort if deep recursion
%endif
	test ax, msg.stack_overflow.parens

	lodsb
	call getexpression	; (recursive) bx:dx = number, ah = type
	cmp al, ')'		; closing parens ?
	jne .err5_NZ		; no -->
	lodsb

.term_end_recount:
	db __TEST_IMM16		; skip pop, stc; NC
.term_end:
	pop cx			; get count+1 of unary operators and type specifiers
	stc
	pop di			; -> term
	xchg si, di
	push di			; save -> behind
		; si-> unary operators and types
	jc .unary_processnext	; if we preserved the count -->

	push bx
	push dx
	push si
	call count_unary_operators
	mov cx, dx		; get count+1 again
	pop si
	pop dx
	pop bx

.unary_processnext:
	loop .unary_doprocess
	pop si			; -> behind term
	 dec si			; -> character to reload in skipwhite
	jmp short .term_done

.unary_doprocess:
	push si
	push cx
	push bx
	push dx

	mov di, cx		; count+1 of operators to skip
	call count_unary_operators_restrict	; skip them
	jnz .err		; if not enough --> (?!)

	call istype?		; get type info if it's a type
	jc .unary_processnotype	; isn't a type -->
	xchg al, ah		; al = type input
	shr bx, 1		; CF = signedness
	lahf			; with CF = signedness
	mov cx, word [ typehandlers + bx ]	; function
.unary_processcall:
	pop dx
	pop bx
	call cx			; call type or unary operator handler
				;  bx:dx = output number, ah = type
	pop cx			; restore processing counter
	pop si			; restore ->term
	jmp short .unary_processnext

.unary_processnotype:
	call isunaryoperator?	; get unary operator index
	jne .err		; if no unary operator --> (?!)
	mov bx, cx
	shl bx, 1
	mov cx, word [ unaryoperatorhandlers + bx ]
	jmp short .unary_processcall

.err:
	jmp error

.term_done:	; bx:dx = number, ah = type
		; get the operator following this number
	call skipwhite
	mov word [bp+lB+0], dx
	mov word [bp+lB+2], bx		; store numeric value
	mov bx, word [hh_depth_of_single_term]
	cmp bx, word [hh_depth]
	je .operator_invalid
	call isoperator?		; cl = operator index (if any)
	je .operator_apparently_valid
	call iseol?			; end of line follows ?
	je .operator_invalid
.hh_twofold_check:
		; If we are processing a ?? :: conditional
		;  construct then we want the :: to end
		;  the parsing of the expression at that
		;  point, to then execute the OPERATOR_COND
		;  function of_cond. That means here we
		;  want to not apply the hh twofold case.

		; The next check is for an ?? :: construct
		;  if the :: happens at hh depth == 1 and
		;  there is no nested ?? :: construct.
	cmp byte [bp + lA + 4], OPERATOR_COND
	je .operator_invalid		; special check for conditional operator

		; The next check is for a nested ?? ::
		;  construct, or if the clause between
		;  ?? and :: contains any other operators
		;  than ??. (All operators have higher
		;  precedence than ??.)
		; If any ?? is waiting for its :: then we
		;  must accept the next :: as an invalid
		;  operator, thus not as a separator that
		;  activates the hh twofold operation.
	mov cx, word [bp + lCount]	; = how many operators waiting
	jcxz @FF			; if none waiting --> (do hh twofold)
	mov bx, sp			; -> first waiting operator on stack
@@:
	cmp byte [bx], OPERATOR_COND	; is it a conditional operator ?
	je .operator_invalid		; yes, treat as invalid operator -->
					;  (it must be a :: to be valid)
	add bx, 6			; -> next waiting operator on stack
	loop @B				; loop if any waiting operator left -->
@@:
	cmp word [hh_depth], 1		; are we in first level expression ?
	jne .operator_invalid		; no, do not do special H operation -->
	test byte [hhflag], 1|2		; special H operation requested ?
	jz .operator_invalid		; no -->
	mov bl, OPERATOR_MINUS		; assume it's sub
	test byte [hhflag], 2		; sub requested by H ?
	jnz .hh_twofold_found		; yes -->
	dec bx				; else it must be add
%if (OPERATOR_MINUS - 1) != OPERATOR_PLUS
 %error Remove optimisation
%endif
.hh_twofold_found:
	or byte [hhflag], 4		; set flag for H twofold operation
	call skipcomm0
	jmp short .operator_done	; return this -->

.operator_apparently_valid:
	mov bx, cx
	add bx, bx
	call [operatordispatchers + bx]
	test bl, bl			; valid ?
	jz .hh_twofold_check		; no, check for H twofold operation -->
	call skipwhite
	db __TEST_IMM16
.operator_invalid:
	xor bx, bx			; bl = 0 (no operator)
.operator_done:
	mov bh, ah			; bh = type info
	mov word [bp+lB+4], bx		; store type and following operator

%if _EXPRESSION_DEBUG
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	lea bx, [bp + lA]
	mov dx, .msgA
	call .debugdump

	lea bx, [bp + lB]
	mov dx, .msgB
	call .debugdump

	pop di
	pop si
	push si
	push di
	call .debugline
	jmp .debugend

.debugline:
	call .debugpad
	mov dx, .msg_end1
	call putsz

	dec si
	mov dx, si
@@:
	lodsb
	call iseol?
	jne @B
	mov cx, si
	dec cx
	sub cx, dx
	call puts

	mov dx, .msg_end2
	call putsz
	retn

	usesection lDEBUG_DATA_ENTRY

.msgB:		asciz "B = "
.msgA:		asciz "A = "
.msg@:		asciz "@ = "
.msgPop:	asciz "Pop = "
.msg_opA:	asciz "(A) "
.msg_opB:	asciz " op (B)",13,10
.msg_none:	asciz "none"
.msg_op:	asciz ", "
.msg_end:	db " op"
.msg_linebreak:	asciz 13,10
.msg_end1:	asciz '"'
.msg_end2:	asciz '"',13,10,13,10
.msg_looping_cond:
		asciz "Looping for OPPREC_COND",13,10

	usesection lDEBUG_CODE

.debugpad:
	mov cx, word [hh_depth]
	dec cx
	jz @FF
@@:
	mov al, 32
	call putc
	loop @B
@@:
	retn

.debugdump:
	call .debugpad
	call putsz

	cmp byte [bx + 4], OPERATOR_RIGHTOP
	jne @F
	mov dx, .msg_none
	call putsz
	jmp .debugdump_none

@@:
	sub sp, 8
	mov di, sp
	mov ax, word [bx + 2]
	call hexword
	mov ax, word [bx]
	call hexword
	mov cx, di
	mov di, sp
@@:
	cmp byte [di], '0'
	jne @F
	inc di
	cmp cx, di
	ja @B
	dec di
@@:
	sub cx, di
	mov dx, di
	push bx
	call puts
	pop bx
	add sp, 8

.debugdump_none:
	mov dx, .msg_op
	call putsz
	mov si, word [bx + 4]
	and si, 00FFh
	add si, si
	mov dx, word [operatornames + si]
	call putsz
	mov dx, .msg_end
	call putsz
	retn

.debugend:
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
%endif

	mov cl, byte [bp+lA+4]
	call .compare_operators		; (cmp bl, cl = cmp Boprtr, Aoprtr)
	jb .high_precedence_A		; compute the first operand first -->
					;  (jump taken for invalid Boprtr too)
	jne @F
		; If we get the same operator precedence for
		;  Boprtr and Aoprtr we generally want to
		;  handle this as a high-precedence A.
		; This is not true for operator cond. If
		;  an expression like this is parsed:
		;  Acond ?? Bcond ?? Btrue :: Bfalse :: Afalse
		; Then we get first:
		;  A = none, right op
		;  B = Acond, cond op
		; After the right op is processed:
		;  A = Acond, cond op
		;  B = Bcond, cond op
		; Next we want:
		;  @ = Acond, cond op
		;  A = Bcond, cond op
		;  B = Btrue, invalid op (::)
		; If we did high precedence A here instead
		;  we would get:
		;  A = (Acond) cond op (Bcond)
		; At this point of_cond would parse "Btrue..."
		;  as the :: clause, failing because it doesn't
		;  start with a "::".
	cmp bx, OPPREC_COND
	jne .high_precedence_A

@@:
	inc word [bp+lCount]		; increase loop count
	push word [bp+lA+0]
	push word [bp+lA+2]
	push word [bp+lA+4]		; push A and its operator

%if _EXPRESSION_DEBUG
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	lea bx, [bp + lA]
	mov dx, .msg@
	call .debugdump
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
%endif

	push word [bp+lB+4]
	push word [bp+lB+2]
	push word [bp+lB+0]
	pop word [bp+lA+0]
	pop word [bp+lA+2]
	pop word [bp+lA+4]		; set A to B, including operator

%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_PRECEDENCE_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_PRECEDENCE_STACK_CHECK
	call stack_check		; abort if deep recursion
%endif
	test ax, msg.stack_overflow.precedence

d4	call d4message
d4	asciz "getexpression: Entering loop/recursion",13,10

.loop_j:
	jmp .loop			; start again (former B as first term) -->

.cont:

d4	call d4message
d4	asciz "getexpression: End of loop/recursion",13,10

	push word [bp+lA+4]
	push word [bp+lA+2]
	push word [bp+lA+0]
	pop word [bp+lB+0]
	pop word [bp+lB+2]
	pop word [bp+lB+4]		; set B to A, including operator

	pop word [bp+lA+4]
	pop word [bp+lA+2]
	pop word [bp+lA+0]		; pop A and its operator

%if _EXPRESSION_DEBUG
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	lea bx, [bp + lA]
	mov dx, .msgPop
	call .debugdump
	lea bx, [bp + lB]
	mov dx, .msgB
	call .debugdump

	pop di
	pop si
	push si
	push di
	call .debugline

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
%endif

.high_precedence_A:

%if _EXPRESSION_DEBUG
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	call .debugpad
	mov dx, .msg_opA
	call putsz
	lea bx, [bp + lA]
	mov si, word [bx + 4]
	and si, 00FFh
	add si, si
	mov dx, word [operatornames + si]
	call putsz
	mov dx, .msg_opB
	call putsz

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
%endif

	mov cx, word [bp+lA+4]		; retrieve A's type info and operator
	push word [bp+lA+2]
	push word [bp+lA+0]
	 mov ax, word [bp+lB+4]		;  retrieve B's type info and operator
	  mov byte [hhtype], ch		;   set type info
	pop word [hhvar]
	  mov ch, 0			;   cx = A's 1-based operator index
	pop word [hhvar+2]		; retrieve A's number
	 mov dx, word [bp+lB+0]
	  mov di, cx
	 mov bx, word [bp+lB+2]		;  retrieve B's number
	  add di, di			;   = offset into dispatch table
	 push ax			;  preserve B's operator
	call near [operatorfunctions+di]; compute: (A) operatorA (B)
	 pop cx				; cl = B's operator

	mov word [bp+lA+0], dx
	mov al, cl			; B's operator
	mov word [bp+lA+2], bx
	mov word [bp+lA+4], ax		; store result in A, with B's operator

%if OPERATOR_INVALID != 0
 %error Remove optimisation
%endif
	test al, al			; (previous B's) operator valid ?
	jz .end				; no, end of sequence -->

	cmp word [bp+lCount], byte 0	; in recursion ?
	je .loop_j			; no, loop -->

	pop bx
	push bx				; retrieve saved ('@') operator

	call .compare_operators		; (cmp bl, cl = cmp @oprtr, Aoprtr)
	jb .loop_j			; A's operator's precedence higher -->

%if _EXPRESSION_DEBUG
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	cmp bx, OPPREC_COND
	jne @F
	mov dx, .msg_looping_cond
	call putsz
@@:

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
%endif

	cmp bx, OPPREC_COND
	je .loop_j

d4	call d4message
d4	asciz "getexpression: Loop/recursion found to be not necessary anymore",13,10

;	dec word [bp+lCount]
;	jmp .cont			; return to previous level -->

.end:
	dec word [bp+lCount]		; decrease loop count
	jns .cont			; process next operand from stack -->

	mov dx, word [bp+lA+0]
	mov bx, word [bp+lA+2]		; retrieve A
	mov ax, word [bp+lA+4]		;  (discard (invalid) operator)

	lleave code			; remove the stack frame

	pop di
	pop cx				; restore registers

	dec word [hh_depth]
	 dec si
	 lodsb				; (restore al)

.countsignificantbits:
	push cx
	push dx
	push bx
	mov cx, 1
	 push bx
	test ah, 40h			; signed type ?
	jz .unsigned			; no -->
	test bh, 80h			; negative value ?
	jz .unsigned			; no -->
.signed:
	and bx, dx
	inc bx				; = 0 if -1 (all bits set)
	 pop bx
	jz .done			; is -1, 1 significant bit -->
	mov cl, 32+1+1			; number of significant bits is 1 + 1-based index of highest clear bit
.signedloop:
	shl dx, 1
	rcl bx, 1			; shift up the number
	dec cx				; maintain index
	jc .signedloop			; still a set bit -->
	jmp short .done
.unsigned:
	or bx, dx			; = 0 if 0 (all bits cleared)
	 pop bx
	jz .done			; is 0, 1 significant bit -->
	mov cl, 32+1			; number of significant bits is 1-based index of highest set bit
.unsignedloop:
	shl dx, 1
	rcl bx, 1
	dec cx
	jnc .unsignedloop
	test ah, 40h			; positive signed value ?
	jz .done			; no -->
	inc cx				; then the following zero bit is required too
.done:
	and ah, 1100_0000b
		; insure we only pass the top two type bits
	or ah, cl
		; low 6 bits = number of significant bits
	pop bx
	pop dx
	pop cx
	retn


		; INP:	bl = operator index 1
		;	cl = operator index 2
		; OUT:	flags as for "cmp precedence1, precedence2"
		; CHG:	bx, cx
.compare_operators:
	call .getprecedence
	call .getprecedence
	cmp bx, cx
	retn

.getprecedence:
	xor bh, bh
	mov bl, byte [operatorprecedences+bx]
	xchg bx, cx
	retn

getexpression.lit_ishexdigit?:
	mov cx, "9F"
getexpression.lit_isdigit?:
	cmp al, '0'
	jb .no
	cmp al, cl
	jbe .yes
	push ax
	call uppercase
	cmp al, ch
	ja .no_p
	cmp al, 'A'
	jb .no_p
	pop ax
.yes:
	clc
	retn

.no_p:
	pop ax
.no:
	stc
	retn


of_cond:
	push ax			; second operand type
	dec si
	lodsw
	cmp ax, "::"
	jne short .error
%if _EXPRESSION_INDIRECTION_STACK_CHECK == _EXPRESSION_COND_STACK_CHECK
	call stack_check_indirection
%else
	mov ax, _EXPRESSION_COND_STACK_CHECK
	call stack_check	; abort if deep recursion
%endif
	test ax, msg.stack_overflow.cond

	mov ax, word [hhvar]
	or ax, word [hhvar + 2]	; ax = flag (zero if to take third operand)
	push bx
	push dx
	push ax			; preserve stack
	lodsb
	call getexpression	; parse third operand
	call skipcomm0		; allow comma afterwards
	pop cx
	jcxz @F			; if to take third operand -->
	pop dx
	pop bx			; take second operand
	mov cl, al
	pop ax			; ah = second operand type
	mov al, cl		; preserve al
	jmp @FF

@@:
	pop cx
	pop cx			; discard second operand value
	pop cx			; discard second operand type
@@:
	pop cx			; discard near return address
	pop cx			; discard cx on stack

	mov byte [bp+lA+4], OPERATOR_RIGHTOP
	dec si
	jmp getexpression.term_done

.error:
	jmp error

	lleave ctx


		; INP:	si-> possible unary operators
		; OUT:	dx = 1 + count of unary operators
		;	al, si-> behind identified unary operators
		;	ch = bit mask of required bytes,
		;	 bits 0..3 represent one byte of a dword each
		;	 bits 4..7 are clear
		; CHG:	bx, ch, di
		;
		; Type specifications are parsed as unary operators
		; here. (Elsewhere, "unary operators" refers only to
		; the unary operators specified as one of "+-~!?".)
count_unary_operators:
	xor di, di
		; INP:	si-> possible unary operators
		;	di = maximum count+1 of unary operators to process,
		;	      zero means unlimited
		; OUT:	dx = 1 + count of unary operators,
		;	      at most di
		;	al, si-> behind identified unary operators
		;	ch = bit mask of required bytes,
		;	 bits 0..3 represent one byte of a dword each
		;	 bits 4..7 are clear
		;	ZR if maximum reached
		;	NZ if maximum not reached
		; CHG:	bx, ch
count_unary_operators_restrict:
	mov ch, 1111b		; default to access a full dword
	xor dx, dx		; initialize counter to zero
	db __TEST_IMM8		; skip pop
.loop:
	pop di			; get maximum count
	inc dx			; count unary operators and type specifiers
	push di			; save maximum count again
	call skipwhite		; load next character and skip blanks
	cmp dx, di		; reached maximum ?
	je .end			; yes --> (ZR)
	 push cx
	call istype?		; check for type and if so retrieve info
	jc .notype		; not a type -->
	 pop cx

	shr bx, 1		; discard signedness bit
	mov si, di		; -> behind the type specifier
	mov cl, 01Fh		; prepare shift count register
	and cx,[typebitmasks+bx]; apply mask and get shift count register
	shl ch, cl		; apply shift
	jmp short .loop		; check for more -->

.notype:
	call isunaryoperator?	; is it a unary operator?
	 pop cx
	je .loop		; yes, check for more -->
				; (NZ)
.end:
	pop di			; discard
	retn


get3byte.checksignificantbits:
	push ax
	and ah, 3Fh
	cmp ah, 24
	jmp checksignificantbitscommon

getword: section_of_function
	push bx
	call getexpression
	pop bx
.checksignificantbits:
	push ax
	and ah, 3Fh
	cmp ah, 16
	jmp checksignificantbitscommon

getbyte:
	push bx
	push dx
	call getexpression
	pop bx
	mov dh, bh
	pop bx
.checksignificantbits:
	push ax
	and ah, 3Fh
	cmp ah, 8
checksignificantbitscommon:
	pop ax
	ja short errorj6	; if error
	retn

errorj6:
	jmp error


;	GETNYB - Convert the hex character in AL into a nybble.  Return
;	carry set in case of error.

getnyb:
	push ax
	sub al, '0'
	cmp al, 9
	jbe .return		; if normal digit
	pop ax
	push ax
	call uppercase
	sub al, 'A'
	cmp al, 'F'-'A'
	ja .error		; if not A..F
	add al, 10
.return:
	inc sp			; normal return (first pop old AX)
	inc sp
	clc
	retn
.error:
	pop ax			; error return
	stc
	retn


		; INP:	si -> input line
		; OUT:	NC if normal range (nonzero, upper >= lower),
		;	 bx:dx = TO value (upper bound)
		;	 cx:di = FROM value (lower bound)
		;	CY if not normal,
		;	 bx:dx and cx:di may be invalid
		;	 ZR if FROM LENGTH with zero length, cx:di valid
		;	al = first character
		;	si -> next character
		;	jumps to error if invalid input
get_value_range:
	call skipwhite
	dec si
	mov dx, msg.from
	call isstring?
	lodsb
	jne .not_from

.from:
	call getexpression	; (recursive)
	mov di, dx
	mov cx, bx		; cx:di = from

	dec si
	mov dx, msg.to
	call isstring?
	je .from_to
	mov dx, msg.length
	call isstring?
	jne short .error

.from_length:
	lodsb
	push cx
	call get_length		; (recursive call to getexpression)
	pop cx
	test bx, bx
	jnz @F
	test dx, dx
	jnz @F
				; ZR = length is zero, cx:di = from
.notnormal:
	stc
	retn

.error:
	jmp error

@@:
				; bx:dx = length
	add dx, di
	adc bx, cx		; bx:dx = from + length
	sub dx, 1
	sbb bx, 0		; bx:dx = from + length - 1
	jmp @F

.from_to:
	lodsb

	call getexpression	; (recursive)
				; bx:dx = to
				; cx:di = from
	jmp @F

.not_from:
	call getexpression	; (recursive)
	mov di, dx
	mov cx, bx		; bx:dx = cx:ax = value

@@:
	cmp bx, cx
	jne @F
	cmp dx, di
@@:
	jb .notnormal		; NZ = length overflow or not normal FROM TO
	retn			; (NC)


stack_check_indirection:
	mov ax, _EXPRESSION_INDIRECTION_STACK_CHECK

		; INP:	ax = how much stack should be left
		;	word [cs:ip + 1] = message for location
		; OUT:	doesn't return if stack overflow
		; CHG:	ax
		; STT:	ds = ss
stack_check:
%if _SYMBOLIC
	call .internal
%else
	add ax, stack
	cmp sp, ax
%endif
	jb @F
	retn

@@:
	pop ax

	push ss
	pop es
	mov sp, [throwsp]

	mov di, msg.stack_overflow.caller
	call hexword
	mov dx, msg.stack_overflow
	call putsz
	xchg ax, bx
	mov dx, [cs:bx + 1]
	call putsz

	jmp near [errret]


%if _SYMBOLIC
		; INP:	ax = how much stack should be left
		; OUT:	CY if stack overflow
		; CHG:	ax
		; STT:	ds = ss
.internal:
	add ax, word [stack_low_address]
	cmp sp, ax
	retn
%endif

%if _PM
		; INP:	ss:sp -> 8-byte save area
		; OUT:	in PM: save area filled
		;	save area left on stack
		; CHG:	-
		; STT:	es = ds = debugger data selector
save_scratchsel:
	lframe near
	lpar qword, savearea
	lpar_return
	lenter
	call ispm
	jnz .ret
	push ax
	push bx
	_386_o32
	push di
_386	xor edi, edi
	lea di, [bp + ?savearea]
	mov bx, word [scratchsel]
	mov ax, 000Bh
	int 31h			; get descriptor
	_386_o32
	pop di
	pop bx
	pop ax
.ret:
	lleave
	lret

		; INP:	ss:sp -> 8-byte save area
		; OUT:	in PM: save area used
		;	save area popped from stack
		; CHG:	-
		; STT:	es = ds = debugger data selector
restore_scratchsel:
	lframe near
	lpar qword, savearea
	lenter
	call ispm
	jnz .ret
	push ax
	push bx
	_386_o32
	push di
_386	xor edi, edi
	lea di, [bp + ?savearea]
	mov bx, word [scratchsel]
	mov ax, 000Ch
	int 31h			; set descriptor
	_386_o32
	pop di
	pop bx
	pop ax
.ret:
	lleave
	lret
%endif
