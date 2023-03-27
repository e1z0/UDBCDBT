
%if 0

lDebugX PM initialisation

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2021 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

		; Int2F handler. Starts with an IISP header.
	align 2, db 0
iispentry debug2F, 0, debug2F
oldi2F:		equ debug2F.next
	pushf
	cmp ax, 1687h
dpmidisable:		; set this byte to __TEST_IMM8 to disable new DPMI entry
	je short dpmiquery
	popf
jumpoldi2F:
	jmp far [ cs:oldi2F ]

dpmiquery:
	push cs
	call jumpoldi2F
	test ax, ax
	jnz .nohost

	mov word [ cs:dpmientry+0 ], di
	mov word [ cs:dpmientry+2 ], es
	mov di, mydpmientry
	push cs
	pop es
.nohost:
	iret

mydpmientry:
	mov byte [ cs:dpmi32 ], 0
	test al, 1
	jz .16
	inc byte [ cs:dpmi32 ]
.16:
	call far [ cs:dpmientry ]
	jnc installdpmi
debug2F.hwreset:
	retf


subcpu 286
installdpmi:
	pusha
	mov bp, sp		; [bp+16]=ip, [bp+18]=cs
	pushf
	push ds
	push es

	mov bx, cs
	mov ax, 0006h
	int 31h
	jc .fataldpmierr	; get base address of cs
	test dl, 15
	jnz .fataldpmierr
	test cx, 0FFF0h
	jnz .fataldpmierr
	shr dx, 4
	shl cx, 12
	or dx, cx
	cmp word [cs:pspdbg], dx
	jne .fataldpmierr

	mov bx, cs
	mov ax, 000Ah		; get a data descriptor for DEBUG's segment
	int 31h
	jc .fataldpmierr
	mov ds, ax
	mov word [ cssel ], cs
	mov word [ dssel ], ax

	clropt [internalflags3], dif3_ss_b_bit_set
	mov bx, ax		; bx = debugger ss/ds selector
	lar cx, bx		; ch = access rights
	jnz .fataldpmierr
	shr cx, 8		; get format for 31.0009 call
	testopt [options3], opt3_ss_b_bit_set
	jz @F
	setopt [internalflags3], dif3_ss_b_bit_set
	mov ch, 40h		; set B bit if requested
@@:
	mov ax, 0009h
	int 31h			; set or clear B bit
	jc .fataldpmierr

	mov cx, 1		; allocate code_sel selector
	xor ax, ax
	int 31h
	jc .fataldpmierr
	mov word [ code_sel ], ax
	mov bx, ax
	xor cx, cx
	or dx, -1		; cx:dx = 0FFFFh
	mov ax, 0008h
	int 31h			; set limit 64 KiB
	jc .fataldpmierr
	lar cx, word [ cssel ]	; get access rights/type of cs
	jnz .fataldpmierr
	shr cx, 8		; proper format for 31.0009
				; high byte zero (16-bit and byte-granular selector)
	mov ax, 0009h
	int 31h			; set descriptor access rights/type
	jc .fataldpmierr

	mov dx, word [ code_seg ]
	mov cx, dx
	shl dx, 4
	shr cx, 12
	mov ax, 0007h
	int 31h			; set selector base to code segment's base

	call entry_to_code_sel
	dw installdpmi_code

.fataldpmierr:
	mov ax, 4CFFh
	int 21h


	usesection lDEBUG_CODE

	code_insure_low_byte_not_0CCh
installdpmi_code:
_386	push ebx
_386	push edx
		; Some code (particularly d4message) may expect us to
		;  run on the debugger's stack, to access the data
		;  segment. Therefore, switch stacks.
	mov dx, ds		; dx = ds = debugger data selector
	mov ax, ss
	_386_o32	; mov ebx, esp
	mov bx, sp		; ax:bx = stack to restore
	mov ss, dx
%ifn _ONLYNON386
..@patch_no386_ds_4:		; (insure to set sp directly after ss)
	o32		; mov esp, dword [run_sp]
%endif
	mov sp, [run_sp]	; switch to our stack

	push ax
	_386_o32	; push ebx
	push bx			; save original stack, far pointer

	setopt [internalflags], protectedmode

d4	call d4message
d4	asciz "In installdpmi_code",13,10

	mov cx, 2 + !!_DUALCODE + !!_IMMASM + 2 * !!_SYMBOLIC
				; allocate some descriptors
	xor ax, ax
	int 31h
	jc .fataldpmierr

d4	call d4message
d4	asciz "In installdpmi_code, allocated some descriptors",13,10

	mov word [ scratchsel ], ax	; the first is used as scratch descriptor
	mov bx, ax
	xor cx, cx
%if 1
_386	dec cx			; set a limit of FFFFFFFFh if 386
%else
	cmp byte [ dpmi32 ], 0
	je .16
	dec cx			; set a limit of FFFFFFFFh if 32-bit client
.16:
%endif
	or dx, byte -1
	mov ax, 0008h
	int 31h
	mov ax, 0003h
	int 31h			; get selector increment
%if 0
	jnc .03sup
	mov ax, 8
.03sup:
%endif
	xor cx, cx		; cx:dx = 0FFFFh
%if _SYMBOLIC
	add bx, ax		; the second selector is used as symbol selector
	mov word [symsel1], bx
	push ax
	mov ax, 0008h
	int 31h			; set limit 0FFFFh (only used with di anyway)
	pop ax
	add bx, ax		; the third selector is used as another symbol selector
	mov word [symsel2], bx
	push ax
	mov ax, 0008h
	int 31h			; set limit 0FFFFh (only used with di anyway)
	pop ax
%endif
%if _DUALCODE
	add bx, ax		; this selector is used as code2 sel
	mov word [ code2_sel ], bx
	push ax
	mov ax, 0008h
	int 31h			; set limit 64 KiB
	jc .fataldpmierr
	 push cx
	 push dx		; preserve limit = 0FFFFh
	lar cx, word [ cssel ]	; get access rights/type of cs
	jnz .fataldpmierr
	shr cx, 8		; proper format for 31.0009
				; high byte zero (16-bit and byte-granular selector)
	mov ax, 0009h
	int 31h			; set descriptor access rights/type
	jc .fataldpmierr

	mov dx, word [ code2_seg ]
	mov cx, dx
	shl dx, 4
	shr cx, 12
	mov ax, 0007h
	int 31h			; set selector base to code segment's base
	 pop dx
	 pop cx			; restore limit
	pop ax
%endif
%if _IMMASM
	add bx, ax
	push ax
	push cx
	push dx

	xor cx, cx
	mov dx, immasm_length
	mov ax, 0008h
	int 31h
	lar cx, word [ cssel ]
	jnz .fataldpmierr
	shr cx, 8
	mov ax, 0009h
	int 31h

	mov dx, word [immseg]
	call setrmaddr
	mov word [immsel], bx

	pop dx
	pop cx
	pop ax
%endif
	add bx, ax		; the last selector is client's CS
				; (this limit is FFFFh even for 32-bits)
	mov ax, 0008h
	int 31h

	_386_o32
	pop dx			; pop preserved (e)sp
	pop ax			; get client ss
	mov es, ax
	push ax			; preserve ss again
	_386_o32
	push dx			; preserve (e)sp again

	mov dx, word [ es:bp+18 ]
				; get client's CS
	call setrmaddr		; set base
	lar cx, word [ cssel ]
	jnz .fataldpmierr
	shr cx, 8		; CS remains 16-bit
	mov ax, 0009h
	int 31h
	mov word [ es:bp+18 ], bx

	cld

	mov si, convsegs
	mov cx, convsegs.amount_fixed
.loopseg:
d4	call d4message
d4	asciz "In installdpmi_code.loopseg",13,10

	lodsw
	mov di, ax
	mov bx, word [di + soaSegment]
	mov ax, 0002h
	int 31h
	jc .fataldpmierr
	mov word [di + soaSelector], ax
	loop .loopseg

d4	call d4message
d4	asciz "In installdpmi_code after .loopseg",13,10

_386	push edi
_386	xor edi, edi		; clear edih
	clropt [internalflags], canswitchmode|switchbuffer
	xor bp, bp
_386	inc bp
_386	inc bp
.save16:
	mov ax, 0305h		; get raw mode-switch save state addresses
	int 31h
	jc .cannotswitch
	cmp ax, _AUXBUFFSIZE	; fits into auxbuff ?
	ja .cannotswitch	; no -->
	test ax, ax
	jz .nobuffer

.switchbuffer_init:
	push ax				; ax = target switchbuffer size
	push bx
	push cx
	push si
	push di				; preserve regs returned by fn 0305h

	mov di, word [auxbuff_switchbuffer_size]
	mov cx, di
	sub cx, ax
	je .no_switchbuffer_size_change
	mov es, word [auxbuff_segorsel + soaSelector]
	ja .fillblanks

	neg cx				; = amount of bytes to move

.try_again:
	mov si, word [auxbuff_behind_last_silent]
					; (auxbuff):di -> next buffer (if it fits)
	mov ax, _AUXBUFFSIZE
	sub ax, si			; number of bytes left free
	cmp ax, cx			; fits ?
	jae .simple			; yes -->

.delete:
	call silence_delete_one_string.internal
	jc .switchbuffer_error
	jmp .try_again

.simple:
	push ds
	 push si
		; -> behind used data minus start of auxbuff (0)
		; = how much to copy
	dec si				; -> last used byte
	mov di, si			; -> last used source byte
	add di, cx			; -> destination of last byte
	add word [auxbuff_behind_last_silent], cx
					; update silent pointer
	add word [auxbuff_behind_while_condition], cx
					; update while pointer
	 pop cx				; = how much to copy
	std				; _AMD_ERRATUM_109_WORKAROUND as below
	 push es
	 pop ds

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
	rep movsb			; copy up silent buffer and while cond
	cld

	pop ds

		; (fall through. cx is equal to zero here so the
		;  rep stosb in .fillblanks is effectively a no-op.)

.fillblanks:
		; This isn't really supported but we'll do enough
		;  to handle basic cases. WHILE conditions use the
		;  switchbuffer size to find their stored condition.
		;  So fill the additional space with blanks, which
		;  WHILE condition parsing will happily eat up.
	mov al, 32
	rep stosb

.no_switchbuffer_size_change:
	db __TEST_IMM8			; (skip stc, NC)
.switchbuffer_error:
	stc
	pop di
	pop si
	pop cx
	pop bx				; restore returned registers
	pop ax				; ax = target switchbuffer size
	jc .cannotswitch
	mov word [auxbuff_switchbuffer_size], ax
	setopt [internalflags], switchbuffer
	mov word [dpmi_rmsav+0], cx
	mov word [dpmi_rmsav+2], bx
	_386_o32	; mov dword [dpmi_pmsav], edi
	mov word [dpmi_pmsav], di
	mov word [ds:bp+dpmi_pmsav+2], si
.nobuffer:
_386	xor edi, edi		; clear edih
	mov ax, 0306h		; get raw mode-switch addresses
	int 31h
	jc .cannotswitch
	setopt [internalflags], canswitchmode
	mov word [dpmi_rm2pm+0], cx
	mov word [dpmi_rm2pm+2], bx
	_386_o32	; mov dword [dpmi_pm2rm], edi
	mov word [dpmi_pm2rm], di
	mov word [ds:bp+dpmi_pm2rm+2], si
.cannotswitch:
_386	pop edi

_386	push eax
%if (_CATCHPMINT214C || _DEBUG) || 1
	push es
	 push ds
	 pop es
%endif

%if CATCHEXCAMOUNT
	mov si, exctab		; hook several exceptions
%if (_CATCHPMINT214C || _DEBUG) || 1
	mov di, excsave
%endif
_386	xor edx, edx		; clear edxh
	mov dx, exc_first
.loopexc:
	lodsb
d4	call d4message
d4	asciz "In installdpmi_code.loopexc, ax="
d4	push ax
d4	call d4disp_stack_hex
d4	call d4message
d4	asciz 13,10

	mov bl, al
%if (_CATCHPMINT214C || _DEBUG) || 1
	_386_o32	; push edx
	push dx			; preserve excXX pointer
	mov ax, 0202h
		; (edxh is zero)
	int 31h			; cx:(e)dx -> prior handler
	_386_o32	; xchg eax, edx
	xchg ax, dx
	_386_o32	; stosd
	stosw			; store offset (dword on 386+, else word)
	xchg ax, cx
	stosw			; store selector
	mov ax, 4
	sub ax, bp		; if 386, ax = 4 - 2 = 2, else ax = 4
	add di, ax		; -> next entry of excsave
	_386_o32	; pop edx
	pop dx
%endif
	mov cx, word [cssel]	; -> our handler for this exception
	mov ax, 0203h
	int 31h			; set our handler
	add dx, byte exc_second - exc_first
				; -> next handler
	cmp si, endexctab	; if another to go -->
	jb .loopexc
%endif

%if CATCHPMINTAMOUNT
	mov si, pminttab	; ds:si -> pminttab
	mov di, pmintsave	; es:di -> pmintsave
.loopint:
	lodsb			; get interrupt number
	mov bl, al		; bl = interrupt number
_386	xor edx, edx		; clear edxh
	mov ax, 0204h
	int 31h			; cx:(e)dx -> prior handler
	_386_o32	; xchg eax, edx
	xchg ax, dx		; (e)ax = offset
	_386_o32	; stosd
	stosw			; store offset (dword on 386+, else word)
	xchg ax, cx
	stosw			; store selector
	mov ax, 4
	sub ax, bp		; if 386, ax = 4 - 2 = 2, else ax = 4
	add di, ax		; -> next entry of pmintsave
	lodsw			; ax -> our handler
_386	xor edx, edx
	xchg ax, dx		; (e)dx -> our handler
	mov cx, word [cssel]	; cx:(e)dx -> our handler
	mov ax, 0205h
	int 31h
	cmp si, pminttab.end
	jb .loopint
%endif

%if (_CATCHPMINT214C || _DEBUG) || 1
	pop es
%endif
_386	pop eax

	mov si, convsegs
@@:
	lodsw
	xchg ax, di
	push word [di + soaSelector]
	pop word [di + soaSegSel]
	cmp si, convsegs.end_fixed
	jb @B

	testopt [internalflags], hooked2F
	jz .notours		; not currently hooked -->

	mov al, 2Fh		; interrupt number
	mov si, debug2F		; -> IISP entry header
	mov dx, opt4_int_2F_force >> 16
	call UnhookInterruptForce
				; try unhooking it
	jnc .got2F

.not2F:
	mov word [msg.serial_cannot_unhook.int], "2F"
	mov dx, msg.serial_cannot_unhook
	jmp @F

.got2F:
	clropt [internalflags], hooked2F
	clropt [internalflags4], dif4_int_2F_hooked
	call update_inttab_optional
	mov word [msg.serial_late_unhook.int], "2F"
	mov dx, msg.serial_late_unhook
@@:
	call putsz
.notours:


		; This is a bit silly: If we're never
		;  entered in PM except for installdpmi
		;  then the debugger thinks that the
		;  SegSel fields should be segments.
		; It won't re-initialise them because
		;  according to the flag used by run,
		;  it was last running in 86 Mode.
		; So for now just hack them back.
	mov si, convsegs
@@:
	lodsw
	xchg ax, di
	push word [di + soaSegment]
	pop word [di + soaSegSel]
	cmp si, convsegs.end_fixed
	jb @B

d4	call d4message
d4	asciz "In installdpmi_code end",13,10

	_386_o32	; pop ebx
	pop bx
	pop ax
	mov ss, ax
%ifn _ONLYNON386
..@patch_no386_ds_5:		; (insure to set sp directly after ss)
	o32		; mov esp, ebx
%endif
	mov sp, bx		; return to user stack

_386	pop edx
_386	pop ebx
	pop es
%if _BREAK_INSTALLDPMI
	testopt [options3], opt3_break_installdpmi
	jnz .break
%endif
	pop ds
	popf
	popa
	retf

%if _BREAK_INSTALLDPMI
.break:
	cli
	pop word [reg_ds]	; set client ds
	pop word [reg_efl]	; set client fl
	popa			; restore client GPRs
	mov word [reg_eax], ax	; set client ax
	mov ax, ds		; ax = entry/data selector
	pop word [reg_eip]	; set client ip (intrtn_code clears eiph)
	pop word [reg_cs]	; set client cs
	mov word [run_int], installdpmimsg	; remember interrupt type
	setopt [internalflags], protectedmode
	jmp intrtn_code.from_installdpmi
%endif


.fataldpmierr:
d4	call d4message
d4	asciz "In installdpmi_code.fataldpmierr",13,10
	mov ax, 4CFFh
	int 21h
