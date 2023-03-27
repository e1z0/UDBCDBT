
%if 0

lDebug initialisation

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection INIT

initcode:
%if ($ - $$) != 0
 %fatal initcode expected at start of section
%endif

	mov ax, ss
	mov dx, ds
	sub ax, dx
	xor dx, dx
	mov cx, 4
@@:
	shl ax, 1
	rcl dx, 1
	loop @B

	push ax			; (if sp was zero)

	add ax, sp
	adc dx, 0
	add ax, 15
	adc dx, 0

	and al, ~15

	cmp dx, NONBOOTINITSTACK_END >> 16
	ja .stackdownfirst
	jb .memupfirst
	cmp ax, NONBOOTINITSTACK_END & 0FFFFh
	jae .stackdownfirst
.memupfirst:
	mov bx, paras(NONBOOTINITSTACK_END)
	mov ah, 4Ah
	int 21h
	jnc @F
.memfail:
	mov dx, imsg.early_mem_fail
.earlyfail:
	call init_putsz_cs
	mov ax, 4CFFh
	int 21h

@@:
.stackdownfirst:
	mov ax, ds
	add ax, paras(NONBOOTINITSTACK_START)
	cli
	mov ss, ax
	mov sp, NONBOOTINITSTACK_SIZE
	sti

		; if jumped to .stackdownfirst: now, shrink our memory block
		; else: no-op (already grew or shrunk block)
	mov bx, paras(NONBOOTINITSTACK_END)
	mov ah, 4Ah
	int 21h
	jc .memfail


	mov ax, ds
	add ax, paras(INITSECTIONOFFSET)
	mov dx, ds
	add dx, paras(NONBOOTINITTARGET)
	mov cx, init_size_p
	call init_movp

	push dx
	call init_retf

	mov bx, ds
	mov dx, bx
	add bx, paras(AUXTARGET1)
	add dx, paras(CODETARGET1)
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	mov ax, bx
	add ax, paras(auxbuff_size)
  CODETARGET1_equ equ CODETARGET1
  CODETARGET2_equ equ CODETARGET2
  AUXTARGET1_equ equ AUXTARGET1
  AUXTARGET2_equ equ AUXTARGET2
  %if AUXTARGET1_equ <= CODETARGET1_equ
%assign nn AUXTARGET1_equ
%assign mm CODETARGET1_equ
   %error Unexpected layout aux = nn code = mm
  %endif
 %endif
	mov cx, dx
	call init_check_auxbuff
	jz @F

	mov bx, ds
	mov dx, bx
	add bx, paras(AUXTARGET2)
	add dx, paras(CODETARGET2)
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
  ldebug_codes_size_equ equ ldebug_code_size + ldebug_code2_size
  auxbuff_size_equ equ auxbuff_size
  %if (paras(AUXTARGET1_equ) + paras(auxbuff_size_equ)) \
	!= (paras(CODETARGET2_equ) + paras(ldebug_codes_size_equ))
   %error Unexpected layout
  %endif
 %endif
	call init_check_auxbuff
	jz @F

		; If both prior attempts failed, we allocate
		;  an additional 8 KiB and move the buffer to
		;  that. This should always succeed.
	mov word [cs:memsize], paras(AUXTARGET3 \
			+ auxbuff_size \
			+ historysegment_size)
				; enlarge the final memory block size

	mov bx, ds
	add bx, paras(AUXTARGET3)
	mov dx, cx
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	mov ax, bx
	add ax, paras(auxbuff_size)
 %endif
	call init_check_auxbuff
	jz @F

		; Because this shouldn't happen, this is
		;  considered an internal error.
	mov dx, imsg.early_reloc_fail
	jmp .earlyfail

@@:
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	push ax
 %endif
	mov ax, ds
	add ax, paras(CODESECTIONOFFSET)
	mov cx, ldebug_code_size_p + ldebug_code2_size_p
	call init_movp

	mov word [code_seg], dx		; initialise code segment reference
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	pop ax
	mov word [history.segorsel + soaSegSel], ax
%if _PM
	mov word [history.segorsel + soaSegment], ax
%endif
	mov es, ax
	xor di, di
	mov cx, historysegment_size >> 1
	xor ax, ax
	rep stosw
 %endif

	mov ax, bx

	mov word [auxbuff_segorsel + soaSegSel], ax
%if _PM
	mov word [auxbuff_segorsel + soaSegment], ax
					; initialise auxbuff references
%endif
%if _IMMASM  && _IMMASM_AUXBUFF
	mov word [immseg], ax
%endif

	mov es, ax
	xor di, di
	mov cx, _AUXBUFFSIZE >> 1
	xor ax, ax
	rep stosw			; initialise auxbuff

	cli
	mov ax, ds
	mov es, ax
	mov ss, ax
	mov sp, stack_end		; application mode stack switch
	sti

	mov ah, 4Ah
	mov bx, paras(NONBOOTINITSTACK_START)
	int 21h				; shrink to drop init stack

	jmp old_initcode


init_retf:
	retf


		; INP:	bx => destination for auxbuff
		;	(The following are not actually used by this function,
		;	 they're just what is passed in and preserved to
		;	 be used by the caller after returning.)
		;	dx => destination for code image
		;	(if boot-loaded:) cx => destination for pseudo-PSP
		;		(implies cx+10h => destination for data_entry)
		;	ax => segment for history buffer
		; OUT:	ZR if this destination for auxbuff doesn't cross
		; 	 a 64 KiB boundary
		;	NZ else
		; CHG:	si, di
init_check_auxbuff:
	mov si, bx		; => auxbuff
%if _AUXBUFFSIZE < 8192
 %error Expected full sector length auxbuff
%endif
	lea di, [si + (8192 >> 4)]; => behind auxbuff (at additional paragraph)
	and si, 0F000h		; => 64 KiB chunk of first paragraph of auxbuff
	and di, 0F000h		; => 64 KiB chunk of additional paragraph
	cmp di, si		; same ?
				; ZR if they are the same
	retn


%if _BOOTLDR
		; Our loader transfers control to us with these registers:
		; INP:	ss:bp -> BPB
		;	ss:bp - 16 -> loadstackvars
		;	ss:bp - 32 -> loaddata
		;	(loader enters at) cs:0 -> loaded payload
		;	(loader enters at) cs:32 -> entry point
		;	(entrypoint sets up) ds:100h -> loaded payload
		; STT:	EI, UP
		;	all interrupts left from BIOS
boot_initcode:
	cld

d4	call init_d4message
d4	asciz "In boot_initcode",13,10

	mov dx, word [bp + ldMemoryTop]

		; initialise sdp
	mov ax, word [bp + bsBPB + bpbHiddenSectors + 2]
	mov word [load_data - LOADDATA2 + bsBPB + bpbHiddenSectors + 2], ax
	mov ax, word [bp + bsBPB + bpbHiddenSectors]
	mov word [load_data - LOADDATA2 + bsBPB + bpbHiddenSectors], ax

	xor bx, bx
	mov al, byte [bp + bsBPB + ebpbNew + bpbnBootUnit]
	mov byte [load_data - LOADDATA2 + bsBPB + ebpbNew + bpbnBootUnit], al
	mov bl, al			; bx = LD unit
	 test al, al			; hdd or diskette ?
	mov ax, word [bp + ldQueryPatchValue]
	 jns @F				; diskette -->
	xchg al, ah			; get high word of query patch value
@@:
	test al, al			; use for our access to this unit ?
	jns @F				; no -->
	and al, luf_mask_writable	; clear unused bits
	mov byte [load_unit_flags + bx], al
					; save here
@@:


	mov bx, ds
	mov es, bx			; => data entry image
	mov di, loaddata_loadedfrom	; -> loaded from data (ldp)

		; initialise LOADDATA, LOADSTACKVARS, and BPB
	push ss
	pop ds
	lea si, [bp + LOADDATA]		; -> LOADDATA on stack
	mov cx, (-LOADDATA + bsBPB + ebpbNew + BPBN_size)
	rep movsb

		; initialise cmdline_buffer from below LOADDATA
	lea si, [bp + ldCommandLine.start]
	mov di, cmdline_buffer		; -> our buffer in data entry

	cmp word [si], 0FF00h
	jne @F

	 push cs
	 pop ds
	mov si, imsg.default_cmdline

@@:
	lodsb
	test al, al
	jz @FF

	setopt [es:internalflags3], dif3_input_cmdline
	db __TEST_IMM16
.switch_c_loop:
	stosb
	lodsb
.switch_c_loop_after_semicolon:
	cmp al, 0
	je @F
	cmp al, ';'
	jne .switch_c_not_semicolon
	mov al, 13
	stosb
	call init_skipwhite
	jmp .switch_c_loop_after_semicolon

.switch_c_not_semicolon:
	cmp al, '\'
	jne .switch_c_loop
	lodsb
	cmp al, 0
	jne .switch_c_loop

@@:
	stosb
@@:

	mov ax, dx
	sub ax, paras(BOOTDELTA)
	jc .error_out_of_memory
		; We exaggerate the target size (BOOTDELTA) for the
		;  worst case, thus we do not need to check for narrower
		;  fits later on. BOOTDELTA includes the pseudo-PSP size,
		;  data_entry size, asmtable1_size, asmtable2_size,
		;  datastack_size, code_size, 2 times auxbuff_size,
		;  historysegment_size,
		;  plus 16 bytes for the image ident prefix paragraph,
		;  and all of that rounded to a kibibyte boundary.

	mov cx, cs
	add cx, paras(init_size + BOOTINITSTACK_SIZE)
	jc .error_out_of_memory
	cmp cx, dx
	ja .error_out_of_memory
		; This requires that above the image (including init)
		;  there is some 512 bytes free. That could be a problem
		;  except we've already exhausted the iniload-internally
		;  used buffers + stack, and do not need to preserve any
		;  of those. Recall that dx holds MemoryTop, *not* the
		;  lower LoadTop which could be right behind the image.
		; The sector buffer alone is documented as being 8 KiB
		;  sized, not to mention the FAT buffer and stack and
		;  LOADDATA/LOADSTACKVARS/BPB/boot sector.

	mov di, cs
	cli
	mov ss, di
	mov sp, init_size + BOOTINITSTACK_SIZE
	sti

d4	call init_d4message
d4	asciz "Switched to init stack",13,10

	lframe none
	lvar word,	target
	lenter
	lvar word,	targetstart
	 push ax
	lvar word,	memtop
	 push dx
	lea di, [bx + 10h]
	lvar word,	data
	 push di
	lea di, [bx + paras(CODESECTIONOFFSET)]
	lvar word,	code
	 push di

	cmp cx, ax			; does init end below-or-equal target ?
	jbe .no_relocation		; yes, no relocation needed -->

d4	call init_d4message
d4	asciz "Needs relocation of init segment",13,10

	mov ax, word [bp + ?data]
	sub ax, paras(init_size + BOOTINITSTACK_SIZE)
	jc .error_out_of_memory		; already at start of memory -->
	cmp ax, 60h
	jb .error_out_of_memory		; already at start of memory -->

		; The relocation never overlaps, as we move init
		;  and its stack to the space before the image.
		;  Therefore we can move UP. And only a single
		;  rep movsw instruction is needed as init and
		;  its stack always fit in a single segment.
	push cs
	pop ds
	xor si, si			; -> init source
	mov es, ax
	xor di, di			; -> init destination
	mov cx, words(init_size + BOOTINITSTACK_SIZE)
	rep movsw			; relocate only init
		; Must not modify the data already on the stack here,
		;  until after either .done_relocation or
		;  .entire_relocation_done (which both relocate ss).

	push ax
	call init_retf			; jump to new init

	; mov ss, ax
		; Logic error: The entire load image relocation would
		;  make it so that the stack would corrupt part of the
		;  load image that had been relocated the first time
		;  already here. We want to keep using the high stack
		;  here until both relocations are done, at which point
		;  we *must* relocate ss so as to get the stack out of
		;  the way of installing the final image components.
		; Avoid modifying the stack frame variables until after
		;  we have relocated the stack. However, temporary use
		;  of the stack is okay before relocating it. (It must
		;  be because IRQs and debugger tracing will use it.)
	mov cx, word [bp + ?code]
	add cx, paras(ldebug_code_size + ldebug_code2_size)
	cmp cx, word [bp + ?targetstart]
					; does code end below-or-equal target ?
	jbe .done_relocation		; yes, relocated enough -->

.entire_relocation_needed:
d4	call init_d4message
d4	asciz "Needs relocation of entire load image",13,10

	mov dx, 60h
	mov es, dx
	mov ax, cs
	cmp dx, ax			; already at start of memory ?
	jae .error_out_of_memory	; then error -->
		; This move is always downwards, that is, we can
		;  move UP. Multiple instructions operating on
		;  different segments may be needed as the image
		;  can be larger than 64 KiB.
		; However, init was already moved to before the
		;  remainder of the image, so the first 64 KiB
		;  move will always leave init finished and
		;  ready to use. So only move the first chunk
		;  in the special relocator, then jump into the
		;  final relocated init to do the remainder.

	inc dx
	; cmp dx, ax
	; ja .error_out_of_memory
	 push dx
	 push word [cs:.word_relocated]	; on stack: far address of .relocated

	mov cx, ax			; source
	sub cx, dx			; source - target = how far to relocate

	xor di, di			; es:di -> where to put relocator
	push es
	push di				; on stack: relocator destination
	push cx				; on stack: how far to relocate
	 push cs
	 pop ds
	mov si, .relocator		; -> relocator source
	mov cx, 8
	rep movsw			; put relocator stub

	mov es, dx
	pop dx				; dx = how far to relocate

	xor di, di			; -> where to relocate to
	xor si, si			; -> relocate start

BOOTRELOC1 equ	paras( init_size + BOOTINITSTACK_SIZE + ldebug_data_entry_size \
			+ asmtable1_size + asmtable2_size \
			+ ldebug_code_size + ldebug_code2_size)

%if 0
	mov cx, BOOTRELOC1		; how much to relocate
	mov bx, 1000h
	mov ax, cx
	cmp ax, bx			; > 64 KiB?
	jbe @F
	mov cx, bx			; first relocate the first 64 KiB
@@:
	sub ax, cx			; how much to relocate later
	shl cx, 1
	shl cx, 1
	shl cx, 1			; how much to relocate first,
					;  << 3 == convert paragraphs to words
%else
	mov bx, 1000h
 %if BOOTRELOC1 > 1000h
	mov cx, 8000h
	mov ax, BOOTRELOC1 - 1000h
 %else
	mov cx, BOOTRELOC1 << 3
	xor ax, ax
 %endif
%endif
	retf				; jump to relocator

	align 2, db 0
.word_relocated:
	dw .relocated

		; ds:si -> first chunk of to be relocated data
		; es:di -> first chunk of relocation destination
		;  (si = di = 0, and es always <= ds)
		; cx = number of words in first chunk
		; ax = how many paragraphs remain after first chunk is done
		; dx = how far to relocate in paragraphs
		; bx = 1000h
		; ss:sp -> far return address into relocated init section
		;  (this always points into the first chunk)
.relocator:
	rep movsw
	retf				; jump to relocated cs : .relocated

.relocated:
		; ds => prior chunk of relocation source (may be corrupted)
		; es => prior chunk of relocation destination
		; cx = 0
		; ax = how many paragraphs remain
		; dx = how far to relocate in paragraphs
		; bx = 1000h
@@:
	mov cx, es
	add cx, bx
	mov es, cx	; => next segment

	mov cx, ds
	add cx, bx
	mov ds, cx	; => next segment

	sub ax, bx	; = how much to relocate after this round
	mov cx, 1000h << 3	; in case another full 64 KiB to relocate
	jae @F		; another full 64 KiB to relocate this round -->
	add ax, bx	; restore (possibly zero)
	shl ax, 1
	shl ax, 1
	shl ax, 1	; convert paragraphs to words
	xchg cx, ax	; cx = that many words (possibly zero)
	xor ax, ax	; no more to relocate after this round

@@:
		; ds:0 -> next chunk of source
		; es:0 -> next chunk of destination
		; cx = how many words in this chunk, may be zero
		; ax = how many paragraphs remain for next round
		; (if ax is nonzero then cx is 8000h for a full 64 KiB)
		; dx = how far to relocate in paragraphs
		; bx = 1000h
	xor si, si	; -> source
	xor di, di	; -> destination
	rep movsw	; relocate next chunk
	test ax, ax	; another round needed?
	jnz @BB		; yes -->

.entire_relocation_done:
	mov ax, cs
	mov ss, ax			; relocate the stack
	nop
		; The stack frame variables have been relocated here
		;  along with the INIT segment data.

		; Now okay to modify relocated stack frame variables.
	sub word [bp + ?data], dx
	jc .error_internal
	sub word [bp + ?code], dx
	jc .error_internal

	mov cx, word [bp + ?code]
	add cx, paras(ldebug_code_size + ldebug_code2_size)
	cmp cx, word [bp + ?targetstart]
					; does code end below-or-equal target ?
	jbe .done_relocation		; yes -->

.error_out_of_memory:
	mov dx, imsg.boot_error_out_of_memory
.putsz_error:
	call init_putsz_cs_bootldr
	jmp init_booterror.soft

.error_internal:
	mov dx, imsg.boot_error_internal
	jmp .putsz_error


.done_relocation:
.no_relocation:
	mov ax, cs
	mov ss, ax			; relocate the stack
	nop
		; Not needed if we got here after having executed
		;  .entire_relocation_done or by branching to this
		;  place through the .no_relocation label, but
		;  doesn't hurt in those cases either.
		; The stack frame variables have been relocated here
		;  along with the INIT segment data.

	mov byte [cs:init_booterror.patch_switch_stack], __TEST_IMM8
					; SMC in section INIT

d4	call init_d4message
d4	asciz "Relocated enough",13,10


	int 12h
	mov cl, 6
	shl ax, cl

	push ax
	push ds
	xor si, si
	xchg dx, ax
	mov ds, si
	lds si, [4 * 2Fh]
	add si, 3
	lodsb
	cmp al, 'R'
	jne .no_rpl
	lodsb
	cmp al, 'P'
	jne .no_rpl
	lodsb
	cmp al, 'L'
	jne .no_rpl
	mov ax, 4A06h
	int 2Fh
.no_rpl:
	xchg ax, dx
	pop ds
	pop dx

	cmp ax, dx
	je .no_error_rpl
		; in case RPL is present, error out (for now)

		; notes for +RPL installation:
		; 1. Allocate enough memory for our MCB + an PSP + our image + the last and the RPL MCB
		; 2. Create the RPL's MCB + a last MCB
		; 3. Relocate, initialise PSP
		; 4. Hook Int2F as RPLOADER to report DOS our new size

	mov dx, imsg.rpl_detected
	jmp .putsz_error

.no_error_rpl:
d4	call init_d4message
d4	asciz "Loader past RPL detection",13,10

	mov bx, word [bp + ?memtop]
	cmp bx, ax
	je @F

		; Special debugging support: If memtop is below
		;  what we detected using int 12h then update
		;  memtop and continue with the changed memtop.
		; The relocations done will suffice in any case.
	mov word [bp + ?memtop], ax
	mov bx, ax
	jb @F

	mov dx, imsg.mismatch_detected
	jmp .putsz_error

@@:					; bx => behind usable memory
%if 0
	mov ah, 0C1h
	stc
	int 15h				; BIOS, do you have an EBDA?
	mov ax, es
	jnc .ebda			; segment in ax -->
					; I don't believe you, let's check
%endif	; Enabling this would enable the BIOS to return an EBDA even if it isn't
	; noted at 40h:0Eh, which would be useless because we have to relocate it.

	xor dx, dx			; initialise dx to zero if no EBDA
	mov ax, 40h
	mov es, ax
	mov ax, word [ es:0Eh ]		; EBDA segment (unless zero) or LPT4 base I/O address (200h..3FCh)
	cmp ax, 400h
	jb .noebda			; -->
.ebda:
d4	call init_d4message
d4	asciz "EBDA detected",13,10

	inc byte [cs:init_boot_ebdaflag]
	cmp ax, bx
	;jb init_booterror.soft		; uhh, the EBDA is inside our memory?
	;ja init_booterror.soft		; EBDA higher than top of memory. This is just as unexpected.
	je @F
	mov dx, imsg.boot_ebda_unexpected
	jmp .putsz_error

@@:
	mov ds, ax
	xor dx, dx
	mov dl, byte [ 0 ]		; EBDA size in KiB
	mov cl, 6
	shl dx, cl			; *64, to paragraphs
	mov word [cs:init_boot_ebdasize], dx
	mov word [cs:init_boot_ebdasource], ax
d4	jmp @F
.noebda:
d4	call init_d4message
d4	asciz "No EBDA detected",13,10
@@:


	mov cx, word [bp + ?memtop]
	add cx, [cs:init_boot_ebdasize]
	sub cx, paras(INITSECTIONOFFSET + datastack_size + auxbuff_size + historysegment_size)
					; cx = paragraph of pseudo-PSP if here
	dec cx				; => paragraph of image ident
	and cx, ~ (paras(1024) - 1)	; round down to kibibyte boundary
	inc cx				; => paragraph of pseudo-PSP if here

	mov bx, cx
	mov dx, bx
	add bx, paras(AUXTARGET1)	; => auxbuff target if here
	add dx, paras(CODETARGET1)	; => code target if here
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	mov ax, bx
	add ax, paras(auxbuff_size)
  %if AUXTARGET1_equ <= CODETARGET1_equ
   %error Unexpected layout
  %endif
 %endif
	call init_check_auxbuff
	jz @F

d4	call init_d4message
d4	asciz "First layout rejected",13,10

	mov bx, cx			; attempt same target again
	mov dx, bx
	add bx, paras(AUXTARGET2)	; => auxbuff target if here
	add dx, paras(CODETARGET2)	; => code target if here
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
  %if (paras(AUXTARGET1_equ) + paras(auxbuff_size_equ)) \
	!= (paras(CODETARGET2_equ) + paras(ldebug_codes_size_equ))
   %error Unexpected layout
  %endif
 %endif
	call init_check_auxbuff
	jz @F

d4	call init_d4message
d4	asciz "Second layout rejected",13,10

		; If both prior attempts failed, we allocate
		;  an additional 8 KiB and move the buffer to
		;  that. This should always succeed.
	mov cx, word [bp + ?memtop]
	add cx, [cs:init_boot_ebdasize]
	sub cx, paras(INITSECTIONOFFSET + datastack_size + auxbuff_size*2 + historysegment_size)
					; cx = paragraph of pseudo-PSP if here
	dec cx				; => paragraph of image ident
	and cx, ~ (paras(1024) - 1)	; round down to kibibyte boundary
	inc cx				; => paragraph of pseudo-PSP if here

	mov bx, cx
	mov dx, bx
	add bx, paras(AUXTARGET1)	; => auxbuff target if here
		; Note that we use AUXTARGET1 here, not AUXTARGET3, because
		;  we move where the debugger starts rather than where it ends.
	add dx, paras(CODETARGET1)	; => code target if here
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	mov ax, bx
	add ax, paras(auxbuff_size)
 %endif
	call init_check_auxbuff
	jz @F

		; Because this shouldn't happen, this is
		;  considered an internal error.
	mov dx, imsg.early_reloc_fail
	jmp .putsz_error


		; cx => data_entry target
		; dx => code target
		; bx => auxbuff target
		; ax => history segment
@@:
d4	call init_d4message
d4	asciz "Layout found"
d4	call init_d4dumpregs
d4	call init_d4message
d4	asciz 13,10

 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	push ax
 %endif
	mov word [bp + ?target], cx
	push dx
	cmp byte [cs:init_boot_ebdaflag], 0
	jz .reloc_memtop_no_ebda
	dec cx
	sub cx, word [cs:init_boot_ebdasize]
	mov ax, word [cs:init_boot_ebdasource]
	mov dx, cx
	mov word [cs:init_boot_ebdadest], cx
	mov cx, word [cs:init_boot_ebdasize]
	call init_movp
	add word [bp + ?memtop], cx
	or byte [cs:init_boot_ebdaflag], 2
	mov ax, 40h
	mov es, ax
	mov word [es:0Eh], dx	; relocate EBDA

d4	call init_d4message
d4	asciz "EBDA relocated",13,10

	jmp @F

.reloc_memtop_no_ebda:
	mov dx, cx
@@:
	mov cl, 6
	shr dx, cl
	mov ax, 40h
	mov es, ax
	mov word [ cs:init_boot_new_memsizekib ], dx
	xchg word [es:13h], dx
	mov word [ cs:init_boot_old_memsizekib ], dx
	pop dx
d4	call init_d4message
d4	asciz "Memory top relocated",13,10

	mov cx, word [bp + ?target]
	mov ds, cx
	mov di, word [bp + ?memtop]	; => memory top
	sub di, paras(1024+8192)
	mov es, di
	cmp di, cx			; max padding starts below target PSP ?
	jb @F				; yes, do not initialise padding
	xor di, di			; -> padding
	mov cx, words(1024+8192)
	xor ax, ax
	rep stosw			; initialise padding
@@:

	mov ax, word [bp + ?code]	; => code source
					; dx => code target
	mov cx, ldebug_code_size_p + ldebug_code2_size_p
					; = size
	call init_movp			; relocate code to target
d4	call init_d4message
d4	asciz "Code segment relocated",13,10

		push dx			; (code segment)
	mov ax, word [bp + ?data]	; => data_entry source
	mov dx, ds
	add dx, paras(100h)		; => data_entry target
	mov cx, paras(ldebug_data_entry_size + asmtable1_size + asmtable2_size)
	call init_movp			; relocate data_entry to target
		pop word [code_seg]	; initialise code reference
d4	call init_d4message
d4	asciz "Data segment relocated",13,10

 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	pop ax
	mov word [history.segorsel + soaSegSel], ax
%if _PM
	mov word [history.segorsel + soaSegment], ax
%endif
	mov es, ax
	xor di, di
	mov cx, historysegment_size >> 1
	xor ax, ax
	rep stosw
 %endif

	mov ax, bx
	mov word [auxbuff_segorsel + soaSegSel], ax
%if _PM
	mov word [auxbuff_segorsel + soaSegment], ax
					; initialise auxbuff references
%endif
%if _IMMASM  && _IMMASM_AUXBUFF
	mov word [immseg], ax
%endif

	mov es, ax
	xor di, di
	mov cx, _AUXBUFFSIZE >> 1
	xor ax, ax
	rep stosw			; initialise auxbuff
d4	call init_d4message
d4	asciz "auxbuff initialised",13,10

	push ds
	pop es
	xor di, di
	mov cx, words(100h)
	rep stosw			; initialise pseudo-PSP

init_boot_imageident:
	mov ax, ds
	dec ax
	mov es, ax			; => paragraph for imageident
	xor di, di			; -> imageident target
	mov bx, word [bp + ?memtop]
	sub bx, ax			; = how many paragraphs do we use ?

	 push cs
	 pop ds
	mov word [imageident.size], bx	; set image ident size

	mov si, imageident
	push si
	mov cx, 8
	xor dx, dx
.loop:
	lodsw
	add dx, ax
	loop .loop
	pop si

	neg dx
	mov word [imageident.check], dx	; set image ident checksum

	mov cl, 8
	rep movsw			; write image ident paragraph

	mov ax, word [bp + ?target]

	lleave ctx			; dropping this frame for stack switch

	cli
	mov ds, ax
	mov ss, ax
	mov sp, stack_end		; boot mode stack switch
	sti

	push word [cs:init_boot_old_memsizekib]
	pop word [boot_old_memsizekib]
	push word [cs:init_boot_new_memsizekib]
	pop word [boot_new_memsizekib]
	mov al, byte [cs:init_boot_ebdaflag]
	and al, 1
	mov byte [boot_ebdaflag], al

	setopt [internalflags], nodosloaded
	clropt [internalflags], notstdinput|inputfile|notstdoutput|outputfile
	mov byte [notatty], 0	; it _is_ a tty
	setopt [internalflags3], dif3_gotint19

	mov dx, imsg.crlf
	call init_putsz_cs

d4	call init_d4message
d4	asciz "New boot_initcode done",13,10

	jmp boot_old_initcode


init_booterror:
.soft:
	xor ax, ax
	db __TEST_IMM16			; (skip mov)
.hard:
	mov al, 1

;d4	call init_d4pocketdosmemdump
d4	call init_d4dumpregs

.patch_switch_stack:
	jmp strict short .no_switch_stack

	mov bx, cs
	cli
	mov ss, bx
	mov sp, init_size + BOOTINITSTACK_SIZE
	sti

.no_switch_stack:
	push ax

	mov ax, 40h
	mov es, ax

	test byte [cs:init_boot_ebdaflag], 2
	jz @F

	mov dx, [cs:init_boot_ebdasource]
	mov ax, [cs:init_boot_ebdadest]
	mov cx, [cs:init_boot_ebdasize]
	call init_movp

	mov word [es:0Eh], dx
@@:

	mov dx, [cs:init_boot_old_memsizekib]
	test dx, dx
	jz @F
	mov word [es:13h], dx
@@:

	mov dx, imsg.booterror
	call init_putsz_cs_bootldr
	call init_getc_bootldr
	pop ax
	test ax, ax
	jnz @F
	int 19h
@@:
	jmp 0FFFFh:0
%endif	; _BOOTLDR

%if _DEVICE
		; Our entrypoint transfers control to us with these registers:
		; INP:	ss:sp -> bx, fl, ds, ax, far return address to DOS
		;	ds:100h -> loaded payload
device_initcode:
	cld

	or word [device_header.next], -1
		; ! this uses offset 100h in the adjusted ds

	pop bx
	push es
	push bx
	push cx
	push dx
	push si
	push di

	mov ax, word [es:bx + 0Eh + 2]	; => behind available memory
	mov dx, ds
	add dx, 10h			; => our memory
	sub ax, dx
	jc .memorybad
	xor dx, dx			; dx:ax = amount available paragraphs
	mov cx, 4
@@:
	shl ax, 1
	rcl dx, 1
	loop @B				; dx:ax = amount available bytes

	add ax, word [es:bx + 0Eh]
	adc dx, 0			; dx:ax = amount available bytes

	and al, ~15			; (round down)

	cmp dx, DEVICEINITSIZE >> 16
	jne @F
	cmp ax, DEVICEINITSIZE & 0FFFFh
@@:
	jae .memorygood

.memorybad:
	mov dx, imsg.early_mem_fail
	call init_putsz_cs

	mov ax, 3000h
	int 21h
	cmp al, 5
	jae @F
	mov dx, imsg.dos_below_5
.earlyfail:
	call init_putsz_cs
@@:

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop es

	mov ax, ds
	add ax, paras(100h)
	mov word [es:bx + 3], 8105h	; error, done, code: bad structure length
	and word [es:bx + 0Eh], 0
	mov word [es:bx + 0Eh + 2], ax	; -> behind memory in use

	popf
	pop ds
	pop ax
	retf

.memorygood:
	mov ax, ds
	add ax, paras(INITSECTIONOFFSET)
	mov dx, ds
	add dx, paras(DEVICEINITTARGET)
	mov cx, init_size_p + deviceshim_size_p
	call init_movp

	push dx
	call init_retf

	mov bx, ds
	add bx, paras(DEVICEADJUST)
	mov dx, bx
	add bx, paras(AUXTARGET1)
	add dx, paras(CODETARGET1)
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	mov ax, bx
	add ax, paras(auxbuff_size)
 %endif
	mov cx, dx
	call init_check_auxbuff
	jz @F

	mov bx, ds
	add bx, paras(DEVICEADJUST)
	mov dx, bx
	add bx, paras(AUXTARGET2)
	add dx, paras(CODETARGET2)
	call init_check_auxbuff
	jz @F

		; If both prior attempts failed, we allocate
		;  an additional 8 KiB and move the buffer to
		;  that. This should always succeed.
	mov word [cs:memsize], paras(AUXTARGET3 \
			+ auxbuff_size \
			+ historysegment_size)
				; enlarge the final memory block size

	mov bx, ds
	add bx, paras(DEVICEADJUST)
	add bx, paras(AUXTARGET3)
	mov dx, cx
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	mov ax, bx
	add ax, paras(auxbuff_size)
 %endif
	call init_check_auxbuff
	jz @F

		; Because this shouldn't happen, this is
		;  considered an internal error.
	mov dx, imsg.early_reloc_fail
	jmp .earlyfail

@@:
 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	push ax
 %endif
	mov ax, ds
	add ax, paras(CODESECTIONOFFSET)
	mov cx, ldebug_code_size_p + ldebug_code2_size_p
	call init_movp

	mov word [code_seg], dx		; initialise code segment reference

	mov ax, ds
	add ax, 10h
	mov dx, ax
	add dx, paras(deviceshim_size + 110h)
	mov cx, paras(DATAENTRYTABLESIZE)
	call init_movp

	mov ax, cs
	add ax, init_size_p
	mov dx, ds
	add dx, paras(100h)
	mov cx, deviceshim_size_p
	call init_movp

	mov dx, ds
	add dx, paras(100h) + deviceshim_size_p
	mov es, dx
	push ds
	sub dx, deviceshim_size_p + 1
	mov ds, dx
	xor ax, ax
	xor di, di
	mov cx, 4
	rep stosw
	mov si, 8
	mov cl, 4
	rep movsw
	pop ds

	mov dx, ds
	add dx, paras(DEVICEADJUST)
	mov ds, dx

	push bx
	mov ah, 51h
	int 21h
	mov word [pspdbe], bx
	pop bx

	mov es, dx
	mov cx, words(256)
	xor di, di
	xor ax, ax
	rep stosw		; clear buffer for PSP + command line tail

		; PSP creation moved to later, after command line parsing

 %if _HISTORY_SEPARATE_FIXED && _HISTORY
	pop ax
	mov word [history.segorsel + soaSegSel], ax
%if _PM
	mov word [history.segorsel + soaSegment], ax
%endif
	mov es, ax
	xor di, di
	mov cx, historysegment_size >> 1
	xor ax, ax
	rep stosw
 %endif

	mov ax, bx

	mov word [auxbuff_segorsel + soaSegSel], ax
%if _PM
	mov word [auxbuff_segorsel + soaSegment], ax
					; initialise auxbuff references
%endif
%if _IMMASM  && _IMMASM_AUXBUFF
	mov word [immseg], ax
%endif

	mov es, ax
	xor di, di
	mov cx, _AUXBUFFSIZE >> 1
	xor ax, ax
	rep stosw			; initialise auxbuff

	mov ax, ds			; => PSP
	sub ax, deviceshim_size_p + paras(10h)
	mov word [device_header_address + 2], ax

	mov ax, ds			; => PSP
	mov bx, word [cs:memsize]
		; = amount paragraphs for PSP + DATA ENTRY + TABLE
		;  + DATA STACK + CODE + AUXBUFF + HISTORY
	add ax, bx			; => placeholder for trailing container
	add bx, deviceshim_size_p + paras(10h) + paras(10h)
		; (layout is deviceshim, MCB placeholder, debugger segments,
		;  placeholder for trailing container MCB)
					; = amount paragraphs expected in MCB
	mov word [device_mcb_paragraphs], bx

	mov es, ax
	xor di, di			; -> buffer for trailing container MCB
	mov cx, words(10h)		; = amount words
	push ds
	 push cs
	 pop ds
	 mov si, init_container_signature	; -> init string
	rep movsw
	pop ds
	inc ax				; => behind memory used for device

	pop word [reg_edi]
	pop word [reg_esi]
	pop word [reg_edx]
	pop word [reg_ecx]
	mov word [reg_ebp], bp
	pop bx
	pop es
	mov word [reg_ebx], bx
	mov word [reg_es], es

	mov word [es:bx + 3], 100h	; no error, done
	and word [es:bx + 0Eh], 0
	mov word [es:bx + 0Eh + 2], ax	; -> behind memory in use

	pop word [reg_efl]
	pop word [reg_ds]
	pop word [reg_eax]
	mov word [reg_ss], ss
	mov word [reg_esp], sp

	mov word [reg_cs], ds
	mov word [reg_eip], entry_retf

.cmdline:
	push ds
	lds si, [es:bx + 12h]		; ds:si -> device command line
	pop es
	mov di, 81h			; es:di -> PSP command line tail

		; Writing MS-DOS Device Drivers, second edition, page 349
		;  specifies the following as to the command line termination:
		; "Note that the DEVICE= command string is terminated by an
		;  Ah when there are no arguments. When there are arguments,
		;  the string is terminated with the following sequence:
		;  0h, Dh, Ah."

		; First skip past name.
@@:
	lodsb
	cmp al, 32			; blank ?
	je @F
	cmp al, 9
	je @F				; yes, got past executable filename -->
	cmp al, 0
	je .cmdline_end
	cmp al, 13
	je .cmdline_end
	cmp al, 10
	je .cmdline_end			; if empty tail -->
	jmp @B
@@:
	cmp di, 0FFh			; can store and still have space for CR ?
	je .cmdline_end_truncate	; no -->
	stosb				; store it
	cmp al, 0			; EOL ?
	je .cmdline_end
	cmp al, 13
	je .cmdline_end
	cmp al, 10
	je .cmdline_end			; yes -->
	lodsb
	cmp al, '!'			; escape for small letters ?
	jne @B				; no -->
	lodsb
	cmp al, 0
	je .cmdline_end_escaped
	cmp al, 13
	je .cmdline_end_escaped
	cmp al, 10
	je .cmdline_end_escaped
	; cmp al, '!'			; (automatically supported)
	cmp al, 'A'			; is it a capital letter ?
	jb @B
	cmp al, 'Z'
	ja @B
	xor al, 'a' ^ 'A'		; get the small letter
	jmp @B

.cmdline_end_escaped:
	mov dx, imsg.device_end_escaped
	jmp @F

.cmdline_end_truncate:
	mov dx, imsg.device_end_truncate
 @@:
	call init_putsz_cs
.cmdline_end:
	mov al, 13
	stosb				; store CR
	xchg ax, di
	 mov bx, es
	 mov ds, bx
	sub al, 82h			; if -> 82h (CR at 81h). get 0
	mov byte [80h], al		; store length

	cli
	 mov ss, bx
	 mov sp, stack_end		; device mode stack switch
	sti

	setopt [internalflags], tsrmode
	clropt [internalflags], attachedterm
	setopt [internalflags6], dif6_device_mode
	jmp old_initcode


init_device_error_late:
	testopt [internalflags], has386
	jz .16

subcpu 386
	mov eax, [reg_eax]
	mov ebx, [reg_ebx]
	mov ecx, [reg_ecx]
	mov edx, [reg_edx]
	mov esi, [reg_esi]
	mov edi, [reg_edi]
	mov ebp, [reg_ebp]
	push dword [reg_efl]
	popfd
	mov fs, [reg_fs]
	mov gs, [reg_gs]
subcpureset

.16:
		; ax done last
	mov bx, [reg_ebx]
	mov cx, [reg_ecx]
	mov dx, [reg_edx]
	mov si, [reg_esi]
	mov di, [reg_edi]
	mov bp, [reg_ebp]
	push word [reg_efl]
	popf
	mov es, [reg_es]
	mov ss, [reg_ss]
	mov sp, [reg_esp]
	push word [reg_eax]
	mov ax, ds
	mov ds, [reg_ds]

	sub ax, paras(deviceshim_size + 10h)
	mov word [es:bx + 3], 8103h	; error, done, code: unknown command
	and word [es:bx + 0Eh], 0
	mov word [es:bx + 0Eh + 2], ax	; -> behind memory in use
	pop ax
	retf
%endif


%if _DEBUG4 || _DEBUG5
%define _DEB_ASM_PREFIX init_
%include "deb.asm"
%endif


%if _SYMBOLIC
		; Moved this here so the nearcall macro is used
		;  before we write the relocate_from_code table.
	usesection lDEBUG_CODE
..@switch_s_cont:
	nearcall zz_save_strat
	nearcall zz_switch_s
	dec si
	mov dx, si
	retf

..@switch_s_catch:
	mov sp, word [throwsp]	; restore stack
				;  (needed here if returned to errret)
	mov dx, errcarat
	call putsz
	xor dx, dx
	retf
	usesection INIT
%endif


%macro __writepatchtable2 0-*.nolist
%if %0 & 1
 %fatal Expected even number of arguments
%endif
%rep %0 >> 1
	%1 %2
%rotate 2
%endrep
%endmacro

%macro __patchtable2_entry 0.nolist
  ; only if this isn't the first (pseudo-)entry
  %if %$lastcount != 0
    %assign %$runscount %[%$runscount]+1
   ; if the offset from %$previous is less than 255
   %if (%$last-%$previous) < 255
    %assign %$$method2tablesize %$$method2tablesize+1
    ; then write a single byte (number of bytes not to patch between)
    %xdefine %$$method2list %$$method2list,db,%$last-%$previous
   %else
    ; otherwise write a 255 ("reposition") and write the 16-bit address afterwards
    %assign %$$method2tablesize %$$method2tablesize+3
    %xdefine %$$method2list %$$method2list,db,255,dw,%$last
    %assign %$reposcount %[%$reposcount]+1
   %endif
   %if %$lastcount == 1
    %assign %$onecount %[%$onecount]+1
   %endif
   %assign %$$method2tablesize %$$method2tablesize+1
   ; and write the number of bytes to be patched
   %xdefine %$$method2list %$$method2list,db,%$lastcount
   ; define %$previous for the next entry: it points to the next non-patched byte
   %define %$previous (%[%$last]+%[%$lastcount])
  %endif
%endmacro

%macro writepatchtable 2-*.nolist

	numdef %{1}_FORCE_METHOD, 0
%push
		; Determine length of simple table:
%assign %$method1tablesize (%0 - 2)*2

%if !_%{1}_FORCE_METHOD || _%{1}_FORCE_METHOD == 2
		; Determine length of complicated table:
%assign %$method2tablesize 0
%define %$method2list db,""
%push
%if _WPT_LABELS
 %define %$previous code_start	; if list contains labels
%else
 %define %$previous 0
%endif
%define %$last %[%$previous]
%assign %$lastcount 0
%assign %$onecount 0
%assign %$reposcount 0
%assign %$bytescount %0 - 2
%assign %$runscount 0
%rotate 1
%rep %0 - 2
 %rotate 1
 ;if  it continues the previous patch   and not too long   and this isn't the first
 %if ((%$last+%$lastcount) == %1) && (%$lastcount < 255) && (%$lastcount != 0)
  ; then do not write an entry, just increase the patch's size
  %assign %$lastcount %[%$lastcount]+1
 %else
  ; otherwise write the last entry
  __patchtable2_entry
  ; define new %$last to this parameter, %$lastcount to one
  %define %$last %1
  %assign %$lastcount 1
 %endif
%endrep
__patchtable2_entry

; at the end, there's a patch with offset 0, size 0
%assign %$$method2tablesize %$$method2tablesize+2
%xdefine %$$method2list %$$method2list,db,0,db,0

%assign %$$onecount %$onecount
%assign %$$reposcount %$reposcount
%assign %$$bytescount %$bytescount
%assign %$$runscount %$runscount
%pop
%rotate 1
%endif

%if _%{1}_FORCE_METHOD == 2
 %define __%{1}_method 2
%elif _%{1}_FORCE_METHOD == 1
 %define __%{1}_method 1
%else
 %if _%{1}_FORCE_METHOD
  %fatal Invalid forced method selected: _%{1}_FORCE_METHOD
 %endif
 %if %$method1tablesize > (%$method2tablesize+20)
  %define __%{1}_method 2
 %else
  %define __%{1}_method 1
 %endif
%endif


%1:
%if __%{1}_method == 2
	__writepatchtable2 %$method2list
	endarea %1
%assign %$size %1_size
%warning %1: %$size bytes (Method 2)
%warning 1B=%$onecount repo=%$reposcount run=%$runscount byte=%$bytescount
%else
%rotate 1
%rep %0 - 2
%rotate 1
	dw %1
%endrep
%rotate 1
	endarea %1
%assign %$size %1_size
%warning %1: %$size bytes (Method 1)
%endif
%pop
%endmacro

	align 2, db 0				; align on word boundary
		; Table of patches that are to be set NOP if not running on a 386.
writepatchtable patch_no386_table, PATCH_NO386_TABLE
%undef PATCH_NO386_TABLE

	align 2, db 0
		; Table of patches that are to be set NOP if running on a 386.
writepatchtable patch_386_table, PATCH_386_TABLE
%undef PATCH_386_TABLE

%if _DUALCODE
	align 2, db 0				; align on word boundary
		; Table of patches that are to be set NOP if not running on a 386.
writepatchtable patch_no386_table2, PATCH_NO386_TABLE2
%undef PATCH_NO386_TABLE2

	align 2, db 0
		; Table of patches that are to be set NOP if running on a 386.
writepatchtable patch_386_table2, PATCH_386_TABLE2
%undef PATCH_386_TABLE2
%else
 %assign __patch_no386_table2_method 0
 %assign __patch_386_table2_method 0
%endif

%unmacro __writepatchtable2 0-*.nolist
%unmacro __patchtable2_entry 0.nolist
%unmacro writepatchtable 2-*.nolist


%if _DUALCODE && ! _PM
	align 2, db 0
relocate_from_code:
	dw  PATCH_RELOCATE_FROM_lDEBUG_CODE
.end:
	align 2, db 0
relocate_from_code2:
	dw  PATCH_RELOCATE_FROM_lDEBUG_CODE2
.end:

%unimacro dualcall 1.nolist
	; make sure we do not allow later uses
%endif


%if _BOOTLDR
	align 16, db 0
	; Image identification
	; First dword: signature
	; Next word: version, two ASCII digits
	; Next word: checksum. adding up all words of the paragraph gives zero
	; Next word: size of image (including this paragraph)
	; Three words reserved, zero.
imageident:
	db "NDEB00"
.check:	dw 0
.size:	dw 0
	times 3 dw 0
%endif
%if _DEVICE
	align 16, db 0
init_container_signature:
	fill 16, 0, db "FOR_SD_CONTAINER"
%endif
	align 2, db 0
memsize:	dw paras(CODETARGET2 \
			+ ldebug_code_size \
			+ ldebug_code2_size \
			+ historysegment_size)
			; same as paras(AUXTARGET1 + auxbuff_size + historysegment_size)

%if _BOOTLDR
init_boot_new_memsizekib:	dw 0
init_boot_old_memsizekib:	dw 0

init_boot_ebdasize:	dw 0
init_boot_ebdasource:	dw 0
init_boot_ebdadest:	dw 0
init_boot_ebdaflag:	db 0
%endif


imsg:
.early_mem_fail:
	db _PROGNAME,": Failed to allocate memory!"
.crlf:
	asciz 13,10
.early_reloc_fail:
	asciz _PROGNAME,": Failed to relocate, internal error!",13,10
%if _DEVICE
.dos_below_5:
	asciz " Note: DOS must be at least version 5.",13,10
.device_end_escaped:
	asciz _PROGNAME,": Error, got escaped command line end!",13,10
.device_end_truncate:
	asciz _PROGNAME,": Error, truncating too long command line!",13,10
%endif
.help.defaultfilename:
	db _FILENAME
.help.defaultfilename.length equ $ - .help.defaultfilename
.help.1:
	db _PROGNAME,_VERSION,", debugger.",13,10
	db 13,10
	db "Usage: "
	asciz
.help.2:
	db "[.COM] [/C=commands] [[drive:][path]progname.ext [parameters]]",13,10
	db 13,10
	db "  /C=commands",9,9,	"semicolon-separated list of commands (quote spaces)",13,10
	db "  /B",9,9,9,	"run a breakpoint within initialisation",13,10
	db "  /F[+|-]",9,9,	"always treat executable file as a flat binary",13,10
	db "  /E[+|-]",9,9,	"for flat binaries set up Stack Segment != PSP",13,10
%if _VXCHG
	db "  /V[+|-]",9,9,	"enable/disable video screen swapping",13,10
%endif
%if _DEBUG && _DEBUG_COND
	db "  /D[+|-]",9,9,	"enable/disable debuggable mode",13,10
%endif
%if _ALTVID
	db "  /2[+|-]",9,9,	"enable/disable use alternate video adapter for output",13,10
%endif
	db "  progname.ext",9,9,"(executable) file to debug or examine",13,10
	db "  parameters",9,9,	"parameters given to program",13,10
	db 13,10
	db "For a list of debugging commands, run "
	asciz
.help.3:
	db " and type ? at the prompt.",13,10
	asciz
%if _ONLY386
.no386:	ascizline "Error: This ",_PROGNAME," build requires a 386 CPU or higher."
%elif _ONLYNON386
.386:	asciiline "Warning: This ",_PROGNAME," build is ignorant of 386 CPU specifics."
	ascizline 9," It does not allow access to the available 386-specific registers!"
%endif

%if _SYMBOLIC
.switch_s_garbage:
	asciz "Ignoring garbage at end of /S switch!",13,10
.switch_s_error:
	asciz "Switch /S invalid content",13,10
%endif
.invalidswitch:
	db "Invalid switch - "
.invalidswitch_a:
	asciz "x",13,10
.switch_c_error:
	asciz "Switch /C invalid content",13,10
.switch_f_error:
	asciz "Switch /F invalid content",13,10
.switch_e_error:
	asciz "Switch /E invalid content",13,10
%if _MCLOPT
.switch_m_error:
	asciz "Switch /M invalid content",13,10
%endif
%if _VXCHG
.switch_v_error:
	asciz "Switch /V invalid content",13,10
%endif
%if _ALTVID
.switch_2_error:
	asciz "Switch /2 invalid content",13,10
%endif
%if _DEBUG && _DEBUG_COND
.switch_d_error:
	asciz "Switch /D invalid content",13,10
%endif
%if _BOOTLDR
.default_cmdline:
	db _BOOTSCRIPTPREFIX
	db "@if exists y ldp/",_BOOTSCRIPTNAME," :",_BOOTSCRIPTLABEL
	db " then y ldp/",_BOOTSCRIPTNAME," :",_BOOTSCRIPTLABEL
	asciz
.rpl_detected:
	asciz "RPL detected! Currently unsupported.",13,10
.mismatch_detected:
	asciz "Mismatch in memory size detected! Internal error!",13,10
.boot_ebda_unexpected:
	asciz "EBDA at unexpected position.",13,10
.boot_error_out_of_memory:
	asciz "Out of memory!",13,10
.boot_error_internal:
	asciz "Internal error while relocating load image!",13,10
.booterror:
	asciz 13,10,_PROGNAME," boot error. Press any key to reboot.",13,10
%endif
%if _DOSEMU
.dosemudate:	db "02/25/93"
%endif
%if _VDD
.vdd:		asciz "DEBXXVDD.DLL"
.dispatch:	asciz "Dispatch"
.init:		asciz "Init"
.mouse:		db "MOUSE",32,32,32		; Looks like a device name
.andy:		db "Andy Watson"		; I don't know him and why he's inside the NTVDM mouse driver
	endarea .andy
.ntdos:		db "Windows NT MS-DOS subsystem Mouse Driver"	; Int33.004D mouse driver copyright string (not ASCIZ)
	endarea .ntdos

		; INP:	-
		; OUT:	CY if not NTVDM
		;	NC if NTVDM
		;	ds = es = cs
		; CHG:	ax, bx, cx, dx, di, si, bp, es, ds
isnt:
		mov ax, 5802h			; Get UMB link state
		int 21h
		xor ah, ah
		push ax				; Save UMB link state
		mov ax, 5803h			; Set UMB link state:
		mov bx, 1			;  Add UMBs to memory chain
		int 21h
		mov ah, 52h
		mov bx, -1
		int 21h				; Get list of lists
		inc bx				; 0FFFFh ?
		jz .notnt			; invalid -->
		mov ax, word [es:bx-3]		; First MCB
		push cs
		pop es				; reset es
.loop:
		mov ds, ax			; ds = MCB
		inc ax				; Now segment of memory block itself
		xor dx, dx
		xor bx, bx
		cmp byte [bx], 'Z'		; End of MCB chain?
		jne .notlast
		inc dx
		jmp short .notchain
 .notlast:
		cmp byte [bx], 'M'		; Valid MCB chain?
		jne .error
 .notchain:
		mov cx, [bx+3]			; MCB size in paragraphs
				; ax = current memory block
				; cx = size of current memory block in paragraphs
				; dx = flag whether this is the last MCB
				; ds = current MCB (before memory block)
		cmp word [bx+1], 8		; MCB owner DOS?
		jne .notfound_1
		cmp word [bx+8], "SD"		; MCB name "SD"?
		jne .notfound_1
.loopsub:
		mov ds, ax			; SD sub-segment inside memory block
		inc ax
		dec cx
		mov bp, word [bx+3]		; Paragraphs 'til end of SD sub-segment
				; ax = current SD sub-segment
				; cx = paragraphs from SD sub-segment start (ax) to current memory block end
				; ds = current SD sub-MCB (like MCB, but for SD sub-segment)
				; bp = current SD sub-segment size in paragraphs
		cmp cx, bp
		jb .notfound_1			; Goes beyond memory block, invalid -->
		cmp byte [bx], 'Q'		; NTVDM type 51h sub-segment ?
		jne .notfound_2			; no -->
		mov si, 8			; Offset of device name (if SD device driver sub-segment)
		mov di, imsg.mouse
		push cx
		mov cx, si			; length of name
		repe cmpsb			; blank-padded device name "MOUSE" ?
		pop cx
		jne .notfound_2			;  Device name doesn't match, try next SD sub-segment
		mov ax, ds
		inc ax
		mov ds, ax			; Segment of SD sub-segment
				; ds = current SD sub-segment
		mov ax, bp			; Leave paragraph value in bp
		test ax, 0F000h			; Would *16 cause an overflow?
		jnz .notfound_3			;  Then too large -->
		push cx
		mov cl, 4
		shl ax, cl			; *16
		pop cx
				; ax = current SD sub-segment size in byte
.andy:
		mov di, imsg.andy
		push cx
		mov cx, imsg.andy_size
		call findstring			; String "Andy Watson"?
		pop cx
		jc .notfound_3
.ntdos:
		mov di, imsg.ntdos
		push cx
		mov cx, imsg.ntdos_size
		call findstring			; String "Windows NT MS-DOS subsystem Mouse Driver"?
		pop cx
		jnc .found			; (NC)
.notfound_3:
		mov ax, ds
.notfound_2:
		cmp cx, bp
		je .notfound_1			; End of SD memory block, get next MCB
		add ax, bp			; Address next SD sub-MCB
		sub cx, bp
		jmp short .loopsub		; Try next SD sub-segment
.notfound_1:
		add ax, cx			; Address next MCB
		test dx, dx			; Non-zero if 'Z' MCB
		jz .loop			; If not at end of MCB chain, try next
		; jmp short .notnt		;  Otherwise, not found
 .error:
 .notnt:
		stc
.found:
		push cs
		pop ds				; restore ds

		pop bx				; saved UMB link state
		mov ax, 5803h
		pushf
		int 21h				; Set UMB link state
		popf
		retn

findstring:
		xor si, si
.loop:
		push si
		add si, cx
		jc .notfound_c
		dec si				; The largest offset we need for this compare
		cmp ax, si
 .notfound_c:
		pop si
		jb .return			; Not found if at top of memory block -->
		push di
		push si
		push cx
		repe cmpsb			; String somewhere inside program?
		pop cx
		pop si
		pop di
		je .return			;  Yes, proceed --> (if ZR, NC)
		inc si				; Increase pointer by one
		jmp short .loop			;  Try next address
.return:
		retn
%endif


		; Move paragraphs
		;
		; INP:	ax:0-> source
		;	dx:0-> destination
		;	cx = number of paragraphs
		; CHG:	-
		; Note:	Doesn't work correctly on HMA; doesn't always wrap to LMA either.
		;	Do not provide a wrapped/HMA source or destination!
init_movp:
	push cx
	push ds
	push si
	push es
	push di

	cmp ax, dx		; source above destination ?
	ja .up			; yes, move up (forwards) -->
	je .return		; same, no need to move -->
	push ax
	add ax, cx		; (expected not to carry)
	cmp ax, dx		; end of source is above destination ?
	pop ax
	ja .down		; yes, move from top down -->
	; Here, the end of source is below-or-equal the destination,
	;  so they do not overlap. In this case we prefer moving up.

.up:
	push ax
	push dx
.uploop:
	mov ds, ax
	mov es, dx
	xor di, di
	xor si, si		; -> start of segment
	sub cx, 1000h		; 64 KiB left ?
	jbe .uplast		; no -->
	push cx
	mov cx, 10000h /2
	rep movsw		; move 64 KiB
	pop cx
	add ax, 1000h
	add dx, 1000h		; -> next segment
	jmp short .uploop	; proceed for more -->
.uplast:
	add cx, 1000h		; restore counter
	shl cx, 1
	shl cx, 1
	shl cx, 1		; *8, paragraphs to words
	rep movsw		; move last part
	pop dx
	pop ax
	jmp short .return

.down:
	std			; _AMD_ERRATUM_109_WORKAROUND as below
.dnloop:
	sub cx, 1000h		; 64 KiB left ?
	jbe .dnlast		; no -->
	push ax
	push dx
	add ax, cx
	add dx, cx
	mov ds, ax		; -> 64 KiB not yet moved
	mov es, dx
	pop dx
	pop ax
	mov di, -2
	mov si, di		; moved from last word down
	push cx
	mov cx, 10000h /2
	rep movsw		; move 64 KiB
	pop cx
	jmp short .dnloop	; proceed for more -->
.dnlast:
	add cx, 1000h		; restore counter
	shl cx, 1
	shl cx, 1
	shl cx, 1		; *8, paragraphs to words
	mov di, cx
	dec di
	shl di, 1		; words to offset, -> last word
	mov si, di
	mov ds, ax
	mov es, dx		; first segment correct


	numdef AMD_ERRATUM_109_WORKAROUND, 1
%if 0

Jack R. Ellis pointed out this erratum:

Quoting from https://www.amd.com/system/files/TechDocs/25759.pdf page 69:

109   Certain Reverse REP MOVS May Produce Unpredictable Behavior

Description

In certain situations a REP MOVS instruction may lead to
incorrect results. An incorrect address size, data size
or source operand segment may be used or a succeeding
instruction may be skipped. This may occur under the
following conditions:

* EFLAGS.DF=1 (the string is being moved in the reverse direction).

* The number of items being moved (RCX) is between 1 and 20.

* The REP MOVS instruction is preceded by some microcoded instruction
  that has not completely retired by the time the REP MOVS begins
  execution. The set of such instructions includes BOUND, CLI, LDS,
  LES, LFS, LGS, LSS, IDIV, and most microcoded x87 instructions.

Potential Effect on System

Incorrect results may be produced or the system may hang.

Suggested Workaround

Contact your AMD representative for information on a BIOS update.

%endif

%if _AMD_ERRATUM_109_WORKAROUND
	jcxz @FF
	cmp cx, 20
	ja @FF
@@:
	movsw
	loop @B
@@:
%endif
	rep movsw		; move first part
	cld
.return:
	pop di
	pop es
	pop si
	pop ds
	pop cx
	retn


%if _BOOTLDR
		; only called for boot-loaded mode
init_getc_bootldr:
	xor ax, ax
	int 16h
	retn
%endif

init_putsz_cs:
	push ax
	push bx
	push cx
	push dx
	push ds
	push es
	push di
	 push cs
	 pop es
	 push cs
	 pop ds
	mov di, dx			; es:di-> string
	xor al, al
	mov cx, -1
	repne scasb			; search zero
	not cx
	dec cx				; cx = length of message
	pop di
	call init_puts_ds
	pop es
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	retn

%if _BOOTLDR
init_putsz_cs_bootldr:
	push ax
	push bx
	push cx
	push dx
	push ds
	push es
	push di
	 push cs
	 pop es
	 push cs
	 pop ds
	mov di, dx			; es:di-> string
	xor al, al
	mov cx, -1
	repne scasb			; search zero
	not cx
	dec cx				; cx = length of message
	pop di
	call init_puts_ds_bootldr
	pop es
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	retn
%endif

init_puts_ds:
%if _BOOTLDR
	testopt [ss:internalflags], nodosloaded
	jz @F

init_puts_ds_bootldr:
	push si
	push bp
	mov si, dx
	jcxz .return
.loop:
	lodsb
	mov bx, 0007
	mov ah, 0Eh
	int 10h
	loop .loop
.return:
	pop bp
	pop si
	retn

@@:
%endif
	mov bx, 1			; standard output
	mov ah, 40h			; write to file
	jcxz @F
	int 21h
@@:
	retn



%if _BOOTLDR
		; Initial entry when boot loading.

		; ds = ss = debugger data segment
		; (ds - 1) = image ident prefix paragraph
boot_old_initcode:
	cld

d4	call init_d4message
d4	asciz "In boot loader; press any key",13,10
d4	call init_d4pauseforkey

	mov word [execblk.cmdline], 80h
	mov byte [81h], 0Dh
	mov byte [fileext], EXT_OTHER	; empty file name and command line as per N
%endif	; _BOOTLDR

old_initcode:
	cld
	d0bp
	mov ax, ds
	mov word [execblk.cmdline + 2], ax
	mov word [execblk.fcb1 + 2], ax
	mov word [execblk.fcb2 + 2], ax	; set up parameter block for exec command
	mov word [pspdbg], ax

%if _IMMASM  && !_IMMASM_AUXBUFF
	add ax, (immasm_buffer + DATASECTIONFIXUP) >> 4
	mov word [immseg], ax
%endif

	push ds
	mov ax, 40h
	mov ds, ax
	mov ax, word [82h]	; end of circular keypress buffer
	mov dx, word [80h]	; start of circular buffer
	test ax, ax
	jz .forcekeybuffer
	test dx, dx
	jz .forcekeybuffer
	mov bx, ax
	sub bx, dx		; cmp end, start
	jbe .forcekeybuffer	; below or equal is invalid -->
	test bl, 1		; even amount of bytes ?
	jnz .forcekeybuffer	; no, invalid -->
	mov bx, word [1Ah]	; current head of circular buffer
	cmp bx, ax
	jae .forcekeybuffer
	sub bx, dx
	jb .forcekeybuffer
	test bl, 1
	jnz .forcekeybuffer	; invalid -->
	mov bx, word [1Ch]	; current tail of circular buffer
	cmp bx, ax
	jae .forcekeybuffer
	sub bx, dx
	jb .forcekeybuffer
	test bl, 1
	jz @F			; valid -->
.forcekeybuffer:
	pop ds
	mov word [io_end_buffer], 3Eh
	mov word [io_start_buffer], 1Eh
	db __TEST_IMM8		; (skip pop)
@@:
	pop ds

%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz .checkio
d4	call init_d4message
d4	asciz "Common initialisation, determining processor type now",13,10
	jmp .determineprocessor
.checkio:
%endif
		; Check for console input vs. input from a file or other device.
		; This has to be done early because MS-DOS seems to switch CON
		; to cooked I/O mode only then.
	mov ax, 4400h		; IOCTL get device information
	xor bx, bx		; StdIn
	mov dl, 83h		; default if 21.4400 fails
	int 21h
	test dl, 80h
	jz .inputfile
	clropt [internalflags], inputfile
	test dl, 3
	jz .inputdevice		; if not the console input
	clropt [internalflags], notstdinput
	mov byte [notatty], 0	; it _is_ a tty
.inputdevice:
.inputfile:
	mov ax, 4400h		; IOCTL get device information
	inc bx			; StdOut
	mov dl, 83h		; default if 21.4400 fails
	int 21h
	test dl, 80h
	jz .outputfile
	clropt [internalflags], outputfile
	test dl, 3
	jz .outputdevice	; if not the console output
	clropt [internalflags], notstdoutput
.outputdevice:
.outputfile:

		; Check DOS version
%if _VDD
	push ds
	 push cs
	 pop ds
	 push cs
	 pop es
	call isnt		; NTVDM ?
	pop ds
	jc .isnotnt		; no -->
	setopt [internalflags], runningnt
.isnotnt:
%endif

	mov ax, 3000h		; check DOS version
	int 21h
	xchg al, ah
	cmp ax, ver(3,31)	; MS-DOS version > 3.30 ?
	jb .notoldpacket	; no -->
	setopt [internalflags], oldpacket	; assume Int25/Int26 packet method available
.notoldpacket:
	push ax
	xor bx, bx		; preset to invalid value
	mov ax, 3306h
	int 21h
	test al, al		; invalid, DOS 1.x error -->
	jz .213306invalid
	cmp al, -1		; invalid
.213306invalid:
	pop ax
	je .useoldver
	test bx, bx		; 0.0 ?
	jz .useoldver		; assume invalid -->
	xchg ax, bx		; get version to ax
	xchg al, ah		; strange Microsoft version format
.useoldver:
	cmp ax, ver(7,01)	; MS-DOS version > 7.00 ?
	jb .notnewpacket	; no -->
	setopt [internalflags], newpacket| oldpacket	; assume both packet methods available
.notnewpacket:
%if _VDD
	testopt [internalflags], runningnt
	jz .novdd
	push ds
	 push cs
	 pop ds
	 push cs
	 pop es
	mov si, imsg.vdd	; ds:si-> ASCIZ VDD filename
	mov bx, imsg.dispatch	; ds:bx-> ASCIZ dispatching entry
	mov di, imsg.init	; es:di-> ASCIZ init entry
	clc			; !
	RegisterModule		; register VDD
	pop ds
	jc .novdd		; error ? -->
	mov word [hVdd], ax
	setopt [internalflags], ntpacket| oldpacket	; assume old packet method also available
.novdd:
%endif
.determineprocessor:
d4	call init_d4message
d4	asciz "Determining processor type",13,10

	mov cx, 0121h
	shl ch, cl
	jnz .found_186_plus	; normal 186 masks shift count with 31 -->

		; To make it easier to trace past the long-form pop cx
		;  instruction, we now run it in a subfunction.
	call .detect_nec
	jcxz .found_186_plus	; if it was a nop -->
	jmp .cpudone		; is an actual 8088/8086 -->


		; INP:	-
		; OUR:	cx = 0 if NEC V20 or NEC V30
		;	cx = 1 else
		; CHG:	ax, cx
.detect_nec:
		; The NEC V20/V30 processors do support the 186 extensions
		;  to the instruction set but do not mask the shift count.
		;  Therefore, specifically detect them here. Based on the
		;  text in http://www.textfiles.com/hamradio/v20_bug.txt
	mov ax, sp
	mov cx, 1		; = 1 if on actual 8088/8086
	push cx
	dec cx			; = 0 if on NEC V20/V30

		; NB: Do *NOT* trace this instruction with Trace Flag = 1 and
		;  do *NOT* write a breakpoint at the mov sp instruction,
		;  that is the very next instruction after the pop cx.
		; Doing either leads to locking up the HP 95LX, requiring to
		;  reset the system using Ctrl-Shift-On (which zeroes the
		;  system date and time).
	db 8Fh, 0C1h		; pop r/m16 with cx as operand
				;  (reportedly a nop on the NECs)
	mov sp, ax		; reset stack to known state
	retn

.found_186_plus:
d4	call init_d4message
d4	asciz "Found 186+ processor",13,10
	inc byte [ machine ]	; 1
	push sp
	pop ax
	cmp ax, sp
	jne .cpudone		; 80186 pushes the adjusted value of sp -->

d4	call init_d4message
d4	asciz "Found 286+ processor",13,10
		; Determine the processor type.  This is adapted from code in the
		; Pentium<tm> Family User's Manual, Volume 3:  Architecture and
		; Programming Manual, Intel Corp., 1994, Chapter 5.  That code contains
		; the following comment:
		;
		; This program has been developed by Intel Corporation.
		; Software developers have Intel's permission to incorporate
		; this source code into your software royalty free.
		;
		; Intel 286 CPU check.
		; Bits 12-15 of the flags register are always clear on the
		; 286 processor in real-address mode.
		; Bits 12-15 of the FLAGS register are always set on the
		; 8086 and 186 processor.
	inc byte [ machine ]	; 2
	 pushf			; save IF
	pushf			; get original flags into ax
	pop ax
	or ax, 0F000h		; try to set bits 12-15
	and ax, ~0200h		; clear IF
	push ax			; save new flags value on stack
	popf			; replace current flags value; DI
	pushf			; get new flags
	pop ax			; store new flags in ax
	 popf			; restore IF (in 86 Mode)
	test ax, 0F000h		; if bits 12-15 clear, CPU = 80286
	jz .cpudone		; if 80286 -->

d4	call init_d4message
d4	asciz "Found 386+ processor",13,10
		; Intel 386 CPU check.
		; The AC bit, bit #18, is a new bit introduced in the EFLAGS
		; register on the Intel486 DX cpu to generate alignment faults.
		; This bit cannot be set on the Intel386 CPU.
		;
		; It is now safe to use 32-bit opcode/operands.
subcpu 386
	setopt [internalflags], has386
	inc byte [ machine ]	; 3

%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jz @F

	mov word [reg_fs], fs
	mov word [reg_gs], gs
%macro set_gpr_h 1
	push e %+ %1
	pop %1
	pop word [reg_e %+ %1 + 2]
%endmacro
	set_gpr_h ax
	set_gpr_h bx
	set_gpr_h cx
	set_gpr_h dx
	set_gpr_h bp
	set_gpr_h si
	set_gpr_h di
		; esph and eiph remain zero
	pushfd
	popfw
	pop word [reg_efl + 2]
%endif
@@:

	mov bx, sp		; save current stack pointer to align
	and sp, ~3		; align stack to avoid AC fault
	pushfd			; push original EFLAGS
	pop eax			; get original EFLAGS
	mov ecx, eax		; save original EFLAGS in ECX (including IF)

	xor eax, 40000h		; flip AC bit in EFLAGS
	and ax, ~0200h		; clear IF
	push eax		; put new EFLAGS value on stack
	popfd			; replace EFLAGS value; DI
	pushfd			; get new EFLAGS
	pop eax			; store new EFLAGS value in EAX
	mov ax, cx		; ignore low bits (including IF)
	cmp eax, ecx
	je .cpudone_stack_eax_equals_ecx	; if 80386 -->

d4	call init_d4message
d4	asciz "Found 486+ processor",13,10
		; Intel486 DX CPU, Intel487 SX NDP, and Intel486 SX CPU check.
		; Checking for ability to set/clear ID flag (bit 21) in EFLAGS
		; which indicates the presence of a processor with the ability
		; to use the CPUID instruction.
	inc byte [ machine ]	; 4
	mov eax, ecx		; get original EFLAGS
	xor eax, 200000h	; flip ID bit in EFLAGS
	and ax, ~0200h		; clear IF
	push eax		; save new EFLAGS value on stack
	popfd			; replace current EFLAGS value; DI
	pushfd			; get new EFLAGS
	pop eax			; store new EFLAGS in EAX
	mov ax, cx		; ignore low bits (including IF)

.cpudone_stack_eax_equals_ecx:
	push ecx
	popfd			; restore AC,ID bits and IF in EFLAGS (86 Mode)
	mov sp, bx		; restore sp

	cmp eax, ecx		; check if it's changed
	je .cpudone		; if it's a 486 (can't toggle ID bit) -->

d4	call init_d4message
d4	asciz "Found processor with CPUID support",13,10
		; Execute CPUID instruction.
subcpu 486		; NASM (at least 2.10rc1) handles cpuid itself as a
			;  586+ instruction, but we know better. So this
			;  part is declared for 486 compatibility, and only
			;  the cpuid instructions are emitted with 586
			;  compatibility to appease NASM.
%if 0
d4	call init_d4message
d4	asciz "CPUID will NOT be executed, to work around official DOSBox releases",13,10
d4	jmp .cpudone
%endif
	xor eax, eax		; set up input for CPUID instruction
d4	call init_d4message
d4	asciz "Executing CPUID 0",13,10
	  [cpu 586]
	 cpuid
	  __CPU__
d4	call init_d4message
d4	asciz "CPUID 0 executed",13,10
	cmp eax, byte 1
	jb .cpudone		; if 1 is not a valid input value for CPUID
	xor eax, eax		; otherwise, run CPUID with eax = 1
	inc eax
d4	call init_d4message
d4	asciz "Executing CPUID 1",13,10
	  [cpu 586]
	 cpuid
	  __CPU__
d4	call init_d4message
d4	asciz "CPUID 1 executed",13,10
%if _MMXSUPP
	test edx, 80_0000h
	setnz byte [has_mmx]
%endif

	mov al, ah
	and al, 0Fh		; bits 8..11 are the model number
	cmp al, 6
	jb .below686		; if < 6
	mov al, 6		; if >= 6, set it to 6
.below686:
	mov byte [ machine ], al; save machine type (486, 586, 686+)

.cpudone:
subcpureset			; subcpu 486
subcpureset			; subcpu 386
d4	call init_d4message
d4	asciz "Determining floating-point unit",13,10

		; Next determine the type of FPU in a system and set the mach_87
		; variable with the appropriate value.  All registers are used by
		; this code; none are preserved.
		;
		; Coprocessor check.
		; The algorithm is to determine whether the floating-point
		; status and control words can be written to.  If not, no
		; coprocessor exists.  If the status and control words can be
		; written to, the correct coprocessor is then determined
		; depending on the processor ID.  The Intel 386 CPU can
		; work with either an Intel 287 NDP or an Intel 387 NDP.
		; The infinity of the coprocessor must be checked
		; to determine the correct coprocessor ID.
	mov al, byte [ machine ]
	mov byte [ mach_87 ], al	; by default, set mach_87 to machine
	inc byte [ has_87 ]
	mov byte [encodedmach87], 0Ch
	cmp al, 5			; a Pentium or above always will have a FPU
	jae .fpudone
	dec byte [ has_87 ]		; assume no FPU
	mov byte [encodedmach87], 0C0h

	fninit				; reset FPU
	mov al, -1			; initialise with a non-zero value
	push ax
	mov bx, sp
	fnstsw word [ss:bx]		; save FP status word
	pop ax				; retrieve it
	test al, al
	jnz .fpudone			; if no FPU present

		; al = 0 here
	push ax
	fnstcw word [ss:bx]		; save FP control word
	pop ax				; retrieve it
	and ax, 103Fh			; see if selected parts look OK
	cmp ax, byte 3Fh
	jne .fpudone			; if no FPU present
	inc byte [ has_87 ]		; there's an FPU
	mov byte [encodedmach87], 0Ch

		; If we're using a 386, check for 287 vs. 387 by checking whether
		; +infinity = -infinity.
	cmp byte [ machine ], 3
	jne .fpudone			; if not a 386
[cpu 386]
	fld1				; must use default control from FNINIT
	fldz				; form infinity
	fdivp ST1			; 1 / 0 = infinity
	fld ST0
	fchs				; form negative infinity
	fcompp				; see if they are the same and remove them
	fstsw ax
	sahf				; look at status from FCOMPP
	jne .fpudone			; if they are different, then it's a 387
	dec byte [ mach_87 ]		; otherwise, it's a 287
	mov byte [encodedmach87], 0C2h
__CPU__
.fpudone:

apply_patches:
%if _ONLY386
	testopt [internalflags], has386
	jnz @F				; okay -->
 %if _BOOTLDR
	testopt [internalflags], nodosloaded
	lahf				; remember status
 %endif
	mov dx, imsg.no386
	call init_putsz_cs		; display the error
 %if _BOOTLDR
	sahf
	jnz init_booterror.soft		; abort for loader -->
 %endif
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz init_device_error_late
%endif
	mov ax, 4C01h
	int 21h				; abort our process

@@:
%elif _ONLYNON386
	testopt [internalflags], has386
	jz @F				; okay -->
	mov dx, imsg.386
	call init_putsz_cs		; display the warning
@@:
%endif

		; Determine which patch table to use, then patch
		; out either the 386+ or non-386 code as appropriate.
	mov es, [code_seg]
	testopt [internalflags], has386
	jz @F
	mov si, patch_386_table		; table of patches to set for 386+
%if __patch_386_table_method == 1
	mov cx, patch_386_table_size_w
	call .patch1
%else
	call .patch2
%endif
	jmp .patch_code1_end

@@:
%ifn _ONLYNON386
	mov byte [es:..@patch_no386_ds], 3Eh	; write a ds prefix
	mov byte [es:..@patch_no386_iret], 0CFh	; write an iret instruction
 %if _PM && _CATCHPMINT214C
	mov byte [es:..@patch_no386_ds_2], 3Eh
	mov byte [es:..@patch_no386_ds_3], 3Eh
	mov byte [ss:..@patch_no386_nop_DATA_ENTRY], 90h
					; write a nop (note the segment!)
 %endif
 %if _PM && _CATCHPMINT41
	mov byte [ss:..@patch_no386_nop_2_DATA_ENTRY], 90h
					; write a nop (note the segment!)
 %endif
 %if _PM
	mov byte [es:..@patch_no386_ds_4], 3Eh
	mov byte [es:..@patch_no386_ds_5], 3Eh	; write some more ds prefixes
 %endif
 %if _CATCHINT07 || _CATCHINT0C || _CATCHINT0D
	mov byte [ss:..@patch_no386_ds_6_DATA_ENTRY], 3Eh
					; write a ds prefix (note the segment!)
 %endif
%endif
	mov si, patch_no386_table	; table of patches to set for 16-bit CPU
%if __patch_no386_table_method == 1
	mov cx, patch_no386_table_size_w
	call .patch1
%else
	call .patch2
%endif
	jmp .patch_code1_end

		; Complicated table patch code.
%if __patch_no386_table_method == 2 || __patch_386_table_method == 2 \
	|| __patch_no386_table2_method == 2 || __patch_386_table2_method == 2
.patch2:
	mov di, code_start		; initialise offset
	xor ax, ax			; initialise ah
.looppatch2:
	cs lodsb
	add di, ax			; skip number of bytes to skip
	cmp al, 255			; really repositioning?
	jne .l2patch			; no -->
	xchg ax, di			; (to preserve ah)
	cs lodsw			; ax = new address
	xchg ax, di			; di = new address
.l2patch:
	cs lodsb
	mov cx, ax			; cx = number of bytes to patch
	jcxz .patchesdone		; end of table -->
	mov al, 90h			; patch to NOP
	rep stosb			; patch as many bytes as specified
	jmp short .looppatch2
%endif

		; Simple table patch code.
%if __patch_no386_table_method == 1 || __patch_386_table_method == 1 \
	|| __patch_no386_table2_method == 1 || __patch_386_table2_method == 1
.patch1:
	jcxz .patchesdone
.looppatch1:
	cs lodsw			; load address of patch
	xchg bx, ax			; (set bx = ax, CHG ax)
	mov byte [es:bx], 90h		; patch
	loop .looppatch1
%endif
.patchesdone:
	retn

.patch_code1_end:

%if _DUALCODE
	mov dx, es
	add dx, ldebug_code_size_p
	mov word [code2_seg], dx
patch_relocate:

 %if ! _PM
	mov si, relocate_from_code
	mov di, relocate_from_code.end
	call .patch
	mov si, relocate_from_code2
	mov di, relocate_from_code2.end
 %endif
	push es
	mov es, dx
	pop dx
 %if ! _PM
	call .patch
	jmp .done

.loop:
	cs lodsw
	xchg bx, ax
	mov word [es:bx], dx
.patch:
	cmp si, di
	jb .loop
	retn

.done:
 %endif

	testopt [internalflags], has386
	jz @F
	mov si, patch_386_table2	; table of patches to set for 386+
%if __patch_386_table2_method == 1
	mov cx, patch_386_table2_size_w
	call apply_patches.patch1
%else
	call apply_patches.patch2
%endif
	jmp .patch_code2_end

@@:
	mov si, patch_no386_table2	; table of patches to set for 16-bit CPU
%if __patch_no386_table2_method == 1
	mov cx, patch_no386_table2_size_w
	call apply_patches.patch1
%else
	call apply_patches.patch2
%endif
.patch_code2_end:
%endif



%if _DEVICE
		; This must be done after CPU detection
		;  because we want to get the high parts
		;  of the registers only initialised here.
	push ds
	pop es
	mov si, regs
	mov di, device_quittable_regs
	mov cx, words(regs.size)
	rep movsw
%endif


		; Check for dosemu. This is done for the boot loaded instance
		; too, as we might be running as DOS inside dosemu.
%if _DOSEMU
	mov ax, 0F000h
	mov es, ax
	push ds
	 push cs
	 pop ds			; avoid "repe cs cmpsw" (8086 bug)
	mov di, 0FFF5h
	mov si, imsg.dosemudate
	mov cx, 4
	repe cmpsw		; running in DosEmu?
	pop ds
	jne .dosemuchecked
	setopt [internalflags], runningdosemu
.dosemuchecked:
%endif

	push ds
	pop es			; => lDEBUG_DATA_ENTRY

	mov di, line_in
	mov al, 255
	stosb
	mov al, 0
	stosb
	mov al, 13
	stosb				; overwrite line_in beginning

	mov sp, stack_end		; stack pointer (paragraph aligned)
	mov di, ..@init_first
	mov cx, ..@init_behind - ..@init_first
	xor ax, ax
	rep stosb			; initialise breakpoint lists, line_out
%if 1
%if ..@init_behind != stack
	mov di, stack
%endif
	mov cx, stack_end - stack
	mov al, 5Eh
	rep stosb			; initialise the stack
%endif

	mov byte [ trim_overflow ], '0'	; initialise line_out so the trimputs loop doesn't overflow
	mov word [line_out_overflow], 2642h


%if _AREAS_HOOK_SERVER
	mov ax, ds
	mov word [ddebugareas.next + 2], ax
	mov word [ddebugareas.prev + 2], ax
	mov word [..@patch_entry_seg], ax
%endif

%if _AREAS && _AREAS_HOOK_CLIENT
	mov ax, word [code_seg]
	xor dx, dx
	mov cx, 4
@@:
	shl ax, 1
	rcl dx, 1
	loop @B

	add word [areas_sub + areastrucsubLinear], ax
	adc word [areas_sub + areastrucsubLinear + 2], dx
	add word [areas_sub + areastrucsubLinearEnd], ax
	adc word [areas_sub + areastrucsubLinearEnd + 2], dx

	add word [areas_fun + areastrucfunLinear], ax
	adc word [areas_fun + areastrucfunLinear + 2], dx
	add word [areas_fun + areastrucfunLinearEnd], ax
	adc word [areas_fun + areastrucfunLinearEnd + 2], dx
%endif


%if _DEBUG && _DEBUG_COND && _DEBUG_COND_DEFAULT_ON
	setopt [internalflags6], dif6_debug_mode
	setopt [options6], opt6_debug_mode
	setopt [startoptions6], opt6_debug_mode
%endif

%if _CATCHINT06 && _DETECT95LX
	mov ax, 4DD4h
	xor bx, bx
	int 15h				; HP 95LX/100LX/200LX detect
	cmp bx, 4850h			; "HP" reversed
	jne @F
	cmp cx, 0101h			; 95LX ?
	jne @F				; no -->

	mov ax, word [inttab.i2D + 1]
	mov word [inttab.i06 + 1], ax	; overwrite i06 entry with i2D
	mov al, byte [inttab.i2D]
	mov byte [inttab.i06], al	; interrupt number too
	dec word [inttab_number_variable]	; remember one less in use
@@:
%endif

	mov ah, 0Fh
	int 10h
	mov byte [vpage], bh


%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz initdos
d4	call init_d4message
d4	asciz "386-related patches applied, boot initialisation proceeding",13,10


%if CATCHINTAMOUNT
		; Set up interrupt vectors.

		; ds still => lDEBUG_DATA_ENTRY
%if _CATCHINT06 && _DETECT95LX
	mov cx, word [inttab_number_variable]
%else
	mov cx, inttab_number
%endif
	mov si, inttab
.bootintloop:

		; assumes ss = lDEBUG_DATA_ENTRY
	ss lodsb
	xor bx, bx
	mov ds, bx
	mov bl, al
	add bx, bx
	add bx, bx
	xchg ax, di
	ss lodsw		; get address of IISP header
	xchg ax, di
%if _DEBUG && !_DEBUG_COND
				; vectors are set only when debuggee runs
%else
 %if _DEBUG
	testopt [ss:internalflags6], dif6_debug_mode
	jnz @F
 %endif
	push word [ bx+2 ]
	push word [ bx ]	; get vector
	pop word [ ss:di + ieNext ]
	pop word [ ss:di + ieNext + 2 ]
				; store it
	mov word [ bx+2 ], ss
	mov word [ bx ], di	; set interrupt vector
@@:
%endif
	loop .bootintloop
%endif


	push ss
	pop ds
	push ss
	pop es

d4	call init_d4message
d4	asciz "Jumping to final boot initialisation code",13,10
	push word [code_seg]
	push word [cs:.word_initcont.boot_entry]
	retf

	align 2, db 0
.word_initcont.boot_entry:
	dw initcont.boot_entry
%endif

initdos:
%if _MCB || _INT
	mov ax, 5802h
	int 21h
	xor ah, ah		; some "DOS" only return al
	push ax			; save UMB link

getfirstmcb:
	mov ah, 52h		; get list of lists
	int 21h
	mov ax, word [ es:bx-2 ]; start of MCBs
	mov word [firstmcb], ax

getfirstumcb:
			; We try to get the first UMCB for gateout
			;  for now. To harden our code it should
			;  not be assumed that the address is of
			;  a valid MCB. However, it is fine to
			;  compare an actual MCB address with it.
  %if _GUARD_86M_INT2F
	push es
	xor ax, ax
	mov es, ax		; (only used in 86 Mode)
	mov ax, [es:2Fh * 4]
	cmp ax, -1
	je @F			; --> (ZR)
	or ax, [es:2Fh * 4 + 2]
@@:
	pop es
	jz .determine
  %endif
	mov ax, 1261h		; PTS-DOS: Get first UMCB
	stc
	int 2Fh
	jc .determine		; not supported -->
	inc ax
	cmp ax, byte 2		; -1, 0, 1 ?
	jbe .determine		; not supported (or none) -->
	dec ax
	mov word [ firstumcb ], ax	; set UMB
	jmp short .got		; got it -->

.determine:
	mov ax, 5803h
	xor bx, bx
	int 21h			; disable UMB link, leave only LMA chain
	jc .none		; that isn't supported either -->

	mov ax, word [firstmcb]
	push ds
	mov dx, ax		; first MCB
	xor bx, bx		; use offsets from bx, not addresses
.looplmb:
	mov ds, ax
	inc ax
	add ax, word [ bx + 3 ]	; next MCB's address
	cmp byte [ bx ], 'M'
	je .looplmb		; not last -->
	cmp byte [ bx ], 'Z'
	jne .none_pop_ds	; corrupted -->
	xchg ax, dx		; dx = what we assume to be the first UMA chain MCB
				; ax = first MCB

	push ax
	inc bx			; = 1
	mov ax, 5803h
	int 21h			; enable UMB link, include UMA chain
	pop ax
	jc .none		; so we can disable it but not enable? -->

	dec bx			; = 0
	xor cx, cx		; flag if assumed first UMCB found
.loopumb:
	cmp ax, dx
	jne .notlastlmb
	inc cx			; there it is
.notlastlmb:
	mov ds, ax
	cmp byte [ bx ], 'M'
	jne .islastumb?		; last or corrupted -->
	inc ax
	add ax, word [ bx + 3 ]
	jmp short .loopumb	; process next -->
.islastumb?:
	cmp byte [ bx ], 'Z'
	pop ds
	jne .none		; corrupted -->
	jcxz .none		; didn't find that UMCB -->
			; The MCB at dx which was behind the one that contained the 'Z'
			; signature when we disabled the UMB link is now a valid MCB in
			; the MCB chain after we enabled the UMB link. All previous MCBs
			; are now 'M'.
	mov word [ firstumcb ], dx
.none:
	db __TEST_IMM8		; (skip pop)
.none_pop_ds:
	pop ds
.got:
	pop bx
	mov ax, 5803h
	int 21h			; restore UMB link
%endif

	mov ah, 34h
	int 21h
	mov word [pInDOS + so16aOffset], bx
	mov word [pInDOS + so16aSegSel], es
%if _PM
	mov word [pInDOS + so16aSegment], es
%endif

		; get address of DOS swappable DATA area
		; to be used to get/set PSP and thus avoid DOS calls
		; will not work for DOS < 3
%if _USESDA
	push ds
	mov ax, 5D06h
	stc				; initialise to CY
	int 21h
	mov ax, ds
	pop ds
	jc .noSDA			; if CY returned, not supported -->
	mov es, ax			; es:si -> SDA
 %if _DEVICE
	push ax
	mov ah, 51h
	int 21h				; bx = current PSP
	pop ax
 %else
	mov bx, ds			; bx = our PSP (= current PSP in app mode)
 %endif
	cmp word [es:si + 10h], bx	; does this seem like the current PSP field ?
	jne .noSDA			; no -->
	mov word [pSDA + so16aOffset], si
	mov word [pSDA + so16aSegSel], ax
%if _PM
	mov word [pSDA + so16aSegment], ax
%endif
.noSDA:
%endif

	mov ax, 3531h
	int 21h
	mov bx, es
%if _USESDA
	cmp bx, word [pSDA + so16aSegSel]
	jne @F
%endif
	cmp bx, word [pInDOS + so16aSegSel]
	jne @F

	setopt [internalflags2], dif2_int31_segment
@@:


commandline:
	push ss
	pop es

		; Interpret switches and erase them from the command line.
	mov ax, 3700h			; get switch character
	mov dl, '/'			; preset with default value
	int 21h
	mov byte [ switchar ], dl
	cmp dl, '/'
	jne .notslash
	mov byte [ swch1 ], dl
.notslash:
	mov si, DTA+1
.blankloop:
	lodsb
	cmp al, 32
	je .blankloop
	cmp al, 9
	je .blankloop

		; Process the /? switch (or the [switchar]? switch).
		; If switchar != / and /? occurs, make sure nothing follows.
	cmp al, byte [switchar]
	je .switch		; if switch character -->
	cmp al, '/'
	jne .noswitches		; if not the help switch -->
	mov al, byte [ si ]
	cmp al, '?'
	jne .noswitches		; if not /?
	mov al, byte [ si+1 ]
	cmp al, 32
	je .help		; if nothing after /?
	cmp al, 9
	je .help		; ditto
	cmp al, 13
	jne .noswitches		; if something after /? -->

		; Print a help message
.help:
	push ds
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jz .help_not_device

		; We modify the device command line here.
		;  Is that wise? Seems to work though.
	mov si, word [reg_ebx]
	mov ds, word [reg_es]
	lds si, [si + 12h]
	push si
@@:
	lodsb
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 0
	je @F
	cmp al, 13
	je @F
	cmp al, 10
	je @F
	jmp @B

@@:
	mov byte [si - 1], 0
	pop si
	jmp .help_common

.help_not_device:
%endif
	mov ax, word [2Ch]	; => environment
	test ax, ax
	jz .help.no_name
	mov ds, ax
	xor si, si
@@:
	lodsb
	test al, al
	jnz @B
	lodsb
	test al, al
	jnz @B
	lodsw
	cmp ax, 1
	jne .help.no_name
.help_common:
@@:
	mov bx, si
@@:
	lodsb
	cmp al, 'a'
	jb @F
	cmp al, 'z'
	ja @F
	sub byte [si - 1], 'a' - 'A'
@@:
	cmp al, '\'
	je @BBB
	cmp al, '/'
	je @BBB
	test al, al
	jnz @BB

	mov cx, si
	dec cx
	sub cx, bx

@@:
	dec si
	cmp si, bx
	jb @F
	cmp byte [si], '.'
	jne @B

	mov cx, si
	sub cx, bx
@@:
	jcxz .help.no_name
@@:
	mov dx, imsg.help.1	; command-line help message
	call init_putsz_cs
	push bx
	mov dx, bx
	call init_puts_ds
	mov dx, imsg.help.2
	call init_putsz_cs
	pop dx
	call init_puts_ds
	mov dx, imsg.help.3
	call init_putsz_cs
	pop ds
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz init_device_error_late
%endif
	mov ax, 4C00h
	int 21h			; done

.help.no_name:
	push cs
	pop ds
	mov bx, imsg.help.defaultfilename
	mov cx, imsg.help.defaultfilename.length
	jmp @B


		; Do the (proper) switches.
.switch:lodsb
	cmp al,'?'
	je .help		; if -?
	cmp al, 'c'
	je .switch_c
	cmp al, 'C'
	je .switch_c
	cmp al, 'f'
	je .switch_f
	cmp al, 'F'
	je .switch_f
	cmp al, 'e'
	je .switch_e
	cmp al, 'E'
	je .switch_e
	cmp al, 'b'
	je .switch_b
	cmp al, 'B'
	je .switch_b
%if _VXCHG
	cmp al, 'v'
	je .switch_v
	cmp al, 'V'
	je .switch_v
%endif
%if _ALTVID
	cmp al, '2'
	je .switch_2
%endif
%if _MCLOPT
	cmp al, 'm'
	je .switch_m
	cmp al, 'M'
	je .switch_m
%endif

%if _SYMBOLIC
	cmp al, 's'
	je .switch_s
	cmp al, 'S'
	je .switch_s
%endif

%if _DEBUG && _DEBUG_COND
	cmp al, 'd'
	je .switch_d
	cmp al, 'D'
	je .switch_d
%endif

		; Other switches may go here.
	mov [ cs:imsg.invalidswitch_a ], al
	mov dx, imsg.invalidswitch	; Invalid switch
..@init_cmdline_error:
	call init_putsz_cs	; print string
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz init_device_error_late
%endif
	mov ax, 4C01h		; Quit and return error status
	int 21h

.switch_c:
@@:
	lodsb
	cmp al, '='
	je @B
	cmp al, ':'
	je @B

	mov di, cmdline_buffer
	mov ah, 0		; initialise to 0 = unquoted
	db __TEST_IMM16
.switch_c_loop:
	stosb
.switch_c_quoted:
	lodsb
.switch_c_loop_after_semicolon:

	cmp al, 13
	je .switch_c_eol
	cmp al, ah		; close quote mark ?
	jne @F			; no -->
	cmp al, 0
	je .switch_c_eol
	mov ah, 0		; continue unquoted
	jmp .switch_c_quoted	; and load next character -->

@@:
	test ah, ah		; currently quoted ?
	jnz .switch_c_not_blank	; yes -->

	cmp al, '"'		; open quote mark ?
	je @F
	cmp al, "'"
	jne @FF			; no -->
@@:
	mov ah, al		; remember quoted state
	jmp .switch_c_quoted	; and load next character -->

@@:
	cmp al, 32		; blank while unquoted ?
	je .unquoted_blank
	cmp al, 9
	je .unquoted_blank	; yes -->
.switch_c_not_blank:
	cmp al, ';'		; unescaped semicolon ?
	jne .switch_c_not_semicolon
	mov al, 13		; yes, replace by CR
	stosb
	test ah, ah
	jz .switch_c_quoted
	call init_skipwhite
	jmp .switch_c_loop_after_semicolon

.switch_c_not_semicolon:
	cmp al, '\'		; escape ?
	jne .switch_c_loop	; no, store literal -->
	lodsb			; load escaped character
				;  (may be backslash, semicolon, quote)
	cmp al, 13		; guard against EOL
	jne .switch_c_loop
.switch_c_error:
	mov dx, imsg.switch_c_error
	jmp ..@init_cmdline_error

.switch_c_eol:
	test ah, ah		; in quoted state ?
	jnz .switch_c_error	; yes, error -->
.unquoted_blank:
	mov al, 0
	stosb			; terminate command line buffer
	setopt [internalflags3], dif3_input_cmdline
	dec si
	jmp .blankloop


.switch_b:
	mov byte [cs:.breakpoint], 0CCh	; SMC in section init, set point
	jmp .blankloop


.switch_f:
	lodsb
	mov bx, opt6_flat_binary + opt6_big_stack
	dec si
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 13
	je @F
	mov bx, opt6_flat_binary
	inc si
	cmp al, '+'
	je @F
	xor bx, bx
	cmp al, '-'
	je @F
.switch_f_error:
	mov dx, imsg.switch_f_error
	jmp ..@init_cmdline_error
@@:
	clropt [options6], opt6_flat_binary
	or word [options6], bx
	jmp .blankloop

.switch_e:
	lodsb
	mov bx, opt6_big_stack
	dec si
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 13
	je @F
	inc si
	cmp al, '+'
	je @F
	xor bx, bx
	cmp al, '-'
	je @F
.switch_e_error:
	mov dx, imsg.switch_e_error
	jmp ..@init_cmdline_error
@@:
	clropt [options6], opt6_big_stack
	or word [options6], bx
	jmp .blankloop

%if _MCLOPT
.switch_m:
	lodsb
	mov bl, 20h
	dec si
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 13
	je @F
	inc si
	cmp al, '+'
	je @F
	mov bl, 8
	cmp al, '-'
	je @F
.switch_m_error:
	mov dx, imsg.switch_m_error
	jmp ..@init_cmdline_error
@@:
	mov byte [master_pic_base], bl
	jmp .blankloop
%endif

%if _VXCHG
.switch_v:
	lodsb
	mov bx, opt6_vv_mode
	dec si
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 13
	je @F
	inc si
	cmp al, '+'
	je @F
	xor bx, bx
	cmp al, '-'
	je @F
.switch_v_error:
	mov dx, imsg.switch_v_error
	jmp ..@init_cmdline_error
@@:
	clropt [options6], opt6_vv_mode
	or word [options6], bx
	jmp .blankloop
%endif

%if _DEBUG && _DEBUG_COND
.switch_d:
	lodsb
	mov bx, dif6_debug_mode
	dec si
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 13
	je @F
	inc si
	cmp al, '+'
	je @F
	xor bx, bx
	cmp al, '-'
	je @F
.switch_d_error:
	mov dx, imsg.switch_d_error
	jmp ..@init_cmdline_error
@@:
	clropt [internalflags6], dif6_debug_mode
	clropt [options6], opt6_debug_mode
	or word [internalflags6], bx
	or word [options6], bx
 %if dif6_debug_mode != opt6_debug_mode
  %error Mismatch of flag and option
 %endif
	jmp .blankloop
%endif


%if _ALTVID
.switch_2:
	lodsb
	mov bl, 1Eh			; "push ds"
	dec si
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	cmp al, 13
	je @F
	inc si
	cmp al, '+'
	je @F
	mov bl, 0C3h			; "retn"
	cmp al, '-'
	je @F
.switch_2_error:
	mov dx, imsg.switch_2_error
	jmp ..@init_cmdline_error
@@:
	mov es, [code_seg]
	mov byte [es:setscreen], bl
	push ss
	pop es
	cmp bl, 0C3h
	je .noaltvid
	mov ax, 1A00h
	int 10h
	cmp al, 1Ah
	jnz .noaltvid
	cmp bh, 0
	jz .noaltvid

	push ds
	mov ax, 40h
	mov ds, ax
	mov dx, [63h]
	pop ds
	xor dl, 60h
	mov [oldcrtp], dx
	mov al, 7
	cmp dl, 0B4h
	jz @F
	mov al, 3
@@:
	mov [oldmode], al
	mov al, 0Eh
	out dx, al
	inc dx
	in al, dx
	mov ah, al
	dec dx
	mov al, 0Fh
	out dx, al
	inc dx
	in al, dx
	mov bl, 80
	div bl
	xchg al, ah
	mov [oldcsrpos], ax
.noaltvid:
	jmp .blankloop
%endif


%if _SYMBOLIC
.switch_s:
	mov dx, si
	lodsb
	mov ah, 0		; flag for not quoted
	cmp al, '"'
	je .s_quoted
	cmp al, "'"
	jne .s_unquoted
.s_quoted:
	mov ah, al		; save away our quote mark
	inc dx			; -> behind the quote mark
@@:
	lodsb
	cmp al, 13
	je .switch_s_error
	cmp al, 0
	je .switch_s_error
	cmp al, ah		; closing quote mark ?
	jne @B			; not yet -->
	jmp .s_end

	db __TEST_IMM8		; (skip lodsb)
@@:
	lodsb
.s_unquoted:
	cmp al, 32
	ja @B

.s_end:
	dec si			; -> blank or terminator or closing quote
	push ax
	push si
	mov byte [si], 13	; put in a CR for good measure
	mov si, dx

	push word [errret]
	push word [throwret]
	push word [throwsp]

	push cs
	call .jump

	pop word [throwsp]
	pop word [throwret]	; restore throw destination
	pop word [errret]
	pop si
	pop ax
	mov byte [si], al	; restore if it wasn't CR
		; si -> next character to process
	test dx, dx
	jz @F

	cmp dx, si
	je @F

	mov dx, imsg.switch_s_garbage
	call init_putsz_cs

@@:
	test ah, ah		; was quoted ?
	jz @F			; no -->
	inc si			; skip closing quote mark
@@:
	jmp .blankloop

.switch_s_error:
	mov dx, imsg.switch_s_error
	jmp ..@init_cmdline_error

.jump:
	mov word [errret], ..@switch_s_catch
	mov word [throwret], ..@switch_s_catch
	mov word [throwsp], sp

	push word [code_seg]
	push word [cs:..@word_switch_s_cont]
	retf
%endif


	usesection INIT
.noswitches:
.breakpoint:
	nop			; SMC in section init
		; Feed the remaining command line to the 'n' command.
	dec si
	push si


%if CATCHINTAMOUNT
		; Set up interrupt vectors.
%if _CATCHINT06 && _DETECT95LX
	mov cx, word [inttab_number_variable]
%else
	mov cx, inttab_number
%endif
	mov si, inttab
.intloop:
	lodsb
	mov ah, 35h
	int 21h			; get vector
	xchg ax, di
	lodsw
	xchg ax, di
%if _DEBUG && !_DEBUG_COND
				; vectors are set only when debuggee runs
%else
 %if _DEBUG
	testopt [internalflags6], dif6_debug_mode
	jnz @F
 %endif
	mov word [ di + ieNext ], bx
	mov word [ di + ieNext + 2 ], es
				; store it
	mov dx, di
	mov ah, 25h		; set interrupt vector
	int 21h			; ds => lDEBUG_DATA_ENTRY
@@:
%endif
	loop .intloop
%endif


		; Disabled this. hook2F (debug.asm) now detects this condition.
%if _PM && 0
		; Windows 9x and DosEmu are among those hosts which handle some
		; V86 Ints internally without first calling the interrupt chain.
		; This causes various sorts of troubles and incompatibilities;
		; in our case, hooking interrupt 2Fh would not intercept calls
		; made to the DPMI interface because the host sees them first.
 %if _WIN9XSUPP
  %if _GUARD_86M_INT2F
	push es
	xor ax, ax
	mov es, ax		; (only used in 86 Mode)
	mov ax, [es:2Fh * 4]
	cmp ax, -1
	je @F			; --> (ZR)
	or ax, [es:2Fh * 4 + 2]
@@:
	pop es
	jz @F
  %endif
	mov ax, 1600h		; running in a Win9x DOS box?
	int 2Fh
	cmp al, 4
	jge .no2Fhook		; this is intentionally a signed comparison!
@@:
 %endif
 %if _DOSEMU
	testopt [internalflags], runningdosemu
	jnz .no2Fhook
 %endif
 %if _WIN9XSUPP || _DOSEMU
	jmp short .dpmihostchecked
.no2Fhook:
	setopt [internalflags], nohook2F
.dpmihostchecked:
 %endif
%endif
	push ds
	pop es

set_parent_pra:
		; Save, then modify termination address and parent PSP.
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz .device
%endif
	mov si, TPIV
	mov di, psp22
	movsw
	movsw				; save Int22
	mov dx, debug22
	mov word [ si-4 ], dx
	mov word [ si-2 ], ds		; set pspInt22 (required)
	mov si, 16h
	movsw				; save parent
	mov word [ si-2 ], ds		; set pspParent
	mov ax, 2522h			; set Int22
	int 21h				; (not really required)

		; shrink to required resident size
	push ds
	pop es
	mov ah, 4Ah
	mov bx, word [cs:memsize]

	push word [code_seg]
	push word [cs:.word_initcont]
	retf

	align 2, db 0
.word_initcont:
	dw initcont
%if _SYMBOLIC
..@word_switch_s_cont:
	dw ..@switch_s_cont
%endif

%if _DEVICE
	align 2, db 0
.word_initcont.device:
	dw initcont.device

.device:
	mov si, 80h			; -> command line tail
	mov cx, si			; = 128
	sub sp, cx			; -> buffer on stack
	mov di, sp
	rep movsb			; preserve it

	mov dx, ds
	mov ah, 55h
	clc
	int 21h				; create child PSP

	mov si, TPIV + 4
	mov dx, debug22
	mov word [ si-4 ], dx
	mov word [ si-2 ], ds		; set pspInt22 (required)
 %if _DEVICE_SET_2324
	mov word [ si ], devint23
	mov word [ si + 2 ], ds		; set pspInt23
	mov word [ si + 4 ], devint24
	mov word [ si + 6 ], ds		; set pspInt24
 %endif
	mov si, 16h + 2
	mov word [ si-2 ], ds		; set pspParent
	; mov ax, 2522h			; set Int22
	; int 21h			; (not really required)

	xor ax, ax
	mov word [2Ch], ax		; set environment to none

	mov si, sp			; -> buffer on stack
	mov di, 80h			; -> command line tail buffer in PSP
	mov ax, di			; = 128
	mov cx, di			; = 128
	rep movsb			;
	add sp, ax			; discard buffer

	push word [code_seg]
	push word [cs:.word_initcont.device]
	retf
%endif


		; Skip blanks and tabs
		;
		; INP:	ds:si-> first character
		; OUT:	al = first non-blank character
		;	ds:si-> character behind the first non-blank
		;	NC
		; CHG:	-
		; STK:	1 word
init_skipwhite:
	lodsb

		; Same as above, but first character in al
		;
		; INP:	al = first character
		;	ds:si-> next character
		; OUT:	al = first non-blank character
		;	ds:si-> character behind the first non-blank
		;	NC
		; CHG:	-
		; STK:	1 word
init_skipwh0:
	cmp al, 32
	je init_skipwhite
	cmp al, 9
	je init_skipwhite
	clc
	retn


	usesection lDEBUG_CODE
initcont:
	int 21h				; resize to required
;	jc ...				; (expected to work since it had to be larger. also we hooked ints)

.device:
%if _VXCHG
	call vv_set
%endif

	push ds
	pop es
	call getint2324			; init run2324 to avoid using or displaying NUL vectors

	push ds
	pop es
	pop si
	lodsb
	call nn				; process the rest of the command line

.boot_entry:
	push ds
	pop es				; => lDEBUG_DATA_ENTRY

%if _ALTVID
;	mov al, ALTSCREENBITS
	call setscreen
%endif

	mov si, cmd3
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz @F
	jmp si				; directly jump to cmd3 of the installed image
@@:
%endif
	push si
	jmp ll3				; load a program if one has been given at the command line
