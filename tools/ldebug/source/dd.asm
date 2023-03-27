
%if 0

lDebug D commands - Dump data

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
ddoffset:	dw 0		; offset word for dd
				;  (number of skipped bytes at start of line)
%if _PM
		dw 0		; high word initialised to and fixed at zero
%endif
ddskipped:	dw 0
%if _PM
		dw 0		; high word initialised to and fixed at zero
%endif
ddsize:		dw 1		; size of dd item
ddoffset2:	db 0


	usesection lDEBUG_CODE

..@dd_access_start:

		; D command - hex/ASCII dump.
ddd:
%if _INT || _PM || _MCB || _DSTRINGS || 1
	call uppercase
%endif
	xchg al, ah
	mov al, byte [si - 2]
	call uppercase
	cmp al, 'D'
	xchg al, ah
	jne .not_d_suffix
%if _DSTRINGS
	cmp al, 'Z'		; DZ command ?
	je dz			; yes -->
	cmp al, '$'		; D$ command ?
	je dcpm			; yes -->
	cmp al, '#'		; D# command ?
	je dcounted		; yes -->
	cmp al, 'W'
	jne .notstring
	push ax
	lodsb
	cmp al, '#'		; DW# command ?
	pop ax
	je dwcounted		; yes -->
	dec si
.notstring:
%endif
%if _INT
	cmp al, 'I'		; DI command ?
	jne .notdi
%if 1
	push ax
	lodsb
	dec si
	and al, TOUPPER
	cmp al, 'P'		; distinguish 'di ...' and 'd ip'
	pop ax
	je .notdi
%endif
	jmp gateout		; yes -->
.notdi:
%endif
%if _PM
	cmp al, 'L'		; DL command ?
	jne .notdl
	jmp descout		; yes -->
.notdl:
	cmp al, 'X'		; DX command ?
_386	je extmem		; yes -->
.notdx:
	cmp al, '.'
	je descsubcommand
%endif
%if _MCB
	cmp al, 'M'		; DM command ?
	jne .notdm
	jmp mcbout		; yes -->
.notdm:
%endif
	mov cx, 1
	cmp al, 'B'
	je .d_suffix_size
	inc cx			; = 2
	cmp al, 'W'
	je .d_suffix_size
	inc cx
	inc cx			; = 4
	cmp al, 'D'
	jne .not_d_suffix
.d_suffix_size:
	mov byte [ddsize], cl
	call skipwhite
	call iseol?
	jne dd1			; jump to getting range --> (with new size)
	jmp lastddd		; default range (ADS:ADO length 128),
				;  but with new size -->

.not_d_suffix:
	call skipwh0
	call iseol?
	jne dd1_bytes		; if an address was given --> (set byte size)

lastddd:
_386_PM	xor eax, eax
	mov ax, word [dd_default_lines]
				; default length in lines, if nonzero
	test ax, ax
	jz @F
	js short .error
	mov word [getrange_lines], ax
	xor ax, ax
	jmp @FF

@@:
	mov word [getrange_lines], 8000h
	mov ax, word [dd_default_length]
	test ax, ax
	jz short .error
	dec ax
@@:
		; byte [ddsize] = size already set
	_386_PM_o32	; mov edx, dword [d_addr]
	mov dx, word [d_addr]	; compute range of 80h or until end of segment
	_386_PM_o32	; mov esi, edx
	mov si, dx
	mov bx, [d_addr + saSegSel]
_386_PM	call test_high_limit
_386_PM	jnz .32
	add dx, ax
	jnc dd2_0
	or dx, byte -1
	jmp short dd2_0

.error:
	jmp error

%if _PM
[cpu 386]
.32:
	add edx, eax
	jnc dd2_0		; if no overflow
	or edx, byte -1
	jmp short dd2_0
__CPU__
%endif

dd1_bytes:
	mov byte [ddsize], 1
dd1:
	mov cx, word [dd_default_length]
				; default length (128 bytes)
	mov di, word [dd_default_lines]
				; default length in lines, if nonzero
	mov bx, word [reg_ds]
	mov word [getrange_lines], 8000h
	call getrangeX.lines	; get address range into bx:(e)dx

	call chkeol		; expect end of line here

	mov word [d_addr + saSegSel], bx
				; save segment (offset is saved later)
%if _PM
	call ispm
	jnz .86m
.pm:
	mov word [d_addr + saSelector], bx
	jmp @F
.86m:
	mov word [d_addr + saSegment], bx
@@:
%endif
	_386_PM_o32	; mov esi, edx
	mov si, dx		; bx:(e)si = start
	_386_PM_o32	; mov edx, ecx
	mov dx, cx		; bx:(e)dx = last
%if _PM && 0
	jmp short dd2_1
%endif

		; Parsing is done.  Print first line.
dd2_0:
	testopt [getrange_lines], 8000h
	jnz .notlines
	call dd_get_one_line_range

.notlines:
%if _PM
	call ispm
	jnz dd2_1
[cpu 286]
	verr bx			; readable ?
__CPU__
	jz dd2_1
%if 1
	mov dx, .errmsg
	jmp putsz_error
	usesection lDEBUG_DATA_ENTRY
.errmsg:asciz "Segment is not readable.",13,10
	usesection lDEBUG_CODE
%else
	mov bx, word [reg_ds]
	mov word [d_addr + saSegSel], bx
%if _PM
	call ispm
	jnz .86m
.pm:
	mov word [d_addr + saSelector], bx
	jmp @F
.86m:
	mov word [d_addr + saSegment], bx
@@:
%endif
%endif
dd2_1:
%endif

	mov ax, word [ddsize]
	dec ax			; 0 = byte, 1 = word, 3 = dword
	and ax, si		; how many bytes to skip at the beginning
	mov byte [ddoffset2], al

	mov ax, opt2_db_header
	cmp byte [ddsize], 2
	jb @F
	mov al, opt2_dw_header
	je @F
	mov ax, opt2_dd_header
@@:
	call dd_header_or_trailer


dd_loop_line:
%if _SYMBOLIC
dd_with_sym:
	lframe near
	lvar dword,	startlinear
	lvar dword,	endlinear
	lvar word,	sym_index
	lvar word,	sym_count
	lenter
	xor ax, ax
	lvar dword,	offset
	 push ax		; (zero-initialise high word)
	 push ax
	lvar dword,	adjust
	 push ax		; (zero-initialise high word)
	 push ax		; (zero-initialise offset (low) word)

	_386_PM_o32
	mov word [bp + ?offset], si

	push bx
	_386_PM_o32
	push si
	_386_PM_o32
	push dx

	testopt [internalflags3], dif3_nosymbols_1 | dif3_nosymbols_2
	jnz .justdisplay

	_386_PM_o32
	xchg dx, si		; bx:(e)dx = start address, bx:(e)si = end
	call getlinear_32bit	; dx:ax = start linear
	jc .justdisplay

	mov word [bp + ?startlinear + 2], dx
	mov word [bp + ?startlinear], ax
	push dx
	push ax

	_386_PM_o32
	xchg dx, si		; bx:(e)dx = end address
	call getlinear_32bit	; dx:ax = end linear

	mov word [bp + ?endlinear + 2], dx
	mov word [bp + ?endlinear], ax
	pop bx
	pop cx			; cx:bx = start linear
	jc .justdisplay

	xchg ax, bx
	xchg dx, cx		; cx:bx = end linear, dx:ax = start linear

	nearcall binsearchmain	; es:di -> first entry, cx = number, bx = index
	mov word [bp + ?sym_index], bx
	test cx, cx
	jz .justdisplay

	_386_PM_o32
	pop dx
	_386_PM_o32
	pop si
	pop bx

.loop:
	mov word [bp + ?sym_count], cx

.loop_no_cx:
	 push word [bp + ?sym_index]
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

	mov bx, word [bp + ?adjust + 2]
	mov cx, word [bp + ?adjust]
				; bx:cx = adjust

	add cx, word [bp + ?startlinear]
	adc bx, word [bp + ?startlinear + 2]
				; bx:cx = adjust + start linear (adjust linear)

	neg bx
	neg cx
	sbb bx, byte 0		; neg bx:cx

	add cx, word [es:di + smLinear]
	adc bx, word [es:di + smLinear + 2]
				; bx:cx = next linear - adjust linear
				; bx:cx = how far from adjust linear to next

	test bx, bx		; is there a chunk of ddsize at least ?
	jnz .chunk
	cmp cx, [ddsize]
	jae .chunk		; yes, display a chunk -->

_386_PM	and ecx, 0FFFFh
	_386_PM_o32
	push si
	_386_PM_o32
	add si, cx
	push ss
	pop es
	mov di, line_out
	call dd_display_offset

	testopt [options], dd_no_blanks_sym
	jnz @FF

	pop ax			; ax = original si value
	push ax

	push si
	mov si, ax
	mov cx, word [ddsize]
	mov ax, cx
	dec cx
	and cx, si		; how many bytes to skip at the beginning
	sub si, cx		; = offset after skipped to first displayed
	add cx, cx		; how many digits to skip at the beginning
	and si, 0Fh		; = offset in single line
	add ax, ax		; = 8 for dword, 4 for word, 2 for byte
	inc ax			; = 9 for dword, 5 for word, 3 for byte
	db __TEST_IMM16		; (skip add in first iteration)
@@:
	add cx, ax		; (in subsequent iterations:) add blanks
	sub si, word [ddsize]	; still a whole unit to add ? (subtract it)
	jae @B			; yes -->
				; cx = number of blanks to skip
	mov al, 32
	rep stosb		; store blanks for each byte
	pop si
@@:

	push dx
	call putsline		; puts offset + blanks

	 push word [bp + ?sym_index]
	dualcall displaystring	; puts symbol label

	 push word [bp + ?sym_index]
	 push ax
	dualcall getfarpointer.main
	 pop di
	 pop es
	xor dx, dx
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jz .16			; no --> (don't display zero high word)
_386_PM	push esi
_386_PM	pop si
_386_PM	pop dx
.16:
	cmp dx, word [es:di + smOffset + 2]
	mov dx, msg.dd_after_symbol.non_wrt
	jne .wrt
	cmp si, word [es:di + smOffset]
	je .non_wrt
.wrt:
	mov dx, msg.dd_after_symbol.1_wrt
	call disp_message

	mov ax, word [d_addr + 4]
	push ss
	pop es
	mov di, line_out
	call hexword
	push bx
	push cx
	call putsline
	pop cx
	pop bx

	mov dx, msg.dd_after_symbol.2_wrt
.non_wrt:
	call disp_message	; puts after
	pop dx

	_386_PM_o32
	pop si

	inc word [bp + ?sym_index]
				; point to next symbol's SYMMAIN (if any)
	mov cx, word [bp + ?sym_count]
	loop .j_loop		; loop if any more to go
	jmp .justdisplay_no_pop	; if none, just display remainder -->


.j_loop:
	jmp .loop


		; Display a chunk.
		;
		; INP:	(e)si = start offset to display
		;	(e)dx = end offset to display
		;	bx:cx = how far from adjust linear to next
		;		(there is always a next symbol if we are here)
.chunk:
	mov ax, word [ddsize]
	dec ax
	not ax
	and cx, ax

	add word [bp + ?adjust], cx
	adc word [bp + ?adjust + 2], bx

_386_PM	push word [bp + ?adjust + 2]
_386_PM	push ax
_386_PM	pop eax
	mov ax, word [bp + ?adjust]
				; (e)ax = adjust
	_386_PM_o32
	dec ax			; (e)ax = adjust - 1

		; have:	(e)si = prior start offset, (e)dx = end offset
		; want:	(e)si = unchanged, (e)dx = intermediate end offset,
		;	preserve intermediate start offset, stack = end offset
	_386_PM_o32
	push dx			; stack := end offset
	_386_PM_o32
	mov dx, word [bp + ?offset]
				; (e)dx := start offset
	_386_PM_o32
	add dx, ax		; (e)dx := intermediate end offset

	call dd_display		; display, (e)dx := intermediate start offset

		; have:	(e)si scrambled, (e)dx = intermediate start offset,
		;	stack = end offset
		; want:	(e)si = intermediate start offset, (e)dx = end offset
	_386_PM_o32
	mov si, dx		; (e)si := intermediate start offset
	_386_PM_o32
	pop dx			; (e)dx := end offset
	jmp .loop_no_cx

.justdisplay:
	_386_PM_o32
	pop dx
	_386_PM_o32
	pop si
	pop bx

.justdisplay_no_pop:
	lleave
%endif

	call dd_display

	testopt [getrange_lines], 8000h
	jnz .notlines
	dec word [getrange_lines]
	jz .linesdone
	_386_PM_o32
	mov si, dx		; = new start offset
	call dd_get_one_line_range
				; get a new end offset
	jmp dd_loop_line

.linesdone:
.notlines:
	mov ax, opt2_db_trailer
	cmp byte [ddsize], 2
	jb @F
	mov al, opt2_dw_trailer
	je @F
	mov ax, opt2_dd_trailer
@@:
		; fall through


		; INP:	ax = flag value to check
		;	 (determines whether "header" or "trailer" is written,
		;	  and which flag must be set in word [options2])
		;	byte [ddoffset2] = how many bytes to skip at the start
		;	bx = segment/selector
		; CHG:	ax, cx, di
		; STT:	ds = es = ss
dd_header_or_trailer:
	test word [options2], ax
	jz .ret
	push bx
	push si
	push dx

	mov di, line_out
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	mov ax, bx
	call hexword
	mov al, ':'
	stosb
 %if _PM
	mov cx, -5
	mov ax, 4 + 2
	call test_high_limit	; 32-bit segment ?
	jz .add			; no -->
	mov al, 8 + 2
	jmp .add
 %else
	mov cx, 1
	jmp .blank
 %endif
@@:
%endif

	mov cx, msg.header.length
	mov dx, msg.header
	test ax, opt2_db_header | opt2_dw_header | opt2_dd_header
	jnz @F
	mov cx, msg.trailer.length
	mov dx, msg.trailer
@@:
	call putsz		; put initial word
	neg cx			; minus length of initial word
	mov ax, 4 + 1 + 4 + 2	; length of address with 16-bit offset
%if _PM
	; mov bx, word [d_addr + saSegSel]
	call test_high_limit	; 32-bit segment ?
	jz .16			; no -->
	mov al, 4 + 1 + 8 + 2	; length of address with 32-bit offset
.16:
%endif
.add:
	add cx, ax		; length of address minus length of word
				;  = length to pad
.blank:
	mov al, 32
	rep stosb		; pad
				; ch = 0

	mov ax, '0 '		; al = '0', ah = blank
	mov cl, byte [ddoffset2]; cx = ddoffset2
	jcxz @FF		; if none to skip -->
@@:
	stosw
	inc ax			; increment the number (up to '3')
	loop @B			; loop for skipping -->
@@:
	sub al, '0'		; = back to numerical (0 .. 3)
	mov dx, ax		; dl = numerical offset

	push dx
	mov bx, [ddsize]	; ddsize
	mov si, 16		; loop counter
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	shr si, 1
%endif
@@:
	mov al, dl		; next numerical offset
	call hexnyb		; display it
	mov cx, bx
	add cx, cx		; cx = 2 * ddsize
	mov al, 32
	rep stosb		; pad to next position
	add dx, bx		; increment dl by how many positions we use
	sub si, bx		; decrement loop counter
	ja @B			; don't jump if si was below-or-equal-to bx
	pop dx

	mov cx, 16		; loop counter
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	shr cx, 1
%endif
@@:
	mov al, dl
	call hexnyb		; display an offset
		; Note that this will wrap around for the last 1, 2, or 3
		;  characters if byte [ddoffset2] is non-zero.
	inc dx			; increment offset
	loop @B			; loop

	call putsline_crlf

	pop dx
	pop si
	pop bx
.ret:
	retn


		; INP:	(e)si = start
		;	word [ddsize] = size of element, 1 or 2 or 4
		; OUT:	(e)dx = end
dd_get_one_line_range:
	_386_PM_o32
	mov dx, si
_386_PM	xor eax, eax
	mov ax, word [ddsize]
	dec ax
	and ax, si
%if _40COLUMNS
	or dl, 7
	testopt [options6], opt6_40_columns
	jnz @F
%endif
	or dl, 15
@@:

 %if _PM
	push bx
	mov bx, word [d_addr + saSegSel]
	call test_high_limit	; 32-bit segment ?
	pop bx
	jz .16			; no -->
.32:
	_386_PM_o32
	add dx, ax
	jnc @F
	_386_PM_o32
	or dx, strict byte -1
@@:
	retn
.16:
 %endif
	add dx, ax
	jnc @F
	or dx, strict byte -1
@@:
	retn


		; INP:	word [d_addr + saSegSel] = segment/selector to dump
		;	(e)si = start offset
		;	(e)dx = end offset
		;	byte [ddsize] = 1, 2, or 4 (for byte, word, or dword)
		; OUT:	(d)word [d_addr] updated
		;	(e)dx = (d)word [d_addr]
		;	displayed
dd_display:
	push ss
	pop es
dd2_loop:
	call handle_serial_flags_ctrl_c

	mov word [lastcmd], lastddd

	mov di, line_out	; reset di for next line
%if _40COLUMNS
	mov bx, ~0Fh
	testopt [options6], opt6_40_columns
	jz @F
	mov bl, ~7
@@:
%endif
	call dd_display_offset.masklownybble
				; ax = offset & ~ 0Fh
%if _40COLUMNS
	mov bx, ax
%endif
	mov cx, word [ddsize]
	push cx
	dec cx			; 0 = byte, 1 = word, 3 = dword
	and cx, si		; how many bytes to skip at the beginning
		; eg:	si = 101h, cx = 1, skip 1 byte,  ax = 101h
		;	si = 102h, cx = 3, skip 2 bytes, ax = 102h
		;	si = 103h, cx = 3, skip 3 bytes, ax = 103h
		;	si = 103h, cx = 1, skip 1 byte,  ax = 101h
		;	si = 10Fh, cx = 1, skip 1 byte,  ax = 101h
		;	si = 10Fh, cx = 3, skip 3 bytes, ax = 103h
	add ax, cx		; = where to start
	mov word [ddoffset], cx
	 push ax
	mov ax, 32 << 8 | 32
	rep stosw
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	testopt [options6], opt6_40_indent_odd
	jz @F
	test bl, 8
	jz @F
	stosb
@@:
%endif
	 pop ax
	pop cx

	mov bx, (2+1)*16	; 16 bytes (2 digits each)
	cmp cl, 2
	jb @F			; if it is 1 -->
	mov bl, (4+1)*8		; 8 words (4 digits each)
	je @F			; if it is 2 -->
				; it is 4
	mov bl, (8+1)*4		; 4 dwords (8 digits each)
@@:
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	shr bx, 1		; half as many items
@@:
%endif
	add bx, di
	call prephack		; set up for faking int vectors 23 and 24

	push ax
		; blank the start of the line if offset isn't paragraph aligned
dd3:
	cmp ax, si		; skip to position in line
	je dd4			; if we're there yet
	ja .error
	push ax
	mov ax, 32 << 8| 32
	 push cx
	rep stosw		; store two blanks (2 * 1) if byte,
				;  four blanks (2 * 2) if word,
				;  eight blanks (2 * 4) if dword
	 pop cx
	stosb			; store additional blank as separator
	 push cx
@@:
	mov byte [es:bx], al
	inc bx
	loop @B			; store as many blanks in text dump as bytes
	 pop cx
	pop ax
	add ax, word [ddsize]	; -> behind the byte/word/dword just written
	jmp short dd3


.error:
	mov dx, .msg_internal_error
	call putsz_error
	mov ax, 0601h
	call setrc
	jmp cmd3

	usesection lDEBUG_DATA_ENTRY
.msg_internal_error:
		asciz "Internal error in dd3.",13,10
	usesection lDEBUG_CODE


		; Begin main loop over lines of output.
dd4:
	pop ax
%if _40COLUMNS
	_386_PM_o32	; xor ecx, ecx
	xor cx, cx
	mov cl, 0Fh
	testopt [options6], opt6_40_columns
	jz @F
	mov cl, 7
@@:
 %if _PM
	push bx
	mov bx, word [d_addr + saSegSel]
	call test_high_limit	; 32-bit segment ?
	pop bx
	jz .16			; no -->
	_386_PM_o32	; add ecx, eax
 %endif
.16:
	add cx, ax
%else
	_386_PM_o32	; mov ecx, eax
	mov cx, ax
 %if _PM
	push bx
	mov bx, word [d_addr + saSegSel]
	call test_high_limit	; 32-bit segment ?
	pop bx
	jz .16			; no -->
	_386_PM_o32	; add ecx, strict byte 0Fh
 %endif
.16:
	add cx, strict byte 0Fh
%endif
	jc @F
	_386_PM_o32	; cmp ecx, edx
	cmp cx, dx		; compare with end address
	jb dd5			; if we write to the end of the line -->
@@:
	;_386_PM_o32	; mov ecx, edx
	mov cx, dx		; only write until (e)dx, inclusive
dd5:
	;_386_PM_o32	; sub ecx, esi
	sub cx, si
	;_386_PM_o32	; inc ecx
	inc cx			; cx = number of bytes to print this line
				;      up to 16. no 32-bit register required
	and word [ddskipped], 0

	call dohack		; substitute interrupt vectors
	mov ds, word [d_addr + saSegSel]

dd6:
	mov ax, word [ss:ddsize]
	cmp ax, cx		; ddsize <= left bytes ?
	jbe dd6_simple		; yes, display ddsize bytes -->

	push ax
	push cx
	push di
	neg cx			; - left bytes
	add cx, ax		; ddsize - left bytes = how many skipped
	mov word [ss:ddskipped], cx

	mov cx, ax		; 1 = bytes, 2 = words, 4 = dwords
	dec cx			; 0 = bytes, 1 = words, 3 = dwords
	mov ax, 'XX'
	rep stosw		; fill filler digits not to be written
	pop di
	pop cx
	pop ax

dd6_simple:
	add ax, ax		; 2 = bytes, 4 = words, 8 = dwords
	push ax
@@:
	dec ax
	dec ax
		; first iteration: 0 = bytes, 2 = words, 6 = dwords
		; second iteration: 0 = words, 4 = dwords
		; third iteration: (0 = 3byte,) 2 = dwords
		; fourth iteration: 0 = dwords
	push di
	add di, ax		; -> where to write next 2 hex digits
	 push ax
	_386_PM_a32
	lodsb			; al = data
	call dd_store		; stores number at es:di->, char at es:bx->
	 pop ax
	pop di			; -> start of hex digits space
	test ax, ax		; did we write the left-most digits?
	loopnz @B		; not yet --> (or no more bytes to display)
	pop ax			; = how many digits we wrote
	add di, ax		; -> after right-most digit
	mov al, 32
	stosb			; store a blank
	test cx, cx
	jnz dd6			; (16-bit. cx <= 16)

	push ss			; restore ds
	pop ds
	_386_PM_o32
	sub si, word [ddoffset]
	_386_PM_o32
	add si, word [ddskipped]

dd9:
%if _40COLUMNS
	mov ax, 0Fh
	testopt [options6], opt6_40_columns
	jz @F
	mov al, 7
@@:
	test si, ax
%else
	test si, 0Fh		; space out till end of line
%endif
	jz dd10
%if _40COLUMNS
	 push ax
%endif
	mov ax, 32 << 8 | 32
	mov cx, word [ddsize]
	push cx
	rep stosw		; store blanks for the number
	stosb			; store additional blank as separator
	pop cx
%if _40COLUMNS
	 pop ax
%endif
@@:
	inc si			; skip as many bytes
%if _40COLUMNS
	test si, ax
%else
	test si, 0Fh
%endif
	jz dd10
	loop @B
	jmp short dd9

dd10:
	_386_PM_o32
	add si, word [ddoffset]
	_386_PM_o32
	sub si, word [ddskipped]

	mov cx, (1 + 8 * (2 + 1))	; go back 8 bytes (2 digits each)
	cmp byte [ddsize], 2
	jb @F				; if it is 1 -->
	mov cl, (1 + 4 * (4 + 1))	; go back 4 words (4 digits each)
	je @F				; if it is 2 -->
					; it is 4
	mov cl, (1 + 2 * (8 + 1))	; go back 2 dwords (8 digits each)
@@:
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	testopt [options6], opt6_40_dash
	jz .nodash			; do not write a dash
 %if 1
		; calculate dash position
	dec cx
	shr cx, 1
	inc cx
 %else
		; dispatch for dash position
	mov cx, (1 + 4 * (2 + 1))	; go back 4 bytes (2 digits each)
	cmp byte [ddsize], 2
	jb @F				; if it is 1 -->
	mov cl, (1 + 2 * (4 + 1))	; go back 2 words (4 digits each)
	je @F				; if it is 2 -->
					; it is 4
	mov cl, (1 + 1 * (8 + 1))	; go back 1 dwords (8 digits each)
 %endif
@@:
%endif
	sub di, cx
	mov byte [di], '-'
.nodash:
	call unhack
	mov di, bx
	push dx
	call putsline_crlf
	pop dx
	_386_PM_o32	; dec esi
	dec si
	_386_PM_o32	; cmp esi, edx
	cmp si, dx
	_386_PM_o32	; inc esi
	inc si
	jb dd2_loop		; display next line -->
dd11:
		; This check is necessary to wrap around at FFFFh (64 KiB)
		; for 16-bit segments instead of at FFFFFFFFh (4 GiB).
	mov bx, word [d_addr + saSegSel]
				; reset bx (also set segment for trailer)
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jz .16			; no -->
	_386_PM_o32	; inc edx
.16:
	inc dx			; set up the address for the next 'D' command.
	_386_PM_o32	; mov dword [d_addr], edx
	mov word [d_addr], dx
	retn


		; INP:	(e)si = offset (to display)
		;	(e)dx = end offset (for range check of 16-bit segment)
		;	word [d_addr + saSegSel] = segment/selector
		;	es:di -> where to write to
		; OUT:	bx = segment/selector
dd_display_offset:
.:
	mov ax, word [d_addr + saSegSel]
	mov bx, ax
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jnz @F
%endif
	call hexword
	mov al, ':'
	stosb
@@:
	_386_PM_o32	; mov eax, esi
	mov ax, si
%if _PM
_386_PM	call test_high_limit	; 32-bit segment ?
	jz .16			; no --> (don't display zero high word)
	call hexword_high	; yes, display high word of address
	jmp short .common

		; Insure that the high word is zero.
.16:
;_386	test esi, ~0FFFFh
;_386	jnz .error
_386	test edx, ~0FFFFh
_386	jz .common
;.error:
_386	mov dx, msg.ofs32
_386	call putsz_error
_386	jmp cmd3
.common:
%endif
	call hexword
	mov ax, 32<<8|32
	stosw
	retn

		; INP:	(e)si = offset (to display)
		;	(e)dx = end offset (for range check of 16-bit segment)
		;	word [d_addr + saSegSel] = segment/selector
		;	es:di -> where to write to
		;	if _40COLUMNS:
		;	 bx = mask to apply to si
		; OUT:	bx = segment/selector
		;	(e)ax = offset & ~0Fh (or offset & bx)
.masklownybble:
	push si
%if _40COLUMNS
	and si, bx
%else
	and si, ~0Fh
%endif
	_386_PM_o32
	push si
	call .
	_386_PM_o32
	pop ax
	pop si
	retn


		; Store a character into the buffer. Characters that can't
		; be displayed are replaced by a dot.
		;
		; INP:	al = character
		;	es:bx-> buffer for displayed characters
		;	es:di-> buffer for hexadecimal number
		; OUT:	es:bx-> behind displayed character
		;	es:di-> behind hexadecimal number and space
		; CHG:	ax
		; STT:	ds unknown
dd_store:
	mov ah, al
	cmp al, 32		; below blank ?
	jb .ctrl		; control char -->
	cmp al, 127		; DEL ?
	je .ctrl		; yes, control char -->
	jb .noctrl		; below, not a control char -->
	testopt [ss:options], cpdepchars	; allow CP-dependant characters ?
	jnz .noctrl		; yes -->
.ctrl:
	mov ah, '.'
.noctrl:
	mov byte [es:bx], ah
	inc bx
	push cx
	call hexbyte
	pop cx
	retn


%if _PM
	usesection lDEBUG_DATA_ENTRY
	align 2, db 0
daresult:	dw -1

	usesection lDEBUG_CODE

descalloc:
	call skipwhite
	call chkeol
	xor ax, ax
	mov cx, 1
	int 31h
	jc .error
	mov di, msg.d.a_success_sel
	call hexword
	mov dx, msg.d.a_success
.display:
	call putsz
	mov word [daresult], ax
	retn

.error:
	mov di, msg.d.a_error_code
	call hexword
	mov dx, msg.d.a_error
	cmp ax, 8000h
	jae @F
	mov ax, 0801h
@@:
	call setrc
	mov ax, -1
	jmp .display


descdealloc:
	call skipwhite
	call getword
	call chkeol
	mov ax, 1
	mov bx, dx
	int 31h
	jc .error
	mov dx, msg.d.d_success
.display:
	jmp putsz

.error:
	mov di, msg.d.d_error_code
	call hexword
	mov dx, msg.d.d_error
	cmp ax, 8000h
	jae @F
	mov ax, 0802h
@@:
	call setrc
	jmp .display


descbase:
	call skipwhite
	call getword
	mov cx, dx
	call getdword
	call chkeol
	xchg cx, bx		; cx:dx = base, bx = desc
	mov ax, 7
	int 31h
	jc .error
	mov dx, msg.d.b_success
.display:
	jmp putsz

.error:
	mov di, msg.d.b_error_code
	call hexword
	mov dx, msg.d.b_error
	cmp ax, 8000h
	jae @F
	mov ax, 0803h
@@:
	call setrc
	jmp .display


desclimit:
	call skipwhite
	call getword
	mov cx, dx
	call getdword
	call chkeol
	xchg cx, bx		; cx:dx = limit, bx = desc
	mov ax, 8
	int 31h
	jc .error
	mov dx, msg.d.l_success
.display:
	jmp putsz

.error:
	mov di, msg.d.l_error_code
	call hexword
	mov dx, msg.d.l_error
	cmp ax, 8000h
	jae @F
	mov ax, 0804h
@@:
	call setrc
	jmp .display


desctype:
	call skipwhite
	call getword
	mov bx, dx
	call getword
	call chkeol
	mov cx, dx		; cx = type, bx = desc
	mov ax, 9
	int 31h
	jc .error
	mov dx, msg.d.t_success
.display:
	jmp putsz

.error:
	mov di, msg.d.t_error_code
	call hexword
	mov dx, msg.d.t_error
	cmp ax, 8000h
	jae @F
	mov ax, 0805h
@@:
	call setrc
	jmp .display


descsubcommand:
	lodsb
	cmp al, '?'
	je deschelp
	call ispm
	jne display_nodesc
	call uppercase
	cmp al, 'A'
	je descalloc
	cmp al, 'D'
	je descdealloc
	cmp al, 'B'
	je descbase
	cmp al, 'L'
	je desclimit
	cmp al, 'T'
	je desctype
	jmp error

deschelp:
	lodsb
	call chkeol
	mov dx, msg.deschelp
	jmp putsz		; print string and return


		; DL command
descout:
	call skipwhite
	call getword	; get word into DX
	mov bx, dx
	call skipcomm0
	mov dx, 1
	call iseol?
	je .onlyone
	call uppercase
	cmp al, 'L'
	jne .notlength
	call skipcomma
.notlength:
	call getword
	call chkeol
.onlyone:
	inc dx		; (note js at nextdesc changed to jz)
	mov si, dx	; save count
	call ispm
	je nextdesc
display_nodesc:
	mov dx, nodesc
	mov ax, 0800h
	call setrc
	jmp putsz
desc_done:
	retn
subcpu 286
nextdesc:
	dec si
	jz desc_done
	mov di, descriptor
	mov ax, bx
	call hexword
	mov di, descriptor.base
	push di
	mov ax, "??"
	stosw
	stosw
	stosw
	stosw
	add di, byte (descriptor.limit - (descriptor.base + 8))
	stosw
	stosw
	stosw
	stosw
	add di, byte (descriptor.attrib - (descriptor.limit + 8))
	stosw
	stosw
	pop di
;	lar ax, bx
;	jnz skipdesc	; tell that this descriptor is invalid
	mov ax, 6
	int 31h
	jc desc_o1
	mov ax, cx
	call hexword
	mov ax, dx
	call hexword
desc_o1:
	mov di, descriptor.limit
	_no386_jmps use16desc
subcpu 386
	lsl eax, ebx
	jnz desc_out
	push ax
	shr eax, 16
	call hexword
	pop ax
	call hexword
	lar eax, ebx
	shr eax, 8
desc_o2:
	mov di, descriptor.attrib
	call hexword
desc_out:
	mov dx, descriptor
	call putsz
	add bx, byte 8
	jmp short nextdesc
subcpureset	; subcpu 386
use16desc:
	lsl ax, bx
	jnz desc_out
	call hexword
	mov ax, 32<<8|32
	stosw
	stosw
	lar ax, bx
	shr ax, 8
	jmp short desc_o2
subcpureset	; subcpu 286
%endif

%if _DSTRINGS
		; D$ command
dcpm:
	mov byte [dstringtype], 36
	mov word [dstringaddr], dcpm_addr
	jmp short dstring

		; DW# command
dwcounted:
	mov byte [dstringtype], 0FEh
	mov word [dstringaddr], dwcount_addr
	jmp short dstring

		; D# command
dcounted:
	mov byte [dstringtype], 0FFh
	mov word [dstringaddr], dcount_addr
	jmp short dstring

		; DZ command
dz:
	mov byte [dstringtype], 0
	mov word [dstringaddr], dz_addr

		; common code for all string commands
dstring:
	call skipwhite
	call iseol?
	jne .getaddr		; if an address was given
.last:
	mov bx, word [dstringaddr]
	_386_PM_o32	; mov edx, dword [bx]
	mov dx, word [bx]
	jmp short .haveaddr	; edx = offset, [bx + saSegSel] = segment
.getaddr:
	mov bx, word [reg_ds]
	call getaddrX		; get address into bx:(e)dx
	call chkeol		; expect end of line here
%if _PM
	push bx
%endif
	push bx
	mov bx, word [dstringaddr]
	pop word [bx + saSegSel]; save segment (offset behind string is saved later)
%if _PM
	call ispm
	jnz .86m
.pm:
	pop word [bx + saSelector]
	jmp @F
.86m:
	pop word [bx + saSegment]
@@:
%endif
.haveaddr:
	mov word [lastcmd], dstring.last
	call prephack
	_386_PM_o32	; mov esi, edx
	mov si, dx
	setopt [internalflags], usecharcounter
	mov byte [ charcounter ], 1
				; initialize
	call dohack
	mov ds, word [bx + saSegSel]
				; ds:(e)si-> string
	cmp byte [ss:dstringtype], 0FEh
	jb .terminated		; terminated string -->
	lahf
	_386_PM_a32
	lodsb			; load first byte
	xor cx, cx
	mov cl, al		; low byte of count
	sahf
	jne .counted		; only byte count -->
	_386_PM_a32
	lodsb			; load second byte
	mov ch, al		; high byte of count
.counted:
	jcxz .done		; length zero -->
.loop:
	_386_PM_a32
	lodsb			; get character
	call .char		; display
	loop .loop		; until done -->
	jmp short .done

.char:
	push ss
	pop ds
	push ax
	call unhack		; restore state
	pop ax
	push si
	push cx
	call putc		; display
	pop cx
	pop si
	call handle_serial_flags_ctrl_c
	call dohack
	mov bx, word [dstringaddr]
	mov ds, word [bx + saSegSel]
				; go back to special state
	retn

.terminated:
	_386_PM_a32
	lodsb			; load character
	cmp al, byte [ss:dstringtype]
	je .done		; it's the terminator -->
	call .char		; display
	jmp short .terminated	; and get next -->

.done:
	push ss
	pop ds			; restore ds
	_386_PM_o32	; mov dword [bx], esi
	mov word [bx], si
	call unhack
	mov al, 13
	call putc
	mov al, 10
	call putc
	retn
%endif

%if _INT
		; DI command
gateout:
	xor cx, cx
	lodsb
	call uppercase
	cmp al, 'R'
	jne @F
	inc cx		; always 86 Mode
	lodsb
@@:
	call uppercase
	cmp al, 'M'
	jne @F
	inc ch		; show MCB names
	lodsb
@@:
	call uppercase
	cmp al, 'L'
	jne @F
	or ch, 2	; follow AMIS interrupt lists
	lodsb
@@:
	call skipwh0

	dec si
	mov dx, msg.in
	call isstring?
	jne .not_in

	push si
	push cx
.in.loop:
	call skipwhite
	dec si

	call get_value_range	; OUT:	cx:di = from, bx:dx = to
	jnc @F
	jnz .error
	cmp di, 255
	ja .error
	jcxz .in.next
.error:
	jmp error

@@:
	cmp di, 255
	ja .error
	jcxz @F
	jmp .error

@@:
	test bx, bx
	jnz .error
	cmp dx, 255
	ja .error

.in.next:
@@:
	call skipwh0
	cmp al, ','
	je .in.loop
	call chkeol
	pop cx
	call .prepare
	pop si

.indo.loop:
	call skipwhite
	dec si

	push cx
	call get_value_range	; OUT:	cx:di = from, bx:dx = to
	pop cx
	jc .indo.next

	mov bx, di
	db __TEST_IMM8		; (skip inc)
@@:
	inc bx
	push dx
	call .do
	pop dx
	cmp bx, dx
	jb @B

.indo.next:
	dec si
	call skipwhite
	cmp al, ','
	je .indo.loop
	retn



.not_in:
	lodsb
	call getbyte	; get byte into DL
	xor dh, dh
	mov bx, dx
	call skipcomm0
	mov dx, 1
	call iseol?
	je .onlyone
	call uppercase
	cmp al, 'L'
	jne .notlength
	call skipcomma
	call getword	; get byte into DL
	test dx, dx
	jz .err
	cmp dx, 100h
	je .checkrange
	push ax
	and ah, 1Fh
	cmp ah, 8
	pop ax
	ja .err
.checkrange:
	push dx
	add dx, bx
	cmp dx, 100h
	pop dx
	jna .rangeok
.err:
	jmp error

.last:
	xor bx, bx
	mov bl, byte [lastint]
	mov cx, word [lastint_is_86m_and_mcb]
	mov dx, 1
	inc bl
	jnz .onlyone
	mov word [lastcmd], dmycmd
	retn

.notlength:
	call getbyte
	xor dh, dh
	sub dl, bl
	inc dx
.rangeok:
	call chkeol
.onlyone:
	call .prepare
	mov si, dx	; save count
.next:
	call .do
	inc bx
	dec si
	jnz .next
	retn


.prepare:
	test ch, 2
	jz @F
	call guard_auxbuff
@@:
	mov word [lastcmd], .last
	mov word [lastint_is_86m_and_mcb], cx
	jmp prephack


		; INP:	bx = interrupt number
		;	cx = options
		; CHG:	di, eax. edx, bp
		; STT:	ds = es = ss
		;	prephack called
.do:
	mov byte [lastint], bl
	call handle_serial_flags_ctrl_c
	call dohack
	mov di, line_out
	mov ax, "in"
	stosw
	mov ax, "t "
	stosw
	mov al, bl
	call hexbyte
	mov al, 32
	stosb
%if _PM
	test cl, cl
	jnz .rm
	call ispm
	jnz .rm

	mov ax, 0204h
	cmp bl, 20h
	adc bh, 1	; if below, bh = 2
.loopexception:
	push cx
	int 31h
	mov ax, cx
	pop cx
	jc .failed
	call hexword
	mov al, ':'
	stosb
	_386_PM_o32	; mov eax, edx
	mov ax, dx
	cmp byte [dpmi32], 0
	jz .gate16
	call hexword_high
.gate16:
	call hexword
	mov al, 32
	stosb
	mov ax, 0202h
	dec bh
	jnz .loopexception
	dec di
	call unhack
	push bx
	push cx
	call putsline_crlf
	pop cx
	pop bx
	retn

.rm:
%endif
	push bx
	push cx
	push si

	push bx
	xor bp, bp
	shl bx, 1
	shl bx, 1
	xor dx, dx
%if _PM
	call setes2dx
%else
	mov es, dx			; es => IVT
%endif
	mov ax, word [es:bx + 2]	; ax = segment
	mov dx, word [es:bx]
	pop bx

	test ch, 2
	jnz int_list

.loop_chain:
	push ax				; segment
	push dx

	 push ss
	 pop es
	call hexword
	mov al, ':'
	stosb
	mov ax, dx
	call hexword

	pop bx
	pop dx				; segment

	mov word [intaddress + 2], dx
	mov word [intaddress], bx

	inc bp
	cmp bp, 256
	ja .toomany

	call check_int_chain
	jc .end_chain

	push dx				; segment
	push ax
	 push ss
	 pop es
	call unhack
	push cx
	call copy_single_counted_string
	pop cx
	call .mcbname
	push cx
	call putsline_crlf
	pop cx
	call handle_serial_flags_ctrl_c
	call dohack
	mov di, line_out
	mov ax, " -"
	stosw
	mov ax, "->"
	stosw
	mov al, 32
	stosb

	pop dx
	pop ax				; (ax = segment)
	jmp .loop_chain

.end_chain:
	 push ss
	 pop es
	jnz @F
	call unhack
	push cx
	call copy_single_counted_string
	pop cx
	jmp @FF
@@:
	call unhack
@@:
	call .mcbname
	call putsline_crlf
.86next:
	pop si
	pop cx
	pop bx
	retn

.toomany:
	mov si, msg.di_toomany
	call copy_single_counted_string
	jmp .end_chain

 %if _PM
.failed:
	call unhack
	pop dx				; discard a near return address
	mov dx, gatewrong
	jmp putsz
 %endif


%if 0

For the DIL command we fill the auxiliary buffer with entries
for each interrupt entrypoint. They are found by starting from
the IVT as well as the AMIS interrupt lists. Of course, any
entrypoint may be found from more than one point.

The format is as follows:

dword - vector
word - flags and AMIS multiplex number (low byte)
	flag 200h = unclaimed, AMIS multiplex number not initialised
	flag 100h = immediately from IVT (and is the very first entry in auxbuff)
	if this word is -1 then it is not an entry, it is a terminator
word - which entry in an AMIS interrupt list

A terminator is an entry with all ones (-1).
Two consecutive terminators indicate the last chain ended.
After a single terminator another chain follows.
Any chain after the very first is a hidden chain,
that is, its handlers are not reachable from the IVT
by walking the downlinks.

A hidden chain may be found which eventually feeds into
another hidden chain found previously. In this case, the
new unique handlers (at least 1) are prepended to the
pre-existing hidden chain, and the downlinks past this
point are not walked further (as they're already known).

If an AMIS interface points to a handler that we already
know (in the first IVT-based cnain or any hidden chain)
then its downlink will not be walked again. However, the
multiplex number and list index will be entered into the
entry for this interrupt handler.

Finally, after the IVT and all interrupt lists of all AMIS
multiplexers have been processed, the auxbuff list is used
to display the found chains (one IVT, any amount hidden).

%endif

int_list:
	push di
	push cx
	push bx
	xchg ax, dx			; dx = segment

	mov es, word [ss:auxbuff_segorsel]
	xor di, di			; -> auxbuff
	stosw				; store offset
	xchg ax, dx			; dx = offset
	stosw				; store segment
	xchg ax, dx			; dx = segment
	xchg bx, ax			; bx = offset
	mov ax, 300h			; flag for IVT | unused
	stosw				; which multiplex number
	xor ax, ax
	stosw				; which int list entry = none = 0

.loop_ivt_chain:
	call check_int_chain
	jc .end_ivt_chain

	cmp di, _AUXBUFFSIZE - 3 * 8
	ja .error
	mov es, word [ss:auxbuff_segorsel]
	stosw				; store offset
	xchg ax, dx
	stosw				; store segment
	xchg ax, dx
	xchg bx, ax
	mov ax, 200h			; flag for unused
	stosw				; found in chain = 200h
	xor ax, ax
	stosw
	jmp .loop_ivt_chain

.end_ivt_chain:
	mov ax, -1
	mov es, word [ss:auxbuff_segorsel]
	mov cx, 8
	rep stosw			; terminator is all-ones
					; (two terminators actually)

	xor ax, ax
.loopplex:
	mov al, 00h			; AMIS installation check
	push cx
		; function 0 changes dx, di, cx, al
%if _PM
	call call_int2D
%else
	int 2Dh				; enquire whether there's anyone
%endif
	pop cx				;  but we don't care who it might be
	inc al				; = FFh ?
	jz .search			; yes, it is in use -->
.nextplex:
	inc ah
	jnz .loopplex			; try next multiplexer -->

.done:
	db __TEST_IMM8			; (NC)
.error:
	stc

	pop bx
	pop cx
	pop di
	 push ss
	 pop ds
	 push ss
	 pop es

	mov si, msg.di_error
	jc .error_string

	xor si, si

.loop_chain:
	mov ds, word [auxbuff_segorsel]
	lodsw
	xchg ax, dx
	lodsw
	cmp word [si], -1
	lea si, [si + 4]
	je .next_seq

.next_chain:
	 push ss
	 pop ds

	push ax				; segment
	push dx

	 push ss
	 pop es
	call hexword
	mov al, ':'
	stosb
	mov ax, dx
	call hexword

	pop bx
	pop dx				; segment

	mov word [intaddress + 2], dx
	mov word [intaddress], bx

	push si
	call check_int_chain
	jc .end_chain

	 push ss
	 pop es
	call unhack
	push cx
	call copy_single_counted_string
	pop cx
	pop si
	call .mpx
	push si
	call gateout.mcbname
	push cx
	call putsline_crlf
	pop cx
	call handle_serial_flags_ctrl_c
	call dohack
	mov di, line_out
	mov ax, " -"
	stosw
	mov ax, "->"
	stosw
	mov al, 32
	stosb

	pop si
	jmp .loop_chain

.end_chain:
	 push ss
	 pop es
	jnz @F
	call unhack
	push cx
	call copy_single_counted_string
	pop cx
	jmp @FF
@@:
	call unhack
@@:
	pop si
	call .mpx
	push si
	call gateout.mcbname
	push cx
	call putsline_crlf
	pop cx
	mov di, line_out
	call handle_serial_flags_ctrl_c
	call dohack
	pop si
	jmp .loop_chain

.next_seq:
	lodsw
	xchg ax, dx
	lodsw
	cmp word [si], -1
	lea si, [si + 4]
	je @F

	 push ss
	 pop ds
	 push ss
	 pop es
	call unhack
	push cx
	push si
	mov si, msg.di_hidden
	call copy_single_counted_string
	pop si
	pop cx
	call handle_serial_flags_ctrl_c
	call dohack

	jmp .next_chain

@@:
	 push ss
	 pop ds
	 push ss
	 pop es
	call unhack
	jmp @F


.error_string:
	call copy_single_counted_string

	call unhack
	call putsline_crlf
@@:
%if 0	; _DEBUG
	mov es, word [auxbuff_segorsel]
	int3
	push ss
	pop es
%endif
	jmp gateout.86next


.mpx:
	mov es, word [auxbuff_segorsel]
	mov ax, word [es:si - 4]
	mov dx, word [es:si - 2]
	push ss
	pop es
	test ah, 2
	jnz @F
	push si
	push cx
	mov si, msg.di_multiplex.1
	call copy_single_counted_string
	call hexbyte
	mov si, msg.di_multiplex.2
	call copy_single_counted_string
	xchg ax, dx
	call hexword
	mov si, msg.di_multiplex.3
	call copy_single_counted_string
	pop cx
	pop si
@@:
	retn


		; INP:	ah = multiplex number of AMIS TSR to search through
		;	ss:sp-> interrupt number (byte), must be preserved
		; CHG:	es, di, dx, bx
.search:
	mov al, 04h
	pop bx
	push bx				; low byte is the interrupt number
		; function 4 changes dx, bx, al
%if _PM
	call call_int2D
%else
	int 2Dh
%endif
	cmp al, 03h			; returned its interrupt entry ?
				; RBIL doesn't explicitly state that this interrupt entry has to
				; be IISP compatible. But I'm too lazy to look up the older AMIS,
				; and SearchIISPChain checks the interrupt entry anyway.
	je .search_dxbx
	cmp al, 04h			; returned list of hooked interrupts ?
	jne .nextplex			; no, try next multiplexer -->
	mov di, bx
	pop bx
	push bx				; bl = interrupt number
	xor cx, cx			; = index into list
	mov al, bl
.search_intlist_seg:
%if _PM
	call setes2dx
%else
	mov es, dx			; es:di-> list
%endif
.search_intlist:		; Search the returned list for the required interrupt number.
	scasb				; our interrupt number ?
	je .search_found_intlist
	cmp byte [es:di-1], 2Dh		; was last in list ?
	je .nextplex
	scasw				; skip pointer
	inc cx
	jmp short .search_intlist	; try next entry -->

.search_found_intlist:
	mov bx, word [es:di]		; dx:bx = es:bx -> IISP entry
	scasw				; skip pointer
	push dx				; preserve dx for .search_intlist_seg
	push di
	call .add
	pop di
	pop dx
	jc .error
	; je .search_found		; found entry -->
		; This specific jump supports TSRs that hook the same
		; interrupt more than once; jumping to .nextplex instead
		; (as previously) aborts the search after the first match
		; in the interrupt list. This support might become useful.
	cmp al, 2Dh			; was last in list ?
	je .nextplex
	inc cx
	jmp short .search_intlist_seg

.search_dxbx:
%if _PM
	call setes2dx
%else
	mov es, dx			; es:bx-> (IISP) interrupt entry
%endif
				; The entry we found now is possibly behind the non-IISP entry that
				; terminated our first SearchIISPChain call (at .hard). We then
				; possibly might find our entry in this hidden part of the chain.
	mov cx, -1			; indicator for return = 3 (no list)
	call .add
	jc .error
	; jne .nextplex			; didn't find our entry in the chain -->
	jmp .nextplex


int_list.add:
	xor di, di			; start at beginning of buffer
	mov ds, word [ss:auxbuff_segorsel]
					; ds => auxbuff
	mov si, -1			; check all
	call .check			; check for match
	jne @F				; not matched, di -> second terminator
	testopt [di + 4], 200h		; not yet claimed by a multiplexer ?
	jz .error			; no, error -->
	mov byte [di + 4], ah		; store the multiplex number
	clropt [di + 4], 200h		; indicate it is claimed
	mov word [di + 6], cx		; = how many list entries before ours,
					;  or = -1 if not from a list
	jmp .done

@@:
		; ds:di -> second terminator (will be overwritten)
	cmp di, _AUXBUFFSIZE - 8 * 3	; enough for 1 entry + 2 terminators ?
	ja .error
	lea si, [di - 8]		; check up to this point later
					;  si -> first of two terminators

	xchg ax, bx
	mov es, word [ss:auxbuff_segorsel]
					; => auxbuff
	stosw				; store offset
	xchg ax, dx			; dx = offset
	stosw				; store segment
	xchg ax, dx			; dx = segment
	xchg ax, bx			; dx:bx = vector -> handler
	push ax
	mov al, 0			; flags = 0 (claimed, not IVT)
	xchg al, ah
	stosw				; which multiplex number
	mov ax, cx
	stosw				; which int list entry
	pop ax				; preserve multiplex number

.loop_chain:
	push ax
	push si
	call check_int_chain		; does it go on ?
	pop si
	pop bx
	jc .end_chain			; no -->

	cmp di, _AUXBUFFSIZE - 3 * 8	; enough for 1 entry + 2 terminators ?
	ja .error
	mov es, word [ss:auxbuff_segorsel]
	stosw				; store offset
	xchg ax, dx
	stosw				; store segment
	xchg ax, dx
	xchg bx, ax			; ah = multiplex number, bx = offset
	push ax
	mov ax, 200h
	stosw				; found in chain = 200h (unclaimed)
	xor ax, ax
	stosw
	pop ax
	push di
	xor di, di			; start at beginning
	call .check			; already listed in another chain ?
	je @F
	pop di
	jmp .loop_chain			; no, try to walk downlink -->

@@:
	pop bx

		; The idea is that if the AMIS interrupt list
		;  pointed to an entry not yet matched then
		;  it is the start of a hidden chain. If a
		;  subsequent handler in this hidden chain
		;  points to another handler that does match
		;  this can only be a valid case if this other
		;  handler also was the start of a hidden chain.
		; If this is the case, prepend the unique new
		;  handlers to that hidden chain in the buffer.
	testopt [di + 4], 100h		; is it from the IVT ? (very first entry)
	jnz .error			; yes, error -->
	cmp word [di - 8 + 4], -1	; is it the start of a hidden chain ?
	jne .error			; no, error -->
		; di -> match (insert to move here)
		; bx -> after repeat
		; bx - 8 -> repeat
		; bx - 16 -> last entry to move (at least 1 to move)
		; si -> single terminator
		; si + 8 -> first entry to move (at least 1 to move)

	sub bx, 16			; -> last entry to move

.insert:
		; di -> match (insert to move here)
		; bx + 8 -> repeat
		; bx -> last entry to move
		; si -> single terminator
		; si + 8 -> first entry to move

	push word [bx + 6]
	push word [bx + 4]
	push word [bx + 2]
	push word [bx]			; get the last entry
	push di
	push si
	push cx				; preserve interrupt list index
	mov es, word [ss:auxbuff_segorsel]
	mov cx, di			; = where to insert
					;  -> first to displace
	neg cx
	mov si, bx			; -> after end of source
	lea di, [bx + 8]		; -> after end of dest
	add cx, si			; after end of source - first to displace
					; = how many bytes to displace
	shr cx, 1
	std				; _AMD_ERRATUM_109_WORKAROUND as below
	cmpsw				; si -= 2, di -= 2

	numdef AMD_ERRATUM_109_WORKAROUND, 1
		; Refer to comment in init.asm init_movp.

%if _AMD_ERRATUM_109_WORKAROUND
	jcxz @FF
	cmp cx, 20
	ja @FF
@@:
	movsw
	loop @B
@@:
%endif
	rep movsw			; relocate up the following entries
					;  by 8 bytes (size of 1 entry)
	cld
	pop cx
	pop si
	pop di
	pop word [di]
	pop word [di + 2]
	pop word [di + 4]
	pop word [di + 6]		; insert moved entry
	add si, 8			; -> at moved single terminator

		; di -> match (inserted here, insert next here)
		; bx + 8 -> repeat
		; bx -> last entry to move (if any)
		; si -> single terminator
		; si + 8 -> first entry to move
	cmp si, bx			; if last to move != terminator
	jne .insert			; then move another ->
	mov di, si			; -> where to put double terminator

	xchg ax, bx
.end_chain:
	push bx
	push cx				; preserve interrupt list index
	mov ax, -1
	mov es, word [ss:auxbuff_segorsel]
	mov cx, 8
	rep stosw			; terminator is all-ones
					; (two terminators actually)
	pop cx
	pop ax				; preserve multiplex number

.done:
	db __TEST_IMM8			; (NC)
.error:
	stc

	retn


		; INP:	dx:bx = 86 Mode far pointer to handler
		;	di -> to check
		;	si = end of area to check (-1 = check all)
		; OUT:	ZR if match found, ds:di -> matching entry
		;	NZ if no match found,
		;	 di -> at second consecutive terminator
		;	 or di >= si
		; CHG:	-
		; STT:	ds => auxbuff
		; REM:	continues loop if di < si and the flags word
		;	 is not -1 in two consecutive entries.
		;	an entry with a flags word -1 is skipped.
.check:
	cmp word [di + 0], bx
	jne .mismatch
	cmp word [di + 2], dx
	jne .mismatch
.match:
	retn				; (ZR)

.mismatch:
	add di, 8

	cmp di, si
	jae .checkret

	cmp word [di + 4], -1
	jne .check

	add di, 8
	cmp word [di + 4], -1
	jne .check
.checkret:
	test di, di			; (NZ)
	retn


		; INP:	dx:bx = 86 Mode far pointer to int handler
		; OUT:	NC if chain found,
		;	 dx:ax = 86 Mode far pointer to next
		;	 ss:si -> type message (counted)
		;	CY if chain not found,
		;	 NZ if no chain
		;	 ZR if chain but next is FFFFh:FFFFh,
		;	  ss:si -> type message
		; STT:	es != ss, ds != ss
check_int_chain:
%if _PM
	call setes2dx
%else
	mov es, dx			; es:bx -> entrypoint
%endif
	call IsIISPEntry?
	jnz .not_iisp

	push word [es:bx + ieNext + 2]
	push word [es:bx + ieNext]

	mov si, msg.di_uninst_iisp
	cmp word [ es:bx + ieEntry ], 0EA90h	; nop\jmp far imm16:imm16 ?
	je @F
	mov si, msg.di_iisp
	cmp byte [ es:bx + ieJmphwreset ], 0EBh	; jmp short ?
	jne .nonstd
	cmp word [ es:bx + ieEntry ], 010EBh	; jmp short $+18 ?
	je @F
.nonstd:
	mov si, msg.di_nonstd_iisp
@@:
	pop ax
	pop dx				; segment

	push ax
	and ax, dx
	inc ax
	pop ax
	jz .CY

	clc
	retn


.not_iisp:
	cmp bx, -8
	ja .not_fd
	cmp byte [es:bx], 0E8h
	jne .not_fd
	cmp byte [es:bx + 3], 0EAh
	jne .not_fd
	push word [es:bx + 4 + 2]
	push word [es:bx + 4]
	mov si, msg.di_freedos_reloc
	jmp @B

.not_fd:
	cmp bx, -5
	ja .not_jmpfar
	mov si, msg.di_jmpfar
	cmp byte [es:bx], 0EAh
	jne .not_jmpfar
	push word [es:bx + 1 + 2]
	push word [es:bx + 1]
	jmp @B

.not_jmpfar:
	mov si, msg.di_jmpfarindirect
	cmp byte [es:bx], 0EBh
	jne .not_testhook_try_jmpfarindirect
	mov si, msg.di_testhook
	mov al, byte [es:bx + 1]
	cbw
	add ax, 2
	add bx, ax
.not_testhook_try_jmpfarindirect:
	cmp bx, -6
	ja .not_testhook_or_jmpfarindirect
	cmp word [es:bx], 0FF2Eh
	jne .not_testhook_or_jmpfarindirect
	cmp byte [es:bx + 2], 2Eh
	jne .not_testhook_or_jmpfarindirect
	mov bx, word [es:bx + 3]
	cmp bx, -4
	ja .not_testhook_or_jmpfarindirect
	push word [es:bx + 2]
	push word [es:bx]
	jmp @B

.not_testhook_or_jmpfarindirect:
	test sp, sp			; NZ
.CY:
	stc
	retn


gateout.mcbname:
	test ch, 1
	jz .ret
	mov dx, word [firstmcb]
	cmp dx, -1
	je .ret
	push cx
	mov ax, word [intaddress]
	mov cl, 4
	shr ax, cl
	add ax, word [intaddress + 2]	; => segment of handler
	jc .hma
.loop:
%if _PM
	call setes2dx
%else
	mov es, dx
%endif
	mov cx, dx
	add cx, word [es:3]
	inc cx
	cmp ax, dx
	jb .next
	cmp ax, cx
	jae .next
	mov dx, word [es:1]
	mov si, msg.di_system_mcb
	cmp dx, 50h
	jb .copy
	dec dx
%if _PM
	call setes2dx
%else
	mov es, dx
%endif
	 push es
	 pop ds
	 push ss
	 pop es
	mov al, 32
	stosb
	mov ax, di
	mov si, 8
	mov cx, si
	push di
	rep movsb
	mov al, 0
	stosb			; append zero-value byte
	pop di			; -> name in buffer
	 push ss
	 pop ds
@@:
	scasb			; is it zero ?
	jne @B			; no, continue -->
				; first dec makes it -> at the zero
@@:
	dec di
	cmp ax, di
	je .empty
	cmp byte [di - 1], 32
	je @B
	jmp .ret_cx

.empty:
	dec di
	mov si, msg.di_empty
	jmp .copy

.hma:
	mov si, msg.di_hma
	jmp .copy

.next:
	mov dx, cx
	cmp dx, word [firstumcb]; is next one the first UMCB ?
	je .loop		; yes, ignore the "Z" (if any) -->
	cmp byte [es:0], "M"	; check current signature
	je .loop		; if "M" then loop to next -->

	mov si, word [firstumcb]
	inc si
	jnz @F
	mov si, 0A000h
@@:
	cmp ax, si
	mov si, msg.di_system_upper
	jae @F
	mov si, msg.di_system_low
@@:
.copy:
	 push ss
	 pop es
	call copy_single_counted_string
.ret_cx:
	pop cx
.ret:
	retn

%endif

%if _MCB
		; DM command
mcbout:
	call skipwhite
	mov dx, word [firstmcb]
	call iseol?
	je .lolmcb
	call getword
	call chkeol
.lolmcb:
	mov si, dx
	mov di, line_out
	mov ax, "PS"
	stosw
	mov ax, "P:"
	stosw
	mov al, 32
	stosb
	mov ax, word [pspdbe]
	call hexword
	call putsline_crlf	; destroys cx,dx,bx
	mov cl, 'M'
.next:
	cmp si, byte -1
	je .invmcb
	cmp si, byte 50h
	jae .valmcb
.invmcb:
	mov dx, msg.invmcbadr
	jmp putsz
.valmcb:
	mov di, line_out
	push ds
%if _PM
	call setds2si
%else
	mov ds, si
%endif
	mov ch, byte [0000]
	mov bx, word [0001]
	mov dx, word [0003]

	mov ax, si
	call hexword		; segment address of MCB
	mov al, 32
	stosb
	mov al, ch
	call hexbyte		; 'M' or 'Z'
	mov al, 32
	stosb
	mov ax, bx
	call hexword		; MCB owner
	mov al, 32
	stosb
	mov ax, dx
	call hexword		; MCB size in paragraphs

	mov al, 32
	stosb
	mov ax, dx		; ax = size in paragraphs
	push bx
	push ax
	push dx
	push cx
	xor dx, dx		; dx:ax = size in paragraphs
	mov cx, 16		; cx = 16, multiplier (get size in bytes)
	mov bx, 4+4		; bx = 4+4, width

	call disp_dxax_times_cx_width_bx_size.store
	pop cx
	pop dx
	pop ax
	pop bx

	test bx, bx
	jz .freemcb		; free MCBs have no name -->
	mov al, 32
	stosb
	push si
	push cx
	push dx

	push ds
	mov si, 8
	mov cx, 2
	cmp bx, si		; is it a "system" MCB? (owner 0008h or 0007h)
	ja @F
	cmp byte [si], "S"	; "S", "SD", "SC" ?
	je .nextmcbchar		; yes, limit name to two characters -->
	jmp .nextmcbchar_cx_si	; no, assume full name given
@@:
	dec bx			; => owner block's MCB
%if _PM
	call setds2bx
%else
	mov ds, bx
%endif
.nextmcbchar_cx_si:
	mov cx, si		; = 8
.nextmcbchar:			; copy name of owner MCB
	lodsb
	stosb
	test al, al
	loopnz .nextmcbchar	; was not NUL and more bytes left ?
	test al, al
	jnz @F
	dec di
@@:
	pop ds

	cmp word [1], 8
	jne .not_s_mcb
	cmp word [8], "S"	; S MCB ?
	jne .not_s_mcb

	mov ax, " t"
	stosw
	mov ax, "yp"
	stosw
	mov ax, "e "
	stosw

	xor ax, ax
	mov al, [10]
	call hexbyte

	 push ss
	 pop ds
	mov si, smcbtypes
.s_mcb_loop:
	cmp word [si], -1
	je .s_mcb_unknown
	cmp word [si], ax
	je .s_mcb_known
	add si, 4
	jmp .s_mcb_loop

.s_mcb_known:
	mov si, word [si + 2]
	jmp .s_mcb_common

.s_mcb_unknown:
	mov si, smcbmsg_unknown
.s_mcb_common:
	mov al, 32
@@:
	stosb
	lodsb
	test al, al
	jnz @B

.not_s_mcb:
	pop dx
	pop cx
	pop si
.freemcb:

	pop ds
	cmp ch, 'M'
	je .disp
	cmp ch, 'Z'
	je .disp
.ret:
	retn

.disp:
	mov cl, ch
	push dx
	push cx
	call putsline_crlf	; destroys cx,dx,bx
	pop cx
	pop dx
	add si, dx
	jc .ret			; over FFFFh, must be end of chain --> (hmm)
	inc si
	jz .ret
	jmp .next

%if _PM
setds2si:
	mov bx, si
setds2bx:
	call ispm
	jnz sd2s_ex
	mov dx, bx
	call setrmsegm
sd2s_ex:
	mov ds, bx
	retn
%endif	; _PM
%endif	; _MCB

;--- DX command. Display extended memory

%if _PM
[cpu 386]
extmem:
	mov dx, word [x_addr+0]
	mov bx, word [x_addr+2]
	call skipwhite
	call iseol?
	je extmem_1
	call getdword		; get linear address into bx:dx
	call chkeol		; expect end of line here
extmem_1:
	mov word [lastcmd], extmem
	push bx
	push dx
	pop ebp

	mov di, stack		; create a GDT for Int15.87
	xor ax, ax
	mov cx, 8
	rep stosw
	mov ax, 007Fh
	stosw
	mov ax, dx
	stosw
	mov al, bl
	stosb
	mov ax, 0093h
	stosw
	mov al, bh
	stosb
	mov ax, 007Fh
	stosw
	mov ax, line_in+128
	mov bx, word [pspdbg]
	movzx ebx, bx
	shl ebx, 4
	movzx eax, ax
	add eax, ebx		; eax = flat address of line_in+128
	stosw
	shr eax, 16
	stosb
	mov bl, ah
	mov ax, 0093h
	stosw
	mov al, bl
	stosb
	mov cx, 8
	xor ax, ax
	rep stosw

	call ispm
	mov si, stack
	mov cx, 0040h
	mov ah, 87h
	jnz extmem_rm
	push word [pspdbg]
	push 15h
	call intcall
	jmp short i15ok
extmem_rm:
	int 15h
i15ok:
	jc extmem_exit
	mov si, line_in+128
	mov ch, 8h
nexti15l:
	call handle_serial_flags_ctrl_c
	mov di, line_out
	mov eax, ebp
	shr eax, 16
	call hexword
	mov ax, bp
	call hexword
	mov ax, 32<<8|32
	stosw
	mov bx, line_out+10+3*16
	mov cl, 10h
nexti15b:
	lodsb
	call dd_store
	mov al, 32
	stosb
	dec cl
	jnz nexti15b
	mov byte [di-(8*3+1)], '-'	; display a '-' after 8 bytes
	add di, 16
	push cx
	call putsline_crlf
	pop cx
	add ebp, byte 10h
	dec ch
	jnz nexti15l
	mov dword [x_addr], ebp
extmem_exit:
	retn
__CPU__
%endif

		; INP:	dx:ax = numerator
		;	cx = multiplier (0 to take si:dx:ax as numerator)
		;	bx = field width
		;	es:di -> buffer where to store
		; STT:	UP, ds = ss
		; OUT:	written to buffer, es:di -> behind written string
disp_dxax_times_cx_width_bx_size: section_of_function
	db __TEST_IMM8		; (skip stc, NC)
.store:
	stc

	lframe near
	lequ 4 + 4 + 2,		buffer_size
		; 4: "2048" (maximum number)
		; 4: " ?iB" (IEC prefixed unit)
		; 2: ???
	lvar ?buffer_size,	buffer
	lvar 6,			dividend
	lenter
	lvar word,		bit_0_is_store
	 pushf
	lvar word,		width
	 push bx
	push si
	push ds
	push cx
	push ax
	push dx
	push es
	push di

	 push ss	; push cs
	 pop ds
	 push ss
	 pop es

	jcxz .use_si

	push dx
	mul cx
	xchg ax, di
	xchg dx, si		; si:di = first mul

	pop ax
	mul cx
	add ax, si
	adc dx, 0		; dx:ax = second mul + adj, dx:ax:di = mul

	jmp @F

.use_si:
	mov di, ax
	xchg ax, dx
	mov dx, si

@@:
	mov word [bp + ?dividend], di
	mov word [bp + ?dividend + 2], ax
	mov word [bp + ?dividend + 4], dx

		; set up divisor for the unit prefixes
	mov cx, 1024		; 1000 here if SI units
	testopt [options], use_si_units	; SI units ?
	jz @F			; no -->
	mov cx, 1000		; yes, use 1000
@@:

	mov si, msg.prefixes	; -> first prefix (blank)
.loop:
	cmp word [bp + ?dividend + 4], 0
	jnz .divide
	cmp word [bp + ?dividend + 2], 0
	jnz .divide
	cmp word [bp + ?dividend], 2048
	jbe .end
.divide:
	inc si			; -> next prefix

	xor dx, dx
	mov di, 6
.loop_divide:
	mov ax, [bp + ?dividend - 2 + di]
	div cx
	mov word [bp + ?dividend - 2 + di], ax
	dec di
	dec di
	jnz .loop_divide
				; dx = last remainder
	jmp .loop

.end:
	lea di, [bp + ?buffer + ?buffer_size - 1]
	std			; _AMD_ERRATUM_109_WORKAROUND does not apply
	mov al, "B"
	stosb
	mov al, [si]
	cmp al, 32
	je @FF

	testopt [options], use_si_units
				; SI units ?
	jnz @F			; yes -->
	and al, ~20h		; uppercase, don't do this if SI units
	testopt [options], use_jedec_units
				; JEDEC units ?
	jnz @F			; yes -->
	push ax
	mov al, "i"
	stosb			; don't store this if SI or JEDEC units
	pop ax
@@:
	stosb
@@:
	mov al, 32
	stosb

	mov ax, word [bp + ?dividend]
	mov cx, 10
.loop_write:
	xor dx, dx
	div cx
	xchg ax, dx
				; ax = remainder (next digit)
				; dx = result of div
	add al, '0'
	stosb
	xchg ax, dx		; ax = result of div
	test ax, ax		; any more ?
	jnz .loop_write		; loop -->

	cld

	inc di			; -> first digit
	lea bx, [bp + ?buffer + ?buffer_size]
				; -> behind 'B'
	sub bx, di		; = length of string
	mov si, di

	pop di
	pop es			; restore es:di
				; -> where to store (if storing)

	mov cx, [bp + ?width]
	sub cx, bx
	jbe .none_blank
	mov al, 32
	test byte [bp + ?bit_0_is_store], 1
	jnz @F
.loop_blank_disp:
	nearcall disp_al
	loop .loop_blank_disp
		; now cx = 0 so the rep stosb is a nop
@@:
	rep stosb
.none_blank:


	mov cx, bx
	test byte [bp + ?bit_0_is_store], 1
	jnz @F

		; ! note ss = ds
	mov dx, si		; ds:dx -> string
	call disp_message_length_cx
	db __TEST_IMM16		; (skip rep movsb)
@@:
		; ! note ss = ds
		; ds:si -> string, cx = length
	rep movsb

	pop dx
	pop ax
	pop cx
	pop ds
	pop si
	pop bx
	lleave
	lret

..@dd_access_end:
