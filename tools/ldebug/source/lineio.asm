
%if 0

lDebug line input and output

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

		; Check for given string (cap-insensitive)
		;
		; INP:	ds:si -> input string to check (either cap),
		;	 terminated by CR (13), NUL, semicolon, space,
		;	 tab, dot, comma, equals, colon, [, ], (, or )
		;	es:dx -> ASCIZ string to check (all-caps)
		; OUT:	Iff string matches,
		;	 ZR
		;	 si -> at separator that terminates the keyword
		;	else,
		;	 NZ
		;	 si = input si
		; STT:	ds = es = ss
		; CHG:	dx, al
isstring?:
	push si
	xchg dx, di
.loop:
	lodsb
	call uppercase
	scasb
	jne .mismatch
	test al, al
	jne .loop
	jmp .matched_zr

.mismatch:
	call iseol?
	je .checkend
	cmp al, 32
	je .checkend
	cmp al, 9
	je .checkend
	cmp al, '.'
	je .checkend
	cmp al, ','
	je .checkend
	cmp al, '='
	je .checkend
	cmp al, ':'
	je .checkend
	cmp al, '['
	je .checkend
	cmp al, ']'
	je .checkend
	cmp al, '('
	je .checkend
	cmp al, ')'
	je .checkend
.ret_nz:
		; NZ
	pop si
.ret:
	xchg dx, di
	retn

.checkend:
	cmp byte [es:di - 1], 0
	jne .ret_nz
.matched_zr:	; ZR
	pop di			; (discard)
	lea si, [si - 1]	; -> separator (preserve ZR)
	jmp .ret


iseol?_or_then:
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jz iseol?
	mov dx, msg.then
	dec si
	call isstring?
	je iseol?.ret
	lodsb
%endif

iseol?:
	cmp al, ';'
	je .ret
.notsemicolon:
	cmp al, 13		; this *IS* iseol?
	je .ret
	cmp al, 0
.ret:
	retn


chkeol_or_then:
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	call skipwh0
	call iseol?_or_then
	je iseol?.ret		; if EOL -->
	jmp @F
%endif

		; Check for end of line
		;
		; INP:	al = first character
		;	ds:si-> next character
		; OUT:	ZR
		;	al = 13 or al = ';' or al = 0
		;	(does not return if anything on line beside blanks)
chkeol: section_of_function
	call skipwh0
	call iseol?
	je iseol?.ret		; if EOL -->

@@:
	mov ax, 0100h
	call setrc
errorj8:
	jmp error


		; Skip blanks, then an optional comma, and then more blanks
		;
		; INP:	ds:si -> first character
		; OUT:	al = first non-blank character behind
		;	ds:si -> character behind the first non-blank behind
		;	NC
		; STK:	3 word
skipcomma:
	lodsb

		; Same as above but we already have the first character in al
skipcomm0:
	call skipwh0
	cmp al, ','
	jne .return		; if no comma
	push si
	call skipwhite
	call iseol?
	jne .noteol		; if not end of line
	pop si
	mov al, ','
	retn
.noteol:
	add sp, byte 2		; pop si into nowhere
.return:
	retn


		; Skip blanks, then an optional equals sign, then more blanks
skipequals:
	lodsb
skipequ0:
	call skipwh0
	cmp al, '='
	jne .return
	call skipwhite
.return:
	retn


		; Skip alphabetic characters, and then white space
		;
		; INP:	ds:si-> first character
		; OUT:	al = first non-blank character behind alphabetic characters
		;	ds:si-> character behind the first non-blank behind alpha.
		;	NC
skipalpha:
.:
	lodsb
	and al, TOUPPER
	sub al, 'A'
	cmp al, 'Z'-'A'
	jbe .
	dec si

		; Skip blanks and tabs
		;
		; INP:	ds:si-> first character
		; OUT:	al = first non-blank character
		;	ds:si-> character behind the first non-blank
		;	NC
		; CHG:	-
		; STK:	1 word
skipwhite: section_of_function
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
skipwh0: section_of_function
	cmp al, 32
	je skipwhite
	cmp al, 9
	je skipwhite
	clc
	retn


		; SHOWSTRING - Print ASCIZ string.
showstring.next:
	stosb
showstring:
	lodsb
	test al, al
	jnz .next
	retn


		; Dump byte as decimal number string
		;
		; INP:	al = byte
		;	di-> where to store
		; OUT:	-
		; CHG:	di-> behind variable-length string
decbyte:
	push ax
	push cx
	mov cx, 100
	call .div
	mov cl, 10
	call .div
	add al, '0'
	stosb
	pop cx
	pop ax
	retn

.div:
	xor ah, ah
	div cl
	or ch, al
	jz .leadingzero
	add al, '0'
	stosb
.leadingzero:
	xchg al, ah
	retn


decword: section_of_function
	push dx
	xor dx, dx
	call decdword
	pop dx
	retn


decdword: section_of_function
	push cx
	xor cx, cx
	call dec_dword_minwidth
	pop cx
	retn


		; Dump dword as decimal number string
		;
		; INP:	dx:ax = dword
		;	cx = minimum width (<= 1 for none, must be < 10)
		;	es:di -> where to store
		; OUT:	es:di -> behind variable-length string
		; CHG:	-
		; STT:	UP
dec_dword_minwidth:
	lframe near
	lequ 10,	bufferlen
	lvar ?bufferlen,buffer
	lenter
	lvar dword,	dividend
	 push dx
	 push ax
	dec cx
	lvar word,	minwidth
	 push cx
	inc cx

	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es

	 push ss
	 pop es

	lea di, [bp + ?buffer + ?bufferlen - 1]
	mov bx, di
	std			; _AMD_ERRATUM_109_WORKAROUND does not apply

		; dword [bp + ?dividend] = number to display
	mov cx, 10		; divisor
.loop_write:

	xor dx, dx
	push di
	mov di, 4
.loop_divide:
	mov ax, [bp + ?dividend - 2 + di]
	div cx
	mov word [bp + ?dividend - 2 + di], ax
	dec di
	dec di
	jnz .loop_divide
				; dx = last remainder
	pop di
	xchg ax, dx		; ax = remainder (next digit)
				; dword [bp + ?dividend] = result of div
	add al, '0'
	stosb
	dec word [bp + ?minwidth]
	jns .loop_write

	cmp word [bp + ?dividend + 2], 0
	jnz .loop_write
	cmp word [bp + ?dividend], 0
				; any more ?
	jnz .loop_write		; loop -->

	cld

	sub bx, di
	mov cx, bx
	mov si, di
	inc si

	pop es
	pop di

@@:
	ss movsb		; do not replace by rep ss movsb, because
				;  some 8086 don't like two-prefix opcodes
	loop @B

	pop si
	pop dx
	pop cx
	pop bx
	pop ax

	lleave
	retn


		; dump high word of eax - assumes 386
hexword_high:
[cpu 386]
	rol eax, 16
	call hexword
	rol eax, 16
__CPU__
	retn


		; hexdword - dump dword (in eax) to hex ASCII - assumes 386
		; HEXWORD - Print hex word (in AX).
		; HEXBYTE - Print hex byte (in AL).
		; HEXNYB - Print hex digit.
		; Uses	none.
%if 0	; currently disabled because only one call made to here (ID command)
hexdword:
	call hexword_high
%endif
hexword: section_of_function
	xchg al, ah
	call hexbyte
	xchg al, ah

hexbyte:
	push cx
	mov cl, 4
	rol al, cl
	call hexnyb
	rol al, cl
	pop cx

hexnyb:
	push ax
	and al, 0Fh
.common:
		; These three instructions change to ASCII hex.
		;  Refer to https://codegolf.stackexchange.com/questions/193793/little-endian-number-to-string-conversion/193842#193842
	cmp al, 10	; set CF according to digit <= 9
	sbb al, 69h	; read CF, set CF and conditionally set AF
	das		; magic, which happens to work
	stosb
	pop ax
	retn

		; TAB_TO - Space fill line_out until reaching the
		;  column indicated by AX. (Display a new line if
		;  necessary.) At least two blanks are stored.
		;
		; INP:	es:di -> behind text to display
		;	es:line_out -> text to display
		;	es:ax -> destination to tab to,
		;	 if ax >= di + 2 then just put blanks,
		;	 else pass content so far to trimputs
		;	 and fill line_out with blanks afterwards
		; OUT:	es:di -> tabbed to buffer in line_out
		; CHG:	ax, bx, cx, dx
tab_to:
	push ax
	dec ax			; make sure at least two blanks
	cmp ax, di
	ja .sameline		; if there's room on this line -->
		; below-or-equal: go to next line.
		; That is, if we are at the indicated column
		;  (actually column - 1 due to dec) then make
		;  a new line. This covers the case of needing
		;  zero blanks (or one due to dec) as needing
		;  a new line, but one or more (two or more) go
		;  to the same line as prior content.
	call trimputs
	mov di, line_out
.sameline:
	pop cx
	sub cx, di
	mov al, 32
	rep stosb		; space fill to the right end
puts.retn:
	retn

		; Trim excess blanks, append linebreak and display line_out.
		;
		; INP:	es:di -> behind last character to display, or blank
		;	es:line_out -> text to display
		; OUT:	es:di -> behind displayed text (CR LF inserted)
		; CHG:	ax, bx, cx, dx
		;
		; Note:	May overflow if line_out only contains blanks. The byte at
		;	trim_overflow is used to avoid overflows.
trimputs:
	dec di
	cmp byte [es:di], 32
	je trimputs
	inc di

		; Append linebreak and display line_out
		;
		; INP:	es:di -> behind last character to display
		;	es:line_out -> text to display
		; OUT:	es:di -> behind displayed text (CR LF inserted)
		; STT:	all segment registers same
		; CHG:	ax, bx, cx, dx
putsline_crlf: section_of_function
	mov ax, 10<<8| 13
	stosw

		; Display line_out
		;
		; INP:	es:di -> behind last character to display
		;	es:line_out -> first character to display
		; STT:	all segment registers same
		; CHG:	ax, bx, cx, dx
putsline: section_of_function
	mov cx, di
	mov dx, line_out
	sub cx, dx

		; Display message
		;
		; INP:	es:dx -> message to display
		;	cx = length of message
		; STT:	ds = ss = debugger data selector
		; CHG:	ax, bx, cx, dx
puts:
;d4	call d4message
;d4	asciz "In puts first",13,10

%if _IMMASM
	testopt [internalflags6], dif6_immasm_no_output
	jnz .retn
%endif
	testopt [internalflags], tt_silence
	jnz puts_silence

	testopt [internalflags3], dif3_unquiet
	jnz @F
	testopt [internalflags3], dif3_quiet_output
	jnz .retn
@@:

		; The following code contains most of the paging support.
		; Based on the number of LF characters in the string it
		; displays only parts of the string, then interrupts it by
		; the "[more]" prompt which waits for any key before
		; proceeding. This is ensured to work proper in InDOS mode.
		;
		; Paging is deactivated if the command's output mustn't be
		; paged (clears pagedcommand, which is set by cmd3). It is
		; also not used when we output to a file.
	testopt [options], nonpagingdevice
	jnz .display			; deactivated by user -->
	testopt [internalflags3], dif3_input_re
	jnz .display
	testopt [internalflags3], dif3_input_cmdline
	jnz .display
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz .display
%endif
	call InDOS_or_BIOS_IO		; InDOS mode ?
	jnz .dontcheckredirection	; yes, then we display with Int10 anyway -->
%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jnz .display
%endif
	testopt [options], enable_serial ; I/O done using serial port ?
	jnz .dontcheckredirection	; yes, is paged -->
	call InDOS_or_BIOS_output
	jnz @F
	testopt [internalflags], outputfile
	jnz .display			; output redirected to file. never page -->
@@:
	testopt [options], nondospaging
	jnz .dontcheckredirection
	testopt [internalflags], inputfile
	jnz .display			; input redirected from a file. never page -->
.dontcheckredirection:
	push di
	push cx				; used as variable: remaining (not yet displayed) line length
	mov di, dx			; es:di-> string, cx = length
.looplf:
	test cx, cx
	jz .display_pop			; end of string (or ended in LF) -->
		; Important: We only ever jump back to .looplf when cx
		; zero means it's okay to ignore the waiting prompt as
		; flagged below. This is (A) at the start of a string,
		; where cx is the whole string's length, (B) after
		; determining that prompting is not yet necessary, in
		; which case the flag was checked earlier already, (C)
		; after the flag has been set and a substring was already
		; displayed (so cx is again the whole substring's length)
		; or (D) just after the prompt was displayed, in which
		; case the flag cannot be set.
		; In case A and C, when the (sub)string is empty (ie. cx
		; is zero) it's crucial to ignore the waiting prompt as
		; this is the exact behaviour we want: If nothing is
		; displayed anymore before the getline code prompts
		; anyway, do not display our prompt.
	testopt [internalflags], promptwaiting	; do we have a prompt to display ?
	jnz .promptnow			; yes, display it before the string -->

	xor ax, ax
	testopt [options], enable_serial ; serial ?
	jz @F				; no -->
	or al, byte [serial_rows]	; ax = number of rows if serial
	jz .display_pop_ZR		; if zero, do not page -->
	dec ax				; was 1 ? (adjust to rows minus one)
	jnz @FF				; no, use as rows minus one --> (NZ)
					; ax = 0 if it was 1
@@:
	or al, [io_rows]
	jz .display_pop_ZR
	dec ax				; was 1 ? (adjust to rows minus one)
	jnz @F				; no, use as rows minus one --> (NZ)
					; yes, automatic (use BDA)
	push es
	mov ax, 40h			; 0040h is a bimodal segment/selector
	mov es, ax
	mov al, byte [ es:84h ]		; rows on screen
	pop es
	test ax, ax
@@:
.display_pop_ZR:
	jz .display_pop

	cmp byte [ linecounter ], al
	jb .notyet			; not yet reached -->
	testopt [internalflags], pagedcommand	; active ?
	jnz .prompt			; yes, prompt -->
	dec byte [ linecounter ]	; keep count, but don't prompt til next LF
	jmp short .notyet

.prompt:
	 pop ax				; ax = length of string, cx = length of string remaining
	sub ax, cx			; ax = length of string til LF
	xchg ax, cx			; cx = til LF incl., ax = behind LF
	 push ax			; new count
				; cx = length til LF
				; es:dx-> start of part til LF
	call .display			; display part of message which fits on screen
	 pop cx
	 push cx			; update cx from variable
	mov dx, di			; dx-> start of next part
	setopt [internalflags], promptwaiting	; mark as prompting necessary
	jmp short .looplf		; now check whether anything follows at all
		; This is the magic to suppress unnecessary prompts as
		; were displayed previously. Now, we'll set this flag
		; which effectively displays the prompt before (!) any
		; other output is done. Previously, the prompt would be
		; displayed right here. The only case where behaviour
		; changed is when no more output occurs until the flag
		; is reset elsewhere - ie. if getline prompts anyway.

.promptnow:
	push dx
	push cx
	mov byte [ linecounter ], 0	; prompting, so reset the line counter
	clropt [internalflags], promptwaiting
	push es
	 push ss
	 pop es
	mov dx, msg.more		; es:dx -> message
	mov cx, msg.more_size
	call .display			; print string (avoiding a recursion)
	pop es

				; This option is a hack for the sole use of
				; demo scripts that only want the user to press
				; a key for paging.
	testopt [options], nondospaging
	jz .getc
	call getc.rawnext		; get a character from BIOS
	jmp short .dispover
.getc:
	call getc			; get a character
.dispover:
	cmp al, 3			; is it Ctrl+C ?
	je .ctrlc			; yes, handle that -->
	call handle_serial_flags_ctrl_c
	push es
	 push ss
	 pop es
	mov dx, msg.more_over		; es:dx -> message
	mov cx, msg.more_over_size
	call .display			; overwrite the prompt (avoiding a recursion)
	pop es
	pop cx
	pop dx

.notyet:
	mov al, 10
	repne scasb			; search LF
	jne .display_pop		; none -->

	inc byte [ linecounter ]	; record how many LFs will be displayed
	jmp .looplf			; search for next LF -->

.display_pop:
	pop cx
	pop di
.display:
		; Non-paged output code follows.
;d4	call d4message
;d4	asciz "In puts.display first",13,10

	testopt [options], enable_serial
	jnz .notdos
	call InDOS_or_BIOS_output
	jnz .notdos
					; es:dx -> message
	mov bx, 1			; standard output
	mov ah, 40h			; write to file
	jcxz @F
%if _PM
d5	push di
	 push es			; point ds of _doscall* to msg segment
d5	call d4message
d5	asciz 13,10,"In puts.display, es="
d5	 push es
d5	call d4disp_stack_hex
d5	call d4message
d5	asciz "h",13,10
	dualcall selector_to_segment	; make sure we give it as a segment
		; This assumes that non-access-slice pointers into the
		;  symbol tables (SYMSTR particularly) can be converted
		;  into 86M segmented addresses, ie that the selector
		;  points into 86M-accessible memory.
d5	call d4message
d5	asciz "In puts.display, on stack ="
d5	pop di
d5	push di
d5	 push di
d5	call d4disp_stack_hex
d5	call d4message
d5	asciz "h",13,10
	dual2call _doscall_return_es_parameter_es_ds
	 add sp, 2			; discard es returned from call
d5	pop di
%else
	push ds
	 push es
	 pop ds				; ds:dx -> message
	int 21h				; simply call into DOS
	pop ds
%endif
@@:
	retn

.ctrlc:
	testopt [internalflags2], dif2_in_silence_dump
	jz @F
	call reset_silent_mode
@@:
	jmp handle_ctrl_c		; abort currently running command -->
		; If handled by DOS, Ctrl+C causes our process to be terminated.
		; Because we are self-owned, we re-enter our code at debug22 then.
		; debug22 only does some re-initialization of registers before
		; entering cmd3. Therefore, instead of aborting we can directly jump
		; to cmd3 here. This has the additional benefit of not requiring DOS
		; at all, so that no workarounds for InDOS mode and boot loader
		; operation are necessary.

		; No command should fail spectacularly when being aborted this way,
		; because in fact every command calling puts can already be aborted by
		; DOS's Ctrl+C checking if DOS is used. This check is really only an
		; _additional_ way the commands can be aborted.

		; Note that a more complete way to support command abortion would be
		; to hook Int1B, and to keep a flag of whether Ctrl+C or Ctrl+Break
		; were requested, and to additionally check before or after every I/O
		; operation whether Ctrl+C was pressed using non-destructive reads.
		; In short, exactly what DOS does.

.notdos:
	push si
	testopt [internalflags], usecharcounter
	jnz .dontresetcharcounter
	mov byte [ charcounter ], 1
		; This assumes we always start at the beginning of a line.
		; Therefore any call to puts must display at the beginning
		; of a line or tab parsing will not work. Only calls to puts
		; not containing tab characters may display partial lines.
		; (Calls to puts with partial lines and tab characters have
		; to set the flag usecharcounter in internalflags.)
.dontresetcharcounter:
	jcxz .return
	mov si, dx
.loop:
	es lodsb
	cmp al, 9
	jne .nottab			; is no tab -->
	mov al, byte [ charcounter ]
	and al, 7			; at 8 character boundary ?
	mov al, 32			; (always replaced by blank)
	jz .nottab			; yes, don't use hack -->
	inc cx
	dec si				; find tab again next lodsb
.nottab:
	cmp al, 13			; (exact match for CR)
	jne .notcr
.cr:
	mov byte [ charcounter ], 0	; increased to one before displaying
.notcr:
%if 0				; currently we never receive BS here
	cmp al, 8
	jne .notbs
	mov bh, byte [vpage]
	mov bl, al			; save number of characters per column
	push cx
	mov ah, 03h
	int 10h				; get cursor position dx
	pop cx
	dec byte [ charcounter ]	; assume not at start of line
	mov al, 8			; changed by Int10
	or dl, dl
	jnz .dontcount			; not first column, so display normal -->
	mov byte [ charcounter ], 1	; assume at start of screen
	or dh, dh
	jz .next			; at start of screen, don't display -->
	dec dh				; previous line
	mov dl, bl
	mov byte [ charcounter ], dl	; really at end of line (one-based counter)
	dec dl				; last column
	mov ah, 02h
	int 10h				; set new cursor position
	jmp short .next
.notbs:
%endif
	cmp al, 10
	je .dontcount_lf		; must not count line feeds!
	inc byte [ charcounter ]
%if _REGSHIGHLIGHT || _GETLINEHIGHLIGHT
	jmp @F
.dontcount_lf:
	clropt [internalflags3], dif3_int10_highlight
@@:
%else
.dontcount_lf:
%endif
	testopt [options], enable_serial
	jz @F

	call serial_send_char

	jmp .next
@@:

%if _REGSHIGHLIGHT || _GETLINEHIGHLIGHT
	cmp al, 27
	je .try_highlight

.nohighlight:
%endif
	mov bh, byte [vpage]		; use the current video page

%if _REGSHIGHLIGHT || _GETLINEHIGHLIGHT
	testopt [internalflags3], dif3_int10_highlight
	jz @F

		; Fix: do not write a musical note for CR.
		;  This was caused by resetting the flag
		;  dif3_int10_highlight only for LF.
		; Also do not write BEL symbol.
		; Hardening: Do not write attributes for
		;  any nonprintable codepoint.
	cmp al, 32
	jb @F
	mov ah, 09h
	mov bl, byte [.attribute]
	push cx
	mov cx, 1
	int 10h
	pop cx
@@:
%endif

	mov bl, 7
	mov ah, 0Eh
	int 10h
.next:
	loop .loop
.return:
	pop si
	retn


%if _REGSHIGHLIGHT || _GETLINEHIGHLIGHT
.try_highlight:
	testopt [options3], opt3_r_highlight_dumb
	jnz .nohighlight
	cmp cx, 2
	jbe .nohighlight
	cmp byte [es:si], '['
	jne .nohighlight
	cmp byte [es:si + 1], 'm'
	je .highlight_reset
	cmp cx, 3
	je .nohighlight
	cmp byte [es:si + 1], '7'
	jne .nohighlight
	cmp byte [es:si + 2], 'm'
	jne .nohighlight
.highlight_set:
	call check_dumb_mode
	jz .nohighlight
		; This test fails if we are at the very left of
		;  the screen or in dosemu -dumb mode. So don't
		;  use highlighting at the beginning of a line!
	setopt [internalflags3], dif3_int10_highlight
	dec cx
	inc si
	jmp @F

.highlight_reset:
	call check_dumb_mode
	jz .nohighlight
		; Refer to above comment.
@@:
	setopt [internalflags3], dif3_int10_highlight
	dec dl
	mov ah, 02h
	mov bh, byte [vpage]
	int 10h			; set cursor position
	mov ah, 08h
	int 10h			; read attribute to ah
	push ax
	inc dl
	mov ah, 02h
	int 10h			; set cursor position
	pop ax
	mov al, ah
	and ax, 8877h
	rol al, 1
	rol al, 1
	rol al, 1
	rol al, 1
	or al, ah
	mov byte [.attribute], al

	dec cx
	dec cx
	inc si
	inc si
	jmp .next


	usesection lDEBUG_DATA_ENTRY
.attribute:	db 0
	usesection lDEBUG_CODE


		; INP:	byte [vpage]
		; CHG:	bx, dx
		; OUT:	ZR iff dumb mode
		; STT:	ds = ss
check_dumb_mode:
	push ax
	push cx
	mov ah, 3
	mov bh, byte [vpage]
	xor dx, dx		; pre-initialise to zero
	int 10h			; dl = column, dh = row
	test dl, dl
	jz .ret
	testopt [internalflags], runningdosemu
	jz .ret_NZ
	mov ax, 55h		; function DOS_HELPER_GET_TERM_TYPE
	int 0E6h		; CHG: ax, bx, cx
	and ah, 1		; ax & 100h = dumb mode
	xor ah, 1		; NZ if not dumb mode
	jmp .ret
.ret_NZ:
	test sp, sp		; (NZ)
.ret:
	pop cx
	pop ax
	retn
%endif


%if _40COLUMNS
		; Display line_out, with IOCLINE linebreaks
		;
		; INP:	es:di -> behind last character to display
		;	es:line_out -> first character to display
		;	ax = last fragment length
		; OUT:	ax = last fragment length
		; STT:	ds = ss = debugger data selector
		; CHG:	ax, bx, cx, dx
putsline_break_line:
	mov cx, di
	mov dx, line_out
	sub cx, dx

		; Display message, with IOCLINE linebreaks
		;
		; INP:	es:dx -> message to display
		;	cx = length of message
		;	ax = last fragment length
		; OUT:	ax = last fragment length
		; STT:	ds = ss = debugger data selector
		; CHG:	ax, bx, cx, dx
puts_break_line:
	add ax, cx				; how much in this line if fits
	mov bx, word [io_columns_getline]	; = how many columns to fill
	test bx, bx				; disabled ?
	jz .no_split				; yes, use normal handler -->
	cmp ax, bx				; total <= columns ?
	jbe .no_input_split			; yes, back to normal code -->
	push si
	mov si, cx				; si = length input
	sub ax, cx				; restore last fragment length
	mov cx, bx
	sub cx, ax				; = how much fits in first line
	sub si, cx				; = how much left, cannot carry

		; INP:	si = how much left for subsequent iterations
		;	es:dx -> data to display
		;	bx = columns to fill
		;	cx = length for this iteration
		; OUT:	cx = length of last write (0 if empty, no linebreak)
		; CHG:	si, dx
	call puts_partial_write
	xchg ax, cx				; ax = last fragment length
	pop si
	retn

.no_split:
.no_input_split:
	push ax
	call puts
	pop ax
puts_break_line_more.retn:
	retn


		; Display a linebreak if last fragment length is zero
		;  and there is more to display. This function is only
		;  to be called if there is more output to display!
		;
		; INP:	ax = last fragment length (no action if nonzero)
		; OUT:	line break displayed if ax was zero
		; CHG:	dx
puts_break_line_more:
	test ax, ax
	jnz .retn
	mov dx, crlf
	jmp putsz


puts_partial_write.loop:
	mov cx, bx

		; INP:	si = how much left for subsequent iterations
		;	es:dx -> data to display
		;	bx = columns to fill
		;	cx = length for this iteration
		; OUT:	cx = length of last write (0 if empty, no linebreak)
		; CHG:	si, dx
puts_partial_write:
	push bx
	push cx
	push dx
	call puts				; display partial
	test si, si				; more to go ?
	jz @F					; no -->
	mov dx, crlf
	call putsz				; put linebreak
@@:
	pop dx
	pop cx
	pop bx
	add dx, cx				; advance pointer
	sub si, bx				; subtract from counter
	jnc .loop				; still above zero, loop -->
	add si, bx				; restore prior si value
	mov cx, si
	push bx
	push cx
	call puts				; display last part (possibly empty)
	pop cx					; return cx
	pop bx
	retn
%else
putsline_break_line: equ putsline
puts_break_line: equ puts
puts_break_line_more: equ dmycmd
%endif


		; INP:	es:dx -> message to display
		;	cx = length of message
		; STT:	ds = ss = debugger data selector
		; CHG:	ax, bx, cx, dx
puts_silence:
	push si
	push di

.try_again:
	mov si, dx			; es:si -> message
	mov di, word [auxbuff_behind_last_silent]
					; (auxbuff):di -> next buffer (if it fits)
	mov ax, _AUXBUFFSIZE - 1
	sub ax, di			; number of bytes left free
					;  (+ 1 byte terminator)
	jc .delete
	cmp ax, cx			; fits ?
	jae .simple			; yes -->

.delete:
	push es
	mov es, word [auxbuff_segorsel]
	call silence_delete_one_string
	pop es
	jmp .try_again

.simple:
	push ds
	push es
	 push es
	 mov es, word [auxbuff_segorsel]
					; es:di -> next buffer
	 pop ds				; ds:si -> message
	rep movsb			; copy over
	pop es
	pop ds
	mov word [auxbuff_behind_last_silent], di
					; update pointer
	pop di
	pop si
	retn


		; INP:	es => auxbuff
		;	ds = ss
		;	[auxbuff_behind_last_silent] -> behind last silent
		;	[auxbuff_behind_while_condition]
		; OUT:	[auxbuff_behind_last_silent] updated
		;	auxbuff updated (deleted one of the dump strings,
		;	 moved forwards in the buffer the remainder)
		;	if error, aborts command by jumping to cmd3
		; CHG:	ax, di, si
silence_delete_one_string:
	call .internal			; call internal implementation
	jnc .retn			; no error ? -->
.error:					; else: error, abort command
	push ss
	pop ds
	push ss
	pop es
	clropt [internalflags], tt_silence
	mov dx, msg.silent_error
	call putsz
	jmp cmd3


		; INP:	as for silence_delete_one_string
		; OUT:	as for silence_delete_one_string, but:
		;	 CY if error (no more space)
		;	 NC if success
		; CHG:	ax, di, si
		; STT:	ds = ss
.internal:
	push cx
	mov cx, [auxbuff_behind_last_silent]
					; -> next buffer position
	mov di, word [auxbuff_behind_while_condition]
	sub cx, di
	mov al, 0
	cmp al, 1			; initialise to NZ (if cx is zero)
	repne scasb
	stc
	jne .retn			; error, no NUL found in data (CY)
		; es:di -> behind first NUL

	mov si, di			; es:si -> next message
	mov di, word [auxbuff_behind_while_condition]
	mov cx, word [auxbuff_behind_last_silent]
	push es
	pop ds				; ds:si -> next message
	sub cx, si			; remaining buffer
	rep movsb			; move to start of silent buffer
	push ss
	pop ds
	mov word [auxbuff_behind_last_silent], di
	pop cx
	clc				; (NC)
.retn:
	retn


		; After having used puts_silence, this dumps all data
		;  remaining in the silent buffer in auxbuff.
		; If word [tt_silent_mode_number] is set, only that many
		;  data strings (zero-terminated) are dumped, from the end
		;  of the buffer.
		;
		; CHG:	ax, bx, cx, dx, si, di, es
		; STT:	ds = ss = debugger data selector
		;	sets es to ss
silence_dump:
	testopt [internalflags], tt_silent_mode	; is in use ?
	jnz @F			; yes -->
	retn			; no. simple

@@:
	clropt [internalflags], tt_silence
	setopt [internalflags2], dif2_in_silence_dump

	testopt [options3], opt3_silence_paging_set
	jz @F
	testopt [options3], opt3_silence_paging_on
	jz .turn_paging_off

.turn_paging_on:
	setopt [internalflags], pagedcommand
	jmp @F

.turn_paging_off:
	clropt [internalflags], pagedcommand
@@:

	mov dx, word [tt_silent_mode_number]
	test dx, dx
	jz .no_number_given

	mov es, word [auxbuff_segorsel]
	mov di, word [auxbuff_behind_while_condition]
	mov cx, word [auxbuff_behind_last_silent]
	sub cx, di
	jz .no_number_given

	xor bx, bx		; counter of zeros
@@:
	mov al, 0
	jcxz @F			; no more data -->
	repne scasb		; another zero ?
	jne @F			; no, done -->
	inc bx			; count zeros
	jmp @B			; search for next -->

@@:
	sub bx, dx		; number of dumps - requested number
				;  = excess number of dumps
	jbe .no_number_given

	mov cx, bx		; use excess number as loop counter
@@:
	call silence_delete_one_string
				; delete one string
	loop @B			; loop for however many to delete -->

.no_number_given:
	mov di, word [auxbuff_behind_while_condition]
				; es:di -> silent buffer
.loop_line:
	mov es, word [auxbuff_segorsel]
	mov cx, word [auxbuff_behind_last_silent]
	sub cx, di		; any more data ?
	jz .return		; no, return -->
	mov al, 0
	dec cx			; (in case of branching for next conditional)
	scasb			; starts with a NUL byte ?
	je @F			; yes, skipped -->
	inc cx			; (restore cx to original value)
	dec di			; no, decrement
@@:
	mov si, di		; es:si -> start of string

	cmp cx, 256		; cx > 256 ?
	jbe @F			; no -->
	mov cx, 256		; limit to 256 bytes per string
				; (line_out is 264 bytes)
@@:
	jcxz .return		; (if single byte that was NUL remaining -->)
	mov bx, cx		; search string length
	mov al, 0
	repne scasb		; scan for NUL bytes
	jne @F			; (if none found: cx = 0)
				; (if any found: cx = remaining length)
	inc cx			; cx = remaining length + 1 (do not output NUL)
@@:
	sub bx, cx		; search length - remaining = found length
	mov cx, bx		; how much to show
	 push es
	 pop ds			; ds => auxbuff (ds:si -> start of string)
	 push ss
	 pop es			; es => line_out
	mov di, line_out	; es:di -> line_out
	 push cx
	rep movsb		; copy over to line_out
	 pop cx			; cx = message length

	push ss
	pop ds			; reset seg regs to ss
	mov dx, line_out	; dx -> message, cx = length
	push si
	call puts		; print out
	pop di			; -> next silent message
	jmp .loop_line

.return:
	push ss
	pop es
	push word [auxbuff_behind_while_condition]
	pop word [auxbuff_behind_last_silent]
reset_silent_mode:
	clropt [internalflags], tt_silent_mode | tt_silence
	clropt [internalflags2], dif2_in_silence_dump
	retn


putsz_error: section_of_function
	setopt [ss:internalflags3], dif3_unquiet_error

		; Display ASCIZ message
		;
		; INP:	ds:dx -> ASCIZ message to display
		; CHG:	-
		; STT:	ds, es don't care
putsz: section_of_function
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	 push ds
	 pop es
	push di
	mov di, dx			; es:di-> string
	xor al, al
	mov cx, -1
	repne scasb			; search zero
	not cx
	dec cx				; cx = length of message
	pop di
	 push ss
	 pop ds				; ds = ss
	call puts
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	retn

		; Display character
		;
		; INP:	al = character to display
		; CHG:	-
		; STT:	ds, es don't care
check_section_of disp_al
putc: section_of_function
	push bx
	push cx
	push dx
	push es
	push ds
	 push ss
	 pop es				; es:dx -> message
	 push ss
	 pop ds				; ds = ss
	push ax
	mov cx, 1			; one character
	mov dx, sp			; ds:dx-> ax on stack
	call puts
	pop ax
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	retn


		; OUT:	ax = 0 if no input available
		;	ax = 0FFFFh if DOS stdin input available
		;	ah = scancode, al = ASCII if int 16h input available
		;	ax = al = ASCII if serial input available
		; REM:	This is always a nondestructive read.
peekc:
	testopt [options], enable_serial
	jnz .serial
	call InDOS_or_BIOS_IO
	jz .dos
.bios:
	mov ah, 01h
	int 16h			; key available ?
@@:
	jz .return_ax_0
	retn

.serial:
	call serial_check_receive_char
				; do serial getc (check rx buffer)
	mov ah, 0
	jmp @B

.dos:
	mov ah, 0Bh
	doscall
	mov ah, -1
	test al, al
	jmp @B

.return_ax_0:
	mov ax, 0
.return:
	retn


		; Get character/key
		;
		; OUT:	al = character (if zero, look for ah)
		;	ah = scan code or zero
		; CHG:	ax
		; STT:	ds = ss = debugger segment/selector
		;
		; Idles system when in InDOS mode. When not in InDOS mode, Int21.08
		; is used which is assumed to idle the system itself.
getc:
	testopt [internalflags3], dif3_input_serial_override
	jnz @F
	testopt [options], enable_serial
	jnz @F
	call InDOS_or_BIOS_IO
	jz getc_dos
@@:
.rawnext:
	call near word [getline_timer_func]

	testopt [internalflags3], dif3_input_serial_override
	jnz @F
	testopt [internalflags3], dif3_input_re
	jnz @F
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz @F
%endif
	testopt [internalflags3], dif3_input_cmdline
	jnz @F
	testopt [options], enable_serial
	jnz @F
	testopt [options], biosidles
				; idling disabled?
	jnz .rawkey		; yes, just request a key -->

@@:
	call getc_if_any	; got a key ?
	jnz .return		; yes -->
.idle:				; common idling for BIOS keyboard and serial
	call handle_serial_flags_ctrl_c
%if _SLEEP_NEW
	push di
	mov di, word [getc_repeat_idle]
	inc di
@@:
	call idle
	dec di
	jnz @B
	pop di
%else
	call idle
%endif
	jmp .rawnext		; check again -->

.rawkey:
	xor ax, ax
	int 16h			; get the key and scancode
.return:
	retn


getc_dos:
%if _PM
	call ispm
	jnz @F			; if 86 Mode -->
	testopt [options2], opt2_getc_idle_dpmi
	jnz @FF
@@:
%endif
	testopt [options2], opt2_getc_idle
	jz getc_dos_internal
@@:
.with_idle:
	call getc_if_any
	jnz .return
	call handle_serial_flags_ctrl_c
	call idle
	jmp .with_idle

.return equ getc_dos_internal_get_extended.return


getc_dos_internal:
		; DOS character devices handle one-byte characters. Therefore
		; non-ASCII keys cannot be returned with scancode in the high
		; byte of the same call. A non-ASCII key will be split into
		; two characters by CON: one NUL byte followed by the scancode.
	testopt [internalflags3], dif3_input_re
	jz @F
	push si
	mov si, word [re_buffer.position]
	xor ax, ax
	lodsb
	test al, al
	mov word [re_buffer.position], si
	pop si
	jnz .return
	dec word [re_buffer.position]
	jmp .return

.return equ getc_dos_internal_get_extended.return


@@:
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz .file
%endif
%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz .file_not
	testopt [internalflags2], dif2_input_file
	jnz .file
%endif
	jmp .file_not

.file:
	push dx
	push cx
	push bx
	push di
	xor ax, ax		; initialise ah to zero
	push ax
	mov dx, sp		; ds:dx -> al byte on stack
	mov cx, 1
%if _INPUT_FILE_BOOT
 %if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file_boot
	jz @F
 %endif
	call yy_boot_remember_seek
	call yy_boot_read
 %if _INPUT_FILE_HANDLES
	jmp @FF
@@:
 %endif
%endif
%if _INPUT_FILE_HANDLES
	mov ah, 3Fh
	call yy_get_handle
	call yy_remember_seek
	doscall			; (depends on ds = ss)
%endif
@@:
	jc @F
	test ax, ax
	jnz .file_got

@@:
	call yy_close_file
	pop ax
	pop di
	pop bx
	pop cx
	pop dx
	jmp getc


.file_got:
	pop ax			; ah = 0, al = character read
	pop di
	pop bx
	pop cx
	pop dx
	retn

.file_not:
%endif
	testopt [internalflags3], dif3_input_cmdline
	jz @F
	push si
	mov si, word [cmdline_buffer.position]
	xor ax, ax
	lodsb
	test al, al
	mov word [cmdline_buffer.position], si
	pop si
	jnz .return
	clropt [internalflags3], dif3_input_cmdline
	jmp getc

@@:
	mov ah, 8
	doscall			; wait for a key

getc_dos_internal_get_extended:
		; FreeDOS kernel 2036 returns with ax=4C00h upon Control C.
		; This is due to setting ax internally to terminate the
		;  process, paired with the termination service just
		;  returning for self-owned processes. This was eventually
		;  fixed in https://sourceforge.net/p/freedos/svn/1469/
		; This is a work around to restart our command line then.
		; Usually ah stays 08h when this call returns.
		;  (06h if calling from getc_if_any to here.)
	cmp ah, 4Ch
	je .freedos_ctrlc_workaround
	mov ah, 0		; assume it is ASCII
	test al, al
	jne .return		; ASCII, return with ah zero -->
	mov ah, 8
	doscall			; scancode of non-ASCII key to al
	xchg al, ah		; to ah
	mov al, 0		; return zero for non-ASCII key
.return:
	retn

.freedos_ctrlc_workaround:
	mov dx, msg.freedos_ctrlc_workaround
	call putsz
	jmp puts.ctrlc		; use common handler


%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT || 1
		; INP:	[input_file_handles], dif2_input_file
		; OUT:	most recent file closed, flag cleared if no longer file
		; CHG:	di, bx, ax
yy_close_file:
	testopt [internalflags3], dif3_input_re
	jz @F
	clropt [internalflags3], dif3_input_re
	setopt [internalflags3], dif3_input_re_closed
	retn

@@:
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	call yy_boot_clear_remember_seek
	jmp yy_boot_close_file

@@:
%endif
%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jz .notfile
	mov di, word [input_file_handles.active]
	push di
	shl di, 1
	shl di, 1
	shl di, 1
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
	mov bx, word [input_file_handles + di + ifhHandle]
	call yy_clear_remember_seek

	testopt [input_file_handles + di + ifhFlags], ifhfIsDup
	jz @F

	push cx
	push dx

	mov bx, word [input_file_handles + di - INPUTFILEHANDLE_size + ifhHandle]
	mov dx, word [input_file_handles + di + ifhParentSeek]
	mov cx, word [input_file_handles + di + ifhParentSeek + 2]
	mov ax, 4200h		; seek from start
	call handle_seek_or_remember

	pop dx
	pop cx

@@:

	call InDos
	jz .closefile

	inc word [input_file_handles.to_close]
	jmp @F

.closefile:
	mov ax, 1
	call yy_close_file_handles

@@:
	pop di
	dec di
	jns .next
	clropt [internalflags2], dif2_input_file
	setopt [internalflags2], dif2_closed_input_file
	jmp .done
.next:
	mov word [input_file_handles.active], di
.done:
	retn

.notfile:
	testopt [internalflags3], dif3_input_cmdline
	jz @F
	clropt [internalflags3], dif3_input_cmdline
	setopt [internalflags3], dif3_input_cmdline_closed
	retn

@@:
	xor si, si
	jmp error


		; INP:	word [input_file_handles.to_close] = how many
		;	ax = additionally how many (0 or 1)
		;	input_file_handles + di -> first to close
		; OUT:	word [input_file_handles.to_close] = 0
		;	closed handle fields = -1
		; CHG:	ax, bx, di
		; STT:	DOS must be accessible
yy_close_file_handles:
	push cx
	xor cx, cx
	xchg cx, word [input_file_handles.to_close]
	add cx, ax
	jz @FF
@@:
	mov bx, -1
	xchg bx, word [input_file_handles + di + ifhHandle]
	mov ah, 3Eh
	doscall
	add di, INPUTFILEHANDLE_size
	loop @B
@@:
	pop cx
	retn


		; INP:	-
		; OUT:	di = active handle offset
		;	bx = active handle
		; CHG:	-
yy_get_handle:
	mov di, word [input_file_handles.active]
	shl di, 1
	shl di, 1
	shl di, 1
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
	mov bx, word [input_file_handles + di + ifhHandle]
%endif
	retn
%endif


		; INP:	-
		; OUT:	NZ if received any,
		;	 al = character
		;	 ah = scan code or zero
		;	ZR if none received
		; CHG:	ax
		; STT:	ds = ss = debugger segment/selector
getc_if_any:
	testopt [internalflags3], dif3_input_serial_override
	jnz .serial
	testopt [internalflags3], dif3_input_terminal_override
	jnz .terminal

	testopt [internalflags3], dif3_input_re
	jz @F
	push si
	mov si, word [re_buffer.position]
	xor ax, ax
	lodsb
	test al, al
	mov word [re_buffer.position], si
	pop si
	jnz .return
	dec word [re_buffer.position]
	cmp al, al			; ZR
	jmp .return

@@:
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz .file
%endif

	testopt [internalflags3], dif3_input_cmdline
	jz @F
	push si
	mov si, word [cmdline_buffer.position]
	xor ax, ax
	lodsb
	test al, al
	mov word [cmdline_buffer.position], si
	pop si
	jnz .return
	clropt [internalflags3], dif3_input_cmdline
	jmp getc_if_any

@@:
.terminal:
	testopt [options], enable_serial
	jz @F			; do BIOS keyboard or DOS getc -->

.serial:
	call serial_receive_char ; do serial getc (check rx buffer)
	jz .return		; no data, go and idle -->
	mov ah, 0
	retn

@@:
	call InDOS_or_BIOS_IO
	jnz .bios

	testopt [internalflags3], dif3_input_terminal_override
	jnz .dos_terminal

%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz .file
%endif
%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz .file_not
	testopt [internalflags2], dif2_input_file
	jnz .file
%endif
	jmp .file_not

.file:
	push di
	push bx
	push cx
	push dx
	xor ax, ax
	push ax

	mov dx, sp		; ds:dx -> al byte on stack
	mov cx, 1		; buffer length = 1
%if _INPUT_FILE_BOOT
 %if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file_boot
	jz @F
 %endif
	call yy_boot_remember_seek
	call yy_boot_read
	jmp @FF
@@:
%endif
%if _INPUT_FILE_HANDLES
	mov ah, 3Fh
	call yy_get_handle
	call yy_remember_seek
	doscall			; DOS read file (depends on ds = ss)
%endif
@@:
	jnc @F
	xor ax, ax
@@:
	test ax, ax
	pop ax
	pop dx
	pop cx
	pop bx
	pop di
	retn			; ZR if no character read

.file_not:
%endif
.dos_terminal:
%if 0
	push dx
	mov ah, 06h
	mov dl, -1
	doscall
	jz .return_dx		; none available
		; bugfix: the 06h call is a *destructive* read.
		;  so do not call getc again, just handle the
		;  possible second byte returned.
	call getc_dos_internal_get_extended
	pop dx
	jmp .return_NZ
.return_dx:
	pop dx
	retn
%else
	mov ah, 0Bh
	doscall
	test al, al
	jz .return
	call getc_dos_internal
	jmp .return_NZ
%endif

.bios:
	mov ah, 01h
	int 16h			; key available ?
	jz .return
	xor ax, ax
	int 16h
.return_NZ:
	push ax
	or al, 1		; (NZ)
	pop ax
.return:
	retn


		; INP:	-
		; OUT:	-
		; CHG:	ax
		; STT:	ds = ss = debugger segment/selector
		;
		; Idle system, using 2F.1680 (in given mode), or 2F.1680
		; (calling down to 86 Mode), or sti \ hlt.
idle:
	sti
		; Might be required for dosemu2 in loops. Refer to
		;  https://hg.pushbx.org/ecm/insight/rev/7973d5dd16f7
	nop			; help debugging

%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .hlt		; can't call 2F -->
%endif
	testopt [options3], opt3_no_idle_2F
	jnz .hlt
 %if _GUARD_86M_INT2F
%if _PM
	call ispm
	jz @FF			; --> (NZ)
%endif
	push es
	xor ax, ax
	mov es, ax		; (only used in 86 Mode)
	mov ax, [es:2Fh * 4]
	cmp ax, -1
	je @F			; --> (ZR)
	or ax, [es:2Fh * 4 + 2]
@@:
	pop es
	jz @FF
@@:
 %endif
	mov ax, 1680h
	int 2Fh			; release timeslice in multitasker
	test al, al
	jz .return		; done idling -->
@@:
%if _PM
	call ispm
	jnz .hlt

	push bx
	push cx
	push es
	_386_PM_o32	; push edi
	push di
_386	xor edi, edi		; clear EDIH
	xor cx, cx		; (copy no words from stack)

[cpu 286]
	push cx			; ss
	push cx			; sp (0:0 = host should allocate a stack)
	sub sp, byte 12		; cs:ip (ignored), segments (uninitialized)
	pushf
	push cx			; EAXH (uninitialized)
	push 1680h		; AX
	sub sp, byte 12		; ecx, edx, ebx (uninitialized)
	push cx
	push cx			; reserved (zero)
	sub sp, byte 12		; ebp, esi, edi (uninitialized)
	push ss
	pop es
	mov di, sp		; es:(e)di -> 86 Mode call structure
	mov ax, 0300h
	mov bx, 2Fh		; bl = interrupt, bh = reserved (zero)
	int 31h			; call real mode 2F.1680
__CPU__

	add sp, byte 28		; discard RM call structure
	pop ax			; get AX
	add sp, byte 20		; discard RM call structure

	_386_PM_o32	; pop edi
	pop di
	pop es
	pop cx
	pop bx

	test al, al
	jz .return		; done idling -->
%endif
.hlt:
	testopt [options], nohlt
	jnz .return
%if _PM
 %if (protectedmode|dpminohlt)&~0FF00h
  %error Option bits re-ordered, adjust code here
 %endif
	mov al, byte [internalflags+1]
	and al, (protectedmode|dpminohlt)>>8
	xor al, (protectedmode|dpminohlt)>>8
	jz .return		; DPMI host throws GPF when we execute hlt -->
%endif
	sti
	hlt			; else idle by hlt
	nop
.return:
	retn


;	GETLINE - Print a prompt (address in DX, length in CX) and read a line
;	of input.
;	GETLINE0 - Same as above, but use the output line (so far), plus two
;	spaces and a colon, as a prompt.
;	GETLINE00 - Same as above, but use the output line (so far) as a prompt.
;	Entry	CX	Length of prompt (getline only)
;		DX	Address of prompt string (getline only)
;
;		DI	Address + 1 of last character in prompt (getline0 and
;			getline00 only)
;
;	Exit	AL	First nonwhite character in input line
;		SI	Address of the next character after that
;	Uses	AH,BX,CX,DX,DI

getline0:
	mov ax, 32<<8|32	; add two spaces and a colon
	stosw
	mov al, ':'
	stosb
getline00:
	mov dx, line_out
	mov cx, di
	sub cx, dx

getline:	; note: this entry is no longer used
	clropt [internalflags3], dif3_quiet_input_single | dif3_return_eof
.use_dif3_flags:
%if _DEBUG && ! _DEBUG_COND
	d0bp
%endif
	mov word [promptlen], cx	; save length of prompt
	mov byte [linecounter], 0	; reset counter
	and word [terminator_in_line_in.offset], 0
					; reset this when reading new line
	clropt [internalflags], promptwaiting

	call getline_is_input_file?
	jc getline_nofile

getline_file:
	testopt [internalflags3], dif3_input_re
	jnz .notquiet

%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	mov ax, LOAD_INPUT_FILE_SIZE
	push dx
	mul word [load_input_file.active]
	pop dx
	mov di, ax
	testopt [load_input_file + di - LOADDATA3 + ldFATType], ifhfQuietInput
	jmp .quiet_if_nz

@@:
%endif
%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz @F
	testopt [internalflags2], dif2_input_file
	jz @F
	mov di, word [input_file_handles.active]
	shl di, 1
	shl di, 1
	shl di, 1		; to qword array index
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
	testopt [input_file_handles + di + ifhFlags], ifhfQuietInput
	jmp .quiet_if_nz

@@:
%endif
	testopt [internalflags3], dif3_input_cmdline
	jz @F
	testopt [options], opt_cmdline_quiet_input
	; jmp .quiet_if_nz

.quiet_if_nz:
	jz @F
.quiet:
	setopt [internalflags3], dif3_quiet_input_single
.notquiet:
@@:

	setopt [internalflags2], dif2_did_getline_file


		; This part reads the input line from a file (in the case of
		; `debug < file').  It is necessary to do this by hand because DOS
		; function 0Ah does not handle EOF correctly otherwise.  This is
		; especially important for DEBUG because it traps Control-C.
	mov word [lastcmd], dmycmd	; disable auto-repeat while reading from a file

%if _NEWFULLHANDLING
	mov di, line_in+3	; read max
%else
	mov di, line_in+2
%endif
	mov si, word [bufnext]
	cmp si, word [bufend]
	jb .char_buffered	; if there's a character already
	call fillbuf
	jnc .fillbuf_had_data
	testopt [internalflags3], dif3_return_eof
	jz @F
	stc
	retn
@@:
	call getline_close_file
	jnz getline

.fillbuf_had_data:
.char_buffered:
%if _NEWFULLHANDLING
	dec di
%endif

		; Discard an LF if the last character read was CR.
	cmp byte [notatty], 13	; last parsed character was CR ?
	jne .no_lf_skip		; no, nothing more to do -->
	cmp byte [si], 10	; first read character is LF ?
	jne .no_lf_skip		; no -->
	inc si			; skip the LF
	inc byte [notatty]	; avoid repeating this
.no_lf_skip:

	cmp si, word [bufend]
	je @F
	cmp byte [si], '@'	; no display ?
	jne @F

	inc si			; increment past @
	setopt [internalflags3], dif3_quiet_input_single
@@:
	testopt [internalflags3], dif3_quiet_input_single
	jnz gl1

	call puts		; display prompt (having checked it wasn't EOF)

		; si-> next character in buffer
		; w[bufend]-> behind last valid character of buffer
gl1:
	mov cx, word [bufend]
	sub cx, si		; cx = number of valid characters in buffer
	jz gl3			; if none -->
gl2:
	lodsb
	cmp al, 13		; (exact match for CR)
	je gl4
	cmp al, 10
	je gl4			; if EOL -->
	stosb
	loop gl2		; if more valid characters -->

		; The buffer is empty. Fill it again.
gl3:
%if _NEWFULLHANDLING
	inc di
%endif
	call fillbuf
%if _NEWFULLHANDLING
	dec di
%endif
	jnc gl1			; if we have more characters -->
	mov al, 10		; make jump after gl4 always branch
%ifn _NEWFULLHANDLING	; should now always have at least one byte free
	cmp di, line_in+LINE_IN_LEN
	jb @F
	dec si
	dec di
@@:
%endif

gl4:
	cmp al, 13		; terminator was CR ?
	jne @F			; no -->
	jcxz @F			; if no other byte buffered -->
	cmp byte [si], 10	; next byte is an LF ?
	jne @F			; no -->
	lodsb			; increment si past the LF and set al = 10
	; dec cx		; (not used in subsequent code)
@@:
	mov word [bufnext], si
	mov byte [notatty], al	; store 10 or 13 (depending on the kind of EOL)

	setopt [internalflags3], dif3_at_line_end
	call getline_reset_notatty

	mov cx, di		; (counter is for input without CR)
	mov al, 13
	stosb			; terminate line for our usage
	mov dx, line_in + 2
	sub cx, dx		; = length of input (no CR)
	testopt [internalflags3], dif3_quiet_input_single
	jnz @F
	setopt [internalflags], usecharcounter
	call puts		; print out the received line
	clropt [internalflags], usecharcounter
@@:
	mov byte [line_in+1], cl
	jmp getline_eol		; done

getline_nofile:
	call yy_reset_buf
	push cx
	push dx
	call puts		; display prompt
	pop dx
	pop cx
	testopt [options], enable_serial
	jnz .getinput_NZ
	call InDOS_or_BIOS_output
		; Special case: If output is to ROM-BI0S we must use
		;  getinput so as to use putc and friends, not the
		;  DOS interrupt 21h service 0Ah.
	jnz .getinput_NZ
%if _PM
	call ispm
	jnz @F			; if 86 Mode -->
	testopt [options2], opt2_getinput_dpmi
	jnz .getinput_NZ
@@:
%endif
	testopt [options], opt_usegetinput
.getinput_NZ:
	jnz getinput

%if _PM
	call ispm
	jnz @F			; if 86 Mode -->
	testopt [internalflags], canswitchmode
	jz @F			; can't switch to 86M -->
	setopt [internalflags], modeswitched
				; set flag for resetmode
	mov al, 0
	call sr_state		; save state
	call switchmode 	; switch to 86M
	call handle_mode_changed	; ! called with flag set

	mov si, getline_extra_int23
	mov al, 23h
	call install_86m_interrupt_handler
				; override DPMI host's int 23h vector
	mov dx, getline_extra_int22
	mov ax, 2522h
	int 21h
	mov word [TPIV], dx	; override our PRI

	mov dx, line_in
	mov ah, 0Ah
	int 21h			; call DOS

	call getline_extra_uninstall
				; undo patches
	jmp getline_eol_enter_history
				; go to common trail

@@:
%endif
	mov dx, line_in
	mov ah, 0Ah		; buffered keyboard input
	doscall

getline_eol_enter_history:
%if _HISTORY
 %if 0

The history buffer is implemented as two arrays that grow towards
the middle from either end of the buffer. The first is a byte array
storing text data that starts at the low end (start) of the buffer.
(This is offset 0 in the separate history segment, if used.) The
text is stored back to back, only the content of the lines, no
separators or terminators.

The second array gives the *end* of each history entry's text.
There are N + 1 array entries, each a 16-bit offset word, where
N is the amount of history entries in use. The words are used as
displacement from a base that's the start of the history buffer.
(Again, the start base is at offset 0 for the separate history
segment. However, for simplicity of the code we always do store
the actual base start offset into a register and displace from
that, even when the actual value of that register will be zero.)
The very first entry of the high array is special. It always
holds the displacement value zero. When used as a displacement
added to the base this points at the very beginning of the
entire history buffer (and thus the start of the low array).

The size of a history entry in the low array is obtained by
reading both the entry's end displacement from the high array,
and the prior entry (at the next higher address) also from the
high array to receive the end displacement of the prior history
entry. The end displacement of the prior entry is also the
start displacement of the current entry. The delta of the two
end displacements is the size of this history entry. This also
explains why the very first entry of the high array is needed,
and why it contains a zero displacement. It is needed in order
to determine the start displacement and size of the very first
actual history entry. (We could special case the first entry a
different way but using the special first entry of the high
array that holds a constant zero simplifies the code.)

The history pointers in the word [history.first] as well as
word [history.last] point at the very first (special) entry
of the high array as well as the very last entry of the high
array. (Because the high array grows from the top down, the
first entry is at the highest address and the last entry is
at the lowest address.) Although the word [history.first] is
currently a constant, we use a variable to enable changing
the history allocation dynamically if desired later. If the
two pointers are one and the same then there is no actual
history entry stored in the history. Otherwise, if the
distance between the last high array entry and the last
low array entry is lower than X + 2 bytes then the history is
too full to insert a new entry, where X is the length of the
new history entry's text data and the additional 2 bytes are
for the high array entry to store the end displacement.

The special first entry of the high array *must* be
initialised to hold the displacement value zero. This happens
in init.asm for the separate history buffer segment (when
initialising the entire segment with zeroes) or in the
zeroing of most of the DATASTACK section (when initialising
the variables from ..@init_first up to ..@init_behind).

Insertion is simple, if enough space is left in the history
buffer: Copy the text to behind the last entry of the low
array (this is pointed to by the end displacement in the last
high array entry), then create a new last entry of the high
array which points behind the text's destination.

Deletion is more difficult. (Generally we will delete the
oldest entry, that is the first history entry.) The second
entry of the high array needs to be deleted. All subsequent
entries of the high array need to move "forward" (towards
the higher address), and also must have the length of the
text of the entry to delete subtracted. In the low array the
text data of the first history entry needs to be overwritten
with any subsequent text data; the entire subsequent text data
must be moved "forward" (towards the lower address) by the
distance that equals the length of the entry's text to delete.

 %endif

.loop:
	mov si, line_in + 2
	xor cx, cx
	or cl, byte [si - 1]
	jz .dontenter
	gethistorysegment es
	gethistoryoffset bx
	mov ax, word [history.last]
	cmp ax, word [history.first]
	je @F			; if there are no history entries -->
	mov di, ax		; -> last displacement in high array
	mov dx, [es:di]		; + bx -> behind last history entry text
	mov di, [es:di + 2]	; + bx -> at last history entry text
	sub dx, di		; = length of last history entry text
	cmp cx, dx		; length match ?
	jne @F			; no -->

		; always ZR here. would need to keep this in mind if
		;  zero-length entries were considered. (repe cmpsb
		;  with cx = 0 leaves cx, si, di, fl unchanged.)
	lea di, [di + bx]	; -> at last text
	push cx
	push si
	repe cmpsb		; compare candidate text to last text
	pop si
	pop cx
	je .dontenter		; equal, so skip it -->
@@:
	mov di, ax		; -> last displacement
	mov dx, [es:di]		; + bx -> free space
	add dx, bx		; -> free space
	neg dx			; - free space start
	jnz @F
	dec dx			; cause add to overflow (= FFFFh)
	inc di			; adjust for prior instruction
@@:
	add dx, di		; - free space start + free space end
				;  = free space size
	jnc .delete		; invalid (shouldn't happen) -->
	jz .delete		; zero bytes free -->
	dec dx
	jz .delete		; one byte free -->
	dec dx
	; jz .delete		; one or two bytes free -->
		; Commented, a zero will fail the next check
		;  already. Unless we modify the above code
		;  so that empty lines can be entered into
		;  the history, at which point we do not want
		;  to fail the insertion when only two bytes
		;  are free, ie enough for the displacement and
		;  a zero-byte length text.

	cmp cx, dx
	jbe .insert		; enough bytes for the new entry -->
.delete:
	cmp ax, word [history.first]
	jne @F

.error:
	mov dx, msg.history_internal_error
	call putsz
	jmp .dontenter

@@:
	mov di, ax		; -> at last displacement
	mov di, [es:di]		; + bx -> after last entry's text
	mov si, word [history.first]
	mov si, [es:si - 2]	; + bx -> at second entry's text
	sub di, si		; = after last - after first
				;  = text length excluding first
	mov cx, di		; length to move
	push si
	add si, bx		; -> after first entry's text
	mov di, bx		; -> start of buffer
	push es
	pop ds
	rep movsb		; move subsequent text
	pop cx			; = how far we moved (deleted text length)
	mov di, ax		; -> at last displacement
	mov dx, word [di]	; load last displacement
@@:
	inc di
	inc di			; -> prior displacement
	cmp di, word [ss:history.first]
	jae @F			; (hardened, shouldn't ever be above)
	sub dx, cx		; adjust next displacement
				;  by deleted text length
	xchg word [di], dx	; store in prior displacement
				;  and load its old value
	jmp @B

@@:
	push ss
	pop ds
	add word [history.last], 2
				; deleted one displacement
	jmp .loop

.insert:
	mov di, ax
	dec ax
	dec ax
	mov word [history.last], ax
	mov di, word [es:di]
	add di, bx
	rep movsb
	sub di, bx
	xchg di, ax		; es:di -> new last history displacement,
				;  ax -> after history text
	stosw

.dontenter:
	push ss
	pop es
%endif

getline_eol:
	testopt [internalflags3], dif3_quiet_input_single
	jnz @F
	mov al, 13
	call putc		; fix ZDOS Int21.0A display bug
	mov al, 10
	call putc
@@:
set_si_line_in_and_skipwhite:
	mov si, line_in+2
	jmp skipwhite		; NC


%if _PM
getline_extra_uninstall:
	mov dx, debug22
	mov ax, 2522h
	int 21h
	mov word [TPIV], dx	; restore default int 22h (PRI)
	mov si, getline_extra_int23
	mov al, 23h
	mov dx, -1
	call UnhookInterruptForce
				; restore DPMI host's int 23h handler
	jmp resetmode		; return to PM

	usesection lDEBUG_DATA_ENTRY

iispentry getline_extra_int23
	stc
	retf			; indicate to abort syscall and process

	align 2
getline_extra_int22:
	cli
.cleartraceflag:
	cld			; reestablish things
	mov ax, cs
	mov ds, ax
	mov ss, ax
	mov sp, word [ savesp ]	; restore stack
	times 1 - (($ - $$) & 1) nop	; align in-code parameter
	call entry_to_code_seg
	dw .code

	usesection lDEBUG_CODE

.code:

	_386_o32		; mov esp
	mov sp, word [ savesp ]		; restore stack
_386	and sp, ~3			; align stack
	_386_o32
	xor ax, ax
	_386_o32
	push ax
	_386_o32
	popf
_386	mov sp, word [ savesp ]		; restore stack
	cld
	sti

	call getline_extra_uninstall
				; undo patches and return to PM
	jmp handle_ctrl_c	; go to common handler -->
%endif


getline_is_input_file?:
	testopt [internalflags3], dif3_input_serial_override
	jnz .nofile
	testopt [internalflags3], dif3_input_re
	jnz .file
	testopt [internalflags3], dif3_input_cmdline
	jnz .file
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz .file
%endif
	call InDOS_or_BIOS_IO
	jnz .nofile		; InDOS, not reading from a file -->
%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jnz .file
%endif
	testopt [options], enable_serial
	jnz .nofile
	cmp byte [notatty], 0	; check this weird flag
	je .nofile		; not reading from a file -->

.file:
	db __TEST_IMM8		; (skip stc, NC)
.nofile:
	stc
	retn


getline_close_file:
	push cx
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT || 1
		; EOF reached. if not input file, quit. else, close input file.
	testopt [internalflags3], dif3_input_re
	jnz .re
%if _INPUT_FILE_HANDLES && _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file | dif2_input_file_boot, 1
	jnz @F
%elif _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jnz @F
%elif _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz @F
%endif
	testopt [internalflags3], dif3_input_cmdline
	jz .qq			; if EOF, quit -->

	mov cl, -2
	db __TEST_IMM16
@@:
	xor cx, cx
	db __TEST_IMM16
.re:
	mov cl, -1

	push cx
	call .resetstuff
	pop cx

	jcxz .notre
	jmp @F

.notre:
	pop cx
	testopt [internalflags3], dif3_at_line_end
	jnz .return		; (NZ)

	mov ax, 13 | 10 << 8	; pretend we read a CR LF sequence
	stosw
	mov word [bufend], di
	dec di
	dec di
	cmp al, al		; ZR
.return:
	retn

@@:
	cmp cl, -2
	pop cx
	je .finish_cmdline

.finish_re:
	testopt [internalflags3], dif3_at_line_end
	jnz @F
	mov dx, msg.unexpected_noneol_re
	call putsz_error
@@:
	jmp dumpregs_extended.exit

.finish_cmdline:
	testopt [internalflags3], dif3_at_line_end
	jnz @F
	mov dx, msg.unexpected_noneol_rc
	call putsz_error
@@:
	jmp cmd3
%endif

.qq:
	mov byte [line_in + 2], 13
	call set_si_line_in_and_skipwhite
	or word [cmd3_set_options], fakeindos
	call qq			; if EOF, quit -->
	jmp cmd3

.resetstuff:
	push di
	call yy_close_file	; close file
	pop di

%if _NEWFULLHANDLING
	mov word [bufnext], line_in + 3
	mov word [bufend], line_in + 3
%else
	mov word [bufnext], line_in + 2
	mov word [bufend], line_in + 2
%endif

	call getline_reset_notatty

	jmp determine_quiet_output


getinput:
	lframe
	lequ 254,	limit
	lvar word,	columns
	lvar word,	maxpercol
	lvar word,	maxpercolhalf
	lenter
	 push es
	 push dx
	lvar dword,	prompt	; (in line_out buffer)
	xor ax, ax
%if _HISTORY
	 push ax
	lvar word,	historyentry
%endif
	 push ax
	lvar word,	length_displayed
	 push ax
	lvar word,	length_input
	 push ax
	lvar word,	offset
	 push ax
	lvar word,	lastskip
	 push ax
	lvar word,	low_redraw_and_high_beep
	lequ ?low_redraw_and_high_beep,		redraw
	lequ ?low_redraw_and_high_beep + 1,	beep
	 push ax
	lvar word,	low_cursormove_and_high_append
	lequ ?low_cursormove_and_high_append,	cursormove
	lequ ?low_cursormove_and_high_append + 1,append
	 push ax
	lvar word,	low_cursorright_and_high_edited
	lequ ?low_cursorright_and_high_edited, cursorright
	lequ ?low_cursorright_and_high_edited + 1, edited
	 push cx
	lvar word,	column
	 push cx
	lvar word,	promptlength

	push di
	push ds
	pop es

.next:
	call handle_serial_flags_ctrl_c

	rol byte [bp + ?redraw], 1
	jnc .no_do_redraw
.do_redraw:

	call get_columns

	mov word [bp + ?columns], ax
	dec ax				; $COLS - 1
	sub ax, word [bp + ?promptlength]; - $promptlength
	mov word [bp + ?maxpercol], ax
	shr ax, 1			; int($maxpercol / 2)
	jnz @F
	inc ax				; insure nonzero
@@:
	mov word [bp + ?maxpercolhalf], ax

	mov ax, [bp + ?length_input]
	mov cx, ax
	xor di, di			; variable "skip"
					; + line_in + 2 -> to display
	cmp ax, word [bp + ?maxpercol]
	mov ax, word [bp + ?column]	; variable "col"
	jna .not_show_a_maxpercol
.show_a_maxpercol:

	mov dx, word [bp + ?maxpercol]
	add dx, word [bp + ?promptlength]

	mov bx, word [bp + ?lastskip]
	cmp word [bp + ?offset], bx
	jae .offset_ae_lastskip		; spaghetti branch -->
.not_offset_ae_lastskip:

@@:
	cmp ax, dx
	jna .not_col_a_maxpercol
.col_a_maxpercol:
	mov bx, [bp + ?maxpercolhalf]	; bx = step length

.offset_ae_lastskip:		; spaghetti branch target, re-uses the
				;  code of .col_a_maxpercol
	; sub ax, bx			; col -= lastskip
	; add di, bx			; skip += lastskip
	; sub cx, bx			; lessen length of part to display
	sub ax, bx			; lessen col
	add di, bx			; heighten offset of part to display
	sub cx, bx			; lessen length of part to display
	jmp @B

.not_col_a_maxpercol:
	cmp di, word [bp + ?lastskip]	; scrolling forwards ?
	ja @F				; yes --> (else backwards or same)
		; The next check should be redundant with the
		;  one after it. However, the call to puts with
		;  the comment "draw (to move cursor)" will
		;  underflow its counter if ax is decremented
		;  when it was below-or-equal ?promptlength.
		;  Therefore, this check hardens against that.
		; By checking here we ensure that the sub there
		;  doesn't underflow, staying above-or-equal 0.
	cmp ax, word [bp + ?promptlength]
					; possibly at end ?
	jbe @F				; no -->
	cmp ax, dx			; are we at end ?
	jne @F				; no -->
	cmp cx, word [bp + ?maxpercol]	; displaying the last fragment ?
	jbe @F				; yes, do not move up -->
	dec ax
	inc di
	dec cx				; move up display by one column
@@:
	cmp cx, word [bp + ?maxpercol]
	jbe @F
	mov cx, word [bp + ?maxpercol]
@@:
.not_show_a_maxpercol:
	cmp word [bp + ?lastskip], di	; need to scroll ?
	je @F				; no -->
	and word [bp + ?low_cursormove_and_high_append], 0
					; yes, ignore appending status
					; ignore cursor move status too
	mov byte [bp + ?cursorright], 0	; also reset this flag
@@:

	mov word [bp + ?lastskip], di	; update variable for next iteration
	lea dx, [di + line_in + 2]	; -> visible text's data

	mov bx, cx
	xchg bx, word [bp + ?length_displayed]
					; bx = remember how much we had,
					;  and update variable

	rol byte [bp + ?cursormove], 1	; cursor move without visible window move ?
	jc .redraw_always_move_cursor	; yes, skip full redraw and also
					;  force to do cursor movement -->
	push ax
	push bx
	push cx
	push dx
		; On stack:
		;  -> start of visible text's data
		;  length of visible text
		;  prior length of visible text (from prior iteration)
		;  variable "col"
	rol byte [bp + ?append], 1	; text appended without visible window move ?
	jnc .not_append_redraw		; no, do full redraw -->
	not byte [bp + ?append]		; = 0, reset flag after its use
	add dx, cx			; -> behind text to write
	dec dx				; -> at last codepoint (appended)
	mov cx, 1			; only append
%if _GETLINEHIGHLIGHT
		; This appended text is always the very last text
		;  in the buffer so it never should get highlighted.
		; If this build option is not in use then the next
		;  branch is to the call puts below, re-using that
		;  call there that's also used for the full redraw.
	call puts			; draw appended text
%endif
	jmp .after_append_redraw	; skip past more prompt/text redraw

.not_append_redraw:
	push cx
	push dx
	mov al, 13
	call putc			; reset to start of line

	les dx, [bp + ?prompt]
	mov cx, [bp + ?promptlength]
	call puts			; redraw the prompt
	 push ss
	 pop es				; reset es
	pop dx
	pop cx				; restore registers for text redraw
%if _GETLINEHIGHLIGHT
					; di for prefix highlight condition
	mov bx, word [bp + ?length_input]
					; bx for suffix highlight condition
	call puts_with_highlight	; redraw the text
.after_append_redraw:
%else
.after_append_redraw:
	call puts			; redraw the text
%endif
	pop dx
	pop bx
	pop cx
		; dx -> start of visible text's data
		; bx = new length of visible text
		; cx = prior length of visible text (from prior iteration)
		; on stack = variable "col"
	sub cx, bx			; = how many blanks needed to overwrite
	jbe @FF
	add bx, cx			; = length displayed
	mov al, 32
@@:
	call putc
	loop @B
@@:
	pop ax
		; dx -> start of visible text's data
		; ax = variable "col"
		; bx = length of redrawn text (including blanks),
		;  this indicates where the cursor is

		; The following check determines whether the
		;  cursor is already where we want it to be
		;  after the full redraw is done, including the
		;  blanks after the visible text that were used
		;  to erase the prior visible text (if any).
		; If this branches then the cursor movement is
		;  not needed and thus we're done redrawing.
	add bx, dx
	sub bx, line_in + 2		; offset into line (with length displayed)
	add bx, word [bp + ?promptlength]; offset into display
	cmp bx, word [bp + ?column]	; same as column ?
	je .do_redraw_done		; yes, skip cursor movement stuff -->

.redraw_always_move_cursor:
	mov byte [bp + ?cursormove], 0	; reset flag after its use

		; dx -> start of visible text's data
		; ax = variable "col"
	rol byte [bp + ?cursorright], 1	; only cursor to the right without
					;  having to scroll the visible text ?
	jnc @F				; no, do redraw -->
	not byte [bp + ?cursorright]	; = 0, reset for next iteration
	mov cx, 1			; length of text to write
	sub ax, word [bp + ?promptlength]
					; = offset into input text
	dec ax				; -> last codepoint of text
	;; test ax, ax			; at start of visible text ?
	jz .redraw_cursorright		; yes, leave di so as to highlight
					;  the prefix as appropriate -->
					; (and dx is correct, we'd add zero)
	xor di, di			; else, tell it not to highlight
	add dx, ax			; dx = "skip" + line_in + 2 + index
	jmp .redraw_cursorright

@@:
	push ax
	push dx
	mov al, 13
	call putc			; reset cursor
	les dx, [bp + ?prompt]
	mov cx, [bp + ?promptlength]
	call puts			; redraw prompt
	 push ss
	 pop es
	pop dx				; -> current input text
	pop cx				; = col
	sub cx, word  [bp + ?promptlength]
					; = offset into input text,
					;  draw that much
.redraw_cursorright:
%if _GETLINEHIGHLIGHT
		; ! di is still set to lastskip value,
		;  or reset to zero if cursor right with
		;  just a single codepoint redraw but the
		;  codepoint to redraw is not at the start
		;  of the visible text.
					; di for prefix highlight condition
		; We need to make sure that the suffix is never
		;  highlighted here because we are writing a
		;  fragment that does not include the last part
		;  of the visible text. By zeroing bx we can
		;  ensure the suffix highlight condition is
		;  always considered to be false.
	xor bx, bx			; bx = 0, never highlight a suffix
	call puts_with_highlight
%else
	call puts			; draw (to move cursor)
%endif

.do_redraw_done:
	not byte [bp + ?redraw]		; = 0

.no_do_redraw:

	rol byte [bp + ?beep], 1
	jnc .no_do_beep
.do_beep:
	mov al, 7
	call putc

	not byte [bp + ?beep]		; = 0
.no_do_beep:

	call getc			; ax = keycode, al = ASCII or 0

		; check scancode for int 16h in non-dumb dosemu first
	xchg al, ah
	cmp al, 48h
	je .up
	cmp al, 4Bh
	je .left
	cmp al, 50h
	je .down
	cmp al, 4Dh
	je .right
	cmp al, 52h
	je .insert
	cmp al, 53h
	je .del
	cmp al, 47h
	je .home
	cmp al, 4Fh
	je .end
	cmp al, 49h
	je .pageup
	cmp al, 51h
	je .pagedown

		; not a special scancode, check character returned
	xchg al, ah
	cmp al, 1Bh
	jne .not_esc

		; check escape scancodes for int 16h in -dumb dosemu
		;  or input from serial I/O (picocom/screen/ssh/mate-terminal)
.esc:
	call getc
	cmp al, '['
	jne .beep
	call getc
	cmp al, 41h
	je .up
	cmp al, 44h
	je .left
	cmp al, 42h
	je .down
	cmp al, 43h
	je .right
	cmp al, 32h
	je .check_insert
	cmp al, 33h
	je .check_del
	cmp al, 31h
	je .check_home
	cmp al, 34h
	je .check_end
	cmp al, 48h
	je .home
	cmp al, 46h
	je .end
	cmp al, 35h
	je .check_pageup
	cmp al, 36h
	je .check_pagedown
	jmp .beep


.check_insert:
.check_del:
.check_home:
.check_end:
.check_pageup:
.check_pagedown:
	push ax
	call getc
	cmp al, 7Eh
	pop ax
	jne .beep

	cmp al, 32h
	je .insert
	cmp al, 33h
	je .del
	cmp al, 31h
	je .home
	cmp al, 34h
	je .end
	cmp al, 35h
	je .pageup
	cmp al, 36h
	je .pagedown

	jmp .beep


.not_esc:
		; check regular characters
	test al, al			; waste?
	je .next
	cmp al, 0E0h
	je .next			; waste -->

	cmp al, 03h
	je .ctrlc
	cmp al, 09h
	je .beep		; tab
	cmp al, 08h
	je .backspace
	cmp al, 7Fh
		; On the server in int 16h of -dumb dosemu, as well as on
		;  both systems across serial I/O this code is used.
	je .backspace
	cmp al, 10			; (allow Linux style linebreak)
	je .done
	cmp al, 13			; (match for CR keypress)
	je .done

.textcodepoint:
	mov bx, word [bp + ?length_input]
	cmp bx, ?limit
	jb @F
.redraw_and_beep:
	mov byte [bp + ?redraw], -1
	jmp .beep
@@:

	mov byte [bp + ?edited], -1
	mov dx, word [bp + ?length_input]
	mov cx, dx
	mov di, dx
	push ss
	pop es
	add di, line_in + 2		; di -> behind end of input
	mov si, di
	dec si				; si -> at last entry of input
	sub cx, word [bp + ?offset]
	jnz @F				; not appending -->
.textappend:
	mov byte [bp + ?append], -1
@@:
	std				; AMD erratum 109 workaround done
	numdef AMD_ERRATUM_109_WORKAROUND, 1
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
	cld
	mov byte [di], al
	inc word [bp + ?column]
	inc word [bp + ?offset]
	inc word [bp + ?length_input]
.redraw:
	mov byte [bp + ?redraw], -1
	jmp .next

.backspace:
	cmp word [bp + ?offset], 0
	je .backspace_offset_0

	mov byte [bp + ?edited], -1
	push ss
	pop es
	mov di, word [bp + ?offset]
	mov cx, word [bp + ?length_input]
	sub cx, di			; length after offset
	add di, line_in + 2		; -> at offset in line
	mov si, di			; -> at offset in line
	dec di				; -> at offset - 1 in line
	rep movsb			; move down additional content
	dec word [bp + ?column]
	dec word [bp + ?offset]
	dec word [bp + ?length_input]
	jmp .redraw

.backspace_offset_0:
.del_offset_ae_length_input:
.left_offset_0:
.right_offset_too_high:
	jmp .beep

.del:
	mov ax, word [bp + ?offset]
	cmp ax, word [bp + ?length_input]
	jae .del_offset_ae_length_input
	mov byte [bp + ?edited], -1
	mov di, ax
	add di, line_in + 2		; -> at current offset
	mov si, di
	inc si				; -> after current offset
	inc ax				; offset + 1
	neg ax				; - (offset + 1)
	add ax, word [bp + ?length_input]; length input - (offset + 1)
	mov cx, ax
	rep movsb			; move down part after deleted point
	dec word [bp + ?length_input]
	jmp .redraw

.home:
	and word [bp + ?offset], 0
	push word [bp + ?promptlength]
	pop word [bp + ?column]
	jmp .redraw

.end:
	mov ax, word [bp + ?length_input]
	mov word [bp + ?offset], ax
	add ax, word [bp + ?promptlength]
	mov word [bp + ?column], ax
	jmp .redraw

.left:
	cmp word [bp + ?offset], 0
	je .left_offset_0
	mov byte [bp + ?cursormove], -1
	dec word [bp + ?offset]
	dec word [bp + ?column]
	jmp .redraw

.right:
	mov ax, word [bp + ?offset]
	cmp ax, word [bp + ?length_input]
	jnb .right_offset_too_high
	mov byte [bp + ?cursormove], -1
	mov byte [bp + ?cursorright], -1
	inc word [bp + ?offset]
	inc word [bp + ?column]
	jmp .redraw

.up:
	rol byte [bp + ?edited], 1
	jc .beep
%if _HISTORY
	mov si, word [bp + ?historyentry]
	test si, si
	lea si, [si + 2]
	jnz @F
	mov si, word [history.last]
@@:
	cmp si, word [history.first]
	je .beep
	jmp .copyline

.down:
	rol byte [bp + ?edited], 1
	jc .beep
	mov si, word [bp + ?historyentry]
	test si, si
	jz .beep
	cmp si, word [history.last]
	jne @F
	xor ax, ax
	xor cx, cx
	jmp .setline

@@:
	dec si
	dec si

.copyline:
	mov ax, si
	gethistorysegment ds
	mov cx, [si]
	mov si, [si + 2]
	sub cx, si

.setline:
	mov word [bp + ?historyentry], ax
	gethistoryoffset bx
	lea si, [si + bx]
	push ss
	pop es
	mov di, line_in + 1
	mov al, cl
	stosb
	rep movsb
	mov al, 13
	stosb
	push ss
	pop ds
%else
.down: equ .beep

	mov byte [bp + ?edited], -1
%endif
	xor ax, ax
	mov al, byte [line_in + 1]
	mov word [bp + ?offset], ax
	mov word [bp + ?length_input], ax
	add ax, word [bp + ?promptlength]
	mov word [bp + ?column], ax
	and word [bp + ?lastskip], 0
	jmp .redraw

.insert:
.pageup:
.pagedown:
.beep:
	mov byte [bp + ?beep], -1
	jmp .next

.ctrlc:
	jmp handle_ctrl_c

.done:
	mov di, word [bp + ?length_input]

	mov dx, word [bp + ?lastskip]
	add dx, word [bp + ?length_displayed]
					; offset into line (with length displayed)
	add dx, word [bp + ?promptlength]; offset into display
	cmp dx, word [bp + ?column]	; same as column ?
	jne @F				; no, always redraw -->

	mov ax, di
	add ax, word [bp + ?promptlength]
	cmp word [bp + ?columns], ax
	ja .done_no_redraw

@@:
	mov al, 13
	call putc			; reset cursor
	les dx, [bp + ?prompt]
	mov cx, [bp + ?promptlength]	; es:dx length cx -> prompt data
%if _40COLUMNS
	xor ax, ax			; last fragment length = 0
	call puts_break_line
	 push ss
	 pop es
	mov cx, word [bp + ?length_input]
	jcxz @F				; if empty line -->
	call puts_break_line_more	; more follows, do a linebreak if ax == 0
@@:
	mov dx, line_in + 2		; es:dx length cx -> input line data
	call puts_break_line		; INP:ax = last fragment length
	; jmp .done_no_redraw
	; (fall through)
%else
	call puts			; redraw prompt
	 push ss
	 pop es
	mov dx, line_in + 2
	mov cx, word [bp + ?length_input]
	call puts			; draw
%endif

.done_no_redraw:
	add di, line_in + 2
	mov al, 13
	stosb				; store the CR (there always is room)
	xchg ax, di			; -> behind CR
	sub al, ((-section.DATASTACK.vstart+100h+ldebug_data_entry_size \
				+asmtable1_size+asmtable2_size) \
		 +line_in+3) & 0FFh	; length of string, excluding CR
		; (This instruction disregards the unnecessary higher byte.)
	pop di				; restore di
	mov byte [line_in+1], al	; store the length byte
	lleave code
	jmp getline_eol_enter_history

	lleave ctx


fullbsout:
	mov al, 8
	call putc
	mov al, 32
	call putc
	mov al, 8
	jmp putc


		; INP:	-
		; OUT:	ax = number of columns to use
		; STT:	ds = ss = debugger data selector
get_columns:
	xor ax, ax
	testopt [options], enable_serial ; serial ?
	jz @F				; no -->
	or al, byte [serial_columns]	; ax = number of columns if serial
	jz .default_columns
	cmp al, 1
	jne @FF
	dec ax				; = 0
	jmp @F				; use IOC / BDA selection
.default_columns:
	mov al, 80
	jmp @FF
@@:
	or al, [io_columns]
	jz .default_columns
	cmp al, 1			; is 1 ?
	jne @F				; no, use as columns -->
					; yes, automatic (use BDA)
	push es
	mov ax, 40h			; 0040h is a bimodal segment/selector
	mov es, ax
	mov ax, word [ es:4Ah ]		; columns on screen
	pop es
@@:
	retn


%if _GETLINEHIGHLIGHT
		; INP:	di = index from total text start to visible text
		;	cx = length of visible text to display
		;	dx -> visible text to display
		;	bx = length of total text
		;	di != 0 if to highlight first codepoint
		;	di + cx < bx if to highlight last codepoint
		; OUT:	if INP:cx == 0,
		;	 just returns
		;	if opt3_getline_highlight not set,
		;	 just chains to puts
		;	no prefix highlight if INP:di == 0,
		;	 else first codepoint displayed highlighted
		;	no suffix highlight if INP:di + INP:cx >= INP:bx,
		;	 else last codepoint displayed highlighted
		;	calls puts and/or putsz to display all text
		; CHG:	ax, bx, cx, dx
puts_with_highlight:
	jcxz .retn
	testopt [options3], opt3_getline_highlight
	jz .justputs
	push cx
	test di, di			; prefix to highlight ?
	jz @F				; no -->
.prefix:
	push bx
	push dx				; ! dx on stack
	mov dx, msg.highlight + 1	; -> ASCIZ escape code
	call putsz
	 pop dx				; ! restore dx -> data
	 push dx
	push cx
	mov cx, 1
	call puts			; draw one codepoint
	pop cx
	mov dx, msg.unhighlight + 1	; -> ASCIZ escape code
	call putsz
	 pop dx				; ! restore dx -> data
	inc dx				; -> second codepoint
	dec cx				; length one less
	pop bx
@@:
	pop ax				; = length of text (including prefix)
	add ax, di			; = index past last displayed
	cmp ax, bx			; is it equal to total text length ?
	jae .justputs			; yes, just display -->

.suffix:
	jcxz .retn			; if no suffix possible -->
	dec cx				; = length of unhighlighted text
	mov ax, cx
	add ax, dx			; -> at codepoint to highlight
	push ax
	call puts			; draw unhighlighted text
	mov dx, msg.highlight + 1	; -> ASCIZ escape code
	call putsz
	pop dx				; -> at codepoint to highlight
	mov cx, 1
	call puts			; draw one codepoint
	mov dx, msg.unhighlight + 1	; -> ASCIZ escape code
	call putsz
.retn:
	retn

.justputs:
	jmp puts			; draw text (no highlight)
%endif


getline_reset_notatty:
	testopt [internalflags3], dif3_input_re_closed
	jz @F
	clropt [internalflags3], dif3_input_re_closed
	testopt [internalflags3], dif3_input_cmdline
	jnz @F
%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz .notfile1
	testopt [internalflags2], dif2_input_file
	jnz @F
.notfile1:
%endif
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz @F
%endif
	jmp .clear_notatty

@@:
	testopt [internalflags3], dif3_input_cmdline_closed
	jz @F
	clropt [internalflags3], dif3_input_cmdline_closed
%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz .notfile2
	testopt [internalflags2], dif2_input_file
	jnz @F
.notfile2:
%endif
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz @F
%endif
	jmp .clear_notatty

@@:
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_closed_input_file_boot
	jz @F
	clropt [internalflags2], dif2_closed_input_file_boot
%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz .notfile3
	testopt [internalflags2], dif2_input_file
	jnz @F
.notfile3:
%endif
	jmp .clear_notatty
@@:
%endif
%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_closed_input_file
	jz @F
	clropt [internalflags2], dif2_closed_input_file
%else
	jmp @F
%endif
.clear_notatty:
	testopt [internalflags], inputfile | notstdinput
	jnz @F
	mov byte [notatty], 0	; it _is_ a tty
@@:
	retn


		; Fill input buffer from file.
		;
		; INP:	di-> first available byte in input buffer
		; OUT:	CY if DOS returned an error or EOF occured
		;	NC if no error
		;	si = di
		; CHG:	-
fillbuf:
	call handle_serial_flags_ctrl_c
	push ax
	push bx
	push cx
	push dx
	mov si, di		; we know this already
	mov cx, line_in+LINE_IN_LEN
	mov dx, di
	sub cx, di
	jbe .ret_cy		; if no more room -->

	testopt [internalflags3], dif3_input_re
	jz .not_re
	push si
	push di
	mov bx, cx
	mov si, word [re_buffer.position]
	mov di, si
	mov cx, -1
	xor ax, ax
	repne scasb
	not cx
	dec cx
	cmp bx, cx
	ja @F
	mov cx, bx
@@:
	mov ax, cx
	mov di, dx
	rep movsb
	mov word [re_buffer.position], si
	pop di
	pop si
	clc
	jmp .after

.not_re:

%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	call yy_boot_remember_seek
	call yy_boot_read
	jmp .after
@@:
%endif

%if _INPUT_FILE_HANDLES
	call InDOS_or_BIOS_IO
	jnz @F
	testopt [internalflags2], dif2_input_file
	jz @F			; if not input file -->
	push di
	call yy_get_handle	; bx = handle
	pop di
	jmp .file_handle
%endif

@@:
	testopt [internalflags3], dif3_input_cmdline
	jz .not_cmdline
	push si
	push di
	mov bx, cx
	mov si, word [cmdline_buffer.position]
	mov di, si
	mov cx, -1
	xor ax, ax
	repne scasb
	not cx
	dec cx
	cmp bx, cx
	ja @F
	mov cx, bx
@@:
	mov ax, cx
	mov di, dx
	rep movsb
	mov word [cmdline_buffer.position], si
	pop di
	pop si
	clc
	jmp .after

.not_cmdline:

@@:
	xor bx, bx		; bx = handle (0 is STDIN)
	call InDOS_or_BIOS_IO
	jnz .ret_cy

.file_handle:
	mov ah, 3Fh		; read from file
	call yy_remember_seek
	doscall
.after:
	jc .ret_cy		; if error -->
	test ax, ax
	jz .ret_cy		; if EOF -->
	add dx, ax		; -> behind last valid byte

	clropt [internalflags3], dif3_at_line_end

	db __TEST_IMM8		; (NC)
.ret_cy:
	stc
	mov word [bufend], dx	; -> behind last valid byte
	pop dx
	pop cx
	pop bx
	pop ax
	retn


%ifn _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
yy equ error
%else
yy:
	call guard_re
%if _INPUT_FILE_BOOT
	testopt [internalflags], nodosloaded
	jnz yy_boot
%endif
%ifn _INPUT_FILE_HANDLES
	jmp error
%else
	call InDOS_or_BIOS_IO
	jz @F
	mov ax, 0305h
	call setrc
	mov dx, msg.yy_no_dos
	jmp .disp_error_1

@@:
		; If input_file_handles.to_close is set,
		;  close all left over open files. (This
		;  may only be done when DOS is available.)
	xor bx, bx		; if none left open, -> first structure
	testopt [internalflags2], dif2_input_file
	jz @F
	mov bx, word [input_file_handles.active]
	inc bx			; point to first unused structure
	shl bx, 1
	shl bx, 1
	shl bx, 1		; to qword array index
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
@@:
	mov di, bx		; + input_file_handles -> to close
	xor ax, ax		; zero base amount to close
	call yy_close_file_handles
				; CHG: ax, bx, di
				; if we remembered to close any, do it now

	dec si
	mov bx, si		; -> start of name
	mov di, si		; -> start of name
	lodsb			; load character
	call iseol?
	jne @F
	mov dx, msg.yy_requires_filename
	mov ax, 0304h
	call setrc
.disp_error_1:
	mov ax, 03FFh
	call setrc
	call putsz_error
	jmp cmd3

@@:
.unquoted_loop:
	cmp al, 32		; blank or EOL outside quoted part ?
	je .blank
	cmp al, 9
	je .blank
	call iseol?		; (includes semicolon in lDebug)
	je .blank		; yes -->
	cmp al, '"'		; starting quote mark ?
	je .quoted		; yes -->
	stosb			; store character
.unquote:
	lodsb			; load character
	jmp .unquoted_loop	; continue in not-quoted loop -->

.quoted_loop:
	call iseol?.notsemicolon; EOL inside quoted part ?
	je .quoted_eol		; if yes, error -->
	cmp al, '"'		; ending quote mark ?
	je .unquote		; yes -->
	stosb			; store character
.quoted:
	lodsb			; load character
	jmp .quoted_loop	; continue in quoted loop -->

.empty:
	mov ax, 0306h
	call setrc
	mov dx, msg.yy_filename_empty
	jmp .disp_error_1

.quoted_eol:
	mov ax, 0307h
	call setrc
	mov dx, msg.yy_filename_missing_unquote
	jmp .disp_error_1

.blank:
	; mov byte [si - 1], 0	; terminate (shouldn't be needed)

	mov al, 0
	xchg al, byte [di]	; terminate after filename
	mov word [terminator_in_line_in.offset], di
	mov byte [terminator_in_line_in.value], al
	cmp bx, di		; empty ?
	je .empty		; yes -->
				; done

	testopt [internalflags2], dif2_input_file
	jz @F
; IFH = 1
; IFH - 1 = 0
; cmp active, 0
; active >= 0 --> error

; IFH = 2
; IFH - 1 = 1
; cmp active, 1
; active >= 1 --> error
	cmp word [input_file_handles.active], _INPUT_FILE_HANDLES - 1
	jb @F

	mov ax, 0308h
	call setrc
	mov dx, msg.yy_too_many_handles
	jmp .disp_error_1
@@:

	cmp byte [bx], ':'
	jne .not_yy_goto_subfunction

	testopt [internalflags2], dif2_input_file
	jnz @F

	call getline_is_input_file?
	jc .no_file
	testopt [internalflags3], dif3_input_cmdline
	jnz .no_file
@@:
	inc bx
	mov bp, bx

	call yy_reset_buf

	xor bx, bx		; bx = handle (0 is STDIN)
%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jz @F			; if not input file -->
	push di
	call yy_get_handle	; bx = handle
	pop di
@@:
%endif
	mov ah, 45h
	doscall			; duplicate file handle
	jc yy_open_file.error
	xchg ax, bx

	xor cx, cx
	xor dx, dx
	mov ax, 4201h		; lseek, from current file position
	doscall			; call DOS
	xchg ax, cx		; dx:cx = current seek
	xchg dx, cx		; cx:dx = current seek
	mov si, ifhfIsDup
	xchg ax, bx
	jmp yy_finish


.no_file:
	mov ax, 0309h
	call setrc
	mov dx, msg.yy_no_file
	jmp .disp_error_1

.not_yy_goto_subfunction:
	xor bp, bp
	call skipwh0
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	mov word [if_exists_then_address], si
%endif
	call iseol?_or_then
	je .not_yy_goto

	cmp al, ':'
	jne error

	call skipwhite
	dec si
	mov bp, si

@@:
	lodsb
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	call iseol?
	jne @B
@@:
	call skipwh0
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	mov word [if_exists_then_address], si
%endif
	call chkeol_or_then

.not_yy_goto:
	mov di, bx

		; INP:	ds:di -> filename
		; OUT:	File opened,
		;	 bx = file handle
		; STT:	ds = es = ss = debugger data selector/segment
yy_open_file:
	call .setup_opencreate			; ds:si -> pathname
	mov ax, 716Ch				; LFN open-create
	push di
	xor di, di				; alias hint
	stc
	doscall
	pop di
	jnc .got		; LFN call succeeded -->

		; Early case for no-LFN-interface available.
	; cmp ax, 1
	; je .try_sfn
	cmp ax, 7100h
	je .try_sfn

		; Only now, we check whether the used drive supports LFNs.
		; If it does, then we treat the error received as an
		; actual error and cancel here. If not, the SFN function
		; is called next as a fallback.
		;
		; We cannot rely on specific error returns like the
		; expected 7100h CY (or 7100h CF-unchanged) or the similar
		; 0001h CY (Invalid function) because no one agrees on what
		; error code to use.
		;
		; dosemu returns 0003h (Path not found) on FATFS and
		; redirected-non-dosemu drives. But may be changed so as to
		; return 0059h (Function not supported on network).
		; MSWindows 98SE returns 0002h (File not found) on
		; DOS-redirected drives.
		; DOSLFN with Fallback mode enabled supports the call (albeit
		; limited to SFNs).
		;
		; To suss out what the error means, check LFN availability.
		;
		; Refer to https://github.com/stsp/dosemu2/issues/770
	push ds
	push es
	push di
	push ax
	lframe
	lvar 34, fstype_buffer
	lvar 4, pathname_buffer
	lenter

	lodsw			; load first two bytes of pathname

	push ss
	pop ds
	mov dx, sp		; ds:dx -> ?pathname_buffer
	push ss
	pop es
	mov di, sp		; es:di -> ?pathname_buffer

	cmp ah, ':'		; starts with drive specifier ?
	je @F			; yes -->

	mov ah, 19h
	doscall			; get current default drive
	add al, 'A'		; A: = 0, convert to drive letter
	mov ah, ':'		; drive specifier
@@:
	stosw
	mov ax, '\'		; backslash and zero terminator
	stosw			; es:di -> ?fstype_buffer

	xor ax, ax
	mov cx, 34 >> 1
	push di
	rep stosw		; initialise ?fstype_buffer to all zeros
	pop di			; -> ?fstype_buffer

	mov cx, 32		; size of ?fstype_buffer
	xor bx, bx		; harden, initialise this
	mov ax, 71A0h		; get volume information
	stc
	doscall			; (depends on ds = es = ss)

	jc @F			; if call not supported -->
				; bx = FS flags
	test bh, 0100_0000b	; LFN interface available ?
	stc			; if no
	jz @F			; no -->

	clc			; is available
@@:

	lleave
	pop ax			; (restore error code)
	pop di
	pop es
	pop ds
	jnc .error		; if LFN interface is available, actual error
				; if LFN interface is not available, try SFN

.try_sfn:
	call .setup_opencreate
	mov ax, 6C00h				; Open-create
	stc
	doscall
	jnc .got

	cmp ax, 1
	je .try_old_open
	cmp ax, 6C00h
	jne .error

.try_old_open:
	mov al, bl				; access and sharing modes
	mov ah, 3Dh				; Open
	mov dx, si				; -> filename
	stc
	doscall
	jnc .got

.error:
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_not_found
%endif
	mov ax, 030Ah
	call setrc
	mov dx, msg.yy_error_file_open
	jmp yy.disp_error_1

.setup_opencreate:
	mov si, di				; -> filename
	mov bx, 0110_0000_0010_0000b		; Auto-commit, no int 24h
						; DENY WRITE, Read-only
	xor cx, cx				; create attribute
	mov dx, 0000_0000_0000_0001b		; no create / open, no truncate
	retn

.got:
		; ax = file handle
	call yy_reset_buf

	xor cx, cx
	xor dx, dx
	xor si, si
yy_finish:
	testopt [internalflags2], dif2_input_file
	jnz @F
	setopt [internalflags2], dif2_input_file
	xor bx, bx
	jmp @FF

@@:
	inc word [input_file_handles.active]
	mov bx, word [input_file_handles.active]
	shl bx, 1
	shl bx, 1
	shl bx, 1		; to qword array index
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
	mov di, word [input_file_handles + bx - INPUTFILEHANDLE_size + ifhFlags]
	and di, ifhfTestReserved1 | ifhfTestReserved2 \
		| ifhfQuietInput | ifhfQuietOutput
	or si, di
@@:

	mov word [input_file_handles + bx + ifhHandle], ax
	mov word [input_file_handles + bx + ifhFlags], si
	mov word [input_file_handles + bx + ifhParentSeek], dx
	mov word [input_file_handles + bx + ifhParentSeek + 2], cx

	clropt [internalflags3], dif3_auxbuff_guarded_1
	mov si, bp
	test si, si
	jnz cmd_goto.yy_entry

%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_found_open
%endif
	retn
%endif
%endif


		; INP:	word [bufnext], word [bufend]
		; CHG:	-
		;
		; Note:	When reading from a file, we buffer some of the input
		;	 in line_in. When switching to a non-file, or starting
		;	 to read from another file, we have to reset the seek
		;	 position of the (prior) file to avoid losing the data.
		;	This cropped up during yy development, but actually
		;	 affects serial and InDOS input, too. Therefore,
		;	 this function is not below the conditional for yy.
yy_reset_buf:
	push cx
%if _NEWFULLHANDLING
	mov cx, line_in + 3
%else
	mov cx, line_in + 2
%endif
	xchg cx, word [bufnext]
	neg cx
	add cx, word [bufend]	; cx = how much remaining in buffer
%if _NEWFULLHANDLING
	mov word [bufend], line_in + 3
%else
	mov word [bufend], line_in + 2
%endif

	testopt [internalflags2], dif2_did_getline_file
	jz .not_used

	push ax
	push dx
	push bx
	mov dx, cx
	xor cx, cx
	neg dx			; dx = minus how much remaining
	jz .done		; if zero, do not seek at all -->
	dec cx			; sign extension into cx:dx

	testopt [internalflags3], dif3_input_re
	jz @F
	add word [re_buffer.position], dx
	jmp .done
@@:

%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	testopt [internalflags3], dif3_auxbuff_guarded_1 | dif3_auxbuff_guarded_2
	jz .do_boot_seek

	push word [load_input_file.active]
	pop word [boot_remember_seek_handle]
	mov word [boot_remember_seek_offset + 2], cx
	mov word [boot_remember_seek_offset], dx
	jmp .done

.do_boot_seek:
	call yy_boot_seek_current
	jmp .done
@@:
%endif

%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jz @F			; if not input file -->
	push di
	call yy_get_handle	; bx = handle
	pop di
	jmp .filehandle
@@:
%endif

	testopt [internalflags3], dif3_input_cmdline
	jz @F
	add word [cmdline_buffer.position], dx
	jmp .done
@@:

	xor bx, bx		; bx = handle (0 is STDIN)
.filehandle:
	mov ax, 4201h		; lseek, from current file position
	call handle_seek_or_remember

.done:
	pop bx
	pop dx
	pop ax

	clropt [internalflags2], dif2_did_getline_file

.not_used:
	pop cx
	retn


handle_seek_or_remember:
	call InDos
	jz @F

	mov word [indos_remember_seek_function], ax
	mov word [indos_remember_seek_handle], bx
	mov word [indos_remember_seek_offset + 2], cx
	mov word [indos_remember_seek_offset], dx
	jmp .done

@@:
	doscall			; call DOS
.done:
	retn


yy_reset_buf_and_seek_start:
	push cx
	push ax
	push dx
	push bx
%if _NEWFULLHANDLING
	mov ax, line_in + 3
%else
	mov ax, line_in + 2
%endif
	mov word [bufnext], ax
	mov word [bufend], ax

	xor dx, dx
	xor cx, cx

	testopt [internalflags3], dif3_input_re
	jz @F
	mov word [re_buffer.position], re_buffer
	jmp .done
@@:

%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	call yy_boot_clear_remember_seek
	call yy_boot_seek_start
	jmp .done
@@:
%endif

%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jz @F			; if not input file -->
	push di
	call yy_get_handle	; bx = handle
	pop di
	jmp .filehandle
@@:
%endif

	testopt [internalflags3], dif3_input_cmdline
	jz @F
	mov word [cmdline_buffer.position], cmdline_buffer
	jmp .done
@@:

	xor bx, bx		; bx = handle (0 is STDIN)

.filehandle:
	call yy_clear_remember_seek
	mov ax, 4200h		; seek from start
	call handle_seek_or_remember
.done:
	pop bx
	pop dx
	pop ax
	pop cx
	retn


yy_clear_remember_seek:
	cmp word [indos_remember_seek_handle], bx
	jne .ret
.clear:
	mov word [indos_remember_seek_function], 4201h
	or word [indos_remember_seek_handle], -1
	and word [indos_remember_seek_offset + 2], 0
	and word [indos_remember_seek_offset], 0
.ret:
	retn


yy_remember_seek:
	cmp word [indos_remember_seek_handle], bx
	jne yy_clear_remember_seek.ret

	push ax
	push cx
	push dx
	mov cx, word [indos_remember_seek_offset + 2]
	mov dx, word [indos_remember_seek_offset]
	mov ax, word [indos_remember_seek_function]
	doscall			; call DOS
	pop dx
	pop cx
	pop ax
	jmp yy_clear_remember_seek.clear


%if _INPUT_FILE_BOOT
yy_boot_clear_remember_seek:
	push ax
	mov ax, word [load_input_file.active]
	cmp word [boot_remember_seek_handle], ax
	jne .ret_pop_ax
.clear:
	or word [boot_remember_seek_handle], -1
	and word [boot_remember_seek_offset + 2], 0
	and word [boot_remember_seek_offset], 0
.ret_pop_ax:
	pop ax
	retn


yy_boot_remember_seek:
	push ax
	mov ax, word [load_input_file.active]
	cmp word [boot_remember_seek_handle], ax
	jne yy_boot_clear_remember_seek.ret_pop_ax

	push bx
	push cx
	push dx
	mov cx, word [boot_remember_seek_offset + 2]
	mov dx, word [boot_remember_seek_offset]
	call yy_boot_seek_current
	pop dx
	pop cx
	pop bx
	jmp yy_boot_clear_remember_seek.clear
%endif


cmd_goto:
	call skipwhite
	cmp al, ':'
	jne @F
	call skipwhite
@@:

	call getline_is_input_file?
	jnc @F
	mov dx, msg.goto_not_file
	mov ax, 0300h
	call setrc
.error:
	mov ax, 03FFh
	call setrc
	jmp putsz_error

@@:
	dec si
.yy_entry:		; si -> destination label
	mov dx, msg.sof
	call isstring?
	jne @F
	call skipwhite
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	mov word [if_exists_then_address], si
%endif
	call chkeol_or_then
	call resetrc
	call yy_reset_buf_and_seek_start
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_found_open
%endif
	retn

@@:
	mov dx, msg.eof
	call isstring?
	jne @F
	call skipwhite
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	mov word [if_exists_then_address], si
%endif
	call chkeol_or_then
	call resetrc
	call getline_close_file
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_found_closed
%endif
	retn

@@:
	mov bx, si
	mov cx, -1
.loop:
	inc cx
	lodsb
	cmp al, 32
	je .end
	cmp al, 9
	je .end
	call iseol?
	jne .loop
.end:
	mov byte [si - 1], 0	; terminate
	call skipwh0
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	mov word [if_exists_then_address], si
%endif
	call chkeol_or_then
	jcxz .empty
	inc cx			; space for zero terminator
	inc cx			; round up
	and cl, ~1		; align to word
	mov bp, sp
	sub sp, cx		; allocate stack space
	mov si, bx		; -> label in line_in
	mov di, sp		; -> buffer on stack
	push di
@@:
	lodsb
	call uppercase		; normalise the name in buffer
	stosb
	loop @B			; copy all or all+1

	call yy_reset_buf_and_seek_start

.next_line:
	setopt [internalflags3], dif3_quiet_input_single | dif3_return_eof
	xor cx, cx
	call getline.use_dif3_flags
	jc .notfound
	mov bx, si
	cmp al, ':'
	jne .next_line
	call skipwhite
	pop dx
	push dx
	dec si
	call isstring?
	jne .next_line
	lea si, [bx - 1]
	lodsb
	mov sp, bp
	call resetrc
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_found_open
%endif
	pop dx			; return address to cmd3
	jmp cmd3_notblank

.notfound:
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz @F
%endif
	mov ax, 0301h
	call setrc
	mov dx, msg.goto_not_found.1
	call putsz_error
	pop dx
	call putsz_error
	mov dx, msg.goto_not_found.2
	call putsz_error
	mov sp, bp
	jmp getline_close_file

%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
@@:
	mov sp, bp
	call getline_close_file
	jmp if_exists_not_found
%endif

.empty:
	mov ax, 0302h
	call setrc
	mov dx, msg.goto_empty
	jmp .error


resetrc:
	push word [priorrc]
	pop word [rc]
	retn


%include "serialp.asm"
