
%if 0

lDebug E command (enter into memory)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

..@ee_access_start:

%if 0

ee 0:
	push ss
	pop ds
	push ss
	pop es
	mov ax, word [ savesp ]
	inc ax
	inc ax
	mov sp, ax		; restore stack
	mov bx, word [e_addr + saSegSel]
	_386_PM_o32
	mov dx, word [ e_addr ]	; get back address


		; Prompt mode.
ee 1:
	mov word [ errret ], ee 0

		; Begin loop over lines.
ee 2:				; <--- next line
	mov word [e_addr + saSegSel], bx
%if _PM
	call ispm
	jnz .86m
.pm:
	mov word [e_addr + saSelector], bx
	jmp @F
.86m:
	mov word [e_addr + saSegment], bx
@@:
%endif
	_386_PM_o32
	mov word [ e_addr ], dx	; save address
	mov di, line_out
	mov ax, bx		; print out segment and offset
	call hexword

===


	mov al, '.'
	stosb
	call getline00		; read input line
	call iseol?
	je .end
%if _PM
	xor bx, bx
%endif
	mov dx, 1
	call ee_checkplusminus
	jne .notplusminus
	cmp al, '+'
	je ee 3
	jmp short ee 2

.notplusminus:

===


		; INP:	al = character, si-> line
		;	bx:dx = increment to add/subtract if this is an add/sub request
		; OUT:	al, si unchanged
		;	NZ if no add/sub request
		;	ZR if add/sub request,
		;	 [ e_addr ] offset adjusted
ee_checkplusminus:
	cmp al, '-'
	jne .not
	cmp al, '+'
	jne .not
	push si
	push ax
	call skipwhite
	call iseol?
	pop ax
	pop si
	jne .not
	cmp al, '-'
	je .minus
	add word [ e_addr ], dx
_386_PM	adc word [ e_addr+2 ], bx
	jmp short .done

.minus:
	sub word [ e_addr ], dx
_386_PM	sbb word [ e_addr+2 ], bx
.done:
	cmp al, al
.not:
	retn

===


ee 9:
	call getline00

%endif


		; E command - edit memory.
ee:
	call prephack
	call iseol?
	jne @F
	mov bx, word [e_addr + saSegSel]
	_386_PM_o32
	mov dx, word [e_addr]
	jmp ee1

@@:
	mov bx, word [reg_ds]
	call getaddrX		; get address into bx:(e)dx, no scratchsel
	call skipcomm0
	call iseol?
	je ee1			; if prompt mode

eeparsestr:
	push bx
%if _PM
	call verifysegm_or_error; get scratchsel if code selector
%endif
	_386_PM_o32	; push edx
	push dx			; save destination offset
	call getstr		; get data bytes
	mov cx, di
	mov dx, line_out
	sub cx, dx		; length of byte string
	_386_PM_o32	; pop edi
	pop di
	jz ee2_empty		; if length == 0 -->
_386_PM	movzx ecx, cx
	_386_PM_o32	; mov eax, ecx
	mov ax, cx
	dec ax			; (cannot be 0)
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jz .16			; no -->
	_386_PM_o32	; add eax, edi
.16:
	add ax, di
	jc short errorj4	; if it wraps around
	call dohack
	mov si, dx
	mov es, bx
%if _PM
	cmp byte [bAddr32], 0
	jz ee_2
[cpu 386]
	movzx esi, si		; ds:esi -> source
	a32		; a32 rep movsb
__CPU__
ee_2:
%endif
	rep movsb
ee2_empty:
	pop bx
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jnz .32			; yes -->
_386_PM	movzx edi, di		; limit to 16 bits
.32:
	mov word [e_addr + saSegSel], bx
	_386_PM_o32
	mov word [e_addr], di

		; Restore ds + es and undo the interrupt vector hack.
		; This code is also used by the `m' command.
ee0a:
	push ss			; restore ds
	pop ds
	push ss			; restore es
	pop es
	mov di, run2324		; debuggee's int 23/24 values
	call prehak1		; copy things back
	jmp unhack


..@ee_interactive_access_start:

		; Prompt mode.
ee1:
	call guard_re
		; Begin loop over lines.
ee2:				; <--- next line
	mov ax, bx		; print out segment and offset
	call hexword		;  (uses original selector, not scratchsel)
	mov al, ':'
	stosb
	_386_PM_o32		; mov eax, edx
	mov ax, dx
%if _PM
	call test_high_limit	; 32-bit segment ?
	jz .16			; no -->
	call hexword_high
.16:
%endif
	call hexword

%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz ee3
	mov al, 32
	stosb			; only one blank byte here
	jmp @F
%endif

		; Begin loop over bytes.
ee3:				; <--- next byte
	mov ax, 32<<8|32	; print old value of byte
	stosw
@@:
	call dohack		; do the INT pointer hack
	call readmem		; read mem at BX:(E)DX
	call unhack		; undo the INT pointer hack
	mov word [e_addr + saSegSel], bx
	_386_PM_o32
	mov word [e_addr], dx
	call hexbyte
	mov al, '.'
	stosb
	mov byte [ linecounter ], 0	; reset counter
	clropt [internalflags], promptwaiting
	push bx
	push dx
	call putsline
	pop dx
	pop bx
	mov si, line_out+16	; address of buffer for characters
	xor cx, cx		; number of characters so far

ee4_next:
	call getline_is_input_file?
	jc ee9_getc_tty		; if it's a TTY

ee_getc_file:
	setopt [internalflags2], dif2_did_getline_file
				; set this flag so yy_reset_buf knows
				;  that we may have buffered the file

	push si
%if _NEWFULLHANDLING
	mov di, line_in+3       ; read max
%else
	mov di, line_in+2
%endif
	mov si, word [bufnext]
ee5:
	cmp si, word [bufend]
	jb ee6			; if there's a character already
	call fillbuf
	mov al, 13
	jc ee8			; if eof
ee6:
	cmp byte [notatty], 13
	jne ee7			; if no need to compress CR/LF
	cmp byte [si], 10
	jne ee7			; if not a line feed
	inc si			; skip it
	inc byte [notatty]	; avoid repeating this
	jmp ee5			; next character

ee7:
	lodsb			; get the character
	mov byte [notatty], al
ee8:
	mov word [bufnext], si
	pop si
	jmp ee10_got_codepoint

ee9_getc_tty:
	call getc		; character input without echo
ee10_got_codepoint:
	cmp al, 32		; (go to next byte)
	je ee13_write
	cmp al, '-'		; (go to prior byte)
	je ee13_write
	cmp al, '.'		; (exit E interactive mode)
	je ee13_write
	cmp al, 10
	je ee13_write
	call iseol?.notsemicolon; (also exit E interactive mode)
	je ee13_write		; all: done with this byte -->
	cmp al, 8
	je ee11_backspace	; if backspace -->
	cmp al, 7Fh
	je ee11_backspace	; if DEL (handle like backspace) -->
	cmp cx, byte 2		; otherwise, it should be a hex character
	jae ee4_next		; if we have a full byte already
	mov byte [si], al
	call getnyb
	jc ee4_next		; if it's not a hex character
	inc cx
	lodsb			; get the character back
	jmp ee12_put_then_next

ee112_priorbyte:
	call putc		; display the minus
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jz .16			; no -->
	_386_PM_o32
.16:
	dec dx			; decrement offset (16 bit or 32 bit)
	mov di, line_out
	jmp ee15_linebreak_and_ee2

ee11_backspace:
	jcxz ee4_next		; if nothing to backspace over
	dec cx
	dec si
	call fullbsout
	jmp ee4_next

ee12_put_then_next:
	call putc
	jmp ee4_next		; back for more

		; We have a byte (if CX != 0).
		;
		; cx = number of digits we have (0..2)
		; al = codepoint specifying how to proceed after writing
ee13_write:
	jcxz ee14_done_write	; if no change for this byte

	push ax			; preserve proceed control
	xor ax, ax		; ah = 0, al = 0
	mov byte [si], al	; terminate the string
	sub si, cx		; point to beginning
@@:
	add ah, ah
	add ah, ah
	add ah, ah
	add ah, ah		; prior value times 16
	add ah, al		; add next digit (0 in first iteration)
	lodsb			; load next digit (or NUL)
	call getnyb
	jnc @B			; if another digit --> (NC)
				; (CY can only mean we reached the NUL)
	mov al, ah		; get byte value
	call dohack		; do the INT pointer hack
	call writemem		; write AL at BX:(E)DX (may use scratchsel)
	mov di, run2324		; debuggee's int 23/24
	call prehak1		; copy things back
	call unhack		; undo the INT pointer hack
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jz .16			; no -->
	_386_PM_o32
.16:
	inc word [e_addr]
	pop ax			; al = how to proceed

		; End the loop over bytes.
ee14_done_write:
	mov di, line_out	; reset output buffer

	cmp al, 32		; (go to next byte)
	je ee_nextbyte
	cmp al, '-'		; (go to prior byte)
	je ee112_priorbyte
	cmp al, '.'		; (exit E interactive mode)
	je ee16_end
	cmp al, 10
	je ee16_end
	call iseol?.notsemicolon; (also exit E interactive mode)
	je ee16_end
	jmp error		; unexpected value

ee_nextbyte:
_386_PM	call test_high_limit	; 32-bit segment ?
_386_PM	jz .16			; no -->
	_386_PM_o32
.16:
	inc dx			; increment offset (16 bit or 32 bit)
%if _40COLUMNS
	testopt [options6], opt6_40_columns
	jz @F
	test dl, 3
	jz ee15_linebreak_and_ee2
@@:
%endif
	test dl, 7
	jz ee15_linebreak_and_ee2
				; if new line
	not cx
	add cx, byte 4		; compute 3 - cx
	mov al, 32
	rep stosb		; store that many spaces
	jmp ee3			; back for more

ee15_linebreak_and_ee2:
	mov ax, 10 << 8 | 13	; terminate this line
	stosw
	jmp ee2			; back for a new line

ee16_end:
	jmp putsline_crlf	; call putsline and return

..@ee_interactive_access_end:

..@ee_access_end:
