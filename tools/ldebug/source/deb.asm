
%if 0

Debugging output functions independent of DOS
Public Domain, 2008-2012 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

%if 0

Only the registers noted here are changed or returned, including flags.

d4dumpregs	displays numeric value of all 16-bit registers.
d4message	displays ASCIZ message behind the call. (Returns behind message.)
d5dumpregs	displays numeric value of all 16-bit registers.
d5message	displays ASCIZ message behind the call. (Returns behind message.)
d4disp_al	displays character in al.
d4disp_al_dec	displays al as decimal number, without leading zeros.
d4disp_al_hex	displays al as hexadecimal number, with two digits.
d4disp_ax_hex	displays ax as hexadecimal number, with four digits.
d4uppercase	uppercases character in al.
d4disp_stack_hex displays the word on the stack as hexadecimal number (and pops it).
d4disp_msg	displays ASCIZ string pointed to by ds:si.
d4getc		returns ASCII character in al, scancode in ah. (al might be zero.)
d4pauseforkey	waits until a key is pressed. (The key-press is discarded.)

This was all thrown together really quick without more thought to it.

%endif

	numdef DEBUG_TO_SERIAL, 1
%ifidni _DEB_ASM_PREFIX, init_
	overridedef DEBUG_TO_SERIAL, 0
%endif


%[_DEB_ASM_PREFIX]d5dumpregs:
%[_DEB_ASM_PREFIX]d4dumpregs:
	pushf
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	mov ax, sp
	add ax, byte 18
	push ax
	push cs
	push ss
	push ds
	push es

	mov bx, sp
	mov ax, 1<<8|7
	push ax
	mov ax, 14
	push cs
	pop ds
	mov dx, .regnames

	mov bp, sp
%assign _columns	0
%assign _blanks		1

	xor cx, cx

.looprows:
	push ax
	mov al, 13
	call %[_DEB_ASM_PREFIX]d4disp_al
	mov al, 10
	call %[_DEB_ASM_PREFIX]d4disp_al
%if 0
	push cx
	push dx
	xor cx, cx
	mov cl, byte [ _blanks + bp ]
	jcxz .doneblanks
	mov al, 32
.loopblanks:
	call %[_DEB_ASM_PREFIX]d4disp_al
	loop .loopblanks
.doneblanks:
	pop dx
	pop cx
%endif
	pop ax

	mov cl, byte [ _columns + bp ]
.loopcolumns:
	call %[_DEB_ASM_PREFIX]d4disp_reg
	add dx, byte 2
	add bx, byte 2
	dec ax
	jz .done
	loop .loopcolumns
	jmp short .looprows

.done:
	pop ax

	pop es
	pop ds
	pop ax
	pop ax
	pop ax
	pop bp
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	popf
	retn

.regnames:
	db "esdssscsspbpdisidxcxbxaxflip"


	; ss:bx-> word value
	; ds:dx-> 2-byte message
%[_DEB_ASM_PREFIX]d4disp_reg:
	push ax
	xchg bx, dx
	mov ax, [ bx ]				; get 2 bytes at [ ds:dx ]
	xchg bx, dx
	call %[_DEB_ASM_PREFIX]d4disp_al
	xchg al, ah
	call %[_DEB_ASM_PREFIX]d4disp_al

	mov al, '='
	call %[_DEB_ASM_PREFIX]d4disp_al

	mov ax, [ ss:bx ]
	call %[_DEB_ASM_PREFIX]d4disp_ax_hex

	mov al, 32
	call %[_DEB_ASM_PREFIX]d4disp_al
	pop ax
	retn


%if 0
%[_DEB_ASM_PREFIX]d4pocketdosmemdump:
	pushf
	push es
	push ds
	push si
	push di
	push bx
	push ax
	push cx

	cld
	push cs
	pop es
	mov di, .pocketdosmemfnfull
	mov cx, -1
	mov al, ':'
.colonloop:
	repne scasb
	cmp di, .dummycolon+1
	je .colondone
	cmp byte [es:di], '\'
	je .colonloop
	mov byte [es:di-1], '.'
	jmp short .colonloop
.colondone:

		; A simple check whether we're running in PocketDOS.
	xor ax, ax
	mov ds, ax
	mov ax, word [ 0F8h*4 ]
	inc ax				; 0FFFFh ?
	jz .notfound			; not PocketDOS -->
	dec ax				; 0 ?
	jz .notfound			; not PocketDOS -->

	push cs
	pop es
	mov bx, .pocketdosmemdll	; es:bx-> ASCIZ DLL file name
	xor ax, ax
	int 0F8h			; IntF8.00 initialize

	test al, al
	jz .notfound			; --> 00h, not found
	cmp al, -1
	je .notfound			; --> FFh, too many DLLs loaded

	push cs
	pop ds
	mov si, .pocketdosmemfnfull	; ds:si-> FN
	xor bx, bx			; bx = number of paragraphs, 0 = 10000h
	mov es, bx			; es:0-> start of dump
	mov ah, 02h
	push ax
	int 0F8h			; IntF8.02 call DLL
	pop ax				; preserve handle in al

	mov si, .pocketdosmemfninit0	; ds:si-> FN
	mov bx, resimagepsp_size_p
	push cs
	pop es				; es:0-> start of dump
	mov ah, 02h
	push ax
	int 0F8h			; IntF8.02 call DLL
	pop ax				; preserve handle in al

	mov ah, 01h
	int 0F8h			; IntF8.01 close

	call %[_DEB_ASM_PREFIX]d4message
	asciz 13,10,"Memory dumps created."

.notfound:
	pop cx
	pop ax
	pop bx
	pop di
	pop si
	pop ds
	pop es
	popf
	retn

.pocketdosmemdll:	asciz "PDOSMEM.DLL"
.pocketdosmemfnfull:	asciz "D:\LITEST\init-",__DATE__,"T",__TIME__,"-full.bin"
.pocketdosmemfninit0:	asciz "D:\LITEST\init-",__DATE__,"T",__TIME__,"-ldebug.bin"
.dummycolon:		db ":"
%endif


%[_DEB_ASM_PREFIX]d4disp_stack_hex:
	push ax
	push bp
	mov bp, sp
	mov ax, [bp+6]
	pop bp
	call %[_DEB_ASM_PREFIX]d4disp_ax_hex
	pop ax
	retn 2


..@symhint_skip_caller_asciz_ %+ %[_DEB_ASM_PREFIX] %+ d4message:
%[_DEB_ASM_PREFIX]d5message:
%[_DEB_ASM_PREFIX]d4message:
	push ax
	push bx
	push si
	push bp
	mov bp, sp
	mov si, [bp+8]
	pushf
	cmp byte [cs:si], 0CCh
	jne @FF
	int3
	cmp byte [cs:si], 0CCh
	jne @FF
	mov si, .msg_error
	call %[_DEB_ASM_PREFIX]init0display
@@:
	sti
	hlt
	int3
	jmp @B

@@:
	call %[_DEB_ASM_PREFIX]init0display
	popf
	mov word [bp+8], si
	pop bp
	pop si
	pop bx
	pop ax
	retn

.msg_error:
	asciz "Error in d4message, detected breakpoint in message.",13,10


%[_DEB_ASM_PREFIX]d4disp_al:
	push ax
	push bx
	pushf
	call %[_DEB_ASM_PREFIX]init0int10tty
	popf
	pop bx
	pop ax
	retn


%[_DEB_ASM_PREFIX]d4disp_al_dec:
	pushf
	push ax
	push cx
	mov cx, 100
	call .div
	mov cl, 10
	call .div
	add al, '0'
	call %[_DEB_ASM_PREFIX]d4disp_al
	pop cx
	pop ax
	popf
	retn

.div:
	xor ah, ah
	div cl
	or ch, al
	jz .leadingzero
	add al, '0'
	call %[_DEB_ASM_PREFIX]d4disp_al
.leadingzero:
	xchg al, ah
	retn


%[_DEB_ASM_PREFIX]d4uppercase:
	pushf
	cmp al, 'a'
	jb .return
	cmp al, 'z'
	ja .return
	and al, ~ 20h
.return:
	popf
	retn


%[_DEB_ASM_PREFIX]d4disp_ax_hex:		; ax
		xchg al,ah
		call %[_DEB_ASM_PREFIX]d4disp_al_hex
						; display former ah
		xchg al,ah			;  and fall trough for al
%[_DEB_ASM_PREFIX]d4disp_al_hex:		; al
		push cx
		pushf
		mov cl,4
		ror al,cl
		call %[_DEB_ASM_PREFIX]d4disp_al_lownibble_hex
						; display former high-nibble
		rol al,cl
		popf
		pop cx
						;  and fall trough for low-nibble
%[_DEB_ASM_PREFIX]d4disp_al_lownibble_hex:
		pushf
		push ax				; save ax for call return
		and al, 00001111b		; high nibble must be zero
		add al, '0'			; if number is 0-9, now it's the correct character
		cmp al, '9'
		jna .decimalnum			; if we get decimal number with this, ok -->
		add al, 7			;  otherwise, add 7 and we are inside our alphabet
 .decimalnum:
		call %[_DEB_ASM_PREFIX]d4disp_al
		pop ax
		popf
		retn


%[_DEB_ASM_PREFIX]d4disp_msg:
	pushf
	push si
	push ax
	mov si, dx
	cld
.loop:
	lodsb
	test al, al
	jz .return
	call %[_DEB_ASM_PREFIX]d4disp_al
	jmp short .loop

.return:
	pop ax
	pop si
	popf
	retn


		; Display character with Int10
		;
		; INP:	al = character
		; OUT:	-
		; CHG:	ax, bx
		; STT:	-
%[_DEB_ASM_PREFIX]init0int10tty:
%if _DEBUG_TO_SERIAL
	testopt [ss:options], enable_serial
	jz @F
	push ds
	push es
	 push ss
	 pop ds
	 push ss
	 pop es
	call serial_send_char
	pop es
	pop ds
	retn

@@:
%endif
	push ax
	mov ah, 0Fh
	int 10h					; get video mode and active page
	pop ax
	mov ah, 0Eh
	mov bl, 07h
	push bp
	int 10h					; write character
	pop bp
	retn

		; Display message with Int10
		;
		; INP:	cs:si-> ASCIZ message
		; OUT:	-
		; CHG:	ax, si, bx
		; STT:	UP
%[_DEB_ASM_PREFIX]init0display.display:
	call %[_DEB_ASM_PREFIX]init0int10tty
%[_DEB_ASM_PREFIX]init0display:
	cld
	cs lodsb				; get character
	test al, al				; zero ?
	jnz .display				; no, display and get next character -->
%[_DEB_ASM_PREFIX]init0flush.retn:
	retn


%[_DEB_ASM_PREFIX]d4getc:
	pushf
%if _DEBUG_TO_SERIAL
	push ds
	push es
	 push ss
	 pop ds
	 push ss
	 pop es
	call getc.rawnext
	pop es
	pop ds
%else
	xor ax, ax
	int 16h
%endif
	popf
	retn

%[_DEB_ASM_PREFIX]d4pauseforkey:
	push ax
	call %[_DEB_ASM_PREFIX]d4getc
d4	call %[_DEB_ASM_PREFIX]d4message
d4	asciz "d4pauseforkey received ax="
d4	call %[_DEB_ASM_PREFIX]d4disp_ax_hex
d4	call %[_DEB_ASM_PREFIX]d4message
d4	asciz 13,10
	pop ax
	retn


%ifidni _DEB_ASM_PREFIX, init_
	resetdef DEBUG_TO_SERIAL
%endif
