
%if 0

lDebug S commands (search, sleep)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

sleepcmd:
	call skipcomma
	call getdword
	push bx
	push dx
	call skipwh0
	call iseol?
	je .seconds
	dec si
	mov dx, msg.seconds
	call isstring?
	je .seconds_check_eol
	mov dx, msg.ticks
	call isstring?
.error_NZ:
	jne error
.ticks_check_eol:
	lodsb
	call chkeol

	mov ax, 1
	jmp .common

.seconds_check_eol:
	lodsb
	call chkeol
.seconds:
	mov ax, 18
.common:
	pop dx
	push ax
	mul dx			; dx:ax = low word times multiplier
	mov bx, dx
	mov cx, ax		; bx:cx = low word times multiplier
	pop ax
	pop dx
	mul dx			; dx:ax = high word times multiplier
	add bx, ax
	adc dx, 0		; dx:bx:cx = entire result
	jnz .error_NZ

	test cx, cx
	jnz @F
	test bx, bx
	jz .end
@@:

	mov ax, 40h		; bimodal segment/selector
	mov es, ax
.loop_reset:
	mov dx, word [es:6Ch]
.loop:
	cmp dx, word [es:6Ch]
	jne .next
	call handle_serial_flags_ctrl_c
	testopt [options3], opt3_check_ctrlc_0bh
	jnz @F			; already called function 0Bh -->
	call InDos
	jnz @F
	mov ah, 0Bh
	doscall			; allow to break with Ctrl-C
@@:
%if _SLEEP_NEW
	mov di, word [sleep_repeat_idle]
	inc di
@@:
	call idle
	dec di
	jnz @B
%else
	call idle
%endif
	jmp .loop

.next:
%if _SLEEP_NEW
	neg dx			; minus prior tick
	add dx, word [es:6Ch]	; new tick - prior tick

	cmp dx, word [sleep_delta_limit]
	jbe @F
	mov dx, word [sleep_delta_limit]
	test dx, dx
	jnz @F
	inc dx			; limit 0 would lead to stagnant sleep
@@:
	cmp dx, word [sleep_highest_delta]
	jbe @F
	mov word [sleep_highest_delta], dx
@@:
	sub cx, dx
	sbb bx, 0
	jc .end
%else
	sub cx, 1
	sbb bx, 0
%endif
	jnz .loop_reset
	jcxz .end
	jmp .loop_reset

.end:
	retn


		; S command - search for a string of bytes.
sss:
	dec si
	dec si			; -> at 'S'
	mov dx, msg.sleep
	call isstring?		; check for "SLEEP"
	je sleepcmd
	inc si			; skip 'S'
	lodsb			; load next

	clropt [internalflags3], dif3_sss_is_reverse

	mov bx, word [reg_ds]	; get search range
	_386_PM_o32		; xor ecx, ecx
	xor cx, cx
	call getrangeX		; get address range into BX:(E)DX..BX:(E)CX
	call skipcomm0
	_386_PM_o32		; push edx
	push dx
	_386_PM_o32		; push ecx
	push cx
	push bx

	mov word [sss_silent_count_used], 0

	mov dx, msg.reverse
	dec si
	call isstring?
	jne @F

	setopt [internalflags3], dif3_sss_is_reverse
	call skipwhite
	dec si
@@:

	mov dx, msg.silent
	call isstring?
	jne @F
	call skipequals
	call getdword
	mov word [sss_silent_count], dx
	mov word [sss_silent_count + 2], bx
	not byte [sss_silent_count_used]
	dec si
@@:

	mov dx, msg.range
	call isstring?
	lodsb
	jne .notrange

	mov bx, word [reg_ds]	; get search range
	xor cx, cx
	call getrangeX		; try to get second range
	push si
	call chkeol		; and insure end-of-line
				; successful if it returned
	_386_PM_o32		; mov esi, edx
	mov si, dx		; bx:esi-> source string
	_386_PM_o32		; sub ecx, edx
	sub cx, dx		; ecx = count - 1
	jmp short .setesedi

.notrange:
	call getstr		; get string of bytes
	push si
	sub di, line_out	; di = number of bytes to look for
	jz error
	mov cx, di
	dec di			;     minus one
	mov si, line_out
	push di
	call guard_auxbuff
	mov es, word [auxbuff_segorsel]
	xor di, di
	rep movsb		; move to auxbuff
	_386_PM_o32	; xor esi, esi
	xor si, si
	mov bx, es		; bx:esi -> auxbuff
	pop cx
_386_PM	movzx ecx, cx		; ecx = count - 1
.setesedi:
	push ss
	pop es
	mov di, search_results
	xor ax, ax
	mov word [sscounter], ax
	mov word [sscounter + 2], ax
	push cx
%if _PM
	mov cx, (6 * 16) >> 1
%else
	mov cx, (4 * 16) >> 1
%endif
	rep stosw
	pop cx

	call prephack		; set up for the interrupt vector hack
	call dohack
	mov ds, bx
	pop di			; original si
	pop es
	_386_PM_jmpn .386init	; 386 -->
.init:
	pop bx
	pop dx
.init_popped:
	sub bx, dx		; bx = number of bytes in search range minus one
	sub bx, cx		; = number of possible positions of string minus 1
	jb .error_unhack_di
	mov di, dx
	mov dx, cx
	mov cx, bx

		; ds:si-> search string, length (dx+1)
		; es:di-> data to search in, (cx+1) bytes
	testopt [ss:internalflags3], dif3_sss_is_reverse
	jnz .reverse
.loop:
	or al, 1		; NZ (iff cx==0, repne scasb doesn't change ZF)
	push si
	lodsb			; first character in al
	repne scasb		; look for first byte
	je .foundbyte
	scasb			; count in cx was cnt-1
	jne .done
.found_last_byte:
	call .handle_found_byte
	jmp .done

.foundbyte:
	call .handle_found_byte
	pop si
	jmp .loop		; cx = 0 if one to search,
				;  cx = 1 if two to search, etc

.reverse:
	add di, cx		; -> last position to check
.reverseloop:
	or al, 1		; NZ (iff cx==0, repne scasb doesn't change ZF)
	push si
	lodsb			; first character in al
	std			; no AMD erratum workaround needed
	repne scasb		; look for first byte
	je .reversefoundbyte
	scasb			; count in cx was cnt-1
	jne .done
.reversefound_last_byte:
	cld
	add di, 2
	cmp al, al		; ZR for case if dx = 0
	call .handle_found_byte
	sub di, 2
	jmp .done

.reversefoundbyte:
	cld
	add di, 2
	cmp al, al		; ZR for case if dx = 0
	call .handle_found_byte
	sub di, 2
	pop si
	jmp .reverseloop	; cx = 0 if one to search,
				;  cx = 1 if two to search, etc

.done:
	pop si			; discard
.commondone:
	cld
	push ss
	pop ds
	call unhack
	mov di, line_out

	mov ax, word [sscounter + 2]
	test ax, ax
	jz .nohighcounter
	call hexword
.nohighcounter:

	mov ax, word [sscounter]
	call hexword
	call putsline
	mov dx, msg.matches
	jmp putsz


		; INP:	ZR
.handle_found_byte:
	push cx
	push di
	mov cx, dx
	repe cmpsb		; compare string behind first byte
		; If we're searching for a single-byte value then
		;  dx is equal to zero here. In that case cx gets
		;  the value zero and then repe cmpsb does not
		;  alter ZF, meaning it will stay ZR (as noted for
		;  the comment INP section).
	pop di
	je .display		; if equal
.next:
	pop cx
	retn

.display:
	mov bx, es
	push di
	push ds
	push es
	 push ss
	 pop ds
	call unhack		; undo the interrupt vector hack and restore es
	push di
	cmp word [sscounter + 2], 0
	jne @F
	mov di, word [sscounter]
	cmp di, 16
	jae @F
%if _PM
	add di, di		; * 2
	mov ax, di
	add di, di		; * 4
	add di, ax		; * 4 + * 2 = * 6
%else
	add di, di
	add di, di		; * 4
%endif
	add di, search_results
	pop ax
	push ax
	dec ax
	stosw
%if _PM
	xor ax, ax
	stosw
%endif
	mov ax, bx
	stosw

@@:
	add word [sscounter], 1
	adc word [sscounter + 2], 0
	rol byte [sss_silent_count_used], 1
	jnc @F
	mov ax, word [sss_silent_count]
	or ax, word [sss_silent_count + 2]
	pop ax
	push dx
	jz .nodisplay
	pop dx
	push ax
	sub word [sss_silent_count], 1
	sbb word [sss_silent_count + 2], 0
@@:
	mov ax, bx
	mov di, line_out
	call hexword		; 4 (segment)
	mov al, ':'
	stosb			; +1=5
	pop ax
	dec ax
	call hexword
%if _SDUMP
	testopt [options], ss_no_dump
	jnz @F
	stc
	adc ax, dx		; -> behind result
	jbe .noresult		; end of segment
	mov si, ax
	mov ax, 32<<8|32
	stosw
	lea bx, [di+3*16]
	mov cx, si
	neg cx
	cmp cx, byte 16
	jbe .cxdone
	mov cx, 16
.cxdone:
	 pop ds
	 push ds		; restore search's segment
	push cx
.disploop:
	lodsb
	call dd_store
	mov al, 32
	stosb
	loop .disploop
	pop cx
	 push ss
	 pop ds
	neg cx
	add cx, byte 16
	jz .noblanks
.loopblanks:
	mov ax, 32<<8|32
	stosw
	stosb
	loop .loopblanks
.noblanks:
	mov byte [di-(1+(8*3))], '-'
	mov di, bx
.noresult:
@@:
%endif	; _SDUMP
	push dx
	call putsline_crlf
.nodisplay:
	call dohack
	pop dx
	pop es
	pop ds
	pop di
	jmp .next


.error_unhack_di:
	push ss
	pop ds
	call unhack
	mov si, di
	jmp error


%if _PM
	subcpu 386

.386init:
	pop ebx
	pop edx
	call ispm
	jnz .init_popped	; not PM -->
	sub ebx, edx		; ebx = number of bytes in search range minus one
	sub ebx, ecx		; = number of possible positions of string minus 1
	jb .error_unhack_di
	mov edi, edx
	mov edx, ecx
	mov ecx, ebx

		; ds:esi-> search string, length (edx+1)
		; es:edi-> data to search in, (ecx+1) bytes
		; Although 386+ RM still uses 64 KiB segments, it allows
		; us to use the 32-bit addressing variant of the string
		; instructions as long as we never access any byte above
		; the 64 KiB limit. (Even if the index register contains
		; 00010000h after an instruction executed.)
	testopt [ss:internalflags3], dif3_sss_is_reverse
	jnz .386reverse
.386loop:
	or al, 1		; NZ (iff cx==0, repne scasb doesn't change ZF)
	push esi
	a32 lodsb		; first character in al
	a32 repne scasb		; look for first byte
	je .386foundbyte
	a32 scasb		; count in ecx was cnt-1
	jne .386done
.386found_last_byte:
	call .386handle_found_byte
	jmp .386done

.386foundbyte:
	call .386handle_found_byte
	pop esi
	jmp .386loop		; ecx = 0 if one to search,
				;  ecx = 1 if two to search, etc

.386reverse:
	add edi, ecx		; -> last position to check
.386reverseloop:
	or al, 1		; NZ (iff cx==0, repne scasb doesn't change ZF)
	push esi
	a32 lodsb		; first character in al
	std			; no AMD erratum workaround needed
	a32 repne scasb		; look for first byte
	je .386reversefoundbyte
	a32 scasb		; count in ecx was cnt-1
	jne .386done
.386reversefound_last_byte:
	cld
	add edi, 2
	cmp al, al		; ZR for case if edx = 0
	call .386handle_found_byte
	sub edi, 2
	jmp .386done

.386reversefoundbyte:
	cld
	add edi, 2
	cmp al, al		; ZR for case if edx = 0
	call .386handle_found_byte
	sub edi, 2
	pop esi
	jmp .386reverseloop	; ecx = 0 if one to search,
				;  ecx = 1 if two to search, etc

.386done:
	pop esi			; discard
	jmp .commondone


		; INP:	ZR
.386handle_found_byte:
	push ecx
	push edi
	mov ecx, edx
	a32 repe cmpsb		; compare string behind first byte
		; If we're searching for a single-byte value then
		;  edx is equal to zero here. In that case ecx gets
		;  the value zero and then a32 repe cmpsb does not
		;  alter ZF, meaning it will stay ZR (as noted for
		;  the comment INP section).
	pop edi
	je .386display		; if equal
.386next:
	pop ecx
	retn

.386display:
	mov bx, es
	push edi
	push ds
	push es
	 push ss
	 pop ds
	call unhack		; undo the interrupt vector hack and restore es
	push edi
	mov edi, dword [sscounter]
	cmp edi, 16
	jae @F
	add di, di		; * 2
	mov ax, di
	add di, di		; * 4
	add di, ax		; * 4 + * 2 = * 6
	add di, search_results
	pop eax
	push eax
	dec eax
	stosd
	mov ax, bx
	stosw

@@:
	inc dword [sscounter]
	rol byte [sss_silent_count_used], 1
	jnc @F
	cmp dword [sss_silent_count], 0
	pop eax
	push dx
	je .386nodisplay
	pop dx
	push eax
	sub word [sss_silent_count], 1
	sbb word [sss_silent_count + 2], 0
@@:
	mov ax, bx
	mov di, line_out
	call hexword		; 4 (segment)
	mov al, ':'
	stosb			; +1=5
	pop eax
	dec eax
	call test_high_limit
	jz .noa32
	call hexword_high
.noa32:
	call hexword
%if _SDUMP
	testopt [options], ss_no_dump
	jnz @F
	stc
	adc eax, edx		; -> behind result
	jbe .386noresult	; end of segment
	mov esi, eax
	mov ax, 32<<8|32
	stosw
	lea bx, [di+3*16]
	mov ecx, esi
	neg ecx
	cmp ecx, byte 16
	jbe .386cxdone
	mov cx, 16
.386cxdone:
	 pop ds
	 push ds		; restore search's segment
	push cx
.386disploop:
	a32 lodsb
	call dd_store
	mov al, 32
	stosb
	loop .386disploop
	pop cx
	 push ss
	 pop ds
	neg cx
	add cx, byte 16
	jz .386noblanks
.386loopblanks:
	mov ax, 32<<8|32
	stosw
	stosb
	loop .386loopblanks
.386noblanks:
	mov byte [di-(1+(8*3))], '-'
	mov di, bx
.386noresult:
@@:
%endif	; _SDUMP
	push dx
	call putsline_crlf
.386nodisplay:
	call dohack
	pop dx
	pop es
	pop ds
	pop edi
	jmp .386next

subcpureset
%endif	; _PM
