
%if 0

lDebug B commands (permanent breakpoints, break upwards)

Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

bb_dispatch:
.scan:
%if _BREAKPOINTS
	db 'P'
	db 'N'
	db 'C'
	db 'D'
	db 'E'
	db 'T'
	db 'L'
	db 'I'
	db 'W'
 %if BPSIZE == 6 || BPSIZE == 9
	db 'O'
 %endif
	db 'S'
%endif
	db 'U'		; BU command
	db 0		; placeholder, never matches
.scanamount: equ $ - .scan

	align 2, db 0
.offset:
%if _BREAKPOINTS
	dw point_set
	dw point_number
	dw point_clear
	dw point_disable
	dw point_enable
	dw point_toggle
	dw point_list
	dw point_id
	dw point_when
 %if BPSIZE == 6 || BPSIZE == 9
	dw point_offset
 %endif
	dw point_swap
%endif
	dw bu_breakpoint
	dw error
.offsetamount: equ ($ - .offset) / 2

%if .scanamount != .offsetamount
 %error bb dispatch mismatch
%endif


	usesection lDEBUG_CODE

bb:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz @F
	dec si
	dec si			; -> at 'B'
	mov dx, msg.boot
	call isstring?		; check for "BOOT"
	je bootcmd
	inc si			; skip 'B'
	lodsb			; load next
@@:
%endif
	call uppercase
	mov di, bb_dispatch.scan
	mov cx, bb_dispatch.scanamount
	repne scasb
				; di -> behind the NUL if no valid subcommand
	sub di, bb_dispatch.scan + 1
	shl di, 1
	jmp near word [bb_dispatch.offset + di]


%if _BREAKPOINTS
point_set:
	call skipwhite

	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	mov di, .get_saved	; access saved address later
	call findpointat	; do we find it ?
	jc .find_new		; no, treat as if "NEW" keyword given -->
				; point index is in dx
	push dx
	jmp @FF			; skip check whether used

.not_at:
	mov di, .get_addr	; get address from input command line later
	call getpointindex
	jnc @F			; got an index -->
	jz error		; "ALL" is invalid
				; got "NEW" keyword
.find_new:
	xor cx, cx
	push ax
.new_loop:
	mov ax, cx		; try this index
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz .new_found		; found unused one -->

	inc cx			; = next index
	cmp cx, _NUM_B_BP	; valid ?
	jb .new_loop		; yes, try next -->

	mov dx, msg.bb_no_new
	jmp prnquit


		; INP:	al=, si-> input line
		; OUT:	al=, si-> after
		;	bx:dx = linear adddress
		;	does not return if error
		; CHG:	edxh
.get_addr:
	mov bx, word [reg_cs]
	call getlinearaddr
	jnc .retn
	jmp error


		; INP:	al=, si-> input line
		; OUT:	bx:dx = linear address
		; CHG:	-
.get_saved:
	mov dx, word [..@bb_saved_linear]
	mov bx, word [..@bb_saved_linear + 2]
.retn:
	retn

	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
..@bb_saved_linear:
	dd 0
	usesection lDEBUG_CODE


.new_found:
	pop ax
	push cx
	jmp @FF

@@:
	push dx
	push ax
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	pop ax
	jnz error
@@:
	call di			; call either .get_addr or .get_saved
				; in any case, bx:dx = linear address
	mov di, 8000h		; default counter
	push dx
	xor dx, dx

	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
..@bb_id_start:
	dw 0
..@bb_id_length:
	dw 0
..@bb_when_start:
	dw 0
..@bb_when_length:
	dw 0
	usesection lDEBUG_CODE


	mov word [..@bb_id_length], dx
	mov word [..@bb_when_length], dx
				; initialise to empty ID
	push dx
.loop_additional:
	call skipwh0
	dec si
	mov dx, msg.number
	call isstring?
	je .additional_number
	mov dx, msg.counter
	call isstring?
	je .additional_number
	mov dx, msg.id
	call isstring?
	je .additional_id
	mov dx, msg.when
	call isstring?
	je .additional_when
%if BPSIZE == 6 || BPSIZE == 9
	mov dx, msg.offset
	call isstring?
	je .additional_offset
%endif
	lodsb
	call iseol?
	je .no_additional
	pop dx
	test dx, dx		; already got a number without keyword?
	jnz error		; yes -->
	inc dx			; remember for subsequent iterations
	push dx
	call skipwh0
	jmp @F

.additional_number:
	call skipequals
	call iseol?
	je error
@@:
	call getcounter
	jmp .loop_additional

%if BPSIZE == 6 || BPSIZE == 9
.additional_offset:
	pop dx
	or dl, 1		; remember for subsequent iterations
				;  not to accept number without keyword
	push dx
	call skipequals
%if _PM
	push bx
	call getdword
	mov word [bp_offset], dx
	mov word [bp_offset + 2], bx
	pop bx
%else
	call getword
	mov word [bp_offset], dx
%endif
	jmp .loop_additional
%endif

.additional_when:
	pop dx
	or dl, 1		; remember for subsequent iterations
				;  not to accept number without keyword
	push dx
	call skipequals
	dec si
	call get_when
	jmp .loop_additional

.additional_id:
	call skipequals
	dec si
	call get_id

.no_additional:
	pop dx			; discard non-keyword NUMBER indicator
	pop dx			; restore dx = low word of linear

	xchg bx, dx		; dx:bx = linear
	xchg bx, ax		; dx:ax = linear
	pop bx			; = 0-based point index to set
	push dx
	push ax			; on stack: dword linear

	push di
	xchg dx, bx		; dx = point index

		; As for set_id, set_when will free a prior condition
		;  when writing the new one. However, we check for the
		;  appropriate buffer size being still free before
		;  calling set_when because we want to cancel the point
		;  initialisation if either the ID or the condition do
		;  not fit, without having yet written anything.
	call check_when_space	; CHG ax, bx, cx, si, di

		; Note that point_clear and init both leave the
		;  empty word in the ID array. Therefore we can
		;  always handle this by freeing the prior value
		;  first, which is required if we're resetting
		;  an existing point with BP AT.
		; The set_id function takes care of this.
	call set_id		; CHG ax, bx, cx, si, di

		; After check_when_space then set_id both returned,
		;  we have finally checked all error conditions and
		;  are now actually modifying things.
	call set_when		; CHG ax, bx, cx, si, di
	xchg ax, dx		; ax = point index
	pop di			; preserve counter value

	mov bx, ax
	add bx, bx
	add bx, bx
%if BPSIZE == 4
%elif BPSIZE == 5
	add bx, ax
%elif BPSIZE == 6
	add bx, ax		; * 5
	add bx, ax		; * 6
%elif BPSIZE == 9
	add bx, bx		; * 8
	add bx, ax		; * 9
%else
 %error Unexpected breakpoint size
%endif
	pop word [ b_bplist.bp + bx ]
		; These two instructions need to stay in that order.
		; For the non-PM version, the pop overwrites the byte
		; that is then initialized to 0CCh (the breakpoint
		; content byte).
		; (This is not true for BPSIZE == 6. Instead, the pop
		;  overwrites the first byte of the preferred offset.)
	pop word [ b_bplist.bp + bx + 2 ]
	mov byte [ b_bplist.bp + bx + BPSIZE - 1 ], 0CCh
%if BPSIZE == 6
	push word [bp_offset]
	pop word [ b_bplist.bp + bx + 3 ]
%elif BPSIZE == 9
	push word [bp_offset]
	pop word [ b_bplist.bp + bx + 4 ]
	push word [bp_offset + 2]
	pop word [ b_bplist.bp + bx + 6 ]
%endif
	mov bx, ax
	add bx, bx
	mov word [ b_bplist.counter + bx ], di
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	or byte [b_bplist.used_mask+bx], ah
	not ah
	and byte [b_bplist.disabled_mask+bx], ah
 %if _BREAKPOINTS_STICKY
	and byte [b_bplist.sticky_mask+bx], ah
 %endif
%else
	or byte [b_bplist.used_mask], ah
	not ah
	and byte [b_bplist.disabled_mask], ah
 %if _BREAKPOINTS_STICKY
	and byte [b_bplist.sticky_mask], ah
 %endif
%endif
	retn


		; INP:	si -> first non-blank character
		; OUT:	..@bb_id_start and ..@bb_id_length set
		;	does not return if error (too long)
		; CHG:	ax, cx, si
get_id:
	mov word [..@bb_id_start], si
@@:
	lodsb
	call iseol?.notsemicolon
	jne @B
		; si -> after EOL char
		; si - 1 -> EOL char
@@:
	dec si
	cmp si, word [..@bb_id_start]
	je @F
	cmp byte [si - 1], 32
	je @B
	cmp byte [si - 1], 9
	je @B
@@:
	mov cx, si
	sub cx, word [..@bb_id_start]
	mov word [..@bb_id_length], cx
	cmp cx, 63
	ja error

%if 0
	push dx
	mov dx, msg.id
	call putsz
	mov al, 32
	call putc
	mov al, '"'
	call putc
	mov dx, word [..@bb_id_start]
	mov cx, word [..@bb_id_length]
	call disp_message_length_cx
	mov al, '"'
	call putc
	mov dx, crlf
	call putsz
	pop dx
%endif
	retn


		; INP:	si -> first non-blank character
		; OUT:	..@bb_when_start and ..@bb_when_length set
		;	does not return if error
		;	al = character after the condition, si -> next
		; CHG:	ax, cx, si
get_when:
	mov word [..@bb_when_start], si
@@:
	lodsb
	push dx
	push bx
	call getexpression
	pop bx
	pop dx
		; si -> after condition char
		; si - 1 -> last condition char
@@:
	dec si
	cmp si, word [..@bb_when_start]
	je @F
	cmp byte [si - 1], 32
	je @B
	cmp byte [si - 1], 9
	je @B
@@:
	mov cx, si
	sub cx, word [..@bb_when_start]
	mov word [..@bb_when_length], cx

	lodsb
%if 0
	push dx
	mov dx, msg.when
	call putsz
	mov al, 32
	call putc
	mov al, '"'
	call putc
	mov dx, word [..@bb_when_start]
	mov cx, word [..@bb_when_length]
	call disp_message_length_cx
	mov al, '"'
	call putc
	mov dx, crlf
	call putsz
	dec si
	lodsb
	pop dx
%endif
	retn


		; INP:	al = first character, si -> next character
		;	di = default value
		; OUT:	di = counter value (default if EOL)
		;	al = first character after number, si -> next
		; CHG:	-
		;	does not return if error encountered
getcounter:
.:
	call skipwh0
	call iseol?
	je .got_counter
	push dx
	call getword
	mov di, dx
	pop dx
.got_counter:
	retn


		; INP:	al = first character, si -> next character
		; OUT:	di = counter value (defaults to 8000h)
		; CHG:	ax, si (flags not changed)
		;	does not return if error encountered
.pushf_chkeol:
	mov di, 8000h		; default counter
	pushf
	push dx
	dec si
	mov dx, msg.number
	call isstring?
	je @F
	mov dx, msg.counter
	call isstring?
	jne @FF
@@:
	call skipequals
	db __TEST_IMM8		; (skip lodsb)
@@:
	lodsb
	pop dx
	call .
	call chkeol
	popf
	retn


point_number:
	call skipwhite

	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	call findpointat	; do we find it ?
	jc error		; not found -->
	call getcounter.pushf_chkeol
	jmp @F			; point index is in dx -->

.not_at:
	call getpointindex
	call getcounter.pushf_chkeol
	jnc @F
	jnz error		; "NEW" is invalid -->

	xor cx, cx
.all_loop:
	mov ax, cx
	mov dx, cx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz .all_next
	call .setnumber
.all_next:
	inc cx
	cmp cx, _NUM_B_BP
	jb .all_loop
	retn

@@:
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz error

.setnumber:
	mov bx, dx
	add bx, bx
	mov word [b_bplist.counter + bx], di
	retn


point_id:
	call skipwhite

	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	call findpointat	; do we find it ?
	jc error		; not found -->
	jmp @F			; point index is in dx -->

.not_at:
	call getpointindex
	jc error		; "NEW" and "ALL" keywords are invalid -->

@@:
	push ax
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz error
	pop ax

	call skipwh0

	dec si
	push dx
	mov dx, msg.id
	call isstring?
	jne .no_id_kw
	call skipequals
	dec si
.no_id_kw:
	pop dx
	call get_id

		; INP:	..@bb_id_start
		;	..@bb_id_length
		;	dx = point index
		; OUT:	jumps to error if failure (too long)
		; CHG:	ax, bx, cx, si, di
set_id:
	mov bx, -1
	call get_set_id_offset_length
	mov cl, 10
	mov ax, bx
	shr bx, cl		; bx = length of ID to free

	mov si, word [b_bplist.idbuffer.free]
				; offset free
	neg si			; - offset free
	add si, b_bplist.idbuffer.length
				; 1024 - offset free = amount free
	add si, bx		; amount free + length of ID to free
	mov cx, word [..@bb_id_length]
	jcxz .empty		; if no ID to set -->
	cmp si, cx		; enough free ?
	mov si, word [..@bb_id_start]
				; -> ID string
	jb error		; no -->

	push cx
	call free_id		; actually free it now
	pop cx
	push cx
	mov bx, cx		; length
	mov cl, 10		; offset part is 10 bits (0..1023)
	shl bx, cl		; length is in top 6 bits (0..63)
	pop cx
	mov di, word [b_bplist.idbuffer.free]
				; = offset of free part
	add word [b_bplist.idbuffer.free], cx
				; mark space as used
	or bx, di		; OR in the offset
	add di, b_bplist.idbuffer
				; -> into buffer space
	rep movsb		; write

				; now remember this
.after_empty:
		; INP:	dx = 0-based point index
		;	bx = word to set (-1 if not to modify)
		; OUT:	bx = word read
get_set_id_offset_length: equ $
	xchg dx, bx
	shl bx, 1
	push word [b_bplist.id + bx]
	cmp dx, -1
	je @F
	mov word [b_bplist.id + bx], dx
@@:
	pop dx
	shr bx, 1
	xchg dx, bx
	retn

.empty:
	call free_id
	xor bx, bx		; offset = 0 and length = 0
	jmp .after_empty


		; INP:	ax = offset/length word of ID to free
		;	 (length zero means none)
		;	b_bplist.id = ID array (ONE of which matches ax)
		; CHG:	ax, bx, cx
		; OUT:	b_bplist.id entries adjusted
		;	 (the one that is being freed is unaffected)
		;	b_bplist.idbuffer adjusted
		; STT:	UP, ss = ds = es
		; REM:	The b_bplist.id array contains zeroes as
		;	 indicators of unused entries. This implies
		;	 that the length field is zero too. However,
		;	 the canonical NULL entry is *all* zeros.
free_id:
	mov cl, 10
	mov bx, ax
	and bx, 1023		; bx = offset of ID to free
	shr ax, cl		; ax = length of ID to free
	xchg cx, ax		; cx = length of ID to free

	push si
	push di

	jcxz .return		; if none to free -->

	 push cx
	lea di, [b_bplist.idbuffer + bx]
				; -> ID to be freed
				;  (destination of following data)
	mov si, di
	add si, cx		; -> behind ID to be freed
				;  (source of following data)
	mov cx, si
	neg cx			; minus pointer to first subsequent data
	add cx, b_bplist.idbuffer + b_bplist.idbuffer.length
				; pointer behind buffer - pointer subsequent data
				;  = length of data to move
	rep movsb		; now di -> first uninitialised byte
	 pop cx			; = length of data freed

	sub word [b_bplist.idbuffer.free], cx
				; mark as free
	push cx
	xor al, al
	rep stosb		; clear the buffer trailer (uninitialised part)
	pop di			; di = length of data freed

	mov si, b_bplist.id
%if _NUM_B_BP < 256
	mov cl, _NUM_B_BP
%else
	mov cx, _NUM_B_BP
%endif
.loop:
	lodsw
	and ax, 1023		; get offset
	cmp ax, bx		; offset matches what we're freeing ?, OR
				;  is it below/equal the offset we're freeing ?
	jbe .next		; yes --> (also jumps if ax == 0)
	sub word [si - 2], di	; adjust offset
		; This subtraction shouldn't underflow the 10 bits
		;  used for the offset, so it should leave the top
		;  6 bits for the ID length unchanged.
.next:
	loop .loop

.return:
	pop di
	pop si
	retn


%if BPSIZE == 6 || BPSIZE == 9
point_offset:
	call skipwhite

	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	call findpointat	; do we find it ?
	jc error		; not found -->
	jmp @F			; point index is in dx -->

.not_at:
	call getpointindex
	jc error		; "NEW" and "ALL" keywords are invalid -->

@@:
	push ax
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz error
	pop ax

	call skipwh0
	push dx
	mov dx, -1
%if BPSIZE == 9
	mov bx, dx
%endif
	call iseol?
	je @F
	dec si
	mov dx, msg.offset
	call isstring?
	jne .no_offset_kw
	call skipequals
	dec si
.no_offset_kw:
	lodsb
%if BPSIZE == 9
	call getdword		; bx:dx = offset
%else
	call getword		; dx = offset
%endif
	call chkeol
@@:
	pop ax
	mov di, ax
	add di, di
	add di, di		; * 4
%if BPSIZE == 6
	add di, ax		; * 5
	add di, ax		; * 6
	add di, b_bplist.bp + 3
%elif BPSIZE == 9
	add di, di		; * 8
	add di, ax		; * 9
	add di, b_bplist.bp + 4
%else
 %error Unexpected breakpoint size
%endif
	xchg ax, dx
	stosw			; store low word of offset
%if BPSIZE == 9
	xchg ax, bx
	stosw			; store high word of offset
%endif
	retn
%endif


point_when:
	call skipwhite

	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	call findpointat	; do we find it ?
	jc error		; not found -->
	jmp @F			; point index is in dx -->

.not_at:
	call getpointindex
	jc error		; "NEW" and "ALL" keywords are invalid -->

@@:
	push ax
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz error
	pop ax

	call skipwh0
	and word [..@bb_when_length], 0
	call iseol?
	je @F
	dec si
	push dx
	mov dx, msg.when
	call isstring?
	jne .no_when_kw
	call skipequals
	dec si
.no_when_kw:
	pop dx
	call get_when
	call chkeol
@@:


		; INP:	..@bb_when_start
		;	..@bb_when_length
		;	dx = point index
		; OUT:	jumps to error if failure (too long)
		; CHG:	ax, bx, cx, si, di
		; STT:	UP, ss = ds = es
set_when:
	call check_when_space	; cx = length (with terminating NUL) or 0,
				;  si -> clause (if cx != 0),
				;  ax = prior pointer or 0
	jcxz .empty

	push cx
	call free_when		; actually free it now (INP ax)
	pop cx
	mov di, word [b_bplist.whenbuffer.free]
				; = offset of free part
	add word [b_bplist.whenbuffer.free], cx
				; mark space as used
	add di, b_bplist.whenbuffer
				; -> into buffer space
	mov bx, di		; bx -> buffer for clause, for set function
				; si -> new clause (left by check function)
	rep movsb		; write (with space for the NUL)
	mov byte [di - 1], 0	; actually write a NUL

				; now remember this
.after_empty:
		; INP:	dx = 0-based point index
		;	bx = word to set (-1 if not to modify)
		; OUT:	bx = word read
get_set_when_offset: equ $
	xchg dx, bx
	shl bx, 1
	push word [b_bplist.when + bx]
	cmp dx, -1
	je @F
	mov word [b_bplist.when + bx], dx
@@:
	pop dx
	shr bx, 1
	xchg dx, bx
	retn

.empty:
	call free_when
	xor bx, bx		; offset = 0
	jmp .after_empty


		; INP:	..@bb_when_start
		;	..@bb_when_length
		;	dx = point index
		; OUT:	jumps to error if failure (too long)
		;	ax = prior pointer from array (to be freed),
		;	 or 0 if no prior clause to free
		;	cx = length (including terminating NUL)
		;	 or = 0 if no WHEN clause
		;	(if cx != 0) si -> WHEN clause data
		; CHG:	ax, bx, cx, si, di
		; STT:	UP, ss = ds = es
check_when_space:
	mov bx, -1
	call get_set_when_offset

	push bx
	test bx, bx
	jz @F
	mov di, bx
	mov cx, -1
	mov al, 0
	repne scasb
	not cx			; = length to free (including terminating NUL)
	mov bx, cx
@@:				; bx = length to free
	pop ax			; ax -> prior clause in .whenbuffer

	mov si, word [b_bplist.whenbuffer.free]
				; offset free
	neg si			; - offset free
	add si, b_bplist.whenbuffer.length
				; 1024 - offset free = amount free
	add si, bx		; amount free + length of condition to free
	mov cx, word [..@bb_when_length]
	jcxz .empty		; if no condition to set -->
	inc cx			; count terminating NUL
	cmp si, cx		; enough free ?
	mov si, word [..@bb_when_start]
				; -> condition string
	jb error		; no -->
.empty:
	retn


		; INP:	ax = offset word of condition to free
		;	 (zero means none)
		;	b_bplist.when = condition array (ONE of which matches ax)
		; CHG:	ax, bx, cx
		; OUT:	b_bplist.when entries adjusted
		;	 (the one that is being freed is unaffected)
		;	b_bplist.whenbuffer adjusted
		; STT:	UP, ss = ds = es
		; REM:	The b_bplist.when array contains actual offsets
		;	 into the b_bplist.whenbuffer space. Therefore
		;	 a value of zero acts as a NULL pointer and valid
		;	 values are >= b_bplist.whenbuffer.
free_when:
	push si
	push di

	test ax, ax
	jz .return

	mov di, ax
	mov bx, ax
				; -> condition to be freed
				;  (destination of following data)
	push di
	mov cx, -1
	mov al, 0
	repne scasb
	not cx			; = length to free (including terminating NUL)
	pop di
	 push cx
	mov si, di
	add si, cx		; -> behind condition to be freed
				;  (source of following data)
	mov cx, si
	neg cx			; minus pointer to first subsequent data
	add cx, b_bplist.whenbuffer + b_bplist.whenbuffer.length
				; pointer behind buffer - pointer subsequent data
				;  = length of data to move
	rep movsb		; now di -> first uninitialised byte
	 pop cx			; = length of data freed

	sub word [b_bplist.whenbuffer.free], cx
				; mark as free
	push cx
	xor al, al
	rep stosb		; clear the buffer trailer (uninitialised part)
	pop di			; di = length of data freed

	mov si, b_bplist.when
%if _NUM_B_BP < 256
	mov cl, _NUM_B_BP
%else
	mov cx, _NUM_B_BP
%endif
.loop:
	lodsw
	cmp ax, bx		; offset we're freeing ?, OR
				;  is it below/equal the offset we're freeing ?
	jbe .next		; yes --> (also jumps if ax == 0)
	sub word [si - 2], di	; adjust offset
.next:
	loop .loop

.return:
	pop di
	pop si
	retn


point_clear:
	mov di, .clear
	jmp point_clear_enable_disable_toggle_common

.clear:
	not ax
%if ((_NUM_B_BP+7)>>3) != 1
	and byte [b_bplist.used_mask+bx], ah
	and byte [b_bplist.disabled_mask+bx], ah
%else
	and byte [b_bplist.used_mask], ah
	and byte [b_bplist.disabled_mask], ah
%endif
	push cx

	xor bx, bx		; replace by empty word
	call get_set_id_offset_length
	xchg ax, bx		; ax = word what to free
	call free_id		; actually free it now

	xor bx, bx		; replace by empty word
	call get_set_when_offset
	xchg ax, bx		; ax = word what to free
	call free_when		; actually free it now

%if 0
	xor cx, cx

	mov bx, dx
	add bx, bx		; * 2
	mov word [b_bplist.counter + bx], cx

	add bx, bx		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add bx, dx		; * 5
%elif BPSIZE == 6
	add bx, dx		; * 5
	add bx, dx		; * 6
%elif BPSIZE == 9
	add bx, bx		; * 8
	add bx, dx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add bx, b_bplist.bp
	mov word [bx], cx
	mov word [bx + 2], cx
%if BPSIZE == 4
%elif BPSIZE == 5
	mov byte [bx + 4], cl
%elif BPSIZE == 6
	mov word [bx + 4], cx
%elif BPSIZE == 9
	mov word [bx + 4], cx
	mov word [bx + 6], cx
	mov byte [bx + 8], cl
%else
 %error Unexpected breakpoint size
%endif

%endif

	pop cx
	retn

point_clear_enable_disable_toggle_common:
	call skipwhite

	mov bp, .checkend
	dec si
	mov dx, msg.in
	call isstring?
	jne .notin
	mov bp, .parse_next
	call skipwhite
	dec si
	mov dx, msg.existing
	call isstring?
	lodsb
	jne .notexisting
	mov bp, .parse_next_existing
	call skipwh0
.notexisting:
	push si
	jmp .in

.in_multiple:
	push di
	dec si
	call get_value_range	; OUT:	cx:di = from, bx:dx = to
	jnc @F
	jnz .error
	cmp di, _NUM_B_BP
	jae .error
	jcxz .in_multiple.next	; (cx = 0)
.error:
	jmp error

@@:
	cmp di, _NUM_B_BP
	jae .error
	jcxz @F
	jmp .error

@@:
	test bx, bx
	jnz .error
	cmp dx, _NUM_B_BP
	jae .error

	mov cx, dx
	sub cx, di
	inc cx			; = amount (nonzero)
.in_multiple.next:
	mov dx, di		; dx = first index
	pop di
	clc			; no keywords
	retn


	align 2, db 0
	dw .in_multiple
.do_next_existing:
	jcxz @FFF		; value range with length zero ?
@@:
	call .checkindex_internal
	jz @F
	call di
@@:
	inc dx
	loop @BB		; loop through value range
@@:
	dec si
	call skipcomma
	call iseol?
	jne .in
	retn

	align 2, db 0
	dw .in_multiple
.parse_next_existing:
	dec si
	call skipcomma
	call iseol?
	jne .in

	pop si
	mov bp, .do_next_existing
	jmp .in

	align 2, db 0
	dw .in_multiple
.do_next:
	jcxz @FFF		; value range with length zero ?
@@:
	call .checkindex
	call di
@@:
	inc dx
	loop @BB		; loop through value range
@@:
	dec si
	call skipcomma
	call iseol?
	jne .in
	retn

	align 2, db 0
	dw .in_multiple
.parse_next:
	jcxz @FFF		; value range with length zero ?
@@:
	call .checkindex
@@:
	inc dx
	loop @BB		; loop through value range
@@:
	dec si
	call skipcomma
	call iseol?
	jne .in

	pop si
	mov bp, .do_next

.in:
	dec si
.notin:
	lodsb
	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	call findpointat	; do we find it ?
	jc error		; not found -->
	mov cx, 1		; (for bb IN)
	jmp @F			; point index is in dx -->

.not_at:
	call near word [cs:bp - 2]
	jnc @F			; point index is in dx, no keyword -->
.error_NZ:
	jnz error		; "NEW" is invalid -->

	call chkeol
	xor cx, cx
.all_loop:
	mov ax, cx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz .all_next
	mov dx, cx
	call di
.all_next:
	inc cx
	cmp cx, _NUM_B_BP
	jb .all_loop
	retn

@@:
	jmp bp

	align 2, db 0
	dw getpointindex
.checkend:
	call chkeol
	call .checkindex
	jmp di

.checkindex:
	call .checkindex_internal
	jz error
	retn

.checkindex_internal:
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	retn


point_enable:
	mov di, .enable
	jmp point_clear_enable_disable_toggle_common

.enable:
	not ax
%if ((_NUM_B_BP+7)>>3) != 1
	and byte [b_bplist.disabled_mask+bx], ah
%else
	and byte [b_bplist.disabled_mask], ah
%endif
	retn


point_disable:
	mov di, .disable
	jmp point_clear_enable_disable_toggle_common

.disable:
%if ((_NUM_B_BP+7)>>3) != 1
	or byte [b_bplist.disabled_mask+bx], ah
%else
	or byte [b_bplist.disabled_mask], ah
%endif
	retn


point_toggle:
	mov di, .toggle
	jmp point_clear_enable_disable_toggle_common

.toggle:
%if ((_NUM_B_BP+7)>>3) != 1
	xor byte [b_bplist.disabled_mask+bx], ah
%else
	xor byte [b_bplist.disabled_mask], ah
%endif
	retn


point_list:
	call skipwhite
	call iseol?
	je .all

	call getpointat		; "AT" keyword ?
	jc .not_at		; no -->
	call findpointat	; do we find it ?
		; Here we ignore the point index in dx, we just
		;  take note that at least one point matching the
		;  specified address exists. The points are matched
		;  against the linear address in ..@bb_saved_linear.
	jnc .all_matching

	mov dx, msg.bpnone_at
	call putsz
	retn


.all_matching:
	xor bp, bp
	xor bx, bx
	xor dx, dx
	mov di, line_out
.loop_matching:
	mov si, bx
	add si, si
	add si, si
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, bx
%elif BPSIZE == 6
	add si, bx		; * 5
	add si, bx		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, bx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, b_bplist.bp
	lodsw
	cmp word [..@bb_saved_linear], ax
	jne .next_matching
%if _PM
	lodsw
%else
	xor ax, ax
	lodsb
%endif
	cmp word [..@bb_saved_linear + 2], ax
	jne .next_matching

	push di
	mov al, 32
	mov cx, 40
	rep stosb		; initialize field with blanks
	xor al, al
	stosb			; terminate it
	pop di

	call .single		; fill buffer

	push dx
	push bx
%if 0
	test dl, 1		; an odd point ?
	jnz .odd_matching	; yes -->
	mov di, line_out + 40	; write next point after the field
	jmp .was_even_matching
.odd_matching:
%endif
	call putsline_crlf	; put line with linebreak (and no excess blanks)
	call handle_bl_when
	mov di, line_out	; write next point at start of field
.was_even_matching:
	pop bx
	pop dx
	inc dx			; increment odd/even counter
.next_matching:
	inc bx			; increment breakpoint index
	cmp bx, _NUM_B_BP
	jne .loop_matching
	jmp .end


.not_at:
	call getpointindex
	jnc @F
	jnz error		; "NEW" is invalid -->

	call chkeol
	jmp .all
@@:
	call chkeol
	mov bx, dx
	mov di, line_out
	call .single
	call putsline_crlf
	jmp handle_bl_when


.all:
	xor bp, bp		; high byte: any set points encountered yet,
				; low byte: current line has any set points
	xor bx, bx
	mov di, line_out
.loop:
	push di
	mov al, 32
	mov cx, 40
	rep stosb		; initialize field with blanks
	xor al, al
	stosb			; terminate it
	pop di

	call .single		; fill buffer

	push bx
%if 0
	test bl, 1		; an odd point ?
	jnz .odd		; yes -->
	mov di, line_out + 40	; write next point after the field
	jmp .was_even
.odd:
%endif
	test bp, 00FFh		; any point set in this line ?
	jz .skip_putsline	; no -->
	call putsline_crlf	; put line with linebreak (and no excess blanks)
	call handle_bl_when

	and bp, ~00FFh		; clear flag for next line processing
.skip_putsline:
	mov di, line_out	; write next point at start of field
.was_even:
	pop bx
	inc bx
	cmp bx, _NUM_B_BP
	jne .loop
.end:
	cmp di, line_out
	je @F
	call putsline_crlf
	call handle_bl_when
@@:
	test bp, 0FF00h
	jnz @F
	mov dx, msg.bpnone
	call putsz
@@:
	retn

.single:
	mov si, msg.bp
	call showstring
	push bx
	mov ax, bx
	call hexbyte		; store index of this point
	call calcpointbit
	mov si, msg.bpunused
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask+bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jnz @F			; if set -->
	call showstring
	xor si, si
	jmp .unused

@@:
	or bp, 0101h		; flag that there was a point set in this line
	mov si, msg.bpdisabled
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.disabled_mask+bx], ah
%else
	test byte [b_bplist.disabled_mask], ah
%endif
	jnz .disabled		; disabled --> (D)
	mov si, msg.bpenabled
.disabled:
	call showstring
	mov si, msg.bpaddress
	call showstring
	 pop ax
	 push ax
	mov si, ax
	add si, si
	add si, si
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, ax
%elif BPSIZE == 6
	add si, ax		; * 5
	add si, ax		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, ax		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, b_bplist.bp	; -> point
	push dx
	lodsw
	xchg ax, dx
%if _PM
	lodsw
	call hexword
%else
	xor ax, ax
	lodsb
	call hexbyte
%endif
	 push ax
	mov al, '_'
	stosb
	 pop ax
	xchg ax, dx
	call hexword		; display (linear) address
%if BPSIZE == 6 || BPSIZE == 9
		; INP:	dx:ax = linear address
		;	si -> (d)word offset
		;	di -> where to store
		; OUT:	cx = length displayed
		;	si -> after offset
		;	di -> after stored string
		; CHG:	ax, dx
	call bp_display_offset	; BPSIZE implied
%else
	xor cx, cx
%endif
	pop dx
	lodsb
	push ax
	mov si, msg.bpcontent
	call showstring
	pop ax
	call hexbyte		; display content
	mov si, msg.bpcounter
	call showstring
	 pop ax
	 push ax
	mov bx, ax
	push dx
	mov dx, ax
	add bx, bx
	mov ax, word [b_bplist.counter + bx]
	call hexword

	mov bx, -1
	call get_set_id_offset_length
	test bh, 63 << 2	; length nonzero ?
	jz @F			; no -->

		; The maximum length of a short ID is based on
		;  how much space there is assuming 80 columns.
	mov si, msg.bb_hitpass_id.short
	shl cl, 1
	shl cl, 1
	neg cl
	add cl, 35 << 2
	cmp bh, cl		; long ?
	jb .trigger_short_id
		; This jump MUST be a jb, not jbe. The jbe
		;  would not match ZR for words where the
		;  idbuffer offset is a nonzero value.
	mov si, msg.bb_hitpass_id.long
.trigger_short_id:

	call copy_single_counted_string
	mov cl, bh
	shr cl, 1
	shr cl, 1		; cx = length
	and bx, 1023		; bx = offset
	lea si, [b_bplist.idbuffer + bx]
	rep movsb

@@:

	mov bx, -1
	call get_set_when_offset
	mov si, bx
	pop dx

.unused:
	pop bx			; restore counter (if displaying all)
	retn


		; CHG:	si, al
handle_bl_when:
	xchg dx, si
	test dx, dx
	jz @F
	push dx
	mov dx, msg.bb_when
	call putsz
	pop dx
	call putsz
%if 0
	mov al, '$'
	call putc
%endif
	mov dx, crlf
	call putsz
@@:
	xchg dx, si
	retn


		; INP:	ax = 0-based index of point
		; OUT:	(bx-> byte to access. only if at least 9 points)
		;	(bx = 0 always if 8 or fewer points)
		;	ah = value to access
		; CHG:	al
calcpointbit:
%if ((_NUM_B_BP+7)>>3) != 1
	mov bx, ax
%endif
	and al, 7
	mov ah, 1
	xchg ax, cx
	shl ch, cl
%if ((_NUM_B_BP+7)>>3) != 1
	mov cl, 3
	shr bx, cl
%else
	xor bx, bx
%endif
	xchg ax, cx
	retn


		; INP:	bx:dx = linear address
		; OUT:	NC if point found,
		;	 dx = point index
		;	CY if point not found,
		;	 bx:dx unchanged
		; CHG:	di
findpointat:
	lframe near
	lenter
	lvar word,	orig_ax
	 push ax
	lvar word,	orig_si
	 push si
	lvar dword,	orig_bxdx
	 push bx
	 push dx
	xor dx, dx
.loop:
	mov ax, dx
	call calcpointbit
%if ((_NUM_B_BP+7)>>3) != 1
	test byte [b_bplist.used_mask + bx], ah
%else
	test byte [b_bplist.used_mask], ah
%endif
	jz .next

	mov si, dx
	add si, si
	add si, si
%if BPSIZE == 4
%elif BPSIZE == 5
	add si, dx
%elif BPSIZE == 6
	add si, dx		; * 5
	add si, dx		; * 6
%elif BPSIZE == 9
	add si, si		; * 8
	add si, dx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	add si, b_bplist.bp	; -> point
	lodsw
	cmp word [bp + ?orig_bxdx], ax
	jne .next
%if _PM
	lodsw
%else
	xor ax, ax
	lodsb
%endif
	cmp word [bp + ?orig_bxdx + 2], ax
	jne .next
				; (NC)
	pop bx			; discard dx on stack, clobbering bx
	jmp .ret_with_dx

.next:
	inc dx
	cmp dx, _NUM_B_BP
	jb .loop

	stc
.ret:
	pop dx
.ret_with_dx:
	pop bx			; pop ?orig_bxdx
	pop si			; pop ?orig_si
	pop ax			; pop ?orig_ax
	lleave
	lret


		; INP:	si->, al=
		; OUT:	CY if no "AT" keyword + address,
		;	 si, al unchanged
		;	NC if "AT" keyword + address,
		;	 si->, al= after
		;	 bx:dx = dword [..@bb_saved_linear] = linear address
		; CHG:	edx, bx
getpointat:
	dec si
	mov dx, msg.at
	call isstring?
	lodsb
	je .at
	stc
	retn

.at:
	mov bx, word [reg_cs]
	call getlinearaddr
	jc error
	mov word [..@bb_saved_linear], dx
	mov word [..@bb_saved_linear + 2], bx
	retn


		; INP:	si->, al=
		; OUT:	NC if a point is specified,
		;	 dx = point index (0-based, below _NUM_B_BP)
		;	CY if a keyword is specified,
		;	 ZR if "ALL" keyword specified
		;	 NZ if "NEW" keyword specified
getpointindex:
	dec si
	mov dx, msg.all
	call isstring?
	je .is_all		; (ZR)
	mov dx, msg.new
	call isstring?
	je .is_new
	lodsb
	call getword
	cmp dx, _NUM_B_BP
	jae error
	clc			; (NC)
	retn

.is_new:
	test si, si		; (NZ)
.is_all:
	stc			; (CY)
	lodsb			; al = separator, si-> after
	retn
%endif


%if BPSIZE == 6 || BPSIZE == 9
		; INP:	dx:ax = linear address
		;	si -> (d)word offset
		;	di -> where to store
		; OUT:	cx = length displayed
		;	si -> after offset
		;	di -> after stored string
		; CHG:	ax, dx
bp_display_offset:
	lframe
	lvar dword,	offset
	lenter
	lvar dword,	linear
	 push dx
	 push ax
	mov ax, "  "
	lvar word,	prefix
	 push ax
	lvar word,	start_write
	 push di
	push bx
	lodsw
%if _PM
	xchg ax, dx
	lodsw
	cmp ax, -1
	xchg ax, dx
	jne @F
%else
	xor dx, dx
%endif
	cmp ax, -1
	je .skip
@@:
	mov word [bp + ?offset + 2], dx
	mov word [bp + ?offset], ax

	mov dx, word [bp + ?linear + 2]
	mov ax, word [bp + ?linear]
	sub ax, word [bp + ?offset]
	sbb dx, word [bp + ?offset + 2]

%if _PM
	call ispm
	jnz .r86m

	push dx
	push ax
	mov ax, 6
	mov bx, word [reg_cs]
	int 31h
	pop ax
	pop bx
	jc .try_r86m

	cmp cx, bx
	jne .try_r86m
	cmp dx, ax
	jne .try_r86m

	mov ax, "  "
	stosw
	mov ax, "CS"
	stosw
	jmp .offset

.try_r86m:
	mov byte [bp + ?prefix + 1], '$'
%endif

.r86m:
	mov cx, 4
	test al, 15
	jnz .questionmarks
	test dx, 0FFF0h
	jnz .questionmarks
	shr ax, cl
	ror dx, cl
	or dx, ax
	mov ax, word [bp + ?prefix]
	stosw
	xchg ax, dx
	call hexword

.offset:
	mov al, ':'
	stosb
%if _PM
	mov ax, word [bp + ?offset + 2]
	test ax, ax
	jz @F
	call hexword
@@:
%endif
	mov ax, word [bp + ?offset]
	call hexword

.skip:
	pop bx
	pop cx			; get ?start_write
	neg cx
	add cx, di
	lleave
	retn

.questionmarks:
	mov ax, "  "
	stosw
	mov ax, "??"
	stosw
	stosw
	jmp .offset


point_swap:
	call skipwhite
	call getpointindex
	jc .error
	mov bp, dx
	call getpointindex
	jc .error
	call chkeol

	mov cx, 2
.loop_push:
	mov ax, dx
	call calcpointbit
	mov al, ah
	and ah, byte [b_bplist.used_mask + bx]
	and al, byte [b_bplist.disabled_mask + bx]
	push ax
	mov bx, dx
	add bx, bx		; * 2
	push word [b_bplist.counter + bx]
	push word [b_bplist.id + bx]
	push word [b_bplist.when + bx]

	add bx, bx		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add bx, dx		; * 5
%elif BPSIZE == 6
	add bx, dx		; * 5
	add bx, dx		; * 6
%elif BPSIZE == 9
	add bx, bx		; * 8
	add bx, dx		; * 9
%else
 %error Unexpected breakpoint size
%endif
	push word [b_bplist.bp + bx]
	push word [b_bplist.bp + bx + 2]
%if BPSIZE == 4
%elif BPSIZE == 5
	push word [b_bplist.bp + bx + 4]
%elif BPSIZE == 6
	push word [b_bplist.bp + bx + 4]
%elif BPSIZE == 9
	push word [b_bplist.bp + bx + 4]
	push word [b_bplist.bp + bx + 6]
	push word [b_bplist.bp + bx + 8]
%else
 %error Unexpected breakpoint size
%endif
	xchg bp, dx
	loop .loop_push

	mov cl, 2
.loop_pop:
	mov bx, dx
	add bx, bx		; * 2
	add bx, bx		; * 4
%if BPSIZE == 4
%elif BPSIZE == 5
	add bx, dx		; * 5
%elif BPSIZE == 6
	add bx, dx		; * 5
	add bx, dx		; * 6
%elif BPSIZE == 9
	add bx, bx		; * 8
	add bx, dx		; * 9
%else
 %error Unexpected breakpoint size
%endif
%if BPSIZE == 4
%elif BPSIZE == 5
	pop ax
	mov byte [b_bplist.bp + bx + 4], al
%elif BPSIZE == 6
	pop word [b_bplist.bp + bx + 4]
%elif BPSIZE == 9
	pop ax
	mov byte [b_bplist.bp + bx + 8], al
	pop word [b_bplist.bp + bx + 6]
	pop word [b_bplist.bp + bx + 4]
%else
 %error Unexpected breakpoint size
%endif
	pop word [b_bplist.bp + bx + 2]
	pop word [b_bplist.bp + bx]

	mov bx, dx
	add bx, bx		; * 2
	pop word [b_bplist.when + bx]
	pop word [b_bplist.id + bx]
	pop word [b_bplist.counter + bx]
	pop si
	mov ax, dx
	call calcpointbit
	xchg si, dx
	not ah
	and byte [b_bplist.used_mask + bx], ah
	and byte [b_bplist.disabled_mask + bx], ah
	not ah
	test dl, dl
	jz @F
	or byte [b_bplist.disabled_mask + bx], ah
@@:
	test dh, dh
	jz @F
	or byte [b_bplist.used_mask + bx], ah
@@:
	xchg si, dx
	xchg bp, dx
	loop .loop_pop
	retn

.error:
	jmp error
%endif

%if _DUALCODE
	usesection lDEBUG_CODE2

section_of bu_relocated
dualfunction
bu_relocated: section_of_function
	lframe dualdistance
	lpar word, sign
	lenter
	mov ax, word [bp + ?sign]
	mov di, msg.bu_relocated.sign
	nearcall hexword
	mov dx, msg.bu_relocated
	nearcall putsz
	lleave
	dualreturn
	lret

	usesection lDEBUG_CODE
%endif

bu_breakpoint:
	lodsb
%if _DUALCODE
	cmp al, '2'
	je .2
%endif
	call chkeol
%if _DEBUG
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jnz @F
	mov dx, msg.bu_disabled
	jmp putsz
@@:
 %endif
	mov dx, msg.bu
	call putsz
	pop dx			; discard near return address
	mov dx, dmycmd		; point dx to empty function
	jmp cmd4.int3		; run a breakpoint right before dispatcher
%else
	mov dx, msg.notbu
	jmp putsz
%endif

%if _DUALCODE
.2:
	call skipwhite
	call chkeol
	mov ax, 2642h
	 push ax
	dualcall bu_relocated
	retn
%endif
