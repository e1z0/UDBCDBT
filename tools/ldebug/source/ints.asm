
%if 0

Interrupt hooking and unhooking

2021 by C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

	usesection lDEBUG_CODE


		; INP:	86 Mode ss = word [pspdbg] = cs for handler
		;	si -> handler entrypoint
		;	dword [si + ieNext] = storage for next vector
		;	al = interrupt number
		; CHG:	ax, bx, cx, dx
		; STT:	ss = ds (= word [pspdbg] if in 86 Mode)
install_86m_interrupt_handler:
%if _PM
	call ispm
	jnz .rm

	xchg ax, bx		; bl = interrupt number
	mov ax, 0200h
	int 31h			; cx:dx = interrupt vector
	mov word [si + ieNext], dx
	mov word [si + ieNext + 2], cx

	mov ax, 0201h
				; bl still = interrupt number
	mov cx, word [pspdbg]	; cx => lDEBUG_DATA_ENTRY
	mov dx, si		; cx:dx -> our entrypoint
	int 31h			; change vector to our handler
	retn

.rm:
%endif

	call InDos
	jz .notindos
	push es
	push ds
	xor bx, bx
	mov ds, bx
	mov bl, al
	add bx, bx
	add bx, bx		; ds:bx -> interrupt vector
	les dx, [bx]		; es:dx = vector
	mov word [ss:si + ieNext], dx
	mov word [ss:si + ieNext + 2], es
				; save prior vector
	mov word [bx], si
	mov word [bx + 2], ss	; ss => lDEBUG_DATA_ENTRY
				; change vector to our handler
	pop ds
	pop es
	retn

.notindos:
	 push es
	mov ah, 35h
	int 21h
	mov word [si + ieNext], bx
	mov word [si + ieNext + 2], es
	 pop es
	mov ah, 25h
	mov dx, si		; ds => lDEBUG_DATA_ENTRY
	int 21h			; change vector to our handler
	retn


%if _PM
get_86m_interrupt_handler_no_dos:
	call ispm
	jnz get_86m_interrupt_handler.rm_indos
%endif

		; INP:	al = interrupt number
		; OUT:	dx:bx = 86 Mode far pointer
		; CHG:	ah
get_86m_interrupt_handler:
%if _PM
	call ispm
	jnz .rm

	push ax
	push cx
	xchg ax, bx		; bl = interrupt number
	mov ax, 0200h
	int 31h			; cx:dx = interrupt vector
	xchg cx, bx		; bx:dx
	xchg bx, dx		; dx:bx
	pop cx
	pop ax
	retn

.rm:
%endif

	call InDos
	jz .notindos
%ifn _PM
get_86m_interrupt_handler_no_dos: equ $
%endif
.rm_indos:
	push ds
	xor bx, bx
	mov ds, bx
	mov bl, al
	add bx, bx
	add bx, bx		; ds:bx -> interrupt vector
	mov dx, word [bx + 2]
	mov bx, word [bx]
	pop ds
	retn

.notindos:
	 push es
	mov ah, 35h
	int 21h
	mov dx, es
	 pop es
	retn


		; INP:	al = interrupt number
		; OUT:	ZR if offset = -1 or segment = 0
		;	NZ else
		; CHG:	ah, dx, bx
intchk:
	call get_86m_interrupt_handler
	inc bx
	jz @F			; was 0FFFFh -->
	test dx, dx
	; jz @F			; was 0000h -->
@@:
	retn


		; INP:	al = interrupt number
		;	dx:bx = 86 Mode far pointer
		; CHG:	ah
set_86m_interrupt_handler:
	push ax
	push bx
	push cx
	push dx
%if _PM
	call ispm
	jnz .rm

	xchg ax, bx		; bl = interrupt number,
				;  dx:ax = vector
	xchg ax, dx		; ax:dx
	xchg cx, ax		; cx:dx
	mov ax, 0201h
	int 31h			; cx:dx = interrupt vector
	jmp .ret

.rm:
%endif

	push ds
	call InDos
	jz .notindos
	 push bx
	xor bx, bx
	mov ds, bx
	mov bl, al
	add bx, bx
	add bx, bx		; ds:bx -> interrupt vector
	mov word [bx + 2], dx
	 pop word [bx]
	jmp .ret_ds

.notindos:
	xchg bx, dx
	mov ds, bx		; ds:dx = vector
	mov ah, 25h
	int 21h
.ret_ds:
	 pop ds
.ret:
	pop dx
	pop cx
	pop bx
	pop ax
	retn


		; INP:	dx = 86 Mode segment to access
		; OUT:	es => segment
		; CHG:	-
%if _PM
setes2dx:
	call ispm
	jnz @F
	push bx
	call setrmsegm
	mov es, bx
	pop bx
	retn

@@:
	mov es, dx
	retn
%endif	; _PM


		; INP:	ds:si -> source IISP header (or pseudo header)
		;	es:di -> destination IISP header
		; OUT:	EI
		;	si and di both incremented by 6
		; CHG:	-
		; STT:	UP
update_iisp_header:
	push ax
	push cx
	push dx
	push di
	push si
	push es
%if _PM
	 dualcall selector_to_segment
%endif
	push ds
	 push ss
	 pop ds
	call findinstalleddebugger
	mov word [debuggerfunction], ax
	pop ds			; must be not using scratchsel !
%if _PM
	pop dx
	call setes2dx		; this one uses scratchsel
%else
	pop es
%endif
	pop si
	pop di
	pop dx
	pop cx
	; push ax
	mov ax, word [ss:debuggerfunction]
	test ax, ax			; found the debugger ?
	jz @F				; no -->
%if _PM
	call ispm
	jnz .86m
[cpu 286]
	push es				; es
	 dualcall selector_to_segment	; convert to segment
	push ds				; ds
	 dualcall selector_to_segment	; convert to segment
	push word 2Dh			; int 2Dh
	push bp				; bp
	call intcall_return_parameter_es_parameter_ds
	add sp, 4			; discard returned ds, es
__CPU__
	db __TEST_IMM16			; (skip int)
%endif
.86m:
	int 2Dh				; call its Update IISP Header function
	cmp al, 0FFh			; supported ?
	pop ax
	je .ret				; yes. done -->
	db __TEST_IMM8			; (skip pop)
@@:
	pop ax				; restore ax, then do manual update
%if _PM
   push ax
  call push_if
%else
  pushf
%endif
	cli				; try to rest while updating chain
	cmpsw				; skip over first word (entrypoint)
					;  (generally xxEBh or 0EA90h)
	movsw
	movsw				; transfer source ieNext to dest ieNext
%if _PM
  call pop_if
%else
  popf
%endif
.ret:
	retn


		; INP:	ds = ss = cs
		; OUT:	CY, ax = 0000h if not found
		;	NC, al = 30h if found,
		;	 ah = multiplex number
		; CHG:	si, di, es, cx, dx
findinstalleddebugger:
	mov al, 2Dh
	push bx
	call intchk
	pop bx
	jz .zero

	testopt [options3], opt3_no_call_update
	jnz .zero

	mov ah, byte [try_debugger_amis_multiplex_number]
	call .check
	jnc @F

	mov ah, 0FFh		; start with multiplex number 0FFh
.loop:
	call .check
@@:
	mov al, 30h		; al = 30h to indicate found, ah = multiplex number
	jnc .end		; if found --> (NC)
	sub ah, 1		; search is backward (to find latest installed first), from 0FFh to 00h including
	jnc .loop		; try next if we didn't check all yet -->

.zero:
	xor ax, ax		; al = 0 to indicate none found
	stc			; (CY)
.end:
	retn


		; INP:	ah = multiplex number to check
		;	ds = ss = cs
		; OUT:	CY if multiplex number unused or no signature match,
		;	 bp, ah, ds unmodified
		;	NC if match found,
		;	 ah = multiplex number (unmodified)
		; CHG:	si, di, es, cx, dx
.check:
	testopt [internalflags4], dif4_int_2D_hooked
	jz @F
	cmp ah, byte [amis_multiplex_number]
	je .notfound		; do not use our own multiplexer -->
@@:
	mov al, 00h		; AMIS installation check
%if _PM
	call call_int2D
%else
	int 2Dh			; AMIS (or "DOS reserved" = iret if no AMIS present)
%endif
	cmp al, 0FFh
	jne .notfound
	mov si, debuggeramissig	; ds:si -> our AMIS name strings
%if _PM
	call setes2dx
%else
	mov es, dx		; es:di -> name strings of AMIS multiplexer that just answered
%endif
	mov cx, 8		; Ignore description, only compare vendor and program name
	repe cmpsw
	je .checkret		; ZR, NC = match -->
.notfound:
	stc			; NZ, CY no match
.checkret:
	retn


		; INP:	al = interrupt number
		;	ds:si-> interrupt entry
		;	dx = flag in word [options4 + 2] to force
		;	dx = -1 to force unconditionally
		; OUT:	es = ss
		;	CY if unhooking failed,
		;	 ds:si preserved
		;	NC if unhooking successful
		; CHG:	ah, di, si
		; STT:	ds = ss => data entry segment/selector
		;	word [pspdbg] = data entry 86 Mode segment
UnhookInterruptForce:
	call UnhookInterrupt
	jnc .ret
	cmp dx, -1
	je UnhookInterrupt.easy
	test word [options4 + 2], dx
	jnz UnhookInterrupt.easy
	stc
.ret:
	retn


		; INP:	al = interrupt number
		;	ds:si-> interrupt entry
		; OUT:	es = ss
		;	CY if unhooking failed,
		;	 ds:si preserved
		;	NC if unhooking successful
		; CHG:	ah, di, si
		; STT:	ds = ss => data entry segment/selector
		;	word [pspdbg] = data entry 86 Mode segment
UnhookInterrupt:
			; UnhookInterruptSim (below) only checks if it's possible to unhook this interrupt.
			; This function really unhooks the interrupt if possible.
			;
			; This is to cover the situation when some of the hooked interrupts can unhook,
			; but some can't. If the uninstaller would start to unhook the interrupts and then
			; catch the interrupt that can't be unhooked the user would end up with a dead TSR
			; that's uninstalled halfway. Very bad.
			;
			; "Simulating" the unhooking first and checking if all interrupts can unhook
			; usually will not return such a state.
	call UnhookInterruptSim
	jc .ret				; bad. --> (CY)
	jz .easy
.hard:
				; "hard" case: UnhookInterruptSim has however already done the work,
				; so the hard case is here indeed easier than the easy case.
	push si
	call update_iisp_header		; copies our stored pointer into the other's entry
	pop si				; restore --> header
	jmp .done
.easy:
	push dx
	push bx
	mov dx, word [si + ieNext + 2]
	mov bx, word [si + ieNext]	; get what we stored in the entry
	call set_86m_interrupt_handler	; easy case - just reset to the value stored
	pop bx
	pop dx
.done:
	or word [si + ieNext + 2], -1
	or word [si + ieNext], -1	; (NC)
.ret:
	push ss
	pop es
	retn


		; INP:	al = interrupt number
		;	ds:si-> interrupt entry
		;	dx = flag in word [options4 + 2] to force
		; OUT:	NC if no error (hard, easy, or forced case)
		;	CY if error
		; CHG:	ah, es, di
		; STT:	ds = ss => data entry segment/selector
		;	word [pspdbg] = data entry 86 Mode segment
UnhookInterruptForceSim:
	test word [options4 + 2], dx
	jnz UnhookInterruptSim.retn	; --> (NC)


		; INP:	ds:si-> IISP entry
		;	al = interrupt number
		; OUT:	NC if no error (either hard or easy case),
		;	 ZR if easy case,
		;	  ds:si-> our IISP entry, containing stored interrupt
		;	 NZ if hard case,
		;	  ds:si-> our IISP entry
		;	  es:di-> IISP entry to modify
		;	 implies dword [es:di + 2] = far pointer to ours
		;	CY if error (not first handler and no IISP chain to this handler)
		; CHG:	ah, es, di
		; STT:	ds = ss => data entry segment/selector
		;	word [pspdbg] = data entry 86 Mode segment
UnhookInterruptSim:
	push dx
	push bx

	; harden this, check we are an IISP entry
	 push ds
	 pop es				; es => our handler segment
	mov bx, si			; es:bx -> our handler
	call IsIISPEntry?		; does it have an IISP header ?
	jne .fail			; fail if not

	call get_86m_interrupt_handler	; get current vector
	cmp si, bx			; our pointer ?
	jne .hard
	cmp word [pspdbg], dx		; our segment ?
	jne .hard

	and ah, 00h			; NC, ZR
	pop bx
	pop dx
.retn:
	retn

.hard:
%if _PM
	call setes2dx
%else
	mov es, dx
%endif

		; INP:	ds:si-> IISP entry
		;	es:bx-> current interrupt entry
		; OUT:	CY if error
		;	NC, NZ if no error,
		;	 ds:si-> our IISP entry
		;	 es:di-> IISP entry to modify
		;	 implies dword [es:di + 2] = far pointer to ours
		; CHG:	ah, es, di, (bx, dx)
		; STT:	ds = ss => data entry segment/selector
		;	word [pspdbg] = data entry 86 Mode segment
	call SearchIISPChain
	jne .harder
.found:				; found reference to our interrupt handler
	mov di, bx			; es:di-> IISP entry that references our's
	or ah, 0FFh			; NC, NZ
	pop bx
	pop dx
	retn

.harder:			; Desperate attempt to find IISP entry that references ours by
				; searching through the interrupts hooked by other AMIS TSRs. Note
				; that the plexer loop will find and search through the list of
				; hooked interrupts of the uninstalling TSR itself, but this causes
				; no trouble.
		; INP:	ds:si-> IISP entry
		; OUT:	CY if error
		;	NC, NZ if no error,
		;	 ds:si-> our IISP entry
		;	 es:di-> IISP entry to modify
		;	 implies dword [es:di + 2] = far pointer to ours
		; CHG:	ah, es, di, (bx, dx)
		; STT:	ds = ss => data entry segment/selector
		;	word [pspdbg] = data entry 86 Mode segment
	push ax				; register with interrupt number last

	mov al, 2Dh
	call intchk			; ZR if offset = -1 or segment = 0
					; CHG: ax, dx, bx
	jz .fail_ax

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
.fail_ax:
	pop ax
.fail:					; IISP incompatible TSR between current interrupt entry and our entry
					;  and no AMIS compatible TSR installed on top of our entry
	stc
	pop bx
	pop dx
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
	jmp short .search_intlist	; try next entry -->

.search_found_intlist:
	mov bx, word [es:di]		; dx:bx = es:bx -> IISP entry
	scasw				; skip pointer
	push dx				; preserve dx for .search_intlist_seg
	call SearchIISPChain
	pop dx
	je .search_found		; found entry -->
		; This specific jump supports TSRs that hook the same
		; interrupt more than once; jumping to .nextplex instead
		; (as previously) aborts the search after the first match
		; in the interrupt list. This support might become useful.
	cmp al, 2Dh			; was last in list ?
	je .nextplex
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
	call SearchIISPChain
	jne .nextplex			; didn't find our entry in the chain -->
.search_found:
	pop ax
	jmp short .found


SearchIISPChain.next:
%if _PM
		; dx already next segment
	mov bx, word [es:bx + ieNext]	; get next offset
	call setes2dx			; point es:bx -> next handler
%else
	les bx, [es:bx + ieNext]	; get next interrupt entry
%endif

		; INP:	ds:si-> IISP entry
		;	es:bx-> current interrupt entry
		; OUT:	NZ if reference to ds:si not found in IISP chain es:bx->
		;	ZR if reference found,
		;	 es:bx-> IISP (or uninstalled iHPFS) interrupt entry with reference
		; CHG:	es, bx, dx
SearchIISPChain:
	call IsIISPEntry?			; that an IISP entry ?
	jnz .return				; nope --> (NZ)
	mov dx, word [es:bx + ieNext + 2]	; (for _PM: dx = next segment)
	cmp si, word [es:bx + ieNext]		; our offset ?
	jne .next				; no, try next -->
	cmp dx, word [pspdbg]			; our segment ?
	jne .next				; no, try next -->
.return:					; yes, found (ZR)
	retn


		; INP:	es:bx-> interrupt entry
		; OUT:	NZ if non-IISP entry
		;	ZR if IISP entry
IsIISPEntry?:
	cmp bx, - (ieSignature + 2)		; may access word at offset FFFFh ?
	ja .return				; yes, avoid --> (NZ)
	cmp word [ es:bx + ieSignature ], "KB"	; "KB"/424Bh ? ("BK" in MASM)
	jne .return
	cmp word [ es:bx + ieEntry ], 0EA90h	; nop\jmp far imm16:imm16 ?
	je .return				; unused IISP entry (created by iHPFS) -->
	cmp byte [ es:bx + ieEntry ], 0EBh	; jmp short ... ?
		; (This opcode should strictly be jmp short $+18 but there's programs
		; that save an additional jmp opcode by jumping directly into their
		; code even though it's not right behind the header.)
	jne .return
	cmp byte [ es:bx + ieJmphwreset ], 0EBh	; jmp short ... ?
	je .return				; usual IISP entry -->
	cmp byte [ es:bx + ieJmphwreset ], 0CBh	; retf ?
	je .return				; a shorter variant -->
	cmp byte [ es:bx + ieJmphwreset ], 0CFh	; iret ?
.return:
	retn


		; ! might be called with unknown ss
update_inttab_optional:
	push ax
	push bx
	push cx
	push dx
	push si
	mov si, inttab_optional
	mov bx, inttab
.loop:
	lodsw
	cmp ax, -1
	je .end
	xchg ax, cx
	lodsw
	xchg ax, cx
	xchg ax, dx
	lodsw
	xchg ax, dx
	test word [internalflags4], dx
	jz .next
	dec bx
	dec bx
	mov word [bx], cx
	dec bx
	mov byte [bx], al
.next:
	jmp .loop

.end:
	mov word [amisintr_offset], bx
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	retn
