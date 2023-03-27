
%if 0

Serial port handling code

2019 by C. Masloch
 based on http://www.sci.muni.cz/docs/pc/serport.txt The Serial Port rel. 14

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

iisphwreset serial_interrupt_handler

  align 2, db 0
iispentry serial_interrupt_handler, 80h, serial_interrupt_handler
  testopt [cs:serial_flags], sf_init_done
  jnz @F
.notours:
  jmp far [cs:.next]
@@:
  push ax
  push ds

  push cs
  pop ds
  mov al, 20h	; acknowledge interrupt
  cmp byte [serial_use_irqmask + 1], 0
  je @F
	call serial_next_is_iret
	jne @FF	; not our job to issue EOI -->
  out 0A0h, al	; to secondary PIC
  out 20h, al	; to primary PIC
  jmp @FF

@@:
	; Fix for a condition when we use interrupt 0Ch (for COM1).
	; If _CATCHINT0C then we need to pass along Stack Fault in R86M to
	;  the next handler, which will also be ours. Therefore do the inverse
	;  of the In-Service Register check here.
  mov al, 0Bh	; request In-Service Register (ISR)
  out 20h, al	; from primary PIC
  in al, 20h	; read the ISR
  test al, byte [serial_use_irqmask]
  jnz .ours
  pop ds
  pop ax
  jmp .notours

.ours:
	call serial_next_is_iret
	jne @F	; not our job to issue EOI -->
  mov al, 20h	; acknowledge interrupt
  out 20h, al	; to primary PIC
@@:
  push cx
  push dx
  push si

  cld
ih_continue:
  mov dx, [baseport]
  inc dx
  inc dx	; (base + 2) write FCR, read IIR
%if _USE_TX_FIFO
  mov al, byte [serial_fcr_setting]
  and al, ~ 0000_0110b
  out dx, al	; (write FCR) configure FIFOs
  xor ax, ax
%else
  xor ax, ax
  out dx, al	; (write FCR) configure FIFOs
  nop
%endif
  in al, dx	; (read IIR) get interrupt cause
  test al, 1	; did the UART generate the int?
  jnz ih_sep	; no, then it's somebody else's problem
  and al, 6	; mask bits not needed
  mov si, ax	; make a pointer out of it
  dec dx
  dec dx	; = base
  call near word [serial_interrupt_table + si]  ; serve this int
  jmp  ih_continue  ; and look for more things to be done
ih_sep:
  pop  si
  pop  dx  ; restore regs
  pop  cx
  testopt [options6], opt6_share_serial_irq
  jz .ret
  testopt [options6], opt6_serial_EOI_call
	; Sharing the IRQ means we need to pass along
	;  the call to the next handler. This enables
	;  using the same IRQ, eg IRQ #3 for COM2 and
	;  COM4, by two different applications.
  jz .chain
  call serial_next_is_iret
  je .chain		; already issued EOI -->
  pop  ds
  pop  ax
   pushf
   push cs
  call serial_interrupt_handler.notours
  push ax
  push ds
  mov al, 20h	; acknowledge interrupt
  cmp byte [cs:serial_use_irqmask + 1], 0
  je @F
  out 0A0h, al	; to secondary PIC
@@:
  out 20h, al	; to primary PIC
.ret:
  pop  ds
  pop  ax
  iret

.chain:
  pop  ds
  pop  ax
  jmp  serial_interrupt_handler.notours


		; INP:	ds = cs => data entry segment
		; OUT:	ZR if should issue EOI
		;	NZ if should not issue EOI
		; REM:	This returns ZR if we aren't sharing
		;	 the IRQ or we are but the next handler
		;	 consists of an iret instruction only.
serial_next_is_iret:
	push bx
	push ds
	testopt [options6], opt6_share_serial_irq
	jz @F				; if not sharing --> (ZR)
	lds bx, [serial_interrupt_handler.next]
	cmp byte [bx], 0CFh		; iret ?
@@:
	pop ds
	pop bx
	retn

	align 2, db 0
serial_interrupt_table:  dw  int_modem,int_tx,int_rx,int_status


int_modem:
  ; just clear modem status, we are not interested in it
  add dx, 6
  in al, dx		; read MSR
  retn


int_tx:
  mov dx, [baseport]
  mov si, word [txtail]

  push dx
  add dx, 5
  in al, dx		; (base + 5) read LSR
  pop dx
  test al, 20h		; Transmitter Holding Register Empty ?
  jz itx_setup_int	; no, it was a spurious interrupt -->
	; This conditional detects the condition specified in
	;  the section "Known problems with several chips":
	; When a 1 is written to the bit 1 (Tx int enab) in the
	;  IER, a Tx interrupt is generated. This is an erroneous
	;  interrupt if the THRE bit is not set. [So don't set
	;  this bit as long as the THRE bit isn't set. CB]

  ; check if there's something to be sent
%if _USE_TX_FIFO
	mov cx, 1
	test byte [serial_flags], sf_built_in_fifo
	jz @F
	mov cl, byte [serial_fifo_size]
@@:
%endif
itx_more:
  cmp  si, word [txhead]
  je  itx_nothing
  lodsb
  out dx, al		; write it to the THR
  ; check for wrap-around in our fifo
  tx_checkwrap
%if _USE_TX_FIFO
  ; send as much bytes as the chip can take when available
  loop itx_more
%endif
itx_setup_int:
  cmp si, word [txhead]
  je itx_nothing
  inc dx
  mov al, 0000_0011b
  out dx, al		; write to IER
  jmp itx_dontstop
itx_nothing:
  ; no more data in the fifo, so inhibit TX interrupts
  inc dx
  mov al, 0000_0001b
  out dx, al		; write to IER
itx_dontstop:
  mov word [txtail], si
  retn


int_rx:
  mov  si, word [rxhead]
irx_more:
  mov dx, [baseport]
  in al, dx		; read from RBR
  cmp al, 3
  jne @FF
  testopt [serial_flags], sf_ctrl_c
  jz @F
  setopt [serial_flags], sf_double_ctrl_c
@@:
  setopt [serial_flags], sf_ctrl_c
@@:
  mov  byte [si], al
  mov ax, si
  inc  si
  ; check for wrap-around
  rx_checkwrap
  cmp word [rxtail], si
  je @FF
  ; see if there are more bytes to be read
  add dx, 5
  in al, dx		; read LSR
  test al, 1		; Data Available ?
  jnz irx_more
.end:
  mov word [rxhead], si
;  test al, 20h	; Transmitter Holding Register Empty ?
;  jnz int_tx	; yes, do transmit next -->
	; Sometimes when sending and receiving at the
	; same time, TX ints get lost. This is a cure.
;  retn
  jmp int_tx		; (this checks for THRE)

@@:
  mov dx, [baseport]
  in al, dx		; read RBR (discard)
  db __TEST_IMM16	; (skip mov)
@@:
  mov si, ax
  add dx, 5
  in al, dx		; read LSR
  test al, 1		; Data Available ?
  jnz @BB
  jmp .end


int_status:
  ; just clear the status ("this trivial task is left as an exercise
  ; to the student")
  add dx, 5
  in al, dx		; read LSR
  retn


	usesection lDEBUG_CODE

	; OUT:	ZR if no new character in buffer
	;	NZ if new character read,
	;	 al = character
	; STT:	ds = debugger segment
serial_receive_char:
	push si
		; see if there are bytes to be read from the fifo
	mov si, word [rxtail]

	cmp si, word [rxhead]
	je .nodata
	lodsb
%if _ECHO_RX_TO_TX
 %if _RX_TO_TX_ADD_LF
	call serial_send_char_add_lf
 %else
	call serial_send_char
 %endif
%endif
		; check for wrap-around
	rx_checkwrap
	mov  word [rxtail], si
	test si, si	; (NZ)
	jmp .return

.nodata:
	xor ax, ax	; (ZR)
.return:
	pop si
	retn


	; OUT:	ZR if no new character in buffer
	;	NZ if new character available,
	;	 al = character
	; STT:	ds = debugger segment
serial_check_receive_char:
	push si
		; see if there are bytes to be read from the fifo
	mov si, word [rxtail]

	cmp si, word [rxhead]
	je .nodata
	lodsb
	test si, si	; (NZ)
	jmp .return

.nodata:
	xor ax, ax	; (ZR)
.return:
	pop si
	retn


		; OUT:	NC if successful
		;	CY if handler hooked in different interrrupt
		;	 and couldn't unhook
serial_install_interrupt_handler:
  ; install interrupt handler first
	mov al, byte [serial_use_intnum]
	testopt [internalflags4], dif4_int_serial_hooked
	jz @F
	cmp al, byte [serial_installed_intnum]
	je .ret			; --> (NC)
	push ax
	call serial_uninstall_interrupt_handler
	pop ax
	jc .ret			; --> (CY)
@@:
	mov byte [serial_installed_intnum], al
	mov si, serial_interrupt_handler
	call install_86m_interrupt_handler
	setopt [internalflags4], dif4_int_serial_hooked
	call update_inttab_optional
				; (NC)
.ret:
	retn


%if 0

If you do the following:

r dspvi FF
r dco or= 4000
(wait for KEEP prompt to fail)
r dspvi 0B
r dco or= 4000
(try to reply to the KEEP prompt)

In dosemu2 the default interrupt handler apparently
doesn't send an EOI to the PIC and thus the interrupts
get stuck when prompting with the correct handler.
Therefore, we should send an EOI to the PIC just in case.

%endif

		; INP:	word [serial_use_irqmask]
		; CHG:	ax
serial_eoi:
  mov al, 20h	; acknowledge interrupt
  cmp byte [serial_use_irqmask + 1], 0
  je @F
  out 0A0h, al	; to secondary PIC
@@:
  out 20h, al	; to primary PIC
  retn


serial_clear_fifos:
  ; clear fifos (not those in the 16550A, but ours)
  mov  ax, rxfifo
  mov  word [rxhead], ax
  mov  word [rxtail], ax
  mov  ax, txfifo
  mov  word [txhead], ax
  mov  word [txtail], ax
  retn


	numdef SERIAL_DL_WORD, 0

serial_init_UART:
  call serial_eoi
  ; initialize the UART
  mov dx, [baseport]
  add dx, 3		; (base + 3) read/write LCR
  in al, dx		; read LCR
  mov byte [serial_save_lcr], al
  mov al, 80h		; DLAB = 1
  out dx, al		; write LCR, make DL register accessible
  push dx
  mov dx, [baseport]	; (base)
%if _SERIAL_DL_WORD
  in ax, dx		; read bps rate divisor (DL)
  mov word [serial_save_dl], ax
  mov ax, word [serial_use_dl]
  out dx, ax		; write bps rate divisor (DL)
%else
  in al, dx		; read bps rate divisor low byte (DL)
  inc dx
  mov byte [serial_save_dl], al
  in al, dx		; read bps rate divisor high byte (DL)
  mov byte [serial_save_dl + 1], al
  mov al, byte [serial_use_dl + 1]
  out dx, al		; write bps rate divisor high byte (DL)
  dec dx
  mov al, byte [serial_use_dl]
  out dx, al		; write bps rate divisor low byte (DL)
%endif
  pop dx		; (base + 3) write LCR
  mov al, byte [serial_use_params]
			; DLAB = 0 and control parameters
  out dx, al		; write parameters

  ; is it a 16550A?
  dec dx		; (base + 2) write FCR, read IIR
%if _USE_TX_FIFO
  mov al, 0000_0111b
  or al, byte [serial_use_fifo]
  mov byte [serial_fcr_setting], al
  out dx, al			; (write FCR) try to clear and enable FIFOs
  nop
  in al, dx			; read IIR
  or byte [serial_flags], sf_built_in_fifo
				; in case of built-in tx FIFO
  and al, 1100_0000b		; mask of FIFO functional bits
  cmp al, 1100_0000b		; both bits set ?
  je @F				; yes -->
  and byte [serial_flags], ~ sf_built_in_fifo
				; no built-in tx FIFO
  xor ax, ax
  mov byte [serial_fcr_setting], al
  out dx, al			; (write FCR) disable the FIFOs
@@:
%else
  xor ax, ax
  out dx, al			; (write FCR) disable the FIFOs
%endif
  dec dx		; (base + 1)
  in al, dx		; read IER
  mov byte [serial_save_ier], al
  mov al, 0000_0001b	; allow RX interrupts
  out dx, al		; write to IER
  dec dx		; (base + 0) read RBR
  in al, dx		; clear receiver
  add dx, 5		; (base + 5) read LSR
  in al, dx		; clear line status
  inc dx		; (base + 6) read MSR
  in al, dx		; clear modem status
	; free interrupt in the ICU
  mov cx, word [serial_use_irqmask]
  not cx		; negated mask of bits to change
  xor bx, bx		; all bits clear (= IRQ ON)
  call set_irq
  mov word [serial_save_irq_off], bx
  mov word [serial_save_irq_mask], cx
 	; and enable ints from the UART
  dec dx
  dec dx		; (base + 4)
  in al, dx		; read MCR
  mov byte [serial_save_mcr], al
  mov al, 0000_1000b
  out dx, al		; write MCR
  retn

		; INP:	cx = negated mask of bits to change
		;	 (if bit is clear, modify corresponding IRQ)
		;	bx = mask of what to set bits to (0 = IRQ ON, 1 = IRQ OFF)
		; OUT:	bx = mask of bits previously set
		; CHG:	ax
set_irq:
  push dx
  mov dx, 1
.loop:
  test dx, cx
  jnz .next
  test dl, dl
  jz .high

.low:
  in al, 21h		; get PIC configuration
   push ax
  not dx		; dx = mask of bits to keep
  and al, dl		; mask to 0 the bit to set
    push bx
  not dx		; dx = mask of bits to change
  and bl, dl		; get bit state to change to
  or al, bl		; set this bit state
  out 21h, al		; configure the PIC
    pop bx		; = saved states / still to set states
  not dx		; dx = mask of bits to keep
   pop ax		; = prior config
  and bl, dl		; clear bits to change
  not dx		; dx = mask of bits to change
  and al, dl		; separate out only bits to change
  or bl, al		; set in bx
  jmp .next

.high:
  in al, 0A1h		; get PIC configuration
   push ax
  not dx		; dx = mask of bits to keep
  and al, dh		; mask to 0 the bit to set
    push bx
  not dx		; dx = mask of bits to change
  and bh, dh		; get bit state to change to
  or al, bh		; set this bit state
  out 0A1h, al		; configure the PIC
    pop bx		; = saved states / still to set states
  not dx		; dx = mask of bits to keep
   pop ax		; = prior config
  and bh, dh		; clear bits to change
  not dx		; dx = mask of bits to change
  and al, dh		; separate out only bits to change
  or bh, al		; set in bx

.next:
  add dx, dx
  jnz .loop
  pop dx
  retn


		; OUT:	NC if successful
		;	CY if couldn't unhook
serial_clean_up:
  call serial_eoi
  xor ax, ax
  mov dx, [baseport]
  add dx, 4		; (base + 4)
			; disconnect the UART from the int line
  out dx, al		; write MCR
  dec dx
  dec dx
  dec dx		; (base + 1) disable UART ints
  out dx, al		; write IER
  inc dx		; (base + 2)
			; disable the FIFOs (old software relies on it)
  out dx, al		; write FCR

  ; reset the UART
  mov dx, [baseport]
  add dx, 3		; (base + 3) read/write LCR
  mov al, 80h		; DLAB = 1
  out dx, al		; write LCR, make DL register accessible
  push dx
  mov dx, [baseport]	; (base)
%if _SERIAL_DL_WORD
  mov ax, word [serial_save_dl]
  out dx, ax		; write bps rate divisor (DL)
%else
  mov al, byte [serial_save_dl]
  out dx, al		; write bps rate divisor low byte (DL)
  inc dx
  mov al, byte [serial_save_dl + 1]
  out dx, al		; write bps rate divisor high byte (DL)
%endif
  pop dx		; (base + 3) write LCR
  mov al, byte [serial_save_lcr]
  out dx, al		; write parameters

  dec dx		; (base + 2) write FCR, read IIR
  xor ax, ax
  out dx, al		; (write FCR) disable the FIFOs
  dec dx		; (base + 1)
  mov al, byte [serial_save_ier]
  out dx, al		; write to IER
  mov bx, [serial_save_irq_off]
			; bits clear for IRQ ON
  mov cx, [serial_save_irq_mask]
			; negated mask of bits to change
  call set_irq
  inc dx
  inc dx
  inc dx		; (base + 4)
  mov al, byte [serial_save_mcr]
  out dx, al		; write MCR

  ; restore int vector
		; OUT:	NC if successful
		;	CY if couldn't unhook
serial_uninstall_interrupt_handler:
	mov si, serial_interrupt_handler
	mov al, byte [serial_installed_intnum]
	mov dx, opt4_int_serial_force >> 16
	call UnhookInterruptForce
	jc @F
	clropt [internalflags4], dif4_int_serial_hooked
	call update_inttab_optional
				; (NC)
@@:
	retn


serial_send_char_add_lf:
	push ax
.loop:
	call serial_send_char
	cmp al, 13	; add LF after CR; change it if you don't like it
	mov al, 10
	je .loop
	pop ax
	retn

serial_send_char:
  push si
  push cx
  push dx
  push es

  mov  si, word [txhead]
  mov  byte [si],al
  inc  si
  ; check for wrap-around
  tx_checkwrap

  push ax
%if _PM
   push ax
  call push_if
%else
  pushf
%endif
  cmp word [txtail], si
  jne .no_wait

	; Because we enable the tx empty interrupt
	;  when putting data into the buffer, it
	;  should still be enabled here when the
	;  buffer is currently full. So we only
	;  need to wait for the interrupt to
	;  occur and be processed by our handler.

  xor cx, cx
  mov dx, 40h			; 0040h is a bimodal segment/selector
  mov es, dx
.wait_reset_dx:
  mov dx, word [es:6Ch]

.wait:
  call idle
%if _PM
  call pop_if
   push ax
  call push_if
%else
  popf
  pushf
%endif

  cmp si, word [txtail]
  jne .no_wait

%if _SLEEP_NEW
	mov ax, word [es:6Ch]
	cmp dx, ax
	je .wait
	neg dx			; minus prior tick
	add dx, ax		; new tick - prior tick

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
	add cx, dx
	jc @F
%else
  cmp dx, word [es:6Ch]
  je .wait
  inc cx
%endif
  cmp cx, 5 * 18
  jb .wait_reset_dx
@@:

	clropt [options], enable_serial
	mov dx, msg.no_progress
	call putsz
	jmp cmd3

.no_wait:
  mov  word [txhead], si
  cli		; try to avoid interrupt while emptying buffer
  ; test if we can send a byte right away
%if 0		; int_tx checks for THRE ...-
  mov dx, [baseport]
  add dx, 5	; (base + 5)
  in al, dx	; read LSR
  test al, 20h	; Transmitter Holding Register Empty ?
  jz .crank	; no, just enable the interrupt -->
%endif

;  call int_tx	; send bytes, enables or disables the tx interrupt
	push cs
	call code_to_int_tx

%if 0		; -... and sets up the interrupt accordingly
  jmp .dontcrank
.crank:
  ; crank it up
  ; note that this might not work with some very old 8250s
  add dx, 1 - 5	; (base + 1) write IER
  mov al, 0000_0011b
  out dx, al	; enable tx empty interrupt
.dontcrank:
%endif
%if _PM
  call pop_if
%else
  popf
%endif
  pop ax
  pop es
  pop dx
  pop cx
  pop si
  retn

%if _PM
push_if:
	lframe near
	lpar word,	flags
	lpar_return
	lenter
	call ispm
	jnz .86m
.pm:
	push ax
	mov ax, 0902h
	int 31h
	test al, al	; 0 = disabled ?
	mov ax, 0	; initialise to IF=0
	jz @F
	mov ah, 2	; else, IF=1
@@:
	mov word [bp + ?flags], ax
	pop ax
	jmp .end
.86m:
	pushf
	pop word [bp + ?flags]
.end:
	lleave
	lret

pop_if:
	lframe near
	lpar word,	flags
	lenter
	call ispm
	jnz .86m
	push ax
	mov ax, 0900h	; initialise to disable VIF = 0900h
	test byte [bp + ?flags + 1], 2
	jz @F		; if to disable -->
	inc ax		; else enable VIF = 0901h
@@:
	int 31h
	pop ax
	jmp .end
.86m:
	push word [bp + ?flags]
	popf
.end:
	lleave
	lret
%endif

code_to_int_tx:
	push word [cs:.entry_retf_word]
%if _PM
			; near return address
	call ispm
	jnz .rm

	push word [cssel]
	jmp @F

%endif
.rm:
	push ss
@@:
	push word [cs:.int_tx_word]
	retf		; jump to lDEBUG_DATA_ENTRY:int_tx

	align 2, db 0
.int_tx_word:
	dw int_tx
.entry_retf_word:
	dw entry_retf


	usesection lDEBUG_DATA_ENTRY

entry_int3_retf:
	int3
entry_retf:
	retf
