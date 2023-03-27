
%if 0

lDebug code and command (V) to flip video screens

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


%if _VXCHG

;--- show debuggee screen, wait for a keypress, then restore debugger screen

vv:
	dec si
	mov dx, msg.on
	call isstring?
	jne @F
vv_on:
	lodsb
	call chkeol
	setopt [options6], opt6_vv_mode
	retn

@@:
	mov dx, msg.off
	call isstring?
	jne @F
vv_off:
	call skipwhite
	dec si
	mov dx, msg.keep
	call isstring?
	jne .notkeep
	setopt [options6], opt6_vv_keep
.notkeep:
	call skipwhite
	dec si
	mov dx, msg.nokeep
	call isstring?
	jne .notnokeep
	clropt [options6], opt6_vv_keep
.notnokeep:
	lodsb
	call chkeol
	clropt [options6], opt6_vv_mode
	retn

@@:
	lodsb
	call chkeol

	testopt [internalflags6], dif6_vv_mode
	jnz @F
	mov dx, msg.vv_disabled
	jmp putsz

@@:
	mov al, 0
	call swapscreen
%if 0 ;n _VXCHGBIOS	; no longer needed, swapscreen has set cursor

;--- swapscreen has restored screen and cursor pos, but we want
;--- the cursor be shown on the screen - so set it through BIOS calls.

	mov ah, 0Fh	; get current mode (and video page in BH)
 	int 10h
	mov ah, 3	; get cursor pos of page in BH
	int 10h
	mov ah, 2	; set cursor pos of page in BH
	int 10h
%endif

	testopt [options6], opt6_vv_int16
	jnz .int16
.terminal:
	setopt [internalflags3], dif3_input_terminal_override
				; make sure we get terminal input
	call getc
	clropt [internalflags3], dif3_input_terminal_override
	jmp @F

.int16:
	; mov ah, 10h
	xor ax, ax
	int 16h
@@:

	mov al, 1
	call swapscreen
	retn


;--- AL=0: save debugger screen, restore debuggee screen
;--- AL=1: save debuggee screen, restore debugger screen

swapscreen:
	testopt [internalflags6], dif6_vv_mode
	jz .done

%ifn _VXCHGBIOS
	mov si, xmsmove
	cmp word [si + XMSM.dsthdl], 0
	jz .done

	mov cl, 14
	shl ax, cl	; 0 -> 0000, 1 -> 4000h
	mov word [si + XMSM.dstadr], ax

;--- use offset & size of current video page as src/dst for
;--- xms block move. Also toggle cursor pos debuggee/debugger.

	mov ax, 40h		; bimodal selector/segment
	mov es, ax
	mov ax, [es:4Ch]
	mov word [si + XMSM.size_], ax
	mov ax, [es:4Eh]
	mov word [si + XMSM.srcadr + 0], ax

;--- get/set cursor position manually for speed reasons.
	mov bl, [es:62h]
	mov bh, 0
	shl bx, 1
	mov dx, [es:bx + 50h]	; get cursor position of current page
 	xchg dx, [csrpos]
 %if 0
	mov [es:bx + 50h], dx
 %else
	mov bh, byte [es:62h]
	mov ah, 2
	int 10h
 %endif

	mov ah, 0Bh		; save video screen to XMS
	call runxms
	call swapsrcdst
	xor byte [si + XMSM.srcadr + 1], 40h
	mov ah, 0Bh		; restore video screen from XMS
	call runxms
	call swapsrcdst
;	xor byte [si + XMSM.dstadr + 1], 40h
%else
	mov ah, 05h		; just use BIOS to activate video page
	int 10h
%endif
.done:
	retn

%ifn _VXCHGBIOS
swapsrcdst:
	mov ax, [si + XMSM.srchdl]
	mov cx, word [si + XMSM.srcadr + 0]
	mov dx, word [si + XMSM.srcadr + 2]
	xchg ax, [si + XMSM.dsthdl]
	xchg cx, word [si + XMSM.dstadr + 0]
	xchg dx, word [si + XMSM.dstadr + 2]
	mov [si + XMSM.srchdl], ax
	mov word [si + XMSM.srcadr + 0], cx
	mov word [si + XMSM.srcadr + 2], dx
	retn
runxms:
	push ds
	pop es
%if _PM
	call ispm
	jz @F
%endif
	call far [xmsdrv]
	retn
%if _PM
@@:
%ifn _ONLYNON386
_no386	jmp .286
subcpu 386
	push dword 0	; ss:sp
	push dword [xmsdrv]
			; cs:ip
	push dword 0	; fs,gs
	push word [pspdbg]
			; ds
	push word 0	; es
	pushf
	pushad
	mov edi, esp
	xor cx, cx
	mov bh, 0
	mov ax, 0301h
	int 31h
	popad
	add sp, 50 - 32	; sizeof RMCS - 32
			; ie discard fl, segregs, cs:ip, ss:sp
	retn
subcpureset
%endif
%ifn _ONLY386
.286:
subcpu 286
	push word 0
	push word 0	; ss:sp
	push word [xmsdrv + 2]
	push word [xmsdrv]
			; cs:ip
	push word 0
	push word 0	; fs,gs
	push word [pspdbg]
			; ds
	push word 0	; es
	pushf
	push word 0
	push ax
	push word 0
	push cx
	push word 0
	push dx
	push word 0
	push bx
	push word 0
	push word 0
	push word 0	; esp
	push bp
	push word 0
	push si
	push word 0
	push di
	mov di, sp
	xor cx, cx
	mov bh, 0
	mov ax, 0301h
	int 31h
	pop di
	pop ax
	pop si
	pop ax
	pop bp
	pop ax
	pop ax		; esp
	pop ax
	pop bx
	pop ax
	pop dx
	pop ax
	pop cx
	pop ax
	pop ax		; ax
	add sp, 50 - 32 + 2 	; sizeof RMCS - 32 + 2
			; ie discard eaxh and fl, segregs, cs:ip, ss:sp
	retn
subcpureset
%endif
%endif
%endif

vv_set:
	testopt [options6], opt6_vv_mode
	jnz .checkenable

.checkdisable:
	testopt [internalflags6], dif6_vv_mode
	jnz vv_disable
	retn

.checkenable:
	testopt [internalflags6], dif6_vv_mode
	jz vv_enable
.ret:
	retn

vv_enable:
	mov al, 2Fh
	call intchk
	jz .noxmm

%ifn _VXCHGBIOS
	mov ax, 4300h		; check if XMM is here
%if _PM
	call .call2F
%else
	int 2Fh
%endif
	cmp al, 80h
	jnz .noxmm		; no - no screen flip
	mov ax, 4310h
%if _PM
	call .call2F
	mov word [xmsdrv + 0], bx
	mov word [xmsdrv + 2], dx
%else
	int 2Fh
	mov word [xmsdrv + 0], bx
	mov word [xmsdrv + 2], es
%endif

	mov dx, 32		; alloc 32 KiB EMB
	mov ah, 9
	call runxms
	cmp ax, 1
	jnz .noxmm
	mov si, xmsmove
	mov [si + XMSM.dsthdl], dx	; save the handle in block move struct.
	mov byte [si + XMSM.dstadr + 1], 40h
					; the XMS memory will be used to
					; save/restore 2 screens, with a max
					; capacity per screen of 16 KiB
	mov ax, 40h			; bi-modal segment/selector
	mov es, ax
	mov ax, [es:4Ch]		; current screen size, might change!
	mov word [si + XMSM.size_], ax
	mov ax, [es:4Eh]		; page start in video memory
	mov word [si + XMSM.srcadr + 0], ax
	mov ax, 0B000h
	cmp byte [es:463h], 0B4h
	je @F
	or ah, 8
@@:
	mov word [si + XMSM.srcadr + 2], ax

	mov ah, 0Fh			; get active video page in BH
	int 10h
	mov ah, 03h			; get cursor pos in DX of active page
	int 10h
	mov [csrpos], dx
	mov al, [es:84h]
	mov [vrows], al
	mov ah, 0Bh			; save current screen now
	call runxms
%else
;--- use BIOS to swap page 0/1, a simple approach
;--- that in theory would fit perfectly, but
;--- unfortunately in reality may have quirks.
	mov ax, 40h
	mov es, ax
	mov si, [es:4Eh]
	mov cx, [es:4Ch]
	shr cx, 1
	mov ax, 0501h			; debugger page is 1
	int 10h
	mov di, [es:4Eh]
	mov dx, [es:50h+0*2]
	mov [es:50h+1*2], dx
	mov ax, 0B000h
	cmp byte [es:63h], 0B4h
	jz @F
	or ah, 8
@@:
	mov es, ax
	push ds
	mov ds, ax
	rep movsw
	pop ds
%endif
	setopt [internalflags6], dif6_vv_mode
	push ds
	pop es
	retn

.noxmm:
	clropt [options6], opt6_vv_mode
	push ds
	pop es
	mov dx, msg.vv_enable_failure
	jmp putsz

%if _PM
.call2F:
	call ispm
	jz @F
	push es
	int 2Fh
	mov dx, es
	pop es
	retn

@@:
subcpu 286
	push word [pspdbg]
	push word [pspdbg]
	push word 2Fh
	push bp
	call intcall_return_parameter_es_parameter_ds
	pop dx		; discard returned ds
	pop dx		; get es
subcpureset
	retn
%endif

vv_disable:
	testopt [internalflags6], dif6_vv_mode
	jz .ret
%ifn _VXCHGBIOS
	mov dx, word [xmsmove + XMSM.dsthdl]
	test dx, dx
	jz @F
%endif
	testopt [options6], opt6_vv_keep
	jnz .keep
	push dx
	mov al, 0		; restore debuggee screen
	call swapscreen
	pop dx
.keep:
%ifn _VXCHGBIOS
	mov ah, 0Ah		; and free XMS handle
	call runxms
	xor ax, ax
	mov word [xmsmove + XMSM.dsthdl], ax
	mov word [xmsmove + XMSM.srchdl], ax
%endif

@@:
	clropt [internalflags6], dif6_vv_mode
.ret:
	retn
%else

vv equ error

%endif
