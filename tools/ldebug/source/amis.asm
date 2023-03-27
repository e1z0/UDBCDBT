
%if 0

lDebug AMIS interface

Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

	align 2, db 0
debuggerfunction:	dw 0

%if ! _CATCHINT2D
	align 2, db 0
debuggeramissig:
.ven:	fill 8,32,db "ecm"		; vendor
.prod:	fill 8,32,db "lDebug"		; product
	db 0, 0

try_debugger_amis_multiplex_number:
	db -1
%else
%if 0

Supported Int2D functions:

AMIS - Installation check
INP:	al = 00h
OUT:	al = 0FFh
	cx = Private version number (currently 0100h)
	dx:di-> signature: "ecm     ", "lDebug  "

AMIS - Get private entry point - NOP: no private entry point
INP:	al = 01h
OUT:	al = 00h

AMIS - Uninstall - NOP: can't uninstall
INP:	al = 02h
OUT:	al = 00h (not implemented)

AMIS - Request pop-up - NOP: no pop-up
INP:	al = 03h
OUT:	al = 00h

AMIS - Determine chained interrupts
INP:	al = 04h
OUT:	al = 04h
	dx:bx -> interrupt hook list (Int2D always.)

AMIS - Get hotkeys - NOP: no hotkeys
INP:	al = 05h
OUT:	al = 00h

AMIS - Get device driver information
INP:	al = 06h
OUT:	al = 00h if not device mode
	al = 01h to indicate one device,
	 ah = device flags = 01h
	  (01h set = installed from CONFIG.SYS,
	   02h clear = device is linked into DOS device chain,
	   04h clear = inreentrant device)
	 dx:bx -> device header

AMIS - Reserved for AMIS
INP:	al = 07h..0Fh
OUT:	al = 00h

TSR - Reserved for TSR
INP:	al = 10h..2Fh
OUT:	al = 00h

lDebug - Update IISP Header
INP:	al = 30h
	ds:si -> source IISP header (or pseudo header)
	es:di -> destination IISP header
OUT:	al = FFh to indicate suppported,
	 si and di both incremented by 6
	 destination's ieNext field updated from source
	al != FFh if not supported,
	 si and di unchanged
CHG:	-
REM:	This function is intended to aid in debugging
	 handler re-ordering, removal, or insertion.
	 The 32-bit far pointer needs to be updated
	 as atomically as possible to avoid using
	 an incorrect pointer.
	Test case: Run a program such as our TSRs'
	 uninstaller or SHUFHOOK and step through it
	 with "tp fffff" when operating on something
	 crucial such as interrupt 21h. Without this
	 function the machine will crash!
	To enable this function to be called, first run
	 the command "r dco4 or= 8", or "INSTALL AMIS"
	 (install our AMIS multiplexer handler).
	Other workaround: Use SILENT for TP and disable
	 DCO3 flag 4000_0000 (do not call int 21.0B to
	 check for Ctrl-C status).
	Yet another workaround: Set flag DCO 8 (enable
	 fake InDOS mode, avoid calling int 21h).
REM:	The source may be a pseudo IISP header. In this
	 case the ieEntry field should hold 0FEEBh
	 (jmp short $) and the ieSignature field
	 should indicate the source, eg "VT" for the IVT
	 or "NH" for inserting a New Handler.

lDebugX - Install DPMI hook
INP:	al = 31h
OUT:	al = FFh if installed
	al = FEh..F0h if not installed but call is supported
	al = 00h if not supported
CHG:	-
STT:	not in DOS

lDebugX - Reserved
INP:	al = 32h

lDebugX - Install fault areas
INP:	al = 33h
	dx:bx -> fault area structure of client
OUT:	al = FFh if installed
	al = FEh..01h if not installed but call is supported
	al = 00h if not supported
CHG:	al, bx, cx, dx, si, di, es, ds
REM:	The area structure is defined in the lDebug sources'
	 debug.mac file. The first 32 bytes of the structure
	 start with a signature word, which is equal to the
	 word value CBF9h (encoding the instruction sequence
	 of stc \ retf) if the structure is not currently
	 installed into any debugger. The remainder of the
	 32 bytes, as well as the details of how the first
	 two bytes are used otherwise, are private to the
	 debugger that provides this service (the server).
	The area structure may be far-called in 86 Mode. The
	 only currently defined function (in al) for this call
	 is function 00h, which attempts to uninstall the area
	 structure which is being called. It is valid for
	 either the server or the client to uninstall an
	 area structure if they so wish.
	The fields of the structure behind the first 32 bytes
	 point to a number of sub-structures and area function
	 lists and area lists. All of these structures are
	 to be accessed using the same segment as the main
	 area structure. They contain linear start and linear
	 end addresses, which the client sets up before it
	 tries to install the areas. The linear start address
	 is also assumed to point to the segment base address
	 which is used as the reference for the area functions
	 and areas. (They do not have to match the offset part
	 actually used to run the code, but the lists must be
	 based on the linear start address.)

TSR - Reserved for TSR
INP:	al = 34h..FFh
OUT:	al = 00h

%endif

	align 2, db 0
debuggeramissig:
amissig:
.ven:	fill 8,32,db "ecm"		; vendor
.prod:	fill 8,32,db "lDebug"		; product
.desc:	asciz _PROGNAME,_VERSION,", debugger."
					; description
%if $ - .desc > 64
 %error AMIS description too long
%endif

try_amis_multiplex_number:
	db 0
try_debugger_amis_multiplex_number:
	db -1


iispentry int2D
	cmp ah, 0			; magic bytes, used by ecm RENUMBER
amis_multiplex_number equ $-1		; AMIS multiplex number (data for cmp opcode)
		; SMC in section lDEBUG_DATA_ENTRY
	je .handle			; our multiplex number -->
	jmp far [cs:.next]		; else go to next handler -->

.handle:
	test al, al
	jz .installationcheck		; installation check -->
	; cmp al, 02h
	; je .uninstall			; uninstallation -->
	cmp al, 04h
	je .determineinterrupts		; determine hooked interrupts -->
%if _DEVICE
	cmp al, 06h
	je .getdevice
%endif
	cmp al, 30h
	je .updateiispheader
%if _PM
	cmp al, 31h
	je .installdpmihook
%endif
%if _AREAS_HOOK_SERVER
	cmp al, 33h
	je .installareas
%endif
				; all other functions are reserved or not supported by TSR
.uninstall:
.nop:
	mov al, 0			; show not implemented
	iret

.installationcheck:
	dec al				; (= FFh) show we're here
	mov cx, 0100h			; = version
	mov di, amissig			; dx:di -> AMIS signature strings of this program
.iret_dx_cs:
	mov dx, cs
.iret:
	iret

.determineinterrupts:			; al = 04h, always returns list
	mov bx, word [cs:amisintr_offset]
					; dx:bx -> hooked interrupts list
	jmp short .iret_dx_cs

%if _DEVICE
.getdevice:
	testopt [cs:internalflags6], dif6_device_mode
	jz .nop
	mov ax, 0101h			; CONFIG.SYS, one device
	xor bx, bx
	mov dx, cs
	sub dx, paras(deviceshim_size + 10h)
					; -> device header
	iret
%endif

.updateiispheader:
	mov al, 0FFh			; show supported
	cld
	cli				; try to rest while updating chain
	cmpsw				; skip over first word (entrypoint)
					;  (generally xxEBh or 0EA90h)
	movsw
	movsw				; transfer source ieNext to dest ieNext
	iret

%if _PM
.installdpmihook:
	push bx
	push ax
	push cx
	push dx
	push di
	push es
	push ds
	 push cs
	 pop ds
	setopt [internalflags6], dif6_in_amis_hook2F
	push cs
	call .installdpmitocode
	clropt [internalflags6], dif6_in_amis_hook2F
	pop ds
	pop es
	pop di
	pop dx
	pop cx
	pop bx
	mov ah, bh
	pop bx
	iret

.installdpmitocode:
	call entry_to_code_seg
	dw .installdpmicode

	usesection lDEBUG_CODE
.installdpmicode:
	call hook2F.not_in_dos
		; CHG:	bx, cx, dx, di, es
		; STT:	V86/RM
		;	ds = debugger data segment
	retf
%endif


%if _AREAS_HOOK_SERVER
	usesection lDEBUG_DATA_ENTRY
.installareas:
	 push ax
	push cs
	pop ds
	mov es, dx
	cmp word [es:bx + areastrucEntry], 0CBF9h
	mov cl, 1
	jne .areasend

		; Set new prev = old last
	mov ax, word [ddebugareas.prev]
	mov word [es:bx + areastrucPrev], ax
	mov ax, word [ddebugareas.prev + 2]
	mov word [es:bx + areastrucPrev + 2], ax

		; Set old last's next to new
	lds si, [ddebugareas.prev]
	mov word [si + areastrucNext], bx
	mov word [si + areastrucNext + 2], dx

		; Set new's next to entry
	mov word [es:bx + areastrucNext], ddebugareas.
	mov word [es:bx + areastrucNext + 2], cs

		; Set entry's prev to new
	push cs
	pop ds
	mov word [ddebugareas.prev], bx
	mov word [ddebugareas.prev + 2], dx

	mov di, bx
	mov si, .areascode
	mov cx, words(.areascodelength)
	cld
	rep movsw

	mov cl, 0FFh
.areasend:
	 pop ax
	mov al, cl
	iret


	align 2, db 0
.areascode:
	call 0:areastruc_entry
..@patch_entry_seg: equ $ - 2
.areasaftercall:
	nop

	align 2, nop
.areascodelength: equ $ - .areascode

	align 2, db 0
ddebugareas:
 istruc AREASTRUC
.:
  at areastrucNext
.next:	dw ., 0
  at areastrucPrev
.prev:	dw ., 0
 iend


		; INP;	al = function number (up to 7Fh)
		;	dword [ss:sp] -> after far call 16:16 of areas struc
		;	dword [ss:sp + 4] = far return address to caller
		; OUT:	al = 0FFh if success
		;	al = 80h if function not supported
		;	al = 80h..0FEh if supported but error
		;	al = unchanged INP:al if not supported
		;		(probably area struc not installed)
		; CHG:	ax, bx, cx, dx, si, di, ds, es
areastruc_entry:
	pop bx
	pop dx
	sub bx, int2D.areasaftercall - int2D.areascode
.qq_entry:
	cmp al, 0
	je .uninstall
	stc
	mov al, 80h
	retf


		; INP:	al = 00h
		;	dx:bx -> areas struc
		; OUT:	al = 0FFh if success
		;	al = 80h if function not supported
		;	al = 81h if your area struc not found in our list
		;	al = 82h if your prev does not match our next to you
		;	al = 83h if your next's prev does not match you
		;	al = 84h if your area struc not found and malformed list
		;	al = 80h..0FEh if supported but error
		;	al = unchanged INP:al if not supported
		; CHG:	ax, bx, cx, dx, si, di, ds, es
.uninstall:
	mov cx, cs
	mov ds, cx
	mov si, ddebugareas

	xor di, di
.uninstall_loop:
	dec di
	mov al, 84h
	jz .uninstall_error
	cmp word [si + areastrucNext], bx
	jne .uninstall_next
	cmp word [si + areastrucNext + 2], dx
	je .uninstall_found
.uninstall_next:
	lds si, [si + areastrucNext]
	mov ax, ds
	cmp ax, cx
	jne .uninstall_loop
	cmp si, ddebugareas
	jne .uninstall_loop
	mov al, 81h
	db __TEST_IMM16
.uninstall_error_2:
	mov al, 82h
.uninstall_error:
	stc
	retf

.uninstall_found:
	mov es, dx
	cmp word [es:bx + areastrucPrev], si
	jne .uninstall_error_2
	mov ax, ds
	cmp word [es:bx + areastrucPrev + 2], ax
	jne .uninstall_error_2

	push ds
	push si
	lds si, [es:bx + areastrucNext]
	cmp word [si + areastrucPrev], bx
	jne @F
	cmp word [si + areastrucPrev + 2], dx
	pop si
	pop ds
@@:
	mov al, 83h
	jne .uninstall_error

	mov ax, word [es:bx + areastrucNext]
	mov word [si + areastrucNext], ax
	mov ax, word [es:bx + areastrucNext + 2]
	mov word [si + areastrucNext + 2], ax

	lds si, [es:bx + areastrucNext]
	mov ax, word [es:bx + areastrucPrev]
	mov word [si + areastrucPrev], ax
	mov ax, word [es:bx + areastrucPrev + 2]
	mov word [si + areastrucPrev + 2], ax

	mov al, 0FFh
	clc
	mov word [es:bx + areastrucEntry], 0CBF9h
	retf
%endif	; _AREAS_HOOK_SERVER

%endif	; _CATCHINT2D
