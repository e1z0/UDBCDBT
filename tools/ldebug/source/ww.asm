
%if 0

lDebug W commands (write sector, write program)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

lockdrive:
	push ax
	push bx
	push cx
	push dx
	mov bl, al
	inc bl
	mov bh, 0
	mov cx, 084Ah
	mov dx, 0001h
	mov ax, 440Dh
	int 21h
	pop dx
	pop cx
	pop bx
	pop ax
	retn

unlockdrive:
	push ax
	push bx
	push cx
	push dx
	mov bl, al
	inc bl
	mov bh, 0
	mov cx, 086Ah
	mov dx, 0001h
	mov ax, 440Dh
	int 21h
	pop dx
	pop cx
	pop bx
	pop ax
	retn


		; W command - write a program, or disk sectors, to disk.
ww:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz @F
	mov dx, msg.nobootsupp
	jmp putsz
@@:
%endif

	call parselw		; parse L and W argument format
	jz ww4			; if request to write program
%if _PM && _NOEXTENDER
	call ispm
	jnz .rm
	call isextenderavailable
	jc nodosextinst
.rm:
%endif
	testopt [ss:internalflags], newpacket| ntpacket
	jz .oldint
	mov dl, al		; A: = 0, ...
	mov si, 6001h		; write, assume "file data"
%if _VDD
	testopt [internalflags], ntpacket
	jnz .vdd
%endif
	inc dl			; A: = 1, ...
	call lockdrive
	mov ax, 7305h		; ds:(e)bx-> packet
	stc
	int 21h			; use int 21h here, not doscall
	pushf
	call unlockdrive
	popf
	jmp short .done
%if _VDD
.vdd:
	mov ax, word [hVdd]
	mov cx, 5
%if _PM
	add cl, byte [dpmi32]
%endif
	DispatchCall
	jmp short .done
%endif
.oldint:
	int 26h
.done:
	mov dx, writing
ww1:
	mov bx, ss		; restore segment registers
	mov ds, bx
	mov sp, word [savesp]
	mov es, bx
	jnc ww3			; if no error
	cmp al, 0Ch
	jbe ww2			; if in range
	mov al, 0Ch
ww2:
	cbw			; ah = 0
	mov bx, dskerrs		; -> byte table
	xlatb			; get offset from dskerrs
	add ax, bx		; -> message
	mov di, line_out
	mov si, ax
	call showstring
	mov si, dx
	call showstring
	mov si, drive
	call showstring
	call putsline_crlf
ww3:
	jmp cmd3		; can't return because Int26 leaves stack wrong

;	Write to file.  First check the file extension.
;   size of file is in client's BX:CX,
;   default start address is DS:100h

ww4:
	call InDos
	jnz not_while_indos

	mov al, byte [fileext]	; get flags of file extension
	testopt [options6], opt6_flat_binary
	jnz ww5
	test al, EXT_EXE + EXT_HEX
	jz ww5			; if not EXE or HEX
	mov dx, nowhexe
	jmp short ww6

ww5:
	cmp al, 0
	jnz ww7			; if extension exists
	mov dx, nownull
ww6:
	jmp ww16

		; File extension is OK; write it.  First, create the file.
ww7:
%if _PM
	call ispm
	jnz ww7_1
	mov dx, nopmsupp
	jmp putsz
ww7_1:
%endif
	mov bp, line_out
	cmp dh, 0FEh
	jb ww8			; if (dx < 0xFE00)
	sub dh, 0FEh		;  dx -= 0xFE00;
	add bx, 0FE0h
ww8:
	mov word [bp+10], dx	; save lower part of address in line_out+10
	mov si, bx		; upper part goes into si
	mov ah, 3Ch		; create file
	xor cx, cx		; no attributes
	mov dx, DTA
	doscall
	jc ww15			; if error
	push ax			; save file handle

		; Print message about writing.
	mov dx, wwmsg1
	call putsz		; print string
	mov ax, word [reg_ebx]
	cmp ax, 10h
	jb ww9			; if not too large
	xor ax, ax		; too large:  zero it out
ww9:
	mov word [bp+8], ax
	test ax, ax
	jz ww10
	call hexnyb
ww10:
	mov ax, word [reg_ecx]
	mov word [bp+6], ax
	call hexword
	call putsline		; print size
	mov dx, wwmsg2
	call putsz		; print string

		; Now write the file.  Size remaining is in line_out+6.
	pop bx			; recover file handle
	mov dx, word [bp+10]	; address to write from is si:dx
ww11:
	mov ax, 0FE00h
	sub ax, dx
	cmp byte [bp+8], 0
	jnz ww12		; if more than 0FE00h bytes remaining
	cmp ax, word [bp+6]
	jb ww12			; ditto
	mov ax, word [bp+6]
ww12:
	xchg ax, cx		; mov cx, ax
	mov ds, si
	mov ah, 40h		; write to file
	int 21h			; use INT, not doscall
	push ss			; restore DS
	pop ds
	cmp ax, cx
	jne ww13		; if disk full
	xor dx, dx		; next time write from xxxx:0
	add si, 0FE0h		; update segment pointer
	sub word [bp+6], cx
	lahf
	sbb byte [bp+8], 0
	jnz ww11		; if more to go
	sahf
	jnz ww11		; ditto
	jmp short ww14		; done

ww13:
	mov dx, diskful
	call putsz		; print string
	call ww14		; close file

	mov ah, 41h		; unlink file
	mov dx, DTA
	doscall
	retn

		; Close the file.
ww14:
	mov ah, 3Eh		; close file
	int 21h
	retn

		; Error opening file.  This is also called by the load command.
ww15:
	cmp ax, byte 2
	mov dx, doserr2		; File not found
	je ww16
	cmp ax, byte 3
	mov dx, doserr3		; Path not found
	je ww16
	cmp ax, byte 5
	mov dx, doserr5		; Access denied
	je ww16
	cmp ax, byte 8
	mov dx, doserr8		; Insufficient memory
	je ww16
	cmp ax, byte 11
	mov dx, doserr11	; Invalid format
	je ww16
	mov di, openerr1
	call hexword
	mov dx, openerr		; Error ____ opening file
ww16:
	jmp putsz
