
%if 0

lDebug L commands (load sector, load program)

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_CODE

		; L command - read a program, or disk sectors, from disk.
ll:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz @F
	mov dx, msg.nobootsupp
	jmp putsz
@@:
%endif

	call parselw		; parse L and W argument format
	jz ll1			; if request to read program
%if _PM && _NOEXTENDER
	call ispm
	jnz .rm
	call isextenderavailable
	jc nodosextinst
.rm:
%endif
	testopt [ss:internalflags], newpacket| ntpacket
	jz .oldint
	mov dl, al		; zero-based drive
	mov si, 6000h		; read, assume "file data"
%if _VDD
	testopt [internalflags], ntpacket
	jnz .vdd
%endif
	inc dl			; one-based drive
	mov ax, 7305h		; ds:(e)bx-> packet
	stc
	int 21h			; use int 21h here, not doscall
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
	int 25h
.done:
	mov dx, reading
	jmp ww1

		; For .COM or .EXE files, we can only load at cs:100.  Check that first.
ll1:
	call InDos
	jnz not_while_indos
	call guard_re
	testopt [options6], opt6_flat_binary
	jnz ll4
	test byte [fileext], EXT_COM| EXT_EXE
	jz ll4			; if not .COM or .EXE file
	cmp bx, word [reg_cs]
	jne ll2			; if segment is wrong
	cmp dx, 100h
	je ll4			; if address is OK (or not given)
ll2:
	jmp error		; can only load .COM or .EXE at cs:100

not_while_indos:
	mov ax, 0105h
	call setrc
	mov dx, msg.not_while_indos
	jmp putsz_error


		; load (any) file (if not .EXE or .COM, load at BX:DX)
ll3:
	cmp byte [fileext], 0
	jne ll4
	retn

		; open file and get length
ll4:
	mov si, bx		; save destination address, segment
	mov di, dx		; and offset
	mov ax, 3D00h		; open file for reading
	mov dx, DTA
	doscall
	jc ll16			; error
	xchg ax, bx		; mov bx, ax
	mov ax, 4202h		; lseek
	xor cx, cx
	xor dx, dx
	int 21h

;	Split off file types
;	At this point:
;		bx	file handle
;		dx:ax	file length
;		si:di	load address (CS:100h for .EXE or .COM)

	testopt [options6], opt6_flat_binary
	jnz @F
	test byte [fileext], EXT_COM | EXT_EXE
	jnz ll13		; if .COM or .EXE file
@@:

%if _PM
;--- dont load a file in protected mode,
;--- the read loop makes some segment register arithmetic
	call ispm
	jnz .rm
	mov dx, nopmsupp
	call putsz
	jmp ll12
.rm:
%endif

		; Load it ourselves.
		; For non-.com/.exe files, we just do a read, and set BX:CX to the
		; number of bytes read.
		;
		; si:di = address where to load

	call ensuredebuggeeloaded	; make sure a debuggee is loaded
	jnz ll9.common			; if have no process -->
				; si:di = preserved if had a process,
				;  else si:di = cs:ip (psp:100h)
				; ? Can we ever get NC, ZR return here ?

	mov es, word [pspdbe]

		; Check the size against available space.
	push	si
	push	bx

	cmp si, word [es:ALASAP]
	pushf
	neg si
	popf
	jae ll6				; if loading past end of mem, allow through ffff
	add si, word [es:ALASAP]	; si = number of paragraphs available
ll6:
	mov cx, 4
	xor bx, bx			; bx:si = amount of paragraphs
ll7:
	shl si, 1
	rcl bx, 1
	loop ll7			; bx:si = amount of bytes from paragraphs
	sub si, di
	sbb bx, cx			; bx:si = amount of bytes left
	jb ll9				; if already we're out of space -->
	cmp bx, dx			; cmp bx:si, dx:ax (compare high word)
	jne @F				; if high word differs -->
	cmp si, ax			; compare low word
@@:
	jae ll10			; if not out of space -->
ll9:
	pop bx				; out of space
	pop si
.common:
	mov dx, doserr8			; not enough memory
	call putsz			; print string
	jmp short ll12			; finally close file -->

ll10:
	pop	bx
	pop	si

;	Store length in registers

; seems a bit unwise to modify registers if a debuggee is running
; but MS DEBUG does it as well

%if 0
	mov cx,[reg_cs]
	cmp cx,[pspdbe]
	jnz .noregmodify
	cmp word [reg_eip], 100h
	jnz .noregmodify
%endif
	mov word [reg_ebx], dx
	mov word [reg_ecx], ax
.noregmodify:

	testopt [options6], opt6_big_stack
	jz .nostacksetup
	mov ax, word [pspdbe]
	dec ax
	mov ds, ax		; => MCB
	mov dx, word [3]	; size in paragraphs
	sub dx, 20h - 1
	add dx, ax
	mov ds, dx		; => our stack
	and word [200h - 2], 0	; put a zero on top
	push ss
	pop ds			; restore debugger segment
	mov word [reg_ss], dx
	mov word [reg_esp], 200h - 2
				; -> at the zero

.nostacksetup:

		; Rewind the file
	mov ax, 4200h		; lseek
	xor cx, cx
	xor dx, dx
	int 21h

	mov dx, 0Fh
	and dx, di
	mov cl, 4
	shr di, cl
	add si, di		; si:dx -> address to read to

		; Loop over chunks to read
ll11:
	mov ah, 3Fh		; read from file into DS:(E)DX
	mov cx, 0FE00h		; read up to this many bytes
	mov ds, si
	int 21h			; ax = how many bytes read

	add si, 0FE0h		; (won't work in protected mode!)
	cmp ax, cx		; read a full chunk ?
	je ll11			; yes, end of file maybe not yet reached -->

		; Close the file and finish up.
ll12:
	mov ah, 3Eh		; close file
	int 21h
	push ss			; restore ds
	pop ds

		; INP:	execblk.cmdline
		;	es => PSP to populate
		; CHG:	si, di, cx
		; STT:	ds = ss
ll_copy_cmdline:
	lds si, [execblk.cmdline]
	mov di, 80h		; es:di -> PSP command line field
	mov cx, di		; counter = 128 bytes
	rep movsb		; copy over
	push ss
	pop ds
	retn			; done


ll13:
		; file is .EXE or .COM

		; Previously: adjust .exe size by 200h (who knows why)
		; ecm: this is wrong. It needs to be adjusted by the header size,
		; which is stored (as number of paragraphs) in the .EXE header.
		; The header size is often 200h, but not always.
	push dx
	push ax

	mov ax, 4200h		; lseek set
	xor cx, cx
	xor dx, dx
	int 21h
	 push ss
	 pop ds

	mov bp, sp
	mov cx, EXEHEADER_size
	sub sp, cx
	mov dx, sp
	mov si, sp
	mov ah, 3Fh
	int 21h

		; Close the file
	push ax
	mov ah, 3Eh		; close file
	int 21h
	pop ax

	cmp ax, cx
	jne .no_exe		; (ax = 0 if empty file)
	cmp word [si + exeSignature], "MZ"
	je @F
	cmp word [si + exeSignature], "ZM"
	jne .no_exe
@@:

		; This possibly should honour the size of the image in pages
		; as indicated by the header, instead of the file size.
		; Oh well, for now we use the file size (on stack).
	mov ax, [si + exeHeaderSize]
	xor si, si
	mov cx, 4
@@:
	shl ax, 1
	rcl si, 1
	loop @B			; si:ax <<= 4

	mov sp, bp
	pop bx
	pop dx

	sub bx, ax
	sbb dx, si		; file size minus header size

	mov al, 1		; indicate nonzero filesize
	jmp @F

.no_exe:
	mov sp, bp
	pop bx
	pop dx			; full file size
@@:

		; Clear registers

ll14:
	push ax			; zero if empty file
	push bx
	push dx
;	mov word [reg_ebx], dx
;	mov word [reg_ecx], bx

;--- cancel current process (unless there is none)
;--- this will also put cpu back in real-mode!!!

	call terminate_attached_process
	jz ll_attached_unterminated
%if _PM
	call ispm
	jz ll_still_pm
%endif

	call zeroregs

	pop word [reg_ebx]
	pop word [reg_ecx]

		; Fix up interrupt vectors in PSP
	mov si, CCIV		; address of original INT 23 and 24 (in PSP)
	mov di, run2324
	movsw
	movsw
	movsw
	movsw

		; Prior to our report in 2022 May, the FreeDOS kernel
		;  incorrectly returned NC without loading a process
		;  when passed a file that is empty (0 byte). Refer
		;  to https://github.com/FDOS/kernel/issues/70
	pop ax			; zero if empty file
	test ax, ax
	mov al, 0Bh		; ax = 000Bh (Invalid format)
	jz ll16

		; Actual program loading.  Use the DOS interrupt.
	mov ax, 4B01h		; load program
	mov dx, DTA		; offset of file to load
	mov bx, execblk		; parameter block
	int 21h			; load it
	jc ll16			; if error
	mov ax, sp
	sub ax, [SPSAV]
	cmp ax, 80h
	jb ll15			; if in range
	mov ax, 80h
ll15:
	mov word [spadjust], ax
	les si, [execblk.sssp]
	es lodsw		; recover ax
	mov word [reg_eax], ax
	mov word [reg_esp], si
	mov word [reg_ss], es
	les si, [execblk.csip]
	mov word [reg_eip], si
	mov word [reg_cs], es
	push ss
	pop es
	call getpsp
	xchg ax, bx		; ax = PSP, clobber bx
	mov word [pspdbe], ax
	clropt [internalflags], attachedterm
	mov di, reg_ds
	stosw
	scasw
	stosw			; reg_es
	push ax
	call setpspdbg

		; Finish up. Set termination address.
	mov ax, 2522h		; set interrupt vector 22h
	mov dx, int22		; ds => lDEBUG_DATA_ENTRY
	int 21h
	pop ds
	mov word [TPIV], dx
	mov word [TPIV+2], ss	; => lDEBUG_DATA_ENTRY
	push ss
	pop ds

		; Set up initial addresses for 'a', 'd', and 'u' commands.
adusetup:
	mov ax, word [reg_eip]
	mov cx, word [reg_eip+2]
	mov bx, word [reg_cs]
	mov dx, var_addr_entries.amount
	mov di, var_addr_entries

.loop:
	stosw			; IP
%if saSegSel == 4
	mov word [di], cx
	scasw			; skip this word
%endif
	xchg ax, bx
	stosw			; CS
%if _PM
 %if SEGADR_size != 10
  %error Unexpected SEGADR size
 %endif
	call ispm
	jnz .86m
.pm:
	scasw			; skip saSegment
	stosw			; store saSelector
	jmp @F
.86m:
	stosw			; store saSegment
	scasw			; skip saSelector
@@:
%else
 %if SEGADR_size == 10
  %error Unexpected SEGADR size
 %endif
%endif
	xchg ax, bx		; d_addr

	dec dx
	jnz .loop
	retn

		; Error messages.  Print and quit.
ll16:
	jmp ww15		; print error message


ll_attached_unterminated:
	call putrunint
	mov dx, msg.ll_unterm
%if _PM
	jmp @F

ll_still_pm:
	mov dx, msg.cannotpmload
%endif
@@:
	call putsz
	jmp cmd3
