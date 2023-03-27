
%if 0

lDebug INSTALL commands

Copyright (C) 2008-2022 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


uninstall:
	mov cx, install.clear
	jmp install.common

install:
	mov cx, .set

.common:
	call skipwhite
	push si
.loopcheck:
	dec si
	call checkinstallflag
	call skipcomma
	call iseol?
	jne .loopcheck

	pop si
.loopdo:
	dec si
	call checkinstallflag
	mov dx, word [bx + 2]
	mov di, word [bx + 4]
	mov ax, word [bx + 6]
	test di, di
	jnz @F
	call ax
	db __TEST_IMM16
@@:
	call cx
	call skipcomma
	call iseol?
	jne .loopdo
	retn


.set:
	test word [di], ax
	jnz @F
	or word [di], ax
	call putsz
	mov dx, msg.tryenable
	jmp .putsz

@@:
	call putsz
	mov dx, msg.alreadyenabled
.putsz:
	jmp putsz


.clear:
	test word [di], ax
	jz @F
	not ax
	and word [di], ax
	call putsz
	mov dx, msg.trydisable
	jmp .putsz

@@:
	call putsz
	mov dx, msg.alreadydisabled
	jmp .putsz


checkinstallflag:
	mov bx, installflags
@@:
	mov dx, word [bx]
	test dx, dx
	jz .error
	call isstring?
	je @F
	add bx, 8
	jmp @B

@@:
	retn

.error:
	jmp error


%if _AREAS && _AREAS_HOOK_CLIENT
install_areas:
	cmp cx, install.clear
	je uninstall_areas

	push si
	cmp word [areas_struc + areastrucEntry], 0CBF9h
	je @F
	mov dx, msg.areasalreadyinstalled
	mov ax, 0703h
.setrc_putsz_ret:
	call setrc
.putsz_ret:
	call putsz
	pop si
	retn

@@:
	call findinstalleddebugger
				; CHG: si, di, es, ax, cx, dx
	push ss
	pop es
	jnc @F

	mov dx, msg.areasnodebuggerfound
	mov ax, 0704h
	jmp .setrc_putsz_ret

@@:
	mov al, 33h
	mov bx, areas_struc
%if _PM
	mov dx, word [pspdbg]
	call call_int2D
%else
	mov dx, ss
	int 2Dh			; install areas
%endif
	push ss
	pop ds
	push ss
	pop es

	test al, al
	jz .not_supported
	cmp al, -1
	je .installed
	mov di, msg.areasnotinstalled.code
	call hexbyte
	mov dx, msg.areasnotinstalled
	mov ax, 0702h
	jmp .setrc_putsz_ret

.not_supported:
	mov dx, msg.areasnotsupported
	mov ax, 0701h
	jmp .setrc_putsz_ret

.installed:
	mov dx, msg.areasinstalled
	jmp .putsz_ret


uninstall_areas:
	push si
		; cx != 0
	cmp word [areas_struc + areastrucEntry], 0CBF9h
	jne .is_installed
	mov dx, msg.areasalreadyuninstalled
	mov ax, 0705h
.setrc_putsz_ret:
	jmp install_areas.setrc_putsz_ret
.putsz_ret:
	jmp install_areas.putsz_ret


.qq_entry:
	push si
	xor cx, cx
	cmp word [areas_struc + areastrucEntry], 0CBF9h
	jne .is_installed
.ret:
	pop si
	retn


.is_installed:
	push cx
	mov al, 0
%if _PM
	call ispm
	jnz .86m
subcpu 286
.pm:
	lframe none
	lvar 32h, 86m_call_struc
	lenter
	mov word [bp + ?86m_call_struc +1Ch], ax	; eax
	xor ax, ax
	mov word [bp + ?86m_call_struc +20h], ax	; flags
	mov word [bp + ?86m_call_struc +0Ch + 2], ax
	mov word [bp + ?86m_call_struc +0Ch], ax
	mov word [bp + ?86m_call_struc +2Eh], ax	; sp
	mov word [bp + ?86m_call_struc +30h], ax	; ss
	mov word [bp + ?86m_call_struc +22h], ax	; es
	mov word [bp + ?86m_call_struc +24h], ax	; ds
	mov ax, word [pspdbg]
	mov word [bp + ?86m_call_struc +2Ah], areas_struc + areastrucEntry
							; ip
	mov word [bp + ?86m_call_struc +2Ch], ax	; cs
	; push ss
	; pop es			; => stack
	lea di, [bp + ?86m_call_struc]	; -> 86-Mode call structure
_386	movzx edi, di			; (previously checked b[dpmi32] here)
	xor bx, bx			; flags/reserved
	xor cx, cx			; do not copy from PM stack
	mov ax, 0301h
	int 31h				; call 86 mode function with far return
	mov ah, byte [bp + ?86m_call_struc +20h]	; flags
	sahf
	mov ax, word [bp + ?86m_call_struc +1Ch]	; eax
	lleave
subcpureset
	jmp .common
%endif
.86m:
	push cs
	call .86m_to_entry_areas_struc
.common:
	pop cx
	push ss
	pop ds
	push ss
	pop es
	jcxz .ret
	mov di, msg.areasuninstalled.code
	call hexbyte
	mov dx, msg.areasuninstalled
	jmp .putsz_ret

.86m_to_entry_areas_struc:
	mov bx, areas_struc + areastrucEntry
	push ss
	push bx
	retf
%endif
