
%if 0

lDebug "boot" commands - boot loading

Copyright (C) 2008-2017 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

	align 16, db 0
load_unit_flags:
	times 128 db _LUF_DEFAULT_DISKETTE
	times 128 db _LUF_DEFAULT_HARDDISK

	align 16, db 0
load_partition_table:	times 16 * 4 db 0
.end:

	align 16, db 0
		; Data passed to us from loader (in case we booted)
loaddata_loadedfrom:
	times (-LOADDATA + bsBPB + ebpbNew + BPBN_size) db 0

	align 16, db 0
load_data_lowest:
	times LOADDATA3_size db 0
		; data used to access storage
load_data:
	times (-LOADDATA2 + bsBPB + ebpbNew + BPBN_size) db 0
	align 16, db 0
load_current_settings:
	istruc LOADSETTINGS
at lsKernelName,	load_kernelname_default:	dw 0
at lsAddName,		load_addname_default:		dw 0
at lsMinPara,		load_minpara:			dw 0
at lsMaxPara,		load_maxpara:			dw 0
at lsOptions,		load_options:			dw 0
at lsSegment,		load_loadseg:			dw 0
at lsEntry,		load_entrypoint:		dd 0
at lsBPB,		load_bpb:			dd 0
at lsCheckOffset,	load_check_offset:		dw 0
at lsCheckValue,	load_check_value:		dw 0
at lsName
	;	iend
	%pop		; (pop off the istruc context)

%if _INPUT_FILE_BOOT
	align 16, db 0
load_yy_direntry:
	times DIRENTRY_size db 0

LOAD_INPUT_FILE_SIZE equ fromparas(paras(-LOADDATA3 + bsBPB + ebpbNew + BPBN_size))
	align 16, db 0
load_input_file:
	times _INPUT_FILE_BOOT * LOAD_INPUT_FILE_SIZE db 0
.active:
	dw 0
.goto_offset:
	dw 0
%endif

	align 4, db 0
load_bpb_dest:		dd 0
load_sectors:		equ load_data - LOADDATA2 + bsBPB + bpbCHSSectors
load_heads:		equ load_data - LOADDATA2 + bsBPB + bpbCHSHeads
load_sectorsize:	equ load_data - LOADDATA2 + bsBPB + bpbBytesPerSector
load_sectorsizepara:	equ load_data - LOADDATA2 + ldParaPerSector
load_sectorseg:		equ load_data - LOADDATA2 + ldSectorSeg
load_partition_sector:	dd 0
load_ldp_sector:	equ loaddata_loadedfrom - LOADDATA + bsBPB + bpbHiddenSectors
; load_sdp_sector:	equ load_data - LOADDATA2 + bsBPB + bpbHiddenSectors
load_partition_cycle:	dw 0
load_cmdline:		dw 0
load_ldflags:		equ load_data - LOADDATA2 + ldFlags
load_unit:		equ load_data - LOADDATA2 + bsBPB + ebpbNew + bpbnBootUnit
load_partition_entry:	dw 0
load_partition:		db 0
load_current_partition:	db 0
load_found_partition:	db 0
load_check_dir_attr:	db 0
load_sector_alt:	db 0

	align 4, db 0
load_kernel_name:	times 11 + 1 db 0
	align 2, db 0
;	load_kernelname_default:dw 0
		; ASCIZ filename for if load_kernelname_input terminates in '/'
load_kernelname_input:	dw 0
		; input (ASCIZ '/'-terminated pathnames + optional filename)
load_kernelname_next:	dw 0
		; next element in ASCIZ load_kernelname_input,
		;  empty string if pathname terminated in '/'
;	load_addname_default:	dw 0
load_addname_input:	dw 0
load_addname_next:	dw 0
%if _INPUT_FILE_BOOT
load_yyname_input:	dw 0
load_yyname_next:	dw 0
%endif

	align 2, db 0
load_adr_dirbuf_segment:dw 0
boot_new_memsizekib:	dw 0
boot_old_memsizekib:	dw 0
boot_ebdaflag:	db 0		; EBDA flag: non-zero if present

	align 4, db 0
load_readwrite_sector:	dd 0
load_readwrite_buffer:	dw 0
load_readwrite_count:	dw 0
load_readwrite_function:dw 0


	usesection lDEBUG_CODE

init_bootcmd:
	call guard_re
	mov bp, load_data - LOADDATA2

	testopt [internalflags3], dif3_partition_changed
	jz @F

	push word [bp + bsBPB + bpbHiddenSectors + 2]
	push word [bp + bsBPB + bpbHiddenSectors]
	pop word [load_partition_sector]
	pop word [load_partition_sector + 2]
	clropt [internalflags3], dif3_partition_changed
@@:

	xor cx, cx
	mov [bp + bsBPB + bpbHiddenSectors + 0], cx
	mov [bp + bsBPB + bpbHiddenSectors + 2], cx
	mov byte [load_sector_alt], cl
	retn


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
bootcmd_dispatch:
	dw bootcmd.list
	dw msg.list
	dw bootcmd.quit
	dw msg.quit
	dw boot_read
	dw msg.read
	dw boot_write
	dw msg.write
	dw boot_dir
	dw msg.dir
	dw bootcmd.protocol
	dw msg.protocol
	dw 0			; table end marker


	usesection lDEBUG_CODE

bootcmd:
%if _PM
	call ispm
	jnz .rm
	mov dx, nopmsupp
	mov ax, 0201h
	call setrc
	jmp putsz_error
.rm:
%endif
d4	call d4message
d4	asciz "In bootcmd",13,10

	call init_bootcmd

	call skipcomma
	dec si
	mov bx, bootcmd_dispatch
@@:
	mov cx, [bx]
	mov dx, [bx + 2]
	add bx, 4
	jcxz .notproto
	call isstring?
	jne @B
	jmp cx


.quit:
 %if _DOSEMU
	testopt [internalflags], runningdosemu
	jz .quit_not_dosemu

	xor bx, bx
	mov ax, -1
	int 0E6h			; dosemu quit

.quit_not_dosemu:
 %endif

; from https://stackoverflow.com/a/5240330/738287
	mov ax, 5301h
	xor bx, bx
	int 15h				; connect to APM API

	mov ax, 530Eh
	xor bx, bx
	mov cx, 0102h
	int 15h				; set APM version to 1.02

	mov ax, 5307h
	mov bx, 1
	mov cx, 3
	int 15h				; shut down system

	mov dx, msg.boot_quit_fail
	mov ax, 0202h
	call setrc
	jmp putsz_error


.protocol:
	call skipequals
	dec si
	mov dx, msg.sector
	call isstring?
	je .proto_sector

	mov dx, msg.sector_alt
	call isstring?
	je .proto_sector_alt

	mov bx, loadsettings
.proto_settings_next:
	lea dx, [bx + lsName]
	call isstring?
	je .proto_settings
	add bx, LOADSETTINGS_size
	cmp word [bx], 0
	jne .proto_settings_next

	jmp error

.proto_sector_alt:
	mov byte [load_sector_alt], 1
.proto_sector:
	call skipcomma
	db __TEST_IMM8
.notproto:
	lodsb
	call parseloadunit_default_sdp
	jmp .load


.errordec:
	dec si
.error:
	db __TEST_IMM8		; skip pop
.errorpop:
	pop si
	jmp error


.proto_settings:
	push si
	mov si, bx
	mov di, load_current_settings
	mov cx, lsName >> 1
	rep movsw
%if lsName & 1
	movsb
%endif
	push word [load_kernelname_default - (load_current_settings + lsName) + di]
	pop word [load_kernelname_input - (load_current_settings + lsName) + di]
	push word [load_addname_default - (load_current_settings + lsName) + di]
	pop word [load_addname_input - (load_current_settings + lsName) + di]

	pop si

d4	call d4message
d4	asciz "In bootcmd.proto_settings",13,10

.proto_next:
	call skipcomma
	dec si
	mov dx, msg.segment
	call isstring?
	je .proto_segment
	mov dx, msg.entry
	call isstring?
	je .proto_entry
	mov dx, msg.bpb
	call isstring?
	je .proto_bpb
	mov dx, msg.minpara
	call isstring?
	je .proto_minpara
	mov dx, msg.maxpara
	call isstring?
	je .proto_maxpara
	mov dx, msg.checkoffset
	call isstring?
	je .proto_checkoffset
	mov dx, msg.checkvalue
	call isstring?
	je .proto_checkvalue

	mov bx, loadoptiontable
.proto_lot_next:
	mov cx, [bx]		; flag
	mov dx, [bx + 2]	; -> ASCIZ message
	test cx, cx
	jz .proto_done
	call isstring?
	je .proto_lot
	add bx, 4
	jmp .proto_lot_next

.proto_lot:
	call skipequals
	mov dx, cx
	not dx
	and word [load_options], dx
	call getexpression
	call toboolean
	test dx, dx
	jz @F
	or word [load_options], cx
@@:
	dec si
	jmp .proto_next

.proto_segment:
	call skipequals
	call getword		; dx = word
	cmp dx, 50h
	jb @F
	mov word [load_loadseg], dx
	dec si
	jmp .proto_next

@@:
	mov dx, msg.boot_segment_too_low
	mov ax, 0210h
	call setrc
	jmp .fail


.proto_entry:
	call skipequals
	xor bx, bx
	call getaddr		; bx:(e)dx = addr
_386_PM	test edx, 0_FFFF_0000h
_386_PM	jnz .error
	mov word [load_entrypoint], dx
	mov word [load_entrypoint + 2], bx
	dec si
	jmp .proto_next

.proto_bpb:
	call skipequals
	xor bx, bx
	call getaddr		; bx:(e)dx = addr
_386_PM	test edx, 0_FFFF_0000h
_386_PM	jnz .error
	mov word [load_bpb], dx
	mov word [load_bpb + 2], bx
	dec si
	jmp .proto_next

.proto_minpara:
	call skipequals
	call getword		; dx = word
	mov word [load_minpara], dx
	dec si
	jmp .proto_next

.proto_maxpara:
	call skipequals
	call getword		; dx = word
	mov word [load_maxpara], dx
	dec si
	jmp .proto_next

.proto_checkoffset:
	call skipequals
	call getword		; dx = word
	mov ax, dx
	and al, 31
	cmp al, 31
	je .error
		; Previously we rejected all odd values here. Now,
		;  we only reject a subset of odd values, namely
		;  those that may actually cross a sector boundary.
		; Note that sectors may be as small as 32 bytes.
		; This insures the checked word never crosses a sector
		;  boundary. Thus, loading a single sector suffices.
	mov word [load_check_offset], dx
	dec si
	jmp .proto_next

.proto_checkvalue:
	call skipequals
	call getword		; dx = word
	mov word [load_check_value], dx
	dec si
	jmp .proto_next

.proto_done:
	mov bx, loadoptiontable.incompatible
@@:
	mov cx, [bx]
	or cx, [bx + 2]
	jz .proto_compatible
	mov dx, word [load_options]
	and dx, cx
	cmp dx, cx
	je .proto_incompatible
	add bx, 4
	jmp @B

.proto_incompatible:
	 push ss
	 pop es
	mov ax, 0203h
	call setrc
	mov dx, msg.bootfail
	call putsz_error
	mov dx, msg.boot_cannot_set_both
	call putsz_error
	 mov ax, word [bx]
	 call .proto_incompatible_get_label
	 call putsz_error
	mov dx, msg.boot_and
	call putsz_error
	 mov ax, word [bx + 2]
	 call .proto_incompatible_get_label
	 call putsz_error
	mov dx, msg.boot_dot_crlf
.putsz_errret:
	mov ax, 0204h
	call setrc
	call putsz_error
	jmp near word [errret]

.proto_incompatible_get_label:
	push bx
	mov bx, loadoptiontable
@@:
	mov cx, [bx]
	jcxz .proto_internal_error
	cmp ax, cx
	mov dx, word [bx + 2]
	je @F
	add bx, 4
	jmp @B

@@:
	pop bx
	retn

.proto_internal_error:
	mov dx, msg.boot_internal_error
	mov ax, 0205h
	call setrc
	jmp .putsz_errret


.proto_compatible:
	lodsb
	call parseloadunit_default_sdp
	jz .fn_done_eol		; no filename given, use defaults -->
		; al was = '/' or '\' or first pathname's first character
		; si-> next char
	mov bx, load_kernelname_input
	call .pathname_parse_super

	push si
	push ax
	call skipwh0
	call iseol?
	pop ax
	pop si
	je .fn_done

	mov bx, load_addname_input

	call skipwh0
	cmp al, '/'
	je @F
	cmp al, '\'
	jne .proto_not_double_slash
@@:
	cmp byte [si], '/'
	je @F
	cmp byte [si], '\'
	jne .proto_not_double_slash
@@:
	dec si
	mov word [bx], si
	mov word [si], "//"
	inc si
	inc si
	lodsb
	mov byte [si - 1], 0
	jmp .fn_done

.proto_not_double_slash:
	call .pathname_parse_super
	jmp .fn_done


		; INP:	bx -> word variable to hold filename
		; OUT:	word [bx] -> list of /-separated pathnames, zero-terminated
		; CHG:	ax, cx, si, di, [es:load_kernel_name]
.pathname_parse_super:
d4	call d4message
d4	asciz "In bootcmd.pathname_parse_super",13,10

	call skipwh0
	cmp al, '/'
	je @F
	cmp al, '\'
	jne @FF
@@:
	dec si
	mov word [bx], si
	inc si
	jmp .pathname_check

@@:
	dec si
	mov word [bx], si

.pathname_parse:
.pathname_next:
d4	call d4message
d4	asciz "In bootcmd.pathname_parse",13,10
	call boot_parse_fn
		; al = separator char
		; si -> next char after that (if any)
	cmp al, '/'		; path separator?
	je .pathname_check
	cmp al, '\'
	jne .pathname_none	; no, this was the filename -->
.pathname_check:
d4	call d4message
d4	asciz "In bootcmd.pathname_parse_check",13,10
	mov byte [si - 1], '/'	; normalise path separator
	lodsb
	cmp al, 32		; space ?
	je .pathname_gotfirst
	cmp al, 9
	je .pathname_gotfirst	; yes, allow for second name -->
	dec si
	call iseol?		; EOL ?
	jne .pathname_next	; no, next pathname element -->
	mov byte [si], 0	; terminate after trailing path sep
	retn

.pathname_gotfirst:
.pathname_none:
	mov al, 0
	dec si
	xchg al, byte [si]	; terminate after filename
	mov word [terminator_in_line_in.offset], si
	mov byte [terminator_in_line_in.value], al
	inc si
	retn


		; DPR:word [load_kernelname_input] -> ASCIZ pathname.
		; if it ends in '/', append DPR:word [load_kernelname_default]
		; DPR:word [load_addname_input] -> ASCIZ pathname.
		; if it ends in '/', append DPR:word [load_addname_default]
		; if it's empty (and no trailing '/' in front of the zero)
		;  then no additional name is given.
.fn_done_eol:
	mov al, 13
.fn_done:
d4	call d4message
d4	asciz "In bootcmd.fn_done",13,10

	and word [load_cmdline], 0

	call iseol?
	je @F

	testopt [load_options], LOAD_CMDLINE
	jz error

	call skipwh0
	mov ah, 0
	cmp al, '"'
	je .cmdline_quote
	cmp al, "'"
	jne .cmdline_no_quote
.cmdline_quote:
	mov ah, al
	db __TEST_IMM8
.cmdline_no_quote:
	dec si
	mov word [load_cmdline], si
.cmdline_loop:
	lodsb
	call iseol?.notsemicolon
	je .cmdline_eol
	cmp al, ah
	jne .cmdline_loop
.cmdline_quote_eol:
	mov byte [si - 1], 0
	lodsb
	call chkeol
	jmp @F

.cmdline_eol:
	test ah, ah
	jnz error
	mov byte [si - 1], 0

@@:
	testopt [internalflags3], dif3_load_is_dp
	jnz .load_kernel_from_ldp

	mov bl, [load_partition]
	cmp byte [load_unit], 80h
	jb .p_f_is_diskette
	test bl, bl		; partition specified ?
	jz .error		; no, error -->

	call query_geometry

	mov cx, load_freedos_from_partition
	call scan_partitions
	mov dx, msg.boot_partition_not_found
	mov ax, 0211h
	call setrc
	jmp .fail


.load_kernel_from_ldp:
	call query_geometry
	mov ax, word [load_partition_sector]
	mov dx, word [load_partition_sector + 2]
	jmp @F


.p_f_is_diskette:
	test bl, bl		; partition specified ?
	jnz .error		; yes, error -->

	call query_geometry

	xor ax, ax
	xor dx, dx
@@:
	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	 push dx
	 push ax
	call read_ae_512_bytes

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch
	 pop ax
	 pop dx

	push es
	testopt [load_options], LOAD_SET_DSSI_PARTINFO
	jz @F

	push ax
	xor di, di
	mov es, di
	mov di, 600h
	mov ax, 19CDh
	stosw				; fake boot sector loader
	mov cx, 508 / 2
	xor ax, ax
	rep stosw			; initialise pseudo MBR
	mov ax, 0AA55h
	stosw				; fake boot sector signature
	pop ax

	mov si, 600h + 512 - 2 - 64
	mov word [es:si + 8], ax
	mov word [es:si + 8 + 2], dx	; store in partition table entry
	mov byte [es:si + piBoot], 80h	; fake primary active
	mov byte [es:si + piType], 0FFh	; fake a type
	inc byte [es:si + piLength]	; fake a size

	mov word [load_partition_entry], si
@@:
	pop es

	jmp load_freedos_common


.load:
	jnz bootcmd.error

	testopt [internalflags3], dif3_load_is_dp
	jnz .load_sector_from_ldp

	cmp byte [load_partition], 0
	je .load_boot

	call query_geometry

d4	call d4message
d4	asciz "In bootcmd.load (before call to scan_partitions)",13,10

	mov cx, load_from_partition
	call scan_partitions
	mov dx, msg.boot_partition_not_found
	mov ax, 0212h
	call setrc
	jmp .fail


.load_sector_from_ldp:
	call query_geometry
	mov ax, word [load_partition_sector]
	mov dx, word [load_partition_sector + 2]
	jmp @F

.load_boot:
	call query_geometry

	xor ax, ax
	xor dx, dx
@@:
	mov bx, 7C0h
d4	call d4dumpregs
d4	call d4message
d4	asciz 13,10,"In bootcmd.load_boot (before call to read_sector)",13,10
	 push dx
	 push ax
	call read_ae_512_bytes
d4	call d4message
d4	asciz "In bootcmd.load_boot (after call to read_sector)",13,10
	xor dx, dx
	mov es, dx

	mov al, byte [load_unit]	; al = boot unit
	mov bx, 7C00h

	cmp word [es:7C00h + 510], 0AA55h
	jne boot_sigmismatch

	cmp word [es:bx], 0
	je boot_codemismatch

	push ax
	mov cx, 510 / 2
	mov di, 600h			; MBR location
	xor ax, ax
	rep stosw			; initialise (sector and all entries)
	mov ax, 0AA55h
	stosw				; initialise boot sector signature
	mov word [es:600h], 019CDh	; initialise boot sector code
	mov di, 600h + 510 - 4*16	; -> first partition table entry
	pop ax
	 pop word [es:di + piStart]
	 pop word [es:di + piStart + 2]	; = boot sector LBA
	mov byte [es:di + 0], 80h	; "bootable" flag set
	mov byte [es:di + 4], 0FFh	; dummy value for FS type (nonzero)
	mov byte [es:di + 12], 1	; dummy value for length (nonzero)

load_partition_common: equ $
	and word [reg_efl], ~(400h|200h|100h)	; UP, DI, TF=0
	mov word [reg_esi], di
	mov word [reg_ebp], di
	mov word [reg_ds], cx		; ds:si -> 0:600h + offset to first entry
	mov byte [reg_edx], al		; dl = boot unit
	mov word [reg_eip], bx
	mov word [reg_eip + 2], cx
	mov word [reg_cs], cx		; cs:eip = 0:7C00h
	 cmp byte [load_sector_alt], 0
	 je @F
	 mov word [reg_eip], cx
	 mov word [reg_cs], 7C0h	; cs:eip = 07C0h:0
@@:
	mov word [reg_esp], bx
	mov word [reg_esp + 2], cx
	mov word [reg_ss], cx		; ss:esp = 0:7C00h
	setopt [internalflags2], dif2_boot_loaded_kernel
	retn


.fail_read:
	 push ss
	 pop es
	mov di, msg.bootfail_read_errorcode
	mov al, ah
	mov ah, 04h
	call setrc
	call hexbyte
	mov dx, msg.bootfail_read

.fail:
	 push ss
	 pop es
	push dx
	mov dx, msg.bootfail
	call putsz_error
	pop dx
	call putsz_error
	mov ax, 02FFh
	call setrc
	jmp near word [errret]


bootcmd.list:
	call skipcomma

	call parseloadunit_default_sdp
	jnz bootcmd.error

	call query_geometry

	testopt [internalflags3], dif3_load_is_dp
	jnz .list_ldp

	cmp byte [load_partition], 0
	je .listall

	mov byte [load_found_partition], 0
	mov cx, list_single_partition
	call scan_partitions
	cmp byte [load_found_partition], 0
	jne @F
	mov dx, msg.boot_partition_not_found
	mov ax, 0213h
	call setrc
	jmp bootcmd.fail
@@:
	retn

.listall:
	mov cx, list_any_partition
	jmp scan_partitions

.list_ldp:
	mov byte [load_found_partition], 0
	mov cx, list_partition_if_ldp
	call scan_partitions
	cmp byte [load_found_partition], 0
	jne @F
	mov dx, msg.boot_partition_not_found
	mov ax, 0214h
	call setrc
	jmp bootcmd.fail
@@:
	retn


list_partition_if_ldp:
d4	call d4message
d4	asciz "In list_partition_if_ldp",13,10

	mov ax, word [bp + di - 8]
	mov dx, word [bp + di - 6]	; root
	add ax, word [es:si + 8]
	adc dx, word [es:si + 8 + 2]	; add partition offset
	cmp word [load_partition_sector], ax
	jne @F
	cmp word [load_partition_sector + 2], dx
	je list_single_partition.gotit
@@:
	retn

list_single_partition:
d4	call d4message
d4	asciz "In list_single_partition",13,10

	mov al, byte [load_current_partition]
	cmp al, byte [load_partition]
	je .gotit
	retn

.gotit:
	inc byte [load_found_partition]

		; INP:	es:si -> partition table entry,
		;	 si = load_partition_table .. load_partition_table+48,
		;	 es = ss
		;	bp + di -> above part table metadata,
		;	 dwo [bp + di - 4] = root (outermost extended position)
		;	 dwo [bp + di - 8] = base (current table position)
		; CHG:	ax, bx, (cx), dx
list_any_partition:
	 push es
	 push cx
	 push si
	 push di

	mov di, line_out	; reset di
	mov al, "u"
	stosb
	mov al, byte [load_unit]
	call hexbyte
	test al, al
	mov al, '.'
	stosb
	js @F
	mov al, 32
	stosb
	stosb
	jmp @FF

@@:
	mov al, byte [load_current_partition]
	call decbyte
	cmp al, 10
	mov al, 32
	jae @F
	stosb
@@:
	stosb

	mov al, byte [load_unit]
	push ax
	mov bx, "fd"
	cmp al, 80h
	jb @F
	mov bl, "h"
@@:
	and al, ~80h
	add al, 'a'
	cmp al, 'z'
	jbe @F
	pop ax
	mov al, 32
	mov cx, 3 + 2
	rep stosb
	jmp .beyondZ

@@:
	xchg ax, bx
	stosw
	xchg ax, bx
	stosb
	pop ax
	test al, al
	js @F
	mov al, 32
	stosb
	stosb
	jmp @FF

@@:
	mov al, byte [load_current_partition]
	call decbyte
	cmp al, 10
	mov al, 32
	jae @F
	stosb
@@:
.beyondZ:
	stosb

	mov al, byte [si + 4]
	call hexbyte

	mov al, 32
	stosb

	mov cx, di		; (preserve di in line_out)
	 pop di			; get di of scan_partitions
	 push di
	mov ax, word [bp + di - 8]
	mov dx, word [bp + di - 6]	; root
	mov di, cx		; (preserve di in line_out)
	add ax, word [si + 8]
	adc dx, word [si + 8 + 2]	; add partition offset
	xchg ax, dx
	call hexword
	xchg ax, dx
	call hexword

	 push ax
	mov ax, " ("
	stosw
	 pop ax
	  push cx
	  push bx
	mov cx, [load_sectorsize]
	mov bx, 4+4
	call disp_dxax_times_cx_width_bx_size.store
	 push ax
	mov ax, ") "
	stosw
	 pop ax

	push dx
	push ax

	mov ax, word [si + 12]
	mov dx, word [si + 12 + 2]
	xchg ax, dx
	call hexword
	xchg ax, dx
	call hexword

	 push ax
	mov ax, " ("
	stosw
	 pop ax
	call disp_dxax_times_cx_width_bx_size.store
	mov al, ")"
	stosb

	pop ax
	pop dx

	  pop bx
	  pop cx

	cmp byte [si + piType], ptLinux
	jne .notlinux

	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	call read_ae_1536_bytes

	cmp word [es:1024 + 56], 0xEF53	; s_magic == EXT2_SUPER_MAGIC ?
	jne .nolabel

	cmp word [es:1024 + 76 + 2], 0
	jne .nolabel
	cmp word [es:1024 + 76], 1	; s_rev_level == EXT2_DYNAMIC_REV ?
	jne .nolabel

	push es
	pop ds
	mov si, 1024 + 120
	mov cx, 16
	push ss
	pop es

	mov al, 32
	stosb
@@:
	lodsb
	test al, al
	jz @F
	stosb
	loop @B
@@:

	push ss
	pop ds

	pop cx
	pop si
	push si				; get si of scan_partitions
	push cx

.notlinux:
	mov bl, byte [si + piType]
	cmp bl, ptFAT12
	je .isfat
	cmp bl, ptFAT16_16BIT_CHS
	je .isfat
	cmp bl, ptFAT16_CHS
	je .isfat
	cmp bl, ptFAT32_CHS
	je .isfat
	cmp bl, ptFAT32
	je .isfat
	cmp bl, ptFAT16
	jne .notfat
.isfat:

	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	call read_ae_512_bytes

	cmp word [es:510], 0AA55h
	jne .nolabel
	cmp word [es:bsBPB + bpbBytesPerSector], 0
	je .nolabel
	mov si, bsBPB + bpbNew + bpbnVolumeLabel
	cmp word [es:bsBPB + bpbSectorsPerFAT], 0
	jne @F
	mov si, bsBPB + ebpbNew + bpbnVolumeLabel
@@:
	cmp byte [es:si - bpbnVolumeLabel + bpbnExtBPBSignature], 29h
	jne .nolabel
	mov cx, 11

	push es
	pop ds
	push ss
	pop es

	mov al, 32
	stosb
@@:
	lodsb
	test al, al
	jz @F
	stosb
	loop @B
@@:

	push ss
	pop ds
.notfat:
.nolabel:
	push ss
	pop es
	call putsline_crlf

	 pop di
	 pop si
	 pop cx
	 pop es
	retn


		; INP:	al = first character
		;	si -> next
		; OUT:	NC
		;	byte [load_unit] set
		;	byte [load_partition] set
		;	 (zero if none specified, -1 if ldp or sdp)
		;	opt [internalflags3] & dif3_load_is_ldp
		;	opt [internalflags3] & dif3_load_is_sdp
		;	dword [load_partition_sector] set if ldp or sdp
		;	ZR if no filename specified (at end of input)
		;	NZ if presumably a filename specified,
		;	 al = first character (slash or whatever non-blank)
		;	 si -> next
		; CHG:	bx, cx, dx, ax, si, di
		; STT:	ds = es = ss
parseloadunit_default_sdp:
	call parseloadunit
	jnc @F

	mov dl, byte [load_data - LOADDATA2 \
			+ bsBPB + ebpbNew + bpbnBootUnit]
	mov byte [load_unit], dl
	mov byte [load_partition], -1
	or byte [internalflags3], dif3_load_is_sdp

	call skipwh0
	call iseol?
	clc
@@:
	retn


		; INP:	al = first character
		;	si -> next
		; OUT:	CY if no load unit
		;	 (not "HD[A-Z]", "FD[A-Z]", "LD[P]", "SD[P]", "U[0-9A-F]")
		;	 note: this is barely used!
		;	NC else,
		;	 byte [load_unit] set
		;	 byte [load_partition] set
		;	  (zero if none specified, -1 if ldp or sdp)
		;	 opt [internalflags3] & dif3_load_is_ldp
		;	 opt [internalflags3] & dif3_load_is_sdp
		;	 dword [load_partition_sector] set if ldp or sdp
		;	 ZR if no filename specified (at end of input)
		;	 NZ if presumably a filename specified,
		;	  al = first character (slash or whatever non-blank)
		;	  si -> next
		; CHG:	bx, cx, dx, ax, si, di
		; STT:	ds = es = ss
parseloadunit:
	xor bx, bx
	lframe near
	lvar word,	unit_low_partition_high
	lequ ?unit_low_partition_high,		unit
	lequ ?unit_low_partition_high + 1,	partition
	lenter
	lvar word,	dif3_set
	 push bx
	lvar dword,	load_partition_sector
	 push word [load_partition_sector + 2]
	 push word [load_partition_sector]

	mov di, si
	clropt [internalflags3], dif3_load_is_dp
	call uppercase
	cmp al, 'H'
	je .load_hd
	cmp al, 'F'
	je .load_fd
	mov ah, dif3_load_is_ldp
	cmp al, 'L'
	je .load_ld_sd
	mov ah, dif3_load_is_sdp
	cmp al, 'S'
	je .load_ld_sd
%if _INPUT_FILE_BOOT
	mov ah, dif3_load_is_ydp
	cmp al, 'Y'
	je .load_ld_sd
%endif
	cmp al, 'U'
	je .load_u
.retc:
	mov si, di
	dec si
	lodsb
	stc
.ret:
	lleave code
	lret

.load_ld_sd:
	lodsb
	call uppercase
	cmp al, 'D'
	jne .retc

d4	call d4message
d4	asciz "In parseloadunit.load_ld_sd",13,10

	mov dl, byte [load_data - LOADDATA2 \
			+ bsBPB + ebpbNew + bpbnBootUnit]
	cmp ah, dif3_load_is_sdp
	je @F
	mov dl, byte [loaddata_loadedfrom - LOADDATA \
			+ bsBPB + ebpbNew + bpbnBootUnit]
%if _INPUT_FILE_BOOT
	cmp ah, dif3_load_is_ldp
	je @F
	testopt [internalflags2], dif2_input_file_boot
	jz .retc

	xchg ax, bx
	mov ax, LOAD_INPUT_FILE_SIZE
	mul word [load_input_file.active]

;	test dx, dx
;	jnz .error

	xchg ax, bx
	mov dl, byte [load_input_file + bx - LOADDATA3 \
			+ bsBPB + ebpbNew + bpbnBootUnit]
%endif
@@:
	lodsb
	call uppercase
	cmp al, 'P'
	jne .got_unit

d4	call d4message
d4	asciz "In parseloadunit.load_ld_sd with ldp",13,10

	mov byte [bp + ?unit], dl
	mov byte [bp + ?partition], -1
	or byte [bp + ?dif3_set], ah

	cmp ah, dif3_load_is_sdp
	je @F
%if _INPUT_FILE_BOOT
	cmp ah, dif3_load_is_ldp
	je .is_ldp
	push word [load_input_file + bx - LOADDATA3 \
			+ bsBPB + bpbHiddenSectors + 2]
	push word [load_input_file + bx - LOADDATA3 \
			+ bsBPB + bpbHiddenSectors]
	jmp .is_ldp_ydp

%endif
.is_ldp:
	push word [load_ldp_sector + 2]
	push word [load_ldp_sector]
.is_ldp_ydp:
	pop word [bp + ?load_partition_sector]
	pop word [bp + ?load_partition_sector + 2]
@@:

	call skipwhite
	call iseol?
	jne .fn
	jmp .ret_nc

.load_u:
	lodsb
	call uppercase

	cmp al, '('
	je @F
	cmp al, '0'
	jb .retc
	cmp al, '9'
	jbe @F
	cmp al, 'A'
	jb .retc
	cmp al, 'F'
	ja .retc
@@:

	cmp al, '('
	jne .u_not_expr
	lodsb
	call getexpression
	call skipwh0
	cmp al, ')'
	lodsb
	jne bootcmd.errordec
	jmp .u_check_dot

.u_not_expr:
	call boot_get_hexadecimal_literal
.u_check_dot:
	cmp al, '.'
	jne bootcmd.error
	lodsb
	test bx, bx
	jnz bootcmd.error
	cmp dx, 256
	jae bootcmd.error
	jmp .got_unit

.load_fd:
	mov dl, 0

d4	call d4message
d4	asciz "In parseloadunit.load_fd",13,10

	db __TEST_IMM16		; skip mov
.load_hd:
	mov dl, 80h
d4	call d4message
d4	asciz "In parseloadunit.load_fd or .load_hd",13,10

	lodsb
	call uppercase
	cmp al, 'D'
	jne .retc
	lodsb
	call uppercase
	sub al, 'A'
	cmp al, 'Z' - 'A'
	ja .retc
	or dl, al			; hdX: 80h + number, fdX: 0 + number
	lodsb

.got_unit:
	mov cx, dx
	mov byte [bp + ?unit], cl
	mov byte [bp + ?partition], 0
	cmp al, '/'			; slash ?
	je .fn
	cmp al, '\'
	je .fn				; got a filename -->
	cmp al, 32			; or blank ?
	je @F
	cmp al, 9
	jne .checkeol			; check for EOL -- but no filename
		;  (hdd1name is invalid -- must be hdd1/name or hdd1 name)
@@:					; was blank
	call skipwh0			; skip blanks
	call iseol?			; EOL ?
	jne .fn				; no, is filename -->
					; will jump after this
.checkeol:
	call iseol?			; EOL ?
	je .ret_nc			; yes, no filename -->

	cmp al, '('
	je @F
	cmp al, '0'
	jb .retc
	cmp al, '9'
	ja .retc
@@:

d4	call d4message
d4	asciz "In parseloadunit (after no EOL found)",13,10
	push cx
	cmp al, '('
	jne .not_expr
	lodsb				; skip opening paren

d4	call d4message
d4	asciz "In parseloadunit (before call to getexpression)",13,10

	call getexpression

d4	call d4message
d4	asciz "In parseloadunit (after call to getexpression)",13,10
	call skipwh0
	cmp al, ')'
	lodsb
	jne .errordec
	mov cx, dx
	or cx, bx
	jnz .got_expr
	pop dx
	jmp .got_unit

.errordec:
	dec si
.error:
	jmp bootcmd.error

.not_expr:
d4	call d4message
d4	asciz "In parseloadunit (before call to boot_get_decimal_literal)",13,10
	call boot_get_decimal_literal
.got_expr:			; bx:dx = load partition number
d4	call d4message
d4	asciz "In parseloadunit.got_expr",13,10
	pop cx			; cl = load unit
	cmp cl, 80h
	jb bootcmd.error		; diskettes aren't partitioned
	test bx, bx
	jnz bootcmd.error
	cmp dx, 255
	ja bootcmd.error
	test dx, dx
	jz bootcmd.error
	call skipwh0
	mov byte [bp + ?partition], dl
	mov byte [bp + ?unit], cl
	call iseol?
	jne .fn
.ret_nc:
	xor bx, bx		; NC, ZR
	jmp @F

.fn:
	call skipwh0
	or bx, 1		; NC, NZ
@@:
	 pop word [load_partition_sector]
	 pop word [load_partition_sector + 2]
				; pop from ?load_partition_sector
	pushf
	mov bx, word [bp + ?unit_low_partition_high]
	mov byte [load_unit], bl
	mov byte [load_partition], bh
	mov bl, byte [bp + ?dif3_set]
	or byte [internalflags3], bl
	popf			; ZF, CF
	jmp .ret

	lleave ctx


	align 4, db 0

boot_read:
	mov word [load_readwrite_function], read_sector
	jmp boot_readwrite

boot_write:
	mov word [load_readwrite_function], write_sector

boot_readwrite:

d4	call d4message
d4	asciz "In boot_readwrite",13,10

	call skipequals
	call parseloadunit_default_sdp
	jz .error

%if 0
	call skipwh0
	mov bx, word [reg_ds]	; default segment
	call getaddr		; get buffer address into bx:(e)dx

_386_PM	test edx, 0FFFF_0000h
_386_PM	jnz .error

		; (variable must be a dword!)
	mov word [load_readwrite_buffer], dx
	mov word [load_readwrite_buffer + 2], bx
%else
	call getword
		; (variable is a word)
	mov word [load_readwrite_buffer], dx
%endif

	xor bx, bx
	push bx			; hidden specified flag (0)
	push bx
	push bx			; hidden number
	call skipwh0
	call iseol?
	jne @F

d4	call d4message
d4	asciz "In boot_readwrite no sector given no count given",13,10

	and word [load_readwrite_sector], 0
	and word [load_readwrite_sector + 2], 0
	mov dx, 1
	jmp @FF

@@:
	mov dx, msg.hidden
	dec si
	call isstring?
	jne .nothidden

	add sp, 6		; discard hidden number and specified flag
	mov dx, 1

.hiddencommon:
	push dx			; hidden specified flag (1 or 2)

	call skipequals

	call getdword
	push bx
	push dx			; hidden number
	call skipwh0

	jmp .hiddendone
.nothidden:
	mov dx, msg.hiddenadd
	call isstring?
	jne .nothiddenadd

	add sp, 6		; discard hidden number and specified flag
	mov dx, 2
	jmp .hiddencommon

.nothiddenadd:
	lodsb
.hiddendone:
	call getexpression	; bx:dx = value

	mov word [load_readwrite_sector], dx
	mov word [load_readwrite_sector + 2], bx

	call skipwh0
	mov dx, 1
	call iseol?
	je @F
	call getword
	call chkeol
@@:
	mov word [load_readwrite_count], dx

	call query_geometry

	testopt [internalflags3], dif3_load_is_dp
	jnz .ldp

	cmp byte [load_partition], 0
	je .whole_unit

	mov byte [load_found_partition], 0
	mov cx, .single_partition
	call scan_partitions
;	cmp byte [load_found_partition], 0
;	jne @F
	mov dx, msg.boot_partition_not_found
	mov ax, 0215h
	call setrc
	jmp bootcmd.fail
;@@:
;	retn


.single_partition:
		; INP:	es:si -> partition table entry,
		;	 si = load_partition_table .. load_partition_table+48,
		;	 es = ss
		;	bp + di -> above part table metadata,
		;	 dwo [bp + di - 4] = root (outermost extended position)
		;	 dwo [bp + di - 8] = base (current table position)
		;; CHG:	ax, bx, (cx), dx
		; CHG:	all

d4	call d4message
d4	asciz "In boot_readwrite.single_partition",13,10

	mov al, byte [load_current_partition]
	cmp al, byte [load_partition]
	je .gotit
	retn

.gotit:
d4	call d4message
d4	asciz "In boot_readwrite.gotit",13,10

;	inc byte [load_found_partition]

	mov ax, [bp + di - 8]
	mov dx, [bp + di - 6]		; base (current table position)

	add ax, [es:si + 8]
	adc dx, [es:si + 8 + 2]		; add offset to logical partition

	mov sp, bp
	pop bp				; restore bp (scan_partitions)
	pop bx				; discard ret address (scan_partitions)
	jmp .gotbase_dxax


.ldp:
	mov ax, word [load_partition_sector]
	mov dx, word [load_partition_sector + 2]

.gotbase_dxax:
	mov word [bp + bsBPB + bpbHiddenSectors + 0], ax
	mov word [bp + bsBPB + bpbHiddenSectors + 2], dx
	setopt [internalflags3], dif3_partition_changed

.whole_unit:
	pop dx
	pop bx				; hidden number (or zero)
	pop cx				; hidden specified flag
	jcxz @FF			; if not specified -->
	dec cx				; hidden flag is 1 ?
	jz @F				; yes, replacce -->
	add dx, word [bp + bsBPB + bpbHiddenSectors + 0]
	adc bx, word [bp + bsBPB + bpbHiddenSectors + 2]
					; add to hidden (HIDDENADD=)

@@:
	mov word [bp + bsBPB + bpbHiddenSectors + 0], dx
	mov word [bp + bsBPB + bpbHiddenSectors + 2], bx
					; overwrite hidden number with this
@@:

	mov ax, word [load_readwrite_sector]
	mov dx, word [load_readwrite_sector + 2]
	mov cx, word [load_readwrite_count]
	mov bx, word [load_readwrite_buffer]

	jcxz @FF
@@:
	call near word [load_readwrite_function]
	loop @B
@@:
	retn

.error:
	jmp error


		; INP:	ds:si-> first letter of name
		;	es:load_kernel_name-> 12-byte buffer (for fn + 0)
		; CHG:	ax, cx, di
		; OUT:	al = first character after name (EOL, blank, or slash)
		;	si -> next character
boot_parse_fn:
	mov al, 32
	mov di, load_kernel_name
	mov cx, 11
	rep stosb		; initialise to empty

	mov di, load_kernel_name
	mov cx, 9
.loop_name:
	lodsb
	call uppercase
	call iseol?
	je .loop_name_done
	cmp al, 32
	je .loop_name_done
	cmp al, 9
	je .loop_name_done
	cmp al, '/'
	je .loop_name_done
	cmp al, '\'
	je .loop_name_done
	cmp al, '.'
	je .loop_name_ext
	stosb
	loop .loop_name
.invalid:
	mov dx, msg.boot_invalid_filename
	mov ax, 0216h
	call setrc
	jmp bootcmd.fail

.loop_name_ext:
	cmp cx, 9
	je .invalid
	mov cx, 4
	mov di, load_kernel_name + 8
.loop_ext:
	lodsb
	call uppercase
	call iseol?
	je .loop_ext_done
	cmp al, 32
	je .loop_ext_done
	cmp al, 9
	je .loop_ext_done
	cmp al, '/'
	je .loop_ext_done
	cmp al, '\'
	je .loop_ext_done
	cmp al, '.'
	je .invalid
	stosb
	loop .loop_ext
	jmp .invalid

.loop_ext_done:
	cmp cx, 4
	je .invalid
.loop_name_done:
	cmp cx, 9
	je .invalid
	mov byte [load_kernel_name + 11], 0
	cmp byte [load_kernel_name], 0E5h
	jne @F
	mov byte [load_kernel_name], 05h
@@:
	retn


		; INP:	es:si -> partition table entry,
		;	 si = load_partition_table .. load_partition_table+48,
		;	 es = ss
		;	bp + di -> above part table metadata,
		;	 dwo [bp + di - 4] = root (outermost extended position)
		;	 dwo [bp + di - 8] = base (current table position)
		; CHG:	ax, bx, (cx), dx
load_from_partition:
d4	call d4message
d4	asciz "In load_from_partition",13,10

	mov al, byte [load_current_partition]
	cmp al, byte [load_partition]
	je .gotit
	retn

.gotit:
d4	call d4message
d4	asciz "In load_from_partition.gotit",13,10

	mov ax, [bp + di - 8]
	mov dx, [bp + di - 6]		; base (current table position)

	push dx
	push ax
	push es
	mov bx, 60h
	call read_ae_512_bytes		; load partition table to 0:600h
	pop es
	pop ax
	pop dx

	add ax, [es:si + 8]
	adc dx, [es:si + 8 + 2]		; add offset to logical partition

	mov word [es:si + 8], ax
	mov word [es:si + 8 + 2], dx	; store in partition table entry

	xor cx, cx
	mov es, cx			; es = 0
	lea si, [si - (load_partition_table + DATASECTIONFIXUP) + 600h + (510 - 64)]
					; si = 600h + 510-64 .. 600h + 510-16
	mov word [es:si + 8], ax
	mov word [es:si + 8 + 2], dx	; store in partition table entry

					; dx:ax = absolute sector number
	mov bx, 7C0h			; bx:0 = 7C0h:0 -> boot sector area
	call read_ae_512_bytes		; load partition boot sector to 0:7C00h

	mov sp, bp
	pop bp				; restore bp (scan_partitions)
	pop ax				; discard ret address (scan_partitions)

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch

	xor cx, cx
	cmp word [es:0], cx
	je boot_codemismatch

	mov es, cx			; cx = 0, es = 0
	mov di, si			; di -> partition table entry (seg 0)
	or byte [es:di + 0], 80h	; set bootable flag
	mov al, byte [load_unit]	; al = unit
	mov bx, 7C00h			; bx = 7C00h
	jmp load_partition_common


		; INP:	es:si -> partition table entry,
		;	 si = load_partition_table .. load_partition_table+48,
		;	 es = ss
		;	bp + di -> above part table metadata,
		;	 dwo [bp + di - 4] = root (outermost extended position)
		;	 dwo [bp + di - 8] = base (current table position)
		; CHG:	ax, bx, (cx), dx
load_freedos_from_partition:
d4	call d4message
d4	asciz "In load_freedos_from_partition",13,10

	mov al, byte [load_current_partition]
	cmp al, byte [load_partition]
	je .gotit
	retn

.gotit:
d4	call d4message
d4	asciz "In load_freedos_from_partition.gotit",13,10

	mov ax, [bp + di - 8]
	mov dx, [bp + di - 6]		; base (current table position)

	add ax, [es:si + 8]
	adc dx, [es:si + 8 + 2]		; add offset to logical partition

	mov word [es:si + 8], ax
	mov word [es:si + 8 + 2], dx	; store in partition table entry
	or byte [es:si + 0], 80h	; set bootable flag


	testopt [load_options], LOAD_SET_DSSI_PARTINFO
	jz @F

	push dx
	push ax

	mov ax, [bp + di - 8]
	mov dx, [bp + di - 6]		; base (current table position)

	push dx
	push ax
	push es
	mov bx, 60h
	call read_ae_512_bytes		; load partition table to 0:600h
	pop es
	pop ax
	pop dx

	xor cx, cx
	mov es, cx			; es = 0
	lea di, [si - (load_partition_table + DATASECTIONFIXUP) + 600h + (510 - 64)]
					; si = 600h + 510-64 .. 600h + 510-16
	mov cl, 16 / 2
	push di
	rep movsw

	pop word [load_partition_entry]

	pop ax
	pop dx
@@:


	mov sp, bp
	pop bp				; restore bp (scan_partitions)
	pop bx				; discard ret address (scan_partitions)

					; dx:ax = absolute sector number
	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	push ax
	push dx
	call read_ae_512_bytes		; load partition boot sector

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch

	xor cx, cx
;	cmp word [es:0], cx
;	je boot_codemismatch

	pop dx
	pop ax

		; dx:ax = boot sector
		; byte [load_unit] = unit
		; es:0-> read sector
load_freedos_common:
	mov word [es:bsBPB + bpbHiddenSectors], ax
	mov word [es:bsBPB + bpbHiddenSectors + 2], dx

	mov bx, [bp + bsBPB + bpbBytesPerSector]
	cmp bx, [es:bsBPB + bpbBytesPerSector]
	jne boot_secsizemismatch

		; preserve some variables from our pseudo BPB
	xor ax, ax
	push word [bp + bsBPB + bpbCHSSectors]
	pop word [es:bsBPB + bpbCHSSectors]
	push word [bp + bsBPB + bpbCHSHeads]
	pop word [es:bsBPB + bpbCHSHeads]	; preserve geometry

	mov bx, word [bp + ldParaPerSector]
	shr bx, 1
	mov word [bp + ldEntriesPerSector], bx

	cmp word [es:bsBPB + bpbSectorsPerFAT], ax
	mov bl, byte [bp + bsBPB + ebpbNew + bpbnBootUnit]
	je .is_fat32
	mov byte [es:bsBPB + bpbNew + bpbnBootUnit], bl
	jmp short .was_fat1612
.is_fat32:
	mov byte [es:bsBPB + ebpbNew + bpbnBootUnit], bl
.was_fat1612:

	 push es
	 push ds
	push es
	pop ds
	xor si, si				; -> BPB from boot partition
	push ss
	pop es
	mov di, load_data - LOADDATA2		; -> our copy of a BPB
	mov cx, (bsBPB + ebpbNew + BPBN_size)
	rep movsb				; get the BPB

	 pop ds
	setopt [internalflags3], dif3_partition_changed

	cmp word [bp + bsBPB + bpbSectorsPerFAT], ax
	je @F					; is FAT32 -->
	mov si, load_data - LOADDATA2 + bsBPB + bpbNew
	mov di, load_data - LOADDATA2 + bsBPB + ebpbNew
	mov cx, BPBN_size
	rep movsb				; clone the FAT16 / FAT12 BPBN
						; to where the FAT32 BPBN lives
@@:
	 pop es

	call bootgetmemorysize
	mov word [bp + ldMemoryTop], dx
	sub dx, (20 * 1024) >> 4
		; leave 20 KiB free at the top, to
		; allow loading with the lDOS protocol (needs BPB and FAT seg
		; to live below its destination buffers for these)
	jnc @F
.outofmem:
	jmp query_geometry.out_of_memory_error
@@:

	sub dx, 8192 >> 4
	jc .outofmem
	mov word [bp + lsvFATSeg], dx
	mov ax, -1
	mov word [bp + lsvFATSector], ax
	mov word [bp + lsvFATSector + 2], ax

	push word [load_loadseg]
	pop word [bp + lsvLoadSeg]

	xor bx, bx
	mov cx, ((- LOADSTACKVARS + 15 + 4096) & ~15) >> 1
	testopt [load_options], LOAD_NO_BPB
	jnz @F
	mov bx, 512
	mov cx, ((512 - LOADSTACKVARS + 15 + 4096) & ~15) >> 1
@@:

	cmp word [load_bpb + 2], -1
	je .auto_bpb

	mov ax, [load_bpb]
	shr ax, 1
	shr ax, 1
	shr ax, 1
	shr ax, 1		; round down: start of BPB
	add ax, [load_bpb + 2]	; start of BPB
	sub ax, (4096 - LOADSTACKVARS + 15) >> 4
				; start of stack area
	push ax
	jc .bpb_too_low

	cmp ax, word [bp + lsvLoadSeg]
	ja .loads_below_bpb

	cmp ax, 60h
	jb .bpb_too_low

	mov ax, [load_bpb]
	add ax, bx
	add ax, 15
	shr ax, 1
	shr ax, 1
	shr ax, 1
	shr ax, 1
	add ax, [load_bpb + 2]	; end of BPB / pseudo-boot-sector

	cmp ax, word [bp + lsvLoadSeg]
	jbe .loads_above_bpb

	mov dx, msg.boot_bpb_load_overlap
	mov ax, 0217h
	call setrc
.fail:
	mov ax, 0218h
	call setrc
	jmp bootcmd.fail

.bpb_too_low:
	mov dx, msg.boot_bpb_too_low
	mov ax, 0219h
	call setrc
	jmp .fail



.loads_below_bpb:
	mov dx, ax		; set load top to before BPB/lsv/stack

.loads_above_bpb:		; dx = word [bp + ldLoadTop] = word [bp + lsvFATSeg]
	push word [load_bpb + 2]
	pop word [load_bpb_dest + 2]
	push word [load_bpb]
	pop word [load_bpb_dest]
	jmp .got_bpb_set_load_top


		; auto-BPB: allocate BPB at top and load below that
.auto_bpb:
	sub dx, (-LOADSTACKVARS + 15 + 4096) >> 4
	jc .outofmem
	test bx, bx
	jz @F
	sub dx, 512 >> 4
	jc .outofmem
@@:

	push dx
	mov ax, dx
			; eg dx = 800h
			; want (((800h<<4) + 4096 - LOADSTACKVARS) - 7C00h) >> 4
			; which is 143h
			; which :7C00h = 903h:0
			; dx + ( 4096 - LOADSTACKVARS - 7C00h) / 16
			; dx - (-4096 + LOADSTACKVARS + 7C00h) / 16
	mov bx, (-4096 +LOADSTACKVARS)
	add bx, word [load_bpb]
	push cx
	mov cl, 4
	shr bx, cl
	pop cx
	sub ax, bx
	; sub ax, (-4096 +LOADSTACKVARS + 7C00h) / 16
	jc .outofmem
	; mov word [load_bpb_dest], 7C00h
	push word [load_bpb]
	pop word [load_bpb_dest]
	mov word [load_bpb_dest + 2], ax
.got_bpb_set_load_top:
	mov word [bp + ldLoadTop], dx

.got_bpb:		; cx = how many words are used for stack/lsv/BPB
	pop ax		; -> stack area
	push es
	mov es, ax
	xor di, di
	xor ax, ax
	rep stosw
	pop es

	call initialise_fs

	mov byte [load_check_dir_attr], 0
	mov si, word [load_kernelname_input]
	cmp byte [si], '/'
	jne @F
	inc si
@@:
	cmp byte [si], 0
	jne @F
	mov si, word [load_kernelname_default]
@@:
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_kernelname_next], si
@@:

	mov di, -1
	mov si, di
	mov [bp + lsvFATSector], di
	mov [bp + lsvFATSector + 2], si

	xor ax, ax
	xor dx, dx

scan_dir_kernelname_loop:
	mov word [bp + ldDirCluster], ax
	mov word [bp + ldDirCluster + 2], dx

	xor bx, bx
	mov es, bx
	mov bx, 500h
	call scan_dir_aux_for_file

	cmp byte [load_check_dir_attr], ATTR_DIRECTORY
	jne got_kernelentry

	push si
	push di
	mov byte [load_check_dir_attr], 0
	mov si, word [load_kernelname_next]
	cmp byte [si], 0
	jne @F
	mov si, word [load_kernelname_default]
@@:
	push es
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	pop es
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_kernelname_next], si
@@:
	pop di
	pop si

	xor dx, dx
	mov ax, [es:bx + deClusterLow]
				; = first cluster (not FAT32)
	cmp byte [bp + ldFATType], 32
	jne @F
	mov dx, [es:bx + deClusterHigh]
				; dx:ax = first cluster (FAT32)
@@:

	jmp scan_dir_kernelname_loop


got_kernelentry:
	push si
	push di

	mov byte [load_check_dir_attr], 0
	mov si, word [load_addname_input]
	cmp byte [si], '/'
	jne @F
	inc si
	cmp byte [si], '/'
	je got_no_addentry.no_dxax_on_stack

	xor ax, ax
	mov word [bp + ldDirCluster], ax
	mov word [bp + ldDirCluster + 2], ax
				; search from root directory
@@:
	cmp byte [si], 0
	jne @F
	mov si, word [load_addname_default]
	cmp byte [si], 0
	je got_no_addentry.no_dxax_on_stack
@@:
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_kernelname_next], si
@@:
	pop di
	pop si
	jmp @F

scan_dir_addname_loop:
	mov word [bp + ldDirCluster], ax
	mov word [bp + ldDirCluster + 2], dx

@@:
	xor bx, bx
	mov es, bx
	mov bx, 520h	;  0:bx -> space for second directory entry
	call scan_dir_aux_for_file

	cmp byte [load_check_dir_attr], ATTR_DIRECTORY
	jne got_addentry

	push si
	push di
	push dx
	push ax
	mov byte [load_check_dir_attr], 0
	mov si, word [load_addname_next]
	cmp byte [si], 0
	jne @F
	mov si, word [load_addname_default]
	cmp byte [si], 0
	je got_no_addentry
@@:
	push es
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	pop es
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_addname_next], si
@@:
	pop ax
	pop dx
	pop di
	pop si

	xor dx, dx
	mov ax, [es:bx + deClusterLow]
				; = first cluster (not FAT32)
	cmp byte [bp + ldFATType], 32
	jne @F
	mov dx, [es:bx + deClusterHigh]
				; dx:ax = first cluster (FAT32)
@@:

	jmp scan_dir_addname_loop


helper_shift_down_and_clamp:
		mov cx, 4
@@:
		shr dl, 1
		rcr ax, 1
		rcr bx, 1
		loop @B

		or al, dl
		mov dx, bx		; size in paragraphs
		test ax, ax		; > 0FFFFh ?
		jz @F			; no, take actual size -->
		mov dx, 0FFFFh		; clamp to 0FFFFh
@@:
		retn

got_no_addentry:
	pop ax
	pop dx
.no_dxax_on_stack:
;	push ax
	mov cx, 16
	xor ax, ax
	mov es, ax
	mov di, 520h	; es:di -> space for second directory entry
	rep stosw	; store zeros
;	pop ax

	pop di
	pop si

got_addentry:
	xor ax, ax
	mov es, ax


; (boot32.asm code starts here)

	mov ax, word [bp + ldLoadTop]
	sub ax, word [bp + ldParaPerSector]
	jc load_freedos_common.outofmem
	mov [bp + ldLastAvailableSector], ax

		mov bx, [es:500h + deSize]
		mov ax, [es:500h + deSize + 2] ; ax:bx = file size
		mov dl, [es:500h + 12]	; dl = FAT+ size bits
		mov dh, dl
		and dx, 0E007h		; obtain bits 7-5 and 2-0
		shr dh, 1
		shr dh, 1
		or dl, dh		; dl:ax:bx = file size
		push dx
		push ax
		push bx

		call helper_shift_down_and_clamp
					; round down to next paragraph boundary
		cmp word [load_minpara], dx
		ja error_filetoosmall

		pop bx
		pop ax
		pop dx
		mov cx, [bp + bsBPB + bpbBytesPerSector]
		dec cx			; BpS - 1
		add bx, cx
		adc ax, 0
		adc dl, 0		; round up to next sector
		not cx			; ~ (BpS - 1)
		and bx, cx		; mask to limit to rounded-up sector
		call helper_shift_down_and_clamp

			; dl:ax:bx = size in paragraphs
		mov ax, word [load_maxpara]
		cmp dx, ax		; actual size below maximum ?
		jbe @F			; yes, use actual size -->
		mov dx, ax		; use maximum size
@@:
		mov word [bp + ldParasLeft], dx
		mov word [bp + ldParasDone], 0

; get starting cluster of file
		xor dx, dx
		mov ax, [es:500h + deClusterLow]
					; = first cluster (not FAT32)

		cmp byte [bp + ldFATType], 32
		jne @F
		mov dx, [es:500h + deClusterHigh]
					; dx:ax = first cluster (FAT32)
@@:

		mov word [bp + lsvFirstCluster], ax
		mov word [bp + lsvFirstCluster + 2], dx

		call check_clust
		jc error_badchain

next_load_cluster:
		call clust_to_first_sector
			; dx:ax = first sector of cluster
			; cx:bx = cluster value
		push cx
		push bx			; preserve cluster number for later

		mov cx, [bp + ldClusterSize]

		mov bx, [bp + lsvLoadSeg]
; xxx - this will always load an entire cluster (e.g. 64 sectors),
; even if the file is shorter than this
@@:
		cmp bx, [bp + ldLastAvailableSector]
		jbe @F
		cmp word [load_maxpara], 0
		je @FF			; if to allow partial load -->
		mov dx, msg.boot_file_too_big_error
		mov ax, 021Ah
		call setrc
		jmp bootcmd.fail

@@:
		push es		; (must preserve ADR_FATBUF reference)
		call read_sector
		pop es
		mov [bp + lsvLoadSeg], bx	; => after last read data

		push ax
		mov ax, [bp + ldParaPerSector]
		add word [bp + ldParasDone], ax
		cmp word [load_maxpara], 0
		je .donotuseleft
		sub word [bp + ldParasLeft], ax
		pop ax
		jbe @F		; read enough -->
		db __TEST_IMM8	; (skip pop)
.donotuseleft:
		pop ax

		loop @BB
		pop bx
		pop cx

		call clust_next
		jnc next_load_cluster
		inc ax
		inc ax
		test al, 8	; set in 0FFF_FFF8h--0FFF_FFFFh,
				;  clear in 0, 1, and 0FFF_FFF7h
		jz error_badchain
		db __TEST_IMM16
@@:
		pop bx
		pop cx

		mov ax, word [load_minpara]
		cmp ax, word [bp + ldParasDone]
		jbe @F
error_filetoosmall:
		mov dx, msg.boot_file_too_small_error
		mov ax, 021Bh
		call setrc
		jmp bootcmd.fail
@@:


	mov es, word [load_loadseg]
	mov di, word [load_check_offset]
	mov ax, word [load_check_value]
	test ax, ax
	jz @F
	scasw
	jne .error_check_mismatch
@@:

; turn off floppy motor
		mov dx,3F2h
		mov al,0
		out dx,al

; Set-up registers for and jump to loaded file

		mov dl, [bp + bsBPB + ebpbNew + bpbnBootUnit]
;		testopt [load_options], LOAD_SET_DL_UNIT
;		jz @F
	; (always set dl)
		mov byte [reg_edx], dl
@@:

;		testopt [load_options], LOAD_SET_BL_UNIT
;		jz @F
	; (always set bl -- overwritten later if LOAD_SET_AXBX_DATASTART)
		mov byte [reg_ebx], dl
@@:

		mov ch, byte [bp + bsBPB + bpbMediaID]
		mov byte [reg_ecx + 1], ch

		testopt [load_options], LOAD_DATASTART_HIDDEN
		jz @F
		mov bx, [bp + bsBPB + bpbHiddenSectors]
		mov ax, [bp + bsBPB + bpbHiddenSectors + 2]
		add word [bp + lsvDataStart], bx
		adc word [bp + lsvDataStart + 2], ax
@@:

		testopt [load_options], LOAD_SET_AXBX_DATASTART
		jz @F
		mov bx, word [bp + lsvDataStart]
		mov ax, word [bp + lsvDataStart + 2]
		mov word [reg_ebx], bx
		mov word [reg_eax], ax
@@:

		testopt [load_options], LOAD_SET_AXBX_ROOT_HIDDEN
		jz @F
		mov bx, word [bp + ldRootSector]
		mov ax, word [bp + ldRootSector + 2]
		add bx, word [bp + bsBPB + bpbHiddenSectors]
		adc ax, word [bp + bsBPB + bpbHiddenSectors + 2]
		mov word [reg_ebx], bx
		mov word [reg_eax], ax
@@:

		testopt [load_options], LOAD_SET_SIDI_CLUSTER
		jz @F
		mov dx, word [bp + lsvFirstCluster + 2]
		mov ax, word [bp + lsvFirstCluster]
		mov word [reg_esi], dx
		mov word [reg_edi], ax
@@:

; (boot.asm code ends here)


	les di, [load_bpb_dest]
	push di
	sub di, -LOADSTACKVARS
	mov si, load_data - LOADDATA2 + LOADSTACKVARS
	mov cx, -LOADSTACKVARS
	testopt [load_options], LOAD_NO_BPB
	jnz .no_bpb_movsb
	mov cx, -LOADSTACKVARS + bsBPB + bpbNew
	rep movsb		; move common BPB part

	cmp word [bp + bsBPB + bpbSectorsPerFAT], cx
	mov cx, ebpbNew - bpbNew + BPBN_size	; move FAT32 EBPB part + BPBN
	je @F
	add si, ebpbNew - bpbNew; -> BPBN
	mov cx, BPBN_size	; move only BPBN
@@:
	rep movsb

	mov ax, di
	pop di
	sub ax, di
	dec ax
	dec ax
	xchg al, ah
	mov al, 0EBh
	mov word [es:di], ax
	mov byte [es:di + 2], 90h

		testopt [load_options], LOAD_LBA_SET_TYPE
		jz @F
	test byte [bp + ldFlags], ldfHasLBA
	jz @F

	mov byte [es:di + 2], 0Eh	; (LBA-enabled) FAT16 FS partition type
	cmp byte [bp + ldFATType], 32
	jb @F
	mov byte [es:di + 2], 0Ch	; (LBA-enabled) FAT32 FS partition type
@@:

	mov word [es:di + 510], 0AA55h
	 push ds
	xor cx, cx
	mov ds, cx
	mov si, 500h
	 push di
	add di, 512 - 2 - 2 - 14 - 12 - 12
			; 2: AA55 sig, 2: null word, 14: MS-DOS 7 protocol
			;  message table pointer lives here,
			;  12: add name, 12: kernel name
	mov cl, 11
	rep movsb	; put kernel filename into the pseudo boot sector
	inc di
	mov si, 520h
	mov cl, 11
	rep movsb	; put additional filename (if any), cx = 0
	 pop di
	 pop ds

			; cx = 0
	db __TEST_IMM8	; (skip pop)
.no_bpb_movsb:
	pop ax		; discard word on stack
	rep movsb

	and word [reg_efl], ~(400h|200h|100h)	; UP, DI, TF=0

	mov word [reg_eip + 2], cx
	push word [load_entrypoint]
	pop word [reg_eip]
	mov ax, word [load_entrypoint + 2]
	add ax, word [load_loadseg]
	mov word [reg_cs], ax

	mov ax, word [load_bpb_dest + 2]
	mov word [reg_ss], ax
;	testopt [load_options], LOAD_SET_DSBP_BPB
;	jz @F
		; (always set ds -- overwritten later if LOAD_SET_DSSI_DPT)
	mov word [reg_ds], ax
@@:
	mov bx, word [load_bpb_dest]
	mov word [reg_ebp + 2], cx
	mov word [reg_ebp], bx
	mov ax, bx
	sub bx, -LOADSTACKVARS	; (subtracts --10h)
	mov word [reg_esp + 2], cx
	mov word [reg_esp], bx

	testopt [load_options], LOAD_NO_BPB
	jnz @F

		testopt [load_options], LOAD_MESSAGE_TABLE
		jz @F
	mov cx, (bsBPB + ebpbNew + BPBN_size + 2 + 15) & ~15
	add ax, cx
	mov word [es:di + 1EEh], ax
		; this pointer points to the MS-DOS 7 message table.
		;
		; note that in actual MS-DOS 7 boot sectors, this value is
		; eg 17Fh, which is incorrectly used with the boot sector's
		; ss to load the table into the initial loader.
		;
		; refer to comments in msg.asm about msdos7_message_table.
	mov si, msdos7_message_table
	add di, cx
	mov cx, msdos7_message_table.size
	rep movsb
@@:

	testopt [load_options], LOAD_CMDLINE
	jz .no_cmdline

	mov si, word [load_cmdline]
	test si, si
	jnz @F
	mov si, load_cmdline
@@:

		; due to the size of our line_in buffer,
		;  the command line is never too long for
		;  the lsv command line buffer (256 bytes).
	mov cx, lsvclBufferLength / 2
	sub word [reg_esp], - lsvCommandLine.start + LOADSTACKVARS
		; hazard: if sp is too low, this underflows!
	jc .error_stack_underflow
	mov es, [reg_ss]
	mov di, [reg_esp]	; es:di -> stack area for the pointers
	cmp di, 256
	jb .error_stack_underflow
	push di
	rep movsw
	mov byte [es:di - 1], cl; truncate command line if too long
	mov ax, lsvclSignature
	stosw			; write lsvCommandLine.signature
	xor ax, ax
	stosw			; write lsvExtra
	pop di
	mov cx, lsvclBufferLength
	repne scasb		; search terminator
	rep stosb		; zero buffer behind terminator
.no_cmdline:

		xor ax, ax
		mov es, ax
		mov di, 1Eh * 4
		mov dx, word [es:di + 2]
		mov bx, word [es:di]

		testopt [load_options], LOAD_SET_DSSI_DPT
		jz @F
		mov word [reg_ds], dx
		mov word [reg_esi], bx
@@:
		testopt [load_options], LOAD_PUSH_DPT
		jz @F

		 push es
		 push di
		sub word [reg_esp], 4 * 2	; push four words
		mov es, [reg_ss]
		mov di, [reg_esp]	; es:di -> stack area for the pointers
		 pop ax		; di (1Eh * 4)
		stosw
		 pop ax		; es (0)
		stosw
		mov ax, bx
		stosw		; si (Int1E offset)
		mov ax, dx
		stosw		; ds (Int1E segment)
@@:

	testopt [load_options], LOAD_SET_DSSI_PARTINFO
	jz @F
	mov ax, word [load_partition_entry]
	and word [reg_ds], 0
	mov word [reg_esi], ax
	testopt [load_options], LOAD_NO_BPB
	jz @F
	mov word [reg_ebp], ax
@@:
	setopt [internalflags2], dif2_boot_loaded_kernel
	retn


.error_check_mismatch:
	dec di
	dec di			; = offset into file
	push word [es:di]	; = value we got in file
	push di			; = offset
	 push ss
	 pop es			; set STT
	mov di, msg.bootfail_check_mismatch.check_value
	call hexword		; write expected value
	pop ax
	mov di, msg.bootfail_check_mismatch.check_offset
	call hexword		; write offset
	pop ax
	mov di, msg.bootfail_check_mismatch.check_got
	call hexword		; write what we got in file

	mov dx, msg.bootfail_check_mismatch
	mov ax, 021Ch
	call setrc
	jmp @F			; fail with error message

.error_stack_underflow:
	mov dx, msg.bootfail_stack_underflow
	mov ax, 021Dh
	call setrc
@@:
	jmp bootcmd.fail


boot_dir:
	clropt [internalflags3], dif3_load_dir_dir
	call skipwhite
	dec si
	mov dx, msg.dir
	call isstring?
	jne @F
	setopt [internalflags3], dif3_load_dir_dir
@@:
	mov word [load_kernelname_input], msg.emptydirname
	lodsb
	call parseloadunit_default_sdp
	jz .fn_done_eol		; no filename given, use defaults -->
		; al was = '/' or '\' or first pathname's first character
		; si-> next char
	mov bx, load_kernelname_input
	call bootcmd.pathname_parse_super
	call chkeol

.fn_done_eol:
	testopt [internalflags3], dif3_load_is_dp
	jnz .boot_dir_from_ldp

	mov bl, [load_partition]
	cmp byte [load_unit], 80h
	jb .boot_dir_is_diskette
	test bl, bl		; partition specified ?
	jz bootcmd.error	; no, error -->

	call query_geometry

	mov cx, boot_dir_from_partition
	call scan_partitions
	mov dx, msg.boot_partition_not_found
	mov ax, 0211h
	call setrc
	jmp bootcmd.fail


.boot_dir_from_ldp:
	call query_geometry
	mov ax, word [load_partition_sector]
	mov dx, word [load_partition_sector + 2]
	jmp @F


.boot_dir_is_diskette:
	test bl, bl		; partition specified ?
	jnz bootcmd.error	; yes, error -->

	call query_geometry

	xor ax, ax
	xor dx, dx
@@:
	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	 push dx
	 push ax
	call read_ae_512_bytes

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch
	 pop ax
	 pop dx

	jmp boot_dir_common


		; INP:	es:si -> partition table entry,
		;	 si = load_partition_table .. load_partition_table+48,
		;	 es = ss
		;	bp + di -> above part table metadata,
		;	 dwo [bp + di - 4] = root (outermost extended position)
		;	 dwo [bp + di - 8] = base (current table position)
		; CHG:	ax, bx, (cx), dx
boot_dir_from_partition:
d4	call d4message
d4	asciz "In boot_dir_from_partition",13,10

	mov al, byte [load_current_partition]
	cmp al, byte [load_partition]
	je .gotit
	retn

.gotit:
d4	call d4message
d4	asciz "In boot_dir_from_partition.gotit",13,10

	mov ax, [bp + di - 8]
	mov dx, [bp + di - 6]		; base (current table position)

	add ax, [es:si + 8]
	adc dx, [es:si + 8 + 2]		; add offset to logical partition

	mov word [es:si + 8], ax
	mov word [es:si + 8 + 2], dx	; store in partition table entry
	or byte [es:si + 0], 80h	; set bootable flag

	mov sp, bp
	pop bp				; restore bp (scan_partitions)
	pop bx				; discard ret address (scan_partitions)

					; dx:ax = absolute sector number
	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	push ax
	push dx
	call read_ae_512_bytes		; load partition boot sector

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch

	xor cx, cx
;	cmp word [es:0], cx
;	je boot_codemismatch

	pop dx
	pop ax

		; dx:ax = boot sector
		; byte [load_unit] = unit
		; es:0-> read sector
boot_dir_common:
	mov word [es:bsBPB + bpbHiddenSectors], ax
	mov word [es:bsBPB + bpbHiddenSectors + 2], dx

	mov bx, [bp + bsBPB + bpbBytesPerSector]
	cmp bx, [es:bsBPB + bpbBytesPerSector]
	jne boot_secsizemismatch

		; preserve some variables from our pseudo BPB
	xor ax, ax
	push word [bp + bsBPB + bpbCHSSectors]
	pop word [es:bsBPB + bpbCHSSectors]
	push word [bp + bsBPB + bpbCHSHeads]
	pop word [es:bsBPB + bpbCHSHeads]	; preserve geometry

	mov bx, word [bp + ldParaPerSector]
	shr bx, 1
	mov word [bp + ldEntriesPerSector], bx

	cmp word [es:bsBPB + bpbSectorsPerFAT], ax
	mov bl, byte [bp + bsBPB + ebpbNew + bpbnBootUnit]
	je .is_fat32
	mov byte [es:bsBPB + bpbNew + bpbnBootUnit], bl
	jmp short .was_fat1612
.is_fat32:
	mov byte [es:bsBPB + ebpbNew + bpbnBootUnit], bl
.was_fat1612:

	 push es
	 push ds
	push es
	pop ds
	xor si, si				; -> BPB from boot partition
	push ss
	pop es
	mov di, load_data - LOADDATA2		; -> our copy of a BPB
	mov cx, (bsBPB + ebpbNew + BPBN_size)
	rep movsb				; get the BPB

	 pop ds
	setopt [internalflags3], dif3_partition_changed

	cmp word [bp + bsBPB + bpbSectorsPerFAT], ax
	je @F					; is FAT32 -->
	mov si, load_data - LOADDATA2 + bsBPB + bpbNew
	mov di, load_data - LOADDATA2 + bsBPB + ebpbNew
	mov cx, BPBN_size
	rep movsb				; clone the FAT16 / FAT12 BPBN
						; to where the FAT32 BPBN lives
@@:
	 pop es

	push word [auxbuff_segorsel]
	pop word [bp + lsvFATSeg]
	or byte [bp + ldFlags], ldfFATInvalid
	mov ax, -1
	mov word [bp + lsvFATSector], ax
	mov word [bp + lsvFATSector + 2], ax

	call initialise_fs

	mov word [handle_scan_dir_entry], scan_dir_entry

	mov byte [load_check_dir_attr], 0
	mov si, word [load_kernelname_input]
	cmp byte [si], '/'
	jne @F
	inc si
@@:
	cmp byte [si], 0
	je .root
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_kernelname_next], si
@@:
	test al, al
	jnz @F
	mov word [handle_scan_dir_entry], scan_dir_entry_dir_or_file
@@:

	mov di, -1
	mov si, di
	mov [bp + lsvFATSector], di
	mov [bp + lsvFATSector + 2], si

	xor ax, ax
	xor dx, dx

	sub sp, 32

.scan_dir_dirname_loop:
	mov word [bp + ldDirCluster], ax
	mov word [bp + ldDirCluster + 2], dx

	push ss
	pop es
	mov bx, sp
	call scan_dir_aux_with_error

	cmp byte [load_check_dir_attr], ATTR_DIRECTORY
	jne .got_direntry

	push si
	push di
	mov byte [load_check_dir_attr], 0
	mov si, word [load_kernelname_next]
	cmp byte [si], 0
	je .sub

	push es
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	pop es
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_kernelname_next], si
@@:
	test al, al
	jnz @F
	mov word [handle_scan_dir_entry], scan_dir_entry_dir_or_file
@@:

	pop di
	pop si

	xor dx, dx
	mov ax, [es:bx + deClusterLow]
				; = first cluster (not FAT32)
	cmp byte [bp + ldFATType], 32
	jne @F
	mov dx, [es:bx + deClusterHigh]
				; dx:ax = first cluster (FAT32)
@@:

	jmp .scan_dir_dirname_loop

.got_direntry:
	testopt [internalflags3], dif3_load_dir_dir
	jnz @F
	test byte [es:bx + deAttrib], ATTR_DIRECTORY
	jnz @FF
@@:
	mov di, bx
	call list_dir_entry
	add sp, 32
	retn

.root:
	xor ax, ax
	xor dx, dx
	jmp .scan

.sub:
	pop di
	pop si

@@:
	add sp, 32

	xor dx, dx
	mov ax, [es:bx + deClusterLow]
				; = first cluster (not FAT32)
	cmp byte [bp + ldFATType], 32
	jne @F
	mov dx, [es:bx + deClusterHigh]
				; dx:ax = first cluster (FAT32)
@@:
.scan:
	mov word [bp + ldDirCluster], ax
	mov word [bp + ldDirCluster + 2], dx

	mov word [handle_scan_dir_entry], list_dir_entry
	mov word [handle_scan_dir_not_found], dmycmd

	jmp scan_dir_aux


list_dir_entry:
	cmp byte [es:di], 0
	je .ret			; (NC, ZR)
	cmp byte [es:di], 0E5h
	jne @F
	or bl, 1		; (NC, NZ)
	retn

@@:
	mov bl, byte [es:di + deAttrib]
	test bl, ATTR_VOLLABEL
	jnz .ret_NC_NZ		; skip volume labels (and LFNs) --> (NZ)

	cmp byte [es:di], '.'	; dot or dotdot entry ?
	je .ret_NC_NZ		; yes, skip -->

	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	 push es
	 pop ds
	mov si, di

	 push ss
	 pop es
	mov di, line_out
	mov cx, 8
	rep movsb
	mov ax, 2020h
	stosb
	mov cl, 3
	rep movsb
	stosw

	pop ds
	mov al, '-'
	test bl, ATTR_ARCHIVE
	jz @F
	mov al, 'A'
@@:
	stosb
	mov al, '-'
	test bl, ATTR_HIDDEN
	jz @F
	mov al, 'H'
@@:
	stosb
	mov al, '-'
	test bl, ATTR_READONLY
	jz @F
	mov al, 'R'
@@:
	stosb
	mov al, '-'
	test bl, ATTR_SYSTEM
	jz @F
	mov al, 'S'
@@:
	stosb
	mov ax, 2020h
	stosw

	test bl, ATTR_DIRECTORY
	jz @F
	mov si, msg.dirinsteadsize
	call copy_single_counted_string
	jmp @FF

@@:
	 pop es
	 pop si
	 push si
	 push es
	mov al, byte [es:si + dePlusSize]
	mov ah, al
	and ax, 0E007h
	mov dl, ah
	mov ah, 0
	mov dh, 0
	shr dx, 1
	shr dx, 1
	or ax, dx
	 push ax
	mov dx, word [es:si + deSize + 2]
	mov ax, word [es:si + deSize]
	 pop si
	 push ss
	 pop es
	xor cx, cx
	mov bx, 4+4
	call disp_dxax_times_cx_width_bx_size.store

@@:
	mov ax, 2020h
	stosw
	 pop es
	 pop si
	 push si
	 push es
	mov bx, word [es:si + deDate]
	mov si, word [es:si + deTime]
	 push ss
	 pop es
	mov ax, bx
	mov cx, 9
	shr ax, cl
	add ax, 1980
	xor dx, dx
	mov cl, 4
	call dec_dword_minwidth
	mov al, '-'
	stosb
	mov ax, bx
	mov cl, 5
	shr ax, cl
	and ax, 15
	mov cl, 2
	call dec_dword_minwidth
	mov al, '-'
	stosb
	mov ax, bx
	and ax, 31
	call dec_dword_minwidth
	mov al, 32
	stosb
	mov ax, si
	mov cl, 11
	shr ax, cl
	mov cl, 2
	call dec_dword_minwidth
	mov al, ':'
	stosb
	mov ax, si
	mov cl, 5
	shr ax, cl
	and ax, 63
	mov cl, 2
	call dec_dword_minwidth
	mov al, ':'
	stosb
	mov ax, si
	and ax, 31
	shl ax, 1
	call dec_dword_minwidth

	call putsline_crlf

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax

.ret_NC_NZ:
	or bl, 1		; (NC, NZ)
.ret:
	retn


	usesection lDEBUG_DATA_ENTRY

	align 2, db 0
handle_scan_dir_entry:
	dw error
handle_scan_dir_not_found:
	dw error


	usesection lDEBUG_CODE

scan_dir_entry_dir_or_file:
	mov bh, 1
	db __TEST_IMM16		; (skip mov)

scan_dir_entry:
	mov bh, 0
	cmp byte [es:di], 0
	stc
	je .ret
	mov bl, byte [es:di + deAttrib]
	test bl, ATTR_VOLLABEL
	jnz @F			; skip volume labels (and LFNs) --> (NZ)
	test bh, bh
	jnz .no_check_dir
	and bl, ATTR_DIRECTORY	; isolate directory bit
	cmp bl, byte [load_check_dir_attr]	; is it what we're searching?
	jne @F			; no -->
.no_check_dir:
	push si
	push di
	push cx
	mov si, load_kernel_name	; ds:si-> name to match
	mov cx, 11		; length of padded 8.3 FAT filename
	repe cmpsb		; check entry
	pop cx
	pop di
	pop si
@@:
	clc
.ret:
	retn


		; INP:	es:bx -> where to place directory entry
		;	si:di = loaded FAT sector (0 = first FAT sector)
		;	dword [bp + ldDirCluster] = directory cluster to scan,
		;					0 for root dir
		;	byte [bp + ldFATType] = size of FAT entry in bits
		; OUT:	es:bx -> directory entry (es:bx unchanged)
		;	si:di = loaded FAT sector
		; CHG:	dx, ax, si, di, cx
scan_dir_aux_for_file:
	mov word [handle_scan_dir_entry], scan_dir_entry

scan_dir_aux_with_error:
	mov word [handle_scan_dir_not_found], error_filenotfound

scan_dir_aux:
	push word [auxbuff_segorsel]
	pop word [load_adr_dirbuf_segment]

scan_dir:
	mov ax, word [bp + ldDirCluster]
	mov dx, word [bp + ldDirCluster + 2]

	test ax, ax
	jnz fat32_scan_root.dir_clust_dxax
	test dx, dx
	jnz fat32_scan_root.dir_clust_dxax

		; got to scan root directory. use FAT12/FAT16 walker if so,
		;  else use FAT32 walker

	cmp byte [bp + ldFATType], 16
	ja fat32_scan_root

	push si
	push di
	push es
	push bx

	mov si, word [bp + bsBPB + bpbNumRootDirEnts]


; (boot.asm code starts here)

fat16_scan_root:
	test si, si
	jz handle_filenotfound_fat16

	mov ax, [bp + ldRootSector]
	mov dx, [bp + ldRootSector + 2]

; Scan root directory for file. We don't bother to check for deleted
;  entries (E5h) or entries that mark the end of the directory (00h).
		; number of root entries in si here
fat16_next_sect:
	mov bx, [load_adr_dirbuf_segment]
	call read_sector

	mov cx, [bp + ldEntriesPerSector] ; entries per sector as loop counter
	xor di, di		; es:di-> first entry in this sector
fat16_next_ent:
	call near word [handle_scan_dir_entry]
	jc handle_filenotfound_fat16
	lea di, [di + DIRENTRY_size]	; bytes/dirent
	je fat16_found_it	; found entry -->

	dec si			; count down entire root's entries
	loopnz fat16_next_ent	; count down sector's entries (jumps iff si >0 && cx >0)
	test si, si		; work around qemu bug
	jnz fat16_next_sect	; (jumps iff si >0 && cx ==0)
				; ends up here iff si ==0
				;  ie all root entries checked unsuccessfully
%if 0

qemu prior to 2020-08 has a bug which affects the above
conditionals. The bug is that if NZ is set (like when the
branch to fat16_found_it is not taken) and then another
instruction sets ZR (like the dec si at the end of the root
directory) and then loopnz is used which sets cx to zero
then after the loopnz FL will be NZ leading to the jnz branch
to be taken. Eventually the entire load unit is traversed and
qemu returns error 01h when trying to read past the end of
the unit (at least for 1440 KiB diskettes).

The bug is now worked around by the comparison with zero at
the fat16_next_ent label. The test si, si serves as another
workaround to forcibly set the flag correctly after loopnz.
It is only really needed when the entire root is filled with
non-zero directory entries and an older qemu is used.

Reference: https://bugs.launchpad.net/qemu/+bug/1888165

%endif

handle_filenotfound_fat16:
	pop bx
	pop es
	pop di
	pop si

	db __TEST_IMM16		; (skip pop and pop)
handle_filenotfound_fat32:
	pop bx
	pop es

handle_filenotfound:
	jmp near word [handle_scan_dir_not_found]

error_filenotfound:
%if _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_not_found
%endif
	mov dx, msg.boot_file_not_found
	mov ax, 021Eh
	call setrc
	jmp bootcmd.fail

fat16_found_it:
	  pop bx
	  pop ax
	mov cx, 32
	sub di, cx
	push ds
	 push es
	 pop ds
	mov si, di		; ds:si -> entry in directory buffer
	mov di, bx
	mov es, ax		; es:di -> destination for entry
	rep movsb
	pop ds
	  pop di
	  pop si
	retn


; (boot32.asm code starts here)

fat32_scan_root:
	mov ax, [bp + bsBPB + ebpbRootCluster]
	mov dx, [bp + bsBPB + ebpbRootCluster + 2]

.dir_clust_dxax:
	push es
	push bx

	call check_clust
	jc handle_filenotfound_fat32

fat32_next_root_clust:
	call clust_to_first_sector
	push cx
	push bx
	mov cx, [bp + ldClusterSize]
fat32_next_root_sect:
	push cx
	mov cx, [bp + ldEntriesPerSector]

; Scan root directory for file. We don't bother to check for deleted
;  entries (E5h) or entries that mark the end of the directory (00h).
	mov bx, [load_adr_dirbuf_segment]
	call read_sector

	push di
	xor di, di		; es:di-> first entry in this sector
fat32_next_ent:
	call near word [handle_scan_dir_entry]
	jc handle_filenotfound_fat32_pop
	lea di, [di + DIRENTRY_size]	; bytes/dirent
	je fat32_found_it	; found entry -->

	loop fat32_next_ent	; count down sector's entries (jumps iff cx >0)
	pop di
	pop cx
	loop fat32_next_root_sect
	pop bx
	pop cx
	call clust_next
	jnc fat32_next_root_clust
	jmp @F

handle_filenotfound_fat32_pop:
	pop di
	pop cx
	pop bx
	pop cx
@@:
	jmp handle_filenotfound_fat32


fat32_found_it:
	pop dx			; value for di
	add sp, 6		; discard sector-in-cluster counter and cluster
	  pop bx
	  pop ax
	mov cx, 32
	sub di, cx
	push ds
	 push es
	 pop ds
	push si
	mov si, di		; ds:si -> entry in directory buffer
	mov di, bx
	mov es, ax		; es:di -> destination for entry
	rep movsb
	pop si
	pop ds
	  mov di, dx		; restore si:di = loaded FAT sector
	retn


; (iniload.asm code continues here)

		; INP:	dx:ax = cluster - 2 (0-based cluster)
		; OUT:	cx:bx = input dx:ax
		;	dx:ax = first sector of that cluster
		; CHG:	-
clust_to_first_sector:
	push dx
	push ax
	 push dx
	mul word [load_data - LOADDATA2 + ldClusterSize]
	xchg bx, ax
	xchg cx, dx
	 pop ax
	mul word [load_data - LOADDATA2 + ldClusterSize]
	test dx, dx
	jnz .error_badchain
	xchg dx, ax
	add dx, cx
	jc .error_badchain
	xchg ax, bx

	add ax, [load_data - LOADDATA2 + lsvDataStart]
	adc dx, [load_data - LOADDATA2 + lsvDataStart + 2]
	jc .error_badchain
				; dx:ax = first sector in cluster
	pop bx
	pop cx			; cx:bx = cluster
	retn

.error_badchain:
	jmp error_badchain


		; INP:	cx:bx = cluster (0-based)
		;	si:di = loaded FAT sector, -1 if none
		; OUT:	CY if no next cluster
		;	NC if next cluster found,
		;	 dx:ax = next cluster value (0-based)
		;	si:di = loaded FAT sector
		; CHG:	cx, bx
clust_next:
	mov ax, bx
	mov dx, cx
.dxax:
	add ax, 2
	adc dx, 0

	push es
	cmp byte [load_data - LOADDATA2 + ldFATType], 16
	je .fat16
	ja .fat32

.fat12:
; FAT12 entries are 12 bits, bytes are 8 bits. Ratio is 3 / 2,
;  so multiply cluster number by 3 first, then divide by 2.
					; ax = cluster number (up to 12 bits set)
		mov dx, ax
		shl ax, 1		; = 2n (up to 13 bits set)
		add ax, dx		; = 2n+n = 3n (up to 14 bits set)
		shr ax, 1		; ax = byte offset into FAT (0..6129)
					; CF = whether to use high 12 bits
		sbb cx, cx		; = -1 iff CY, else 0

; Use the calculated byte offset as an offset into the FAT
;  buffer, which holds all of the FAT's relevant data.
		mov es, [load_data - LOADDATA2 + lsvFATSeg]
		mov bx, ax		; -> 16-bit word in FAT to load

	test byte [load_ldflags], ldfFATInvalid
	jz .fat12_have_fat	; already have it -->
	push cx
	xor dx, dx
	div word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
				; dx = remainder, byte offset
				; ax = sector to read
	push dx
	xor dx, dx
	add ax, [load_data - LOADDATA2 + bsBPB + bpbReservedSectors]
	adc dx, dx
	mov bx, [load_data - LOADDATA2 + lsvFATSeg]
	call read_sector
	pop cx
	cmp cx, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	jne .fat12_have_fat_cx
	push es
	call read_sector	; read second sector for straddling entry
	pop es

.fat12_have_fat_cx:
	mov bx, cx
	pop cx

.fat12_have_fat:

; get 16 bits from FAT
		mov ax, [es:bx]

		and cl, 4	; = 4 iff CY after shift, else 0
		shr ax, cl	; shift down iff odd entry, else unchanged
		and ax, 0FFFh	; insure it's only 12 bits
		xor dx, dx
	jmp short .gotvalue

.fat32:
		; * 4 = byte offset into FAT (0--4000_0000h)
	add ax, ax
	adc dx, dx
.fat16:
		; * 2 = byte offset into FAT (0--2_0000h)
	add ax, ax
	adc dx, dx

	 push ax
	xchg ax, dx
	xor dx, dx		; dx:ax = high word
	div word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	mov bx, ax
	 pop ax			; dx = remainder, ax = low word
	div word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	xchg dx, bx		; dx:ax = result, bx = remainder
				; dx:ax = sector offset into FAT (0--200_0000h)
				; bx = byte offset into FAT sector (0--8190)
	test byte [load_ldflags], ldfFATInvalid
	jnz .read_no_store	; always read -->

	cmp dx, si
	jne @F		; read sector
	cmp ax, di
	je @FF		; sector is already buffered
@@:
	mov si, dx
	mov di, ax
	mov word [load_data - LOADDATA2 + lsvFATSector + 2], dx
	mov word [load_data - LOADDATA2 + lsvFATSector + 0], ax

.read_no_store:
	push bx
	add ax, [load_data - LOADDATA2 + bsBPB + bpbReservedSectors]
	adc dx, 0
	mov bx, [load_data - LOADDATA2 + lsvFATSeg]
	call read_sector
	pop bx
@@:
	mov es, [load_data - LOADDATA2 + lsvFATSeg]
	xor dx, dx
	mov ax, [es:bx]

	cmp byte [load_data - LOADDATA2 + ldFATType], 16
	je @F
	mov dx, [es:bx + 2]
@@:
.gotvalue:
	pop es

		; INP:	dx:ax = cluster value, 2-based
		; OUT:	dx:ax -= 2 (makes it 0-based)
		;	CY iff invalid cluster
check_clust:
	and dh, 0Fh
	sub ax, 2
	sbb dx, 0

	cmp byte [load_data - LOADDATA2 + ldFATType], 16
	ja .fat32
	je .fat16

.fat12:
	cmp ax, 0FF7h - 2
	jmp short .common

.fat32:
	cmp dx, 0FFFh
	jb @F		; CY here means valid ...-

.fat16:
	cmp ax, 0FFF7h - 2
@@:			;  -... or if NC first, CY here also
.common:
	cmc		; NC if valid
	jc .ret
	cmp dx, word [load_data - LOADDATA2 + ldMaxCluster + 2]
	jne @F
	cmp ax, word [load_data - LOADDATA2 + ldMaxCluster]
@@:
	ja .ret_CY
	db __TEST_IMM8	; (skip stc, NC)
.ret_CY:
	stc
.ret:
	retn


partition_table equ load_partition_table
partition_table.end equ load_partition_table.end
%define _SCANPTAB_PREFIX
%define _SCANPTAB_DEBUG4_PREFIX
%assign _PARTITION_TABLE_IN_CS 0
%define _BASE bp
%include "scanptab.asm"


		; INP:	al = first character
		;	si -> next
		; OUT:	doesn't return if error
		;	bx:dx = number read
		;	al = character after the number
		;	si -> next
		; CHG:	cx, ax, di
boot_get_decimal_literal:
	mov dx, 10		; set base: decimal
%if 1
	mov cx, '9' | (('A'-10-1 + 10) << 8)
%else
	mov cl, dl
	add cl, '0'-1
	cmp cl, '9'
	jbe .lit_basebelow11
	mov cl, '9'
.lit_basebelow11:		; cl = highest decimal digit for base ('1'..'9')
	mov ch, dl
	add ch, 'A'-10-1	; ch = highest letter for base ('A'-x..'Z')
%endif
	jmp @F


boot_get_hexadecimal_literal:
	mov dx, 16		; set base: hexadecimal
%if 1
	mov cx, '9' | (('A'-10-1 + 16) << 8)
%else
	mov cl, dl
	add cl, '0'-1
	cmp cl, '9'
	jbe .lit_basebelow11
	mov cl, '9'
.lit_basebelow11:		; cl = highest decimal digit for base ('1'..'9')
	mov ch, dl
	add ch, 'A'-10-1	; ch = highest letter for base ('A'-x..'Z')
%endif

@@:
	mov ah, 0
	xor bx, bx
	mov di, dx		; di = base

	call getexpression.lit_isdigit?	; first character must be a digit
	jc .err2
	xor dx, dx		; initialize value
.lit_loopdigit:
	cmp al, '_'
	je .lit_skip
	call getexpression.lit_isdigit?	; was last character ?
	jc .lit_end		; yes -->
	call uppercase
	sub al, '0'
	cmp al, 9		; was decimal digit ?
	jbe .lit_decimaldigit	; yes -->
	sub al, 'A'-('9'+1)	; else adjust for hexadecimal digit
.lit_decimaldigit:
	push ax
	mov ax, dx
	push bx
	mul di			; multiply low word with base
	mov bx, dx
	mov dx, ax
	pop ax
	push dx
	mul di			; multiply high word with base
	test dx, dx
	pop dx
	jnz .err2		; overflow -->
	add bx, ax		; add them
	pop ax
	jc .err2		; overflow -->
	add dl, al		; add in the new digit
	adc dh, 0
	adc bx, byte 0
	jc .err2		; overflow -->

.lit_skip:
	lodsb
	jmp short .lit_loopdigit

.lit_end:
	call isseparator?	; after the number, there must be a separator
	jne .err2		; none here -->
	retn

.err2:
	jmp error


query_geometry:
	call guard_auxbuff
	mov dl, [load_unit]
;	test dl, dl		; floppy?
;	jns @F			; don't attempt query, might fail -->
	; Note that while the original PC BIOS doesn't support this function
	;  (for its diskettes), it does properly return the error code 01h.
	; https://sites.google.com/site/pcdosretro/ibmpcbios (IBM PC version 1)
	mov ah, 08h
	xor cx, cx		; initialise cl to 0
	mov [load_heads], cx
	mov [load_sectors], cx
	xor bx, bx
	mov bl, dl
	testopt [load_unit_flags + bx], lufForceGeometry
	jnz .try_bootsector
	stc			; initialise to CY
	call .int13_retry	; query drive geometry
	jc .try_bootsector	; apparently failed -->
	mov dl, dh
	mov dh, 0		; dx = maximum head number
	inc dx			; dx = number of heads (H is 0-based)
	mov ax, cx		; ax & 3Fh = maximum sector number
	and ax, 3Fh		; get sectors (number of sectors, S is 1-based)
	jnz .got_sectors_heads	; valid (S is 1-based), use these -->
				; zero = invalid
.try_bootsector:
	mov es, word [auxbuff_segorsel]	; es => auxbuff
	xor bx, bx			; es:bx -> auxbuff
	mov ax, 0201h			; read sector, 1 sector
	mov cx, 1			; sector 1 (1-based!), cylinder 0 (0-based)
	mov dh, 0			; head 0 (0-based)
	mov dl, [load_unit]
	stc
	call .int13_retry
	jc .access_error

		; note: the smallest supported sector size, 32 bytes,
		;  does contain these entries (offset 18h and 1Ah in sector)
		;  within the first BPB sector.
	mov ax, word [es:bx + bsBPB + bpbCHSSectors]
	mov dx, word [es:bx + bsBPB + bpbCHSHeads]

.got_sectors_heads:
	mov word [load_sectors], ax
	mov word [load_heads], dx

	test ax, ax
	jz .invalid_sectors
	cmp ax, 63
	ja .invalid_sectors
	test dx, dx
	jz .invalid_heads
	cmp dx, 100h
	ja .invalid_heads

	mov es, word [auxbuff_segorsel]	; es => auxbuff
	xor bx, bx			; es:bx -> auxbuff
	xor ax, ax

%if _AUXBUFFSIZE < 8192+2
 %error Expecting to use auxbuff as sector size detection buffer
%endif

d5	call d5dumpregs
d5	call d5message
d5	asciz 13,10,"In query_geometry 0",13,10

	mov di, bx
	mov cx, (8192 + 2) >> 1
					; es:bx -> auxbuff, es:di = same
	rep stosw			; fill buffer, di -> behind (auxbuff+8192+2)
	mov ax, 0201h			; read sector, 1 sector
	inc cx				; sector 1 (1-based!), cylinder 0 (0-based)
	mov dh, 0			; head 0 (0-based)
	mov dl, [load_unit]
	stc
	call .int13_retry
	jc .access_error

	std				; _AMD_ERRATUM_109_WORKAROUND does not apply
	scasw				; -> auxbuff+8192 (at last word to sca)
d5	call d5dumpregs
d5	call d5message
d5	asciz 13,10,"In query_geometry 1",13,10
	mov cx, (8192 + 2) >> 1
	xor ax, ax
	repe scasw
	add di, 4			; di -> first differing byte (from top)
	cld
	push di

	mov di, bx
	mov cx, (8192 + 2) >> 1
	dec ax				; = FFFFh
	rep stosw

	mov ax, 0201h
	inc cx
	mov dh, 0
	mov dl, [load_unit]
	stc
	call .int13_retry
	jc .access_error

	std				; _AMD_ERRATUM_109_WORKAROUND does not apply
	scasw				; di -> auxbuff+8192 (last word to sca)
d5	call d5dumpregs
d5	call d5message
d5	asciz 13,10,"In query_geometry 2",13,10
	pop dx
	mov ax, -1
	mov cx, (8192 + 2) >> 1
	repe scasw
%if 0
AAAB
   ^
	sca B, match
  ^
	sca B, mismatch
 ^
	stop
%endif
	add di, 4			; di -> first differing byte (from top)
	cld

%if 0
0000000000000
AAAAAAAA00000
	^
FFFFFFFFFFFFF
AAAAAAAA00FFF
	  ^
%endif
	cmp dx, di			; choose the higher one
	jae @F
	mov dx, di
@@:
	sub dx, bx			; dx = sector size

d5	call d5dumpregs
d5	call d5message
d5	asciz 13,10,"In query_geometry 3",13,10

	cmp dx, 8192 + 2
	jae .sector_too_large
	mov ax, 32
	cmp dx, ax
	jb .sector_too_small
@@:
	cmp dx, ax
	je .got_match
	cmp ax, 8192
	jae .sector_not_power
	shl ax, 1
	jmp @B

.got_match:
	mov word [load_sectorsize], ax
	mov cl, 4
	shr ax, cl
	mov word [load_sectorsizepara], ax

	mov byte [load_ldflags], 0
	mov dl, [load_unit]
	xor bx, bx
	mov bl, dl
	testopt [load_unit_flags + bx], lufForceCHS
	jnz .no_lba
	testopt [load_unit_flags + bx], lufForceLBA
	jnz .yes_lba
	mov ah, 41h
	mov bx, 55AAh
	stc
	int 13h		; 13.41.bx=55AA extensions installation check
	jc .no_lba
	cmp bx, 0AA55h
	jne .no_lba
	test cl, 1	; support bitmap bit 0
	jz .no_lba

.yes_lba:
%if ldfHasLBA != 1
 %error Assuming ldfHasLBA is 1
%endif
	inc byte [load_ldflags]
.no_lba:

	mov ax, word [auxbuff_segorsel]	; ax => auxbuff
	mov dx, ax
	add dx, (8192 - 16) >> 4
	mov bx, ax
	mov cx, dx
	and bx, 0F000h
	and cx, 0F000h
	cmp cx, bx
	jne @F
	mov word [load_sectorseg], ax
	retn

@@:
	mov dx, msg.boot_auxbuff_crossing
	mov al, 20h
	jmp .error_common_j


.int13_retry:
	pushf
	push ax
	int 13h		; first try
	jnc @F		; NC, success on first attempt -->

; reset drive
	xor ax, ax
	int 13h
	jc @F		; CY, reset failed, error in ah -->

; try read again
	pop ax		; restore function number
	popf		; CF
	int 13h		; retry, CF error status, ah error number
	retn

@@:			; NC or CY, stack has function number
	inc sp
	inc sp
	inc sp
	inc sp		; discard two words on stack, preserve CF
	retn


.out_of_memory_error:
	mov dx, msg.boot_out_of_memory_error
	mov al, 21h
	jmp .error_common_j
.access_error:
%if _INPUT_FILE_BOOT
	testopt [internalflags3], dif3_in_if
	jnz if_exists_not_found
%endif
	mov dx, msg.boot_access_error
	mov al, 22h
	jmp .error_common_j
.sector_too_large:
	mov dx, msg.boot_sector_too_large
	mov al, 23h
	jmp .error_common_j
.sector_too_small:
	mov dx, msg.boot_sector_too_small
	mov al, 24h
	jmp .error_common_j
.sector_not_power:
	mov dx, msg.boot_sector_not_power
	mov al, 25h
	jmp .error_common_j
.invalid_sectors:
	mov dx, msg.boot_invalid_sectors
	mov al, 26h
	jmp .error_common_j
.invalid_heads:
	mov dx, msg.boot_invalid_heads
	mov al, 27h
.error_common_j:
	mov ah, 02h
	call setrc
	jmp bootcmd.fail


		; INP:	dx:ax = first sector
		;	bx:0 -> buffer
		; OUT:	dx:ax = sector number after last read
		;	es = input bx
		;	bx:0 -> buffer after last written
		; CHG:	-
		; STT:	ds = ss
read_ae_1536_bytes:
	push cx
	push bx
	mov cx, 1536
.loop:
	call read_sector
	sub cx, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	ja .loop
	pop es
	pop cx
	retn

		; INP:	dx:ax = first sector
		;	bx:0 -> buffer
		; OUT:	dx:ax = sector number after last read
		;	es = input bx
		;	bx:0 -> buffer after last written
		; CHG:	-
		; STT:	ds = ss
read_ae_512_bytes:
	push cx
	push bx
	mov cx, 512
.loop:
	call read_sector
	sub cx, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	ja .loop
	pop es
	pop cx
	retn


		; Write a sector using Int13.03 or Int13.43
		;
		; Protocol as for read_sector
write_sector:
	db __TEST_IMM8		; (skip stc, NC)

		; Read a sector using Int13.02 or Int13.42
		;
		; INP:	dx:ax = sector number (within partition)
		;	bx:0-> buffer
		;	(_LBA) ds = ss
		;	dword[load_data - LOADDATA2 + bsBPB + bpbHiddenSectors]
		;	 = base sector number (dx:ax is added to this to get
		;	    the absolute sector number in the selected unit.)
		; OUT:	If unable to read,
		;	 ! jumps to error instead of returning
		;	If sector has been read,
		;	 dx:ax = next sector number (has been incremented)
		;	 bx:0-> next buffer (bx = es+word[load_sectorsizepara])
		;	 es = input bx
		; CHG:	-
		;
		; Note:	If error 09h (data boundary error) is returned,
		;	 the read is done into the load_sectorseg buffer,
		;	 then copied into the user buffer.
read_sector:
	stc

read_sector_CY_or_write_sector_NC:
	lframe near
	lenter
	lvar word, is_read_bit0
	 pushf

.err: equ bootcmd.fail_read
d5	call d5dumpregs
d5	call d5message
d5	asciz 13,10,"In read_sector",13,10

	push dx
	push cx
	push ax
	push si

	mov es, bx

; DX:AX==LBA sector number
; add partition start (= number of hidden sectors)
		add ax,[load_data - LOADDATA2 + bsBPB + bpbHiddenSectors + 0]
		adc dx,[load_data - LOADDATA2 + bsBPB + bpbHiddenSectors + 2]

	sbb si, si	; -1 if was CY, 0 else
	neg si		; 1 if was CY, 0 else
	xor cx, cx
	push cx
	push si		; bit 32 = 1 if operating in 33-bit space
	push dx
	push ax		; qword sector number (lpSector)
	push bx
	push cx		; bx:0 -> buffer (lpBuffer)
	inc cx
	push cx		; word number of sectors to read (lpCount)
	mov cl, 10h
	push cx		; word size of disk address packet (lpSize)
	mov si, sp	; ds:si -> disk address packet (on stack)

	test byte [load_data - LOADDATA2 + ldFlags], ldfHasLBA
	jz .no_lba

d5	call d5message
d5	asciz "In read_sector.lba",13,10

	mov dl, byte [load_unit]
	call .set_ah_function_42_or_43
	int 13h		; 13.42 extensions read
	jnc .lba_done

	xor ax, ax
	int 13h
	jc .lba_error

		; have to reset the LBAPACKET's lpCount, as the handler may
		;  set it to "the number of blocks successfully transferred".
		; (in any case, the high byte is still zero.)
	mov byte [si + lpCount], 1

	call .set_ah_function_42_or_43
	int 13h
	jnc .lba_done

	cmp ah, 9	; data boundary error?
	jne .lba_error

.lba_sectorseg:
d4	call d4dumpregs
d4	call d4message
d4	asciz 13,10,"In read_sector.lba_sectorseg",13,10

	test byte [bp + ?is_read_bit0], 1
	jnz .lba_sectorseg_read

.lba_sectorseg_write:
	push es

	push ds
	push si
	push di
	mov cx, word [load_sectorsize]
	mov es, word [load_sectorseg]	; => sectorseg
	; lds si, [si + lpBuffer + 0]
	mov ds, word [si + lpBuffer + 2]; => user buffer
	xor si, si
	xor di, di
	rep movsb			; copy data into sectorseg
	pop di
	pop si
	pop ds

	 mov es, word [load_sectorseg]
	 mov word [si + lpBuffer + 2], es
					; => sectorseg
	; and word [si + lpBuffer + 0], byte 0

	mov byte [si + lpCount], 1
	mov ah, 43h
	int 13h
	jnc @F

	xor ax, ax
	int 13h
	jc .lba_error

	mov byte [si + lpCount], 1
	mov ah, 43h
	int 13h
	jc .lba_error
@@:

	pop es
	jmp .lba_done


.lba_sectorseg_read:
		; the offset part of the pointer is already zero!
	; push word [si + lpBuffer + 0]
	push es				; => user buffer
	 mov es, word [load_sectorseg]
	 mov word [si + lpBuffer + 2], es
	; and word [si + lpBuffer + 0], byte 0

	mov byte [si + lpCount], 1
	call .set_ah_function_42_or_43
	int 13h
	jnc .lba_sectorseg_done

	xor ax, ax
	int 13h
	jc .lba_error

	mov byte [si + lpCount], 1
	call .set_ah_function_42_or_43
	int 13h
	jc .lba_error
.lba_sectorseg_done:

	xor si, si
	pop es
	; pop cx
	 push di
	; mov di, cx
	xor di, di
	mov cx, word [load_sectorsize]
	mov ds, word [load_sectorseg]
	rep movsb
	 pop di

	push ss
	pop ds
.lba_done:
	add sp, 10h
	jmp .done

.lba_error: equ .err

.no_lba:
	add sp, 8
	pop ax
	pop dx
	pop si
	pop cx
	test si, si
	jnz .err

; DX:AX=LBA sector number
; divide by number of sectors per track to get sector number
; Use 32:16 DIV instead of 64:32 DIV for 8088 compatability
; Use two-step 32:16 divide to avoid overflow
			mov cx,ax
			mov ax,dx
			xor dx,dx
			div word [load_sectors]
			xchg cx,ax
			div word [load_sectors]
			xchg cx,dx

; DX:AX=quotient, CX=remainder=sector (S) - 1
; divide quotient by number of heads
			mov bx, ax
			xchg ax, dx
			xor dx, dx
			div word [load_heads]
			xchg bx, ax
			div word [load_heads]

; bx:ax=quotient=cylinder (C), dx=remainder=head (H)
; move variables into registers for INT 13h AH=02h
			mov dh, dl	; dh = head
			inc cx		; cl5:0 = sector
			xchg ch, al	; ch = cylinder 7:0, al = 0
			shr ax, 1
			shr ax, 1	; al7:6 = cylinder 9:8
	; bx has bits set iff it's > 0, indicating a cylinder >= 65536.
			 or bl, bh	; collect set bits from bh
			or cl, al	; cl7:6 = cylinder 9:8
	; ah has bits set iff it was >= 4, indicating a cylinder >= 1024.
			 or bl, ah	; collect set bits from ah
			mov dl, [load_unit]
					; dl = drive
			mov ah, 04h	; error number: sector not found
			 jnz .err	; error if cylinder >= 1024 -->
					; ! bx = 0 (for 13.02 call)

; we call INT 13h AH=02h once for each sector. Multi-sector reads
; may fail if we cross a track or 64K boundary
			mov si, 16 + 1
.loop_chs_retry_repeat:
			call .set_ax_function_0201_or_0301
			int 13h		; read one sector
			jnc .done
			push ax
			xor ax, ax
			int 13h		; reset disk
			pop ax
			dec si		; another attempt ?
			jnz .loop_chs_retry_repeat	; yes -->

	cmp ah, 9	; data boundary error?
	jne .err

.chs_sectorseg:
d4	call d4dumpregs
d4	call d4message
d4	asciz 13,10,"In read_sector.chs_sectorseg",13,10

	test byte [bp + ?is_read_bit0], 1
	jnz .chs_sectorseg_read

.chs_sectorseg_write:
	push es

	push ds
	push di
	push cx
	mov cx, word [load_sectorsize]
	push es
	mov es, word [load_sectorseg]	; => sectorseg
	pop ds				; => user buffer
	xor si, si
	xor di, di
	rep movsb			; copy data into sectorseg
	pop cx
	pop di
	pop ds

	mov ax, 0301h
	int 13h
	jnc @F

	xor ax, ax
	int 13h
	jc .err

	mov ax, 0301h
	int 13h
	jc .err
@@:
	pop es
	jmp .done


.chs_sectorseg_read:

	push es		; user buffer
	 mov es, word [load_sectorseg]

	call .set_ax_function_0201_or_0301
	int 13h
	jnc .chs_sectorseg_done

	xor ax, ax
	int 13h
	jc .err

	call .set_ax_function_0201_or_0301
	int 13h
	jc .err
.chs_sectorseg_done:

	xor si, si
	pop es
	 push di
	xor di, di
	mov cx, word [load_sectorsize]
	mov ds, word [load_sectorseg]
	rep movsb
	 pop di

	push ss
	pop ds
.done:
; increment segment
	mov bx, es
	add bx, word [load_sectorsizepara]

	pop si
	pop ax
	pop cx
	pop dx
; increment LBA sector number
	inc ax
	jne @F
	inc dx
@@:
	lleave code
	retn

.set_ah_function_42_or_43:
	mov ah, 42h
	test byte [bp + ?is_read_bit0], 1
	jnz @F
	mov ah, 43h
@@:
	retn

.set_ax_function_0201_or_0301:
	mov al, 1
.set_ah_function_02_or_03:
	mov ah, 02h
	test byte [bp + ?is_read_bit0], 1
	jnz @F
	mov ah, 03h
@@:
	retn

	lleave ctx


%if _INPUT_FILE_BOOT
yy_boot:
	call init_bootcmd

	call parseloadunit_default_sdp
	jnz .have_filename

	mov dx, msg.yy_requires_filename
.disp_error_1:
	mov ax, 0200h
	call setrc
	call putsz_error
	jmp near word [errret]

.have_filename:
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	cmp word [load_input_file.active], _INPUT_FILE_BOOT - 1
	jb @F

	mov dx, msg.yy_too_many_handles
	jmp .disp_error_1
@@:

		; al was = '/' or '\' or first pathname's first character
		; si-> next char
	cmp al, ':'
	jne .not_yy_goto_subfunction

	testopt [internalflags2], dif2_input_file_boot
	jz @F

	call skipwhite
	dec si
	mov word [load_input_file.goto_offset], si

	call yy_boot_get
	setopt [internalflags3], dif3_partition_changed
	jmp load_yy_finish

@@:
	mov dx, msg.yy_no_file
	jmp .disp_error_1
.not_yy_goto_subfunction:

	mov bx, load_yyname_input
	call bootcmd.pathname_parse_super

	and word [load_input_file.goto_offset], 0
	call skipwh0
	mov word [if_exists_then_address], si
	call iseol?_or_then
	je .not_yy_goto

	cmp al, ':'
	jne error

	call skipwhite
	dec si
	mov word [load_input_file.goto_offset], si

@@:
	lodsb
	cmp al, 32
	je @F
	cmp al, 9
	je @F
	call iseol?
	jne @B
@@:
	call skipwh0
	mov word [if_exists_then_address], si
	call chkeol_or_then

.not_yy_goto:

	testopt [internalflags3], dif3_load_is_dp
	jnz .load_yy_from_ldp

	mov bl, [load_partition]
	cmp byte [load_unit], 80h
	jb .p_f_is_diskette
	test bl, bl		; partition specified ?
	jz error		; no, error -->

	call query_geometry

	mov cx, load_yy_from_partition
	call scan_partitions
	testopt [internalflags3], dif3_in_if
	jnz if_exists_not_found
	mov dx, msg.boot_partition_not_found
	mov ax, 0206h
	call setrc
	jmp bootcmd.fail


.p_f_is_diskette:
	test bl, bl		; partition specified ?
	jnz error		; yes, error -->

	call query_geometry

	xor ax, ax
	xor dx, dx
@@:
	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	 push dx
	 push ax
	call read_ae_512_bytes

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch
	 pop ax
	 pop dx

	jmp load_yy_common


.load_yy_from_ldp:
	call query_geometry
	mov ax, word [load_partition_sector]
	mov dx, word [load_partition_sector + 2]
	jmp @B
%endif


boot_sigmismatch:
	mov dx, msg.bootfail_sig
	mov ax, 0207h
@@:
	call setrc
	jmp bootcmd.fail

boot_codemismatch:
	mov dx, msg.bootfail_code
	mov ax, 0208h
	jmp @B

boot_secsizemismatch:
	mov dx, msg.bootfail_secsizediffer
	mov ax, 0209h
	jmp @B


%if _INPUT_FILE_BOOT
		; INP:	es:si -> partition table entry,
		;	 si = load_partition_table .. load_partition_table+48,
		;	 es = ss
		;	bp + di -> above part table metadata,
		;	 dwo [bp + di - 4] = root (outermost extended position)
		;	 dwo [bp + di - 8] = base (current table position)
		; CHG:	ax, bx, (cx), dx
load_yy_from_partition:
d4	call d4message
d4	asciz "In load_yy_from_partition",13,10

	mov al, byte [load_current_partition]
	cmp al, byte [load_partition]
	je .gotit
	retn

.gotit:
d4	call d4message
d4	asciz "In load_yy_from_partition.gotit",13,10

	mov ax, [bp + di - 8]
	mov dx, [bp + di - 6]		; base (current table position)

	add ax, [es:si + 8]
	adc dx, [es:si + 8 + 2]		; add offset to logical partition

	mov word [es:si + 8], ax
	mov word [es:si + 8 + 2], dx	; store in partition table entry

	mov sp, bp
	pop bp				; restore bp (scan_partitions)
	pop bx				; discard ret address (scan_partitions)

					; dx:ax = absolute sector number
	mov bx, word [auxbuff_segorsel]	; bx => auxbuff
	push ax
	push dx
	call read_ae_512_bytes		; load partition boot sector

	cmp word [es:510], 0AA55h
	jne boot_sigmismatch

	xor cx, cx
;	cmp word [es:0], cx
;	je boot_codemismatch

	pop dx
	pop ax

		; dx:ax = boot sector
		; byte [load_unit] = unit
		; es:0-> read sector
load_yy_common:
	mov word [es:bsBPB + bpbHiddenSectors], ax
	mov word [es:bsBPB + bpbHiddenSectors + 2], dx

	mov bx, [bp + bsBPB + bpbBytesPerSector]
	cmp bx, [es:bsBPB + bpbBytesPerSector]
	jne boot_secsizemismatch

		; preserve some variables from our pseudo BPB
	xor ax, ax
	push word [bp + bsBPB + bpbCHSSectors]
	pop word [es:bsBPB + bpbCHSSectors]
	push word [bp + bsBPB + bpbCHSHeads]
	pop word [es:bsBPB + bpbCHSHeads]	; preserve geometry

	mov bx, word [bp + ldParaPerSector]
	shr bx, 1
	mov word [bp + ldEntriesPerSector], bx

	cmp word [es:bsBPB + bpbSectorsPerFAT], ax
	mov bl, byte [bp + bsBPB + ebpbNew + bpbnBootUnit]
	je .is_fat32
	mov byte [es:bsBPB + bpbNew + bpbnBootUnit], bl
	jmp short .was_fat1612
.is_fat32:
	mov byte [es:bsBPB + ebpbNew + bpbnBootUnit], bl
.was_fat1612:

	 push es
	 push ds
	push es
	pop ds
	xor si, si				; -> BPB from boot partition
	push ss
	pop es
	mov di, load_data - LOADDATA2		; -> our copy of a BPB
	mov cx, (bsBPB + ebpbNew + BPBN_size)
	rep movsb				; get the BPB

	 pop ds
	setopt [internalflags3], dif3_partition_changed

	cmp word [bp + bsBPB + bpbSectorsPerFAT], ax
	je @F					; is FAT32 -->
	mov si, load_data - LOADDATA2 + bsBPB + bpbNew
	mov di, load_data - LOADDATA2 + bsBPB + ebpbNew
	mov cx, BPBN_size
	rep movsb				; clone the FAT16 / FAT12 BPBN
						; to where the FAT32 BPBN lives
@@:
	 pop es

.outofmem: equ query_geometry.out_of_memory_error

	push word [auxbuff_segorsel]
	pop word [bp + lsvFATSeg]
	or byte [bp + ldFlags], ldfFATInvalid
	mov ax, -1
	mov word [bp + lsvFATSector], ax
	mov word [bp + lsvFATSector + 2], ax

	call initialise_fs

	mov byte [load_check_dir_attr], 0
	mov si, word [load_yyname_input]
	cmp byte [si], '/'
	jne @F
	inc si
@@:
	cmp byte [si], 0
	jne @F
..@yy_filename_empty:
	mov dx, msg.yy_filename_empty
	mov ax, 020Ah
	call setrc
	jmp bootcmd.fail
@@:
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_yyname_next], si
@@:

	mov di, -1
	mov si, di
	mov [bp + lsvFATSector], di
	mov [bp + lsvFATSector + 2], si

	xor ax, ax
	xor dx, dx

scan_dir_yyname_loop:
	mov word [bp + ldDirCluster], ax
	mov word [bp + ldDirCluster + 2], dx

	push ss
	pop es
	mov bx, load_yy_direntry

	call scan_dir_aux_for_file

	cmp byte [load_check_dir_attr], ATTR_DIRECTORY
	jne got_yyentry

	push si
	push di
	mov byte [load_check_dir_attr], 0
	mov si, word [load_yyname_next]
	cmp byte [si], 0
	jne @F
	jmp ..@yy_filename_empty
@@:
	push es
	 push ss
	 pop es
	call boot_parse_fn	; get next pathname
	pop es
	cmp al, '/'
	jne @F
	mov byte [load_check_dir_attr], ATTR_DIRECTORY
	mov word [load_yyname_next], si
@@:
	pop di
	pop si

	xor dx, dx
	mov ax, [es:bx + deClusterLow]
				; = first cluster (not FAT32)
	cmp byte [bp + ldFATType], 32
	jne @F
	mov dx, [es:bx + deClusterHigh]
				; dx:ax = first cluster (FAT32)
@@:

	jmp scan_dir_yyname_loop


got_yyentry:


; (boot32.asm code starts here)

		mov bx, [load_yy_direntry + deSize]
		mov ax, [load_yy_direntry + deSize + 2]
					 ; ax:bx = file size
		mov dl, [load_yy_direntry + 12]
					; dl = FAT+ size bits
		mov dh, dl
		and dx, 0E007h		; obtain bits 7-5 and 2-0
		jz @F
	mov dx, msg.yy_too_large
	mov ax, 020Bh
	call setrc
	jmp bootcmd.fail
@@:
	mov word [bp + ldFileSize], bx
	mov word [bp + ldFileSize + 2], ax

	or ax, bx
	jnz @F

	testopt [internalflags3], dif3_in_if
	jnz if_exists_not_found
	mov dx, msg.yy_empty
	mov ax, 020Ch
	call setrc
	jmp bootcmd.fail
@@:

; get starting cluster of file
		xor dx, dx
		mov ax, [load_yy_direntry + deClusterLow]
					; = first cluster (not FAT32)

		cmp byte [bp + ldFATType], 32
		jne @F
		mov dx, [load_yy_direntry + deClusterHigh]
					; dx:ax = first cluster (FAT32)
@@:

	and dh, 0Fh
		mov word [bp + lsvFirstCluster], ax
		mov word [bp + lsvFirstCluster + 2], dx

	mov word [bp + ldCurrentCluster], ax
	mov word [bp + ldCurrentCluster + 2], dx
	xor bx, bx
	mov word [bp + ldCurrentSeek], bx
	mov word [bp + ldCurrentSeek + 2], bx
		call check_clust
		jc error_badchain

load_yy_finish:
	xor bp, bp
	xor bx, bx
	mov si, load_data_lowest
	mov cx, (LOAD_INPUT_FILE_SIZE + 1) >> 1

	mov di, load_input_file
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	mov bx, word [load_input_file.active]
	inc bx
	mov ax, LOAD_INPUT_FILE_SIZE
	mul bx
	; test dx, dx
	; jnz error
	add di, ax
	mov dx, word [di - LOAD_INPUT_FILE_SIZE - LOADDATA3 + ldFATType]
	and dx, ifhfTestReserved1 | ifhfTestReserved2 \
		| ifhfQuietInput | ifhfQuietOutput
	or bp, dx

@@:

	or word [load_data - LOADDATA2 + ldFATType], bp
	push ss
	pop es
	rep movsw

		; hazard: this uses load_input_file and
		;  load_data_lowest if we are already in
		;  a yy_boot script.
	clropt [internalflags3], dif3_auxbuff_guarded_1
	call yy_reset_buf

	mov word [load_input_file.active], bx
	setopt [internalflags2], dif2_input_file_boot

	mov si, word [load_input_file.goto_offset]
	test si, si
	jnz cmd_goto.yy_entry

	testopt [internalflags3], dif3_in_if
	jnz if_exists_found_open
	retn


		; INP:	[load_input_file], dif2_input_file_boot
		; OUT:	most recent file closed, flag cleared if no longer file
		; CHG:	di, bx, ax
yy_boot_close_file:
	mov ax, word [load_input_file.active]
	dec ax
	jns .next
	clropt [internalflags2], dif2_input_file_boot
	setopt [internalflags2], dif2_closed_input_file_boot
	retn
.next:
	mov word [load_input_file.active], ax
	retn


		; INP:	[load_input_file]
		; OUT:	[load_data_lowest] = LOADDATA[123], lsv, BPB
		; CHG:	es, ax, dx, di, si
yy_boot_get:
	push ss
	pop es

	mov ax, LOAD_INPUT_FILE_SIZE
	mul word [load_input_file.active]

;	test dx, dx
;	jnz .error

	push cx
	mov di, load_data_lowest
	mov cx, (LOAD_INPUT_FILE_SIZE + 1) >> 1

	mov si, load_input_file
	add si, ax

	rep movsw
	pop cx
	retn


		; INP:	[load_data_lowest] = LOADDATA3
		; OUT:	[load_input_file]
		; CHG:	es, ax, dx, di, si
		; Note:	Preserves flags (ZF, CF)
yy_boot_update:
	pushf

	push ss
	pop es

	mov ax, LOAD_INPUT_FILE_SIZE
	mul word [load_input_file.active]

;	test dx, dx
;	jnz .error

	push cx
	mov si, load_data_lowest
	mov cx, LOADDATA3_size >> 1

	mov di, load_input_file
	add di, ax

	rep movsw
	pop cx
	popf			; ZF, CF
	retn



		; INP:	[load_input_file] = active file
		;	cx = how many bytes to read
		;	ds:dx -> buffer
		; OUT:	CY if error reading
		;	NC if success
		;	ax = how many bytes read
		; CHG:	bx, cx
		; STT:	ds = es = ss
yy_boot_read:
	lframe near
	lvar word, length
	lvar word, length_this_cluster
	lvar dword, bytes_per_cluster
	lvar dword, how_far_in_cluster
	lenter
	lvar dword, pointer
	 push ds
	 push dx
	xor ax, ax
	lvar word, did_guard_auxbuff
	 push ax
	lvar word, read_how_many
	 push ax
	push dx
	push si
	push di

	push word [errret]
	push word [throwret]
	push word [throwsp]

	push bp

	mov word [errret], .err_ret
	mov word [throwret], .err_ret
	mov word [throwsp], sp

	call yy_boot_get

	call guard_auxbuff
	inc word [bp + ?did_guard_auxbuff]

	mov dx, word [load_data - LOADDATA2 + ldFileSize + 2]
	mov ax, word [load_data - LOADDATA2 + ldFileSize]

	sub ax, word [load_data - LOADDATA2 + ldCurrentSeek]
	sbb dx, word [load_data - LOADDATA2 + ldCurrentSeek + 2]

	test dx, dx
	jnz .use_count
	cmp ax, cx
	jae .use_count
	mov cx, ax
.use_count:
	test cx, cx
	jz .success
	mov word [bp + ?length], cx

.next_cluster:
	mov si, word [load_data - LOADDATA2 + ldCurrentSeek + 2]
	mov di, word [load_data - LOADDATA2 + ldCurrentSeek]
	mov ax, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	mul word [load_data - LOADDATA2 + ldClusterSize]

	mov word [bp + ?bytes_per_cluster], ax
	mov word [bp + ?bytes_per_cluster + 2], dx
	sub ax, 1
	sbb dx, 0
	and si, dx
	and di, ax		; how far are we into cluster

	mov word [bp + ?how_far_in_cluster], di
	mov word [bp + ?how_far_in_cluster + 2], si

	neg si
	neg di
	sbb si, byte 0		; neg si:di

	add di, word [bp + ?bytes_per_cluster]
	adc si, word [bp + ?bytes_per_cluster + 2]
				; cluster size - how far we are
				;  = how much to read from this cluster
	test si, si
	jnz .use_count_2
	cmp di, cx
	jae .use_count_2
	mov cx, di
.use_count_2:
	mov word [bp + ?length_this_cluster], cx

.next_sector:
	mov ax, word [load_data - LOADDATA2 + ldCurrentCluster]
	mov dx, word [load_data - LOADDATA2 + ldCurrentCluster + 2]
	call check_clust
	jc .error
	call clust_to_first_sector
	mov bx, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	mov di, word [bp + ?how_far_in_cluster]
	mov si, word [bp + ?how_far_in_cluster + 2]
	xchg dx, si
	xchg ax, di
	div bx
		; dx = byte offset into sector
		; ax = sector offset into cluster's data
	add di, ax
	adc si, 0
	xchg dx, si
	xchg ax, di
		; dx:ax = sector in fs
		; si = byte offset

	mov cx, si		; = byte offset
	neg cx			; - byte offset
	add cx, bx		; sector size - byte offset
				;  = length this sector
	cmp cx, word [bp + ?length_this_cluster]
	jbe @F			; sector has less than requested -->
	mov cx, word [bp + ?length_this_cluster]
				; fill entire remaining request
@@:
	mov bx, word [auxbuff_segorsel]
	call read_sector
	push ds

	 push es
				; es:si -> data in sector buffer
	les di, [bp + ?pointer]
				; es:di -> buffer
	add word [bp + ?read_how_many], cx
	sub word [bp + ?length_this_cluster], cx
	sub word [bp + ?length], cx
	add word [load_data - LOADDATA2 + ldCurrentSeek], cx
	adc word [load_data - LOADDATA2 + ldCurrentSeek + 2], 0
	add word [bp + ?how_far_in_cluster], cx
	adc word [bp + ?how_far_in_cluster + 2], 0
	 pop ds			; ds:si -> data in sector buffer
	rep movsb		; copy
	mov word [bp + ?pointer], di
				; update pointer
	pop ds

	cmp word [bp + ?length_this_cluster], 0
	jne .next_sector

	mov di, word [bp + ?bytes_per_cluster]
	mov si, word [bp + ?bytes_per_cluster + 2]
	cmp word [bp + ?how_far_in_cluster], di
	jne @F
	cmp word [bp + ?how_far_in_cluster + 2], si
	je @FF
@@:
	cmp word [bp + ?length], 0
	jne .error
	jmp .success

@@:
	mov ax, word [load_data - LOADDATA2 + ldCurrentCluster]
	mov dx, word [load_data - LOADDATA2 + ldCurrentCluster + 2]
	call check_clust
	jc .error
	call clust_next.dxax
	jnc @F			; (NC) -->
	mov ax, 0FFF8h - 2
	mov dx, 0FFFh
	mov di, word [load_data - LOADDATA2 + ldCurrentSeek]
	mov si, word [load_data - LOADDATA2 + ldCurrentSeek + 2]
	cmp si, word [load_data - LOADDATA2 + ldFileSize]
	jne .set_error
	cmp di, word [load_data - LOADDATA2 + ldFileSize + 2]
	je .do_not_set_error	; if same then NC -->
.set_error:
	stc
.do_not_set_error:
@@:
	pushf
	add ax, 2
	adc dx, 0
	mov word [load_data - LOADDATA2 + ldCurrentCluster], ax
	mov word [load_data - LOADDATA2 + ldCurrentCluster + 2], dx
	popf			; CF
	jc .error
	mov cx, word [bp + ?length]
	jcxz .success
	jmp .next_cluster

.success:
	db __TEST_IMM8		; (skip stc, NC)
.error:
	stc

	pop bp

	pop word [throwsp]
	pop word [throwret]	; restore throw destination
	pop word [errret]

	pushf
	testopt [bp + ?did_guard_auxbuff], 1
	jz @F
	clropt [internalflags3], dif3_auxbuff_guarded_1
@@:
	popf			; CF

	call yy_boot_update

	pop di
	pop si
	pop dx
	pop ax			; pop from ?read_how_many
	lleave
	retn

.err_ret:
	mov sp, word [throwsp]	; restore stack
				;  (needed here if returned to errret)
	jmp .error


		; INP:	[load_input_file] = active file
		;	cx:dx = signed seek distance from current
		; OUT:	-
		; CHG:	ax, bx, cx, dx
		; STT:	ds = es = ss
yy_boot_seek_current:
	lframe near
	lvar dword, length
	lvar dword, length_this_cluster
	lvar dword, bytes_per_cluster
	lvar dword, how_far_in_cluster
	lenter
	lvar dword, seek_distance
	 push cx
	 push dx
	xor ax, ax
	lvar word, did_guard_auxbuff
	 push ax
	push si
	push di

	push word [errret]
	push word [throwret]
	push word [throwsp]

	push bp

	mov word [errret], .err_ret
	mov word [throwret], .err_ret
	mov word [throwsp], sp

	call yy_boot_get

	call guard_auxbuff
	inc word [bp + ?did_guard_auxbuff]

	test cx, cx
	jns .plus

	mov si, word [load_data - LOADDATA2 + ldCurrentSeek + 2]
	mov di, word [load_data - LOADDATA2 + ldCurrentSeek]
	mov ax, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	mul word [load_data - LOADDATA2 + ldClusterSize]

	mov word [bp + ?bytes_per_cluster], ax
	mov word [bp + ?bytes_per_cluster + 2], dx
	sub ax, 1
	sbb dx, 0
	not ax
	not dx

	push si
	push di

	and si, dx
	and di, ax		; start seek of current cluster

	pop ax
	pop dx			; current seek

	add ax, word [bp + ?seek_distance]
	adc dx, word [bp + ?seek_distance + 2]
	jnc .error		; target seek

		; if the cluster is invalid, need to reset
	cmp word [load_data - LOADDATA2 + ldCurrentCluster + 2], 0FFFh
	jb @F
	cmp word [load_data - LOADDATA2 + ldCurrentCluster], 0FFF8h
	jae .minus_reset
@@:

	cmp dx, si		; is target seek
				;  >= start seek of current cluster?
	jne @F
	cmp ax, di
@@:
	jae .minus_simple	; yes, simple -->

.minus_reset:
		; reset current cluster to first, seek to 0
	push word [load_data - LOADDATA2 + lsvFirstCluster + 2]
	push word [load_data - LOADDATA2 + lsvFirstCluster]
	pop word [load_data - LOADDATA2 + ldCurrentCluster]
	pop word [load_data - LOADDATA2 + ldCurrentCluster + 2]
	and word [load_data - LOADDATA2 + ldCurrentSeek], 0
	and word [load_data - LOADDATA2 + ldCurrentSeek + 2], 0

		; set distance
	mov word [bp + ?seek_distance], ax
	mov word [bp + ?seek_distance + 2], dx
		; use positive seek code
	jmp .plus

.minus_simple:
		; just set seek, current cluster still valid
	mov word [load_data - LOADDATA2 + ldCurrentSeek], ax
	mov word [load_data - LOADDATA2 + ldCurrentSeek + 2], dx
	jmp .return

.plus:
	mov dx, word [load_data - LOADDATA2 + ldFileSize + 2]
	mov ax, word [load_data - LOADDATA2 + ldFileSize]

	sub ax, word [load_data - LOADDATA2 + ldCurrentSeek]
	sbb dx, word [load_data - LOADDATA2 + ldCurrentSeek + 2]

	jc .eof
		; dx:ax = how far we can seek within file
	cmp dx, word [bp + ?seek_distance + 2]
	jne @F
	cmp ax, word [bp + ?seek_distance]
@@:
	ja .not_eof
.eof:
	mov ax, word [bp + ?seek_distance]
	mov dx, word [bp + ?seek_distance + 2]
	add word [load_data - LOADDATA2 + ldCurrentSeek], ax
	adc word [load_data - LOADDATA2 + ldCurrentSeek + 2], dx
	mov word [load_data - LOADDATA2 + ldCurrentCluster], 0FFF8h
	mov word [load_data - LOADDATA2 + ldCurrentCluster + 2], 0FFFh
	jmp .return

.not_eof:
	mov cx, word [bp + ?seek_distance]
	mov bx, word [bp + ?seek_distance + 2]
	test bx, bx
	jnz @F
	test cx, cx
	jz .return
@@:
	mov word [bp + ?length], cx
	mov word [bp + ?length + 2], bx

		; bx:cx = length
.next_cluster:
	mov si, word [load_data - LOADDATA2 + ldCurrentSeek + 2]
	mov di, word [load_data - LOADDATA2 + ldCurrentSeek]
	mov ax, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	mul word [load_data - LOADDATA2 + ldClusterSize]

	mov word [bp + ?bytes_per_cluster], ax
	mov word [bp + ?bytes_per_cluster + 2], dx
	sub ax, 1
	sbb dx, 0
	and si, dx
	and di, ax		; how far are we into cluster

	mov word [bp + ?how_far_in_cluster], di
	mov word [bp + ?how_far_in_cluster + 2], si

	neg si
	neg di
	sbb si, byte 0		; neg si:di

	add di, word [bp + ?bytes_per_cluster]
	adc si, word [bp + ?bytes_per_cluster + 2]
				; cluster size - how far we are
				;  = how much to read from this cluster
	cmp si, bx
	jne @F
	cmp di, cx
@@:
	jae .use_count_2
	mov cx, di
	mov bx, si
.use_count_2:
		; bx:cx = how far to seek in this cluster
	mov word [bp + ?length_this_cluster], cx
	mov word [bp + ?length_this_cluster + 2], bx

.next_sector:
	mov ax, word [load_data - LOADDATA2 + ldCurrentCluster]
	mov dx, word [load_data - LOADDATA2 + ldCurrentCluster + 2]
	call check_clust
	jc .error
	mov bx, word [load_data - LOADDATA2 + bsBPB + bpbBytesPerSector]
	mov di, word [bp + ?how_far_in_cluster]
	mov si, word [bp + ?how_far_in_cluster + 2]

	xchg dx, si
	xchg ax, di
	div bx
		; dx = byte offset into sector
		; ax = sector offset into cluster's data
	xchg dx, si
	xchg ax, di
		; si = byte offset

	mov cx, si		; = byte offset
	neg cx			; - byte offset
	add cx, bx		; sector size - byte offset
				;  = length this sector
	cmp word [bp + ?length_this_cluster + 2], 0
	jne @F			; fill remaining sector size
	cmp cx, word [bp + ?length_this_cluster]
	jbe @F			; sector has less than requested -->
	mov cx, word [bp + ?length_this_cluster]
				; fill entire remaining request
@@:
	sub word [bp + ?length_this_cluster], cx
	sbb word [bp + ?length_this_cluster + 2], 0
	sub word [bp + ?length], cx
	sbb word [bp + ?length + 2], 0
	add word [load_data - LOADDATA2 + ldCurrentSeek], cx
	adc word [load_data - LOADDATA2 + ldCurrentSeek + 2], 0
	add word [bp + ?how_far_in_cluster], cx
	adc word [bp + ?how_far_in_cluster + 2], 0

	cmp word [bp + ?length_this_cluster + 2], 0
	jne .next_sector
	cmp word [bp + ?length_this_cluster], 0
	jne .next_sector

	mov di, word [bp + ?bytes_per_cluster]
	mov si, word [bp + ?bytes_per_cluster + 2]
	cmp word [bp + ?how_far_in_cluster], di
	jne @F
	cmp word [bp + ?how_far_in_cluster + 2], si
	je @FF
@@:
	cmp word [bp + ?length + 2], 0
	jne .error
	cmp word [bp + ?length], 0
	jne .error
	jmp .return

@@:
	mov ax, word [load_data - LOADDATA2 + ldCurrentCluster]
	mov dx, word [load_data - LOADDATA2 + ldCurrentCluster + 2]
	call check_clust
	jc .error
	call clust_next.dxax
	jnc @F			; (NC) -->
	mov ax, 0FFF8h - 2
	mov dx, 0FFFh
	mov di, word [load_data - LOADDATA2 + ldCurrentSeek]
	mov si, word [load_data - LOADDATA2 + ldCurrentSeek + 2]
	cmp si, word [load_data - LOADDATA2 + ldFileSize]
	jne .set_error
	cmp di, word [load_data - LOADDATA2 + ldFileSize + 2]
	je .do_not_set_error	; if same then NC -->
.set_error:
	stc
.do_not_set_error:
	pushf
	xor cx, cx
	xor bx, bx
	xchg cx, word [bp + ?length]
	xchg bx, word [bp + ?length + 2]
	add word [load_data - LOADDATA2 + ldCurrentSeek], cx
	adc word [load_data - LOADDATA2 + ldCurrentSeek + 2], bx
	db __TEST_IMM8		; (skip pushf)
@@:
	pushf
	add ax, 2
	adc dx, 0
	mov word [load_data - LOADDATA2 + ldCurrentCluster], ax
	mov word [load_data - LOADDATA2 + ldCurrentCluster + 2], dx
	popf			; CF
	jc .error
	mov cx, word [bp + ?length]
	mov bx, word [bp + ?length + 2]
	test bx, bx
	jnz .next_cluster
	test cx, cx
	jnz .next_cluster
.return:
.error:

	pop bp

	pop word [throwsp]
	pop word [throwret]	; restore throw destination
	pop word [errret]

	pushf
	testopt [bp + ?did_guard_auxbuff], 1
	jz @F
	clropt [internalflags3], dif3_auxbuff_guarded_1
@@:
	popf			; CF

	call yy_boot_update

	pop di
	pop si
	lleave
	retn

.err_ret:
	mov sp, word [throwsp]	; restore stack
				;  (needed here if returned to errret)
	jmp .error


		; INP:	[load_input_file] = active file
		;	cx:dx = 0
		; OUT:	-
		; CHG:	ax, bx, cx, dx
		; STT:	ds = es = ss
yy_boot_seek_start:
	push si
	push di

	call yy_boot_get

.reset:
		; reset current cluster to first, seek to 0
	push word [load_data - LOADDATA2 + lsvFirstCluster + 2]
	push word [load_data - LOADDATA2 + lsvFirstCluster]
	pop word [load_data - LOADDATA2 + ldCurrentCluster]
	pop word [load_data - LOADDATA2 + ldCurrentCluster + 2]
	and word [load_data - LOADDATA2 + ldCurrentSeek], 0
	and word [load_data - LOADDATA2 + ldCurrentSeek + 2], 0

	call yy_boot_update

	pop di
	pop si
	retn
%endif


		; INP:	bp = load_data - LOADDATA2
initialise_fs:
; (boot.asm code starts here)

	xor ax, ax
; calculate some values that we need:
; adjusted sectors per cluster (store in a word,
;  and decode EDR-DOS's special value 0 meaning 256)
	mov al, [bp + bsBPB + bpbSectorsPerCluster]
	dec al
	inc ax
	mov [bp + ldClusterSize], ax

	mov ax, [bp + ldEntriesPerSector]

; number of sectors used for root directory (store in CX)
	xor dx, dx
	mov bx, ax
	dec ax				; rounding up
	add ax, [bp + bsBPB + bpbNumRootDirEnts]	; (0 iff FAT32)
	adc dx, dx			; account for overflow (dx was zero)
	div bx				; get number of root sectors
	xchg ax, cx			; cx = number of root secs


; (iniload.asm code starts here)

	push cx				; number of root secs
	xor ax, ax
; first sector of root directory
	mov al, [bp + bsBPB + bpbNumFATs]	; ! ah = 0, hence ax = number of FATs
	mov cx, word [bp + bsBPB + bpbSectorsPerFAT]
	xor di, di			; di:cx = sectors per FAT
					;  iff FAT12, FAT16
	test cx, cx			; is FAT32 ?
	jnz @F				; no -->
	mov cx, word [bp + bsBPB + ebpbSectorsPerFATLarge]
	mov di, word [bp + bsBPB + ebpbSectorsPerFATLarge + 2]	; for FAT32
@@:
	push ax
	mul cx
		; ax = low word SpF*nF
		; dx = high word
	xchg bx, ax
	xchg cx, dx
		; cx:bx = first mul
	pop ax
	mul di
		; ax = high word adjust
		; dx = third word
	test dx, dx
	jz @F
.error_badchain:
error_badchain: equ $
	mov dx, msg.boot_badchain
	mov ax, 020Dh
	call setrc
	jmp bootcmd.fail

@@:
	xchg dx, ax
		; dx = high word adjust
	add dx, cx
		; dx:bx = result
	xchg ax, bx
		; dx:ax = result
	jc .error_badchain

	add ax, [bp + bsBPB + bpbReservedSectors]
	adc dx, byte 0
	jc .error_badchain

	pop cx				; number of root sectors
	xor di, di

; first sector of disk data area:
	add cx, ax
	adc di, dx
	jc .error_badchain
	mov [bp + lsvDataStart], cx
	mov [bp + lsvDataStart + 2], di

	mov [bp + ldRootSector], ax
	mov [bp + ldRootSector + 2], dx

; total sectors
	xor dx, dx
	mov ax, [bp + bsBPB + bpbTotalSectors]
	test ax, ax
	jnz @F
	mov dx, [bp + bsBPB + bpbTotalSectorsLarge + 2]
	mov ax, [bp + bsBPB + bpbTotalSectorsLarge]

		; fall through and let it overwrite the field with the
		; already current contents. saves a jump.
@@:
	mov [bp + bsBPB + bpbTotalSectorsLarge + 2], dx
	mov [bp + bsBPB + bpbTotalSectorsLarge], ax

	; dx:ax = total sectors

	mov bx, [bp + bsBPB + bpbSectorsPerFAT]
	mov byte [bp + ldFATType], 32
	test bx, bx
	jz @F

	xor cx, cx

	mov word [bp + bsBPB + ebpbSectorsPerFATLarge], bx
	mov word [bp + bsBPB + ebpbSectorsPerFATLarge + 2], cx
	mov word [bp + bsBPB + ebpbFSFlags], cx
	; FSVersion, RootCluster, FSINFOSector, BackupSector, Reserved:
	;  uninitialised here (initialised by loaded_all later)

@@:
	; dx:ax = total amount of sectors
	sub ax, word [bp + lsvDataStart]
	sbb dx, word [bp + lsvDataStart + 2]

	; dx:ax = total amount of data sectors
	mov bx, ax
	xchg ax, dx
	xor dx, dx
	div word [bp + ldClusterSize]
	xchg bx, ax
	div word [bp + ldClusterSize]
	; bx:ax = quotient, dx = remainder
	; bx:ax = number of clusters
	test bx, bx
	jz @FF
		; >= 1_0000h clusters, should be FAT32
	cmp bx, 0FFFh
	ja .badclusters
	jne @F
	cmp ax, 0FFF7h - 2
	ja .badclusters
@@:
		; check it is really FAT32
	cmp word [bp + bsBPB + bpbSectorsPerFAT], 0
	je .gotfattype

.badclusters:
	mov dx, msg.boot_badclusters
	mov ax, 020Eh
	call setrc
	jmp bootcmd.fail

@@:
		; <= FFFFh clusters, may be FAT12 or FAT16 (or small FAT32)
		; check if it is small FAT32
	cmp word [bp + bsBPB + bpbSectorsPerFAT], 0
	je .gotfattype

	cmp ax, 0FFF7h - 2		; too much for FAT16 ?
	ja .badclusters
	mov byte [bp + ldFATType], 16
	cmp ax, 0FF7h - 2		; is it FAT12 ?
	ja .gotfattype			; no, is FAT16 -->

	mov byte [bp + ldFATType], 12

	testopt [load_ldflags], ldfFATInvalid
	jnz .gotfattype

	push bx
	push ax
; (boot.asm code continues here)

; Load the entire FAT into memory. This is easily feasible for FAT12,
;  as the FAT can only contain at most 4096 entries.
; (The exact condition should be "at most 4087 entries", or with a
;  specific FF7h semantic, "at most 4088 entries"; the more reliable
;  and portable alternative would be "at most 4080 entries".)
; Thus, no more than 6 KiB need to be read, even though the FAT size
;  as indicated by word[sectors_per_fat] could be much higher. The
;  first loop condition below is to correctly handle the latter case.
; (Sector size is assumed to be a power of two between 32 and 8192
;  bytes, inclusive. An 8 KiB buffer is necessary if the sector size
;  is 4 or 8 KiB, because reading the FAT can or will write to 8 KiB
;  of memory instead of only the relevant 6 KiB. This is always true
;  if the sector size is 8 KiB, and with 4 KiB sector size it is true
;  iff word[sectors_per_fat] is higher than one.)
		mov di, 6 << 10		; maximum size of FAT12 to load
		mov cx, [bp + bsBPB + bpbSectorsPerFAT]
					; maximum size of this FS's FAT
		xor dx, dx
		mov ax, [bp + bsBPB + bpbReservedSectors]; = first FAT sector
		mov bx, [bp + lsvFATSeg]
@@:
		call read_sector	; read next FAT sector
		sub di, [bp + bsBPB + bpbBytesPerSector]
					; di = bytes still left to read
		jbe @F			; if none -->
					; (jbe means jump if CF || ZF)
		loop @B			; if any FAT sector still remains -->
@@:					; one of the limits reached; FAT read
	pop ax
	pop bx

.gotfattype:

; if bx:ax = 1, then entries 0, 1, 2 are valid
	add ax, 1
	adc bx, 0
; max entry is x+1 (2 if x=1)

	mov word [bp + ldMaxCluster], ax
	mov word [bp + ldMaxCluster + 2], bx

; if bx:ax was = 1, then entries below 3 have to exist
	add ax, 1
	adc bx, 0
; if bx:ax was 1, bx:ax now = 3

	mov dx, bx

	cmp byte [bp + ldFATType], 16
	jb .check_fat_limit_12
	je .check_fat_limit_16
.check_fat_limit_32:
	add ax, ax
	adc dx, dx
.check_fat_limit_16:
	add ax, ax
	adc dx, dx
	jmp @F

.check_fat_limit_12:
	mov dx, ax
	add ax, ax
	add ax, dx		; * 3
	shr ax, 1		; * 3 / 2 = * 1.5
	adc ax, 0		; if the last nybble is needed
	xor dx, dx

@@:
	mov bx, word [bp + bsBPB + bpbBytesPerSector]
	dec bx
	add ax, bx
	adc dx, 0
	inc bx

	xchg cx, ax
	mov ax, dx
	xor dx, dx
	div bx
	xchg cx, ax
	div bx
	xchg cx, dx
		; cx = remainder, dx:ax = number of sectors needed

	mov cx, word [bp + bsBPB + bpbSectorsPerFAT]
	xor di, di			; di:cx = sectors per FAT
					;  iff FAT12, FAT16
	test cx, cx			; is FAT32 ?
	jnz @F				; no -->
	mov cx, word [bp + bsBPB + ebpbSectorsPerFATLarge]
	mov di, word [bp + bsBPB + ebpbSectorsPerFATLarge + 2]	; for FAT32
@@:

	cmp di, dx
	jne @F
	cmp cx, ax
@@:
	jae @F
.badfat:
	mov dx, msg.boot_badfat
	mov ax, 020Fh
	call setrc
	jmp bootcmd.fail

@@:
	retn
