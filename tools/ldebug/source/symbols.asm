
%if 0

lDebug symbol handling

Copyright (C) 2008-2018 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
sym_storage:
.clear_on_free.first:
.start:		dd 0
%if _XMS_SYMBOL_TABLE
.main.xms.start:dd 0
.hash.xms.start:dd 0
.str.xms.start:	dd 0
.xms.free:	dd 0
.xms.end:	dd 0
%endif

.first_start:
.main.start:	dd 0		; -> SYMMAIN array
.hash.start:	dd 0		; -> SYMHASH array
.str.start:	dd 0		; -> SYMSTR heap
.free:		dd 0		; -> free table space
.end:		dd 0		; -> after end of allocated table space
.after_start:

	; The following are SYMMAIN array indices.
.main.end:	dw 0
.main.free:	dw 0
.minus1_on_free.first:
.main.based.base:	dw -1
.main.base.first:	dw -1
.main.base.last:	dw -1
.main.bb.first:		dw -1
.main.bb.last:		dw -1
.main.nonbased.first:	dw -1
.main.nonbased.last:	dw -1
.main.reloc.first:	dw -1
.main.reloc.last:	dw -1

%if _ZZ_INSERT_TEMP
	; This is a SYMMAIN array index. However, it
	;  should NOT participate in relocation. Must
	;  be >= .main.free at all times.
.main.temp.first:	dw -1
%endif
.minus1_on_free.after:
%if _ZZ_INSERT_TEMP
	; Amount of in-use temporary SYMMAIN entries.
.main.temp.amount:	dw 0
%endif

	; The following are SYMHASH array dword indices.
.hash.end:	dw 0
.hash.free:	dw 0

	; The following are SYMSTR heap dword indices.
.str.end:	dw 0
.str.free:	dw 0
.clear_on_free.after:

%if _BOOTLDR
	align 16, db 0
	; Image identification
	; First dword: signature
	; Next word: version, two ASCII digits
	; Next word: checksum. adding up all words of the paragraph gives zero
	; Next word: size of image (including this paragraph)
	; Three words reserved, zero.
zz_imageident:
	db "NSYM00"
.check:	dw 0
.size:	dw 0
	times 3 dw 0
%endif

%if _XMS_SYMBOL_TABLE
	align 16, db 0
zz_xms:
.16_zeros:		times 16 db 0
	align 2, db 0
.movestruc:		times XMSMOVE_size db 0
	align 4, db 0
.entry:			dq 0
.handle:		dw -1
.transfer_address:	dd 0
.transfer_size:		dd XMSTRANSFER_default_size
.after_str:		dd 0
%endif	; _XMS_SYMBOL_TABLE

	align 2, db 0
zz_saved_strategy:	dw 0
zz_saved_umblink:	dw 0
zz_flag_existing_default:
.symbol:	db insertfExistingBlockDiffering
.start:		db insertfExistingKeep


	usesection lDEBUG_CODE

 %if _DUALCODE && _SYMBOLICDUALCODE
	usesection lDEBUG_CODE2
 %endif

	overridedef STANDALONE, 0
	overridedef INCLUDECOMMON, 0
	overridedef DEBUG0, 0
	overridedef DEBUG1, 0
	overridedef SYMMAINLARGE, 1
%include "insert.asm"
	resetdef
	resetdef
	resetdef
	resetdef
	resetdef


		; error handlers used by insert.asm
invaliddata:
	mov ax, 0500h
	nearcall setrc
	mov dx, msg.invaliddata
exit_error_msg:

 %if _DUALCODE && _SYMBOLICDUALCODE
  %define SECTION_OF_exit_error_msg_code_entry lDEBUG_CODE
	nearcall exit_error_msg_code_entry

	usesection lDEBUG_CODE

exit_error_msg_code_entry:
	add sp, 6		; discard near + far return address
 %endif
exit_error_msg_code:
	 push ss
	 pop ds
	 push ss
	 pop es
	call putsz_error
	nearcall zz_restore_strat
	mov ax, 05FFh
	call setrc
	jmp near [errret]

 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
	usesection lDEBUG_CODE2

  %define SECTION_OF_error_entry lDEBUG_CODE
section_of symbolasm_error
symbolasm_error: section_of_function
	nearcall error_entry

	usesection lDEBUG_CODE

error_entry:
	add sp, 6
	jmp error
 %else
	usesection lDEBUG_CODE

section_of symbolasm_error
check_section_of symbolasm_error
symbolasm_error equ error
 %endif

zz:
 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
  %define SECTION_OF_zz_relocated lDEBUG_CODE2
	nearcall zz_relocated
	retn

	usesection lDEBUG_CODE2
 %endif

zz_relocated:
	db __TEST_IMM8
.:
	lodsb
	nearcall iseol?
	je .end
	cmp al, 32
	jbe .
	cmp al, '-'
	je .switch
	cmp al, '/'
	je .switch

	dec si
	mov dx, msg.add
	nearcall isstring?
	je zz_add

	mov dx, msg.abort
	nearcall isstring?
	je zz_abort

	nearcall zz_commit_insert


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
.dispatch:
	dw zz_commit
	dw msg.commit
	dw zz_del
	dw msg.del
	dw zz_del
	dw msg.delete
	dw zz_reloc
	dw msg.reloc
	dw zz_reloc
	dw msg.relocate
	dw zz_stat
	dw msg.stat
	dw zz_list
	dw msg.list
	dw zz_match
	dw msg.match
	dw 0			; table end marker


 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
	usesection lDEBUG_CODE2
 %else
	usesection lDEBUG_CODE
 %endif

	mov bx, .dispatch
@@:
	mov cx, [bx]
	mov dx, [bx + 2]
	add bx, 4
	jcxz @F
	nearcall isstring?
	jne @B
	jmp cx

@@:
	mov ax, 0501h
	nearcall setrc
	jmp symbolasm_error

.end:
	retn

.switch:
	nearcall zz_commit_insert

	lodsb
	nearcall uppercase
	cmp al, 'S'
	jne .unknown_switch
	nearcall zz_switch_s
	jmp zz_relocated

.unknown_switch:
	mov ax, 0502h
	nearcall setrc

	db __TEST_IMM8		; (skip dec)
@@:
	dec si
@@:
	jmp symbolasm_error

zz_switch_s.error_pop_si:
	pop si
	jmp @B


		; This is a no-op. In the dispatcher however,
		;  if this command is matched then the function
		;  zz_commit_insert was already called.
zz_commit:
	lodsb
	nearcall chkeol
	retn


		; This is the only command (other than Z ADD) that
		;  is executed without first committing the temporary
		;  inserts. It helps if the debugger gets confused, as
		;  otherwise all commands will fail during their attempt
		;  to commit and the debugger becomes unusable.
zz_abort:
	lodsb
	nearcall chkeol
%if _ZZ_INSERT_TEMP
	and word [sym_storage.main.temp.amount], 0
	or word [sym_storage.main.temp.first], -1
%endif
	retn


zz_reloc:
	nearcall skipwhite

	lframe
	lenter

	mov bx, word [reg_cs]	; default segment = cs
	nearcall getaddrX

	xor cx, cx
	lvar dword,	start_offset
%if _PM
	 push ax
	 push ax
	dualcall push_cxdx_or_edx
%else
	push cx
	push dx
%endif

	push ax
	nearcall getlinear_32bit
	jc symbolasm_error

	mov cx, ax
	pop ax
	lvar dword,	start_source
	 push dx
	 push cx

	nearcall uppercase
	cmp al, 'L'
	je .l

	nearcall getdword
	sub dx, word [bp + ?start_offset]
	sbb bx, word [bp + ?start_offset + 2]
	jc symbolasm_error
	add dx, 1
	adc bx, 0
	jc symbolasm_error
	jmp @F

.l:
	lodsb
	nearcall getdword
@@:
	lvar dword,	length
	 push bx
	 push dx

	mov bx, word [reg_cs]
	nearcall getaddrX
	nearcall chkeol
	nearcall getlinear_32bit
	jc symbolasm_error

	xchg ax, cx
	xchg dx, bx		; bx:cx = start of destination

	mov ax, word [bp + ?start_source]
	mov dx, word [bp + ?start_source + 2]
				; dx:ax = start of source

	push word [bp + ?length + 2]
	push word [bp + ?length]
				; stack = length

	dualcall zz_relocate

	lleave

	mov ax, cx

	test ax, ax
	jnz @F

	mov dx, msg.zz_reloc_amount_none
	jmp @FF

@@:
	mov dx, msg.zz_reloc_amount_1
	nearcall disp_message
	mov di, line_out
	nearcall decword
	push ax
	nearcall putsline
	pop ax
	mov dx, msg.zz_reloc_amount_2.plural
	cmp ax, 1
	jne @F
	mov dx, msg.zz_reloc_amount_2.singular
@@:
	nearcall disp_message
	retn


zz_del:
	nearcall skipwhite

	dec si
	mov dx, msg.unrefstring
	nearcall isstring?
	je delete_unref_str
	mov dx, msg.range
	nearcall isstring?
	je zz_del_range
	jmp zz_del_match


delete_unref_str:
	lodsb
	nearcall chkeol
	xor si, si
	jmp .condition

.loop:
	 push si		; string index
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.str
	 pop di
	 pop es

	mov cx, word [es:di + ssLength]
	cmp ch, 0
	mov ch, 0
	jne .next

	push si
	mov bx, si
	nearcall zz_delete_string
	pop si
	jmp .condition

.next:
		; The maximum length of a SYMSTR is 255 + SYMSTR_size + 3.
	nearcall zz_get_symstr_length_indices.ssLength_in_cx

		; Due to the maximum length, both the byte index and the
		;  SYMSTR_index_size index delta fit into a word each.
	add si, cx		; keep count of what index we're at
.condition:
	cmp si, word [sym_storage.str.free]
	jb .loop		; if not yet at free space -->
	retn


zz_del_match:
	nearcall skipwhite

	db __TEST_IMM8		; skip stc, NC

		; INP:	ss:bx -> symbol name
		;	cx = length of name
.add_poison_entrypoint:
	stc
	lframe
	lvar word,	string_hash
	lenter
	lvar word,	string_length
	 push cx
	lvar word,	bit_0_is_poison
	 pushf
	lvar dword,	string_pointer
	 push ss
	 push bx

	mov cx, 0

	lvar word,	matched_amount
	 push cx

	jc .poison_skip_1


	mov ah, al
	cmp al, "'"
	je .symbol_quoted
	cmp al, '"'
	je .symbol_quoted
	dec si
	mov ah, 0
.symbol_quoted:
	mov word [bp + ?string_pointer], si
.symbol_loop:
	lodsb
	cmp ah, 0
	jne @F
	nearcall ifsep
	je .symbol_end
	jmp .symbol_next
@@:
	cmp al, 13
	je .error
	test al, al
	jz .error
	cmp al, ah
	jne @F
	lodsb
	nearcall ifsep
	jne .error
	jmp .symbol_end
@@:
.symbol_next:
	inc cx
	jmp .symbol_loop

.error:
	mov ax, 0503h
	nearcall setrc
	jmp symbolasm_error

.symbol_end:
	test cx, cx
	jz .error
	mov word [bp + ?string_length], cx

	nearcall chkeol


.poison_skip_1:
	push ds
	lds si, [bp + ?string_pointer]
	mov cx, word [bp + ?string_length]
	nearcall zz_hash
	mov word [bp + ?string_hash], bx
	pop ds

	lvar word,	inner_main_index
	 push ax

.loop:
		; ?inner_main_index used as parameter
	 push word [bp + ?string_pointer + 2]
	 push word [bp + ?string_pointer]
	 push word [bp + ?string_length]
	mov bx, word [bp + ?string_hash]

	dualcall zz_match_symbol
	jc .none

	inc word [bp + ?matched_amount]
	push dx
	mov bx, word [bp + ?inner_main_index]
	nearcall zz_delete_main
	pop bx
	nearcall zz_delete_hash

	jmp .loop

.none:
	mov ax, word [bp + ?matched_amount]
	testopt [bp + ?bit_0_is_poison], 1
	lleave
	jz @F
	test ax, ax
	jz .retn
@@:

zz_del_display_amount:
	test ax, ax
	jnz @F

	mov dx, msg.zz_del_amount_none
	jmp @FF

@@:
	mov dx, msg.zz_del_amount_1
	nearcall disp_message
	mov di, line_out
	nearcall decword
	push ax
	nearcall putsline
	pop ax
	mov dx, msg.zz_del_amount_2.plural
	cmp ax, 1
	jne @F
	mov dx, msg.zz_del_amount_2.singular
@@:
	nearcall disp_message
zz_del_match.retn:
	retn


zz_del_range:
	lodsb

	lframe
	lenter

	mov bx, word [reg_cs]	; default segment = cs
	nearcall getaddrX

	xor cx, cx
	lvar dword,	start_offset
%if _PM
	 push ax
	 push ax
	dualcall push_cxdx_or_edx
%else
	push cx
	push dx
%endif

	push ax
	nearcall getlinear_32bit
	jc symbolasm_error

	mov cx, ax
	pop ax
	lvar dword,	start_source
	 push dx
	 push cx

	nearcall uppercase
	cmp al, 'L'
	je .l

	nearcall getdword
	sub dx, word [bp + ?start_offset]
	sbb bx, word [bp + ?start_offset + 2]
	jc symbolasm_error
	add dx, 1
	adc bx, 0
	jc symbolasm_error
	jmp @F

.l:
	lodsb
	nearcall getdword
@@:
	lvar dword,	length
	 push bx
	 push dx

	nearcall chkeol

	mov ax, word [bp + ?start_source]
	mov dx, word [bp + ?start_source + 2]
				; dx:ax = start

	mov bx, ax
	mov cx, dx
	add bx, word [bp + ?length]
	adc cx, word [bp + ?length + 2]
	sub bx, 1
	sbb cx, 0		; cx:bx = end
	nearcall binsearchmain
	push cx
	jcxz .done
	xchg ax, bx		; ax = main index to delete

.loop:
	push ax			; preserve main index
	push cx			; preserve number of entries
	nearcall zz_delete_main_and_its_hash
	pop cx
	pop ax
	loop .loop		; do next deletion (same main index) -->
	mov word [stack_low_address], str_buffer
.done:
	pop ax
	lleave
	jmp zz_del_display_amount


zz_stat:
	lodsb
	nearcall chkeol

	nearcall list_sym_storage_usage
	retn


zz_match:
	nearcall skipwhite

	lframe
	lvar word,	string_length
	lvar word,	string_hash
	lenter
	lequ 1,		flag_is_add
	lequ 2,		flag_is_base
	lequ 4,		flag_is_single_line
	mov bx, ?flag_is_single_line
	lvar word,	flags
	 push bx
	lvar dword,	string_pointer
	 push ss
	 push bx

	dec si
	mov dx, msg.add
	nearcall isstring?
	jne @F

	inc byte [bp + ?flags]	; set ?flag_is_add

@@:
	nearcall skipwhite

	dec si
	mov dx, msg.base
	nearcall isstring?
	jne @F

	or byte [bp + ?flags], ?flag_is_base

@@:
	nearcall skipwhite

	xor cx, cx
	mov ah, al
	cmp al, "'"
	je .symbol_quoted
	cmp al, '"'
	je .symbol_quoted
	dec si
	mov ah, 0
.symbol_quoted:
	mov word [bp + ?string_pointer], si
.symbol_loop:
	lodsb
	cmp ah, 0
	jne @F
	nearcall ifsep
	je .symbol_end
	jmp .symbol_next
@@:
	cmp al, 13
	je .error
	test al, al
	jz .error
	cmp al, ah
	jne @F
	lodsb
	nearcall ifsep
	jne .error
	jmp .symbol_end
@@:
.symbol_next:
	inc cx
	jmp .symbol_loop

.error:
	mov ax, 0504h
	nearcall setrc
	jmp symbolasm_error

.symbol_end:
	test cx, cx
	jz .error
	mov word [bp + ?string_length], cx

	nearcall chkeol


	push ds
	lds si, [bp + ?string_pointer]
	mov cx, word [bp + ?string_length]
	nearcall zz_hash
	mov word [bp + ?string_hash], bx
	pop ds

	lvar word,	inner_main_index
	 push ax

		; ?inner_main_index used as parameter
	 push word [bp + ?string_pointer + 2]
	 push word [bp + ?string_pointer]
	 push word [bp + ?string_length]

	dualcall zz_match_symbol
	jc .none
		; Here, dx, and bx need to be preserved until
		;  the next call to zz_match_symbol.
	push dx
	push bx

	 push word [bp + ?inner_main_index]
	 push word [bp + ?flags]
	dualcall zz_list_symbol.first
	jmp .next


.loop:
		; ?inner_main_index used as parameter
	 push word [bp + ?string_pointer + 2]
	 push word [bp + ?string_pointer]
	 push word [bp + ?string_length]

	dualcall zz_match_symbol.continue
	jc .end

		; Here, dx, and bx need to be preserved until
		;  the next call to zz_match_symbol.
	push dx
	push bx

	 push word [bp + ?inner_main_index]
	 push word [bp + ?flags]
	dualcall zz_list_symbol.subsequent
.next:
	pop bx
	pop dx
	jmp .loop


.none:
	mov dx, msg.zz_match_none
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_match_add_none
@@:
	nearcall disp_message

.end:
	lleave
	retn


zz_list:
	lframe
	lenter
	lequ 1,		flag_is_add
	lequ 2,		flag_is_base
	xor ax, ax
	lvar word,	flags
	 push ax
	lvar dword,	start
	 push ax
	 push ax
	dec ax
	lvar dword,	end
	 push ax
	 push ax

	nearcall skipwhite

	dec si
	mov dx, msg.add
	nearcall isstring?
	jne @F

	inc byte [bp + ?flags]	; set ?flag_is_add

@@:
	nearcall skipwhite

	dec si
	mov dx, msg.base
	nearcall isstring?
	jne @F

	or byte [bp + ?flags], ?flag_is_base

@@:
	nearcall skipwhite

	nearcall iseol?
	je .do

	mov bx, word [reg_cs]
	nearcall getlinearaddr
	mov word [bp + ?start], dx
	mov word [bp + ?start + 2], bx
	mov word [bp + ?end], dx
	mov word [bp + ?end + 2], bx

	nearcall skipwh0
	nearcall iseol?
	je .do

	mov bx, word [reg_cs]
	nearcall getlinearaddr
	mov word [bp + ?end], dx
	mov word [bp + ?end + 2], bx
	nearcall chkeol

.do:
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_list_add_range
	nearcall disp_message
@@:
	mov dx, msg.zz_list_range_first
	nearcall disp_message
	mov ax, word [bp + ?start + 2]
	nearcall disp_ax_hex
	mov ax, word [bp + ?start]
	nearcall disp_ax_hex
	mov dx, msg.zz_list_range_second
	nearcall disp_message
	mov ax, word [bp + ?end + 2]
	nearcall disp_ax_hex
	mov ax, word [bp + ?end]
	nearcall disp_ax_hex
	mov dx, msg.zz_list_range_third
	nearcall disp_message

	mov dx, word [bp + ?start + 2]
	mov ax, word [bp + ?start]
	mov cx, word [bp + ?end + 2]
	mov bx, word [bp + ?end]

	nearcall binsearchmain
	jcxz .none

	push bx
	push word [bp + ?flags]
	dualcall zz_list_symbol.first
	jmp .next

.loop:
	push bx
	push word [bp + ?flags]
	dualcall zz_list_symbol.subsequent
.next:
	inc bx
	loop .loop
.end:
	mov dx, msg.zz_list_end
	nearcall disp_message
	jmp .ret

.none:
	mov dx, msg.zz_list_none
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_list_add_none
@@:
	nearcall disp_message

.ret:
	lleave
	retn


		; INP:	word [ss:sp + 2] = main index
		;	word [ss:sp] = list flags
		; CHG:	ax, dx, es, di
dualfunction
zz_list_symbol:
.first:
	db __TEST_IMM8		; (skip stc, NC)
.subsequent:
	stc
	lframe dualdistance
	lpar word,	main_index
	lequ 1,		flag_is_add
	lequ 2,		flag_is_base
	lequ 4,		flag_is_single_line
	lpar word,	flags
	lenter
	lvar word,	bit_0_is_subsequent
	 pushf
	mov ax, -1
	lvar word,	base_index
	 push ax
	push bx

	 push word [bp + ?main_index]
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

	test byte [bp + ?bit_0_is_subsequent], 1
	jnz .loop

	test byte [bp + ?flags], ?flag_is_single_line
	jnz .loop_first
	mov dx, msg.zz_list_start
	nearcall disp_message
	jmp .loop_first
.loop:
	mov dx, msg.zz_list_between
	nearcall disp_message
.loop_first:
	mov dx, msg.zz_list_first
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_list_add_first
@@:
	nearcall disp_message
	mov dx, word [es:di + smLinear + 2]
	mov ax, word [es:di + smLinear]

	test byte [bp + ?flags], ?flag_is_base
	jz @F
	sub ax, word [vregs + 1 * 4]
	sbb dx, word [vregs + 1 * 4 + 2]
	jmp .have_base
@@:

	mov bx, word [es:di + smBase]
	cmp bx, -1
	je .have_base
	mov word [bp + ?base_index], bx

	 push bx
	 push ax
	dualcall getfarpointer.main
	 pop di
	 pop es

	sub ax, word [es:di + smLinear]
	sbb dx, word [es:di + smLinear + 2]

	 push word [bp + ?main_index]
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es
.have_base:

	push ax
	mov ax, dx
	test ah, ah
	jz @F
	xchg al, ah
	nearcall disp_al_hex
	xchg al, ah
@@:
	nearcall disp_al_hex
	pop ax
	nearcall disp_ax_hex

	mov al, 'h'
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	nearcall putc
@@:

	test byte [bp + ?flags], ?flag_is_base
	jz @F
	mov dx, msg.zz_list_base
	nearcall disp_message
	jmp .displayed_base
@@:
	mov bx, word [bp + ?base_index]
	cmp bx, -1
	je .displayed_base

	mov dx, msg.zz_list_base_symbol
	nearcall disp_message

	 push bx
	dualcall displaystring

	 push word [bp + ?main_index]
	 push ax		; (reserve space, uninitialised)
	dualcall getfarpointer.main
	 pop di
	 pop es

.displayed_base:

	mov dx, msg.zz_list_second
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_list_add_second
@@:
	nearcall disp_message
	mov ax, word [es:di + smOffset + 2]
	test ax, ax
	jz @F
	nearcall disp_ax_hex
@@:
	mov ax, word [es:di + smOffset]
	nearcall disp_ax_hex

	mov dx, msg.zz_list_middle
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_list_add_middle
@@:
	nearcall disp_message
	 push word [bp + ?main_index]
	dualcall displaystring
	mov dx, msg.zz_list_last
	test byte [bp + ?flags], ?flag_is_add
	jz @F
	mov dx, msg.zz_list_add_last
@@:
	nearcall disp_message

	pop bx
	lleave
	dualreturn
	lret


zz_add:
	nearcall skipwhite

	xor bx, bx
	lframe
	lenter
	lvar dword,	string_pointer
	 push ss
	 push bx
	lvar word,	string_length
	 push bx
	lvar dword,	linear
	 push bx
	 push bx
	lvar dword,	offset
	 push bx
	 push bx
	lvar word,	insertflags
	 push bx

	clropt [internalflags2], dif2_poison
	or word [sym_storage.main.based.base], -1
				; set base to none
		; base is set by expression evaluator if any is used

.loop:
	nearcall iseol?
	jne @F

		; ?linear used as parameter to zz_insert
		; ?offset used as parameter to zz_insert
		; ?insertflags used as parameter to zz_insert
	les dx, [bp + ?string_pointer]
	mov cx, word [bp + ?string_length]
	test cx, cx
	jz .error

	testopt [internalflags2], dif2_poison
	jnz .poisoned

	dualcall zz_insert

.ret:
	lleave code
	retn


		; cx = symbol name length
.poisoned:
	nearcall zz_commit_insert

	mov ax, 050Dh
	nearcall setrc
	mov dx, msg.poison_block
	nearcall putsz_error

	mov bx, word [bp + ?string_pointer]
	nearcall zz_del_match.add_poison_entrypoint
	jmp .ret


@@:
	dec si
	lodsw			; get single-letter type name, if any
	nearcall uppercase		; uppercase type name letter in al
	cmp ah, ':'
	jne @F
	mov ah, '='
@@:
	dec si			; -> at = or :
	cmp ax, "S="
	je .symbol
	cmp ax, "O="
	je .offset
	cmp ax, "L="
	je .linear
	cmp ax, "F="
	je .flags
	dec si			; -> at first letter of type name

	mov dx, msg.symbol
	nearcall isstring?
	je .symbol
	mov dx, msg.offset
	nearcall isstring?
	je .offset
	mov dx, msg.linear
	nearcall isstring?
	je .linear
	mov dx, msg.flags
	nearcall isstring?
	je .flags

	lodsb
	mov bx, word [reg_cs]
	nearcall getaddrX

	xor cx, cx
%if _PM
	 push ax
	 push ax
	dualcall push_cxdx_or_edx
	pop word [bp + ?offset]
	pop word [bp + ?offset + 2]
%else
	mov word [bp + ?offset + 2], cx
	mov word [bp + ?offset], dx
%endif
	push ax
	nearcall getlinear_32bit
	jc .error
	mov word [bp + ?linear], ax
	mov word [bp + ?linear + 2], dx
	pop ax		; al = next character
	jmp .next

.skip_equal:
	lodsb
	cmp al, '='
	je @F
	cmp al, ':'
	jne .error
@@:
	lodsb
	retn

.flags:
	call .skip_equal
	nearcall getword
	mov word [bp + ?insertflags], dx
	jmp .next

.offset:
	call .skip_equal
	nearcall getexpression	; bx:dx = expression
	mov word [bp + ?offset], dx
	mov word [bp + ?offset + 2], bx
	jmp .next

.linear:
	call .skip_equal
	nearcall getexpression	; bx:dx = expression
	mov word [bp + ?linear], dx
	mov word [bp + ?linear + 2], bx
	jmp .next

.symbol:
	call .skip_equal
	xor cx, cx
	mov ah, al
	cmp al, "'"
	je .symbol_quoted
	cmp al, '"'
	je .symbol_quoted
	dec si
	mov ah, 0
.symbol_quoted:
	mov word [bp + ?string_pointer], si
.symbol_loop:
	lodsb
	cmp ah, 0
	jne @F
	nearcall ifsep
	je .symbol_end
	jmp .symbol_next
@@:
	cmp al, 13
	je .error
	test al, al
	jz .error
	cmp al, ah
	jne @F
	lodsb
	nearcall ifsep
	jne .error
	jmp .symbol_end
@@:
.symbol_next:
	inc cx
	jmp .symbol_loop

.symbol_end:
	jcxz .error
	xchg cx, word [bp + ?string_length]
	test cx, cx
	jnz .error
.next:
	nearcall skipwh0
	jmp .loop


.error:
	mov ax, 0505h
	nearcall setrc
	jmp symbolasm_error

	lleave ctx


		; INP:	si -> first character after /S switch start
		; OUT:	al = first character after parsed switch end
		;	si -> next character
zz_switch_s: section_of_function
	lodsb
	nearcall uppercase
	cmp al, 'R'
	jne .notr
	clropt [internalflags2], dif2_sym_req_mask
	setopt [internalflags2], dif2_sym_req_86mm
	jmp @F
.notr:
	cmp al, '*'
	jne .notasterisk
	clropt [internalflags2], dif2_sym_req_mask
	setopt [internalflags2], dif2_sym_req_86mm | dif2_sym_req_xms
	jmp @F
.notasterisk:
	cmp al, 'X'
	jne .notx
	clropt [internalflags2], dif2_sym_req_mask
	setopt [internalflags2], dif2_sym_req_xms
@@:
	lodsb
	cmp al, 32
	push ax
	push si
	ja .x_cont
	jmp .end
.x_cont:
	pop si
	pop ax
.notx:
	cmp al, ':'
	je .skip
	cmp al, '='
	jne symbolasm_error
.skip:
	inc si
	push si
	dec si
	mov dx, msg.max
	nearcall isstring?
	jne @F
	lodsb
	mov dx, -1
	mov bx, dx
	jmp .common

@@:
	pop ax
	lodsb
	push si
	cmp al, '('
	jne .not_expr
	lodsb
	nearcall getexpression
	nearcall skipwh0
	cmp al, ')'
	lodsb
	jne symbolasm_error
	jmp .common

.not_expr:
	nearcall zz_get_literal
.common:
	cmp bx, -1
	jne @F
	cmp dx, -1
	je @FFF
@@:
	testopt [internalflags2], dif2_sym_req_86mm
	jnz @F
	test bx, bx
	jnz .error_pop_si
	cmp dx, kib((SYMMAIN_size + SYMHASH_size + SYMSTR_index_size) * 65535)
	ja .error_pop_si
	jmp @FF

@@:
	test bx, bx
	jnz .error_pop_si
	cmp dx, _86MM_SYMBOL_TABLE_MAX
	ja .error_pop_si
@@:

	pop cx			; (discard si)
	push ax			; preserve next character
	push si
	xchg ax, dx		; ax = size
	test ax, ax
	jz .free

	push ax			; on stack: originally requested amount KiB
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .notxms
%endif

	testopt [internalflags2], dif2_sym_req_xms
	jz .notxms

	cmp word [zz_xms.entry + 4], 0
	jne @F
	cmp word [zz_xms.entry + 6], 0
	jne @F
	cmp word [zz_xms.entry + 2], 0
	je .no_xms
@@:

	cmp ax, -1
	jne @F

	mov ax, kib( \
		(SYMHASH_index_size + SYMMAIN_index_size + SYMSTR_index_size) \
		 * 64 * 1024)	; ax = how many KiB
@@:
	mov bx, ax
	xor dx, dx
	mov cx, 6
.loop_shl_6:
	shl ax, 1
	rcl dx, 1		; to paragraphs
	loop .loop_shl_6

@@:
	push dx
	push ax
	mov ah, 09h
	add bx, kib(fromparas(paras(ssString + 255)) + XMSTRANSFER_default_size)
	jc symbolasm_error
	mov dx, bx
	push bx
	nearcall zz_call_xms	; dx = allocated handle
	dec ax
	pop bx			; bx = how many KiB allocated
	pop ax
	pop cx			; cx:ax = how many paragraphs allocated
				;  (not including the XMS transfer buffer)
	jz @F
	mov ax, 0506h
	mov dx, msg.zz_fail_xms_alloc
	jmp .no_xms.error

@@:
	add sp, 2		; discard ax on stack
	jmp xms_init

.no_xms:			; no XMS driver
	mov ax, 0507h
	mov dx, msg.zz_no_xms
.no_xms.error:
	nearcall disp_message
	testopt [internalflags2], dif2_sym_req_86mm
	jnz .notxms
	mov dx, msg.empty_message
	nearcall setrc
 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
	jmp exit_error_msg
 %else
	jmp exit_error_msg_code
 %endif

.notxms:
	pop ax			; restore KiB requested (-1 = max)

	cmp ax, -1		; max ?
	jne @F
	mov ax, _86MM_SYMBOL_TABLE_MAX
				; set maximum size
@@:

	mov bx, ax
	mov cl, 6
	shl ax, cl		; to paragraphs

%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz @FF
	test ax, ax
	jnz @F
	mov dx, msg.zz_switch_s_internal_error
	mov ax, 0509h
	nearcall setrc
	jmp .putsz_end

@@:
	dec ax			; allow for image ident paragraph
@@:
%endif

	push bx
	mov cx, ax
	xor bx, bx
	nearcall shift_left_4_bxcx

	cmp word [zz_xms.handle], -1
	jne @FF

	cmp word [sym_storage.start + 2], 0
	jne @F
	cmp word [sym_storage.start], 0
	je @FF
@@:

	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall normalise_pointer_with_displacement_bxcx
	 pop cx
	 pop bx

	cmp word [sym_storage.end + 2], bx
	jne @F
	cmp word [sym_storage.end], cx
	je .no_size_change
@@:

	nearcall InDos
	jz @F

%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz .notboot1
	testopt [internalflags2], dif2_boot_loaded_kernel
	jz @F

	mov dx, msg.zz_switch_s_boot_loaded_kernel
	jmp .putsz_end_pop

.notboot1:
%endif

	mov dx, msg.zz_switch_s_indos
.putsz_end_pop:
	pop bx
.putsz_end:
	nearcall putsz_error
	jmp .end

@@:

	mov di, ax

%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz @F
%endif
	testopt [internalflags2], dif2_createdprocess
	jz @F

	mov cx, word [created_psp]
	cmp cx, word [pspdbe]	; current PSP the created one ?
	jne @F			; no -->

	dec cx
%if _PM
	mov ax, cx
	nearcall setes2ax
%else
	mov es, cx		; => MCB
%endif
	inc cx
	cmp word [es:1], cx	; right owner ?
	jne @F			; no -->
	mov cx, word [created_size]
	cmp cx, word [es:3]	; right size ?
	jne @F			; no -->

	mov cx, word [created_psp]
	mov bx, 1000h
	mov word [created_size], bx
	mov ah, 4Ah
	push cx			; save segment value
	 push cx		; = value for es
	dual2call _doscall_return_es	; resize to 64 KiB
	 pop cx			; (discard)
	pop cx			; = segment
	dec cx
%if _PM
	mov ax, cx
	nearcall setes2ax
%else
	mov es, cx		; => MCB
%endif
	inc cx
	mov word [es:1], cx	; restore owner to debuggee process

@@:
	mov ax, di
	pop bx

.no_size_change:
	 push ss
	 pop es
	 push ss
	 pop ds
	push ax			; size in paragraphs
	mov dx, msg.zz_switch_s_received
	nearcall putsz
	mov di, line_out
	mov ax, bx
	nearcall decword
	mov ax, " K"
	stosw
	mov ax, "iB"
	stosw
	mov al, '.'
	stosb
	nearcall putsline_crlf

	nearcall zz_compact
	pop cx			; size in paragraphs

	lframe
	lvar dword,	common_size_bytes
	lenter
	lvar word,	req_size_para
	 push cx

	mov dx, cx
	xor bx, bx
	nearcall shift_left_4_bxcx	; new size in bytes


	cmp word [zz_xms.handle], -1
	je .fromnotxms1

.fromxms1:
	cmp bx, word [sym_storage.xms.free + 2]
	jne @F
	cmp cx, word [sym_storage.xms.free]
@@:
	jb .new_smaller
	mov bx, word [sym_storage.xms.free + 2]
	mov cx, word [sym_storage.xms.free]
	jmp .fromxms_old_smaller

.fromnotxms1:
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall normalise_pointer_with_displacement_bxcx
	 pop cx
	 pop bx			; -> after allocated block, with new size

	cmp bx, word [sym_storage.free + 2]
	jne @F
	cmp cx, word [sym_storage.free]
@@:
	je .lframe_no_reallocation_needed
	jb .new_smaller
.old_smaller:
	mov bx, word [sym_storage.free + 2]
	mov cx, word [sym_storage.free]
	jmp .got_smaller

.new_smaller:
..@new_smaller:
	mov dx, msg.zz_too_full
	nearcall putsz_error
	jmp .lframe_end

.got_smaller:
	 push bx
	 push cx
	dualcall pointer_to_linear
	xchg dx, bx
	xchg ax, cx		; bx:cx = linear after end
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall pointer_to_linear	; dx:ax = linear start
	sub cx, ax
	sbb bx, dx		; end - start = size
				; bx:cx = smaller of current size and requested
				;  (in bytes)
.fromxms_old_smaller:
	mov word [bp + ?common_size_bytes], cx
	mov word [bp + ?common_size_bytes + 2], bx

	nearcall bxcx_to_cx_paragraphs
	mov dx, cx		; dx = smaller in paragraphs

%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz .notboot2

	push dx
	nearcall bootgetmemorysize
	pop bx
	int 12h
	mov cl, 10 - 4
	shl ax, cl
	cmp ax, dx
	je @F

	mov dx, msg.zz_switch_s_boot_rpl
	jmp .lframe_end_putsz_error

@@:
	mov dx, [boot_new_memsizekib]
	shl dx, cl
	cmp ax, dx
	je @F

	mov dx, msg.zz_switch_s_boot_memsize_differ
	jmp .lframe_end_putsz_error

@@:
	cmp word [zz_xms.handle], -1
	jne .fromxms2

	mov cx, [bp + ?req_size_para]
	inc cx
	sub ax, cx
				; => transfer buffer
%if 0
new = how much to allocate (?req_size_para + 1 paragraph)
ebda = how large is EBDA
old = how much was allocated
?common_size_bytes = at most the smaller of new - 16B and old - 16B
transfer area must hold ?common_size_bytes

transfer area may not overlap old source
transfer area may not overlap ebda source
transfer area may not overlap ebda target
transfer area may not overlap new target

ebda source and ebda target may overlap
old source and new target may overlap


new=100 ebda=1 old=1 memsize=200
98 => transfer
198 => ebda
199 => old
100 => target new
99 => target ebda

new=1 ebda=1 old=100 memsize=200
98 => transfer
99 => ebda
100 => old
199 => target new
198 => target ebda

new=2 ebda=100 old=1 memsize=200
97 => transfer
99 => ebda
199 => old
198 => target new
98 => target ebda

new=50 ebda=50 old=1 memsize=200
99 => transfer
149 => ebda
199 => old
150 => target new
100 => target ebda

new=1 ebda=50 old=50 memsize=200
99 => transfer
100 => ebda
150 => old
199 => target new
149 => target ebda

new=40 ebda=1 old=50 memsize=200
109 => transfer
149 => ebda
150 => old
160 => target new
159 => target ebda
%endif
	jc @F
	cmp ax, 1000h
	jae @FF
@@:
	mov dx, msg.zz_switch_s_boot_transfer_too_low
	jmp .lframe_end_putsz_error

@@:
	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]
	push ax			; allocated transfer buffer
	nearcall zz_transfer_buffer	; transfer into transfer buffer
	pop word [sym_storage.start + 2]

.fromxms2:
	mov ax, dx		; => current EBDA
	mov dx, ss
	dec dx			; => NDEB image ident paragraph
	sub dx, [bp + ?req_size_para]
	 push dx
	dec dx			; => target symbol table image ident
	xor cx, cx		; size of EBDA to move (if none)
	push ds
	mov ds, cx
	mov bx, word [40Eh]	; new ref in word [0:40Eh] (if none)
	pop ds
	cmp byte [ boot_ebdaflag ], 0	; any EBDA ?
	jz .noebda

	push ds
	mov ds, ax		; => EBDA
	xor bx, bx
	mov bl, byte [ 0 ]	; EBDA size in KiB
	mov cl, 6
	shl bx, cl		; *64, to paragraphs
	mov cx, bx		; = size of EBDA to move (in paragraphs)
	sub dx, cx		; => target EBDA
	mov bx, dx		; = new EBDA reference to put in word [0:40Eh]
	pop ds

.noebda:

	nearcall movp		; move EBDA
	mov cl, 6
	shr dx, cl		; to KiB
	mov word [ boot_new_memsizekib ], dx
	push ds
	xor cx, cx
	mov ds, cx
	mov word [40Eh], bx	; = new ref in word [0:40Eh]
	mov word [413h], dx
	pop ds

	 pop ax
	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]


	cmp word [zz_xms.handle], -1
	je .fromnotxms3

.fromxms3:
	nearcall zz_xms_to_86mm
	nearcall zz_xms_try_free_handle
	jmp .from3common

.fromnotxms3:
	push ax
	nearcall zz_transfer_buffer		; transfer into target
	pop ax

.from3common:
	push ax
	dec ax
	mov bx, word [bp + ?req_size_para]
	inc bx				; = how many paragraphs used
	mov es, ax			; => paragraph for imageident
	xor di, di			; -> imageident target

	mov word [zz_imageident.size], bx
					; set image ident size
	mov word [zz_imageident.check], di
	mov si, zz_imageident
	push si
	mov cx, 8
	xor dx, dx
@@:
	lodsw
	add dx, ax
	loop @B
	pop si

	neg dx
	mov word [zz_imageident.check], dx
					; set image ident checksum
	mov cl, 8
	rep movsw			; write image ident paragraph

	push ss
	pop es

	pop word [sym_storage.start + 2]

	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall normalise_pointer_with_displacement_bxcx
	 pop word [sym_storage.end]
	 pop word [sym_storage.end + 2]
				; size common part

	jmp .init_tail


.notboot2:
%endif

	cmp word [zz_xms.handle], -1
	jne .fromxms4

.fromnotxms4:
	mov bx, 1		; enable UMB link
	mov ax, 5803h
	doscall
	mov bx, 80h		; first fit, UMA then LMA
	mov ax, 5801h
	doscall
	jnc @F
	xor bx, bx		; first fit (LMA then UMA)
	mov ax, 5801h
	doscall
@@:

	mov bx, dx

	test bx, bx		; prior size was none ?
	jz .no_prefix		; no prefix -->

	mov ah, 48h
	doscall
	jnc @F

	mov dx, msg.zz_s_cannot_alloc_transfer
.lframe_end_putsz_error:
	nearcall putsz_error
	jmp .lframe_end_restore_strat

@@:
	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]

	push ax			; allocated transfer buffer
	nearcall zz_transfer_buffer
	 push word [sym_storage.start + 2]
	mov ah, 49h
	dual2call _doscall_return_es	; free prior buffer
	 pop ax			; (discard)
	pop word [sym_storage.start + 2]
				; relocate

	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall normalise_pointer_with_displacement_bxcx
	 pop word [sym_storage.end]
	 pop word [sym_storage.end + 2]
				; size common part

.no_prefix:
.fromxms4:
	mov bx, 02h		; last fit
	mov ax, 5801h
	doscall
	mov bx, 0		; disable UMB link
	mov ax, 5803h
	doscall

	mov bx, word [bp + ?req_size_para]
	mov ah, 48h
	doscall			; allocate target buffer
	jnc @F

	mov dx, msg.zz_s_cannot_alloc_target
	nearcall putsz_error
	jmp .lframe_end_restore_strat

@@:
	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]

	cmp word [zz_xms.handle], -1
	je .fromnotxms5

.fromxms5:
	nearcall zz_xms_to_86mm
	and word [sym_storage.start], 0
	mov word [sym_storage.start + 2], ax

	mov cx, word [bp + ?common_size_bytes]
	mov bx, word [bp + ?common_size_bytes + 2]
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall normalise_pointer_with_displacement_bxcx
	 pop word [sym_storage.end]
	 pop word [sym_storage.end + 2]
				; size common part
	nearcall zz_xms_try_free_handle
	jmp .from5common

.fromnotxms5:
	push ax			; allocated memory block
	nearcall zz_transfer_buffer
	pop ax			; allocated memory block
	xchg word [sym_storage.start + 2], ax
	test ax, ax
	jz @F
	 push ax		; value for es to int 21h call
	mov ah, 49h
	dual2call _doscall_return_es	; free transfer buffer
	 pop ax			; (discard)
@@:
.from5common:

.init_tail:
	mov cx, word [bp + ?req_size_para]
	xor bx, bx		; bx:cx = number of paragraphs allocated
	nearcall shift_left_4_bxcx	; = number of bytes allocated

	sub cx, word [bp + ?common_size_bytes]
	sbb bx, word [bp + ?common_size_bytes + 2]
				; = number of bytes allocated after end
	push word [sym_storage.end + 2]
	push word [sym_storage.end]
	pop di
%if _PM
	 push word [symsel1]
	dualcall segment_to_selector
%endif
	pop es			; es:di -> after end of common part
	xor ax, ax
.init_loop:
	 push es
%if _PM
	dualcall selector_to_segment
%endif
	 push di
	dualcall normalise_pointer
	 pop di
%if _PM
	 push word [symsel1]
	dualcall segment_to_selector
%endif
	 pop es			; normalise pointer
	sub cx, SEGTRANSFER_size
	sbb bx, 0
	jc .init_last

	push cx
	mov cx, SEGTRANSFER_size >> 1
	rep stosw		; store zeros
	pop cx
	jmp .init_loop

.init_last:
	add cx, SEGTRANSFER_size
	adc bx, 0
	shr cx, 1
	rep stosw		; store zeros

	mov cx, word [bp + ?req_size_para]
	xor bx, bx
	nearcall shift_left_4_bxcx	; requested size bytes
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall normalise_pointer_with_displacement_bxcx
	 pop word [sym_storage.end]
	 pop word [sym_storage.end + 2]
	 			; update .end

	cmp word [bp + ?common_size_bytes + 2], 0
	jne @F			; if it was an empty table
	cmp word [bp + ?common_size_bytes], 0
	jne @F

				; initialise pointers
	mov dx, word [sym_storage.start + 2]
	mov ax, word [sym_storage.start]
	mov word [sym_storage.main.start + 2], dx
	mov word [sym_storage.main.start], ax
	mov word [sym_storage.hash.start + 2], dx
	mov word [sym_storage.hash.start], ax
	mov word [sym_storage.str.start + 2], dx
	mov word [sym_storage.str.start], ax
	mov word [sym_storage.free + 2], dx
	mov word [sym_storage.free], ax
@@:

	nearcall zz_expand

	cmp word [bp + ?common_size_bytes + 2], 0
	jne @F			; if it was an empty table ...-
	cmp word [bp + ?common_size_bytes], 0
	jne @F
	nearcall zz_store_pre_str	;  -... store the pre strings
@@:

.lframe_end_restore_strat:
	nearcall zz_restore_strat

.lframe_end:
.lframe_no_reallocation_needed:
	lleave
	jmp .end

.end_restore_strat:
	nearcall zz_restore_strat

.end:
	pop si
	pop ax			; al = next character
	push ss
	pop es
	push ss
	pop ds
	retn


.free:
	mov dx, msg.zz_switch_s_freeing
	nearcall putsz
	nearcall zz_free_nonxms
	nearcall zz_xms_try_free_handle
	nearcall zz_free_reset
	jmp .end


zz_free_nonxms: section_of_function
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz .free_notboot

	mov dx, msg.zz_switch_s_boot_loaded_kernel
	testopt [internalflags2], dif2_boot_loaded_kernel
	jnz .free_putsz_error_end

	nearcall bootgetmemorysize
	int 12h
	mov cl, 10 - 4
	shl ax, cl
	cmp ax, dx
	je @F

	mov dx, msg.zz_switch_s_boot_rpl
	jmp .free_putsz_error_end

@@:
	mov dx, [boot_new_memsizekib]
	shl dx, cl
	cmp ax, dx
	je @F

	mov dx, msg.zz_switch_s_boot_memsize_differ
.free_putsz_error_end:
	nearcall putsz_error
	retn

@@:
				; ax => current EBDA
	mov dx, ss
	dec dx			; => NDEB image ident paragraph
	xor cx, cx		; size of EBDA to move (if none)
	push ds
	mov ds, cx
	mov bx, word [40Eh]	; new ref in word [0:40Eh] (if none)
	pop ds
	cmp byte [ boot_ebdaflag ], 0	; any EBDA ?
	jz .free_noebda

	push ds
	mov ds, ax		; => EBDA
	xor bx, bx
	mov bl, byte [ 0 ]	; EBDA size in KiB
	mov cl, 6
	shl bx, cl		; *64, to paragraphs
	mov cx, bx		; = size of EBDA to move (in paragraphs)
	sub dx, cx		; => target EBDA
	mov bx, dx		; = new EBDA reference to put in word [0:40Eh]
	pop ds

.free_noebda:
	nearcall movp		; move EBDA
	mov cl, 6
	shr dx, cl		; to KiB
	mov word [ boot_new_memsizekib ], dx
	push ds
	xor cx, cx
	mov ds, cx
	mov word [40Eh], bx	; = new ref in word [0:40Eh]
	mov word [413h], dx
	pop ds
	retn


.free_notboot:
%endif

zz_free_dos: section_of_function
	xor ax, ax
	xchg ax, word [sym_storage.start + 2]

	test ax, ax
	jz @F
	dec ax
%if _PM
	nearcall setes2ax
%else
	mov es, ax
%endif
	xor ax, ax		; ax = 0
	mov word [es:1], ax	; free MCB
@@:
	retn


zz_free_reset: section_of_function
	push ss
	pop es

	xor ax, ax

	mov di, sym_storage.clear_on_free.first
	mov cx, \
	 (sym_storage.clear_on_free.after - sym_storage.clear_on_free.first) \
	 >> 1
	rep stosw

	dec ax			; = 0FFFFh

	mov di, sym_storage.minus1_on_free.first
	mov cx, \
	 (sym_storage.minus1_on_free.after - sym_storage.minus1_on_free.first) \
	 >> 1
	rep stosw
	retn


bxcx_to_cx_paragraphs: section_of_function
	add cx, 15
	adc bx, 0
shift_right_4_bxcx:
%rep 4
	shr bx, 1
	rcr cx, 1		; cx = paragraphs
%endrep
	retn


		; INP:	ax => allocated memory
		;	bx:cx = size of compacted table in bytes
		; CHG:	bx, cx, dx, di, si
zz_xms_to_86mm: section_of_function
	mov dx, word [zz_xms.handle]
	mov si, zz_xms.movestruc
	xor di, di

	mov word [si + xmsmDestHandle], di		; 86M memory
	mov word [si + xmsmDestAddress + 2], ax
	mov word [si + xmsmDestAddress], di		; ax:0 -> memory
	mov word [si + xmsmSourceHandle], dx		; our handle
	mov word [si + xmsmSourceAddress], di
	mov word [si + xmsmSourceAddress + 2], di	; 0 = beginning
	mov word [si + xmsmCount], cx
	mov word [si + xmsmCount + 2], bx		; size of compacted tables
	nearcall call_xms_move

	mov cx, .table.amount
	mov bx, .table
@@:
	mov si, word [bx]		; -> source xms offset dword
	mov di, word [bx + 2]		; -> dest memory far pointer
	add bx, 4			; -> next .table entry, if any
	push cx
	push bx
	xor cx, cx			; = 0
	 push ax
	 push cx			; -> far to start of memory
	mov cx, word [si]
	mov bx, word [si + 2]		; = xms block offset
	dualcall normalise_pointer_with_displacement_bxcx
	 pop word [di]
	 pop word [di + 2]		; write far pointer
	pop bx				; -> next .table entry
	pop cx				; = loop counter
	loop @B
	retn


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
.table:
	dw sym_storage.main.xms.start
	dw sym_storage.main.start

	dw sym_storage.hash.xms.start
	dw sym_storage.hash.start

	dw sym_storage.str.xms.start
	dw sym_storage.str.start

	dw sym_storage.xms.free
	dw sym_storage.free

	dw sym_storage.xms.end
	dw sym_storage.end
.table.amount: equ ($ - .table) >> 2


 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
	usesection lDEBUG_CODE2
 %else
	usesection lDEBUG_CODE
 %endif


		; INP:	word [sym_storage.start + 2] => allocated memory
		;	dx = handle of allocated block
		;	bx:cx = size of compacted table in bytes
		; CHG:	ax, bx, cx, dx, di, si
zz_86mm_to_xms: section_of_function
	mov si, zz_xms.movestruc
	xor di, di

	mov word [si + xmsmSourceHandle], di		; 86M memory
	push word [sym_storage.start + 2]
	pop word [si + xmsmSourceAddress + 2]
	mov word [si + xmsmSourceAddress], di		; .start:0 -> memory
	mov word [si + xmsmDestHandle], dx		; our handle
	mov word [si + xmsmDestAddress], di
	mov word [si + xmsmDestAddress + 2], di		; 0 = beginning
	mov word [si + xmsmCount], cx
	mov word [si + xmsmCount + 2], bx		; size of compacted tables
	nearcall call_xms_move

	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall pointer_to_linear	; dx:ax = linear start

	mov cx, zz_xms_to_86mm.table.amount
	mov bx, zz_xms_to_86mm.table
@@:
	mov di, word [bx]	; -> dest xms offset dword
	mov si, word [bx + 2]	; -> source memory far pointer
	add bx, 4		; -> next .table entry, if any
	push cx
	push bx

	xchg dx, bx
	xchg ax, cx		; bx:cx = linear start


	 push word [si + 2]
	 push word [si]
	dualcall pointer_to_linear	; dx:ax = linear of pointer
	sub ax, cx
	sbb dx, bx		; pointer - start = byte offset
	mov word [di], ax
	mov word [di + 2], dx
	xchg dx, bx
	xchg ax, cx		; dx:ax = linear start

	pop bx			; -> next .table entry
	pop cx			; = loop counter
	loop @B

	retn


		; cx:ax = amount of paragraphs allocated (<= 1.5 MiB/ 2 MiB)
		;	   (not including transfer buffer)
		; bx = amount of KiB allocated
		;	(actual allocation, including transfer buffer)
		; dx = XMS handle
xms_init:
	xor di, di
	lframe near
	lenter
	lvar dword,	common_size_bytes
	 push di
	 push di
	lvar dword,	amountpara
	 push cx
	 push ax
	lvar word,	amountkib
	 push bx
	lvar word,	newhandle
	 push dx

	push ax
	push bx
	push cx
	push dx

	nearcall zz_compact

	mov dx, word [zz_xms.handle]
	cmp dx, -1
	je .notfromxms

.fromxms:
	mov bx, word [bp + ?amountpara + 2]
	mov cx, word [bp + ?amountpara]
	nearcall shift_left_4_bxcx				; requested size bytes

	cmp bx, word [sym_storage.xms.free + 2]
	jne @F
	cmp cx, word [sym_storage.xms.free]
@@:
	jae @F

.new_smaller:
	mov dx, [bp + ?newhandle]
	lleave code

	nearcall zz_xms_try_free
	jmp ..@new_smaller


@@:
	mov bx, word [sym_storage.xms.free + 2]
	mov cx, word [sym_storage.xms.free]

	mov si, zz_xms.movestruc
	xor di, di

	push word [bp + ?newhandle]
	pop word [si + xmsmDestHandle]			; new handle
	mov word [si + xmsmDestAddress + 2], di
	mov word [si + xmsmDestAddress], di		; 0
	mov word [si + xmsmSourceHandle], dx		; old handle
	mov word [si + xmsmSourceAddress], di
	mov word [si + xmsmSourceAddress + 2], di	; 0
	mov word [si + xmsmCount], cx
	mov word [si + xmsmCount + 2], bx		; size of compacted tables

	mov word [bp + ?common_size_bytes], cx
	mov word [bp + ?common_size_bytes + 2], bx
	nearcall call_xms_move

	nearcall zz_xms_try_free

	push word [bp + ?newhandle]
	pop word [zz_xms.handle]

	jmp .fromxmscommon


.notfromxms:
	cmp word [sym_storage.start + 2], 0
	je .fromxmscommon

	mov bx, word [bp + ?amountpara + 2]
	mov cx, word [bp + ?amountpara]
	nearcall shift_left_4_bxcx				; requested size bytes

	push bx
	push cx

	 push word [sym_storage.free + 2]
	 push word [sym_storage.free]
	dualcall pointer_to_linear
	xchg dx, bx
	xchg ax, cx		; bx:cx = linear after end
	 push word [sym_storage.start + 2]
	 push word [sym_storage.start]
	dualcall pointer_to_linear	; dx:ax = linear start
	sub cx, ax
	sbb bx, dx		; end - start = size
				; bx:cx = current size (in bytes)
	mov word [bp + ?common_size_bytes], cx
	mov word [bp + ?common_size_bytes + 2], bx

	pop ax
	pop dx			; = new size
	cmp dx, bx
	jne @F
	cmp ax, cx
@@:
	jb .new_smaller

	mov dx, word [bp + ?newhandle]
	nearcall zz_86mm_to_xms

	push word [bp + ?newhandle]
	pop word [zz_xms.handle]

	nearcall zz_free_nonxms

.fromxmscommon:
	mov dx, msg.zz_switch_s_received_xms
	nearcall putsz
	mov di, line_out
	mov ax, word [bp + ?amountkib]
	nearcall decword
	mov ax, " K"
	stosw
	mov ax, "iB"
	stosw
	mov al, '.'
	stosb
	nearcall putsline_crlf

	pop dx
	pop cx
	pop bx
	pop ax

	mov si, zz_xms.movestruc

	mov bx, cx
	mov cx, ax
	nearcall shift_left_4_bxcx	; to bytes

	xor ax, ax

	cmp word [bp + ?common_size_bytes + 2], ax
	jne @F
	cmp word [bp + ?common_size_bytes], ax
@@:
	jne .no_init_start

		; Initialise

	mov word [sym_storage.main.xms.start], ax
	mov word [sym_storage.main.xms.start + 2], ax

	mov word [sym_storage.hash.xms.start], ax
	mov word [sym_storage.hash.xms.start + 2], ax

	mov word [sym_storage.str.xms.start], ax
	mov word [sym_storage.str.xms.start + 2], ax

	mov word [sym_storage.xms.free], ax
	mov word [sym_storage.xms.free + 2], ax

.no_init_start:
	mov word [sym_storage.xms.end], cx
	mov word [sym_storage.xms.end + 2], bx

	push word [bp + ?newhandle]
	pop word [zz_xms.handle]



XMSTRAIL_SIZE equ fromkib(kib(fromparas(paras(ssString + 255)) + XMSTRANSFER_default_size))
XMSTRAIL_TRANSFER_OFFSET equ XMSTRAIL_SIZE - XMSTRANSFER_default_size
	add cx, XMSTRAIL_TRANSFER_OFFSET & 0FFFFh
	adc bx, XMSTRAIL_TRANSFER_OFFSET >> 16		; -> transfer buffer

	mov word [zz_xms.transfer_address], cx
	mov word [zz_xms.transfer_address + 2], bx	; set address

	mov word [zz_xms.transfer_size], XMSTRANSFER_default_size & 0FFFFh
	mov word [zz_xms.transfer_size + 2], XMSTRANSFER_default_size >> 16
							; set size

	add cx, XMSTRANSFER_default_size & 0FFFFh
	adc bx, XMSTRANSFER_default_size >> 16		; initialise all memory

	push word [bp + ?newhandle]
	pop word [si + xmsmDestHandle]			; our handle
	mov dx, word [sym_storage.xms.free + 2]
	mov di, word [sym_storage.xms.free]
	mov word [si + xmsmDestAddress], di
	mov word [si + xmsmDestAddress + 2], dx
	mov word [si + xmsmSourceHandle], ax		; 86M memory
	push ds
%if _PM
	dualcall selector_to_segment
%endif
	pop word [si + xmsmSourceAddress + 2]
	mov word [si + xmsmSourceAddress], zz_xms.16_zeros

	sub cx, di
	sbb bx, dx

	mov word [si + xmsmCount + 2], ax
	mov ax, cx
	and ax, 15
	mov word [si + xmsmCount], ax
	jz @F

	nearcall call_xms_move
	add word [si + xmsmDestAddress], ax
	adc word [si + xmsmDestAddress + 2], 0
	sub cx, ax
	sbb bx, 0

@@:
	mov byte [si + xmsmCount], 16

.loop:
	sub cx, 16
	sbb bx, 0
	jc .last

	nearcall call_xms_move

	add word [si + xmsmDestAddress], 16
	adc word [si + xmsmDestAddress + 2], 0

	jmp .loop

.last:
	add cx, 16
	adc bx, 0

	jcxz @F
	mov word [si + xmsmCount], cx

	nearcall call_xms_move
@@:

	nearcall zz_expand

	cmp word [bp + ?common_size_bytes + 2], 0
	jne @F			; if it was an empty table ...-
	cmp word [bp + ?common_size_bytes], 0
	jne @F
	nearcall zz_store_pre_str	;  -... store the pre strings

@@:
	lleave

	jmp zz_switch_s.end_restore_strat


zz_free_xms: section_of_function
	nearcall zz_free_reset

zz_xms_try_free_handle: section_of_function
	mov dx, -1
	xchg dx, word [zz_xms.handle]
	cmp dx, -1
	je @F

zz_xms_try_free: section_of_function
	nearcall zz_detect_xms

	mov ah, 0Ah
	nearcall zz_call_xms
	dec ax
	jz @F
	xchg ax, dx
	mov dx, msg.zz_xms_not_freed_1
	nearcall putsz_error
	nearcall disp_ax_hex
	mov dx, msg.zz_xms_not_freed_2
	nearcall putsz_error
@@:
	retn


		; CHG:	ax, ds, es
		; OUT:	ss = es = ds
zz_save_strat: section_of_function
	push ss
	pop ds
	nearcall InDos
	jnz .indos
	mov ax, 5800h
	int 21h
	mov word [zz_saved_strategy], ax
	mov ax, 5802h
	int 21h
	xor ah, ah
	mov word [zz_saved_umblink], ax
.indos:
	nearcall zz_detect_xms
	push ss
	pop ds
	push ss
	pop es
	retn


	; STT:	ss = ds
zz_restore_strat: section_of_function
	nearcall InDos
	jnz .ret
	mov bx, word [zz_saved_umblink]
	mov ax, 5803h
	int 21h
	mov bx, word [zz_saved_strategy]
	mov ax, 5801h
	int 21h
.ret:
	retn


	usesection lDEBUG_CODE

%if _XMS_SYMBOL_TABLE
		; CHG:	-
		; STT:	ss = ds
zz_detect_xms: section_of_function
	testopt [internalflags2], dif2_xms_detection_done
	jnz .retn

	_386_o32
	push ax
	_386_o32
	push bx
	_386_o32
	push cx
	_386_o32
	push dx
	_386_o32
	push si
	_386_o32
	push di
	_386_o32
	push bp

	nearcall InDos
	jz .notindos
	xor ax, ax
%if _PM
	nearcall setes2ax
%else
	mov es, ax
%endif
	mov bx, 4 * 2Fh
	mov ax, word [es:bx + 2]
	mov bx, word [es:bx]
	call .checkxms
.ret:
	_386_o32
	pop bp
	_386_o32
	pop di
	_386_o32
	pop si
	_386_o32
	pop dx
	_386_o32
	pop cx
	_386_o32
	pop bx
	_386_o32
	pop ax

	push ss
	pop ds
	push ss
	pop es
.retn:
	retn

.notindos:
	mov ax, 352Fh
	push word [pspdbg]
	dual2call _doscall_return_es
	pop ax			; returned es
	call .checkxms
	jmp .ret

.checkxms:
%if _PM
	nearcall ispm
	jnz @F
	testopt [options], zz_no_pm_xms
	jnz .checkxms_via_86m
@@:
%endif
	test ax, ax
	jz .no_xms
	cmp bx, -1
	je .no_xms
%if _PM
	clropt [internalflags2], dif2_no_pm_xms
%endif
	mov ax, 4300h
	int 2Fh
	cmp al, 80h
	je .got_xms
%if _PM
	nearcall ispm
	jnz .no_xms
.checkxms_via_86m:
[cpu 286]
	mov ax, 4300h
	push byte 0
	push byte 2Fh
	call intcall
	cmp al, 80h
	jne .no_xms

	mov ax, 4310h
	push byte 0		; es value
	push byte 0		; ds value
	push byte 2Fh
	push bp
	call intcall_return_parameter_es_parameter_ds
	pop ax			; discard returned ds
	pop ax			; get returned es
	and word [zz_xms.entry + 4], 0
	and word [zz_xms.entry + 6], 0
	mov word [zz_xms.entry + 2], ax
	mov word [zz_xms.entry], bx
	setopt [internalflags2], dif2_no_pm_xms | dif2_xms_detection_done
	retn
__CPU__
%endif

.no_xms:
	setopt [internalflags2], dif2_xms_detection_done
	and word [zz_xms.entry], 0
	and word [zz_xms.entry + 2], 0
%if _PM
	and word [zz_xms.entry + 4], 0
	and word [zz_xms.entry + 6], 0
%endif
	retn

.got_xms:
	setopt [internalflags2], dif2_xms_detection_done
	mov ax, 4310h
	int 2Fh
%if _PM
_386	nearcall ispm
_386	jnz .rm
_386	mov dword [zz_xms.entry], ebx
_386	and word [zz_xms.entry + 6], 0
_386	mov word [zz_xms.entry + 4], es
_386	retn
.rm:
%endif
	mov word [zz_xms.entry], bx
	mov word [zz_xms.entry + 2], es
%if _PM
	and word [zz_xms.entry + 4], 0
	and word [zz_xms.entry + 6], 0
%endif
	retn


		; Note:	In dosemu DPMI it seems that 2F.4310 always returns
		;	 a far 32:32 pointer in es:ebx. This must be called
		;	 with a 32-bit stack frame.
zz_call_xms: section_of_function
	cmp word [zz_xms.entry + 2], 0
	jne @F
%if _PM
	cmp word [zz_xms.entry + 4], 0
	jne @F
	cmp word [zz_xms.entry + 6], 0
	jne @F
%endif

.seterror:
	xor ax, ax			; failure
	mov bl, 80h			; function not implemented
	retn

@@:
	push ax
	mov ax, 256
	nearcall stack_check.internal
	pop ax
	jb .seterror			; no, error out -->
%if _PM
_386_PM	nearcall ispm
_386_PM	jnz .rm
	testopt [internalflags2], dif2_no_pm_xms
	jz .pm
.86m_via_pm:
	lframe
	lvar 32h, 86m_call_struc
	lenter
	push es
	_386_PM_o32
	mov word [bp + ?86m_call_struc +00h], di	; edi
	_386_PM_o32
	mov word [bp + ?86m_call_struc +04h], si	; esi
	_386_PM_o32
	mov word [bp + ?86m_call_struc +10h], bx	; ebx
	_386_PM_o32
	mov word [bp + ?86m_call_struc +14h], dx	; edx
	_386_PM_o32
	mov word [bp + ?86m_call_struc +18h], cx	; ecx
	_386_PM_o32
	mov word [bp + ?86m_call_struc +1Ch], ax	; eax
	mov ax, word [bp + ?frame_bp]
	mov word [bp + ?86m_call_struc +08h], ax	; bp
	xor ax, ax
	mov word [bp + ?86m_call_struc +20h], ax	; flags
		; Note:	As noted in the RBIL61 Int31.0301 description,
		;	 the DPMI host will provide a stack if the
		;	 passed ss:sp fields are zero. However, this
		;	 stack is documented to be small, 30 words.
		;	XMS specifies that a stack of 128 words should
		;	 be available to it when calling its entrypoint.
		;
		; Here we re-use our (PM) stack with a heuristically
		;  chosen displacement in the hopes that the host will
		;  not use more than that of the stack itself.
XMS_VIA_86M_CALLBACK_STACK_DISPLACEMENT equ 128
	mov ax, sp
	sub ax, XMS_VIA_86M_CALLBACK_STACK_DISPLACEMENT
	mov word [bp + ?86m_call_struc +2Eh], ax	; sp
	mov ax, 256 + XMS_VIA_86M_CALLBACK_STACK_DISPLACEMENT
	nearcall stack_check.internal	; enough for XMS ?
	jb .stackoverflow

	mov ax, word [pspdbg]
	mov word [bp + ?86m_call_struc +30h], ax	; ss
	mov word [bp + ?86m_call_struc +22h], ax	; es
	mov word [bp + ?86m_call_struc +24h], ax	; ds
		; (ds is used to point to zz_xms.movestruc)
	push word [zz_xms.entry + 2]
	push word [zz_xms.entry]
	pop word [bp + ?86m_call_struc +2Ah]		; ip
	pop word [bp + ?86m_call_struc +2Ch]		; cs
	push ss
	pop es				; => stack
	lea di, [bp + ?86m_call_struc]	; -> 86-Mode call structure
_386	movzx edi, di			; (previously checked b[dpmi32] here)
	xor cx, cx			; copy that many words to 86M stack
	mov ax, 0301h			; far call 86M procedure
	xor bx, bx
	int 31h
	mov ah, byte [bp + ?86m_call_struc +20h]	; flags
	sahf
	mov ax, word [bp + ?86m_call_struc +08h]	; bp
	mov word [bp + ?frame_bp], ax
	_386_PM_o32
	mov di, word [bp + ?86m_call_struc +00h]	; edi
	_386_PM_o32
	mov si, word [bp + ?86m_call_struc +04h]	; esi
	_386_PM_o32
	mov bx, word [bp + ?86m_call_struc +10h]	; ebx
	_386_PM_o32
	mov dx, word [bp + ?86m_call_struc +14h]	; edx
	_386_PM_o32
	mov cx, word [bp + ?86m_call_struc +18h]	; ecx
	_386_PM_o32
	mov ax, word [bp + ?86m_call_struc +1Ch]	; eax
@@:
	pop es
	lleave
	retn

.stackoverflow:
	call .seterror
	jmp @B


.pm:
	_386_PM_o32
.rm:
%endif
	call far [zz_xms.entry]
	retn

fail_xms_access:
	mov dx, msg.zz_fail_xms_access
	mov ax, 050Bh
	nearcall setrc
	jmp exit_error_msg_code

call_xms_move: section_of_function
	push ax
	push bx
	push si
	mov si, zz_xms.movestruc
	mov ah, 0Bh
	nearcall zz_call_xms
	dec ax
	jnz fail_xms_access
	pop si
	pop bx
	pop ax
	retn
%endif


%if _PM
setes2ax: section_of_function
	mov bx, ax
setes2bx:
	nearcall ispm
	jnz @F
	mov dx, bx
	nearcall setrmsegm
@@:
	mov es, bx
	retn
%endif	; _PM


 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
	usesection lDEBUG_CODE2
 %endif

		; INP:	bx:cx = length in bytes
		;	word [sym_storage.start + 2] => source (or 0)
		;	ax => destination
		; CHG:	di, si, cx
		; STT:	ds = es = ss
zz_transfer_buffer: section_of_function
	push cx
	mov di, ax
	mov cx, word [sym_storage.start + 2]
	mov si, sym_storage.first_start
@@:
	lodsw
	lodsw
	sub ax, cx			; x.start - sym_storage.start = para index
	add ax, di			; para index + destination = new seg
	mov word [si - 2], ax
	cmp si, sym_storage.after_start
	jb @B
	mov ax, di
	pop cx

	push word [sym_storage.start + 2]
	push word [sym_storage.start]
	pop si
%if _PM
	 push word [symsel2]
	dualcall segment_to_selector
%endif
	pop ds			; ds:si -> transfer source

	push ax
	xor di, di
%if _PM
	 push word [ss:symsel1]
	dualcall segment_to_selector
%endif
	pop es			; es:di -> transfer destination

.transfer_loop:
	 push es
%if _PM
	dualcall selector_to_segment
%endif
	 push di
	dualcall normalise_pointer
	 pop di
%if _PM
	 push word [ss:symsel1]
	dualcall segment_to_selector
%endif
	 pop es			; normalise pointer
	 push ds
%if _PM
	dualcall selector_to_segment
%endif
	 push si
	dualcall normalise_pointer
	 pop si
%if _PM
	 push word [ss:symsel2]
	dualcall segment_to_selector
%endif
	 pop ds			; normalise pointer
	sub cx, SEGTRANSFER_size
	sbb bx, 0
	jc .transfer_last

	push cx
	mov cx, SEGTRANSFER_size >> 1
	rep movsw
	pop cx
	jmp .transfer_loop

.transfer_last:
	add cx, SEGTRANSFER_size
	adc bx, 0
	shr cx, 1
	rep movsw

	 push ss
	 pop ds
	 push ss
	 pop es
	retn


zz_get_literal: section_of_function
	mov ah, 0
	xor bx, bx		; (in case of decimal base shortcut:
	mov dx, 10		;   set base: decimal)
	cmp al, '#'		; shortcut change to decimal base?
	je .lit_base		; yes -->

.lithex_common:
	call .lit_ishexdigit?	; the first character must be a digit then
	jc .err2
	xor dl, dl		; initialize value
.lithex_loopdigit:
	cmp al, '_'
	je .lithex_skip
	call .lit_ishexdigit?	; was last character ?
	jc .lit_end		; yes -->
	test bh, 0F0h		; would shift bits out ?
	jnz .err2
	nearcall uppercase
	sub al, '0'
	cmp al, 9		; was decimal digit ?
	jbe .lithex_decimaldigit; yes -->
	sub al, 'A'-('9'+1)	; else adjust for hexadecimal digit
.lithex_decimaldigit:
	mov cx, 4
.lithex_loopshift:
	shl dx, 1
	rcl bx, 1
	loop .lithex_loopshift	; *16
	or dl, al		; add in the new digit
.lithex_skip:
	lodsb
	jmp short .lithex_loopdigit

.lit_end:
	cmp al, '#'		; base change specification?
	je .lit_base		; yes -->
	nearcall isseparator?	; after the number, there must be a separator
	jne .err2		; none here -->
	jmp .term_end		; okay -->
.lit_base:
	test bx, bx		; insure base <= 36
	jnz .err2
	cmp dx, byte 36
	ja .err2
	cmp dx, byte 2		;  and >= 2
	jb .err2		; otherwise error -->

	lodsb
	mov ah, 0		; (not sure why this)
	cmp dl, 16		; hexadecimal ?
	je .lithex_common	; yes, use specific handling -->

	mov di, dx		; di = base
	mov cl, dl
	add cl, '0'-1
	cmp cl, '9'
	jbe .lit_basebelow11
	mov cl, '9'
.lit_basebelow11:		; cl = highest decimal digit for base ('1'..'9')
	mov ch, dl
	add ch, 'A'-10-1	; ch = highest letter for base ('A'-x..'Z')

	call .lit_isdigit?	; first character must be a digit
	jc .err2
	xor dx, dx		; initialize value
.lit_loopdigit:
	cmp al, '_'
	je .lit_skip
	call .lit_isdigit?	; was last character ?
	jc .lit_end		; yes -->
	nearcall uppercase
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

.term_end:
	retn

.err2:
	mov ax, 050Ch
	nearcall setrc
	jmp symbolasm_error

.lit_ishexdigit?:
	nearcall getexpression.lit_ishexdigit?
	retn

.lit_isdigit?:
	nearcall getexpression.lit_isdigit?
	retn
