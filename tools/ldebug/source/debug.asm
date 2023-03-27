
%if 0

lDebug - libre 86-DOS debugger

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2021 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

[list -]
%if 0

lDebug build notes (OUTDATED)

This section is superceded by the manual's chapter on building.

Compile MKTABLES:	wcl -ox -3 -d__MSDOS__ mktables.c
or:	gcc -xc MKTABLES.C -o mktables -Wno-write-strings -DOMIT_VOLATILE_VOID
Execute MKTABLES:	mktables
	(This deletes debugtbl.old then creates debugtbl.inc.)
	(A temporary file named debugtbl.tmp is used.)
Assemble DEBUG.COM:	nasm debug.asm -I../macro/ -oDEBUG.COM
Assemble DEBUGX.COM:	nasm debug.asm -I../macro/ -d_PM -oDEBUGX.COM

OpenWatcom 1.9 was used for compiling MKTABLES but it should reasonably well
work with other C compilers. gcc 6.3.0 is now also able to compile MKTABLES.
Assembling lDebug requires NASM (2.12.01 tested).

Note that MKTABLES only needs to be used if either the source files changed or
the MKTABLES program itself has been altered. If the assembler and disassembler
table is not to change, NASM is sufficient to assemble lDebug.


lDebug build options

There are some more options that can be used on the NASM command line to create
alternative lDebug versions. Unless otherwise specified, each option is a flag
and so should be set to either 1 (to enable an option) or 0 (to disable it).
Omitting a numeric value as well as the equals sign enables the option too.

-d_FILENAME=	Set base string for file name. Up to six characters.
-d_PROGNAME=	Set program name string displayed in help screens.
-d_VERSION=	Set version string displayed after _PROGNAME. Include a blank.
-d_REVISIONID=	Set revision ID string. (Disabled if empty or undefined.)
-d_PM=		Set DPMI support.
-d_DEBUG=	Set support for debugging lDebug, for lDebug development. The
		interrupt handlers will be reset to these of the next debugger
		in lDebug code and the BU command breaks to the next debugger.
-d_EMS=		Set EMS allocation commands (XA, XD, XM, XR, XS, X?).
-d_INT=		Set command (DI) to display exception and interrupt handlers.
-d_MCB=		Set command (DM) to display DOS Memory Control Blocks.
-d_RN=		Set command (RN) to display numerical co-processor registers.
-d_DSTRINGS=	Set commands (DZ, D$, D#, DW#) to display strings in memory.
-d_SDUMP=	Set that the search command (S) displays memory after matches.
-d_COND=	Set that the register dump (R, T, P, G) displays jump notices.
-d_USESDA=	Set that switching processes uses the DOS Swappable Data Area.
-d_VDD=		Set NTVDM direct disk access support via loading DEBXXVDD.DLL.
-d_EXPRESSIONS=	Set expression evaluator that accepts calculations.
		(Currently is required enabled.)
-d_VARIABLES=	Set 256 32-bit variables (V0..VFF) that can be set freely.
-d_OPTIONS=	Set 32-bit option variables (DCO, DCS, DIF, DAO, DAS).
-d_BOOTLDR=	Set boot loading support.
-d_BREAKPOINTS=	Set permanent breakpoint support (B commands).
-d_NUM_B_BP=	Set number of permanent breakpoints. Must be numerical.
-d_NUM_G_BP=	Set number of temporary breakpoints. Must be numerical.
-d_MMXSUPP=	Set command (RM) to display MMX registers.
-d_CATCHINT06=	Set that Interrupt 06h (Invalid opcode) is hooked.
-d_CATCHINT08=	Set that Interrupt 08h (IRQ0, timer) is hooked. (Special!)
-d_CATCHINT18=	Set that Interrupt 18h (Diskless boot hook) is hooked.
-d_CATCHINT19=	Set that Interrupt 19h (Boot load) is hooked.
-d_STACKSIZE=	Set size of lDebug's stack (in byte). Must be numerical.
-d_AUXBUFFSIZE=	Set size of the auxiliary buffer (in byte). Must be numerical.
	(The following only apply if DPMI support is enabled.)
-d_NOEXTENDER=	Set support of DPMI hosts without DOS extender.
-d_EXCCSIP=	Set to display where exceptions inside lDebug occured.
-d_CATCHEXC06=	Set that exception 06h (Invalid opcode) is hooked.
-d_CATCHEXC0C=	Set that exception 0Ch (Stack fault) is hooked.
-d_DISPHOOK=	Set to display when the DPMI entry is hooked.

Refer to the list in debug.mac for the default values of these options. You
can also adjust the defaults there. Refer to the source code and commentary
for what each option does if these descriptions are not specific enough.


lDebug contributions

lDebug is based on DEBUG/X 1.13 to 1.18 as released by Japheth, whose work
on DEBUG/X has been released as Public Domain. (Some changes up to version
1.27 were picked up from DEBUG/X since.)

%endif

%include "debug.mac"
[list +]

%ifndef _MAP
%elifempty _MAP
%else	; defined non-empty, str or non-str
	[map all _MAP]
%endif

%if _ONLY386
	cpu 386
%else
	cpu 8086
%endif
	org 100h
	addsection lDEBUG_DATA_ENTRY, align=16 start=100h
data_entry_start:
%define DATASECTIONFIXUP -data_entry_start+100h

	addsection ASMTABLE1, align=16 follows=lDEBUG_DATA_ENTRY
	addsection ASMTABLE2, align=16 follows=ASMTABLE1
	addsection lDEBUG_CODE, align=16 follows=ASMTABLE2 vstart=0
code_start:
%define CODESECTIONFIXUP -code_start+0
_CURRENT_SECTION %+ _start:
%xdefine %[_CURRENT_SECTION %+ FIXUP] - _CURRENT_SECTION %+ _start+0
	addsection lDEBUG_CODE2, align=16 follows=lDEBUG_CODE vstart=0
code2_start:
%define CODE2SECTIONFIXUP -code2_start+0
_CURRENT_SECTION %+ _start:
%xdefine %[_CURRENT_SECTION %+ FIXUP] - _CURRENT_SECTION %+ _start+0

	addsection DATASTACK, align=16 follows=ASMTABLE2 nobits
	addsection INIT, align=16 follows=lDEBUG_CODE2 vstart=0
%if _DEVICE
	addsection DEVICESHIM, align=16 follows=INIT vstart=0
%endif
	addsection RELOCATEDZERO, vstart=0 nobits
relocatedzero:


	usesection lDEBUG_CODE
section_of cmd3.have_cx_convenience
section_of verifysegm, verifysegm_or_error
section_of pp16, resetmode, resetmode_and_test_d_b_bit
section_of pp_fix32bitflags, proceedbreakpoint
section_of putsz, hexword, skipcomma, cmd3
section_of traceone, unexpectedinterrupt, dumpregs_no_disasm
section_of aa_imm_entry, ia_restore_a_addr, ia_restore_u_addr, disasm
section_of ia_restore_ds
%ifn _SYMBOLIC
section_of selector_to_segment
%endif
section_of disp_al, disp_al_hex, disp_al_nybble_hex, disp_ax_dec, disp_ax_hex
section_of disp_dxax_times_cx_width_bx_size
section_of disp_message, disp_message_length_cx
section_of ispm
section_of setrc, error
section_of _doscall, _doscall_return_es, _doscall_return_es_parameter_es_ds

section_of InDos, bootgetmemorysize
section_of chkeol, decword, decdword, getaddrX, getdword, getexpression
section_of getexpression.lit_isdigit?, getexpression.lit_ishexdigit?
section_of getlinearaddr, getword
section_of getlinear_d_b
section_of getlinear_high_limit
section_of getlinear_32bit
section_of getlinear_high_limit.do_not_use_test
section_of getlinear_common
section_of ifsep, iseol?, isseparator?, isstring?, movp
section_of putc, putsline, putsline_crlf, putsz_error
section_of setrmsegm
section_of skipwh0, skipwhite, stack_check.internal
section_of test_d_b_bit, test_high_limit, push_cxdx_or_edx
section_of uppercase
section_of setes2ax
section_of zz_copy_strings_to_str_buffer
section_of call_xms_move, zz_call_xms, zz_detect_xms

 %if _SYMBOLIC && _DUALCODE && _SYMBOLASMDUALCODE
	usesection lDEBUG_CODE2
 %else
	usesection lDEBUG_CODE
 %endif
section_of bxcx_to_cx_paragraphs
section_of zz_list_symbol.first, zz_list_symbol.subsequent
section_of zz_xms_to_86mm, zz_xms_try_free, zz_xms_try_free_handle
section_of zz_86mm_to_xms, zz_del_match.add_poison_entrypoint
section_of zz_free_nonxms, zz_free_xms, zz_free_reset, zz_free_dos
section_of zz_get_literal, zz_restore_strat, zz_save_strat
section_of zz_switch_s, zz_transfer_buffer


%if _SYMBOLIC
 %if _DUALCODE && _SYMBOLICDUALCODE
	usesection lDEBUG_CODE2
 %else
	usesection lDEBUG_CODE
 %endif
section_of shift_left_4_bxcx
section_of selector_to_segment

section_of anti_normalise_pointer_with_displacement_bxcx, displaystring
section_of getfarpointer.hash, getfarpointer.main, getfarpointer.str
section_of move_delete_farpointer.hash, move_delete_farpointer.hash.sidi
section_of move_delete_farpointer.main, move_delete_farpointer.str.sidi
section_of move_insert_farpointer.hash, move_insert_farpointer.hash.sidi
section_of move_insert_farpointer.main, move_insert_farpointer.main.sidi
section_of move_insert_farpointer.str.sidi
section_of normalise_pointer, normalise_pointer_with_displacement_bxcx
section_of pointer_to_linear
section_of save_slice_farpointer.hash, save_slice_farpointer.main
section_of save_slice_farpointer.str
section_of segment_to_selector
section_of zz_insert, zz_match_symbol, zz_match_symbol.continue, zz_relocate

section_of binsearchhash, binsearchmain
section_of check_second_slice
section_of disp_size_hash, disp_size_main, disp_size_str
section_of displayresult, getstring1, getstring2
section_of increment_ss_ref_count
section_of list_sym_storage_usage, str_index_to_pointer
section_of zz_commit_insert, zz_compact, zz_compact_expand_check_nonxms
section_of zz_compact_expand_check_xms
section_of zz_delete_hash, zz_delete_main, zz_delete_main_and_its_hash
section_of zz_delete_string, zz_expand, zz_expand_common
section_of zz_get_symstr_length_bytes
section_of zz_get_symstr_length_bytes.ssLength_in_cx
section_of zz_get_symstr_length_indices
section_of zz_get_symstr_length_indices.ssLength_in_cx
section_of zz_hash, zz_hash.bx_init
section_of zz_insert_main.linklist, zz_reloc_main, zz_reloc_str
section_of zz_store_pre_str, zz_store_string, zz_store_string.hash_bx
section_of zz_unlink_main_next
%endif

%if _DUALCODE
	usesection lDEBUG_CODE2
%else
	usesection lDEBUG_CODE
%endif
section_of FloatToStr


	usesection lDEBUG_DATA_ENTRY

%define CODESECTIONOFFSET (100h+ldebug_data_entry_size+asmtable1_size+asmtable2_size)
%define INITSECTIONOFFSET (CODESECTIONOFFSET+ldebug_code_size+ldebug_code2_size)

%define CODETARGET1 (CODESECTIONOFFSET+datastack_size)
%define CODETARGET2 (CODETARGET1+auxbuff_size)

%define AUXTARGET1 (CODETARGET1+ldebug_code_size+ldebug_code2_size)
%define AUXTARGET2 CODETARGET1
%define AUXTARGET3 AUXTARGET1+auxbuff_size

%define NONBOOTINITTARGET (INITSECTIONOFFSET \
			+ datastack_size \
			+ auxbuff_size * 2 \
			+ historysegment_size)
%define NONBOOTINITSTACK_START (NONBOOTINITTARGET+init_size)
NONBOOTINITSTACK_SIZE equ 512	; must be even
%define NONBOOTINITSTACK_END (NONBOOTINITSTACK_START+NONBOOTINITSTACK_SIZE)

BOOTINITSTACK_SIZE equ 512	; must be divisible by 16
%define BOOTDELTA	(fromkib(kib(auxbuff_size * 2 \
				+ historysegment_size \
				+ datastack_size \
				+ INITSECTIONOFFSET + 16)))

%if _DEVICE
%define DEVICEADJUST	(deviceshim_size + 110h)
%define DEVICEINITTARGET (INITSECTIONOFFSET \
			+ DEVICEADJUST \
			+ datastack_size \
			+ auxbuff_size * 2 \
			+ historysegment_size \
			+ 10h)
%define DEVICEINITSIZE (DEVICEINITTARGET - 100h \
			+ init_size \
			+ deviceshim_size)
%define DATAENTRYTABLESIZE (ldebug_data_entry_size \
			+ asmtable1_size \
			+ asmtable2_size)


		; Note: Once this is implemented, the final
		;  copy of this device header should live
		;  in front of our PSP. Therefore, this space
		;  after the PSP can be re-used for the newly
		;  expanded N buffer. (Refer to N_BUFFER_END.)

		; The device header is of a fixed format.
		;  For our purposes, the 4-byte code for
		;  each the strategy entry and the
		;  interrupt entry is part of this format.
		; (DOS may read the attributes or entrypoint
		;  offsets before calling either, so the
		;  inicomp stage needs to recreate in its
		;  entrypoints part exactly what we have here.)
%macro writedeviceheader 3
	usesection %1
%2:
.next:
%ifidni %1, DEVICESHIM
	dd -1
%else
	fill 2, -1, jmp strict short j_zero_entrypoint
	dw -1
%endif
.attributes:
	dw 8000h			; character device
.strategy:
	dw .strategy_entry %3		; -> strategy entry
.interrupt:
	dw .interrupt_entry %3		; -> interrupt entry
.name:
	fill 8, 32, db "LDEBUG$$"	; character device name
.strategy_entry:
	fill 4, 90h, jmp %2 %+ .device_entrypoint
.interrupt_entry:
	fill 4, 90h, retf
%endmacro

writedeviceheader lDEBUG_DATA_ENTRY, device_header, - 100h
%else
	jmp initcode_j
%endif

		; Startup codes can be discarded after one of
		;  them is used to enter the initialisation part.
		;  Therefore the N buffer is now extended past
		;  these codes, refer to N_BUFFER_END.
%if _BOOTLDR
	align 32, db 0
 %if ($ - $$) != 32
  %error Wrong kernel iniload entrypoint
 %endif
	mov bx, boot_initcode
%endif

%if _BOOTLDR || _DEVICE
device_boot_common_entrypoint:
	mov ax, cs
	sub ax, 10h
	mov ds, ax
	jmp @F
%endif
	align 64, db 0
 %if ($ - $$) != 64
  %error Wrong application entrypoint
 %endif
j_zero_entrypoint:
initcode_j:
	mov ax, cs
	xor bx, bx
@@:
	add ax, paras(INITSECTIONOFFSET)
	push ax
	push bx
	retf


%if _DEVICE
		; INP:	es:bx -> device request header
		;	ss:sp -> a DOS stack, far return address to DOS
		;	cs:0 -> our start image
		; OUT:	bx = offset of init function in INIT segment
		;	ss:sp -> bx, fl, ds, ax, far return address
device_header.device_entrypoint:
	cmp byte [es:bx + 2], 0		; command code 0 (init) ?
	je @F

	mov word [es:bx + 3], 8103h	; error, done, code: unknown command
	retf

@@:
	push ax
	push ds
	pushf
	push bx
	mov bx, device_initcode
	jmp device_boot_common_entrypoint


writedeviceheader DEVICESHIM, shim_device_header, - 0
shim_device_header.device_entrypoint:
	mov word [es:bx + 3], 8103h	; error, done, code: unknown command
	retf

	align 16
deviceshim_size	equ $ - section.DEVICESHIM.vstart
	endarea deviceshim, 1


	usesection lDEBUG_DATA_ENTRY
%endif


	align 2, db 0
N_BUFFER_END equ $		; end of N buffer (starts in PSP at 80h)

cmdlist:	dw aa,bb,cc,ddd,ee,ff,gg,hh,ii,error,error,ll,mm,nn,oo
		dw pp,qq,rr,sss,tt,uu,vv,ww,xx,yy
%if _SYMBOLIC
		dw zz
%endif

	align 4, db 0
				; options, startoptions and internalflags
				; have to be consecutive
options:	dd DEFAULTOPTIONS ; run-time options
dispregs32	equ	  1	; RX: 32-bit register display (R, T/P/G)
traceints	equ	  2	; TM: trace into interrupts (T)
cpdepchars	equ	  4	; allow dumping of CP-dependant characters (D, DX)
fakeindos	equ	  8	; always assume InDOS flag non-zero (all)
nonpagingdevice	equ	 10h	; disallow paged output with [more] prompt (all exc. P, T)
pagingdevice	equ	 20h	; allow paged output with [more] prompt (all exc. P, T)
				; paged output is by default on if the output device is StdOut, else off
hexrn		equ	 40h	; display raw hexadecimal content of FPU registers (RN)
;novdd		equ	 80h	; don't use a registered NTVDM VDD (L, W)
nondospaging	equ	100h	; paging: don't use DOS for input when waiting for a key
nohlt		equ	200h	; HLT doesn't work, don't use it
biosidles	equ	400h	; don't idle with HLT or Int2F.1680, only call BIOS Int16.00
opt_usegetinput	equ	800h	; use getinput for int 21h interactive input
use_si_units	equ    1000h	; in disp_*_size use SI units (kB = 1000, etc)
use_jedec_units	equ    2000h	; in disp_*_size use JEDEC units (kB = 1024)
enable_serial	equ    4000h	; enable serial I/O (preferred over DOS or BIOS terminal)
int8_disable_serial equ	   8000h	; disable serial I/O when breaking due to intr8
gg_do_not_skip_bp equ	 1_0000h	; gg: do not skip a breakpoint (bb or gg)
gg_no_autorepeat equ	 2_0000h	; gg: do not auto-repeat
tp_do_not_skip_bp equ	 4_0000h	; T/TP/P: do not skip a (bb) breakpoint
gg_bb_hit_no_repeat equ	 8_0000h	; gg: do not auto-repeat after bb hit
tp_bb_hit_no_repeat equ	10_0000h	; T/TP/P: do not auto-repeat after bb hit
gg_unexpected_no_repeat equ 20_0000h	; gg: do not auto-repeat after unexpectedinterrupt
tp_unexpected_no_repeat equ 40_0000h	; T/TP/P
ss_no_dump:		equ 80_0000h
%if _SYMBOLIC
dd_no_blanks_sym:	equ 100_0000h
zz_no_pm_xms:		equ 200_0000h
%endif
rr_disasm_no_rept:	equ 1000_0000h
rr_disasm_no_show:	equ 2000_0000h
opt_cmdline_quiet_input:equ 4000_0000h
opt_cmdline_quiet_output:equ 8000_0000h
%if _SYMBOLIC
DEFAULTOPTIONS	equ zz_no_pm_xms
%else
DEFAULTOPTIONS	equ 0
%endif

options2:	dd DEFAULTOPTIONS2
opt2_db_header:		equ 1
opt2_db_trailer:	equ 2
opt2_dw_header:		equ 10h
opt2_dw_trailer:	equ 20h
opt2_dd_header:		equ 100h
opt2_dd_trailer:	equ 200h
opt2_getinput_dpmi:	equ 800h
opt2_hh_compat:		equ 1000h
opt2_getc_idle:		equ 2000h
opt2_getc_idle_dpmi:	equ 4000h
opt2_re_cancel_tpg:	equ 8000h
DEFAULTOPTIONS2	equ opt2_dw_header | opt2_dd_header | opt2_getc_idle_dpmi

options3:	dd DEFAULTOPTIONS3
opt3_tt_no_paging:	equ 1
opt3_tp_no_paging:	equ 2
opt3_pp_no_paging:	equ 4
opt3_gg_no_paging:	equ 8
opt3_silence_paging_set:equ 100h
opt3_silence_paging_on:	equ 200h
opt3_r_highlight_diff:	equ 10000h
opt3_r_highlight_dumb:	equ 20000h
opt3_r_highlight_full:	equ 40000h
opt3_r_highlight_eip:	equ 80000h
%if _PM
opt3_ss_b_bit_set:	equ   10_0000h
 %if _BREAK_INSTALLDPMI
opt3_break_installdpmi:	equ   20_0000h
 %endif
%endif
%if _GETLINEHIGHLIGHT
opt3_getline_highlight:	equ  100_0000h
%endif
opt3_no_idle_2F:	equ  200_0000h
%if _DELAY_BEFORE_BP
opt3_delay_before_bp:	equ  400_0000h
%endif
opt3_no_call_update:	equ  800_0000h
opt3_disable_autorepeat:equ 1000_0000h
opt3_check_ctrlc_keyb:	equ 2000_0000h
opt3_check_ctrlc_0bh:	equ 4000_0000h
opt3_tsr_quit_leave_tf:	equ 8000_0000h
DEFAULTOPTIONS3	equ opt3_tt_no_paging | opt3_tp_no_paging \
		| opt3_pp_no_paging | opt3_gg_no_paging \
		| opt3_check_ctrlc_keyb | opt3_check_ctrlc_0bh

options4:	dd DEFAULTOPTIONS4
opt4_int_2F_hook:	equ 2
opt4_int_08_hook:	equ 4
opt4_int_2D_hook:	equ 8
opt4_int_serial_force:	equ 1_0000h
opt4_int_2F_force:	equ 2_0000h
opt4_int_08_force:	equ 4_0000h
opt4_int_2D_force:	equ 8_0000h
opt4_int_00_force:	equ  100_0000h
opt4_int_01_force:	equ  200_0000h
opt4_int_03_force:	equ  400_0000h
opt4_int_06_force:	equ  800_0000h
opt4_int_18_force:	equ 1000_0000h
opt4_int_19_force:	equ 2000_0000h
opt4_int_09_force:	equ 4000_0000h	; not used at the same time as 15h
opt4_int_15_force:	equ 4000_0000h	; not used at the same time as 09h
opt4_int_07_force:	equ 8000_0000h
opt4_int_0C_force:	equ 8000_0000h
opt4_int_0D_force:	equ 8000_0000h	; this one is triply-used
DEFAULTOPTIONS4 equ opt4_int_2F_hook

options5:	dd 0
options6:	dd DEFAULTOPTIONS6
%if _VXCHG
opt6_vv_mode:		equ 1
opt6_vv_keep:		equ 2
opt6_vv_int16:		equ 10h
 %if _VXCHG_DEFAULT_ON
DEFAULTOPTIONS6 equ opt6_share_serial_irq | opt6_vv_mode
 %else
DEFAULTOPTIONS6 equ opt6_share_serial_irq
 %endif
%else
DEFAULTOPTIONS6 equ opt6_share_serial_irq
%endif
%if _DEBUG
opt6_debug_exception_late:	equ  20h
opt6_debug_exception_early:	equ  40h
 %if _DEBUG_COND
opt6_debug_exception:	equ  80h
opt6_debug_mode:	equ 100h
 %endif
%endif
opt6_bios_output:	equ 200h
opt6_flat_binary:	equ 400h
opt6_big_stack:		equ 800h
opt6_40_columns:	equ 1000h
opt6_40_indent_odd:	equ 2000h
opt6_40_dash:		equ 4000h
opt6_share_serial_irq:	equ 1_0000h
opt6_serial_EOI_call:	equ 2_0000h
%if _DEBUG
opt6_debug_putrunint_early:	equ  40_0000h
 %if _DEBUG_COND
opt6_debug_putrunint:		equ  80_0000h
 %endif
%endif
opt6_bios_io:		equ 100_0000h
opt6_immasm_display_uu:	equ 200_0000h
opt6_immasm_debug:	equ 400_0000h
opt6_immasm_flag:	equ 800_0000h
opt6_immasm_nobranch:	equ 1000_0000h

	; options, options2, options3, options4, options5, options6
	;  are each assumed to be dwords
	;  and all consecutive in expr.asm isvariable?

startoptions:	dd DEFAULTOPTIONS ; options as determined during startup; read-only for user
startoptions2:	dd DEFAULTOPTIONS2
startoptions3:	dd DEFAULTOPTIONS3
startoptions4:	dd DEFAULTOPTIONS4
startoptions5:	dd 0
startoptions6:	dd DEFAULTOPTIONS6
	; startoptions, startoptions2, startoptions3, startoptions4,
	;  startoptions5, startoptions6
	;  are each assumed
	;  to be dwords and all consecutive in expr.asm isvariable?

internalflags:	dd attachedterm|pagedcommand|notstdinput|inputfile|notstdoutput|outputfile|(!!_PM*dpminohlt)|debuggeeA20|debuggerA20
				; flags only modified by DEBUG itself
oldpacket	equ	  1	; Int25/Int26 packet method available (L, W)
newpacket	equ	  2	; Int21.7305 packet method available (L, W)
ntpacket	equ	  4	; VDD registered and usable (L, W)
pagedcommand	equ	  8	; allows paging in puts
notstdinput	equ	 10h	; DEBUG's StdIn isn't a device with StdIn bit (is file or other device)
inputfile	equ	 20h	; DEBUG's StdIn is a file, notstdinput also set
notstdoutput	equ	 40h	; DEBUG's StdOut isn't a device with StdOut bit (is file or other device)
outputfile	equ	  80h	; DEBUG's StdOut is a file, notstdoutput also set
hooked2F	equ	 100h	; Int2F hooked
nohook2F	equ	 200h	; don't hook Int2F.1687 (required for Win9x, DosEmu?)
dpminohlt	equ	 400h	; DPMI doesn't like hlt
protectedmode	equ	 800h	; in (DPMI) protected mode
debuggeeA20	equ	1000h	; state of debuggee's A20
debuggerA20	equ	2000h	; state of debugger's A20 (will be on if possible)
nodosloaded	equ	4000h	; No DOS loaded currently (Boot loader mode)
has386		equ	8000h	; CPU is a 386
usecharcounter	equ    1_0000h	; don't reset charcounter between calls to puts
runningnt	equ    2_0000h	; running in NTVDM
canswitchmode	equ    4_0000h	; can switch modes (auxbuff large enough, DPMI mode switch set up)
modeswitched	equ    8_0000h	; switched mode (now in the mode that we weren't entered in)
promptwaiting	equ   10_0000h	; puts: any more output needs to display a prompt first
switchbuffer	equ   20_0000h	; mode switch needs a buffer (auxbuff)
tsrmode		equ   40_0000h	; in TSR mode; DPI and DPP not valid
attachedterm	equ   80_0000h	; the attached process terminated
runningdosemu	equ  100_0000h	; running in dosemu
; load_is_ldp	equ  200_0000h	; boot load: partition specified as "ldp"
tt_while:	equ  400_0000h	; tt: while condition specified
tt_p:		equ  800_0000h	; tt: proceed past repeated string instructions
tt_silent_mode:	equ 1000_0000h	; tt: run should be silent (dump at end)
tt_silence:	equ 2000_0000h	; tt: silent writing (write to auxbuff instead)
tt_no_bb:	equ 4000_0000h	; tt: do not use bb breakpoints
tt_no_bb_first:	equ 8000_0000h	; tt: do not use bb breakpoints at first

internalflags2:
%if _SYMBOLIC
		dd dif2_sym_req_xms | dif2_sym_req_86mm
%else
		dd 0
%endif
dif2_gg_is_first:	equ   1
dif2_gg_skip_non_cseip:	equ   2
dif2_gg_skip_cseip:	equ   4
dif2_gg_is_gg:		equ   8
dif2_gg_first_detected:	equ  10h
dif2_gg_again:		equ  20h
dif2_tpg_proceed_bp_set:equ  40h
dif2_tpg_keep_proceed_bp: equ 80h
dif2_tpg_have_bp:	equ 100h
dif2_tpg_adjusted_cseip:equ 200h
dif2_tpg_do_not_adjust:	equ 400h
dif2_bp_failure:	equ 800h
dif2_is_pp:		equ 1000h
%if _SYMBOLIC
dif2_createdprocess:	equ 800_0000h	; created empty debuggee process
dif2_sym_req_xms:	equ 2000h
dif2_sym_req_86mm:	equ 4000h
dif2_sym_req_mask equ dif2_sym_req_86mm | dif2_sym_req_xms
dif2_no_pm_xms:		equ 1_0000h
dif2_xms_detection_done:equ 2_0000h
%endif
%if _INPUT_FILE_HANDLES
dif2_input_file:	equ 10_0000h
dif2_closed_input_file:	equ 20_0000h
%endif
dif2_did_getline_file:	equ 40_0000h
%if _SYMBOLIC
dif2_poison:		equ 80_0000h
%endif
dif2_boot_loaded_kernel:equ 100_0000h
%if _INPUT_FILE_BOOT
dif2_input_file_boot:	equ 200_0000h
dif2_closed_input_file_boot:	equ 400_0000h
%endif
dif2_in_silence_dump:	equ 1000_0000h
dif2_int31_segment:	equ 2000_0000h

internalflags3:	dd dif3_partition_changed | dif3_at_line_end
dif3_load_is_ldp:	equ 1	; boot load: partition specified as "ldp"
dif3_load_is_sdp:	equ 2	; boot load: partition specified as "sdp"
dif3_load_is_ydp:	equ 4	; boot load: partition specified as "ydp"
dif3_load_is_dp:	equ dif3_load_is_ldp \
				| dif3_load_is_sdp \
				| dif3_load_is_ydp
dif3_load_dir_dir:	equ 8
dif3_input_terminal_override:	equ 10h
dif3_input_serial_override:	equ 20h
dif3_if_not:		equ 40h
dif3_partition_changed:	equ 80h
;
dif3_input_cmdline:	equ 100h; input reading from cmdline_buffer
dif3_input_cmdline_closed: equ 200h
dif3_at_line_end:	equ 400h
dif3_quiet_input_single:equ 800h
dif3_quiet_output:	equ 1000h
dif3_unquiet_error:	equ 2000h
dif3_unquiet_prompt:	equ 4000h
dif3_unquiet:		equ dif3_unquiet_error | dif3_unquiet_prompt
dif3_return_eof:	equ 8000h
dif3_highlighting:	equ 1_0000h
dif3_do_not_highlight:	equ 2_0000h
dif3_int10_highlight:	equ 2_0000h
dif3_nosymbols_1:	equ 4_0000h
dif3_nosymbols_2:	equ 8_0000h
%if _PM
dif3_ss_b_bit_set:	equ  10_0000h
%endif
dif3_gotint19:		equ  20_0000h
%if _DELAY_BEFORE_BP
dif3_delayed		equ  80_0000h
%endif
dif3_input_re:		equ 100_0000h	; input reading from re_buffer
dif3_input_re_closed:	equ 200_0000h
dif3_accept_getrange_0:	equ 400_0000h
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
dif3_in_if:		equ 800_0000h
%endif
dif3_auxbuff_guarded_1:	equ 1000_0000h
dif3_auxbuff_guarded_2:	equ 2000_0000h
dif3_prior_pm:		equ 4000_0000h
dif3_sss_is_reverse:	equ 8000_0000h

internalflags4:	dd 0
dif4_int_serial_hooked:	equ 1
dif4_int_2F_hooked:	equ 2
dif4_int_08_hooked:	equ 4
dif4_int_2D_hooked:	equ 8

internalflags5:	dd 0
internalflags6:	dd 0
%if _VXCHG
dif6_vv_mode:		equ 1
%endif
dif6_in_hook2F:		equ 2
dif6_in_amis_hook2F:	equ 4
%if _DEBUG && _DEBUG_COND
dif6_debug_mode:	equ 100h
%endif
dif6_device_mode:	equ 40_0000h
%if _IMMASM
dif6_immasm_no_output:	equ 100_0000h
dif6_immasm:		equ 200_0000h
dif6_immasm_rel8:	equ 400_0000h
%endif
	; internalflags, internalflags2, internalflags3, internalflags4,
	;  internalflags5, internalflags6
	;  are each assumed
	;  to be dwords and all consecutive in expr.asm isvariable?

asm_options:	dd DEFAULTASMOPTIONS
disasm_lowercase	equ 1
disasm_commablank	equ 2
disasm_nasm		equ 4
disasm_lowercase_refmem:equ 8
disasm_show_short:	equ 10h
disasm_show_near:	equ 20h
disasm_show_far:	equ 40h
disasm_nec:		equ 80h
disasm_40_columns:	equ 100h
disasm_no_indent:	equ 200h
disasm_a16_memref:	equ 1000h
disasm_a32_memref:	equ 2000h
disasm_a16_string:	equ 4000h
disasm_a32_string:	equ 8000h
DEFAULTASMOPTIONS equ disasm_lowercase|disasm_commablank|disasm_nasm \
	| disasm_a16_memref | disasm_a32_memref \
	| disasm_a16_string | disasm_a32_string

asm_startoptions:
		dd DEFAULTASMOPTIONS

gg_first_cseip_linear:	dd 0
gg_next_cseip_linear:	dd 0
tpg_possible_breakpoint:dd 0
gg_deferred_message:	dw msg.empty_message
bb_deferred_message_in_lineout_behind:
			dw 0
		align 4, db 0
tpg_proceed_bp:		times BPSIZE db 0
%if _DEBUG1
		align 2, db 0
test_records_Readmem:		times 6 * 16 db 0
test_records_Writemem:		times 6 * 16 db 0
test_records_getLinear:		times 6 * 16 db 0
test_records_getSegmented:	times 6 * 16 db 0

test_readmem_value:		db 0
%endif
pp_instruction:		db 0
		align 4, db 0
pp_operand:		dd 0
		align 2, db 0
code_seg:	dw 0
%if _PM
code_sel:	dw 0
%endif
%if _DUALCODE
code2_seg:	dw 0
 %if _PM
code2_sel:	dw 0
 %endif
%endif
%if _IMMASM
immseg:		dw 0
%endif

		db 13
		align 16, db 13	; insure the cmdline_buffer is prefixed by CR
cmdline_buffer:
.size:		equ _RC_BUFFER_SIZE
		times .size db 0
.end:
.position:	dw cmdline_buffer

		db 13
		align 2, db 13	; insure the re_buffer is prefixed by CR
re_buffer:
.size:		equ _RE_BUFFER_SIZE
		fill .size,0,db "@R"
.end:
.position:	dw re_buffer

		align 4, db 0
re_count:	dd 0
re_limit:	dd 256
rc_count:	dd 0
rc_limit:	dd 4096

		align 2, db 0
cmd3_set_options:		dw 0
%if _PM
auxbuff_switchbuffer_size:	dw 0
%endif
auxbuff_segorsel:segonlyaddress
auxbuff_behind_while_condition:
		dw 0		; -> behind while condition stored in auxbuff
				;  (this is also the first silent buffer entry)
auxbuff_behind_last_silent:
		dw 0		; -> behind last silent buffer entry
tt_silent_mode_number:
		dw 0		; if non-zero: maximum amount of dumps
				;  displayed after T/TP/P while silent
%if _SYMBOLIC
created_psp:	dw 0
created_size:	dw 0
%endif

%if _INPUT_FILE_HANDLES
		align INPUTFILEHANDLE_size
input_file_handles:
		times _INPUT_FILE_HANDLES * INPUTFILEHANDLE_size db -1
.active:	dw 0
.to_close:	dw 0
%endif
	align 2, db 0
indos_remember_seek_function:	dw 4201h
indos_remember_seek_handle:	dw -1
	align 4, db 0
indos_remember_seek_offset:	dd 0
%if _INPUT_FILE_BOOT
	align 4, db 0
boot_remember_seek_offset:	dd 0
	align 2, db 0
boot_remember_seek_handle:	dw -1
%endif

charcounter:	db 0		; used by raw output to handle tab
linecounter:	db 0		; used by paging in puts
	align 4, db 0
savesp:		dw stack_end	; saved stack pointer
		dw 0		; 0 to set high word of esp
re_sp:		dw 0
errret:		dw cmd3		; return here if error
throwret:	dw errhandler	; return here if error - priority, no display
throwsp:	dw stack_end	; stack pointer set before jumping to throwret
run_sp:		dw 0		; stack pointer when running
		dw 0		; (zero for esph)
run_sp_reserve:	dw 0		; additional space to reserve between the
				;  near return address of run and the run_sp
spadjust:	dw 40h 		; adjust sp by this amount for save
%if _SYMBOLIC
stack_low_address:
		dw str_buffer	; low end of stack, default = str_buffer
%endif

pspdbe:		dw 0		; debuggee's PSP (86M segment)
				;  unless DIF&attachedterm or bootloaded
pspdbg:		dw 0		; debugger's PSP (86M segment)
	align 4, db 0
run2324:	dd 0,0		; debuggee's interrupt vectors 23h and 24h (both modes)
%if _PM
		dd 0
dbg2324:	dw i23pm, i24pm
%endif
%if _VDD
hVdd:		dw -1		; NTVDM VDD handle
%endif
	align 4, db 0
sav2324:	dd 0,0		; debugger's interrupt vectors 23h and 24h (real-mode only)
hakstat:	db 0		; whether we have hacked the vectors or not
	align 4, db 0
psp22:		dd 0		; original terminate address from our PSP
parent:		dw 0		; original parent process from our PSP (must follow psp22)
%if _MCB || _INT
firstmcb:	dw -1		; start of MCB chain (always segment)
firstumcb:	dw -1
%endif
pInDOS:		segofs16address	; far16 address of InDOS flag (bimodal)
%if _USESDA
pSDA:		segofs16address minusone
				; far16 address of SDA (bimodal)
%endif
machine:	db 0		; type of processor for assembler and disassembler (1..6)
has_87:		db 0		; if there is a math coprocessor present
mach_87:	db 0		; type of coprocessor present
encodedmach87:	db 0		; C0 = no coproceasor, C = coprocessor present,
				;  C2 = 287 present on a 386
%if _MMXSUPP
has_mmx:	db 0
%endif
%if _VXCHG
 %ifn _VXCHGBIOS
		align 4, db 0
xmsdrv:	dd 0		; XMM driver address, obtained thru int 2F, ax=4310h
xmsmove:istruc XMSM	; XMS block move struct, used to save/restore screens
	iend
 %endif
csrpos:	dw 0		; cursor position of currently inactive screen
vrows:	db 0		; current rows; to see if debuggee changed video mode
%endif
%if _ALTVID		; exchange some video BIOS data fields for option /2.
oldcsrpos:	dw 0	; cursor position
oldcrtp:	dw 0	; CRTC port
oldcols:	dw 80	; columns
oldmr:	; label word
oldmode:	db 0	; video mode
oldrows:	db 24	; rows - 1
%endif
bInDbg:		db 1		; 1=debugger is running
notatty:	db 10		; if standard input is from a file
				; this is also used for a linebreak processing hack
vpage:		db 0		; video page the debugger is to use for BIOS output
%if _MCLOPT
master_pic_base:db _MCLOPT_DEFAULT
%endif
switchar:	db 0		; switch character
swch1:		db ' '		; switch character if it's a slash
	align 2, db 0
dd_default_length:
		dw 80h
dd_default_lines:
		dw 0
uu_default_length:
		dw 20h
uu_default_lines:
		dw 0
promptlen:	dw 0		; length of prompt
bufnext:	dw line_in+2	; address of next available character
bufend:		dw line_in+2	; address + 1 of last valid character
rc:		dw 0
priorrc:	dw 0
erc:		dw 0
%if _HISTORY
history:
 %if _HISTORY_SEPARATE_FIXED
.segorsel:	segonlyaddress
.first:		dw _HISTORY_SIZE - 2
.last:		dw _HISTORY_SIZE - 2
 %else
.first:		dw historybuffer.end - 2
.last:		dw historybuffer.end - 2
 %endif
%endif

var_addr_entries:
a_addr:		segmentedaddress; address for next A command
d_addr:		segmentedaddress; address for next D command; must follow a_addr
behind_r_u_addr:segmentedaddress; address behind R's disassembly
u_addr:		segmentedaddress; address for next U command; must follow d_addr
e_addr:		segmentedaddress; address for current/next E command
%if _DSTRINGS
dz_addr:	segmentedaddress; address for next ASCIZ string
dcpm_addr:	segmentedaddress; address for next $-terminated string
dcount_addr:	segmentedaddress; address for next byte-counted string
dwcount_addr:	segmentedaddress; address for next word-counted string
%endif
var_addr_entries.amount equ ($ - var_addr_entries) / SEGADR_size
%if _PM
x_addr:		dd 0		; (phys) address for next DX command
%endif
%if _DSTRINGS
dstringtype:	db 0		; FFh byte-counted, FEh word-counted, else terminator byte
	align 2, db 0
dstringaddr:	dw dz_addr	; -> address of last string
%endif
%if _INT
	align 4, db 0
intaddress:	dd 0
lastint_is_86m_and_mcb:
		dw 0
lastint:	db 0
%endif
	align 4, db 0
search_results:
%if _PM
		times 6 * 16 db 0
%else
		times 4 * 16 db 0
%endif
	align 4, db 0
sscounter:	dd 0
eqflag:		db 0		; flag indicating presence of `=' operand
	align 2, db 0
eqladdr:	dw 0,0,0	; address of `=' operand in G, P and T command
	align 2, db 0
run_int:	dw 0		; interrupt type that stopped the running
lastcmd:	dw dmycmd
fileext:	db 0		; file extension (0 if no file name)
EXT_OTHER	equ 1
EXT_COM		equ 2
EXT_EXE		equ 4
EXT_HEX		equ 8

	align 4, db 0
mmxbuff:	dd 0		; buffer with a (read-only) part of MMX register
				; for access from within expressions
%if _CATCHINT08
intr8_counter:	dw 0
intr8_limit:	dw 18 * 5	; ca 5 seconds
%endif
maxmachinetype:	db 0
serial_rows:
		db 24
serial_columns:
		db 80
serial_keep_timeout:
		db 15
%if _USE_TX_FIFO
serial_fifo_size:
		db _BI_TX_FIFO_SIZE
			; size of built-in TX fifo (1 is as if no FIFO)
%endif
serial_flags:
		db 0
sf_init_done:	equ 1
sf_ctrl_c:	equ 2
sf_double_ctrl_c:	equ 4
sf_built_in_fifo:	equ 8

serial_var_intnum:	db _INTNUM
serial_var_params:	db _UART_PARAMS
serial_var_fifo:	db _UART_FIFO
	align 2, db 0
serial_var_baseport:	dw _UART_BASE
serial_var_dl:		dw _UART_RATE
serial_var_irqmask:	dw _OFFMASK

io_rows:	db 1
io_columns:	db 1
	align 2, db 0
%if _40COLUMNS
io_columns_getline:	dw 0			; byte variable zero-extended
%endif
io_start_buffer:	dw 0
io_end_buffer:		dw 0
io_levels:		dw 255
io_flags:		dw DEFAULTIOFLAGS
iof_extra_iol_for_tpg_re:	equ 1
iof_extra_iol_for_rc:		equ 2
DEFAULTIOFLAGS equ iof_extra_iol_for_tpg_re | iof_extra_iol_for_rc

	align 2, db 0
getline_timer_count:	dw 0
getline_timer_last:	dw 0
getline_timer_func:	dw dmycmd

%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	align 2, db 0
if_exists_then_address:	dw 0
if_exists_si:		dw 0
if_exists_sp:		dw 0
if_exists_length:	dw 0
%endif

	align 2, db 0
terminator_in_line_in:
.offset:		dw 0
.value:			db 0

qqtermcode:		db 0
	align 2, db 0
device_mcb_paragraphs:	dw 0
device_header_address:	dd 0

	align 2, db 0
inttab_optional:
.:
serial_installed_intnum: equ $
	dw 0
	dw serial_interrupt_handler
	dw dif4_int_serial_hooked
%if _PM
	dw 2Fh
	dw debug2F
	dw dif4_int_2F_hooked
%endif
%if _CATCHINT08
	dw 8
	dw intr8
	dw dif4_int_08_hooked
%endif
.amount: equ ($ - .) / 6
	dw -1


amisintr_offset:
	dw inttab

inttab_pre:
	times 3 * inttab_optional.amount db 0

inttab:
%if _CATCHINT00
	db 0
	dw intr0	; table of interrupt initialization stuff
%endif
%if _CATCHINT01
	db 1
	dw intr1
%endif
%if _CATCHINT03
	db 3
	dw intr3
%endif
%if _CATCHINT07
	db 7
	dw intr7
%endif
%if _CATCHINT0C
	db 0Ch
	dw intr0C
%endif
%if _CATCHINT0D
	db 0Dh
	dw intr0D
%endif
%if _CATCHINT18
	db 18h
	dw intr18
%endif
%if _CATCHINT19
	db 19h
	dw intr19
%endif
%if _CATCHSYSREQ
	db _SYSREQINT
	dw intr_sysreq
%endif
%if _CATCHINT06
.i06:	db 6
	dw intr6
%endif
	endarea inttab
	inttab_number equ inttab_size / 3
%if _CATCHINT2D
.i2D:	db 2Dh
	dw int2D
%endif


intforcetab:
%if _CATCHINT00
	db opt4_int_00_force >> 24
%endif
%if _CATCHINT01
	db opt4_int_01_force >> 24
%endif
%if _CATCHINT03
	db opt4_int_03_force >> 24
%endif
%if _CATCHINT07
	db opt4_int_07_force >> 24
%endif
%if _CATCHINT0C
	db opt4_int_0C_force >> 24
%endif
%if _CATCHINT0D
	db opt4_int_0D_force >> 24
%endif
%if _CATCHINT18
	db opt4_int_18_force >> 24
%endif
%if _CATCHINT19
	db opt4_int_19_force >> 24
%endif
%if _CATCHSYSREQ
 %if _SYSREQINT == 09h
	db opt4_int_09_force >> 24
 %elif _SYSREQINT == 15h
	db opt4_int_15_force >> 24
 %else
  %error Unknown SysReq interrupt
 %endif
%endif
%if _CATCHINT06
	db opt4_int_06_force >> 24
%endif

%if _CATCHINT06 && _DETECT95LX
	align 2, db 0
inttab_number_variable:
	dw inttab_number
%endif


	align 2, db 0
		; Parameter block for EXEC call
execblk:
		dw 0		;(00) zero: copy the parent's environment
.cmdline:	dw 0,0		;(02) address of command tail to copy
.fcb1:		dw 5Ch,0	;(06) address of first FCB to copy
.fcb2:		dw 6Ch,0	;(10) address of second FCB to copy
.sssp:		dw 0,0		;(14) initial SS:SP
.csip:		dw 0,0		;(18) initial CS:IP


		; Register save area (32 words).
		; must be DWORD aligned, used as stack
	align 4, db 0
regs:
reg_eax:	dd 0	;+00 eax
reg_ebx:	dd 0	;+04 ebx
reg_ecx:	dd 0	;+08 ecx
reg_edx:	dd 0	;+12 edx
reg_esp:	dd 0	;+16 esp
reg_ebp:	dd 0	;+20 ebp
reg_esi:	dd 0	;+24 esi
reg_edi:	dd 0	;+28 edi
reg_ds:		dd 0	;+32  ds (high word unused)
reg_es:		dd 0	;+36  es (high word unused)
reg_ss:		dd 0	;+40  ss (high word unused)
reg_cs:		dd 0	;+44  cs (high word unused)
reg_fs:		dd 0	;+48  fs (high word unused)
reg_gs:		dd 0	;+52  gs (high word unused)
reg_eip:	dd 0	;+56 eip
reg_efl:	dd 0	;+60 efl(ags)
regs.end:
regs.size: equ regs.end - regs

%if _REGSHIGHLIGHT
regs_prior:
.:
	times 16 dd 0
.end:
.size: equ .end - .

%if .size != regs.size
 %error regs prior save area size mismatch
%endif
%endif

%if _DEVICE
device_quittable_regs:
.:
	times 16 dd 0
.end:
.size: equ .end - .

%if .size != regs.size
 %error regs prior save area size mismatch
%endif
%endif

%if _VARIABLES
vregs: times 256 dd 0	; internal v0..vff
%endif

; possible byte encoding of lDebug variables for dynamic computations:
; xxxxyyyy
; 10: register
;   xx: size (0 = 1, 1 = 2, 2 = 4)
;     yyyy: 0..15: register as stored in the register save area
;                  as SIL, DIL, BPL, SPL aren't supported these map to xH
;                  xSL, IPL and FLL are invalid, ExS are invalid
; 1011: variable
;     yyyy: which variable. variables are always dword-sized
; 11000000: 32-bit compound, next byte stores: xxxxyyyy first, second 16-bit reg
; 11000001..11111111: available for encoding other compound regs, vars, indirection,
;                     symbols, types etc
; 0xxxxxxx: operators


; Instruction set information needed for the 'p' command.
; ppbytes and ppinfo needs to be consecutive.
ppbytes:db 66h,67h,26h,2Eh,36h,3Eh,64h,65h,0F2h,0F3h,0F0h	; prefixes
PPLEN_ONLY_PREFIXES	equ $-ppbytes
.string:
	db 0ACh,0ADh,0AAh,0ABh,0A4h,0A5h	; lods,stos,movs
	db 6Ch,6Dh,6Eh,6Fh			; ins,outs
	db 0A6h,0A7h,0AEh,0AFh			; cmps,scas
PPLEN_ONLY_STRING	equ $-ppbytes
.string_amount:		equ $ - .string
	db 0CCh,0CDh				; int instructions
	db 0E0h,0E1h,0E2h			; loop instructions
	db 0E8h					; call rel16/32
	db 09Ah					; call far seg16:16/32
;	(This last one is done explicitly by the code.)
;	db 0FFh					; FF/2 or FF/3: indirect call

PPLEN	equ	$-ppbytes	; size of the above table

;	Info for the above, respectively. This MUST follow
;	 immediately after ppbytes, as we add + PPLEN - 1 to
;	 di after repne scasb to index into this (ppinfo).
;	80h = prefix; 82h = operand size prefix; 81h = address size prefix.
;	If the high bit is not set, the next highest bit (40h) indicates
;	 that the instruction size depends on whether there is an operand
;	 size prefix; if set, under o32 two bytes are added to the size.
;	 (This is only used for direct near and far call.)
;	If both the two highest bits are clear, then PP_STRDEST,
;	 PP_STRSRC, or PP_STRSRC2 may be set. This only happens for
;	 string instructions, which always are neither prefixes nor
;	 use additional bytes.
;	The remaining bits tell the number of additional bytes in the
;	 instruction. This is at most 4. It must be below-or-equal to
;	 7, or if PP_VARSIZ is used, 5 (so the sum stays below 8).

PP_ADRSIZ	equ 01h
PP_OPSIZ	equ 02h
PP_PREFIX	equ 80h
PP_VARSIZ	equ 40h
PP_STRDEST	equ 20h
PP_STRSRC	equ 10h
PP_STRSRC2	equ 08h
PP_SIZ_MASK	equ 07h

ppinfo:	db PP_PREFIX | PP_OPSIZ, PP_PREFIX | PP_ADRSIZ
	times 9 db PP_PREFIX		; prefixes
	db PP_STRSRC, PP_STRSRC		; lods
	db PP_STRDEST, PP_STRDEST	; stos
	db PP_STRDEST | PP_STRSRC, PP_STRDEST | PP_STRSRC
					; movs
	db PP_STRDEST, PP_STRDEST	; ins
	db PP_STRSRC, PP_STRSRC		; outs
	db PP_STRSRC2 | PP_STRSRC, PP_STRSRC2 | PP_STRSRC
					; cmps
	db PP_STRSRC2, PP_STRSRC2	; scas
	db 0,1				; int
	db 1,1,1			; loop
	db PP_VARSIZ | 2		; call rel16/32 with displacement
	db PP_VARSIZ | 4		; call far 16:16 or 16:32 immediate

%if PPLEN != $-ppinfo
 %error "ppinfo table has wrong size"
%endif


;	Equates for instruction operands.
;	First the sizes.

OP_ALL		equ 40h		; byte/word/dword operand (could be 30h but ...)
OP_1632		equ 50h		; word or dword operand
OP_8		equ 60h		; byte operand
OP_16		equ 70h		; word operand
OP_32		equ 80h		; dword operand
OP_64		equ 90h		; qword operand
OP_1632_DEFAULT	equ 0A0h	; word or dword or default opsize

OP_SIZE	equ OP_ALL		; the lowest of these

;	These operand types need to be combined with a size.
;	Bits 0 to 3 give one of these types (maximum 15),
;	 and bits 4 to 7 specify the size. Table entries
;	 for these are identified by detecting that they
;	 are above-or-equal OP_SIZE.
;	The first parameter to the opsizeditem macro is the
;	 name of the item. It has to match the names used in
;	 the instr.key and debugtbl.inc files.
;	The second parameter is the entry for bittab that
;	 is used by aa.asm (the assembler).
;	The third parameter is the suffix used to create the
;	 entry for asmjmp (prefix aop_) and disjmp2 (dop_).

%macro opsizeditem 3.nolist
 %1 equ nextindex
 %xdefine BITTAB_OPSIZEDITEMS BITTAB_OPSIZEDITEMS,%2
 %xdefine ASMJMP_OPSIZEDITEMS ASMJMP_OPSIZEDITEMS,aop_%3
 %xdefine DISJMP2_OPSIZEDITEMS DISJMP2_OPSIZEDITEMS,dop_%3
 %assign nextindex nextindex + 1
%endmacro
%assign nextindex 0
%define BITTAB_OPSIZEDITEMS ""
%define ASMJMP_OPSIZEDITEMS ""
%define DISJMP2_OPSIZEDITEMS ""
opsizeditem OP_IMM,	ARG_IMMED,	imm	; immediate
opsizeditem OP_RM,ARG_DEREF+ARG_JUSTREG,rm	; reg/mem
opsizeditem OP_M,	ARG_DEREF,	m	; mem (but not reg)
opsizeditem OP_R_MOD,	ARG_JUSTREG,	r_mod	; register, determined from MOD R/M part
opsizeditem OP_MOFFS,	ARG_DEREF,	moffs	; memory offset; e.g., [1234]
opsizeditem OP_R,	ARG_JUSTREG,	r	; reg part of reg/mem byte
opsizeditem OP_R_ADD,	ARG_JUSTREG,	r_add	; register, determined from instruction byte
opsizeditem OP_AX,	ARG_JUSTREG,	ax	; al or ax or eax
%if nextindex > 16
 %error Too many op sized items
%endif

;	These don't need a size.
;	Because the size needs to be clear to indicate
;	 that one of these is to be used, the maximum
;	 value for these is 63 (as 64 is OP_SIZE).
;	The minimum value for these is 1 because a 0
;	 without size means the end of an op list (OP_END).
;	The first parameter to the opitem macro is the name
;	 of the item. It has to match the names used in the
;	 instr.key and debugtbl.inc files.
;	The second parameter is the entry for bittab that
;	 is used by aa.asm (the assembler). The third
;	 parameter is the entry for asmjmp.
;	The fourth parameter is the entry for optab as used
;	 by uu.asm (the disassembler).
;
;	asm_siznum contains entries for OP_M64 to OP_MXX.
;	 (The order has to match their opitem order.)
;	asm_regnum contains entries for OP_DX to OP_GS.
;	 (The order has to match their opitem order.)

%macro opitem 4.nolist
 %1 equ nextindex
 %xdefine BITTAB_OPITEMS BITTAB_OPITEMS,%2
 %xdefine ASMJMP_OPITEMS ASMJMP_OPITEMS,%3
 %xdefine OPTAB_OPITEMS OPTAB_OPITEMS,%4
 %assign nextindex nextindex + 1
%endmacro
	OP_END	equ 0
%assign nextindex 1
%define BITTAB_OPITEMS ""
%define ASMJMP_OPITEMS ""
%define OPTAB_OPITEMS ""
	; order of the following (ao17 entries) must match asm_siznum in aa.asm
OP_FIRST_ASM_SIZNUM	equ nextindex	; corresponding to asm_siznum start
opitem OP_M64,		ARG_DEREF,	ao17,dop_m64	; qword memory (obsolete?)
opitem OP_MFLOAT,	ARG_DEREF,	ao17,dop_mfloat	; float memory
opitem OP_MDOUBLE,	ARG_DEREF,	ao17,dop_mdouble; double-precision floating memory
opitem OP_M80,		ARG_DEREF,	ao17,dop_m80	; tbyte memory
opitem OP_MXX,		ARG_DEREF,	ao17,dop_mxx	; memory (size unknown)
opitem OP_FARIMM,	ARG_FARADDR,	ao21,dop_farimm	; far16/far32 immediate
opitem OP_REL8,		ARG_IMMED,	ao23,dop_rel8	; byte address relative to IP
opitem OP_REL1632,	ARG_IMMED,	ao25,dop_rel1632; word or dword address relative to IP
opitem OP_1CHK,		ARG_WEIRDREG,	ao29,dop49	; check for ST(1)
opitem OP_STI,		ARG_WEIRDREG,	aop_sti,dop_sti	; ST(I)
opitem OP_CR,		ARG_WEIRDREG,	aop_cr,dop_cr	; CRx
opitem OP_DR,		ARG_WEIRDREG,	ao34,dop_dr	; DRx
opitem OP_TR,		ARG_WEIRDREG,	ao35,dop_tr	; TRx
opitem OP_SEGREG,	ARG_WEIRDREG,	ao39,dop_segreg	; segment register
opitem OP_IMMS8,	ARG_IMMED,	ao41,dop_imms8 	; sign extended immediate byte
opitem OP_IMMS8_EXTEND,	ARG_IMMED,	ao41_extend,dop_imms8		; add etc word/dword r/m, imms8
opitem OP_IMM8,		ARG_IMMED,	ao42,dop_imm8	; immediate byte (other args may be (d)word)
opitem OP_IMM8_OPTIONAL,ARG_IMMED,	ao42,dop_imm8_optional
opitem OP_IMM8_INT,	ARG_IMMED,	ao42,dop_imm8_int		; immediate byte for int
opitem OP_MMX,		ARG_WEIRDREG,	aop_mmx,dop_mmx	; MMx
opitem OP_MMX_MOD,	ARG_WEIRDREG,	aop_mmx_mod,dop_mmx_mod	; MMx, but in ModR/M part
opitem OP_SHOSIZ,	0FFh,	ao44,	dop_shosiz	; set flag to always show the size
opitem OP_SHORT,	0FFh,	ao_short,dop_short	; allow short keyword
opitem OP_NEAR,		0FFh,	ao_near,dop_near	; allow near keyword
opitem OP_FAR,		0FFh,	ao_far,	dop_far		; allow far keyword
opitem OP_FAR_REQUIRED,	0FFh,	ao_far_required,dop_far_required	; require far keyword
opitem OP_FAR_M,	0FFh,	ao_modifier_continue,dop_far_m		; les, lds, lss, lfs, lgs, or jmp/call far mem
opitem OP_DOUBLE_M,	0FFh,	ao_modifier_continue,dop_double_m	; bound
opitem OP_M_SRC,	0FFh,	ao_modifier_continue,dop_m_src
opitem OP_M_DST,	0FFh,	ao_modifier_continue,dop_m_dst
opitem OP_M_SRC_DST,	0FFh,	ao_modifier_continue,dop_m_src_dst
opitem OP_STACK_PUSH,	0FFh,	ac09_internal_error,dop_stack_push
opitem OP_STACK_POP,	0FFh,	ac09_internal_error,dop_stack_pop
opitem OP_STACK_SPECIAL,0FFh,	ac09_internal_error,dop_stack_special
opitem OP_M_ALWAYS_16,	0FFh,	ao_m_always_16,dop_m_always_16
opitem OP_E_CX,	ARG_JUSTREG,	aop_e_cx, da_internal_error		; (E)CX
OP_FIRST_STRING equ nextindex
opitem OP_1,	ARG_IMMED,	ao46, "1"	; 1 (simple "string" ops from here on)
opitem OP_3,	ARG_IMMED,	ao47, "3"	; 3
	; order of the following (ao48 entries) must match asm_regnum in aa.asm
OP_FIRST_ASM_REGNUM	equ nextindex	; corresponding to asm_regnum start
opitem OP_DX,	ARG_JUSTREG,	ao48, "DX"	; DX
opitem OP_CL,	ARG_JUSTREG,	ao48, "CL"	; CL
opitem OP_ST,	ARG_WEIRDREG,	ao48, "ST"	; ST (top of coprocessor stack)
opitem OP_CS,	ARG_WEIRDREG,	ao48, "CS"	; CS
opitem OP_DS,	ARG_WEIRDREG,	ao48, "DS"	; DS
opitem OP_ES,	ARG_WEIRDREG,	ao48, "ES"	; ES
opitem OP_FS,	ARG_WEIRDREG,	ao48, "FS"	; FS
opitem OP_GS,	ARG_WEIRDREG,	ao48, "GS"	; GS
opitem OP_SS,	ARG_WEIRDREG,	ao48, "SS"	; SS
OP_AFTER_LAST equ nextindex
%if nextindex > OP_SIZE
 %error Too many op items
%endif
OP_AMOUNT_TABLE	equ nextindex + 16 - 1
		; nextindex: amount sizeless types
		; 16: OP_SIZE combined types
		; -1: OP_END does not occur in tables

	; Instructions that have an implicit operand subject to a segment prefix.
	; This means a prefixed segment is allowed by the strict assembler, and
	; the disassembler treats a segment prefix as part of the instruction and
	; displays it in front of the instruction's mnemonic.
	; (outs, movs, cmps, lods, xlat).
segprfxtab:
	db 06Eh,06Fh,0A4h,0A5h,0A6h,0A7h,0ACh,0ADh
a32prfxtab:
	db 0D7h				; xlat, last in segprfxtab, first in a32prfxtab
SEGP_LEN equ $-segprfxtab

		; Instructions that can be used with REPE/REPNE.
		; (ins, outs, movs, stos, lods; cmps, scas)
replist:db 06Ch,06Eh,0A4h,0AAh,0ACh	; REP (no difference)
REP_SAME_LEN equ $-replist		; number of indifferent replist entries
	db 0A6h,0AEh			; REPE/REPNE
REP_LEN equ $-replist
REP_DIFF_LEN equ REP_LEN-REP_SAME_LEN	; number of replist entries with difference

A32P_LEN equ $-a32prfxtab

; prfxtab P_LEN REP_LEN REPE_REPNE_LEN

		; All the instructions in replist also have an implicit operand
		; subject to ASIZE (similar to segprfxtab). Additionally, the
		; xlat instruction (0D7h) has such an implicit operand too.
		; maskmovq too.

	align 2, db 0
o32prfxtab:
	dw 0Eh, 1Eh, 06h, 16h, SPARSE_BASE + 0A0h, SPARSE_BASE + 0A8h
	; push cs, push ds, push es, push ss, push fs, push gs
	dw 1Fh, 07h, 17h, SPARSE_BASE + 0A1h, SPARSE_BASE + 0A9h
	; pop ds, pop es, pop ss, pop fs, pop gs
O32P_AMOUNT equ ($ - o32prfxtab) / 2


	%include "asmtabs.asm"


	usesection lDEBUG_DATA_ENTRY

msg_start:
	%include "msg.asm"

msg_end:

	numdef SHOWMSGSIZE, _DEFAULTSHOWSIZE
%if _SHOWMSGSIZE
%assign MSGSIZE msg_end - msg_start
%warning msg holds MSGSIZE bytes
%endif


	usesection lDEBUG_DATA_ENTRY
		; INP:	word [cs:ip] = near address to jump to in other segment
..@symhint_trace_caller_entry_to_code_seg:
entry_to_code_seg:
	push ax			; word space for ?jumpaddress_ip, is ax
	mov ax, word [cs:code_seg]
%if _PM
	jmp entry_to_code_common

..@symhint_trace_caller_entry_to_code_sel:
entry_to_code_sel:
	push ax
	mov ax, word [cs:code_sel]
%endif

entry_to_code_common:
	lframe 0
	lpar word, jumpaddress_cs_and_orig_ip
	lpar word, jumpaddress_ip
	lenter

	push si
	pushf
	cld

	xchg word [bp + ?jumpaddress_cs_and_orig_ip], ax	; fill function segment
	mov si, ax
	cs lodsw
%if _DEBUG
	cmp al, 0CCh		; debugger breakpoint ?
	jne @F			; no -->
	int3			; break to make it remove the breakpoint
	dec si
	dec si
	cs lodsw		; reload the word
	cmp al, 0CCh
	jne @F

.l:
	int3
	jmp .l

@@:
%endif
	xchg word [bp + ?jumpaddress_ip], ax		; fill function offset
		; (and restore ax)

	popf
	pop si

	lleave
	retf			; jump to dword [bp + ?jumpaddress]



		; debug22 - Interrupt 22h handler
		;
		; This is for DEBUG itself: it's a catch-all for the various Int23
		; and Int24 calls that may occur unpredictably at any time. What we
		; do is pretend to be a command interpreter (which we are, in a sense,
		; just with different sort of commands) by setting our parent PSP
		; value equal to our own PSP so that DOS does not free our memory when
		; we quit. Therefore control ends up here when DOS detects Control-C
		; or an Abort in the critical error prompt is selected.
debug22:
	cli
.cleartraceflag:
	cld			; reestablish things
	mov ax, cs
	mov ds, ax
	mov ss, ax
	mov sp, word [ savesp ]	; restore stack
%if _PM
	clropt [internalflags], protectedmode	; reset PM flag
%endif
	times 1 - (($ - $$) & 1) nop	; align in-code parameter
	call entry_to_code_seg
	dw cmd2_reset_re_maybe_pm


%if _DUALCODE
	usesection lDEBUG_CODE2
 %if $ - $$
  %error cmd3_mirror is not at offset 0 in lDEBUG_CODE2
 %endif
cmd3_mirror:
	db 0B9h				; mov cx, imm16 (cx = nonzero)
	xor cx, cx			; cx = 0
	dualcall cmd3.have_cx_convenience
%endif


	usesection lDEBUG_CODE
%if $ - $$
 %error cmd3 is not at offset 0 in lDEBUG_CODE
%endif

	code_insure_low_byte_not_0CCh
		; Begin main command loop.
cmd3:
		; A convenience entrypoint: Entering
		;  cmd3 at offset 1 instead of 0 will
		;  make the debugger additionally
		;  display a linebreak early on.
	db 0B9h				; mov cx, imm16 (cx = nonzero)
	xor cx, cx			; cx = 0
.have_cx_convenience:

	push ss
	pop ds
	_386_o32		; mov esp
	mov sp, word [ savesp ]		; restore stack
_386	and sp, ~3			; align stack
	_386_o32
	xor ax, ax
	_386_o32
	push ax
	_386_o32
	popf
_386	mov sp, word [ savesp ]		; restore stack
	cld
	sti
	mov word [ errret ], cmd3
	mov word [ throwret ], errhandler
	mov word [ throwsp ], sp
%if _SYMBOLIC
	mov word [ stack_low_address ], str_buffer
%endif
	xor ax, ax
	xchg ax, word [cmd3_set_options]
	or word [options], ax

	push ds
	pop es
	jcxz @F
	jmp @FF
@@:
	mov dx, crlf
	call putsz
@@:

%if _SYMBOLIC
	clropt [internalflags2], dif2_xms_detection_done
	nearcall zz_save_strat
%endif

	xor di, di
	xchg di, word [terminator_in_line_in.offset]
	test di, di
	jz @F
	cmp byte [di], 0
	jne @F
	mov al, byte [terminator_in_line_in.value]
	stosb
@@:

	and word [run_sp_reserve], 0
%if _IMMASM
	clropt [internalflags6], dif6_immasm_no_output | dif6_immasm
%endif
	clropt [internalflags3], dif3_unquiet_error
%if _REGSHIGHLIGHT
	clropt [internalflags3], dif3_do_not_highlight
%endif
%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
	clropt [internalflags3], dif3_auxbuff_guarded_1 | dif3_in_if
%else
	clropt [internalflags3], dif3_auxbuff_guarded_1
%endif
	clropt [internalflags3], \
		dif3_input_serial_override | dif3_input_terminal_override
	clropt [internalflags2], dif2_in_silence_dump
%if _PM
 %if _DEBUG
	clropt [internalflags6], dif6_in_hook2F | dif6_in_amis_hook2F
 %else
	clropt [internalflags6], dif6_in_amis_hook2F
 %endif
	call resetmode
%endif

%ifn _VXCHG
	mov ah, 0Fh
	int 10h
	mov byte [vpage], bh			; update page
%endif


%if _DEBUG && _DEBUG_COND
cmd3_debug_mode_init:
	testopt [options6], opt6_debug_mode
	jz .check_disable
.check_enable:
	testopt [internalflags6], dif6_debug_mode
	jnz .done
	call reset_interrupts
	setopt [internalflags6], dif6_debug_mode
	jmp .done

.check_disable:
	testopt [internalflags6], dif6_debug_mode
	jz .done
	call set_interrupts
	clropt [internalflags6], dif6_debug_mode
.done:
 %endif


%if _PM
cmd3_int2F_init:
	mov al, 2Fh		; interrupt number
	mov si, debug2F		; -> IISP entry header

	testopt [options4], opt4_int_2F_hook
	jnz .done
.check_disable:
	testopt [internalflags4], dif4_int_2F_hooked
	jz .done

	mov dx, opt4_int_2F_force >> 16
	call UnhookInterruptForce
	jc .done

	clropt [internalflags], hooked2F
	clropt [internalflags4], dif4_int_2F_hooked
	call update_inttab_optional

.done:
%endif


%if _CATCHINT08
cmd3_int08_init:
	mov al, 08h		; interrupt number
	mov si, intr8		; -> IISP entry header

	testopt [options4], opt4_int_08_hook
	jz .check_disable
.check_enable:
	testopt [internalflags4], dif4_int_08_hooked
	jnz .done

	call install_86m_interrupt_handler
	setopt [internalflags4], dif4_int_08_hooked
	call update_inttab_optional
	jmp .done

.check_disable:
	testopt [internalflags4], dif4_int_08_hooked
	jz .done

	mov dx, opt4_int_08_force >> 16
	call UnhookInterruptForce
	jc .done

	clropt [internalflags4], dif4_int_08_hooked
	call update_inttab_optional

.done:
%endif


%if _CATCHINT2D
cmd3_int2D_init:
	mov al, 2Dh		; interrupt number
	mov si, int2D		; -> IISP entry header

	testopt [options4], opt4_int_2D_hook
	jz .check_disable
.check_enable:
	testopt [internalflags4], dif4_int_2D_hooked
	jnz .done

	call intchk			; ZR if offset = -1 or segment = 0
					; CHG: ax, dx, bx
	jz .fail

	mov ah, byte [try_amis_multiplex_number]
	mov al, 00h
		; function 0 changes dx, di, cx, al
%if _PM
	call call_int2D
%else
	int 2Dh				; enquire whether there's anyone
%endif
	test al, al
	jz .got

	xor ax, ax			; start with multiplex number 0
.loopplex:
	mov al, 00h			; AMIS installation check
		; function 0 changes dx, di, cx, al
%if _PM
	call call_int2D
%else
	int 2Dh				; enquire whether there's anyone
%endif
	test al, al			; free ?
	jz .got				; yes, put it to use -->
	inc ah
	jnz .loopplex			; try next multiplexer -->

	mov dx, msg.cannot_hook_2D.nofree
	jmp .fail_putsz

.got:
	mov byte [amis_multiplex_number], ah

	mov al, 2Dh		; interrupt number
	call install_86m_interrupt_handler
	setopt [internalflags4], dif4_int_2D_hooked
	jmp .done

.fail:
	mov dx, msg.cannot_hook_2D.invalid
.fail_putsz:
	call putsz
	clropt [options4], opt4_int_2D_hook
	jmp .done

.check_disable:
	testopt [internalflags4], dif4_int_2D_hooked
	jz .done

	mov dx, opt4_int_2D_force >> 16
	call UnhookInterruptForce
	jc .done

	clropt [internalflags4], dif4_int_2D_hooked

.done:
%endif


	testopt [internalflags3], dif3_input_re
	jnz cmd3_continue_1_re
	clropt [options2], opt2_re_cancel_tpg
	call silence_dump


cmd3_serial_init:
	testopt [options], enable_serial
	jz .check_disable_serial
.check_enable_serial:
	testopt [serial_flags], sf_init_done
	jnz .done_serial
.enable_serial:

	mov al, byte [serial_var_intnum]
	mov byte [serial_use_intnum], al
	mov al, byte [serial_var_params]
	mov byte [serial_use_params], al
	mov al, byte [serial_var_fifo]
	mov byte [serial_use_fifo], al
	mov ax, word [serial_var_baseport]
	mov word [serial_use_baseport], ax
	mov ax, word [serial_var_dl]
	mov word [serial_use_dl], ax
	mov ax, word [serial_var_irqmask]
	mov word [serial_use_irqmask], ax
  call  serial_clear_fifos
  call  serial_install_interrupt_handler
	jnc @F
	mov di, msg.serial_cannot_hook.old_int
	mov al, byte [serial_installed_intnum]
	call hexbyte
	mov di, msg.serial_cannot_hook.new_int
	mov al, byte [serial_use_intnum]
	call hexbyte
	mov dx, msg.serial_cannot_hook
	jmp .no_keep

@@:
	mov byte [serial_interrupt_handler + ieEOI], 80h
  call  serial_init_UART

	setopt [serial_flags], sf_init_done

	mov dx, msg.serial_request_keep
	call putsz

	mov di, line_out
%if _DEBUG
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz @F
 %endif
	mov al, '~'		; indicate instance is to be debugged
	stosb
@@:
%endif
	mov ax, "= "
	stosw

	xor ax, ax
	mov word [getline_timer_count], ax
	push es
	mov ax, 40h
	mov es, ax
	mov ax, word [es:6Ch]
	mov word [getline_timer_last], ax
	pop es
	mov word [getline_timer_func], .timer

		; if we're executing from the command line
		;  buffer or a Y file then we want to
		;  override input to be from serial for the
		;  KEEP confirmation prompt.
		; output is always to serial if we're here.
	setopt [internalflags3], dif3_input_serial_override
	call getline00
	clropt [internalflags3], dif3_input_serial_override

	call skipcomm0
	dec si
	mov dx, msg.keep
	call isstring?
	je .done_serial

	mov dx, msg.serial_no_keep_enter
.no_keep:
	clropt [options], enable_serial
	call putsz
	jmp cmd3


.timer:
	push ax
	push dx
	push cx
	push bx
	push es

	mov dx, 40h
	mov es, dx

  mov al, 0Bh	; request In-Service Register (ISR)
  out 0A0h, al	; from secondary PIC
  in al, 0A0h	; read the ISR
  test al, byte [serial_use_irqmask + 1]
  jnz .timer_ours
  mov al, 0Bh	; request In-Service Register (ISR)
  out 20h, al	; from primary PIC
  in al, 20h	; read the ISR
  and al, ~100b
  test al, byte [serial_use_irqmask]
  jnz .timer_ours
.timer_ours_done:

  mov cx, word [serial_save_irq_mask]
  xor bx, bx		; all bits clear (= IRQ ON)
  call set_irq		; enable IRQs and get prior status
  test bx, bx		; IRQs were still enabled ?
  jz @F			; yes, fine -->
  clropt [options6], opt6_share_serial_irq
			; no, make sure not to chain any longer
@@:
	mov cx, word [getline_timer_count]
	mov dx, word [getline_timer_last]

%if _SLEEP_NEW
	mov ax, word [es:6Ch]
	cmp dx, ax
	je .timer_next
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
	jnc @F
	mov cx, -1
@@:
	mov dx, ax
%else
	cmp dx, word [es:6Ch]
	je .timer_next
	mov dx, word [es:6Ch]
	inc cx
%endif
	mov al, 18
	mul byte [serial_keep_timeout]
	test ax, ax
	jz .timer_next
	cmp cx, ax
	jb .timer_next

	pop es
	mov dx, msg.serial_no_keep_timer
	jmp .no_keep

.timer_ours:
	setopt [options6], opt6_serial_EOI_call
  call serial_eoi
	jmp .timer_ours_done

.timer_next:
	mov word [getline_timer_count], cx
	mov word [getline_timer_last], dx
	pop es
	pop bx
	pop cx
	pop dx
	pop ax
	retn


.check_disable_serial:
		; If serial is initialised, uninstall it.
	testopt [serial_flags], sf_init_done
	jnz .disable_serial
		; Not initialised. Is the interrupt still hooked?
	testopt [internalflags4], dif4_int_serial_hooked
	jz .done_serial
		; Try unhooking the interrupt handler.
	call serial_uninstall_interrupt_handler
	jc .done_serial			; if it failed again -->
	mov di, msg.serial_late_unhook.int
	mov al, byte [serial_installed_intnum]
	call hexbyte
	mov dx, msg.serial_late_unhook
	call putsz
	jmp .done_serial

.disable_serial:

  call serial_clean_up
	jnc @F
	mov di, msg.serial_cannot_unhook.int
	mov al, byte [serial_installed_intnum]
	call hexbyte
	mov dx, msg.serial_cannot_unhook
	call putsz
	mov byte [serial_interrupt_handler + ieEOI], 0
					; we do not issue EOI any longer
@@:
	clropt [serial_flags], sf_init_done
.done_serial:


%if _VXCHG
cmd3_vv_set:
	call vv_set
%endif


%if _PM
cmd3_ss_init:
	call ispm
	jnz .done

subcpu 286
	mov bx, ss
	lar cx, bx
	jnz .done
	shr cx, 8

	testopt [options3], opt3_ss_b_bit_set
	jz .check_clear
.check_set:
	testopt [internalflags3], dif3_ss_b_bit_set
	jnz .done

	mov ch, 40h
	jmp @F

.check_clear:
	testopt [internalflags3], dif3_ss_b_bit_set
	jz .done

@@:
	mov ax, 0009h
	int 31h
	jc .done

	xoropt [internalflags3], dif3_ss_b_bit_set
subcpureset

.done:
%endif


%if _IMMASM
	call near [ ia_restore ]
%endif

%if _PM
	call ispm
	jz @F
%endif
	call ensuredebuggeeloaded	; if no task is active, create a dummy one
%if _PM && 0
	jmp @FF
@@:
	testopt [internalflags], attachedterm
	jz @F
	mov dx, .message
	call putsz

	usesection lDEBUG_DATA_ENTRY
.message:	ascizline "Attached term is set in PM!"
	usesection lDEBUG_CODE
%endif
@@:


cmd3_continue_1_re:
	mov di, line_out	; build prompt
%if _DEBUG
 %if _DEBUG_COND
	testopt [internalflags6], dif6_debug_mode
	jz @F
 %endif
	mov al, '~'		; indicate instance is to be debugged
	stosb
@@:
%endif
%if _INDOS_PROMPT
	call InDos
	jz @F
 %if _BOOTLDR
	testopt [internalflags], nodosloaded
				; boot mode ?
  %if _INDOS_PROMPT_NOBOOT
	jnz @F			; yes, do not show special prompt -->
  %elif _INDOS_PROMPT_NOFLAG
	jnz .indos_prompt	; yes, show special prompt -->
				;  (do not call .real_indos check)
  %endif
 %endif
 %if _INDOS_PROMPT_NOFLAG
	call InDos.real_indos	; real InDOS set ?
	jz @F			; no, do not show special prompt -->
 %endif
.indos_prompt:
	mov al, '!'
	stosb
@@:
%endif
	mov al, '-'		; main prompt
%if _PM
	call ispm
	jnz .realmode
	mov al, '#'		; PM main prompt
.realmode:
%endif
	testopt [internalflags3], dif3_input_cmdline
	jz @F
	mov al, '&'
@@:
	testopt [internalflags3], dif3_input_re
	jz @F
	mov al, '%'
@@:
	stosb

	mov byte [hhflag], 0
	and word [hh_depth], 0
	and word [hh_depth_of_single_term], 0
	mov word [getline_timer_func], dmycmd
	clropt [internalflags], usecharcounter	; reset this automatically

	testopt [internalflags3], dif3_input_re
	jnz cmd3_continue_2_re

	setopt [internalflags], pagedcommand	; 2009-02-21: default to page all commands
	clropt [internalflags], tt_silence | tt_silent_mode
				; reset, in case it's still set
	clropt [internalflags2], dif2_tpg_proceed_bp_set | \
			dif2_bp_failure | dif2_tpg_keep_proceed_bp, 1
%if _INPUT_FILE_HANDLES
	clropt [internalflags2], dif2_closed_input_file
%endif

cmd3_continue_2_re:
	call determine_quiet_output

	xor cx, cx
	xchg cx, word [rc]	; reset rc
	mov word [priorrc], cx	; make prior value available
	jcxz @F
	mov word [erc], cx	; update to last non-zero value
@@:

cmd3_check_relimit:
	testopt [internalflags3], dif3_input_re
	jz cmd3_continue_not_re

	add word [re_count], 1
	adc word [re_count + 2], 0
	mov dx, word [re_limit + 2]
	mov ax, word [re_limit]
	cmp word [re_count + 2], dx
	jne @F
	cmp word [re_count], ax
@@:
		; This branch bypasses cmd3_check_rclimit
		;  because RE buffer commands should not
		;  count towards the RC limit.
	jbe cmd3_continue_relimit_not_reached

	mov dx, msg.re_limit_reached
	jmp cmd3_check_common

cmd3_continue_not_re:

cmd3_check_rclimit:

%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
		; If executing from a script file then
		;  command doesn't count for RC limit.
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jnz .file
%endif
%if _INPUT_FILE_HANDLES
	call InDos
	jnz .file_not
	testopt [internalflags2], dif2_input_file
	jnz .file
%endif
.file_not:

.file: equ cmd3_continue_rclimit_not_reached
%endif

	testopt [internalflags3], dif3_input_cmdline
	jz cmd3_continue_not_rc

	add word [rc_count], 1
	adc word [rc_count + 2], 0
	mov dx, word [rc_limit + 2]
	mov ax, word [rc_limit]
	cmp word [rc_count + 2], dx
	jne @F
	cmp word [rc_count], ax
@@:
	jbe cmd3_continue_rclimit_not_reached

	mov dx, msg.rc_limit_reached
cmd3_check_common:
	call putsz_error
	mov ax, 0104h
	call setrc
	setopt [internalflags3], dif3_at_line_end
	call getline_close_file
	jmp cmd3

cmd3_continue_not_rc:
cmd3_continue_rclimit_not_reached:
cmd3_continue_relimit_not_reached:

cmd3_check_line_out_overflow:
	cmp word [line_out_overflow], 2642h
	je @F
	mov word [line_out_overflow], 2642h
	mov dx, msg.line_out_overflow
	call putsz_error
@@:

cmd3_getline:
	call getline00		; prompted input, also resets linecounter

	call iseol?.notsemicolon
	jne cmd3_notblank
	testopt [options3], opt3_disable_autorepeat
	jnz @F
	mov dx, word [lastcmd]
	mov byte [si], al
	jmp short cmd4

@@:
	mov word [lastcmd], dmycmd
	jmp cmd3

cmd3_notblank:
%if _SYMBOLIC
	clropt [internalflags3], dif3_nosymbols_1
%endif
	mov word [lastcmd], dmycmd
	cmp al, ';'
	je cmd3_j1		; if comment -->
	cmp al, ':'
	je cmd3_j1		; if jump label -->
	cmp al, '?'
	je help			; if request for help -->
	cmp al, '.'
	je immasm		; if assembling/immediate execution -->
	cmp al, '-'
	jne @F			; if not no symbol prefix -->
%if _SYMBOLIC
	setopt [internalflags3], dif3_nosymbols_1
%endif
	call skipwhite		; skip to next command letter
		; Empty line (autorepeat) not valid.
		; Comment not valid. Goto label not valid.
		; Help request not valid.
@@:
	call uppercase
	sub al, 'A'
%if _SYMBOLIC
	cmp al, 'Z' - 'A'
	ja error		; if not recognised -->
	je @F			; if Z, do not commit -->
	nearcall zz_commit_insert	; if not Z, commit now
@@:
%else
	cmp al, 'Y' - 'A'
	ja error		; if not recognised -->
%endif
	cbw
	xchg bx, ax
	call skipcomma
	shl bx, 1
	mov dx, word [ cmdlist+bx ]
cmd4:
	mov di, line_out
%if _DEBUG
	db __TEST_IMM8		; (skip int3)
.int3:
	int3			; used by BU command
%endif
	call dx
cmd3_j1:
	jmp cmd3		; back to the top


	code_insure_low_byte_not_0CCh
cmd2_reset_re_maybe_pm:

	_386_o32		; mov esp
	mov sp, word [ savesp ]		; restore stack
_386	and sp, ~3			; align stack
	_386_o32
	xor ax, ax
	_386_o32
	push ax
	_386_o32
	popf
_386	mov sp, word [ savesp ]		; restore stack
	cld
	sti

%if _PM
	call handle_mode_changed
%endif

	code_insure_low_byte_not_0CCh
cmd2_reset_re:
	mov bx, word [io_levels]
.entry_bx_levels:
	xor cx, cx
.entry_bx_levels_cx_cmdline:
	xor dx, dx
%if _INPUT_FILE_HANDLES
	testopt [internalflags2], dif2_input_file
	jz @F
	add cx, word [input_file_handles.active]
	inc cx
@@:
%endif
%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	add cx, word [load_input_file.active]
	inc cx
@@:
%endif
	testopt [internalflags3], dif3_input_re
	jz @F
	inc cx
	inc dx
		; Flag: If we abort anything (effective IOL >= 1)
		;  then we need to cancel RE buffer execution.
		;  This is so because RE execution is always the
		;  topmost command source, taking precedence over
		;  yy as well as RC buffer execution.
	testopt [io_flags], iof_extra_iol_for_tpg_re
	jz @F
	inc bx
	jnz @F
	dec bx
@@:
	testopt [internalflags3], dif3_input_cmdline
	jz @F
	inc cx
	testopt [io_flags], iof_extra_iol_for_rc
	jz @F
	inc bx
	jnz @F
	dec bx
@@:
	cmp cx, bx
	jbe @F
	mov cx, bx
@@:
	jcxz cmd3_j1			; IOL zero or nothing active -->
	push ds
	pop es
@@:
	push cx
	push dx
	call getline_close_file.resetstuff
	pop dx
	pop cx
	loop @B
	test dx, dx			; first cancelled was RE ?
	jz cmd3_j1			; no, just proceed now -->
	setopt [options2], opt2_re_cancel_tpg
					; set to cancel command
	jmp dumpregs_extended.exit	; clean up RE state


dmycmd:
	retn

help:
	call skipcomma
	call uppercase
%if _EXTHELP
 %if _COND
	mov dx, msg.condhelp
	cmp al, 'C'
	je .spec
 %endif
 %if _OPTIONS
	cmp al, 'O'
	je .options		; option help -->
 %endif
 %if _EXPRESSIONS
	mov dx, msg.expressionhelp
	cmp al, 'E'
	je .spec
 %endif
%endif
%if _EMS
	mov dx, msg.xhelp
	cmp al, 'X'
	je .spec
%endif
	dec si
 %if _BOOTLDR && _EXTHELP
	mov dx, msg.boot
	call isstring?
	mov dx, msg.boothelp
	je .spec
 %endif
%if _PM
	mov dx, msg.desc
	call isstring?
	mov dx, msg.deschelp
	je .spec
%endif
%if _EXTHELP
	mov dx, msg.source
	call isstring?
	mov dx, msg.help_source
	je .spec
%endif
	mov dx, msg.re
	call isstring?
	mov dx, msg.help_re
	je .spec
	mov dx, msg.run
	call isstring?
	mov dx, msg.help_run
	je .spec
	mov dx, msg.string_build
	call isstring?
	mov bx, msg.build_array
	mov cx, msg.build_short_amount
	je .spec_multi
	lodsb
	call uppercase
	mov cx, msg.build_long_amount
	cmp al, 'B'
	je .spec_multi		; build info -->
%if _EXTHELP
	mov dx, msg.license
	cmp al, 'L'
	je .spec		; licence -->
	mov dx, msg.flaghelp
	cmp al, 'F'
	je .spec		; flag help -->
	mov dx, msg.reghelp
	cmp al, 'R'
	je .spec		; register help -->
 %if _VARIABLES || _OPTIONS || _PSPVARIABLES
	mov dx, msg.varhelp
	cmp al, 'V'
	je .spec		; variable help -->
 %endif
%endif
	mov dx, msg.help	; default help
	db __TEST_IMM8		; (skip lodsb)
.spec:
	lodsb
	call chkeol
prnquit:
	call putsz		; print string
cmd3_j1a:
	jmp cmd3_j1		; done

errorj1:jmp error

help.spec_multi:
	lodsb
	call chkeol
.loop:
	mov dx, word [bx]
	call putsz
	inc bx
	inc bx
	loop .loop
	jmp short cmd3_j1a

%if _EXTHELP && _OPTIONS
help.options:
	mov bx, si
	call skipwhite
	call iseol?
	je .all
	call uppercase
	cmp al, 'A'
	mov dx, msg.asmoptions_1
	je .single
	cmp al, 'I'
	mov dx, msg.flags_1
	je .single
	mov di, msg.options_scan
	mov cx, msg.options_scan_amount
	repne scasb
	jne .pages
	sub di, msg.options_scan + 1
	shl di, 1
	mov di, word [msg.options_array + di]
	mov dx, di
	cmp byte [di], 0
	je errorj1
.single:
	jmp help.spec

.pages:
	lea si, [bx - 1]
	mov dx, msg.string_options
	call isstring?
	jne errorj1
	mov dx, msg.options_pages
	jmp .single

.all:
	mov bx, msg.options_array
	mov cx, msg.options_array_amount
.loop:
	mov di, word [bx]
	mov dx, word [bx]
	call putsz
	inc bx
	inc bx
	cmp byte [di], 0
	je @F
	cmp cx, 1
	je @F
	mov dx, crlf
	call putsz
@@:
	loop .loop
	jmp short cmd3_j1a
%endif

determine_quiet_output:
	clropt [internalflags3], dif3_quiet_output

	push di
	push ax
	testopt [internalflags3], dif3_input_re
	jnz .notquiet

%if _INPUT_FILE_BOOT
	testopt [internalflags2], dif2_input_file_boot
	jz @F
	mov ax, LOAD_INPUT_FILE_SIZE
	push dx
	mul word [load_input_file.active]
	pop dx
	mov di, ax
	testopt [load_input_file + di - LOADDATA3 + ldFATType], ifhfQuietOutput
	jmp .quiet_if_nz

@@:
%endif
%if _INPUT_FILE_HANDLES
	call InDos
	jnz @F

	testopt [internalflags2], dif2_input_file
	jz @F
	mov di, word [input_file_handles.active]
	shl di, 1
	shl di, 1
	shl di, 1		; to qword array index
 %if INPUTFILEHANDLE_size != 8
  %error Unexpected structure size
 %endif
	testopt [input_file_handles + di + ifhFlags], ifhfQuietOutput
	jmp .quiet_if_nz

@@:
%endif
	testopt [internalflags3], dif3_input_cmdline
	jz @F
	testopt [options], opt_cmdline_quiet_output
	; jmp .quiet_if_nz

.quiet_if_nz:
	jz @F
.quiet:
	setopt [internalflags3], dif3_quiet_output
.notquiet:
@@:
	pop ax
	pop di
	retn


guard_auxbuff:
	testopt [internalflags3], dif3_auxbuff_guarded_1 | dif3_auxbuff_guarded_2
	jnz @F
	setopt [internalflags3], dif3_auxbuff_guarded_1
	retn

@@:
	mov ax, 0101h
	call setrc
	mov dx, msg.guard_auxbuff_error
.putsz_error:
	call putsz
	jmp cmd3


		; This is used to disallow commands
		;  while reading from the RE buffer.
guard_re:
	testopt [internalflags3], dif3_input_re
	jnz @F
	retn

@@:
	mov ax, 0102h
	call setrc
	mov dx, msg.guard_re_error
	jmp guard_auxbuff.putsz_error


		; This is used to disallow commands
		;  while reading from the RC buffer.
guard_rc:
	testopt [internalflags3], dif3_input_cmdline
	jnz @F
	retn

@@:
	mov ax, 0102h
	call setrc
	mov dx, msg.guard_rc_error
	jmp guard_auxbuff.putsz_error


%include "amis.asm"


	; doscall is used by symbols.asm and run.asm, so define it prior
%if _PM && _NOEXTENDER
%macro doscall 0
	nearcall _doscall
%endmacro
%else
		; When we don't support non-extended DPMI all Int21 calls
		; are either in Real Mode or extended (all are real Int21
		; instructions).
%macro doscall 0
	int 21h
%endmacro
%endif


	usesection lDEBUG_CODE

%if _DEBUG4 || _DEBUG5
%define _DEB_ASM_PREFIX
%include "deb.asm"
%endif


%include "aa.asm"
%include "dd.asm"
%include "rr.asm"
%if _RN
%include "fptostr.asm"
%endif
%include "run.asm"
%include "install.asm"
%include "uu.asm"
%if _IMMASM
%include "immasm.asm"
%else
immasm:
	lodsb
	call chkeol
	jmp cmd3
%endif


	usesection lDEBUG_DATA_ENTRY

%if _PM || _CATCHINT07 || _CATCHINT0C || _CATCHINT0D
	align 4, db 0
exception_csip:	dd 0	; 16:16 far 16-bit address of debugger exception
 %if _AREAS
exception_stack:times 4 dw 0
			; stack of debugger exception
 %endif
%endif

%if _PM
%include "pmdata.asm"
%include "pminit.asm"
%include "pmentry.asm"


	usesection lDEBUG_CODE

resetmode_and_test_d_b_bit:
%if _PM
	call resetmode
%endif

		; Test if bx is a 32-bit selector
		;  (as opposed to a 16-bit selector or a segment)
		;
		; INP:	bx = selector (PM) or segment (86M)
		; OUT:	NZ = 32-bit
		;	ZR = 16-bit (always if 86M)
		;	NC
		; REM:	This checks whether a code segment's D bit or
		;	 a stack segment's B bit is set. This operation
		;	 is not meaningful otherwise.
test_d_b_bit: section_of_function
_386	call ispm
_386	jz .pm				; 386 and PM, check selector -->
		; not PM or no 386
.ZR:
	cmp al, al			; ZR, NC
	retn
.pm:
[cpu 386]
	push eax
	xor eax, eax			; use rights = 0 if inaccessible
	lar eax, ebx			; access rights
		; eax is unchanged if the access rights are inaccessible
		;  (and NZ is set in that case)
	test eax, 400000h		; test bit (NC)
	pop eax
	retn
__CPU__


		; Test if selector in bx has a limit beyond 64 KiB - 1 B
		;
		; INP:	bx = selector (PM) or segment (86M)
		; OUT:	NZ = limit above 64 KiB - 1 B
		;	ZR = limit below 64 KiB (always if 86M)
		;	NC
test_high_limit: section_of_function
_386	call ispm
_386	jz .pm				; 386 and PM, check selector -->
		; not PM or no 386
	jmp test_d_b_bit.ZR

.pm:
[cpu 386]
	push eax
	xor eax, eax			; use limit = 0 if inaccessible
	lsl eax, ebx			; segment limit
		; eax is unchanged if the segment limit is inaccessible
		;  (and NZ is set in that case)
	test eax, 0FFFF_0000h		; (NC) ZR if low limit, else NZ
	pop eax
	retn
__CPU__

subcpureset	; subcpu used in pminit.asm
%endif	; _PM

%if _NOEXTENDER
		; When we support non-extended DPMI, some calls to Int21
		; are (extended) Int21 calls and some are (not extended)
		; calls down to the real mode Int21. doscall is a macro
		; that will always call the non-extended Int21.

		; Execute a non-extended DOS call
_doscall: section_of_function
	pushf
	call ispm
	jnz .rm
subcpu 286
		; Execute a non-extended DOS call from PM
	popf
.pm:
	push word [ss:pspdbg]
	push 21h
	call intcall
	retn
subcpureset
.rm:
	popf
	jmp _int21
%endif


%if _DUALCODE
%push
%assign %$counter 0

%rep 2

%if %$counter == 0
%define %$currentindex 0
%define %$currentname lDEBUG_CODE
%define %$othername lDEBUG_CODE2
%define %$othersegvar code2_seg
%define %$otherselvar code2_sel
%else
%define %$currentindex 1
%define %$currentname lDEBUG_CODE2
%define %$othername lDEBUG_CODE
%define %$othersegvar code_seg
%define %$otherselvar code_sel
%endif

	usesection %$currentname
%if _PM
%$currentname %+ _to_ %+ %$currentname %+ _dualcall_helper:
	push ax		; placeholder
	push ax		; chain
	lframe 0
	lpar word,	offset_segment
	lpar word,	placeholder
	lpar word,	chain
	lenter
	pushf
	push ax
	push bx
	mov bx, word [bp + ?offset_segment]
%if _DEBUG
	mov word [bp + ?placeholder], bx
	inc bx
	inc bx
	mov ax, word [cs:bx]
%else
	mov ax, word [cs:bx]
	inc bx
	inc bx
	mov word [bp + ?placeholder], bx
%endif
	mov word [bp + ?chain], ax
	mov word [bp + ?offset_segment], %$$currentindex
	pop bx
	pop ax
	popf
	lleave
	retn

%$currentname %+ _to_ %+ %$othername %+ _dualcall_helper:
	push ax		; placeholder
	push ax		; chain
	push ax		; chain
	lframe 0
	lpar word,	offset_segment
	lpar word,	placeholder
	lpar dword,	chain
	lenter
	pushf
	push ax
	push bx
	mov bx, word [bp + ?offset_segment]
%if _DEBUG
	mov word [bp + ?placeholder], bx
	inc bx
	inc bx
	mov ax, word [cs:bx]
%else
	mov ax, word [cs:bx]
	inc bx
	inc bx
	mov word [bp + ?placeholder], bx
%endif
	mov word [bp + ?chain], ax
	mov ax, word [ss:%$$otherselvar]
	call _CURRENT_SECTION %+ _ispm
	jz .pm1
	mov ax, word [ss:%$$othersegvar]
.pm1:
	mov word [bp + ?chain + 2], ax
	mov word [bp + ?offset_segment], %$$currentindex
	pop bx
	pop ax
	popf
	lleave
	retf


%$currentname %+ _dualret_helper:
	lframe near
	lpar word,	index_segment
	lpar word,	offset
	lpar_return
	lenter
	pushf
	push ax

	mov ax, word [ss:code_sel]
	call _CURRENT_SECTION %+ _ispm
	jz .pm1
	mov ax, word [ss:code_seg]
.pm1:
	cmp word [bp + ?index_segment], 1
	jb .got
	mov ax, word [ss:code2_sel]
	call _CURRENT_SECTION %+ _ispm
	jz .pm2
	mov ax, word [ss:code2_seg]
.pm2:
.got:
	mov word [bp + ?index_segment], ax
	pop ax
	popf
	lleave
	lret
%endif


%ifn _DUALCODE && ! _PM && _DUALCODENEARDUAL
%$currentname %+ _to_ %+ %$othername %+ _nearcall_helper:
	push ax		; return_offset
	push ax		; placeholder
	push ax		; chain
	push ax		; chain
	lframe 0
	lpar word,	offset_segment
	lpar word,	return_offset	; far return
	lpar word,	placeholder	; near return
	lpar dword,	chain		; far target address
	lenter
	pushf
	push ax
	push bx
	mov bx, word [bp + ?offset_segment]
%if _DEBUG
	mov word [bp + ?return_offset], bx
	inc bx
	inc bx
	mov ax, word [cs:bx]
%else
	mov ax, word [cs:bx]
	inc bx
	inc bx
	mov word [bp + ?return_offset], bx
%endif
	mov word [bp + ?chain], ax

%if _PM
	mov ax, word [ss:%$$otherselvar]
	call _CURRENT_SECTION %+ _ispm
	jz .pm1
%endif
	mov ax, word [ss:%$$othersegvar]
.pm1:
	mov word [bp + ?chain + 2], ax
%if _PM
	mov word [bp + ?offset_segment], %$$currentindex
%else
	mov word [bp + ?offset_segment], cs
%endif
	mov word [bp + ?placeholder], %$$othername %+ _retf_from_dual
	pop bx
	pop ax
	popf
	lleave
	retf

%$currentname %+ _retf_from_dual:
	dualreturn
	retf
%endif


%assign %$counter %$counter + 1
%endrep
%pop
%endif


	usesection lDEBUG_CODE

%if _SYMBOLIC
%include "symbols.asm"
%else

%if _PM


	usesection SECTION_OF_ %+ selector_to_segment

	; For branches other than symbolic, here's selector_to_segment
	;  (as used by the puts in lineio.asm). Picked from symsnip
	;  binsrch.asm at revision 9c232415d568.
		; INP:	word [ss:sp] = selector to access
		; OUT:	word [ss:sp] = segment value to use for access
		; CHG:	-
dualfunction
selector_to_segment: section_of_function
	lframe dualdistance
	lpar word,	in_selector_out_segment
	lpar_return
	lenter

	call _CURRENT_SECTION %+ _ispm
				; is it PM ?
	jnz .ret		; no, 86M --> (selector == segment)

subcpu 286
	push ax
	push bx
	push cx
	push dx

	mov bx, word [bp + ?in_selector_out_segment]
	mov ax, 6
	int 31h			; get segment base to cx:dx
	shr dx, 4
	shl cx, 12
	or dx, cx
	mov word [bp + ?in_selector_out_segment], dx

	pop dx
	pop cx
	pop bx
	pop ax
subcpureset

.ret:
	lleave
	dualreturn
	lret
%endif

%endif


	; support functions for symbols.asm
	usesection lDEBUG_CODE
%if _PM
dualfunction
push_cxdx_or_edx: section_of_function
	lframe dualdistance
	lpar dword, return
	lpar_return
	lenter
_no386	push cx
	_386_o32
	push dx
	pop word [bp + ?return]
	pop word [bp + ?return + 2]
	lleave
	dualreturn
	lret
%endif

		; INP:	ds:dx -> message
		;	cx = length
		; CHG:	-
		; STT:	ds, es don't care
disp_message_length_cx: section_of_function
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	 push ds
	 pop es			; es:dx -> message, cx = length
	 push ss
	 pop ds			; ds = ss (required for puts)
	call puts
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	retn

		; INP:	ds:dx -> message, ASCIZ
		; CHG:	-
		; STT:	ds, es don't care
disp_message: section_of_function
	push es
	 push ds
	 pop es			; es:dx -> message
	call putsz		; (sets up ds = ss internally)
	pop es
	retn

		; INP:	al = character to display
		; CHG:	-
		; STT:	ds, es don't care
disp_al: equ putc


		; Display number in ax hexadecimal, always 4 digits
		;
		; INP:	ax = number
		; OUT:	displayed using disp_al
		; CHG:	none
disp_ax_hex: section_of_function
	xchg al, ah
	nearcall disp_al_hex
	xchg al, ah
disp_al_hex: section_of_function
	push cx
	mov cl, 4
	rol al, cl
	nearcall disp_al_nybble_hex
	rol al, cl
	pop cx
disp_al_nybble_hex: section_of_function
	push ax
	and al, 0Fh
	add al, '0'
	cmp al, '9'
	jbe @F
	add al, -'9' -1 +'A'
@@:
	nearcall disp_al
	pop ax
	retn


		; Display number in ax decimal
		;
		; INP:	ax = number
		; OUT:	displayed using disp_al
		; CHG:	none
disp_ax_dec: section_of_function			; ax (no leading zeros)
		push bx
		xor bx, bx
.pushax:
		push dx
		push ax
		or bl, bl
		jz .nobl
		sub bl, 5
		neg bl
.nobl:
		push cx
		mov cx, 10000
		call .divide_out
		mov cx, 1000
		call .divide_out
		mov cx, 100
		call .divide_out
		mov cl, 10
		call .divide_out
							; (Divisor 1 is useless)
		add al, '0'
		nearcall disp_al
		pop cx
		pop ax
		pop dx
		pop bx					; Caller's register
		retn


		; INP:	ax = number
		;	cx = divisor
		; OUT:	ax = remainder of operation
		;	result displayed
.divide_out:
		push dx
		xor dx, dx
		div cx				; 0:ax / cx
		push dx				; remainder
		dec bl
		jnz .nobl2
		or bh, 1
.nobl2:
		or bh, al
		jz .leadingzero
		add al, '0'
		nearcall disp_al			; display result
 .leadingzero:
		pop ax				; remainder
		pop dx
		retn


%include "cc.asm"


%if _BOOTLDR
 %include "boot.asm"
%endif


%include "bb.asm"


	usesection lDEBUG_CODE

uppercase: section_of_function
	cmp al, 'a'
	jb .ret
	cmp al, 'z'
	ja .ret
	and al, TOUPPER
.ret:
	retn


errorj4:
	jmp error


%include "ee.asm"


%include "ff.asm"


%include "hh.asm"


%include "iioo.asm"


	usesection lDEBUG_CODE

%if _PM

%if _DUALCODE
 %assign REPEAT 2
%else
 %assign REPEAT 1
%endif

	usesection lDEBUG_CODE
%rep REPEAT
		; OUT:	NC
		;	ZR if in protected mode
		;	NZ otherwise
		; STT:	-
		;	([internalflags] & nodosloaded, [internalflags] & protectedmode set up)
_CURRENT_SECTION %+ _ispm:
	push ax
%if protectedmode & ~0FF00h
 %error Internal flags re-ordered, adjust code here
%endif
	mov al, byte [ss:internalflags+1]	; get flag byte
	and al, protectedmode>>8		; separate PM flag
	xor al, protectedmode>>8		; ZR if in PM (NC)
	pop ax
	retn

	usesection lDEBUG_CODE2
%endrep
	usesection lDEBUG_CODE
ispm equ _CURRENT_SECTION %+ _ispm
check_section_of ispm

%endif


setpspdbg:
%if _PM
	mov bx, word [pspdbg]
%else
	mov bx, ss		; = word [pspdbg] (if _PM=0 or in 86M)
%endif

setpsp:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .ret		; no PSPs -->
%endif

%if _USESDA
	cmp word [pSDA+0], byte -1
	je .int21

.86m:
	push ds
	push si
	mov si, pSDA + so16aSegSel
	call update_dosdata_segment
	lds si, [si - so16aSegSel]
	mov word [si+10h], bx	; set PSP segment
	pop si
	pop ds
	retn

.int21:
%endif
	mov ah, 50h
 %if _PM
	call ispm
	jnz .int21_86m
  %if _NOEXTENDER
	jmp _doscall.pm		; insure non-extended (set to bx = PSP segment)
  %else
	mov ax, 0002h
	int 31h			; segment to selector
	xchg bx, ax		; bx = selector
	mov ah, 50h		; reset to function number
  %endif
.int21_86m:
 %endif
	jmp _int21


getpsp:
%if _BOOTLDR
	xor bx, bx		; = placeholder value if no PSPs
	testopt [internalflags], nodosloaded
	jnz .ret		; no PSPs -->
%endif
%if _USESDA
	cmp word [pSDA+0], byte -1
	je .int21
	push ds
	push si
	mov si, pSDA + so16aSegSel
	call update_dosdata_segment
	lds si, [si - so16aSegSel]
	mov bx, word [si + 10h]	; bx = PSP segment
	pop si
	pop ds
	retn

.int21:
%endif
	mov ah, 51h
 %if _PM
	call ispm
	jnz .int21_86m
  %if _NOEXTENDER
	jmp _doscall.pm		; insure non-extended (bx = PSP segment)
  %else
	call _int21		; get PSP as a selector
	push bx
	dualcall selector_to_segment
	pop bx			; bx = PSP segment
	retn
  %endif
.int21_86m:
 %endif
	jmp _int21		; in 86 Mode call DOS the normal way


dual2function
_doscall_return_es: section_of_function
_doscall_return_es_parameter_es_ds: section_of_function
	lframe dual2distance
	lpar word, es_ds_value
	lpar_return
%if _PM
	lvar word, int_number
	lenter
	mov word [bp + ?int_number], 21h
	pushf
	call ispm
	jnz .rm
.pm:
	popf
	push word [bp + ?es_ds_value]
	push word [bp + ?es_ds_value]
	push word [bp + ?int_number]
	push word [bp + ?frame_bp]
	call intcall_return_parameter_es_parameter_ds
	pop word [bp + ?es_ds_value]		; discard returned ds
	pop word [bp + ?es_ds_value]		; get es
	jmp .ret
.rm:
	popf
%else
	lenter
%endif
	 push es
	 push ds
	mov ds, word [bp + ?es_ds_value]
	mov es, word [bp + ?es_ds_value]
	int 21h
	mov word [bp + ?es_ds_value], es
	 pop ds
	 pop es
.ret:
	lleave
	dual2return
	lret


		; Execute real Int21 instruction. If this is in PM it might get extended.
_int21:
%if _BOOTLDR
	pushf
	testopt [internalflags], nodosloaded
	jnz .reterr		; no Int21 --> (throw?)
	popf
%endif
	int 21h
setpsp.ret: equ $
getpsp.ret: equ $
	retn
%if _BOOTLDR
.reterr:
	popf
	mov ax, 1
	stc
	retn
%endif


%if _PM
intcall_return_parameter_es_parameter_ds:
	lframe near
	lpar word, es_value
	lpar word, ds_value
	lpar_return
	lpar word, int_number
	lpar word, bp_value
	lvar 32h, 86m_call_struc
	lenter
	push es
	mov word [bp + ?86m_call_struc +00h], di	; edi
	mov word [bp + ?86m_call_struc +04h], si	; esi
	mov word [bp + ?86m_call_struc +10h], bx	; ebx
	mov word [bp + ?86m_call_struc +14h], dx	; edx
	mov word [bp + ?86m_call_struc +18h], cx	; ecx
	mov word [bp + ?86m_call_struc +1Ch], ax	; eax
	mov ax, word [bp + ?bp_value]
	mov word [bp + ?86m_call_struc +08h], ax	; bp
	mov al, 0					; (preserve flags!)
	lahf
	xchg al, ah
	mov word [bp + ?86m_call_struc +20h], ax	; flags
	xor ax, ax
	mov word [bp + ?86m_call_struc +0Ch + 2], ax
	mov word [bp + ?86m_call_struc +0Ch], ax
	mov word [bp + ?86m_call_struc +2Eh], ax	; sp
	mov word [bp + ?86m_call_struc +30h], ax	; ss
	mov ax, word [bp + ?es_value]			; usually [pspdbg]
	mov word [bp + ?86m_call_struc +22h], ax	; es
	mov ax, word [bp + ?ds_value]			; usually [pspdbg]
	mov word [bp + ?86m_call_struc +24h], ax	; ds
	push ss
	pop es				; => stack
	lea di, [bp + ?86m_call_struc]	; -> 86-Mode call structure
_386	movzx edi, di			; (previously checked b[dpmi32] here)
	mov bx, word [bp + ?int_number]			; int#
	xor cx, cx
	mov ax, 0300h
	int 31h
	mov ah, byte [bp + ?86m_call_struc +20h]	; flags
	sahf
	mov di, word [bp + ?86m_call_struc +00h]	; edi
	mov si, word [bp + ?86m_call_struc +04h]	; esi
	mov bx, word [bp + ?86m_call_struc +10h]	; ebx
	mov dx, word [bp + ?86m_call_struc +14h]	; edx
	mov cx, word [bp + ?86m_call_struc +18h]	; ecx
	mov ax, word [bp + ?86m_call_struc +1Ch]	; eax
	push word [bp + ?86m_call_struc +22h]		; return es value
	pop word [bp + ?es_value]			;  in the parameter
	push word [bp + ?86m_call_struc +24h]		; return ds value
	pop word [bp + ?ds_value]			;  in the parameter
	pop es
	lleave
	lret

intcall:
	lframe near
	lpar word, es_ds_value
	lpar word, int_number
	lenter
	push word [bp + ?es_ds_value]			; es
	push word [bp + ?es_ds_value]			; ds
	push word [bp + ?int_number]			; int number
	push word [bp + ?frame_bp]			; bp
	call intcall_return_parameter_es_parameter_ds
		; (discard returned parameters ?es_value, ?ds_value, done by lleave)
	lleave , forcerestoresp
	lret


call_int2D:
	call ispm
	jnz short .rm
subcpu 286
	push word [ss:pspdbg]	; es ds value. generally unused
	push 2Dh		; interrupt 2Dh
	call intcall		; call it
	retn
subcpureset
.rm:
	int 2Dh			; directly call int 2Dh
	retn


		; Called in PM only, ds unknown.
		;
		; INP:	-
		; OUT:	CY if no DOS extender available ("MS-DOS" on Int2F.168A)
		;	NC if DOS extender available
		; CHG:	-
isextenderavailable:
subcpu 286
	push ds
	push es
	pusha
	push ss
	pop ds
	mov si, msg.msdos
_386	movzx esi, si
	mov ax, 168Ah
	int 2Fh
	cmp al, 1			; CY if al is zero
	cmc				; NC if al is zero, CY else
	popa
	pop es
	pop ds
	retn
subcpureset

nodosextinst:
	push ss
	pop ds
	mov dx, nodosext
	jmp putsz
%endif



%include "ll.asm"


%include "mm.asm"


	usesection lDEBUG_CODE

		; N command - change the name of the program being debugged.
nn:
	push ss
	pop es
%if _BOOTLDR
	testopt [ss:internalflags], nodosloaded
	jz @F
	mov dx, msg.nobootsupp
	jmp putsz
@@:
%endif
	mov	di, DTA		; destination address

		; Copy and canonicalize file name.
nn1:
	cmp di, N_BUFFER_END
	jae .toolong
	call ifsep		; check for separators CR, blank, tab, comma, ;, =
	je nn3			; if end of file name
	cmp al, byte [ss:swch1]
		; The use of ss here appears to be intended to
		;  allow loading from ds different from the
		;  data entry and PSP segment, However, the
		;  subsequent copy of the command tail around
		;  nn4 does not participate in this scheme.
		; So if this is used make sure to adjust that.
	je nn3			; if '/' (and '/' is the switch character)
	call uppercase
	stosb
	lodsb
	jmp short nn1		; back for more

.toolong:
nn4.toolong:
	push ss
	pop ds
	mov dx, msg.n_toolongname
	call putsz
	mov di, N_BUFFER_END - 3
	mov al, 0		; truncate the name
	stosb
	mov byte [fileext], al	; invalid / none
	mov word [execblk.cmdline], di
	mov ax, 13 << 8		; 0 in low byte (tail length), CR in high byte
	stosw
	retn


nn3:
	push ss
	pop ds
	mov al, 0		; null terminate the file name string
	stosb
	mov word [execblk.cmdline], di
				; save start of command tail

%if _DEBUG4
	push dx
	mov dx, DTA
	call d4disp_msg
	mov dx, crlf
	call d4disp_msg
	pop dx
%endif
		; Determine file extension
	cmp di, DTA+1
	je nn3d			; if no file name at all
	cmp di, DTA+5
	jb nn3c			; if no extension (name too short)
	mov al, EXT_HEX
	cmp word [di-5], ".H"
	jne nn3a		; if not .HEX
	cmp word [di-3], "EX"
	je nn3d			; if .HEX
nn3a:
	mov al, EXT_EXE
	cmp word [di-5], ".E"
	jne nn3b		; if not .EXE
	cmp word [di-3], "XE"
	je nn3d			; if .EXE
nn3b:
	mov al, EXT_COM
	cmp word [di-5], ".C"
	jne nn3c		; if not .COM
	cmp word [di-3], "OM"
	je nn3d			; if .COM
nn3c:
	mov al, EXT_OTHER
nn3d:
	mov byte [fileext], al

		; Finish the N command
	push di
	mov di, line_out
	dec si
nn4:
	lodsb			; copy the remainder to line_out
	stosb
	call iseol?.notsemicolon
	jne nn4

	call InDos
	jz .fcb_setup
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz @F
%endif
	and word [reg_eax], 0
@@:
	jmp .fcb_none

.fcb_setup:
		; Set up FCBs.
	mov si, line_out
	mov di, 5Ch
	call nn6		; do first FCB
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz @F
%endif
	mov byte [reg_eax], al
@@:
	mov di, 6Ch
	call nn6		; second FCB
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz @F
%endif
	mov byte [reg_eax+1], al
@@:
.fcb_none:

		; Copy command tail.
	mov si, line_out
	pop di
	cmp di, N_BUFFER_END - 2
	jae .toolong
	push di
	inc di
nn5:
	lodsb
	stosb
	cmp di, N_BUFFER_END
	jae .toolong
	call iseol?.notsemicolon
	jne nn5			; if not end of string
	; test al, al
	; jnz @B
	mov byte [di - 1], 13	; (just overwrite this unconditionally)
; @@:			; jump destination from above if al == 13
@@:			; jump destination from .toolong
	push di
	mov cx, di
	sub cx, N_BUFFER_END
	neg cx
	xor ax, ax
	rep stosb
	pop di
	pop ax			; recover old DI
	xchg ax, di
	sub ax, di		; compute length of tail
	dec ax
	dec ax
	stosb
%if _DEBUG4
	mov dx, DTA
	call d4disp_msg
	mov dx, crlf
	call d4disp_msg
%endif
	retn			; done

.toolong:
	mov dx, msg.n_toolongtail
	call putsz
	mov di, N_BUFFER_END
	mov byte [di - 1], 13
	jmp @B


		; Subroutine to process an FCB.
		;
		; INP:	di -> FCB
		;	si -> input
nn6:
	lodsb
	call iseol?.notsemicolon
	je nn7			; if end
	call ifsep
	je nn6			; if separator (other than CR)
	cmp al, byte [switchar]
	je nn10			; if switch character
nn7:
	dec si
	mov ax, 2901h		; parse filename
	doscall
	push ax			; save AL
nn8:
	lodsb			; skip till separator
	call ifsep
	je nn9			; if separator character (including CR)
	cmp al, byte [swch1]
	jne nn8			; if not switchar (sort of)
nn9:
	dec si
	pop ax			; recover AL
	cmp al, 1
	jne nn9a		; if not 1
	dec ax
nn9a:
	retn

		; Handle a switch (differently).
nn10:	lodsb
	call iseol?.notsemicolon
	je nn7			; if end of string
	call ifsep
	je nn10			; if another separator (other than CR)
	mov al, 0
	stosb
	dec si
	lodsb
	cmp al, 'a'
	jb nn11			; if not a lower case letter
	cmp al, 'z'
	ja nn11
	and al, TOUPPER		; convert to upper case
nn11:	stosb
	mov ax, 32<<8|32
	stosw
	stosw
	stosw
	stosw
	stosw
	xor ax, ax
	stosw
	stosw
	stosw
	stosw
	retn			; return with al = 0


		; Compare character with separators
		;
		; INP:	al = character
		; OUT:	ZR if al is CR, NUL, blank, tab, comma, semicolon, or equal sign
		;	NZ else
		; REM:	This is only used for parsing FCBs.
ifsep: section_of_function
	call iseol?		; semicolon or CR or NUL
	je .return
	cmp al, 32
	je .return
	cmp al, 9
	je .return
	cmp al, ','
	je .return
	cmp al, '='
.return:
	retn


		; Ensure segment in bx is writeable
		;
		; INP:	bx = selector/segment
		; OUT:	NC if in 86M, bx unchanged
		;	NC if in PM and bx not a code segment, bx unchanged
		;	NC if in PM and was a code segment,
		;	 bx = word [scratchsel], set up to mirror INP:bx selector
		;	CY if in PM and a failure occurred, segment not writeable
		; CHG:	bx
		; STT:	ss = debugger data selector
%if _PM
verifysegm_or_error:
	push bx
	call verifysegm
	jc .ro
	add sp, 2		; (discard bx on stack)
	retn

.ro:
	push ss
	pop es
	call ee0a
	mov di, msg.readonly_verifysegm.selector
	pop ax			; get original selector
	call hexword
	mov dx, msg.readonly_verifysegm
	call putsz_error
	jmp cmd3


verifysegm:
	call ispm
	jnz .rm			; (NC)
	push ax
	push es
	_386_o32	; push edi
	push di
	push bp
	mov bp, sp
	sub sp, 8
	 push ss
	 pop es
	mov di, sp
_386	movzx edi, di
	mov ax, 000Bh		; get descriptor
	int 31h
	jc @F
	test byte [di+5], 8	; code segment ?
	jz @F			; (NC) no -->
	and byte [di+5], 0F3h	; reset CODE+conforming attr
	or byte [di+5], 2	; set writable
	mov bx, word [scratchsel]
	mov ax, 000Ch
	int 31h
@@:
	mov sp, bp
	pop bp
	_386_o32	; pop edi
	pop di
	pop es
	pop ax
.rm:
.retn:
	retn

subcpu 286
		; INP:	dx = 86 Mode segment to access
		; OUT:	bx = scratch selector, addressing that segment,
		;	 limit set to 64 KiB (allow all 16-bit accesses)
		; CHG:	-
		; STT:	ss = lDEBUG_DATA_ENTRY selector, in PM
setrmsegm: section_of_function
	mov bx, word [ss:scratchsel]
	call setrmaddr

		; INP:	bx = selector
		; OUT:	limit set to 0FFFFh
		; STT:	in PM
setrmlimit:
	push ax
	push cx
	push dx
	xor cx, cx
; _386	dec cx		; limit 0FFFF_FFFFh on 386+
		; We don't want that here. All users expect a 64 KiB segment.
	mov dx, -1	; limit 0FFFFh on 286
	mov ax, 8
	int 31h		; set limit
	pop dx		; restore base segment
	pop cx
	pop ax
	retn

setrmaddr:		;<--- set selector in BX to segment address in DX
.:
	push ax
	push cx
	push dx
	mov cx, dx
	shl dx, 4
	shr cx, 12	; cx:dx = base address
	mov ax, 7
	int 31h		; set base
	pop dx
	pop cx
	pop ax
	retn

subcpureset
%endif

		; Read a byte relative to cs:eip
		;
		; INP:	reg_cs, reg_eip
		;	cx = (signed) eip adjustment
		; OUT:	al = byte at that address
		;	(e)bx = new offset (eip+adjustment)
		; CHG:	-
getcseipbyte:
	push es
%if _PM
	mov bx, word [reg_cs]
	mov es, bx
	call test_d_b_bit
	jz .16
[cpu 386]
	mov ebx, dword [reg_eip]
	push edx
	movsx edx, cx
	add ebx, edx
..@getcseipbyte_fault_skip_6_near_call:
	mov al, byte [es:ebx]
	pop edx
	pop es
	retn
__CPU__
.16:
%else
	mov es, word [reg_cs]
%endif
	mov bx, word [reg_eip]
	add bx, cx
..@getcseipbyte_fault_skip_2_near_call:
	mov al, byte [es:bx]
	pop es
	retn

		; Write to a byte relative to cs:eip
		;
		; INP:	reg_cs, reg_eip
		;	cx = (signed) eip adjustment
		;	al = source byte to write
		; OUT:	NC if apparently written
		;	CY if failed to get a writeable selector
		; CHG:	(e)bx
setcseipbyte:
	push es
%if _PM
	mov bx, word [reg_cs]
	call verifysegm
	jc .ret
	mov es, bx
	call test_d_b_bit
	jz .16
[cpu 386]
	mov ebx, dword [reg_eip]
	push edx
	movsx edx, cx
..@setcseipbyte_fault_skip_6_near_call:
	mov byte [es:ebx+edx],al
	pop edx
	pop es
	clc
	retn
__CPU__
.16:
%else
	mov es, word [reg_cs]
%endif
	mov bx, word [reg_eip]
	add bx, cx
..@setcseipbyte_fault_skip_2_near_call:
	mov byte [es:bx], al
	clc
.ret:
	pop es
	retn

		; Exchange byte with memory
		;
		; INP:	bx:(e)dx-> destination byte
		;	al = source byte
		; REM:	Determines whether to use edx by the
		;	 segment limit of the selector.
		;	 (Uses in run.asm always pass a segmented
		;	 address obtained from getsegmented. This
		;	 will have edxh = 0 always so it doesn't
		;	 matter whether we use edx or dx.)
		; OUT:	CY if failed due to segment not writable
		;	NC if successful,
		;	 al = previous value of destination byte
		; CHG:	ah
writemem:
%if _DEBUG1
	push dx
	push ax

	call getlinear_high_limit.do_not_use_test	; NB do NOT resetmode
	jc @F			; already an error ?  then return --> (CY)
	push bx
	push cx
	mov bx, test_records_Writemem
	call handle_test_case_multiple_16
				; check whether this should testcase the error
				; CY to indicate error from this call
	pop cx
	pop bx
@@:
	pop ax
	pop dx
	jnc .do_not_use_test
	retn			; return CY here

%endif
.do_not_use_test:

	mov ah, al
%if _PM
	call ispm
	jnz .16			; (NC from ispm) -->
	call verifysegm		; make bx a writeable segment
	jc .ret
_386_PM	call test_high_limit	; 32-bit segment ?
	jz .16			; (NC from test_d_b_bit) -->
[cpu 386]
	push ds
	mov ds, bx
..@writemem_fault_skip_2_near_call_a:
	xchg al, byte [edx]
..@writemem_fault_skip_2_near_call_b:
	cmp ah, byte [edx]
	pop ds
__CPU__
	jmp short .cmp
.16:
%endif
	push ds
	mov ds, bx
	push bx
	mov bx, dx
..@writemem_fault_skip_4_near_call_a:
	xchg al, byte [bx]
..@writemem_fault_skip_4_near_call_b:
	cmp ah, byte [bx]
	pop bx
	pop ds
.cmp:
	je .ret			; (NC)
	stc			; Failed to compare (i.e. memory wasn't our byte after writing).
				; This check catches ROM that will silently fail to write.
.ret:
	retn


		; Read byte from memory
		;
		; INP:	bx:(e)dx-> destination byte
		; REM:	Determines whether to use edx by the
		;	 segment limit of the selector.
		;	 (Uses in run.asm always pass a segmented
		;	 address obtained from getsegmented. This
		;	 will have edxh = 0 always so it doesn't
		;	 matter whether we use edx or dx.)
		; OUT:	 al = value of byte read
readmem:
%if _DEBUG1
	push dx
	push ax

	call getlinear_high_limit.do_not_use_test	; NB do NOT resetmode
	jc @F			; already an error ?  then return --> (CY)
	push bx
	push cx
	mov bx, test_records_Readmem
	call handle_test_case_multiple_16
				; check whether this should testcase the error
				; CY to indicate error from this call
	pop cx
	pop bx
@@:
	pop ax
	pop dx
	jnc .do_not_use_test
	mov al, byte [test_readmem_value]
				; return a most likely wrong value
	retn

%endif
.do_not_use_test:

%if _PM
_386_PM	call test_high_limit	; 32-bit segment ?
	jz .16
[cpu 386]
	push ds
	mov ds, bx
..@readmem_fault_skip_2_near_call:
	mov al, byte [edx]
	pop ds
	retn
__CPU__
.16:
%endif
	push ds
	push bx
	mov ds, bx
	mov bx, dx
..@readmem_fault_skip_4_near_call:
	mov al, byte [bx]
	pop bx
	pop ds
	retn


		; Q command - quit.
qq:
	call guard_re
	xor cx, cx	; no qq mode selected
	dec si
.loop:
	lodsb
	call uppercase
	cmp al, 'A'
	je qq_a
	mov ch, qqmode_b; QB mode (breakpoint before terminate)
	cmp al, 'B'
	je .otherletter
	mov ch, qqmode_c; QC mode (terminate device in a container MCB)
	cmp al, 'C'
	je .otherletter
	mov ch, qqmode_d; QD mode (terminate device in initialisation)
	cmp al, 'D'
	je .otherletter
	mov byte [qq_mode], cl
	jmp qq_default

.otherletter:
	or cl, ch
	jmp .loop

	usesection lDEBUG_DATA_ENTRY
qq_mode:	db 0
qqmode_b:	equ 1
qqmode_c:	equ 2
qqmode_d:	equ 4
	usesection lDEBUG_CODE

qq_a:
	lodsb
	call chkeol
	call terminate_attached_process
	mov bx, msg.qq_a_unterminated
	jz .attached_unterminated
	mov bx, msg.qq_a_terminated
.attached_unterminated:
	call putrunint
	mov dx, bx
	jmp putsz


qq_default:
	call chkeol
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jz .nondevice
	test cl, qqmode_c | qqmode_d
	jnz .deviceselected
	mov dx, msg.qq_device_none_selected
	jmp putsz

.deviceselected:
.nondevice:
%endif
 %if _BOOTLDR
		; Test whether we are in non-DOS mode, and were
		; currently entered in protected mode. Since
		; this will make the entire operation fail,
		; it has to be checked for before modifying
		; or releasing any of the resources.
		; (Does this ever occur? No?)
	testopt [internalflags], nodosloaded
	jz .notpmnodos
%if _PM
	call ispm
  %if _TSR	; same message, reuse code
	jz .cannotpmquit
  %else
	jnz .notpmnodos_nodos
	mov dx, msg.cannotpmquit
	jmp putsz
  %endif
%endif
.notpmnodos_nodos:
	call bootgetmemorysize		; dx => behind usable memory
	mov ax, word [ boot_new_memsizekib ]
	mov cl, 6
	shl ax, cl
	cmp ax, dx			; same?
	je @F
	mov dx, msg.cannotbootquit_memsizes
	jmp .putsz
%if !_TSR || !_PM
	.putsz equ putsz
%endif

@@:
.notpmnodos:
 %endif
%if _PM
 %if _TSR
		; Test whether we are in TSR mode, and were
		; currently entered in protected mode. Since
		; this will make the entire operation fail,
		; it has to be checked for before modifying
		; or releasing any of the resources.
	testopt [internalflags], tsrmode
	jz .notpmtsr
	call ispm
	jnz .notpmtsr

; This isn't yet implemented. Broken down:
; * Uses terminate_attached_process which returns in real mode.
;  * Exception vectors are implicitly restored/discarded by that.
; * (RM) Interrupt vectors are currently restored in real mode. Unnecessary.
; * The VDD is un-registered in real mode. Necessary?
; * Normal 21.4C is used to return to the real parent.
;  * We have to discard our DOS process resources. Any DPMI TSR resources?
;  * We must again gain control in debuggee's mode after discarding them.
;  * We must return to the debuggee and seemlessly discard our memory. The
;    stack trick possibly/probably does not work in protected mode.

.cannotpmquit:
	mov dx, msg.cannotpmquit
.putsz:
	jmp putsz

.notpmtsr:
 %endif

 %if (nohook2F)&~0FF00h
  %fatal Internal flags re-ordered, adjust code here
 %endif
	mov ax, [internalflags]
 	mov al, __TEST_IMM8
	xchg al, [dpmidisable]		 	; disable DPMI hook
						; (SMC in section lDEBUG_DATA_ENTRY)
	push ax
	setopt [internalflags], nohook2F	; avoid a new hook while terminating
%endif


qq_restore_interrupts_simulated:
	xor bp, bp
%if _CATCHINT2D
.2D:
	testopt [internalflags4], dif4_int_2D_hooked
	jz .noint2D

	mov al, 2Dh		; interrupt number
	mov si, int2D		; -> IISP entry header
	mov dx, opt4_int_2D_force >> 16
	call UnhookInterruptForceSim
				; try unhooking it
	push ss
	pop es
	jnc .got2D

.not2D:
	mov word [msg.serial_cannot_unhook.int], "2D"
	mov dx, msg.serial_cannot_unhook.nowarn
	call putsz
	inc bp

.got2D:
.noint2D:
%endif


%if _CATCHINT08
.08:
	testopt [internalflags4], dif4_int_08_hooked
	jz .noint08

	mov al, 08h		; interrupt number
	mov si, intr8		; -> IISP entry header
	mov dx, opt4_int_08_force >> 16
	call UnhookInterruptForceSim
				; try unhooking it
	push ss
	pop es
	jnc .got08

.not08:
	mov word [msg.serial_cannot_unhook.int], "08"
	mov dx, msg.serial_cannot_unhook.nowarn
	call putsz
	inc bp

.got08:
.noint08:
%endif


.serial:
	testopt [internalflags4], dif4_int_serial_hooked
	jz .done_serial
	mov si, serial_interrupt_handler
	mov al, byte [serial_installed_intnum]
	mov dx, opt4_int_serial_force >> 16
	call UnhookInterruptForceSim
	push ss
	pop es
	jnc .done_serial			; if it succeeded -->

	mov di, msg.serial_cannot_unhook.int
	mov al, byte [serial_installed_intnum]
	call hexbyte
	mov dx, msg.serial_cannot_unhook.nowarn
	call putsz
	inc bp

.done_serial:

%if _PM
.2F:
	testopt [internalflags], hooked2F
	jz .noint2F

	mov al, 2Fh		; interrupt number
	mov si, debug2F		; -> IISP entry header
	mov dx, opt4_int_2F_force >> 16
	call UnhookInterruptForceSim
				; try unhooking it
	push ss
	pop es
	jnc .got2F

.not2F:
	mov word [msg.serial_cannot_unhook.int], "2F"
	mov dx, msg.serial_cannot_unhook.nowarn
	call putsz
	inc bp

.got2F:
.noint2F:
%endif


%if CATCHINTAMOUNT && ! (_DEBUG && ! _DEBUG_COND)
 %if _DEBUG
	testopt [internalflags6], dif6_debug_mode
	jnz .skipints
 %endif
		; Simulate to restore interrupt vectors.
	mov si, inttab
	mov di, intforcetab
%if _CATCHINT06 && _DETECT95LX
	mov cx, word [inttab_number_variable]
%else
	mov cx, inttab_number
%endif
	xor dx, dx
.nextintsim:
	lodsb
	xchg ax, bx			; bl = number
	lodsw				; si -> list
	xchg ax, si			; si -> entry, ax -> list
	xchg ax, bx			; al = number, bx -> list
	push di
	mov dh, byte [di]
	call UnhookInterruptForceSim
	pop di
	push ss
	pop es
	jnc @F
	mov di, msg.serial_cannot_unhook.int
	call hexbyte
	mov dx, msg.serial_cannot_unhook.nowarn
	call putsz
	inc bp
@@:
	inc di
	xchg bx, si			; si -> list
	loop .nextintsim
.skipints:
%endif

	mov dx, msg.empty_message
	test bp, bp
	jnz qq_attached_unterminated.common


%if _DEVICE
qq_device_prepare:
	testopt [internalflags6], dif6_device_mode
	jz qq_nondevice

%if _PM
	mov dx, msg.qq_device_pm
	call ispm
	jz @F			; in PM -->
%endif

		; Try quitting early in device init ?
	testopt [qq_mode], qqmode_d
	jz .device_c		; no, must be container quit -->

.device_d:
	mov si, regs
	mov di, device_quittable_regs
	mov cx, words(regs.size)
	repe cmpsw		; can quit to device init ?
	jne .check_device_c
	les di, [device_header_address]
	mov al, -1
	mov cl, 4
	repe scasb		; is next device pointer still -1 ?
	push ss
	pop es
	je qq_device_got	; yes -->

.check_device_c:
		; Cannot quit to device init. Clear the flag
		;  so we know later on that we're trying QC.
	clropt [qq_mode], qqmode_d
	testopt [qq_mode], qqmode_c
				; actually want to try QC ?
	jnz .device_c		; yes -->

	mov dx, msg.qq_device_no_d
@@:
	jmp qq_attached_unterminated.common

.device_c:
	mov ax, 5802h
	int 21h
	mov ah, 0
	push ax			; preserve UMB link
	mov ax, 5803h
	mov bx, 1
	int 21h			; enable UMB link
				;  we want to support the case in which
				;  the first UMCB may have changed. so
				;  instead of searching for it again we
				;  just request the link enabled.

	mov ah, 52h
	int 21h
	mov di, bx
	mov dx, word [es:bx - 2]
	mov cx, 30h
	cmp di, - (30h + 12h)
	ja .no_c

.nulloop:
	mov si, msg.NULblank
	cmpsw			; di += 2, si += 2. compare
	jne .nulnext
.nulcheck:
	push di
	push cx
	mov cl, 3		; 3 more words to go
	repe cmpsw		; match ?
	pop cx
	pop di
	je .nulfound
.nulnext:
	dec di			; di -= 1 so it ends up 1 higher than prior
	loop .nulloop
	jmp .no_c

.nulfound:
	sub di, 3 * 2 + 4 + 2	; (strategy, interrupt, flags are words,
				;  next device pointer is a dword.
				;  additional plus 2 for the cmpsw output.)

		; es:di -> NUL device header
.devloop:
	mov ax, word [device_header_address]
	cmp ax, word [es:di]
	jne .devnext
	mov ax, word [device_header_address + 2]
	cmp ax, word [es:di + 2]
	je .mcb
.devnext:
	inc cx
	js .no_c

	les di, [es:di]
	cmp di, -1
	jne .devloop
	jmp .no_c

.mcb:

	mov word [.device_reference], di
	mov word [.device_reference + 2], es

	and word [.counter], 0
	mov di, ax		; => start of memory allocated to us
	mov cx, word [device_mcb_paragraphs]
				; = amount paragraphs allocated to us
	add di, cx		; => behind memory allocated to us

		; dx => first MCB
.mcbloop:
	mov es, dx
	mov si, dx		; => MCB
	add si, word [es:3]
	inc si			; => next MCB (or behind current MCB)
	cmp byte [es:0], 'M'	; valid MCB ?
	je @F
	cmp byte [es:0], 'Z'
	jne .no_c		; no -->
@@:
	cmp dx, ax		; start of MCB < allocation ?
	jae .mcbnext		; no -->
	cmp si, di		; end of MCB > allocation ?
	jb .mcbnext		; no -->

	cmp word [es:1], 0	; free ?
	je .mcbnext		; do not match -->
	dec ax			; => our (sub) MCB
	cmp dx, ax		; matches (DEVLOAD style) ?
	jne .mcbcontainer	; no -->
	cmp word [es:3], cx	; size matches ?
	jne .mcbcontainer	; no -->
	and word [.container_segment], 0
	jmp .mcbdone		; found a non-container MCB

.mcbnext:
	inc word [.counter]	; safeguard against infinite loop
	jz .no_c
	mov dx, si		; => next MCB
	cmp byte [es:0], 'M'	; prior was 'M' ?
	je .mcbloop		; yes, so loop -->
	jmp .no_c

.mcbcontainer:
	inc ax			; => allocated block (device header)
	cmp word [es:1], 50h	; SD owner system ?
	jae .mcbnext
	cmp word [es:8], "SD"
	jne .mcbnext		; no -->

	push word [es:0]
	pop word [.container_is_z]
	mov word [.container_end], si
	mov word [.container_segment], dx
	inc dx			; => sub-MCB
.submcbloop:
	mov es, dx
	mov si, dx
	add si, word [es:3]
	inc si			; => next sub or MCB (or behind Z MCB)
	cmp dx, ax
	jae .submcbnext
	cmp si, di
	jb .submcbnext

	push ax
	dec ax			; => our (sub) MCB
	cmp dx, ax		; matched start of allocation ?
	pop ax
	jne .submcbnext
	cmp word [es:3], cx	; matches allocation size ?
	jne .submcbnext
	cmp word [es:1], 0	; is not free ?
	je .submcbnext
	jmp .mcbdone		; all yes, found it -->

.submcbnext:
	inc word [.counter]	; safeguard against infinite loop
	jz .no_c
	mov dx, si		; => next sub MCB or after container
	mov si, word [.container_end]
	cmp dx, si		; after container ?
	jb .submcbloop		; no -->
		; This jump could be a jne but generally
		;  we can assume that the container does
		;  not overflow across the 1 MiB limit.
		; And this is more hardened against errors.
	mov dx, si		; insure we use actual container end
	cmp byte [.container_is_z], 'Z'
				; container had a Z ?
	jne .mcbloop		; no -->
		; if here, loop now, dx already updated and
		;  furthermore es does not point at container!
	jmp .no_c

	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
.device_reference:	dd 0
.container_segment:	dw 0
.container_end:		dw 0
.container_is_z:	dw 0
.counter:		dw 0
	usesection lDEBUG_CODE

.mcbdone:

	db __TEST_IMM8		; (skip stc, NC)
.no_c:
	stc
	pop bx
	pushf
	mov ax, 5803h
	int 21h			; restore UMB link
	popf
	push ss
	pop es
	jnc @F
	mov dx, msg.qq_device_no_c
	jmp qq_attached_unterminated.common

@@:

qq_device_got:
qq_nondevice:
%endif


qq_restore_interrupts:
%if _CATCHINT2D
.2D:
	testopt [internalflags4], dif4_int_2D_hooked
	jz .noint2D

	mov al, 2Dh		; interrupt number
	mov si, int2D		; -> IISP entry header
	mov dx, opt4_int_2D_force >> 16
	call UnhookInterruptForce
				; try unhooking it
	jnc .got2D

.not2D:
	mov word [msg.serial_cannot_unhook.int], "2D"
	mov dx, msg.serial_cannot_unhook.nowarn
	jmp qq_attached_unterminated.common

.got2D:
	clropt [internalflags4], dif4_int_2D_hooked
.noint2D:
%endif


%if _CATCHINT08
.08:
	testopt [internalflags4], dif4_int_08_hooked
	jz .noint08

	mov al, 08h		; interrupt number
	mov si, intr8		; -> IISP entry header
	mov dx, opt4_int_08_force >> 16
	call UnhookInterruptForce
				; try unhooking it
	jnc .got08

.not08:
	mov word [msg.serial_cannot_unhook.int], "08"
	mov dx, msg.serial_cannot_unhook.nowarn
	jmp qq_attached_unterminated.common

.got08:
	clropt [internalflags4], dif4_int_08_hooked
	call update_inttab_optional
.noint08:
%endif


.serial:
	testopt [serial_flags], sf_init_done
	jz @F
	call serial_clean_up			; unhook interrupt
	clropt [serial_flags], sf_init_done	; clear (in case return to cmd3)
	clropt [options], enable_serial		; do not output to serial any longer
@@:
	testopt [internalflags4], dif4_int_serial_hooked
	jz .done_serial
	call serial_uninstall_interrupt_handler
	jnc .done_serial			; if it succeeded -->

	mov di, msg.serial_cannot_unhook.int
	mov al, byte [serial_installed_intnum]
	call hexbyte
	mov dx, msg.serial_cannot_unhook.nowarn
	mov byte [serial_interrupt_handler + ieEOI], 0
						; we do not issue EOI any longer
	jmp qq_attached_unterminated.common


.done_serial:

%if _PM
.2F:
	testopt [internalflags], hooked2F
	jz .noint2F

	mov al, 2Fh		; interrupt number
	mov si, debug2F		; -> IISP entry header
	mov dx, opt4_int_2F_force >> 16
	call UnhookInterruptForce
				; try unhooking it
	jnc .got2F

.not2F:
	mov word [msg.serial_cannot_unhook.int], "2F"
	mov dx, msg.serial_cannot_unhook.nowarn
	jmp qq_attached_unterminated.common

.got2F:
	clropt [internalflags], hooked2F
	clropt [internalflags4], dif4_int_2F_hooked
	call update_inttab_optional
.noint2F:
%endif


%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .restoreints
%endif
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz .restoreints
%endif


		; Cancel child's process if any.
		; This will drop to real mode if debuggee is in protected mode.
%if _TSR || _DEVICE
	testopt [internalflags], tsrmode
	jz .terminate_attached

%if _PM
	call ispm
	jz @F			; in PM -->
	testopt [internalflags], canswitchmode
	jz @FF			; in 86 Mode and cannot switch to PM -->

	setopt [internalflags], modeswitched	; set flag for resetmode
	mov al, 0
	call sr_state		; save state
	call switchmode 	; switch to PM
		; ! handle_mode_changed not called here !
		; do not call InDos or other functions using seg/sels
@@:
	call pm_reset_handlers
		; ! this calls resetmode

		; remember that we cannot access Protected Mode any longer
	clropt [internalflags], canswitchmode | switchbuffer
@@:
%endif

	jmp .restoreints

.terminate_attached:
%endif

	call terminate_attached_process
	jz qq_attached_unterminated
%if _PM
	call ispm
	jnz @F

	mov dx, msg.qq_still_pm
	jmp qq_attached_unterminated.common
@@:
%endif

.restoreints:
%if CATCHINTAMOUNT && ! (_DEBUG && ! _DEBUG_COND)
 %if _DEBUG
	testopt [internalflags6], dif6_debug_mode
	jnz .skiprestoreints
 %endif
		; Restore interrupt vectors.
	mov si, inttab
	mov di, intforcetab
%if _CATCHINT06 && _DETECT95LX
	mov cx, word [inttab_number_variable]
%else
	mov cx, inttab_number
%endif
	xor dx, dx
.nextint:
	lodsb
	xchg ax, bx			; bl = number
	lodsw				; si -> list
	xchg ax, si			; si -> entry, ax -> list
	xchg ax, bx			; al = number, bx -> list
	push di
	mov dh, byte [di]
	call UnhookInterruptForce
	pop di
	inc di
	xchg bx, si			; si -> list
	loop .nextint
.skiprestoreints:
%endif


%if _PM
	pop ax					; (discard)
%endif


qqlate:
%if _AREAS && _AREAS_HOOK_CLIENT
	call uninstall_areas.qq_entry
%endif

%if _AREAS_HOOK_SERVER
@@:
	mov dx, word [ddebugareas.next + 2]
	mov bx, word [ddebugareas.next]
	mov ax, ss
	cmp dx, ax
	je @F
	 push dx
	 push bx
	mov al, 0
	push cs
	call qqlate_86m_to_areastruc_entry
	push ss
	pop ds
	push ss
	pop es
	 pop bx
	 pop dx
	cmp dx, word [ddebugareas.next + 2]
	jne @B
	cmp bx, word [ddebugareas.next]
	jne @B
	mov dx, msg.qqlate_areas_error
	jmp putsz

@@:
%endif

%if _SYMBOLIC
		; Free XMS symbol table. 86 Mode memory backed symbol table
		;  is freed by our process's termination.
	nearcall zz_free_xms
%endif

		; Release the registered VDD.
%if _VDD
	testopt [internalflags], ntpacket
	jz .novdd
	mov ax, word [hVdd]
	UnRegisterModule
.novdd:
%endif

%if _VXCHG
	testopt [internalflags6], dif6_vv_mode
	jz @F

	call vv_disable
@@:
%endif

%if _ALTVID
	call setscreen
%endif

		; Restore termination address.
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .bootterminate	; terminate -->
%endif
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jnz .deviceterminate
%endif
%if _TSR || _DEVICE
 %if _TSR
	testopt [internalflags], tsrmode
	jz .nontsrterminate
 %else
	jmp .nontsrterminate
 %endif

.tsrterminate:
	xor si, si
	call guard_auxbuff
	mov es, word [auxbuff_segorsel]
	xor di, di
	xor ax, ax
	mov cx, 8
	rep stosw		; 10h MCB bytes
	mov cx, 40h
	rep movsw		; 80h PSP bytes
	mov ax, es
	inc ax
	mov word [es:1], ax	; fake MCB
	push ds
	mov ds, ax
	mov word [34h], 18h
	mov word [36h], ax	; insure default PHT and fix segment
	mov word [32h], 1	; only one PHT entry (zero might crash)
	mov byte [18h], -1	; PHT entry is closed
	mov word [2Ch], 0	; PSP clear
	call .setparent		; make it self-owned, just in case
	mov bx, ss
	dec bx
%if _DEVICE
	testopt [ss:internalflags6], dif6_device_mode
	jz @F
	sub bx, deviceshim_size_p + paras(10h)
@@:
%endif
	mov ds, bx		; => our (real) MCB
	mov word [1], ax	; parent = fake PSP
	pop ds
	call .setparent		; make the fake PSP our parent
	jmp short terminate_00	; see ya

.nontsrterminate:
%endif
	mov si, psp22		; restore termination address
	mov di, TPIV
	movsw
	movsw
	mov di, 16h		; restore PSP of parent
	movsw
		; Really done.

	testopt [qq_mode], qqmode_b
	jz @F
	int3

@@:
	mov ah, 4Ch		; quit
	mov al, byte [qqtermcode]
				; return code
	int 21h


terminate_00:	; used by terminate_attached_process
	mov ax, 4C00h		; quit
	int 21h


%if _AREAS_HOOK_SERVER
qqlate_86m_to_areastruc_entry:
	mov cx, areastruc_entry.qq_entry
	push ss
	push cx
	retf
%endif


qq_attached_unterminated:
	call putrunint
	mov dx, msg.qq_unterm

.common:
		; Restore state:
%if _PM
 %if (nohook2F)&~0FF00h
  %fatal Internal flags re-ordered, adjust code here
 %endif
 	pop ax
	mov [dpmidisable], al	; (SMC in section lDEBUG_DATA_ENTRY)
	and ah, nohook2F>>8
	clropt [internalflags], nohook2F
	or [internalflags+1], ah
%endif
	jmp putsz


%if _DEVICE
qqlate.deviceterminate:
	testopt [qq_mode], qqmode_d
	jz .mode_c

.mode_d:
		; We modify the device request header
		;  only now, so in case of being unable
		;  to release something then the debugger
		;  will remain usable and stay resident.
	mov es, word [reg_es]
	mov bx, word [reg_ebx]
	mov ax, ds
	sub ax, paras(deviceshim_size + 10h)
	mov word [es:bx + 3], 8103h	; error, done, code: unknown command
	and word [es:bx + 0Eh], 0
	mov word [es:bx + 0Eh + 2], ax	; -> behind memory in use
		; es reset in run

	xor bx, bx		; = 0
	mov cx, word [32h]	; get amount of handles
.loop:
	mov ah, 3Eh
	int 21h			; close it
	inc bx			; next handle
	loop .loop		; loop for all process handles -->

%if _DEBUG
		; avoid hooking interrupts again:
	mov byte [cs:..@patch_tsr_quit_run], __JMP_REL16
				; (SMC in section lDEBUG_CODE)
%endif
	testopt [qq_mode], qqmode_b
	jz @F
	mov word [reg_eip], entry_int3_retf
@@:
	jmp run			; run this


.mode_c:
qqlate_device_container:
	push es
	les di, [qq_device_prepare.device_reference]
	mov dx, es		; => device header pointing to ours
	mov ax, ss
	sub ax, deviceshim_size_p + paras(10h)
		; ! ax is re-used in .handlecontainer
	mov es, ax		; => our device header
	push word [es:0 + 2]
	push word [es:0]	; get our next link
	mov es, dx
	pop word [es:di]
	pop word [es:di + 2]	; update their next link
	mov es, ax
	or word [es:0], -1
	or word [es:0 + 2], -1	; de-initialise our next link
	pop es

	xor cx, cx		; flag: do not shrink our allocation
	mov bx, word [qq_device_prepare.container_segment]
	test bx, bx		; are we in a container ?
	jz .nocontainer		; no -->
.handlecontainer:
	add ax, word [device_mcb_paragraphs]
				; => behind our allocation
	mov dx, ax
	sub dx, word [qq_device_prepare.container_end]
				; are we last in container ?
	push ds
	mov ds, bx		; => container

	je .notrail		; yes, easier -->

	dec ax			; => last paragraph allocated to us
				;  (buffer for trailer container MCB)
	mov es, ax
	xor si, si
	xor di, di
		; copy over MCB letter, owner, and name/type
	mov cx, words(16)
	rep movsw
	mov byte [ss:qq_device_prepare.container_is_z], 'M'
				; tell subsequent handler to use 'M'
	mov word [es:3], dx	; set new size
	inc cx			; flag: shrink our allocation

.notrail:
	mov dx, ss
	sub dx, deviceshim_size_p + paras(10h)
	mov ax, dx
	dec dx
	mov es, dx		; => device mode MCB
	push ax
	mov al, byte [ss:qq_device_prepare.container_is_z]
	mov byte [es:0], al	; set our letter to M or Z
				;  (Z only if container had Z and also
				;  there is no trailing container created)
	pop ax
		; Name and owner should be set already.

	sub word [es:3], cx	; -= 1 in case we have trail
	mov word [es:1], ax	; insure valid owner
	sub dx, bx		; device mode MCB minus container MCB
	dec dx			; account for MCB paragraph to get MCB size
	mov word [3], dx	; adjust size
	mov byte [0], 'M'	; set M unconditionally
	test dx, dx		; size zero ?
	jnz @F
	mov word [1], dx	; yes, zero the owner too
@@:
	pop ds

.nocontainer:
	jmp qqlate.tsrterminate
%endif


%if _TSR || _DEVICE
	usesection lDEBUG_DATA_ENTRY

qq.proceedtsrtermination:
	cli
	cld
	mov ax, cs
	mov ds, ax
	mov ss, ax
	mov sp, stack_end
	sti
	sub word [reg_esp], 2+4+((qq.tsrfreecode_size+1)&~1)
	mov di, word [reg_esp]	; -> stack frame
	mov es, word [reg_ss]
	mov ax, word [reg_ds]
	stosw			; debuggee's ds
	mov ax, word [reg_eip]
	stosw
	mov ax, word [reg_cs]
	stosw			; debuggee's cs:ip
	push es
	push di
	mov si, qq.tsrfreecode
	mov cx, ((qq.tsrfreecode_size+1)>>1)
	rep movsw		; code on stack
	mov ax, cs
	dec ax
%if _DEVICE
	testopt [internalflags6], dif6_device_mode
	jz @F
	sub ax, deviceshim_size_p + paras(10h)
@@:
%endif
	mov word [reg_ds], ax	; = our MCB
	pop word [reg_eip]
	pop word [reg_cs]	; -> code on stack (at int3)
	testopt [qq_mode], qqmode_b
				; QB mode ?
	jnz @F			; yes, leave pointing cs:ip at int3
	inc word [reg_eip]	; point cs:ip past the int3
@@:
	testopt [options3], opt3_tsr_quit_leave_tf
	jnz @F
	clropt [reg_efl], 100h	; clear TF
@@:

	call entry_to_code_seg
	dw .proceedtsrcode


	usesection lDEBUG_CODE

	code_insure_low_byte_not_0CCh
.proceedtsrcode:
%if _DEBUG
		; avoid hooking interrupts again:
	mov byte [cs:..@patch_tsr_quit_run], __JMP_REL16
				; (SMC in section lDEBUG_CODE)
%endif
	jmp run			; run this


	usesection lDEBUG_DATA_ENTRY

	align 2, db 0
	; (Update: Explicitly clears TF now, except if the
	; option opt3_tsr_quit_leave_tf is set. See above.)
	;
	; Note that since we are in control of debuggee's TF and
	; reset it every time the debugger is entered, this code
	; will not be entered with TF set. It might be entered
	; with IF set and an interrupt might occur; the only harm
	; done then is that the interrupt handler has less stack
	; available. All flags must be preserved by this code.
qq.tsrfreecode:
	int3			; breakpoint for QB mode, 1 byte
	mov word [1], 0		; free the MCB
	pop ds			; restore debuggee's ds
	retf ((qq.tsrfreecode_size+1)&~1)	; jump
qq.tsrfreecode_size: equ $-qq.tsrfreecode


	usesection lDEBUG_CODE

qqlate.setparent:
	mov word [16h], ax
	mov word [0Ah], qq.proceedtsrtermination
	mov word [0Ah+2], ss
	retn
%endif


	usesection lDEBUG_CODE

%if _BOOTLDR
qqlate.bootterminate:
	sub word [reg_esp], 2*8+4+((qq.bootfreecode_size+1)&~1)
	mov di, word [reg_esp]	; -> stack frame
	mov es, word [reg_ss]
	mov ax, word [reg_ds]
	stosw
	mov ax, word [reg_es]
	stosw
	mov ax, word [reg_esi]
	stosw
	mov ax, word [reg_edi]
	stosw
	mov ax, word [reg_eax]
	stosw
	mov ax, word [reg_ecx]
	stosw
	mov ax, word [reg_ebx]
	stosw
	mov ax, word [reg_edx]
	stosw
	mov ax, word [reg_eip]
	stosw
	mov ax, word [reg_cs]
	stosw			; debuggee's cs:ip
	push es
	push di
	 push ds
	  push cs
	  pop ds		; => lDEBUG_CODE
	mov si, qq.bootfreecode
	mov cx, ((qq.bootfreecode_size+1)>>1)
	rep movsw		; code on stack
	 pop ds

	 push ss
	 pop es

	mov ax, word [ boot_new_memsizekib ]
	mov cl, 6
	shl ax, cl		; ax => source of EBDA (new position)
	mov dx, word [ boot_old_memsizekib ]
	shl dx, cl		; dx => destination of EBDA (old position)
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
	mov bx, dx		; = new EBDA reference to put in word [0:40Eh]
	pop ds

.noebda:
	mov word [reg_eax], ax	; => relocated (new) EBDA position
				;  (in front of debugger image)
	mov word [reg_ebx], bx	; = what to put in word [0:40Eh],
				;  unchanged content of that word if no EBDA
	mov word [reg_ecx], cx	; = EBDA size, 0 if no EBDA
	mov word [reg_edx], dx	; = original (old) EBDA position
				; = original mem size (in paras)
				;  (behind/in debugger image)
	mov word [reg_ds], 0

	pop word [reg_eip]
	pop word [reg_cs]	; -> code on stack
	testopt [qq_mode], qqmode_b
				; QB mode ?
	jnz @F			; yes, leave pointing cs:ip at int3
	inc word [reg_eip]	; point cs:ip past the int3
@@:
	testopt [options3], opt3_tsr_quit_leave_tf
	jnz @F
	clropt [reg_efl], 100h	; clear TF
@@:
	; call dumpregs
%if _DEBUG
		; avoid hooking interrupts again:
	mov byte [cs:..@patch_tsr_quit_run], __JMP_REL16
				; (SMC in section lDEBUG_CODE)
%endif
	; jmp cmd3
	jmp run			; run this


	align 2, db 0
qq.bootfreecode:
	int3			; breakpoint for QB mode, 1 byte
	pushf
	call movp		; move EBDA back (if any)
	mov word [40Eh], bx	; back relocate EBDA (if any)
	mov cl, 6
	shr dx, cl		; = to KiB
	mov word [413h], dx	; back relocate mem size
	popf
	pop ds
	pop es
	pop si
	pop di
	pop ax
	pop cx
	pop bx
	pop dx
	retf ((qq.bootfreecode_size+1)&~1)


		; Move paragraphs
		;
		; INP:	ax:0-> source
		;	dx:0-> destination
		;	cx = number of paragraphs
		; CHG:	-
		; Note:	Doesn't work correctly on HMA; doesn't always wrap to LMA either.
		;	Do not provide a wrapped/HMA source or destination!
movp: section_of_function
	push cx
	push ds
	push si
	push es
	push di

	cmp ax, dx		; source above destination ?
	ja .up			; yes, move up (forwards) -->
	je .return		; same, no need to move -->
	push ax
	add ax, cx		; (expected not to carry)
	cmp ax, dx		; end of source is above destination ?
	pop ax
	ja .down		; yes, move from top down -->
	; Here, the end of source is below-or-equal the destination,
	;  so they do not overlap. In this case we prefer moving up.

.up:
	push ax
	push dx
.uploop:
	mov ds, ax
	mov es, dx
	xor di, di
	xor si, si		; -> start of segment
	sub cx, 1000h		; 64 KiB left ?
	jbe .uplast		; no -->
	push cx
	mov cx, 10000h /2
	rep movsw		; move 64 KiB
	pop cx
	add ax, 1000h
	add dx, 1000h		; -> next segment
	jmp short .uploop	; proceed for more -->
.uplast:
	add cx, 1000h		; restore counter
	shl cx, 1
	shl cx, 1
	shl cx, 1		; *8, paragraphs to words
	rep movsw		; move last part
	pop dx
	pop ax
	jmp short .return

.down:
	std			; _AMD_ERRATUM_109_WORKAROUND as below
.dnloop:
	sub cx, 1000h		; 64 KiB left ?
	jbe .dnlast		; no -->
	push ax
	push dx
	add ax, cx
	add dx, cx
	mov ds, ax		; -> 64 KiB not yet moved
	mov es, dx
	pop dx
	pop ax
	mov di, -2
	mov si, di		; moved from last word down
	push cx
	mov cx, 10000h /2
	rep movsw		; move 64 KiB
	pop cx
	jmp short .dnloop	; proceed for more -->
.dnlast:
	add cx, 1000h		; restore counter
	shl cx, 1
	shl cx, 1
	shl cx, 1		; *8, paragraphs to words
	mov di, cx
	dec di
	shl di, 1		; words to offset, -> last word
	mov si, di
	mov ds, ax
	mov es, dx		; first segment correct


	numdef AMD_ERRATUM_109_WORKAROUND, 1
		; Refer to comment in init.asm init_movp.

%if _AMD_ERRATUM_109_WORKAROUND
	jcxz @FF
	cmp cx, 20
	ja @FF
@@:
	movsw
	loop @B
@@:
%endif
	rep movsw		; move first part
	cld
.return:
	pop di
	pop es
	pop si
	pop ds
	pop cx
	retn
qq.bootfreecode_size: equ $-qq.bootfreecode
%endif


%include "ss.asm"


	usesection lDEBUG_CODE

%if 0
getdebuggeebyte:
	push bp
	mov bp, sp
	sub sp, byte 4
	push bx
	push cx
%define _dedata -4
%define _bp 0
%define _ip 2
%define _adroffset 4
%define _adrsegment 8
	test byte [], memorydump
	jz .realmemory

	jmp short .return
.realmemory32:
.realmemory:
	mov ax, word [ bp + _adrsegment ]
	mov bx, word [ bp + _adroffset ]
	push ds
	mov ds, ax
	push word [ bx ]
	pop word [ bp + _dedata ]
	push word [ bx +2 ]
	pop word [ bp + _dedata +2 ]
	pop ds
;	test ax, ax
;	jnz .return
	mov dx, ax
	mov cl, 4
	shl ax, cl
	mov cl, 12
	shr dx, cl
	add ax, bx
	adc dx, byte 0
	jnz .return
	sub ax, 23h*4
	jb .return
	cmp ax, 2*4
	jae .return

	push ds
	xor bx, bx
	mov ds, bx
	push si
	push di
	mov si, 22h*4
	mov di, hackints.dummy22
	movsw
	movsw
	mov bl, 8
	add si, bx
	add di, bx
	movsw
	movsw

	mov cl, byte [ bx - 4 + hackints2324 ]
	mov byte [ bp + _dedata ], cl
.return:
	pop cx
	pop bx
	pop ax
	pop dx
	pop bp
	retn 6


		; Interrupt hack table
		;
		; This contains the Int23 and Int24 handler we want to show
		; the user. As we'll retrieve a dword per access,
	align 4, db 0
hackints:
.dummy22:	dd 0
.23:		dd 0
.24:		dd 0
.dummy25:	dd 0
%endif


%include "ww.asm"


	usesection lDEBUG_CODE

%ifn _EMS
xx: equ error
%else
	usesection lDEBUG_DATA_ENTRY
	align 2, db 0
xaresult:	dw -1

	usesection lDEBUG_CODE

		; X commands - manipulate EMS memory.
		;
		; Reference:
		;  http://www.nondot.org/sabre/os/files/MemManagement/LIMEMS41.txt

xx:	cmp al, '?'
	je xhelp		; if a call for help
	or al, TOLOWER
	cmp al, 'a'
	je xa			; if XA command
	cmp al, 'd'
	je xd			; if XD command
	cmp al, 'm'
	je xm			; if XM command
	cmp al, 'r'
	je xr			; if XR command
	cmp al, 's'
	je xs			; if XS command
	jmp error

xhelp:	lodsb
	call chkeol
	mov dx, msg.xhelp
	jmp putsz		; print string and return

		; XA - Allocate EMS.
xa:	call emschk
	call skipcomma
	call getword		; get argument into DX
	call chkeol		; expect end of line here
	mov bx, dx

	or word [xaresult], -1
	mov ax, 5A00h		; use the EMS 4.0 version to alloc 0 pages
	test bx, bx
	jz short .nullcnt
	mov ah, 43h		; allocate handle
.nullcnt:
	call emscall
	xchg ax, dx		; mov ax, dx
	mov word [xaresult], ax
	mov di, xaans1
	call hexword
	mov dx, xaans
	jmp putsz		; print string and return

		; XD - Deallocate EMS handle.
xd:	call emschk
	call skipcomma
	call getword		; get argument into DX
	call chkeol		; expect end of line here

	mov ah, 45h		; deallocate handle
	call emscall
	xchg ax, dx		; mov ax,dx
	mov di, xdans1
	call hexword
	mov dx, xdans
	jmp putsz		; print string and return

		; XR - Reallocate EMS handle.
xr:	call emschk
	call skipcomma
	call getword		; get handle argument into DX
	mov bx, dx
	call skipcomm0
	call getword		; get count argument into DX
	call chkeol		; expect end of line here
	xchg bx, dx

	mov ah, 51h		; reallocate handle
	call emscall
	mov dx, xrans
	jmp putsz		; print string and return

		; XM - Map EMS memory to physical page.
xm:	call emschk
	call skipcomma
	call getword		; get logical page (FFFFh means unmap)
	mov bx, dx		; save it in BX
	call skipcomm0
	call getbyte		; get physical page (DL)
	push dx
	call skipcomm0
	call getword		; get handle into DX
	call chkeol		; expect end of line
	 pop ax			; recover physical page into AL
	 push ax
	mov ah, 44h		; function 5 - map memory
	call emscall
	mov di, xmans1
	xchg ax, bx		; mov ax, bx
	call hexword
	mov di, xmans2
	pop ax
	call hexbyte
	mov dx, xmans
	jmp putsz		; print string and return

		; XS - Print EMS status.
xs:
	call emschk
	lodsb
	call chkeol		; no arguments allowed

		; First print out the handles and handle sizes.  This can be done either
		; by trying all possible handles or getting a handle table.
		; The latter is preferable, if it fits in memory.
	mov ah, 4Bh		; function 12 - get handle count
	call emscall
	cmp bx, (line_out_end-line_out)/4
	jbe short xs3			; if we can do it by getting the table
	xor dx, dx		; handle

xs1:
		; try EMS 4.0 function 5402h to get total number of handles
	mov ax, 5402h
	call emscall.witherrors
	mov cx, bx		; cx = number of handles
	jz @F

	mov cx, 0FFh		; total number of handles (assumed)
				;  this does not match the prior code here,
				;  which used 100h handles assuming that
				;  0FFh is the last valid handle number.
				; however, if we assume that there are 0FFh
				;  valid handles then the last number is 0FEh!
@@:

	mov ah, 4Ch		; function 13 - get handle pages
	call emscall.witherrors
	jnz short .err
	xchg ax, bx		; mov ax,bx
	call hndlshow
.cont:
	inc dx			; increment handle number to access
	jz @F			; (if 0000h handles, do not loop forever)
	cmp dx, cx		; end of the loop ?
	jb short xs1		; if more to be done -->
@@:
	jmp short xs5		; done with this part

.err:
	cmp ah, 83h		; no such handle?
	je short .cont		; just skip -->
	jmp emscall.errorhandle	; if other error -->

		; Get the information in tabular form.
xs3:
	mov ah, 4Dh		; function 14 - get all handle pages
	mov di, line_out
	call emscall
	test bx, bx
	jz short xs5
	mov si, di
xs4:
	lodsw
	xchg ax, dx
	lodsw
	call hndlshow
	dec bx
	jnz short xs4		; if more to go

xs5:
	mov dx, crlf
	call putsz		; print string

		; Next print the mappable physical address array.
		; The size of the array shouldn't be a problem.
	mov ax, 5800h		; function 25 - get mappable phys. address array
	mov di, line_out	; address to put array
	call emscall
	mov dx, xsnopgs
	jcxz xs7		; NO mappable pages!

	mov si, di
xs6:
	push cx
	lodsw
	mov di, xsstr2b
	call hexword
	lodsw
	mov di, xsstr2a
	call hexbyte
	mov dx, xsstr2
	call putsz		; print string
	pop cx			; end of loop
	test cl, 1
	jz short xs_nonl
	mov dx, crlf		; blank line
	call putsz		; print string
xs_nonl:
	loop xs6
	mov dx, crlf		; blank line
xs7:
	call putsz		; print string

		; Finally, print the cumulative totals.
	mov ah, 42h		; function 3 - get unallocated page count
	call emscall
	mov ax, dx		; total pages available
	sub ax, bx		; number of pages allocated
	mov bx, xsstrpg
	call sumshow		; print the line
	mov ah, 4Bh		; function 12 - get handle count
	call emscall
	push bx			; number of handles allocated

		; try EMS 4.0 function 5402h to get total number of handles
	mov ax, 5402h
	call emscall.witherrors	; don't use emscall, this function may fail!
	mov dx, bx
	jz @F

	mov dx, 0FFh		; total number of handles
@@:
	pop ax			; ax = number of handles allocated
	mov bx, xsstrhd
	jmp sumshow		; print the line

		; Call EMS
emscall:
	call .witherrors
	jz short .ret		; return if OK
.errorhandle:
	mov al, ah
	cmp al, 8Bh
	jg short .ce2		; if out of range (signed comparison intended)
	cbw
	mov bx, ax
	shl bx, 1
	mov dx, word [emserrs+100h+bx]
	test dx, dx
	jnz short .ce4		; if there's a word there
.ce2:
	mov dx, emserrx
	call putsz
	mov di, line_out
	call hexbyte
	call putsline_crlf
	jmp cmd3		; quit

.witherrors:
%if _PM
	call ispm
	jnz short .rm
subcpu 286
	push word [ss:pspdbg]
	push 67h
	call intcall
	db __TEST_IMM16		; (skip int opcode)
subcpureset
.rm:
%endif
	int 67h
	test ah, ah
.ret:
emschk.ret:
	retn


		; Check for EMS
		; maybe should disable this while bootloaded ?
emschk:
	mov al, 67h
	call intchk		; ZR if offset = -1 or segment = 0
				; CHG: ax, dx, bx
	jz .failed
	mov ah, 46h
	call emscall.witherrors	; get version
	jz short .ret		; success -->
.failed:
	mov dx, emsnot
emscall.ce4:
	jmp prnquit		; otherwise abort with message -->

		; HNDLSHOW - Print XS line giving the handle and pages allocated.
		;
		; Entry	DX	Handle
		;	AX	Number of pages
		;
		; Exit	Line printed
		;
		; Uses	ax, di
hndlshow:
	mov di, xsstr1b
	call hexword
	mov ax, dx
	mov di, xsstr1a
	call hexword
	push dx
	mov dx, xsstr1
	call putsz		; print string
	pop dx
	retn

		; SUMSHOW - Print summary line for XS command.
		;
		; Entry	AX	Number of xxxx's that have been used
		;	DX	Total number of xxxx's
		;	BX	Name of xxxx
		;
		; Exit	String printed
		;
		; Uses	AX, CX, DX, DI
sumshow:
	mov di, xsstr3
	push di
	call trimhex
	xchg ax, dx		; mov ax,dx
	mov di, xsstr3a
	call trimhex
	pop dx			; mov dx,xsstr3
	call putsz		; print string
	mov dx, bx
	call putsz		; print string
	mov dx, xsstr4
	jmp putsz		; print string and return

		; TRIMHEX - Print word without leading zeroes.
		;
		; Entry	AX	Number to print
		;	DI	Where to print it
		;
		; Uses	AX, CX, DI.
trimhex:
	call hexword
	sub di, 4		; back up DI to start of word
	mov cx, 3
	mov al, '0'
.loop:
	scasb
	jne .done		; return if not a '0'
	mov byte [di-1], ' '
	loop .loop
.done:
	retn
%endif	; _EMS

%if _DUALCODE
	usesection lDEBUG_CODE2
error_mirror:
	dualcall error

	usesection lDEBUG_CODE
%else
error_mirror equ error
%endif

		; Error handlers.
dualfunction
error: section_of_function
	push ss
	pop es
	push ss
	pop ds
	mov cx, si
	sub cx, line_in+3
	cmp cx, 256
	ja .invalid
	add cx, word [promptlen]; number of spaces to skip
	db __TEST_IMM16		; (skip xor)
.invalid:
	xor cx, cx		; if we're really messed up
	mov sp, [throwsp]
	jmp near [throwret]
		; INP:	cx = number of spaces to indent

		; This is the default address in throwret.
		; Display the error, then jump to errret.
errhandler:
	call get_columns	; ax = columns
.:
	sub cx, ax
	jnc .
	add cx, ax
	jz err2
	mov al, 32
.loop:
	call putc
	loop .loop
err2:
	mov dx, errcarat
	call putsz		; print string
	mov ax, 01FFh
	call setrc
	mov word [lastcmd], dmycmd
				; cancel command repetition
	jmp near [errret]	; return to the prompt (cmd3, aa01)


setrc: section_of_function
	cmp word [rc], 0
	jne .ret
	mov word [rc], ax
.ret:
	retn


		; Terminate the attached process, if any
		;
		; OUT:	NZ if now no process attached
		;	ZR if still a process attached,
		;	 ie we failed to terminate this one
terminate_attached_process:
	testopt [internalflags], attachedterm
	jnz @F

	clropt [reg_efl], 300h	; clear TF and IF
	mov word [reg_cs], cs
	mov word [reg_eip], terminate_00
	 push ax		; (dummy to take space for return address)
	mov word [reg_ss], ss
	mov word [reg_esp], sp	; save current ss:sp
	 pop ax			; (discard)
	xor ax, ax
	mov word [reg_eip+2], ax
	mov word [reg_esp+2], ax
	mov word [reg_efl+2], ax
%if _PM
	mov word [reg_es], ax
	mov word [reg_ds], ax
	mov word [reg_fs], ax
	mov word [reg_gs], ax	; insure valid segregs in PM
%endif
	mov word [run_sp_reserve], 128
	call run
		; The dummy stack space above is to hold the return address
		; of this call. The debugger stack is used by this run.
	and word [run_sp_reserve], 0

%if _SYMBOLIC
	nearcall zz_detect_xms
	clropt [internalflags2], dif2_createdprocess
%endif
	testopt [internalflags], attachedterm
@@:
	retn


%include "vv.asm"


%if _DEVICE && _DEVICE_SET_2324
	usesection lDEBUG_DATA_ENTRY
devint23:
	stc
	retf

devint24:
	mov al, 3
	iret

	usesection lDEBUG_CODE
%endif


;--- this is called by "run"
;--- set debuggee's INT 23/24.
;--- don't use INT 21h here, DOS might be "in use".
;--- registers may be modified - will soon be set to debuggee's

		; Low-level functions to reset to debuggee's interrupt vectors 23h/24h
		; INP:	-
		; OUT:	-
		; CHG:	bx, (e)dx, cx, ax
		; STT:	ds = our segment
		;	Do not use Int21, even if not in InDOS mode
setint2324:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .ret		; don't touch int23/24 -->
%endif
%if _PM
	call ispm
	jz .pm
%endif
	push es
	push di
	push si

	xor di, di
	mov es, di
	mov di, 23h *4
	mov si, run2324
	movsw
	movsw
	movsw
	movsw

%if _PM
	call InDos
	jnz @F
	call hook2F
@@:
%endif
	pop si
	pop di
	pop es
.ret:
	retn
%if _PM
.pm:
	push si
	mov si, run2324
	mov bx, 0223h
.loop:
	_386_o32		; mov edx, dword [si+0]
	mov dx, word [si+0]
	mov cx, word [si+4]
	mov ax, 0205h
	int 31h
	add si, 6
	inc bl
	dec bh
	jnz .loop
	pop si
	retn
%endif

		; Low-level functions to save debuggee's interrupt vectors 23h/24h
		;  and set our interrupt vectors instead
		; INP:	-
		; OUT:	-
		; CHG:	-
		; STT:	ds = our segment
		;	Do not use Int21, even if not in InDOS mode
getint2324:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .ret		; don't touch int23/24 -->
%endif
%if _PM
	call ispm
	jz .pm
%endif
	push si
	push di
	push es

	push ds
	pop es
	xor di, di
	mov ds, di
	mov di, run2324
	mov si, 23h *4
	push si
	movsw			; save interrupt vector 23h
	movsw
	movsw			; save interrupt vector 24h
	movsw
	pop di
	push es
	pop ds
	xor si, si
	mov es, si
	mov si, CCIV
	movsw
	movsw
	movsw
	movsw

	pop es
	pop di
	pop si
.ret:
	retn
%if _PM
subcpu 286
.pm:
	_386_o32
	pusha
	mov di, run2324
	mov bx, 0223h
.loop:
	mov ax, 0204h
	int 31h
	_386_o32		; mov dword [di+0], edx
	mov word [di+0], dx
	mov word [di+4], cx
	add di, byte 6
	inc bl
	dec bh
	jnz .loop
%if _ONLYNON386
	db __TEST_IMM8		; (skip pusha)
%else
	db __TEST_IMM16		; (skip pushad)
%endif

restoredbgi2324:
setdbgi2324:
	_386_o32
	pusha
	mov si, dbg2324
	mov bx, 0223h
_386	xor edx, edx
.loop:
	lodsw
	mov dx, ax
	mov cx, word [cssel]
	mov ax, 0205h
	int 31h
	inc bl
	dec bh
	jnz .loop
	_386_o32
	popa
	retn
subcpureset
%endif

%if 0
The next three subroutines concern the handling of Int23 and 24.
These interrupt vectors are saved and restored when running the
child process, but are not active when DEBUG itself is running.
It is still useful for the programmer to be able to check where Int23
and 24 point, so these values are copied into the interrupt table
during parts of the C, D, (DX, DI,) E, M, and S commands, so that
they appear to be in effect. The E command also copies these values
back.

Between calls to dohack and unhack, there should be no calls to DOS,
so that there is no possibility of these vectors being used when
DEBUG itself is running.

; As long as no DOS is loaded anyway, Int23 and Int24 won't be touched
by us, so the whole hack is unnecessary and will be skipped.
%endif

		; PREPHACK - Set up for interrupt vector substitution.
		; Entry	es = cs
prephack:
	cmp byte [hakstat], 0
	jne .err		; if hack status error -->
	push di
	mov di, sav2324		; debugger's Int2324
	call prehak1
	pop di
	retn

.err:
	push dx
	mov dx, ph_msg
	call putsz		; display error
	pop dx
	retn

		; INP:	di-> saved interrupt vectors
		; OUT:	-
		; CHG:	-
prehak1:
%if _PM
	call ispm
	jz .pm			; nothing to do
%endif
	push ds
	push si
	xor si, si
	mov ds, si
	mov si, 23h *4
	movsw
	movsw
	movsw
	movsw
	pop si
	pop ds
.pm:
	retn


		; DOHACK - Fake the interrupt vectors 23h and 24h to debuggee's
		; UNHACK - Restore interrupt vectors 23h and 24h to our values
		;	It's OK to do either of these twice in a row.
		;	In particular, the S command may do unhack twice in a row.
		; INP:	ds = our segment
		; OUT:	es = our segment
		; CHG:	-
		; STT:	Do not use Int21
dohack:
	push ss
	pop es
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz unhack.ret		; nothing to hack -->
%endif
	push si
	mov byte [hakstat], 1
	mov si, run2324		; debuggee's interrupt vectors
%if _PM
	call ispm
	jnz unhack.common
subcpu 286
	_386_o32
	pusha
	mov bx, 0223h
.pm_loop:
	_386_o32
	mov dx, word [si+0+0]
	mov cx, word [si+0+4]
	mov ax, 205h
	int 31h
	add si, byte 6
	inc bl
	dec bh
	jnz .pm_loop
	_386_o32
	popa
	pop si
	retn
subcpureset
%else
	jmp short unhack.common
%endif

unhack:
	push ss
	pop es
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .ret		; nothing to hack -->
%endif
	mov byte [hakstat], 0
%if _PM
	call ispm
	jz restoredbgi2324
%endif
	push si
	mov si, sav2324		; debugger's interrupt vectors
.common:
	push di
	push es
	xor di, di
	mov es, di
	mov di, 23h *4
	movsw
	movsw
	movsw
	movsw
	pop es
	pop di
	pop si
.ret:
	retn


InDOS_or_BIOS_output:
	testopt [options6], opt6_bios_output
	jnz InDos.return	; if should do output to ROM-BIOS -->

InDOS_or_BIOS_IO:
	testopt [options6], opt6_bios_io
	jnz InDos.return	; if should do I/O from/to ROM-BIOS -->


		; OUT:	NZ if InDOS mode
		;	ZR if not
		; CHG:	-
		; STT:	ss = ds
InDos: section_of_function
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .return		; always "in DOS" -->
%endif
	testopt [options], fakeindos
	jnz .return		; faking InDOS on anyway -->
.real_indos:
	push ds
	push si
	mov si, pInDOS + so16aSegSel
	call update_dosdata_segment
	lds si, [si - so16aSegSel]
	cmp byte [si], 0
	pop si
	pop ds
.return:
	retn


		; INP:	si -> word seg or sel, word segment, word selector
update_dosdata_segment:
	testopt [internalflags2], dif2_int31_segment
	jz .ret
	push dx
	push ax
	push bx
	mov al, 31h
	call get_86m_interrupt_handler_no_dos
%if _PM
	cmp word [si + soaSegment], dx
	je @F

	call ispm
	jnz .realmode

	mov bx, dx
	mov ax, 0002h
	int 31h

	mov word [si + soaSegSel], ax
	mov word [si + soaSelector], ax
	jmp @F

.realmode:
	mov word [si + soaSegSel], dx
	and word [si + soaSelector], 0

@@:
	mov word [si + soaSegment], dx
	pop bx
	pop ax
%else
	pop bx
	pop ax
	mov word [si + soaSegSel], dx
%endif
	pop dx
.ret:
	retn


;	PARSECM - Parse command line for C and M commands.
;	Entry	AL		First nonwhite character of parameters
;		SI		Address of the character after that
;		DI		(If _PM) getaddr or getaddrX for second parameter
;	Exit	DS:ESI		Address from first parameter
;		ES:EDI		Address from second parameter
;		ECX		Length of address range minus one
;		[bAddr32]	Set if any high word non-zero

parsecm_have_address:
	call getrangeX_have_address_need_length
	jmp @F

parsecm:
	call prephack
	mov bx, word [reg_ds]	; get source range
	xor cx, cx
	call getrangeX		; get address range into bx:(e)dx bx:(e)cx
		; Bug fixed in Debug/X 2.00: This used the same scratch
		;  selector as the getaddr used for the second operand.
		; As we never write to the first operand of an C or M
		;  command the simple fix is to use getrangeX here.
@@:
	push bx			; save segment first address
	call skipcomm0
	mov bx, word [reg_ds]
	_386_PM_o32	; sub ecx, edx
	sub cx, dx		; number of bytes minus one
	_386_PM_o32	; push edx
	push dx
	_386_PM_o32	; push ecx
	push cx
%if _PM
	mov cl, byte [bAddr32]
	push cx
	call di			; get destination address into bx:edx
	pop cx
	or byte [bAddr32], cl	; if either is 32-bit, handle both as 32-bit
%else
	call getaddr		; get destination address into bx:dx
%endif
	_386_PM_o32
	pop cx		; pop ecx
	_386_PM_o32	; mov edi, edx
	mov di, dx
	_386_PM_o32
	add dx, cx	; add edx, ecx
	jc short errorj7	; if it wrapped around
	call chkeol		; expect end of line
	mov es, bx
	_386_PM_o32	; pop esi
	pop si
	pop ds
	retn

errorj7:
	jmp error

;	PARSELW - Parse command line for L and W commands.
;
;	Entry	AL	First nonwhite character of parameters
;		SI	Address of the character after that
;
;	Exit	If there is at most one argument (program load/write), then the
;		zero flag is set, and registers are set as follows:
;		bx:(e)dx	Transfer address
;
;		If there are more arguments (absolute disk read/write), then the
;		zero flag is clear, and registers are set as follows:
;
;		DOS versions prior to 3.31:
;		AL	Drive number
;		CX	Number of sectors to read
;		DX	Beginning logical sector number
;		DS:BX	Transfer address
;
;		Later DOS versions:
;		AL	Drive number
;		BX	Offset of packet
;		CX	0FFFFh

	usesection lDEBUG_DATA_ENTRY
	align 4, db 0
packet:	dd 0		; sector number
	dw 0		; number of sectors to read
	dd 0		; transfer address Segm:OOOO
%if _PM
	dw 0		; transfer address might be Segm:OOOOOOOO!
%endif

	usesection lDEBUG_CODE
parselw:
	mov bx, word [reg_cs]	; default segment
	_386 xor edx, edx
	mov dx, 100h		; default offset
	call iseol?
	je plw2			; if no arguments
	call getaddr		; get buffer address into bx:(e)dx
	call skipcomm0
	call iseol?
	je plw2			; if only one argument
	push bx			; save segment
	push dx			; save offset
	mov bx, 80h		; max number of sectors to read
	neg dx
	jz plw1			; if address is zero
	mov cl, 9
	shr dx, cl		; max number of sectors which can be read
	mov di, dx
plw1:
	cmp byte [si], ':'	; drive letter specification ?
	jne @F			; no -->

	push ax
	call uppercase
	sub al, 'A'
	cmp al, 32		; valid drive ?
	mov dl, al		; put drive number
	inc si			; -> past the colon
	pop ax
	jb @FF			; got it -->
	dec si			; -> at colon

@@:
	call getbyte		; get drive number (DL)
	db __TEST_IMM8		; (skip lodsb)
@@:
	lodsb
	call skipcomm0
	push dx
	add dl, 'A'
	mov byte [driveno], dl
	call getdword		; get relative sector number
	call skipcomm0
	push bx			; save sector number high
	push dx			; save sector number low
	push si			; in case we find an error
	call getword		; get sector count
	dec dx
	cmp dx, di
	jae errorj7		; if too many sectors
	inc dx
	mov cx, dx
	call chkeol		; expect end of line
	testopt [internalflags], oldpacket| newpacket| ntpacket
	jnz plw3		; if using a packet -->
	pop si			; in case of error
	pop dx			; get LoWord starting logical sector number
	pop bx			; get HiWord
	test bx, bx		; just a 16-bit sector number possible
	jnz errorj7		; if too big
	pop ax			; drive number
	pop bx			; transfer buffer ofs
	pop ds			; transfer buffer seg
	test cx, cx		; NZ
plw2:
	retn

		; disk I/O packet for Int25/Int26, Int21.7305, VDD
plw3:
	pop bx			; discard si
	mov bx, packet
	pop word [bx+0]		; LoWord sector number
	pop word [bx+2]		; HiWord sector number
	mov word [bx+4], cx	; number of sectors
	pop ax			; drive number
	pop word [bx+6]		; transfer address ofs
	pop dx
	xor cx, cx
%if _PM
	call ispm
	jnz plw3_1
	cmp byte [dpmi32], 0
	jz plw3_1
[cpu 386]
	mov word [bx+10], dx	; save segment of transfer buffer
	movzx ebx, bx
	shr edx, 16		; get HiWord(offset)
	cmp byte [bAddr32], 1
	jz plw3_1
	xor dx, dx
__CPU__
plw3_1:
%endif
	mov word [bx+8], dx	; transfer address seg
	dec cx			; NZ and make cx = -1
	retn


%include "expr.asm"


%include "lineio.asm"


%include "ints.asm"


	usesection lDEBUG_CODE

%if _BOOTLDR
		; Determine the amount of actual memory
		;
		; This is important to call at the time we need the size,
		; not just save the size initially. Loading other pre-boot
		; installers or RPLs will change the size.
		;
		; INP:	-
		; OUT:	dx = segment behind usable memory (taking EBDAs & RPLs into account)
		;	ds = ss
		; CHG:	ax, cx, di, si, ds
bootgetmemorysize: section_of_function
	push es
	xor ax, ax
	mov ds, ax
	int 12h					; get memory size in KiB
	mov cl, 6
	shl ax, cl				; *64, convert to paragraphs
	push ax
	lds si, [ 2Fh *4 ]			; get current Int2F
	inc si					; pointer valid (not 0FFFFh) ? (left increased!)
	jz .norpl				; no -->
	mov ax, ds
	test ax, ax				; segment valid (not zero) ?
	jz .norpl				; no -->
	times 2 inc si				; +3 with above inc
	push cs
	pop es
	mov di, .rpl
	mov cx, .rpl_size
	repe cmpsb				; "RPL" signature ?
	jne .norpl				; no -->
	pop dx
	mov ax, 4A06h
	int 2Fh					; adjust usable memory size for RPL
	db __TEST_IMM8				; (skip pop)
.norpl:
	pop dx
		; dx = segment behind last available memory
	 push ss
	 pop ds
	pop es
	retn

.rpl:	db "RPL"
	endarea .rpl
%endif


		; Ensure a debuggee process is loaded
		;
		; INP:	si:di = to preserve if have a process already
		; OUT:	NZ if have no process and unable to create process
		;	ZR if have a process or created empty process
		;	NC if had no process yet, created one or not
		;	CY if had a process already,
		;	 si:di = preserved input
		;	NC, ZR if had no process, created empty one,
		;	 si:di = debuggee cs:ip
		;	NC, NZ if int19 occurred (or bootloaded)
		; CHG:	si, di, cx
ensuredebuggeeloaded:
	push ax
	push bx
	push dx

	testopt [internalflags3], dif3_gotint19
	jz .notint19

	clropt [internalflags3], dif3_gotint19
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jz .dosint19

.bootint19:
	call zeroregs

	mov ax, 60h
	push ax
	mov di, reg_ds
	stosw
	scasw			; (skip dummy high word)
	stosw
	scasw
	stosw
	scasw
	stosw
	call adusetup
	call bootgetmemorysize
	sub dx, 60h
	cmp dx, 1000h
	jbe .bootbelow64kib	; if memory left <= 64 KiB
	xor dx, dx		; dx = 1000h (same thing, after shifting)
.bootbelow64kib:
	mov cl, 4
	shl dx, cl
	dec dx
	dec dx
	mov word [reg_esp], dx
	pop es
	xchg dx, di		; es:di = child stack pointer
	xor ax, ax
	stosw			; push 0 on client's stack

	mov word [es:0], 019CDh	; place opcode for int 19h at cs:ip
	jmp @F
%endif

.dosint19:
	mov dx, word [reg_esp]
	mov bx, word [reg_ss]

	call zeroregs

		; Upon receiving an int 19h in DOS
		;  just set up some shim that will
		;  lead to process termination.
		; Unlike before we do not longer try
		;  to create a new process then.
	mov word [reg_esp], dx
	mov word [reg_ss], bx	; preserve our stack
	push word [pspdbe]
	pop word [reg_cs]	; cs = PSP, ip = 0,
				;  cs:ip -> int 20h instruction

@@:
	testopt [internalflags], attachedterm
	jnz .noprocess		; if also process not loaded

	or dl, 1		; flags return NC, NZ
	jmp .return

.notint19:
	testopt [internalflags], attachedterm
	jnz .noprocess		; not loaded, create -->
				; flags return ZR
	stc			; flags return CY
	jmp .return

.noprocess:
%if _BOOTLDR
	testopt [internalflags], nodosloaded
	jnz .return		; flags return NC, NZ
%endif

.dosnoprocess:
	mov ah, 48h		; get size of largest free block
	mov bx, -1
	int 21h
	cmp bx, 11h		; enough for PSP + one paragraph for code/stack ?
	jb .return_no_clr	; no -->
	mov ah, 48h		; allocate it
	int 21h
	jc .return_no_clr	; (memory taken between the calls)

	push ax
	call zeroregs
	mov byte [reg_eip+1], 100h>>8
	pop ax

	push bx
%if _SYMBOLIC
	push bx
%endif
	mov di, reg_ds		; fill segment registers ds,es,ss,cs
	stosw
	scasw			; (skip dummy high word)
	stosw
	scasw
	stosw
	scasw
	stosw
	call adusetup
	mov bx, word [reg_cs]	; bx:dx = where to load program
	mov es, bx
	pop ax			; get size of memory block
	mov dx, ax
	add dx, bx
	mov word [es:ALASAP], dx
	cmp ax, 1000h
	jbe .below64kib		; if memory left <= 64 KiB
	xor ax, ax		; ax = 1000h (same thing, after shifting)
.below64kib:
	mov cl, 4
	shl ax, cl
	dec ax
	dec ax
	mov word [reg_esp], ax
	xchg ax, di		; es:di = child stack pointer
	xor ax, ax
	stosw			; push 0 on client's stack

		; Create a PSP
	mov ah, 55h		; create child PSP
	mov dx, es
	mov si, word [es:ALASAP]
	clc			; works around OS/2 bug
	int 21h
	call setpspdbg		; reset PSP to ours
	call ll_copy_cmdline
	push ds
	 push es
	 pop ds
	mov dx, 80h
	mov ah, 1Ah		; set DTA to default DTA
	int 21h
	pop ds

		; Finish up. Set termination address.
	mov ax, 2522h		; set interrupt vector 22h
	mov dx, int22
	int 21h
	mov word [es:TPIV], dx
	mov word [es:TPIV+2], ds

	mov byte [es:100h], 0C3h	; place opcode for retn at cs:ip

%if _SYMBOLIC
	pop bx				; size of memory block
%endif
	mov word [pspdbe], es
	mov ax, es
	dec ax
	mov es, ax
	inc ax
	mov word [es:8+0], "DE"
	mov word [es:8+2], "BU"
	mov word [es:8+4], "GG"
	mov word [es:8+6], "EE"	; set MCB name
	mov word [es:1], ax	; set MCB owner

%if _SYMBOLIC
	setopt [internalflags2], dif2_createdprocess
	mov word [created_psp], ax
	mov word [created_size], bx
%endif

	mov si, word [reg_cs]
	mov di, word [reg_eip]		; ? is this ever used ?

	clropt [internalflags], attachedterm
	cmp al, al			; flags return ZR, NC

.return:
@@:
	push ss
	pop es

	pop dx
	pop bx
	pop ax
	retn

.return_no_clr:
	mov dx, msg.ensure_no_memory
	call putsz
	test dx, dx			; flags return NZ, NC
	jmp .return


zeroregs:
	; call set_efl_to_fl	; initialise EFL, and ax = 0
; set_efl_to_fl:
	xor ax, ax		; initialise ax = 0 and FL = ZR NC etc
_no386	push ax			; dummy high word
	_386_o32	; pushfd
	pushf
	pop word [reg_efl]	; set to FL
	pop word [reg_efl+2]	; set to high word of EFL, or zero
	; retn

	mov di, regs
	mov cx, 15 * 2		; (8 standard + 6 segregs + eip) * 2
	rep stosw		; initialise all registers
	retn


%if _PM
		; Hook Int2F if a DPMI host is found. However for Win9x and DosEmu
		; Int2F.1687 is not hooked because it doesn't work. Debugging in
		; protected mode may still work, but the initial switch must be
		; single-stepped.
		;
		; CHG:	ah, bx, cx, dx, di, es
		; OUT:	al = 0FFh if installed
		;	al = status value 0F0h..0FEh else
		; STT:	V86/RM
		;	ds = debugger data segment
		;	! might be called with unknown ss
		;	  if [internalflags6] & dif6_in_amis_hook2F
hook2F:
	call InDos
	mov al, 0FEh
	jnz .return
.not_in_dos:
	mov al, 0FDh
	testopt [internalflags], hooked2F
	jnz .return		; don't hook now -->
	dec ax			; 0FCh
 %if _DEBUG
	testopt [internalflags6], dif6_in_hook2F
	jnz .return
 %endif
.loop:
 %if _GUARD_86M_INT2F
	push es
	xor ax, ax
	mov es, ax		; (only used in 86 Mode)
	mov ax, [es:2Fh * 4]
	cmp ax, -1
	je @F			; --> (ZR)
	or ax, [es:2Fh * 4 + 2]
@@:
	pop es
	mov al, 0FBh
	jz .return
 %endif
 %if _DEBUG
	testopt [internalflags6], dif6_in_amis_hook2F
	jnz @F
	push si
	call findinstalleddebugger
				; CHG: si, di, es, ax, cx, dx
	pop si
	jc @F
	setopt [internalflags6], dif6_in_hook2F
				; avoid recursion
	inc ax			; al = 31h
	int 2Dh			; call out to debugger
	clropt [internalflags6], dif6_in_hook2F
	cmp al, 0FFh
	jne @F
  %if _DISPHOOK
	mov dx, dpmihookamis
	call putsz
  %endif
@@:
 %endif
	mov ax, 1687h		; DPMI host installed?
	int 2Fh
	test ax, ax
	mov al, 0FAh
	jnz .return
	mov word [dpmientry+0], di	; true host DPMI entry
	mov word [dpmientry+2], es
	mov word [dpmiwatch+0], di
	mov word [dpmiwatch+2], es
	dec ax			; 0F9h
	testopt [internalflags], nohook2F
	jnz .return		; can't hook Int2F -->
	dec ax			; 0F8h
	testopt [options4], opt4_int_2F_hook
	jz .return		; requested to not hook -->
	mov ax, 352Fh
	int 21h
	mov word [oldi2F+0], bx
	mov word [oldi2F+2], es
	mov dx, debug2F		; ds => lDEBUG_DATA_ENTRY
	mov ax, 252Fh
	int 21h

		; Test whether we can hook the DPMI entrypoint call.
	mov ax, 1687h
	int 2Fh
	test ax, ax
	jnz .nohost
	cmp di, mydpmientry	; our entrypoint returned ?
	jne .nohook
	mov ax, es
	mov bx, ds		; bx => lDEBUG_DATA_ENTRY
	cmp ax, bx
	jne .nohook		; no -->

	mov word [dpmiwatch+0], mydpmientry
	mov word [dpmiwatch+2], ds	; => lDEBUG_DATA_ENTRY

	setopt [internalflags], hooked2F
	setopt [internalflags4], dif4_int_2F_hooked
	call update_inttab_optional
%if _DISPHOOK
	testopt [internalflags6], dif6_in_amis_hook2F
	jnz @F
	mov ax, ds		; ax => lDEBUG_DATA_ENTRY
	push ds
	pop es
	mov di, dpmihookcs
	call hexword
	mov dx, dpmihook
	call putsz
@@:
%endif
	mov al, 0FFh
.return:
	push ds
	pop es
	retn

.nohost:
.nohook:
	push ds
	lds dx, [oldi2F]
	mov ax, 252Fh
	int 21h			; unhook
	pop ds
	push ds
	pop es			; restore segregs
	setopt [internalflags], nohook2F
				; note that we cannot hook
	testopt [internalflags6], dif6_in_amis_hook2F
	jnz @F
	mov dx, msg.dpmi_no_hook
	call putsz		; display message about it
@@:
	call .loop
	mov al, 0F0h
	retn
%endif


	usesection lDEBUG_DATA_ENTRY
	align 16, db 0
ldebug_data_entry_size	equ $-section.lDEBUG_DATA_ENTRY.vstart
	endarea ldebug_data_entry, 1

	usesection ASMTABLE1
	align 16, db 0
asmtable1_size		equ $-section.ASMTABLE1.vstart
	endarea asmtable1, 1

	usesection ASMTABLE2
	align 16, db 0
asmtable2_size		equ $-section.ASMTABLE2.vstart
	endarea asmtable2, 1


	usesection DATASTACK
%define SECTIONFIXUP -$$+100h+ldebug_data_entry_size \
			+asmtable1_size+asmtable2_size

		; I/O buffers
	alignb 2
line_in:	resb 1			; maximal length of input line
		resb 1			; actual length (must be one less than previous byte)
		resb 255		; buffer for 13-terminated input line
.end:
	; zero-initialisation starts here
..@init_first:
				; b_bplist and g_bplist are expected in that order by initcont
%if _BREAKPOINTS
	alignb 2
b_bplist:
.used_mask:	resb (_NUM_B_BP + _NUM_SYM_BP + 7) >> 3
					; bitmask of used points
.disabled_mask:	resb (_NUM_B_BP + _NUM_SYM_BP + 7) >> 3
					; bitmask of disabled points
 %if _BREAKPOINTS_STICKY
.sticky_mask:	resb (_NUM_B_BP + _NUM_SYM_BP + 7) >> 3
					; bitmask of sticky points
					; desc: stay around during DEBUG's operation unless
					; explicitly removed/un-stickified. This allows
					; to keep breakpoints around while changing from PM.
					; Hits while in DEBUG are ignored though, use DDEBUG.
					; Disabling won't remove them, just ignores hits.
 %endif
	alignb 2
.bp:		resb (_NUM_B_BP + _NUM_SYM_BP) * BPSIZE
	alignb 2
.counter:	resw _NUM_B_BP
	alignb 2
.id:		resw _NUM_B_BP		; array of lengths/offsets, 0 = unused
			; low 10 bits = offset into .idbuffer (0..1023)
			; high 6 bits = length (0..63, 0 if unused)
	alignb 2
.when:		resw _NUM_B_BP		; array of pointers, 0 = unused

.idbuffer.length:	equ _NUM_B_ID_BYTES
.idbuffer.free:
		resw 1			; offset into .idbuffer of free space
					; (0..1024)

.whenbuffer.length:	equ _NUM_B_WHEN_BYTES
.whenbuffer.free:
		resw 1			; *offset* into .whenbuffer
					;  (not a pointer)

.idbuffer:
		resb .idbuffer.length	; buffer holding ID strings
.whenbuffer:
		resb .whenbuffer.length	; buffer holding condition strings
%endif
%if _NUM_G_BP
	resb 1 - (($-$$) % 2)		; make g_bplist.bp aligned
g_bplist:
.used_count:	resb 1			; for the byte counter of saved breakpoints
.bp:		resb _NUM_G_BP*BPSIZE
.end:
%endif
sss_silent_count_used:	resb 1
%if _HISTORY && ! _HISTORY_SEPARATE_FIXED
	alignb 2
historybuffer:	resb _HISTORY_SIZE
.end:
%endif

		; $ - $$	= offset into section
		; % 2		= 1 if odd offset, 0 if even
		; 2 -		= 1 if odd, 2 if even
		; % 2		= 1 if odd, 0 if even
	; resb (2 - (($-$$) % 2)) % 2
		; $ - $$	= offset into section
		; % 2		= 1 if odd offset, 0 if even
		; 1 -		= 0 if odd, 1 if even
	resb 1 - (($-$$) % 2)		; make line_out aligned
trim_overflow:	resb 1			; actually part of line_out to avoid overflow of trimputs loop
line_out:	resb 263
		resb 1			; reserved for terminating zero
line_out_end:
	alignb 2
line_out_overflow:	resw 1		; 2642h if line_out didn't overflow

	alignb 4
sss_silent_count:	resd 1
	alignb 2
getrange_lines:		resw 1

serial_save_irq_mask:	resw 1
serial_save_irq_off:	resw 1
serial_save_dl:		resw 1
serial_save_ier:	resb 1
serial_save_lcr:	resb 1
serial_save_mcr:	resb 1
%if _USE_TX_FIFO
 serial_fcr_setting:	resb 1
%endif
serial_use_intnum:	resb 1
serial_use_params:	resb 1
serial_use_fifo:	resb 1
	alignb 2
baseport:
serial_use_baseport:	resw 1
serial_use_dl:		resw 1
serial_use_irqmask:	resw 1

	alignb 2
rxhead:		resw 1
rxtail:		resw 1
txhead:		resw 1
txtail:		resw 1
	alignb 16
rxfifo:		resb _RXFIFOSIZE
	alignb 16
txfifo:		resb _TXFIFOSIZE


%if _SYMBOLIC
%if _BUFFER_86MM_SLICE || _XMS_SYMBOL_TABLE
	alignb 16
access_slice_buffer:
.:
	resb ssString + 255
	alignb 2
.size:	equ $ - .
 %if _SECOND_SLICE
	alignb 16
second_access_slice_buffer:
.:
	resb ssString + 255
	alignb 2
.size:	equ $ - .
 %endif
%endif
	alignb 16
str_buffer:	resb 512		; long enough for smName1 + smName2 content
		; by placing this buffer below the stack, a stack overflow
		;  might be less harmful if the str_buffer isn't in use.
%endif

	; zero-initialisation ends here
..@init_behind:

		alignb 16		; stack might be re-used as GDT, so align it on a paragraph
stack:		resb _STACKSIZE
		alignb 2		; ensure stack aligned
stack_end:

datastack_size	equ $-section.DATASTACK.vstart
	endarea datastack, 1


	usesection INIT
initstart:

%include "init.asm"

	usesection INIT
		align 16, db 0
init_size	equ $-section.INIT.vstart
	endarea init, 1


	usesection lDEBUG_CODE
	align 16, db 0
ldebug_code_size	equ $-section.lDEBUG_CODE.vstart
	endarea ldebug_code, 1


	usesection lDEBUG_CODE2
	align 16, db 0
ldebug_code2_size	equ $-section.lDEBUG_CODE2.vstart
	endarea ldebug_code2, 1


auxbuff_size: equ (_AUXBUFFSIZE+15) & ~15
	endarea auxbuff, 1


pspsegment_size:	equ 100h+ldebug_data_entry_size \
			+asmtable1_size+asmtable2_size \
			+datastack_size
	endarea pspsegment, 1			; size of PSP and image when installed


	numdef SHOWASMTABLESIZE, _DEFAULTSHOWSIZE
%if _SHOWASMTABLESIZE
%assign ASMTABLESIZE asmtable1_size + asmtable2_size
%warning asmtables hold ASMTABLESIZE bytes
%endif


%assign __INITSIZE init_size
%if __INITSIZE > (64 * 1024)
 %error init segment too large (%[__INITSIZE])
%endif

	numdef SHOWINITSIZE, _DEFAULTSHOWSIZE
%if _SHOWINITSIZE
%warning init segment holds __INITSIZE bytes
%endif


%assign __CODESIZE ldebug_code_size
%if __CODESIZE > (64 * 1024)
 %error code segment too large (%[__CODESIZE])
%endif

	numdef SHOWCODESIZE, _DEFAULTSHOWSIZE
%if _SHOWCODESIZE
%warning code segment holds __CODESIZE bytes
%endif


%assign __CODE2SIZE ldebug_code2_size
%if __CODE2SIZE > (64 * 1024)
 %error code segment 2 too large (%[__CODE2SIZE])
%endif

%if _DUALCODE
	numdef SHOWCODE2SIZE, _DEFAULTSHOWSIZE
%else
	numdef SHOWCODE2SIZE, 0
%endif
%if _SHOWCODE2SIZE
%warning code segment 2 holds __CODE2SIZE bytes
%endif


%assign __PSPSEGMENTSIZE pspsegment_size
%if __PSPSEGMENTSIZE > (64 * 1024)
 %error resident size of PSP segment too large (%[__PSPSEGMENTSIZE])
%endif

	numdef SHOWPSPSIZE, _DEFAULTSHOWSIZE
%if _SHOWPSPSIZE
%warning PSP segment holds __PSPSEGMENTSIZE bytes
%endif

%if CODE_INSURE_COUNT
 %warning code_insure_low_byte_not_0CCh needed CODE_INSURE_COUNT times
%endif
