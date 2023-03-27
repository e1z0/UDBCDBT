
%if 0

lDebug messages

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2012 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

msg:
.help:
	db _PROGNAME,_VERSION," help screen",13,10
	db "assemble",9,	"A [address]",13,10
%if _BREAKPOINTS
	db "set breakpoint",9,	"BP index|AT|NEW address [[NUMBER=]number] [WHEN=cond] [ID=id]",13,10
	db " set ID",9,9,	"BI index|AT address [ID=]id",13,10
	db " set condition",9,	"BW index|AT address [WHEN=]cond",13,10
	db " set offset",9,	"BO index|AT address [OFFSET=]number",13,10
	db " set number",9,	"BN index|AT address|ALL number",13,10
	db " clear",9,9,	"BC index|AT address|ALL",13,10
	db " disable",9,	"BD index|AT address|ALL",13,10
	db " enable",9,9,	"BE index|AT address|ALL",13,10
	db " toggle",9,9,	"BT index|AT address|ALL",13,10
	db " swap",9,9,		"BS index1 index2",13,10
	db " list",9,9,		"BL [index|AT address|ALL]",13,10
%endif
%if _DEBUG
	db "break upwards",9,	"BU",13,10
%endif
	db "compare",9,9,	"C range address",13,10
	db "dump",9,9,		"D [range]",13,10
	db "dump bytes",9,	"DB [range]",13,10
	db "dump words",9,	"DW [range]",13,10
	db "dump dwords",9,	"DD [range]",13,10
%if _INT
	db "dump interrupts",9,	"DI[R][M][L] interrupt [count]",13,10
%endif
%if _PM
	db "dump LDT",9,	"DL selector [count]",13,10
%endif
%if _MCB
	db "dump MCB chain",9,	"DM [segment]",13,10
	;db "dump S/SD MCBs",9,	"DS",13,10
%endif
%if _DSTRINGS
	db "display strings",9,	"DZ/D$/D[W]# [address]",13,10
%endif
%if _PM
	db "dump ext memory",9,	"DX physical_address",13,10
	db "descriptor mod",9,	"D.A/D.D/D.B/D.L/D.T, D.? for help",13,10
%endif
	db "enter",9,9,		"E [address [list]]",13,10
	db "fill",9,9,		"F range [RANGE range|list]",13,10
	db "go",9,9,		"G [=address] [breakpts]",13,10
	db "goto",9,9,		"GOTO :label",13,10
	db "hex add/sub",9,	"H value1 [value2 [...]]",13,10
	db "base display",9,	"H BASE=number [GROUP=number] [WIDTH=number] value",13,10
	db "input",9,9,		"I[W|D] port",13,10
	db "if numeric",9,	"IF [NOT] (cond) THEN cmd",13,10
	db "if script file",9,	"IF [NOT] EXISTS Y file [:label] THEN cmd",13,10
	db "load program",9,	"L [address]",13,10
	db "load sectors",9,	"L address drive sector count",13,10
	db "move",9,9,		"M range address",13,10
	db "80x86/x87 mode",9,	"M [0..6|C|NC|C2|?]",13,10
	db "set name",9,	"N [[drive:][path]progname.ext [parameters]]",13,10
	db "output",9,9,	"O[W|D] port value",13,10
	db "proceed",9,9,	"P [=address] [count [WHILE cond] [SILENT [count]]]",13,10
	db "quit",9,9,		"Q",13,10
	db "quit process",9,	"QA",13,10
	db "quit and break",9,	"QB",13,10
	db "register",9,	"R [register [value]]",13,10
	db "Run R extended",9,	"RE",13,10
	db "RE commands",9,	"RE.LIST|APPEND|REPLACE [commands]",13,10
	db "Run Commandline",9,	"RC",13,10
	db "RC commands",9,	"RC.LIST|APPEND|REPLACE [commands]",13,10
%if _MMXSUPP
	db "MMX register",9,	"RM [BYTES|WORDS|DWORDS|QWORDS]",13,10
%endif
%if _RN
	db "FPU register",9,	"RN",13,10
%endif
	db "toggle 386 regs",9,	"RX",13,10
	db "search",9,9,	"S range [REVERSE] [RANGE range|list]",13,10
	db "sleep",9,9,		"SLEEP count [SECONDS|TICKS]",13,10
	db "trace",9,9,		"T [=address] [count [WHILE cond] [SILENT [count]]]",13,10
	db "trace (exc str)",9
	db			"TP [=address] [count [WHILE cond] [SILENT [count]]]",13,10
	db "trace mode",9,	"TM [0|1]",13,10
%if _TSR
	db "enter TSR mode",9,  "TSR",13,10
%endif
	db "unassemble",9,	"U [range]",13,10
%if _VXCHG
	db "view screen",9,	"V [ON|OFF [KEEP|NOKEEP]]",13,10
%endif
	db "write program",9,	"W [address]",13,10
	db "write sectors",9,	"W address drive sector count",13,10
%if _EMS
	db "expanded mem",9,	"XA/XD/XM/XR/XS, X? for help",13,10
%endif
	db "run script",9,	"Y [partition/][scriptfile] [:label]",13,10
	db 13,10
	db "Additional help topics:",13,10
%if _EXTHELP
	db " Registers",9,	"?R",13,10
	db " Flags",9,9,	"?F",13,10
 %if _COND
	db " Conditionals",9,	"?C",13,10
 %endif
 %if _EXPRESSIONS
	db " Expressions",9,	"?E",13,10
 %endif
 %if _VARIABLES || _OPTIONS || _PSPVARIABLES
	db " Variables",9,	"?V",13,10
 %endif
	db " R Extended",9,	"?RE",13,10
	db " Run keywords",9,	"?RUN",13,10
 %if _OPTIONS
	db " Options pages",9,	"?OPTIONS",13,10
	db " Options",9,	"?O",13,10
 %endif
 %if _BOOTLDR
	db " Boot loading",9,	"?BOOT",13,10
 %endif
%endif
	db " lDebug build",9,	"?BUILD",13,10
	db " lDebug build",9,	"?B",13,10
%if _EXTHELP
	db " lDebug sources",9,	"?SOURCE",13,10
	db " lDebug license",9,	"?L",13,10
%endif
%if _PM
	db 13,10
	db "Prompts: '-' = real or V86 mode; '#' = protected mode",13,10
%endif
	asciz

%if _EXTHELP
.source:asciz "SOURCE"
.help_source:
	db "The original lDebug sources can be obtained from the repo located at",13,10
	db "https://hg.pushbx.org/ecm/ldebug (E. C. Masloch's repo)",13,10
	db 13,10
	db "Releases of lDebug are available via the website at",13,10
	db "https://pushbx.org/ecm/web/#projects-ldebug",13,10
	db 13,10
	db "The most recent manual is hosted at https://pushbx.org/ecm/doc/ in the",13,10
	db "files ldebug.htm, ldebug.txt, and ldebug.pdf",13,10
	asciz
%endif

.re:
	asciz "RE"
.help_re:
	db "The RUN commands (T, TP, P, G) and the RE command use the RE command",13,10
	db "buffer to run commands. Most commands are allowed to be run from the",13,10
	db "RE buffer. Disallowed commands include program-loading L, A, E that",13,10
	db "switches the line input mode, TSR, Q, Y, RE, and further RUN commands.",13,10
	db "When the RE buffer is used as input during T, TP, or P with either",13,10
	db "of the WHILE or SILENT keywords, commands that use the auxbuff are",13,10
	db "also disallowed and will emit an error noting the conflict.",13,10
	db 13,10
	db "RE.LIST shows the current RE buffer contents in a format usable by",13,10
	db "the other RE commands. RE.APPEND appends the following commands to",13,10
	db "the buffer, if they fit. RE.REPLACE appends to the start of the",13,10
	db "buffer. When specifying commands, an unescaped semicolon is parsed",13,10
	db "as a linebreak to break apart individual commands. Backslashes can",13,10
	db "be used to escape semicolons and backslashes themselves.",13,10
	db 13,10
	db "Prefixing a line with an @ (AT sign) causes the command not to be",13,10
	db "shown to the standard output of the debugger when run. Otherwise,",13,10
	db "the command will be shown with a percent sign % or ~% prompt.",13,10
	db 13,10
	db "The default RE buffer content is @R. This content is also",13,10
	db "detected and handled specifically; if found as the only command",13,10
	db "the handler directly calls the register dump implementation",13,10
	db "without setting up and tearing down the special execution",13,10
	db "environment used to run arbitrary commands from the RE buffer.",13,10
	asciz

.run:
	asciz "RUN"
.help_run:
	db "T (trace), TP (trace except proceed past string operations), and P (proceed)",13,10
	db "can be followed by a number of repetitions and then the keyword WHILE,",13,10
	db "which must be followed by a conditional expression.",13,10
	db 13,10
	db "The selected run command is repeated as many times as specified by the",13,10
	db "number, or until the WHILE condition evaluates no longer to true.",13,10
	db 13,10
	db "After the number of repetitions or (if present) after the WHILE condition",13,10
	db "the keyword SILENT may follow. If that is the case, all register dumps",13,10
	db "done during the run are buffered by the debugger and the run remains",13,10
	db "silent. After the run, the last dumps are replayed from the buffer",13,10
	db "and displayed. At most as many dumps as fit into the buffer are",13,10
	db "displayed. (The buffer is currently up to 8 KiB sized.)",13,10
	db 13,10
	db "If a number follows behind the SILENT keyword, only at most that many",13,10
	db "dumps are displayed from the buffer. The dumps that are displayed",13,10
	db "are always those last written into the buffer, thus last occurred.",13,10
	asciz

	align 2, db 0
.build_array:
	dw .build_nameversion
	dw .build_lmacros
%if _SYMBOLIC
	dw .build_symsnip
%endif
	dw .build_scanptab
	dw .build_inicomp
%if _CHECKSUM
	dw .build_inicheck
%endif
	dw .build_ldosboot
.build_short_amount: equ ($ - .build_array) / 2
	dw .build_long
.build_long_amount: equ ($ - .build_array) / 2

%if _OPTIONS && _EXTHELP
	align 2, db 0
.options_array:
	dw .options_1
	dw .options_2
	dw .options_3
	dw .options_4
	dw .options_5
	dw .options_6
.options_array_option_amount: equ ($ - .options_array) / 2
	dw .flags_1
	dw .asmoptions_1
.options_array_amount: equ ($ - .options_array) / 2

.options_scan:
	db "123456"
.options_scan_amount: equ ($ - .options_scan)
%if .options_array_option_amount != .options_scan_amount
 %error Array size mismatch
%endif

.string_options:
	asciz "OPTIONS"

.options_pages:
	db "Enter one of the following commands to get a corresponding help page:",13,10
	db 13,10
	db "?O1",9,"DCO1 - Options",13,10
	db "?O2",9,"DCO2 - More Options",13,10
	db "?O3",9,"DCO3 - More Options",13,10
	db "?O4",9,"DCO4 - Interrupt Hooking Options",13,10
%if _VXCHG || (_DEBUG && _DEBUG_COND)
	db "?O6",9,"DCO6 - More Options",13,10
%endif
	db "?OI",9,"DIF - Internal Flags",13,10
	db "?OA",9,"DAO - Assembler/Disassembler Options",13,10
	asciz
%endif

.string_build:
	asciz "BUILD"

.build_nameversion:
	db _PROGNAME,_VERSION,13,10
%ifnidn _REVISIONID,""
	db "Source Control Revision ID: ",_REVISIONID,13,10
%endif
	asciz
	_fill 128, 0, .build_nameversion
.build_lmacros:
	fill 64, 0, asciz _REVISIONID_LMACROS
%if _SYMBOLIC
.build_symsnip:
	fill 64, 0, asciz _REVISIONID_SYMSNIP
%endif
.build_scanptab:
	fill 64, 0, asciz _REVISIONID_SCANPTAB
.build_inicomp:
	fill 64, 0, asciz _REVISIONID_INICOMP
%if _CHECKSUM
.build_inicheck:
	fill 64, 0, asciz _REVISIONID_INICHECK
%endif
.build_ldosboot:
	fill 64, 0, asciz _REVISIONID_LDOSBOOT

.build_long:
%if _EXTHELP
	db 13,10
 %if _PM
	db "DPMI-capable",13,10
  %if _NOEXTENDER
	db " DPMI host without extender",13,10
  %endif
  %if 0
   %if _WIN9XSUPP
	db " No Windows 4 DPMI hook",13,10
   %endif
   %if _PM && _DOSEMU
	db " No DOSEMU DPMI hook",13,10
   %endif
  %else
	db " Automatic DPMI entrypoint hook detection",13,10
  %endif
  %if _EXCCSIP
	db " Display exception address",13,10
  %endif
  %if _DISPHOOK
	db " Display hooking DPMI entry",13,10
  %endif
 %endif
 %if _DEBUG
	db "Debuggable",13,10
  %if _DEBUG_COND
	db "Conditionally Debuggable",13,10
  %endif
 %endif
 %if _INT
	db "DI command",13,10
 %endif
 %if _MCB
	db "DM command",13,10
 %endif
 %if _DSTRINGS
	db "D string commands",13,10
 %endif
 %if _SDUMP
	db "S match dumps line of following data",13,10
 %endif
 %if _RN
	db "RN command",13,10
 %endif
 %if _USESDA
	db "Access SDA current PSP field",13,10
 %endif
 %if _VDD
	db "Load NTVDM VDD for sector access",13,10
 %endif
 %if _EMS
	db "X commands for EMS access",13,10
 %endif
 %if _MMXSUPP
	db "RM command and reading MMX registers as variables",13,10
 %endif
 %if _EXPRESSIONS
	db "Expression evaluator",13,10
 %endif
 %if _INDIRECTION
	db " Indirection in expressions",13,10
 %endif
 %if _VARIABLES
	db "Variables with user-defined purpose",13,10
 %endif
 %if _OPTIONS
	db "Debugger option and status variables",13,10
 %endif
 %if _PSPVARIABLES
	db "PSP variables",13,10
 %endif
 %if _COND
	db "Conditional jump notice in register dump",13,10
 %endif
 %if _TSR
	db "TSR mode (Process detachment)",13,10
 %endif
 %if _DEVICE
	db "Loadable device driver",13,10
 %endif
 %if _BOOTLDR
	db "Boot loader",13,10
 %endif
 %if _BREAKPOINTS
	db "Permanent breakpoints",13,10
 %endif
%push
	db "Intercepted"
%if _PM
	db " 86M"
%endif
	db " interrupts:"
 %define %$pref " "
%macro dispint 2.nolist
 %if %1
	db %$pref, %2
  %define %$pref ", "
 %endif
%endmacro
	dispint _CATCHINT00, "00"
	dispint _CATCHINT01, "01"
	dispint _CATCHINT03, "03"
	dispint _CATCHINT06, "06"
	dispint _CATCHINT07, "07"
	dispint _CATCHINT0C, "0C"
	dispint _CATCHINT0D, "0D"
	dispint _CATCHINT18, "18"
	dispint _CATCHINT19, "19"
 %ifidn %$pref," "
	db " none"
 %endif
	db 13,10
 %if _PM || _CATCHINT08
	db "Processed"
  %if _PM
	db " 86M"
  %endif
	db " interrupts:"
  %define %$pref " "
	dispint _CATCHINT08, "08"
	dispint _PM, "2F.1687"
  %ifidn %$pref," "
	db " none"
  %endif
	db 13,10
 %endif
 %if _PM
	db "Intercepted DPMI exceptions:"
  %define %$pref " "
	dispint _CATCHEXC00, "00"
	dispint _CATCHEXC01, "01"
	dispint _CATCHEXC03, "03"
	dispint _CATCHEXC06, "06"
	dispint _CATCHEXC0C, "0C"
	dispint _CATCHEXC0D, "0D"
	dispint _CATCHEXC0E, "0E"
  %ifidn %$pref," "
	db " none"
  %endif
	db 13,10
 %endif
 %if _PM && _CATCHPMINT41
	db "Intercepted DPMI interrupts:"
  %define %$pref " "
	dispint _CATCHPMINT41, "41.004F"
  %ifidn %$pref," "
	db " none"
  %endif
	db 13,10
 %endif
 %if _PM && _CATCHPMINT214C
	db "Processed DPMI interrupts:"
  %define %$pref " "
	dispint _CATCHPMINT214C, "21.4C"
  %ifidn %$pref," "
	db " none"
  %endif
	db 13,10
 %endif
%unmacro dispint 2.nolist
%pop
 %if _EXTHELP
	db "Extended built-in help pages",13,10
 %endif
 %if _ONLYNON386
	db "Only supports non-386 operation",13,10
 %endif
 %if _ONLY386
	db "Only supports 386+ operation",13,10
 %endif
%else
 %if _BOOTLDR
	asciz
	; This message is used by mak.sh to detect that we
	;  are building with boot load support.
	db 13,10,"Boot loader",13,10
 %endif
%endif
	asciz

%if _EXTHELP
.license:
	db "lDebug - libre 86-DOS debugger",13,10
	db 13,10
	db "Copyright (C) 1995-2003 Paul Vojta",13,10
	db "Copyright (C) 2008-2021 C. Masloch",13,10
	db 13,10
	db "Usage of the works is permitted provided that this",13,10
	db "instrument is retained with the works, so that any entity",13,10
	db "that uses the works is notified of this instrument.",13,10
	db 13,10
	db "DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.",13,10
	db 13,10
	db 13,10
	db "All contributions by Paul Vojta or C. Masloch to the debugger are available",13,10
	db "under a choice of three different licenses. These are the Fair License, the",13,10
	db "Simplified 2-Clause BSD License, or the MIT License.",13,10
	db 13,10
	db "This is the license and copyright information that applies to lDebug; but note",13,10
	db "that there have been substantial contributions to the code base that are not",13,10
	db "copyrighted (public domain).",13,10
	asciz

.reghelp:
	db "Available 16-bit registers:",9,9,"Available 32-bit registers: (386+)",13,10
	db "AX",9,"Accumulator",9,9,9,"EAX",13,10
	db "BX",9,"Base register",9,9,9,"EBX",13,10
	db "CX",9,"Counter",9,9,9,9,"ECX",13,10
	db "DX",9,"Data register",9,9,9,"EDX",13,10
	db "SP",9,"Stack pointer",9,9,9,"ESP",13,10
	db "BP",9,"Base pointer",9,9,9,"EBP",13,10
	db "SI",9,"Source index",9,9,9,"ESI",13,10
	db "DI",9,"Destination index",9,9,"EDI",13,10
	db "DS",9,"Data segment",13,10
	db "ES",9,"Extra segment",13,10
	db "SS",9,"Stack segment",13,10
	db "CS",9,"Code segment",13,10
	db "FS",9,"Extra segment 2 (386+)",13,10
	db "GS",9,"Extra segment 3 (386+)",13,10
	db "IP",9,"Instruction pointer",9,9,"EIP",13,10
	db "FL",9,"Flags",9,9,9,9,"EFL",13,10
	db 13,10
 %if _MMXSUPP && 0
	db "Available 64-bit Matrix Math Extension (MMX) registers: (if supported)",13,10
	db "MMx",9,"MM(x)",9,"MMX register x, where x is 0 to 7",13,10
	db 13,10
 %endif
	db "Enter ?F to display the recognized flags.",13,10
	asciz

.flaghelp:
	db "Recognized flags:",13,10
	db "Value",9,"Name",9,9,9,	"  Set",9,9,9,		"  Clear",13,10
	db "0800  OF  Overflow Flag",9,9,"OV  Overflow",9,9,	"NV  No overflow",13,10
	db "0400  DF  Direction Flag",9,"DN  Down",9,9,		"UP  Up",13,10
	db "0200  IF  Interrupt Flag",9,"EI  Enable interrupts",9,"DI  Disable interrupts",13,10
	db "0080  SF  Sign Flag",9,9,	"NG  Negative",9,9,	"PL  Plus",13,10
	db "0040  ZF  Zero Flag",9,9,	"ZR  Zero",9,9,		"NZ  Not zero",13,10
	db "0010  AF  Auxiliary Flag",9,"AC  Auxiliary carry",9,"NA  No auxiliary carry",13,10
	db "0004  PF  Parity Flag",9,9,	"PE  Parity even",9,9,	"PO  Parity odd",13,10
	db "0001  CF  Carry Flag",9,9,	"CY  Carry",9,9,	"NC  No carry",13,10
	db 13,10
	db "The short names of the flag states are displayed when dumping registers",13,10
	db "and can be entered to modify the symbolic F register with R. The short",13,10
	db "names of the flags can be modified by R.",13,10
	asciz

 %if _COND
.condhelp:
	db "In the register dump displayed by the R, T, P and G commands, conditional",13,10
	db "jumps are displayed with a notice that shows whether the instruction will",13,10
	db "cause a jump depending on its condition and the current register and flag",13,10
	db 'contents. This notice shows either "jumping" or "not jumping" as appropriate.',13,10
	db 13,10
	db "The conditional jumps use these conditions: (second column negates)",13,10
	db " jo",9,9,"jno",9,9,"OF",13,10
	db " jc jb jnae",9,"jnc jnb jae",9,"CF",13,10
	db " jz je",9,9,"jnz jne",9,9,"ZF",13,10
	db " jbe jna",9,"jnbe ja",9,9,"ZF||CF",13,10
	db " js",9,9,"jns",9,9,"SF",13,10
	db " jp jpe",9,9,"jnp jpo",9,9,"PF",13,10
	db " jl jnge",9,"jnl jge",9,9,"OF^^SF",13,10
	db " jle jng",9,"jnle jg",9,9,"OF^^SF || ZF",13,10
	db " j(e)cxz",9,9,9,"(e)cx==0",13,10
	db " loop",9,9,9,9,"(e)cx!=1",13,10
	db " loopz loope",9,9,9,"(e)cx!=1 && ZF",13,10
	db " loopnz loopne",9,9,9,"(e)cx!=1 && !ZF",13,10
	db 13,10
	db "Enter ?F to display a description of the flag names.",13,10
	asciz
 %endif

 %if _EXPRESSIONS
.expressionhelp:
	db "Recognized operators in expressions:",13,10
	db "|",9,	"bitwise OR",9,9,		"||",9,	"boolean OR",13,10
	db "^",9,	"bitwise XOR",9,9,		"^^",9,	"boolean XOR",13,10
	db "&",9,	"bitwise AND",9,9,		"&&",9,	"boolean AND",13,10
	db ">>",9,	"bit-shift right",9,9,		">",9,"test if above",13,10
	db ">>>",9,	"signed bit-shift right",9,	"<",9,"test if below",13,10
	db "<<",9,	"bit-shift left",9,9,		">=",9,"test if above-or-equal",13,10
	db "><",9,	"bit-mirror",9,9,		"<=",9,"test if below-or-equal",13,10
	db "+",9,	"addition",9,9,			"==",9,"test if equal",13,10
	db "-",9,	"subtraction",9,9,		"!=",9,"test if not equal",13,10
	db "*",9,	"multiplication",9,9,		"=>",9,"same as >=",13,10
	db "/",9,	"division",9,9,			"=<",9,"same as <=",13,10
	db "%",9,	"modulo (A-(A/B*B))",9,		"<>",9,"same as !=",13,10
	db "**",9,	"power",13,10
	db 13,10
	db "Implicit operater precedence is handled in the listed order, with increasing",13,10
	db "precedence: (Brackets specify explicit precedence of an expression.)",13,10
	db " boolean operators OR, XOR, AND (each has a different precedence)",13,10
	db " comparison operators",13,10
	db " bitwise operators OR, XOR, AND (each has a different precedence)",13,10
	db " shift and bit-mirror operators",13,10
	db " addition and subtraction operators",13,10
	db " multiplication, division and modulo operators",13,10
	db " power operator",13,10
	db 13,10
	db "Recognized unary operators: (modifying the next number)",13,10
	db "+",9,	"positive (does nothing)",13,10
	db "-",9,	"negative",13,10
	db "~",9,	"bitwise NOT",13,10
	db "!",9,	"boolean NOT",13,10
	db "?",9,	"absolute value",13,10
	db "!!",9,	"convert to boolean",13,10
	db 13,10
	db "Note that the power operator does not affect unary operator handling.",13,10
	db 'For instance, "- 2 ** 2" is parsed as "(-2) ** 2" and evaluates to 4.',13,10
	db 13,10
	db "Although a negative unary and signed bit-shift right operator are provided",13,10
	db "the expression evaluator is intrinsically unsigned. Particularly the division,",13,10
	db "multiplication, modulo and all comparison operators operate unsigned. Due to",13,10
	db 'this, the expression "-1 < 0" evaluates to zero.',13,10
	db 13,10
	db "Recognized terms in an expression:",13,10
	db " 32-bit immediates",13,10
	db " 8-bit registers",13,10
	db " 16-bit registers including segment registers (except FS, GS)",13,10
	db " 32-bit compound registers made of two 16-bit registers (eg DXAX)",13,10
	db " 32-bit registers and FS, GS only if running on a 386+",13,10
  %if 0 && _MMXSUPP
	db " 64-bit MMX registers only if running on a CPU with MMX (r/o for now)",13,10
	db "  MM0L, MM(0)L accesses the low 32 bits of the register",13,10
	db "  MM0H, MM(0)H accesses the high 32 bits of the register",13,10
	db "  MM0Z, MM(0)Z reads the low 32 bits; writes the full register (zero-extend)",13,10
	db "  MM0S, MM(0)S reads the low 32 bits; writes the full register (sign-extend)",13,10
	db "  MM0, MM(0) is an alias for the MM0Z syntax",13,10
  %endif
  %if _VARIABLES
	db " 32-bit variables V00..VFF",13,10
  %endif
  %if _OPTIONS || _PSPVARIABLES
	db " 32-bit special variable"
   %if _OPTIONS
	db "s DCO, DCS, DAO, DAS, DIF, DPI"
    %if _PSPVARIABLES
	db ","
    %endif
   %endif
   %if _PSPVARIABLES
	db " PPI"
   %endif
	db 13,10
	db " 16-bit special variables"
   %if _OPTIONS
	db " DPR, DPP"
    %if _PM
	db ", DPS"
    %endif
    %if _PSPVARIABLES
	db ","
    %endif
   %endif
   %if _PSPVARIABLES
	db " PSP, PPR"
   %endif
	db 13,10
	db "  (fuller variable reference in the manual)",13,10
  %endif
  %if _INDIRECTION
	db " byte/word/3byte/dword memory content (eg byte [seg:ofs], where both the",13,10
	db "  optional segment as well as the offset are expressions too)",13,10
  %endif
	db "The expression evaluator case-insensitively checks for names of variables",13,10
	db "and registers"
  %if _INDIRECTION
	db		" as well as size specifiers"
  %endif
	db					   '.',13,10
	db 13,10
	db "Enter ?R to display the recognized register names.",13,10
  %if _VARIABLES || _OPTIONS || _PSPVARIABLES
	db "Enter ?V to display the recognized variables.",13,10
  %endif
	asciz
 %endif

 %if _OPTIONS
.options_1:
	db "Available options: (read/write DCO, read DCS)",13,10
	db _4digitshex(dispregs32),	" RX: 32-bit register display",13,10
	db _4digitshex(traceints),	" TM: trace into interrupts",13,10
	db _4digitshex(cpdepchars),	" allow dumping of CP-dependant characters",13,10
	db _4digitshex(fakeindos),	" always assume InDOS flag non-zero, to debug DOS or TSRs",13,10
	db _4digitshex(nonpagingdevice)," disallow paged output to StdOut",13,10
	db _4digitshex(pagingdevice),	" allow paged output to non-StdOut",13,10
	db _4digitshex(hexrn),		" display raw hexadecimal content of FPU registers",13,10
	db _4digitshex(nondospaging),	" when prompting during paging, do not use DOS for input",13,10
	db _4digitshex(nohlt),		" do not execute HLT instruction to idle",13,10
	db _4digitshex(biosidles),	" do not idle, the keyboard BIOS idles itself",13,10
	db _4digitshex(opt_usegetinput)," use getinput function for int 21h interactive input",13,10
	db _4digitshex(use_si_units),	" in disp_*_size use SI units (kB = 1000, etc)."
					db " overrides ",_4digitshex(use_jedec_units),"!",13,10
	db _4digitshex(use_jedec_units)," in disp_*_size use JEDEC units (KB = 1024)",13,10
	db _4digitshex(enable_serial),	" enable serial I/O (port ",_4digitshex(_UART_BASE),"h interrupt ",_2digitshex(_INTNUM),"h)",13,10
	db _4digitshex(int8_disable_serial),	" disable serial I/O when breaking after Ctrl pressed for a while",13,10
	db _8digitssephex(gg_do_not_skip_bp),	" gg: do not skip a breakpoint (bb or gg)",13,10
	db _8digitssephex(gg_no_autorepeat), 	" gg: do not auto-repeat",13,10
	db _8digitssephex(tp_do_not_skip_bp),	" T/TP/P: do not skip a (bb) breakpoint",13,10
	db _8digitssephex(gg_bb_hit_no_repeat),	" gg: do not auto-repeat after bb hit",13,10
	db _8digitssephex(tp_bb_hit_no_repeat),	" T/TP/P: do not auto-repeat after bb hit",13,10
	db _8digitssephex(gg_unexpected_no_repeat)," gg: do not auto-repeat after unexpectedinterrupt",13,10
	db _8digitssephex(tp_unexpected_no_repeat)," T/TP/P: do not auto-repeat after unexpectedinterrupt",13,10
	db _8digitssephex(ss_no_dump),		" S: do not dump data after matches",13,10
	db _8digitssephex(rr_disasm_no_rept),	" R: do not repeat disassembly",13,10
	db _8digitssephex(rr_disasm_no_show),	" R: do not show memory reference in disassembly",13,10
	db _8digitssephex(opt_cmdline_quiet_input)," quiet command line buffer input",13,10
	db _8digitssephex(opt_cmdline_quiet_output)," quiet command line buffer output",13,10
	asciz

.options_2:
	db "More options: (read/write DCO2, read DCS2)",13,10
	db _4digitshex(opt2_db_header),	" DB: show header",13,10
	db _4digitshex(opt2_db_trailer)," DB: show trailer",13,10
	db _4digitshex(opt2_dw_header),	" DW: show header",13,10
	db _4digitshex(opt2_dw_trailer)," DW: show trailer",13,10
	db _4digitshex(opt2_dd_header),	" DD: show header",13,10
	db _4digitshex(opt2_dd_trailer)," DD: show trailer",13,10
	db _4digitshex(opt2_getinput_dpmi)," use getinput function for int 21h interactive input in DPMI",13,10
	db _4digitshex(opt2_hh_compat),	" H: stay compatible to MS-DOS Debug",13,10
	db _4digitshex(opt2_getc_idle),	" idle and check for Ctrl-C in getc",13,10
	db _4digitshex(opt2_getc_idle_dpmi)," idle and check for Ctrl-C in getc in DPMI",13,10
	db _4digitshex(opt2_re_cancel_tpg)," T/TP/P/G: cancel run after RE command buffer execution",13,10
	asciz

.options_3:
	db "More options: (read/write DCO3, read DCS3)",13,10
	db _4digitshex(opt3_tt_no_paging)," T: do not page output",13,10
	db _4digitshex(opt3_tp_no_paging)," TP: do not page output",13,10
	db _4digitshex(opt3_pp_no_paging)," P: do not page output",13,10
	db _4digitshex(opt3_gg_no_paging)," G: do not page output",13,10
	db _4digitshex(opt3_silence_paging_set), " T/TP/P: modify paging for silent dump",13,10
	db _4digitshex(opt3_silence_paging_on), " T/TP/P: if ",_4digitshex(opt3_silence_paging_set)," set: turn paging on, else off",13,10
%if _REGSHIGHLIGHT
	db _6digitssephex(opt3_r_highlight_diff), " R: highlight changed digits (needs ANSI for DOS output)",13,10
	db _6digitssephex(opt3_r_highlight_dumb), " R: highlight escape sequences to int 10h, else video attributes",13,10
	db _6digitssephex(opt3_r_highlight_full), " R: highlight changed registers (overrides ",_6digitssephex(opt3_r_highlight_diff),")",13,10
	db _6digitssephex(opt3_r_highlight_eip), " R: include highlighting of EIP",13,10
%endif
%if _PM
	db _6digitssephex(opt3_ss_b_bit_set), " set PM ss B bit",13,10
 %if _BREAK_INSTALLDPMI
	db _6digitssephex(opt3_break_installdpmi), " break on entering Protected Mode",13,10
 %endif
%endif
%if _GETLINEHIGHLIGHT
	db _8digitssephex(opt3_getline_highlight), " highlight prefix/suffix in getinput if text parts are not visible",13,10
%endif

	db _8digitssephex(opt3_no_idle_2F), " do not call int 2F.1680 for idling",13,10
%if _DELAY_BEFORE_BP
	db _8digitssephex(opt3_delay_before_bp), " delay for a tick before writing breakpoints",13,10
%endif
	db _8digitssephex(opt3_no_call_update), " do not call other lDebug instance's Update IISP Header call",13,10
	db _8digitssephex(opt3_disable_autorepeat), " disable auto-repeat",13,10
	db _8digitssephex(opt3_check_ctrlc_keyb),   " check int 16h buffer for Control-C if inputting from int 16h",13,10
	db _8digitssephex(opt3_check_ctrlc_0bh),    " call DOS service 0Bh to check for Control-C",13,10
	db _8digitssephex(opt3_tsr_quit_leave_tf),  " when Q command is used while TSR, leave TF as is",13,10
	asciz

.options_4:
	db "More options: (read/write DCO4, read DCS4)",13,10
%if _PM
	db _4digitshex(opt4_int_2F_hook)," enable interrupt 2Fh hook while in 86 Mode",13,10
%endif
%if _CATCHINT08
	db _4digitshex(opt4_int_08_hook)," enable interrupt 8 hook",13,10
%endif
%if _CATCHINT2D
	db _4digitshex(opt4_int_2D_hook)," enable interrupt 2Dh hook",13,10
%endif
	db _8digitssephex(opt4_int_serial_force)," force serial interrupt unhooking",13,10
%if _PM
	db _8digitssephex(opt4_int_2F_force)," force interrupt 2Fh unhooking",13,10
%endif
%if _CATCHINT08
	db _8digitssephex(opt4_int_08_force)," force interrupt 8 unhooking",13,10
%endif
%if _CATCHINT2D
	db _8digitssephex(opt4_int_2D_force)," force interrupt 2Dh unhooking",13,10
%endif
%if _CATCHINT00
	db _8digitssephex(opt4_int_00_force)," force interrupt 0 unhooking",13,10
%endif
%if _CATCHINT01
	db _8digitssephex(opt4_int_01_force)," force interrupt 1 unhooking",13,10
%endif
%if _CATCHINT03
	db _8digitssephex(opt4_int_03_force)," force interrupt 3 unhooking",13,10
%endif
%if _CATCHINT06
	db _8digitssephex(opt4_int_06_force)," force interrupt 6 unhooking",13,10
%endif
%if _CATCHINT18
	db _8digitssephex(opt4_int_18_force)," force interrupt 18h unhooking",13,10
%endif
%if _CATCHINT19
	db _8digitssephex(opt4_int_19_force)," force interrupt 19h unhooking",13,10
%endif
%if _CATCHSYSREQ
 %if _SYSREQINT == 09h
	db _8digitssephex(opt4_int_09_force)," force interrupt 9 unhooking",13,10
 %elif _SYSREQINT == 15h
	db _8digitssephex(opt4_int_15_force)," force interrupt 15h unhooking",13,10
 %else
  %error Unknown SysReq interrupt
 %endif
%endif
%if _CATCHINT07 || _CATCHINT0C || _CATCHINT0D
	db _8digitssephex(opt4_int_0C_force)," force interrupt "
%define SEPARATOR ""
%if _CATCHINT07
	db SEPARATOR, "7"
 %define SEPARATOR "/"
%endif
%if _CATCHINT0C
	db SEPARATOR, "0Ch"
 %define SEPARATOR "/"
%endif
%if _CATCHINT0D
	db SEPARATOR, "0Dh"
 %define SEPARATOR "/"
%endif
	db " unhooking",13,10
%endif
.options_5:
	asciz

.options_6:
%if _VXCHG || (_DEBUG && _DEBUG_COND)
	db "More options: (read/write DCO6, read DCS6)",13,10
%endif
%if _VXCHG
	db _4digitshex(opt6_vv_mode)," enable video screen swapping",13,10
	db _4digitshex(opt6_vv_keep)," keep video screen when disabling swapping",13,10
	db _4digitshex(opt6_vv_int16)," read key from interrupt 16h when swapping (V command)",13,10
%endif
%if _DEBUG
	db _4digitshex(opt6_debug_exception_late)," run breakpoint late in debugger exception",13,10
	db _4digitshex(opt6_debug_exception_early)," run breakpoint early in debugger exception",13,10
 %if _DEBUG_COND
	db _4digitshex(opt6_debug_exception)," enable debug mode when debugger exception occurs",13,10
	db _4digitshex(opt6_debug_mode)," enable debug mode (and BU command)",13,10
 %endif
%endif
	db _4digitshex(opt6_bios_output)," use ROM-BIOS output even when DOS available",13,10
	db _4digitshex(opt6_flat_binary)," load and write .EXE and .COM files like flat .BIN files (/F+)",13,10
	db _4digitshex(opt6_big_stack)," for loading flat .BIN files set up Stack Segment != PSP (/E+)",13,10
%if _40COLUMNS
	db _4digitshex(opt6_40_columns)," enable 40-column friendly mode",13,10
	db _4digitshex(opt6_40_indent_odd)," in 40-column mode indent odd D lines more",13,10
	db _4digitshex(opt6_40_dash)," in 40-column mode display dashes at half of D length",13,10
%endif
	db _6digitssephex(opt6_share_serial_irq)," allow to share serial IRQ handler",13,10
%if _DEBUG
	db _6digitssephex(opt6_debug_putrunint_early)," run breakpoint early in putrunint",13,10
 %if _DEBUG_COND
	db _6digitssephex(opt6_debug_putrunint)," enable debug mode when putrunint called",13,10
 %endif
%endif
	db _8digitssephex(opt6_bios_io)," use ROM-BIOS I/O even when DOS available (disables script file read)",13,10
	asciz

.flags_1:
	db "Internal flags: (read DIF)",13,10
	db _6digitssephex(oldpacket),	" Int25/Int26 packet method available",13,10
	db _6digitssephex(newpacket),	" Int21.7305 packet method available",13,10
  %if _VDD
	db _6digitssephex(ntpacket),	" VDD registered and usable",13,10
  %endif
	db _6digitssephex(pagedcommand),	" internal flag for paged output",13,10
	db _6digitssephex(notstdinput),	" DEBUG's input isn't StdIn",13,10
	db _6digitssephex(inputfile),	" DEBUG's input is a file",13,10
	db _6digitssephex(notstdoutput),	" DEBUG's output isn't StdOut",13,10
	db _6digitssephex(outputfile),	" DEBUG's output is a file",13,10
  %if _PM
	db _6digitssephex(hooked2F),	" Int2F.1687 hooked",13,10
	db _6digitssephex(nohook2F),	" Int2F.1687 won't be hooked",13,10
	db _6digitssephex(dpminohlt),	" do not execute HLT to idle in PM",13,10
	db _6digitssephex(protectedmode),	" in protected mode",13,10
  %endif
	db _6digitssephex(debuggeeA20),	" state of debuggee's A20",13,10
	db _6digitssephex(debuggerA20),	" state of debugger's A20 (not implemented: same as previous)",13,10
  %if _BOOTLDR
	db _6digitssephex(nodosloaded),	" debugger booted independent of a DOS",13,10
  %endif
	db _6digitssephex(has386),		" CPU is at least a 386 (32-bit CPU)",13,10
	db _6digitssephex(usecharcounter),	" internal flag for tab output processing",13,10
 %if _VDD
	db _6digitssephex(runningnt),	" running inside NTVDM",13,10
 %endif
 %if _PM
	db _6digitssephex(canswitchmode),	" DPMI raw mode switch usable to set breakpoints",13,10
	db _6digitssephex(modeswitched),	" internal flag for mode switching",13,10
 %endif
	db _6digitssephex(promptwaiting),	" internal flag for paged output",13,10
 %if _PM
	db _6digitssephex(switchbuffer),	" internal flag for mode switching",13,10
 %endif
 %if _TSR
	db _6digitssephex(tsrmode),	" in TSR mode (detached debugger process)",13,10
 %endif
 %if _DOSEMU
	db _8digitssephex(runningdosemu),	" running inside dosemu",13,10
 %endif
	db _8digitssephex(tt_while)
	db " T/TP/P: while condition specified",13,10
	db _8digitssephex(tt_p)
	db " TP: P specified (proceed past string ops)",13,10
	db _8digitssephex(tt_silent_mode)
	db " T/TP/P: silent mode (SILENT specified)",13,10
	db _8digitssephex(tt_silence)
	db " T/TP/P: silent mode is active, writing to silent buffer",13,10
	asciz

.asmoptions_1:
	db "Available assembler/disassembler options: (read/write DAO, read DAS)",13,10
	db _2digitshex(disasm_lowercase), " Disassembler: lowercase output",13,10
	db _2digitshex(disasm_commablank)," Disassembler: output blank behind comma",13,10
	db _2digitshex(disasm_nasm),      " Disassembler: output addresses in NASM syntax",13,10
	db _2digitshex(disasm_lowercase_refmem)
	db				  " Disassembler: lowercase referenced memory location segreg",13,10
	db _2digitshex(disasm_show_short)," Disassembler: always show SHORT keyword",13,10
	db _2digitshex(disasm_show_near), " Disassembler: always show NEAR keyword",13,10
	db _2digitshex(disasm_show_far),  " Disassembler: always show FAR keyword",13,10
	db _2digitshex(disasm_nec),       " Disassembler: NEC V20 repeat rules (for segregs)",13,10
%if _40COLUMNS
	db _4digitshex(disasm_40_columns)," Disassembler: 40-column friendly mode (only 4 bytes machine code per line)",13,10
	db _4digitshex(disasm_no_indent), " Disassembler: do not indent disassembly operands",13,10
%endif
	db _4digitshex(disasm_a16_memref)," Disassembler: access data in a16 referenced memory operand",13,10
	db _4digitshex(disasm_a32_memref)," Disassembler: access data in a32 referenced memory operand",13,10
	db _4digitshex(disasm_a16_string)," Disassembler: simulate repeated a16 scas/cmps string operation",13,10
	db _4digitshex(disasm_a32_string)," Disassembler: simulate repeated a32 scas/cmps string operation",13,10
	asciz
 %endif

 %if _VARIABLES || _OPTIONS || _PSPVARIABLES
.varhelp:
	db "Available "
  %if _PSPVARIABLES && !(_VARIABLES || _OPTIONS)
	db "read-only "
  %endif
	db "lDebug variables:",13,10
  %if _VARIABLES
	db "V0..VF",9,"User-specified usage",13,10
  %endif
  %if _OPTIONS
	db "DCO",9,"Debugger Common Options",13,10
	db "DAO",9,"Debugger Assembler/disassembler Options",13,10
  %endif
  %if _OPTIONS || _PSPVARIABLES && (_OPTIONS || _VARIABLES)
	db " The following variables cannot be written:",13,10
  %endif
  %if _PSPVARIABLES
	db "PSP",9,"Debuggee Process"
  %if _PM
	db " (as real mode segment)"
  %endif
	db 13,10
	db "PPR",9,"Debuggee's Parent Process",13,10
	db "PPI",9,"Debuggee's Parent Process Interrupt 22h",13,10
  %endif
  %if _OPTIONS
	db "DIF",9,"Debugger Internal Flags",13,10
	db "DCS",9,"Debugger Common Startup options",13,10
	db "DAS",9,"Debugger Assembler/disassembler Startup options",13,10
	db "DPR",9,"Debugger Process"
   %if _PM
	db " (as Real mode segment)",13,10
	db "DPS",9,"Debugger Process Selector (zero in real mode)"
   %endif
	db 13,10
	db "DPP",9,"Debugger's Parent Process"
  %if _TSR
	db " (zero in TSR mode)"
  %endif
	db 13,10
	db "DPI",9,"Debugger's Parent process Interrupt 22h"
  %if _TSR
	db " (zero in TSR mode)"
  %endif
	db 13,10
	db 13,10
	db "Enter ?O to display the options and internal flags.",13,10
  %endif
	asciz
 %endif
 %if _BOOTLDR
.boothelp:
	db "Boot loading commands:",13,10
	db "BOOT LIST HDA",13,10
	db "BOOT DIR [partition] [dirname]",13,10
	db "BOOT READ|WRITE [partition] segment [[HIDDEN=sector] sector] [count]",13,10
  %if _DOSEMU
	db "BOOT QUIT",9,"[exits dosemu or shuts down using APM]",13,10
  %else
	db "BOOT QUIT",9,"[shuts down using APM]",13,10
  %endif
	db "BOOT [PROTOCOL=SECTOR] partition",13,10
	db "BOOT PROTOCOL=proto [opt] [partition] [filename1] [filename2] [cmdline]",13,10
	db 9,"the following partitions may be specified:",13,10
	db 9," HDAnum",9,"first hard disk, num = partition (1-4 primary, 5+ logical)",13,10
	db 9," HDBnum",9,"second hard disk (etc), num = partition",13,10
	db 9," HDA",9,"first hard disk (only valid for READ|WRITE|PROTOCOL=SECTOR)",13,10
	db 9," FDA",9,"first floppy disk",13,10
	db 9," FDB",9,"second floppy disk (etc)",13,10
	db 9," LDP",9,"partition the debugger loaded from",13,10
	db 9," YDP",9,"partition the most recent Y command loaded from",13,10
	db 9," SDP",9,"last used partition (default if no partition specified)",13,10
	db 9,"filename2 may be double-slash // for none",13,10
	db 9,"cmdline is only valid for lDOS, RxDOS.2, RxDOS.3 protocols",13,10
	db 9,"files' directory entries are loaded to 500h and 520h",13,10
	db 13,10
	db "Available protocols: (default filenames, load segment, then entrypoint)",13,10
	db " LDOS",9,9,	"LDOS.COM or L[D]DEBUG.COM at 200h, 0:400h",13,10
	db " FREEDOS",9,"KERNEL.SYS or METAKERN.SYS at 60h, 0:0",13,10
	db " DOSC",9,9,	"IPL.SYS at 2000h, 0:0",13,10
	db " EDRDOS",9,9,"DRBIO.SYS at 70h, 0:0",13,10
	db " MSDOS6",9,9,	"IO.SYS + MSDOS.SYS at 70h, 0:0",13,10
	db " MSDOS7",9,9,	"IO.SYS at 70h, 0:200h",13,10
	db " IBMDOS",9,9,	"IBMBIO.COM + IBMDOS.COM at 70h, 0:0",13,10
	db " NTLDR",9,9,	"NTLDR at 2000h, 0:0",13,10
	db " BOOTMGR",9,	"BOOTMGR at 2000h, 0:0",13,10
	db " RXDOS.0",9,"RXDOSBIO.SYS + RXDOS.SYS at 70h, 0:0",13,10
	db " RXDOS.1",9,"RXBIO.SYS + RXDOS.SYS at 70h, 0:0",13,10
	db " RXDOS.2",9,"RXDOS.COM at 70h, 0:400h",13,10
	db " RXDOS.3",9,"RXDOS.COM at 200h, 0:400h",13,10
	db " CHAIN",9,9,"BOOTSECT.DOS at 7C0h, -7C0h:7C00h",13,10
	db " SECTOR",9,9,"(default) load partition boot sector or MBR",13,10
	db " SECTORALT",9,"as SECTOR, but entry at 07C0h:0",13,10
	db 13,10
	db "Available options:",13,10
	db " MINPARA=num",9,9,	"load at least that many paragraphs",13,10
	db " MAXPARA=num",9,9,	"load at most that many paragraphs (0 = as many as fit)",13,10
	db " SEGMENT=num",9,9,	"change segment at that the kernel loads",13,10
	db " ENTRY=[num:]num",9,"change entrypoint (CS (relative) : IP)",13,10
	db " BPB=[num:]num",9,9, \
		"change BPB load address (segment -1 = auto-BPB)",13,10
	db " CHECKOFFSET=num",9,"set address of word to check, must be even",13,10
	db " CHECKVALUE=num",9,9,"set value of word to check (0 = no check)",13,10
	db "Boolean options: [opt=bool]",13,10
	db " SET_DL_UNIT",9,9,"set dl to load unit",13,10
	db " SET_BL_UNIT",9,9,"set bl to load unit",13,10
	db " SET_SIDI_CLUSTER",9,"set si:di to first cluster",13,10
	db " SET_DSSI_DPT",9,9,"set ds:si to DPT address",13,10
	db " PUSH_DPT",9,9,"push DPT address and DPT entry address",13,10
	db " DATASTART_HIDDEN",9,"add hidden sectors to datastart var",13,10
	db " SET_AXBX_DATASTART",9,"set ax:bx to datastart var",13,10
	db " SET_DSBP_BPB",9,9,"set ds:bp to BPB address",13,10
	db " LBA_SET_TYPE",9,9,"set LBA partition type in BPB",13,10
	db " MESSAGE_TABLE",9,9, \
		"provide message table pointed to at 1EEh",13,10
	db " SET_AXBX_ROOT_HIDDEN",9, \
		"set ax:bx to root start with hidden sectors",13,10
	db " NO_BPB",9,9,9, "do not load BPB",13,10
	db " SET_DSSI_PARTINFO",9, "load part table to 600h, point ds:si + ds:bp to it",13,10
	db " CMDLINE",9,9,  "pass a kernel command line (recent FreeDOS extension)",13,10
	asciz
 %endif
%endif

.readonly:	asciz "This lDebug variable cannot be written to. See ?V.",13,10
.readonly_mem:	asciz "This memory variable cannot be written to.",13,10
%if _MMXSUPP
.internal_error_no_mmx:
		asciz "Internal error, MMX variables not supported.",13,10
%endif
%if _PM
.readonly_verifysegm:
		db "Memory using selector "
.readonly_verifysegm.selector:
		asciz "---- is inaccessible for writing.",13,10
%endif
.more:		db "[more]"
 .more_size equ $-.more
.more_over:	db 13,"      ",13		; to overwrite previous prompt
 .more_over_size equ $-.more_over
.ctrlc:		db "^C",13,10
 .ctrlc_size equ $-.ctrlc
 		asciz
.freedos_ctrlc_workaround:
		asciz " (Old FreeDOS kernel Ctrl-C work around happened)",13,10
.not_while_indos:
		asciz "Command not supported while in InDOS mode.",13,10
.rv_mode.before:	asciz "Current mode: "
%if _PM
.rv_mode_dpmi_16:	asciz "DPMI 16-bit CS",13,10
.rv_mode_dpmi_32:	asciz "DPMI 32-bit CS",13,10
%endif
.rv_mode_r86m:		asciz "Real 86 Mode",13,10
.rv_mode_v86m:		asciz "Virtual 86 Mode",13,10

.regs386:	asciz "386 registers are "
.regs386_off:db "not "
.regs386_on:	asciz "displayed",13,10

%if _EMS
.xhelp:
	db "Expanded memory (EMS) commands:",13,10
	db "  Allocate",9,	"XA count",13,10
	db "  Deallocate",9,	"XD handle",13,10
	db "  Map memory",9,	"XM logical-page physical-page handle",13,10
	db "  Reallocate",9,	"XR handle count",13,10
	db "  Show status",9,	"XS",13,10
	asciz
%endif

%if _PM
.desc:	asciz "DESC"
.deschelp:
	db "Descriptor modification commands:",13,10
	db " (only valid in Protected Mode)",13,10
	db "  Allocate",9,	"D.A",13,10
	db "  Deallocate",9,	"D.D selector",13,10
	db "  Set base",9,	"D.B selector base",13,10
	db "  Set limit",9,	"D.L selector limit",13,10
	db "  Set type",9,	"D.T selector type",13,10
	asciz
%endif

%if _MCB
.invmcbadr:	asciz "End of chain: invalid MCB address.",13,10
%endif

%if _TSR
.pspnotfound:	asciz "Cannot go resident, child PSP not found.",13,10
.psphooked:	asciz "Cannot go resident, child PSP parent return address hooked.",13,10
.nowtsr1:	asciz "Patched PSP at "
.nowtsr2:	asciz ", now resident.",13,10
.alreadytsr:	asciz "Already resident.",13,10
%endif
%if _PM && (_TSR || _BOOTLDR)
.cannotpmquit:	asciz "Cannot quit, still in protected mode.",13,10
%endif
%if _PM
.cannotpmload:	asciz "Process loading aborted: Still in protected mode.",13,10
%endif
%if _BOOTLDR
.nobootsupp:	asciz "Command not supported in boot loaded mode.",13,10
.boot_quit_fail:asciz "Shutdown not supported.",13,10
.bootfail:	asciz "Boot failure: "
.bootfail_read:	db "Reading sector failed (error "
.bootfail_read_errorcode:	asciz "__h).",13,10
.bootfail_sig:	asciz "Boot sector signature missing (is not AA55h).",13,10
.bootfail_sig_parttable:	ascii "Partition table signature missing"
				asciz " (is not AA55h).",13,10
.bootfail_code:	asciz "Boot sector code invalid (is 0000h).",13,10
.bootfail_secsizediffer:
		asciz "BPB BpS differs from actual sector size.",13,10
.bootfail_stack_underflow:
		asciz "Boot stack underflowed.",13,10
.bootfail_check_mismatch:
		db "Check mismatch, expected "
.bootfail_check_mismatch.check_value:
		db "____h at offset "
.bootfail_check_mismatch.check_offset:
		db "____h but has "
.bootfail_check_mismatch.check_got:
		asciz "____h.",13,10
.boot_out_of_memory_error:	asciz "Out of memory.", 13,10
.boot_too_many_partitions_error:asciz "Too many partitions (or a loop).",13,10
.boot_partition_cycle_error:	asciz "Partition table cycle detected.",13,10
.boot_partition_not_found:	asciz "Partition not found.",13,10
.boot_access_error:	asciz "Read error.", 13,10
.boot_sector_too_large:	asciz "Sector size too small (< 32 bytes).", 13,10
.boot_sector_too_small:	asciz "Sector size too large (> 8192 bytes).", 13,10
.boot_sector_not_power:	asciz "Sector size not a power of two.", 13,10
.boot_invalid_sectors:	asciz "Invalid geometry sectors.", 13,10
.boot_invalid_heads:	asciz "Invalid geometry heads.", 13,10
.boot_file_not_found:	asciz "File not found.",13,10
.boot_file_too_big_error:	asciz "File too big.",13,10
.boot_file_too_small_error:	asciz "File too small.",13,10
.boot_badclusters:	asciz "Bad amount of clusters.",13,10
.boot_badchain:		asciz "Bad cluster chain.",13,10
.boot_badfat:		asciz "Bad File Allocation Table.",13,10
.boot_invalid_filename:	asciz "Invalid filename.",13,10
.boot_cannot_set_both:	asciz "Cannot set both "
.boot_and:		asciz " and "
.boot_dot_crlf:		asciz ".",13,10
.boot_internal_error:	asciz "! Internal error !",13,10
.boot_bpb_load_overlap:	asciz "BPB and load area overlap.",13,10
.boot_segment_too_low:	asciz "Segment too low.",13,10
.boot_bpb_too_low:	asciz "BPB too low.",13,10
.boot_auxbuff_crossing:	db "! Internal error !, "
			asciz "auxbuff crosses 64 KiB boundary.",13,10
.read:		asciz "READ"
.write:		asciz "WRITE"
.hidden:	asciz "HIDDEN"
.hiddenadd:	asciz "HIDDENADD"
.dir:		asciz "DIR"
.dirinsteadsize:countedb "   [DIR]"
.emptydirname:	asciz "/"
.boot:		asciz "BOOT"
.quit:		asciz "QUIT"
.protocol:	asciz "PROTOCOL"
.segment:	asciz "SEGMENT"
.entry:		asciz "ENTRY"
.bpb:		asciz "BPB"
.minpara:	asciz "MINPARA"
.maxpara:	asciz "MAXPARA"
.checkoffset:	asciz "CHECKOFFSET"
.checkvalue:	asciz "CHECKVALUE"
.sector:	asciz "SECTOR"
.sector_alt:	asciz "SECTORALT"
.freedos_kernel_name:	asciz "KERNEL.SYS"
.dosc_kernel_name:	asciz "IPL.SYS"
.edrdos_kernel_name:	asciz "DRBIO.SYS"
.ldos_kernel_name:	asciz "LDOS.COM"
.msdos7_kernel_name:
.msdos6_kernel_name:	asciz "IO.SYS"
.msdos6_add_name:	asciz "MSDOS.SYS"
.ibmdos_kernel_name:	asciz "IBMBIO.COM"
.ibmdos_add_name:	asciz "IBMDOS.COM"
.ntldr_kernel_name:	asciz "NTLDR"
.bootmgr_kernel_name:	asciz "BOOTMGR"
.chain_kernel_name:	asciz "BOOTSECT.DOS"
.rxdos.0_kernel_name:	asciz "RXDOSBIO.SYS"
.rxdos.1_kernel_name:	asciz "RXBIO.SYS"
.rxdos.0_add_name:
.rxdos.1_add_name:	asciz "RXDOS.SYS"
.rxdos.2_kernel_name:	asciz "RXDOS.COM"
.addname_empty:		asciz
.cannotbootquit_memsizes:	asciz "Cannot quit, memory size changed.",13,10
%endif
.uninstall:	db "UN"
.install:	asciz "INSTALL"
.alreadyenabled:asciz " is already enabled.",13,10
.alreadydisabled:asciz " is already disabled.",13,10
.tryenable:	asciz ": Trying to enable.",13,10
.trydisable:	asciz ": Trying to disable.",13,10
%if _AREAS_HOOK_SERVER
.qqlate_areas_error:
		asciz "Internal error in Q command uninstalling areas.",13,10
%endif
%if _AREAS && _AREAS_HOOK_CLIENT
.areasinstalled:	asciz "Areas installed.",13,10
.areasalreadyinstalled:	asciz "Areas already installed.",13,10
.areasnodebuggerfound:	asciz "Areas not installed, no debugger AMIS interface found!",13,10
.areasnotsupported:	asciz "Areas not installed, debugger AMIS interface does not support function!",13,10
.areasnotinstalled:	db "Areas not installed, debugger returned code "
.areasnotinstalled.code:asciz "--h.",13,10
.areasalreadyuninstalled:	asciz "Areas already uninstalled.",13,10
.areasuninstalled:		db "Areas uninstalled, debugger returned code "
.areasuninstalled.code:		asciz "--h.",13,10
%endif
.then:		asciz "THEN"
.not:		asciz "NOT"
.rvv:		asciz "RVV"
.rvm:		asciz "RVM"
.rvp:		asciz "RVP"
.rvd:		asciz "RVD"
%if _MMXSUPP
.rm:		asciz "RM"
%endif
%if _BOOTLDR
.rvp_boot:		ascizline "Mode: Boot loaded"
%endif
%if _DEVICE
.rvp_device:		ascizline "Mode: Device driver"
%endif
%if _TSR
.rvp_tsr:		ascizline "Mode: Application installed as TSR"
%endif
.rvp_application:	ascizline "Mode: Application"
.vm_codeseg:	counted "Code segment="
%if _DUALCODE
.vm_code2seg:	counted "Code2 segment="
%endif
.vm_dataseg:	counted "Data segment="
.vm_entryseg:	counted "Entry segment="
.vm_auxseg:	counted "Auxbuff segment="
%if _HISTORY_SEPARATE_FIXED && _HISTORY
.vm_hisseg:	counted "History segment="
%endif
%if _PM
.vm_selector:	counted " selector="
%endif
.vp_pspsegment:		counted "Client   PSP="
.vp_dpspsegment:	counted "Debugger PSP="
.vp_dparent:
.vp_parent:		counted " Parent="
.vp_dpra:
.vp_pra:		counted " Parent Return Address="
%if _PM
.vp_dpspsel:
.vp_pspsel:		counted " PSP Selector="
%endif
.rvd_not_device:	asciz "Not loaded in device mode.",13,10
%if _DEVICE
.rvd_deviceheader:	counted "Device header at "
.rvd_size:		counted ". Amount paragraphs allocated is "
%endif
.n_toolongtail:	asciz "Too long N command tail!",13,10
.n_toolongname:	asciz "Too long N command name!",13,10
.number:	asciz "NUMBER"
.counter:	asciz "COUNTER"
.id:		asciz "ID"
.when:		asciz "WHEN"
.offset:	asciz "OFFSET"
.questionmark:	asciz "?"
.or:		db "O"
.r:		asciz "R"
.nd:		asciz "ND"
.lr:		asciz "LR"
.remember:	asciz "REMEMBER"
.goto:		asciz "GOTO"
.sof:		asciz "SOF"
.eof:		asciz "EOF"
.goto_not_file:	asciz "Error: GOTO command not supported when not reading a script.",13,10
.goto_empty:	asciz "Error: GOTO needs a destination label.",13,10
.goto_not_found.1:	asciz "Error: GOTO destination label ",'"'
.goto_not_found.2:	asciz '"'," not found.",13,10
.guard_auxbuff_error:	asciz "Error: auxbuff already guarded!",13,10
.guard_re_error:	asciz "Error: Command not supported while reading from RE buffer.",13,10
.guard_rc_error:	asciz "Error: Command not supported while reading from RC buffer.",13,10
.unexpected_auxbuff_guard:	asciz "Error: Unexpected auxbuff guard!",13,10
%if _SYMBOLIC
.unexpected_nosymbols:		asciz "Error: Unexpected no symbols flag!",13,10
%endif
.unexpected_noneol_re:		asciz "Error: Unexpected non-EOL in RE processing!",13,10
.unexpected_noneol_rc:		asciz "Error: Unexpected non-EOL in RC processing!",13,10
.replace:	asciz "REPLACE"
.append:	asciz "APPEND"
.dword:		db "D"
.word:		asciz "WORD"
.3byte:		db "3"
.byte:		asciz "BYTE"
.lines:		asciz "LINES"
.paragraphs:	asciz "PARAGRAPHS"
.paras:		asciz "PARAS"
.qwords:	asciz "QWORDS"
.dwords:	db "D"
.words:		asciz "WORDS"
.bytes:		asciz "BYTES"
.length:	asciz "LENGTH"
.range:		asciz "RANGE"
%if _VXCHG
.on:		asciz "ON"
.off:		asciz "OFF"
.vv_enable_failure:
		asciz "Unable to enable video swapping.",13,10
.vv_disabled:	asciz "Video swapping is disabled, use V ON to switch it on.",13,10
%endif
.reverse:	asciz "REVERSE"
.value:		asciz "VALUE"
.in:		asciz "IN"
.existing:	asciz "EXISTING"
.from:		asciz "FROM"
.to:		asciz "TO"
.executing:		asciz "EXECUTING"
.executing_value_range:	asciz "FROM LINEAR cs:cip LENGTH abo - cip"
.linear:	asciz "LINEAR"
%if _IMMASM
.immasm_error_eip:
		asciz "Error, branch targets EIP beyond 64 KiB.",13,10
%endif
%if _PM
.desctype:	asciz "DESCTYPE"
%endif
.nottaken:	db "NOT"
.taken:		asciz "TAKEN"
.nt:		db "N"
.t:		asciz "T"
.base:		asciz "BASE"
.group:		asciz "GROUP"
.width:		asciz "WIDTH"
%if _HISTORY
.history_internal_error:
		asciz 13,10,"Internal error in history handling!",13,10
%endif
.di_error:	counted " Error!"
.di_hidden:	counted "hidden "
.di_iisp:	counted " (IISP)"
.di_nonstd_iisp:counted " (nonstandard IISP)"
.di_uninst_iisp:counted " (uninstalled IISP)"
.di_freedos_reloc:
		counted " (FD kernel reloc)"
.di_jmpfar:	counted " (far jmp imm)"
.di_jmpfarindirect:
		counted " (far jmp indirect)"
.di_testhook:	counted " (test hook)"
.di_toomany:	counted " (too many chained handlers)"
.di_empty:	counted " empty MCB name"
.di_system_mcb:	counted " system MCB"
.di_system_upper:
		counted " system in UMA"
.di_system_low:	counted " system in LMA"
.di_hma:	counted " high memory area"
.di_multiplex.1:counted " [mpx:"
.di_multiplex.2:counted "h list:"
.di_multiplex.3:counted "h]"
.header:	asciz "header"
.header.length:	equ $ - 1 - .header
.trailer:	asciz "trailer"
.trailer.length:equ $ - 1 - .trailer
.at:		asciz "AT"
.while:		asciz "WHILE"
.silent:	asciz "SILENT"
.sleep:		asciz "SLEEP"
.seconds:	asciz "SECONDS"
.ticks:		asciz "TICKS"
.re_limit_reached:	asciz "RE processing reached RELIMIT, aborting.",13,10
.rc_limit_reached:	asciz "RC processing reached RCLIMIT, aborting.",13,10
.silent_error:	asciz "! Internal error during silent buffer handling !",13,10
.while_not_true:asciz "While condition not true, returning.",13,10
.while_terminated_before:	asciz "While condition ",'"'
.while_terminated_after:	asciz '"'," no longer true.",13,10
.no_progress:	asciz "No serial comm progress after 5 seconds, giving up. (Keyboard enabled.)",13,10
.serial_request_keep:	asciz 13,10,_PROGNAME," connected to serial port. Enter KEEP to confirm.",13,10
.serial_no_keep_timer:	asciz "No KEEP keyword confirmation after timeout, giving up. (Keyboard enabled.)",13,10
.serial_no_keep_enter:	asciz "No KEEP keyword confirmation, enabling keyboard.",13,10
%if _VXCHG
.nokeep:	db "NO"
%endif
.keep:		asciz "KEEP"
.cannot_hook_2D.invalid:	asciz "Error: Unable to hook interrupt 2Dh due to invalid handler.",13,10
.cannot_hook_2D.nofree:		asciz "Error: Unable to hook interrupt 2Dh, no free multiplex number.",13,10
.serial_cannot_unhook:		db "Warning: "
.serial_cannot_unhook.nowarn:	db "Unable to unhook interrupt "
.serial_cannot_unhook.int:	asciz "--h.",13,10
.serial_cannot_hook:		db "Error: Unable to hook interrupt "
.serial_cannot_hook.new_int:	db "--h because interrupt "
.serial_cannot_hook.old_int:	asciz "--h still hooked.",13,10
.serial_late_unhook:		db "Succeeded in unhooking interrupt "
.serial_late_unhook.int:	asciz "--h.",13,10
.line_out_overflow:	asciz "Internal error, line_out buffer overflowed!",13,10
%if _REGSHIGHLIGHT || _GETLINEHIGHLIGHT
.highlight:	counted 27,"[7m"
 %if _GETLINEHIGHLIGHT
		db 0
 %endif
.unhighlight:	counted 27,"[m"
 %if _GETLINEHIGHLIGHT
		db 0
 %endif
%endif
.prefixes:	asciz " kMGT"
.ll_unterm:	ascizline "Process loading aborted: Attached process didn't terminate!"
.qq_unterm:	ascizline "Cannot quit, attached process didn't terminate!"
%if _PM
.qq_still_pm:	ascizline "Cannot quit, still in PM after attached process terminated!"
%endif
.qq_a_unterminated:	ascizline "Attached process didn't terminate."
.qq_a_terminated:	ascizline "Attached process did terminate."
.ensure_no_memory:	ascizline "Cannot create empty attached process, out of memory!"
%if _DEVICE
.qq_device_none_selected:
 ascizline "Cannot quit normally when loaded as device driver! Try QC or QD command."
.qq_device_no_d:
 ascizline "Cannot quit to device driver initialisation, state modified!"
.qq_device_no_c:
 ascizline "Cannot quit from device driver container, not found!"
 %if _PM
.qq_device_pm:		ascizline "Cannot quit device driver in PM!"
 %endif
	align 2, db 0
.NULblank:		fill 8, 32, db "NUL"
%endif
.c0:	db "C0"
.cr:	db 13

%if _INPUT_FILE_HANDLES || _INPUT_FILE_BOOT
.exists:		asciz "EXISTS"
.y:			asciz "Y"
.yy_requires_filename:	asciz "Y command requires a filename.",13,10
.yy_filename_empty:	asciz "Y command filename is empty.",13,10
.yy_too_many_handles:	asciz "Y command has too many open files.",13,10
.yy_error_file_open:	asciz "Y command failed to open file.",13,10
.yy_no_file:		asciz "Y command limited to label only valid in script file.",13,10
%endif
%if _INPUT_FILE_HANDLES
.yy_no_dos:		asciz "Y command requires DOS to be available.",13,10
.yy_filename_missing_unquote:
			asciz "Y command filename missing ending quote.",13,10
%endif
%if _INPUT_FILE_BOOT
.yy_too_large:		asciz "Y command file too large.",13,10
.yy_empty:		asciz "Y command file empty.",13,10
%endif

%if _SYMBOLIC
.zz_switch_s_received:	asciz "Allocating symbol table buffer of "
.zz_switch_s_received_xms:
			asciz "Allocating XMS symbol table buffer (including transfer buffer) of "
.zz_switch_s_freeing:	asciz "Freeing symbol table buffer.",13,10
.zz_switch_s_indos:	db "Can't change symbol table buffer allocation"
			asciz " while in DOS!",13,10
%if _BOOTLDR
.zz_switch_s_internal_error:
	asciz "Internal error in Z /S switch handling!",13,10
.zz_switch_s_boot_memsize_differ:
	asciz "Cannot change symbol table buffer allocation, memory size changed!",13,10
.zz_switch_s_boot_transfer_too_low:
	asciz "Cannot enlarge symbol table buffer that much, transfer buffer too low!",13,10
.zz_switch_s_boot_loaded_kernel:
	asciz "Cannot change symbol table buffer allocation, kernel has been loaded!",13,10
.zz_switch_s_boot_rpl:
	asciz "Cannot change symbol table buffer allocation, RPL has been loaded!",13,10
%endif
.zz_s_cannot_alloc_transfer:	asciz "Cannot allocate transfer buffer!",13,10
.zz_s_cannot_alloc_target:	asciz "Cannot allocate target buffer!",13,10
.zz_too_full:	asciz "Symbol tables are too full for this reallocation.",13,10
.zz_xms_not_freed_1:	asciz "Unable to free symbol table XMS handle = "
.zz_xms_not_freed_2:	asciz "h.",13,10
.invaliddata:		asciz "Invalid symbol table data!",13,10
 %if _SECOND_SLICE && (_XMS_SYMBOL_TABLE || _BUFFER_86MM_SLICE)
.error_second_slice:	asciz "Invalid symbol table access slice usage!",13,10
 %endif
.main_too_full:		asciz "Symbol main array is too full!",13,10
.main_too_full_crit1:	asciz "Symbol main array is too full! Critical error. (Earlier check succeeded.)",13,10
.hash_too_full_crit1:	asciz "Symbol hash array is too full! Critical error. (Earlier check succeeded.)",13,10
.hash_too_full_crit2:	asciz "Symbol hash array is too full! Critical error. (Main has space.)",13,10
.str_too_full:		asciz "Symbol string heap is too full!",13,10
.str_too_long:		asciz "Symbol string is too long!",13,10
.liststore.main.end.first:	asciz 13,10,"Main total:",9
.liststore.main.free.first:	asciz "Main free:",9
.liststore.main.used.first:	asciz "Main used:",9
.liststore.hash.end.first:	asciz 13,10,"Hash total:",9
.liststore.hash.free.first:	asciz "Hash free:",9
.liststore.hash.used.first:	asciz "Hash used:",9
.liststore.str.end.first:	asciz 13,10,"String total:",9
.liststore.str.free.first:	asciz "String free:",9
.liststore.str.used.first:	asciz "String used:",9
.liststore.second:		asciz " in "
.liststore.third.singular:	asciz " unit",13,10
.liststore.third.plural:	asciz " units",13,10
.liststore.str.first:	asciz "Strings size is "
.liststore.str.unref.first:	asciz "Unreferenced strings size is "
.liststore.str.unref.second:
.liststore.str.second:	asciz " in "
.liststore.str.unref.third.singular:
.liststore.str.third.singular:	asciz " string.",13,10
.liststore.str.unref.third.plural:
.liststore.str.third.plural:	asciz " strings.",13,10
.liststore.str.fourth:	asciz "Average string structure length is <= "
.liststore.str.invalid:	asciz "Error: Average string structure length too large"
.liststore.str.nofourth:asciz "Cannot calculate average string structure length, number of strings is zero"
.liststore.str.last:	asciz ".",13,10
.symhint:
.symhint_store_string:	db "..@symhint_"
.symhint_size:		equ $ - .symhint
			db "store_string_"
.symhint_store_string_size equ $ - .symhint_store_string
.trace_caller:		db "trace_caller"
.trace_caller_size: equ $ - .trace_caller
.trace_here:		db "trace_here"
.trace_here_size: equ $ - .trace_here
.skip_caller:		db "skip_caller_"
.skip_caller_size: equ $ - .skip_caller
.skip_here:		db "skip_here_"
.skip_here_size: equ $ - .skip_here
.asciz:			asciz "ASCIZ"
.zz_list_range_first:	asciz "Range: "
.zz_list_range_second:	asciz "h--"
.zz_list_range_third:	asciz "h"
.zz_list_add_none:	db ""
.zz_list_none:		asciz " No symbols found",13,10
.zz_list_start:		asciz 13,10
.zz_list_between:	asciz
.zz_list_first:		asciz " Linear="
.zz_list_second:	asciz " Offset="
.zz_list_middle:	asciz "h = ",'"'
.zz_list_last:		asciz '"',13,10
.zz_list_end:		asciz
.zz_list_add_range:	asciz "; "
.zz_list_add_first:	asciz "z add linear=("
.zz_list_base:		asciz " + v1"
.zz_list_base_symbol:	asciz " + sl."
.zz_list_add_second:	asciz ") offset="
.zz_list_add_middle:	asciz " symbol='"
.zz_list_add_last:	asciz "'",13,10
.zz_match_add_none:	db ";"
.zz_match_none:		asciz " No symbols found",13,10
.existing_block:	asciz "Symbol already exists and is being blocked.",13,10
.poison_block:		asciz "Symbol definition is poisoned and is being blocked.",13,10
.stat:			asciz "STAT"
.match:			asciz "MATCH"
.add:			asciz "ADD"
.commit:		asciz "COMMIT"
.abort:			asciz "ABORT"
.del:			asciz "DEL"
.delete:		asciz "DELETE"
.unrefstring:		asciz "UNREFSTRING"
.reloc:			asciz "RELOC"
.relocate:		asciz "RELOCATE"
.symbol:		asciz "SYMBOL"
.flags:			asciz "FLAGS"
.sl:			asciz "SL"
.max:			asciz "MAX"
%if _XMS_SYMBOL_TABLE
.zz_no_xms:		asciz "No XMS driver detected!",13,10
.zz_fail_xms_alloc:	asciz "Failed to allocate XMS block!",13,10
.zz_fail_xms_access:	asciz "Failed to access XMS block!",13,10
%endif
.zz_main_hash_mismatch:	asciz "Compaction/expansion failed, differing amounts of hash and main entries.",13,10
.zz_main_not_first:	asciz "Compaction/expansion failed, main array is not first.",13,10
.zz_hash_not_second:	asciz "Compaction/expansion failed, hash array is not second.",13,10
.zz_str_not_third:	asciz "Compaction/expansion failed, string heap is not third.",13,10
.zz_table_not_full:	asciz "Compaction/expansion failed, table is not full.",13,10
.zz_too_much:		asciz "Symbol table size is too large. Internal error!",13,10
.zz_too_short:		asciz "Symbol table size is too short. Internal error!",13,10
.zz_str_overflow:	asciz "String symbol table got too large. Internal error!",13,10
.zz_length_mismatch:	asciz "Symbol table table size mismatch. Internal error!",13,10
.zz_too_small_str:	asciz "String symbol table target is too small.",13,10
.zz_too_small_hash:
.zz_too_small_mainhash:	asciz "Main/hash symbol table target is too small.",13,10
.zz_internal_error_expand:
			asciz "Internal error during symbol table expansion!",13,10
.zz_reloc_amount_none:	asciz "No symbols found in given source range.",13,10
.zz_del_amount_none:	asciz "Symbol not found!",13,10
.zz_reloc_amount_1:		asciz "Relocated "
.zz_del_amount_1:		asciz "Deleted "
.zz_reloc_amount_2.plural:
.zz_del_amount_2.plural:	asciz " symbols.",13,10
.zz_reloc_amount_2.singular:
.zz_del_amount_2.singular:	asciz " symbol.",13,10
.zz_reloc_overflow:	asciz "Cannot relocate, length of source range overflows!",13,10
.bb_sym_too_many:	asciz "Too many symbol breakpoints!",13,10
.bb_sym_beyond_linear:	asciz "Symbol breakpoint linear is beyond reach!",13,10
.bb_sym_beyond_offset:	asciz "Symbol breakpoint offset is beyond reach!",13,10
%endif

%if _BREAKPOINTS
.all:		asciz "ALL"
.new:		asciz "NEW"
.bb_no_new:	asciz "No unused breakpoint left!",13,10
.bb_hit.1:	counted "Hit permanent breakpoint "
.bb_hit.2.nocounter:
		counted 13,10
%if _SYMBOLIC
.bb_sym_hit.1:	counted "Hit symbol breakpoint "
.bb_sym_hit.2.nocounter:
		counted 13,10
%endif
.bb_pass.1:	counted "Passed permanent breakpoint "
.bb_hit.2.counter:
.bb_pass.2:	counted ", counter="
.bb_hit.3.counter.no_id:
.bb_pass.3.no_id:
.bb_hitpass_id.after:
		counted 13,10
.bb_hitpass_id.long:
		counted 13,10," ID: "
.bb_hitpass_id.short:
		counted ", ID: "
.bb_when:	asciz " WHEN "

.bp:		asciz "BP "
.bpenabled:	asciz " +"
.bpdisabled:	asciz " -"
.bpunused:	asciz " Unused"
.bpaddress:	asciz " Lin="
.bpcontent:	asciz " ("
.bpcounter:	asciz ") Counter="
%if 0
BP 00 Unused
BP 00 + Lin=12345678 (CC) Counter=8000
1234567890123456789012345678901234567890
%endif
.bpnone:	asciz "No breakpoints set currently.",13,10
.bpnone_at:	asciz "No breakpoint set at given address currently.",13,10
%endif
.cant_bp_the:			asciz "The "
.cant_bp_type_proceed:		asciz "proceed breakpoint"
.cant_bp_type_permanent:	db    "permanent breakpoint "
.cant_bp_type_permanent.index:	asciz "__"
%if _SYMBOLIC
.cant_bp_type_symbol:		db    "symbol breakpoint "
.cant_bp_type_symbol.index:	asciz "__"
%endif
.cant_bp_type_gg:		asciz " G breakpoint"
.cant_bp_linear:		db    " (linear "
.cant_bp_linear.address1:	db    "----_"
.cant_bp_linear.address2:	asciz "----) "
.cant_bp_write:			asciz "cannot be written."
.cant_bp_restore:		db    "cannot be restored to "
.cant_bp_restore.value:		asciz "__."
%if 0
The 15th G breakpoint (linear 0010_FFFF) cannot be written.
The proceed breakpoint (linear 0010_FFFF) cannot be written.
The permanent breakpoint 0F (linear 0010_FFFF) cannot be written.
The permanent breakpoint 0F (linear 0010_FFFF) cannot be restored to __.
12345678901234567890123456789012345678901234567890123456789012345678901234567890
%endif
.cant_bp_reason:		asciz 13,10," Reason: "
.cant_bp_reason0:		asciz "No error. (Internal error, report!)",13,10
.cant_bp_reason1:		asciz "It is read-only.",13,10
.cant_bp_reason2:		asciz "It is unreachable.",13,10
.cant_bp_reason3:		db    "It has been overwritten with "
.cant_bp_reason3.value:		asciz "__.",13,10
.cant_bp_reasonu:		asciz "Unknown error. (Internal error, report!)",13,10

.list_bp.first:	asciz "   "
.list_bp.second:db " G breakpoint, linear "
.list_bp.address1:
		db "----_"
.list_bp.address2:
		asciz "----"
.list_bp.third:	db ", content "
.list_bp.value:
		asciz "__"
.list_bp_not_cseip: equ crlf
%if _PM
.list_bp_cseip_32:
		asciz " (is at CS:EIP)",13,10
%endif
.list_bp_csip_16:
		asciz " (is at CS:IP)",13,10
.list_bp_none:
		asciz "The G breakpoint list is empty.",13,10
%if 0
   2nd G breakpoint, linear 0003_28D3 $3600:12345678, content CC (is at CS:EIP)
12345678901234567890123456789012345678901234567890123456789012345678901234567890
%endif
.empty_message:	asciz
.list:		asciz "LIST"
.again:		asciz "AGAIN"
%if _SYMBOLIC
.wrt:		asciz "WRT"
%endif
.uu_too_many_repeat:	asciz "Reached limit of repeating disassembly.",13,10
.uu_internal_error:	asciz "Internal error in disassembler!",13,10
.aa_internal_error:	asciz "Internal error in assembler!",13,10
.stack_overflow:	db "Stack overflow occurred, IP="
.stack_overflow.caller:	asciz "____h, due to "
.stack_overflow.indirection:	asciz "expression indirection.",13,10
.stack_overflow.parens:		asciz "expression parentheses.",13,10
.stack_overflow.precedence:	asciz "expression precedence.",13,10
.stack_overflow.value_in:	asciz "expression VALUE x IN y.",13,10
.stack_overflow.linear:		asciz "expression LINEAR.",13,10
%if _PM
.stack_overflow.desctype:	asciz "expression DESCTYPE.",13,10
%endif
.stack_overflow.cond:		asciz "expression conditional ?? x :: y.",13,10
%if _SYMBOLIC
.dd_after_symbol.non_wrt:
.uu_after_symbol.non_wrt:		 db ":"
.dd_after_symbol.2_wrt:
.memref_after_symbol.non_wrt:
.uu_after_symbol.wrt:
.memref_after_symbol.wrt:		asciz 13,10
.dd_after_symbol.1_wrt:
.uu_after_symbol_between_1.wrt:
.uu_between_symbol.wrt:			 db ":"
.memref_between_symbol.wrt:		asciz " wrt "
.uu_after_symbol_between_1.non_wrt:	 db ":"
.uu_after_symbol_between_2.wrt:		asciz " + "
.uu_after_symbol_between_3:		asciz 13,10
%endif
%if _MEMREF_AMOUNT
 %if _DEBUG2 || _SYMBOLIC
.memrefs_branchdirect:	asciz 9, "direct branch target = "
.memrefs_stringsource:	asciz 9, "string source        = "
.memrefs_stringdest:	asciz 9, "string destination   = "
.memrefs_memsource:	asciz 9, "memory source        = "
.memrefs_memdest:	asciz 9, "memory destination   = "
.memrefs_memsourcedest:	asciz 9, "memory source/dest   = "
.memrefs_mem_unknown:	asciz 9, "memory (unknown)     = "
.memrefs_unknown:	asciz 9, "unknown mem ref type = "
.memrefs_length:	counted " length="
 %endif
.memrefs_invalid_internal:
		asciz "Internal error, invalid use of too many memrefs!",13,10
%endif

%if 0
	align 2, db 0
.optiontable:	dw dispregs32, .r32off, .r32on
		dw traceints, .traceoff, .traceon
		dw cpdepchars, .cpoff, .cpon
		dw fakeindos, .dosoff, .doson
		dw nonpagingdevice, .nonpageoff, .nonpageon
		dw pagingdevice, .pageoff, .pageon
		dw hexrn, .readrnoff, .readrnon
		dw 0

.r32off:	asciz "Dump 16-bit register set"
.r32on:		asciz "Dump 32-bit register set"
.traceoff:	asciz "Interrupts are traced"
.traceon:	asciz "Interrupts are processed"
.cpoff:		asciz "Extended ASCII characters replaced"
.cpon:		asciz "Extended ASCII characters displayed"
.dosoff:	asciz "InDOS is checked"
.doson:		asciz "InDOS assumed on"
		;asciz "InDOS assumed off"
.nonpageoff:	asciz
.nonpageon:	asciz "Paging disabled"
.pageoff:	asciz
.pageon:	asciz "Paging enabled"
.readrnoff:	asciz "Readable RN enabled"
.readrnon:	asciz "Readable RN disabled"
%endif

.warnprefix:	asciz "Warning: Prefixes in excess of 14, using trace flag.",13,10

%if _DEBUG
.bu:		asciz "Breaking to next instance.",13,10
 %if _DEBUG_COND
.bu_disabled:	db "Debuggable mode is disabled.",13,10
		asciz "Enable with this command: r DCO6 or= ",_4digitshex(opt6_debug_mode),13,10
 %endif
%else
.notbu:		asciz "Already in topmost instance. (This is no debugging build of lDebug.)",13,10
%endif
%if _DUALCODE
.bu_relocated:	db "Inter-segment calls work. Sign="
.bu_relocated.sign:
		asciz "----h.",13,10
%endif
%if _PM
.ofs32:		asciz "Cannot access 16-bit segment with 32-bit offset.",13,10
.d.a_success:	db "Allocated descriptor with selector "
.d.a_success_sel:
		asciz "----",13,10
.d.d_success:	asciz "Deallocated descriptor",13,10
.d.b_success:	asciz "Set descriptor base",13,10
.d.l_success:	asciz "Set descriptor limit",13,10
.d.t_success:	asciz "Set descriptor type",13,10
.d.a_error:	db "Error "
.d.a_error_code:
		asciz "----",13,10
.d.d_error equ .d.a_error
.d.d_error_code equ .d.a_error_code
.d.b_error equ .d.a_error
.d.b_error_code equ .d.a_error_code
.d.l_error equ .d.a_error
.d.l_error_code equ .d.a_error_code
.d.t_error equ .d.a_error
.d.t_error_code equ .d.a_error_code
%endif


%define smcb_messages ..@notype,""

	%imacro smcbtype 2.nolist
		dw %2, %%label
%defstr %%str	%1
%xdefine smcb_messages smcb_messages,%%label,%%str
	%endmacro

	%imacro smcbmsg 2-*.nolist
%if %0 & 1
 %error Expected even number of parameters
%endif
%rotate 2
%rep (%0 - 2) / 2
%1:	asciz %2
%rotate 2
%endrep
	%endmacro

	align 4, db 0
smcbtypes:
smcbtype S_OTHER,	00h
smcbtype S_DOSENTRY,	01h
smcbtype S_DOSCODE,	02h
smcbtype S_DOSDATA,	03h
smcbtype S_IRQSCODE,	04h
smcbtype S_IRQSDATA,	05h
smcbtype S_CDS,		06h
smcbtype S_LFNCDS,	07h
smcbtype S_DPB,		08h
smcbtype S_UPB,		09h
smcbtype S_SFT,		0Ah
smcbtype S_FCBSFT,	0Bh
smcbtype S_CCB,		0Ch
smcbtype S_IRT,		0Dh
smcbtype S_SECTOR,	0Eh
smcbtype S_NLS,		0Fh
smcbtype S_EBDA,	10h
smcbtype S_INITCONFIG,	19h
smcbtype S_INITFATSEG,	1Ah
smcbtype S_INITSECTORSEG,	1Bh
smcbtype S_INITSTACKBPB,1Ch
smcbtype S_INITPSP,	1Dh
smcbtype S_ENVIRONMENT,	1Eh
smcbtype S_INITIALIZE,	1Fh
smcbtype S_DEVICE,	20h					; Device
smcbtype S_DEVICEMEMORY,21h					; Allocated by device
smcbtype S_EXCLDUMA,	30h					; Excluded UMA
smcbtype S_EXCLDUMASUB,	31h					; Excluded UMA with sub-chain of used MCBs
smcbtype S_EXCLDLH,	32h					; Excluded by LH
smcbtype S_EXCLDDOS,	33h
	dw -1, -1


%define INSTALLMESSAGES db ""

%imacro installflag 4-*.nolist
 %xdefine %%one %1
 %xdefine %%two %2
 %xdefine %%three %3
  %xdefine INSTALLMESSAGES INSTALLMESSAGES, %%message_three:, {asciz %%three}
 %assign %%step 0
 %rep %0 - 3
  %xdefine INSTALLMESSAGES INSTALLMESSAGES, %%message_%[%%step]:, {asciz %4}
	dw %%message_%[%%step]
	dw %%message_three
	dw %%one, %%two
  %rotate 1
  %assign %%step %%step + 1
 %endrep
%endmacro

%imacro installmessages 0-*.nolist
 %rep %0
	%1
  %rotate 1
 %endrep
%endmacro

	align 4, db 0
installflags:
%if _PM
installflag options4, opt4_int_2F_hook, \
	"Interrupt 2Fh DPMI hook", "INT2F", "DPMIHOOK"
%endif
%if _CATCHINT08
installflag options4, opt4_int_08_hook, \
	"Interrupt 8 timer hook", "INT08", "INT8", "TIMER"
%endif
%if _CATCHINT2D
installflag options4, opt4_int_2D_hook, \
	"Interrupt 2Dh AMIS hook", "INT2D", "AMIS"
%endif
%if _AREAS && _AREAS_HOOK_CLIENT
installflag 0, install_areas, \
	"Areas", "AREAS"
%endif
	dw 0, 0


smcbmsg smcb_messages

installmessages INSTALLMESSAGES

smcbmsg_unknown:	asciz "unknown"

%undef smcb_messages
%unimacro smcbtype 2.nolist
%unimacro smcbmsg 2-*.nolist

errcarat:	db "^ Error",7
crlf:		asciz 13,10

%if _SYMBOLIC
pre_str_list:
	db -1, "", 0
.end:
%endif


	align 4, db 0
msgtable_value_range:
	dw msg.executing, msg.executing_value_range
%if _ACCESS_VARIABLES_AMOUNT
	dw .reading, .reading_range
	dw .writing, .writing_range
	dw .memoperand, .memoperand_range
	dw .accessing, .accessing_range
%endif
	dw 0

%if _ACCESS_VARIABLES_AMOUNT
.reading:		asciz "READING"
.reading_range:
 %assign iicounter 0
 %define iiprefix ""
 %rep _ACCESS_VARIABLES_AMOUNT
	_autohexitsstrdef IIDEF, iicounter
			db iiprefix,"FROM readadr",_IIDEF," LENGTH readlen",_IIDEF
 %assign iicounter iicounter + 1
 %define iiprefix ", "
 %endrep
			asciz
.writing:		asciz "WRITING"
.writing_range:
 %assign iicounter 0
 %define iiprefix ""
 %rep _ACCESS_VARIABLES_AMOUNT
	_autohexitsstrdef IIDEF, iicounter
			db iiprefix,"FROM writadr",_IIDEF," LENGTH writlen",_IIDEF
 %assign iicounter iicounter + 1
 %define iiprefix ", "
 %endrep
			asciz

.memoperand:		asciz "MEMOPERAND"
.memoperand_range:	asciz "READING, WRITING"
.accessing:		asciz "ACCESSING"
.accessing_range:	asciz "READING, WRITING, EXECUTING"
%endif


%if _BOOTLDR
%define lot_list
%define lot_comma
%macro lot_entry 2.nolist
LOAD_%2 equ %1
	dw LOAD_%2, .%2
%defstr %%string %2
%xdefine lot_list lot_list lot_comma .%2:, db %%string, db 0
%define lot_comma ,
%endmacro

%macro lot_messages 0-*.nolist
%rep (%0 / 3)
%1
	%2
	%3
%rotate 3
%endrep
%endmacro

	align 4, db 0
loadoptiontable:
	lot_entry    1, SET_DL_UNIT
	lot_entry    2, SET_BL_UNIT
	lot_entry    4, SET_SIDI_CLUSTER
	lot_entry  10h, SET_DSSI_DPT
	lot_entry  20h, PUSH_DPT
	lot_entry  40h, DATASTART_HIDDEN
	lot_entry  80h, SET_AXBX_DATASTART
	lot_entry 100h, SET_DSBP_BPB
	lot_entry 200h, LBA_SET_TYPE
	lot_entry 400h, MESSAGE_TABLE
	lot_entry 800h, SET_AXBX_ROOT_HIDDEN
	lot_entry 1000h, CMDLINE
	lot_entry 2000h, NO_BPB
	lot_entry 4000h, SET_DSSI_PARTINFO
	dw 0, 0

.incompatible:
	dw LOAD_SET_BL_UNIT, LOAD_SET_AXBX_DATASTART
	dw LOAD_SET_BL_UNIT, LOAD_SET_AXBX_ROOT_HIDDEN
	dw LOAD_SET_AXBX_DATASTART, LOAD_SET_AXBX_ROOT_HIDDEN
	dw LOAD_SET_SIDI_CLUSTER, LOAD_SET_DSSI_DPT
	dw LOAD_SET_DSBP_BPB, LOAD_SET_DSSI_DPT
	dw LOAD_NO_BPB, LOAD_SET_DSBP_BPB
	dw LOAD_NO_BPB, LOAD_LBA_SET_TYPE
	dw LOAD_NO_BPB, LOAD_MESSAGE_TABLE
	dw LOAD_SET_DSSI_PARTINFO, LOAD_SET_SIDI_CLUSTER
	dw LOAD_SET_DSSI_PARTINFO, LOAD_SET_DSSI_DPT
	dw LOAD_SET_DSSI_PARTINFO, LOAD_SET_DSBP_BPB
	dw 0, 0

lot_messages lot_list

%unmacro lot_entry 2.nolist
%unmacro lot_messages 0-*.nolist


msdos7_message_table:
		; the first four bytes give displacements to the various
		;  messages. an ASCIZ message indicates that this was the
		;  last message. a message terminated by 0FFh indicates
		;  that the last message (displacement at table + 3) is
		;  to follow after this message.
		; the maximum allowed displacement is 7Fh. the minimum
		;  allowed displacement is 1, to avoid a zero displacement.
		; only the last message is terminated by a zero byte,
		;  as that zero byte indicates the end of the message table.
		;  (the entire table is treated as one ASCIZ string.)
		; MS-DOS 7.10 from MSW 98 SE seems to have at least 167h (359)
		;  bytes allocated to its buffer for these.
		;
		; this message table was discussed in a dosemu2 repo at
		;  https://github.com/stsp/dosemu2/issues/681
.:	db .msg_invalid_system - ($ + 1)
	db .msg_io_error - ($ + 1)
	db .msg_invalid_system - ($ + 1)
	db .msg_press_any_key - ($ + 1)

.msg_invalid_system:
	db 13,10,"Invalid system", -1

.msg_io_error:
	db 13,10,"I/O error", -1

.msg_press_any_key:
	db 13,10,"Change disk and press any key",13,10,0
.end:
.size: equ .end - .

%if .size > 150h
 %error Message table too large!
%endif



	align 4, db 0
loadsettings:
	istruc LOADSETTINGS
at lsKernelName,	dw msg.ldos_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 60h
at lsMaxPara,		dw 0
at lsOptions,		dw LOAD_CMDLINE
at lsSegment,		dw 200h
at lsEntry,		dd 400h
at lsBPB,		dw 7C00h, -1
at lsCheckOffset,	dw 1020
at lsCheckValue,	db "lD"
at lsName,		asciz "LDOS"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.freedos_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 20h
at lsMaxPara,		dw -1
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_BL_UNIT
at lsSegment,		dw 60h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, -1
at lsName,		asciz "FREEDOS"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.dosc_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 20h
at lsMaxPara,		dw -1
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_BL_UNIT
at lsSegment,		dw 2000h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "DOSC"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.edrdos_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 20h
at lsMaxPara,		dw -1
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_BL_UNIT \
			 | LOAD_SET_DSBP_BPB
at lsSegment,		dw 70h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, -1
at lsName,		asciz "EDRDOS"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.msdos6_kernel_name
at lsAddName,		dw msg.msdos6_add_name
at lsMinPara,		dw 20h
at lsMaxPara,		dw 60h
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_AXBX_DATASTART \
			 | LOAD_DATASTART_HIDDEN | LOAD_SET_DSSI_DPT \
			 | LOAD_PUSH_DPT
at lsSegment,		dw 70h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "MSDOS6"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.rxdos.0_kernel_name
at lsAddName,		dw msg.rxdos.0_add_name
at lsMinPara,		dw 20h
at lsMaxPara,		dw 60h
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_AXBX_ROOT_HIDDEN \
			 | LOAD_SET_DSSI_DPT | LOAD_PUSH_DPT
at lsSegment,		dw 70h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "RXDOS.0"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.rxdos.1_kernel_name
at lsAddName,		dw msg.rxdos.1_add_name
at lsMinPara,		dw 20h
at lsMaxPara,		dw 60h
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_AXBX_ROOT_HIDDEN \
			 | LOAD_SET_DSSI_DPT | LOAD_PUSH_DPT
at lsSegment,		dw 70h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "RXDOS.1"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.rxdos.2_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 60h
at lsMaxPara,		dw 0
at lsOptions,		dw LOAD_CMDLINE
at lsSegment,		dw 70h
at lsEntry,		dd 400h
at lsBPB,		dw 7C00h, -1
at lsName,		asciz "RXDOS.2"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.rxdos.2_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 60h
at lsMaxPara,		dw 0
at lsOptions,		dw LOAD_CMDLINE
at lsSegment,		dw 200h
at lsEntry,		dd 400h
at lsBPB,		dw 7C00h, -1
at lsCheckOffset,	dw 1020
at lsCheckValue,	db "lD"
at lsName,		asciz "RXDOS.3"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.ibmdos_kernel_name
at lsAddName,		dw msg.ibmdos_add_name
at lsMinPara,		dw 20h
at lsMaxPara,		dw 80h
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_AXBX_DATASTART \
			 | LOAD_DATASTART_HIDDEN | LOAD_SET_DSSI_DPT \
			 | LOAD_PUSH_DPT
at lsSegment,		dw 70h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "IBMDOS"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.msdos7_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 40h
at lsMaxPara,		dw 80h
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_SIDI_CLUSTER \
			 | LOAD_DATASTART_HIDDEN | LOAD_PUSH_DPT \
			 | LOAD_LBA_SET_TYPE | LOAD_MESSAGE_TABLE
at lsSegment,		dw 70h
at lsEntry,		dd 200h
at lsBPB,		dw 7C00h, -1
at lsCheckOffset,	dw 200h
at lsCheckValue,	db "BJ"
at lsName,		asciz "MSDOS7"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.ntldr_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 20h
at lsMaxPara,		dw -1
at lsOptions,		dw LOAD_SET_DL_UNIT \
			 | LOAD_DATASTART_HIDDEN
at lsSegment,		dw 2000h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "NTLDR"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.bootmgr_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw 20h
at lsMaxPara,		dw -1
at lsOptions,		dw LOAD_SET_DL_UNIT \
			 | LOAD_DATASTART_HIDDEN
at lsSegment,		dw 2000h
at lsEntry,		dd 0
at lsBPB,		dw 7C00h, 0
at lsName,		asciz "BOOTMGR"
	iend
	istruc LOADSETTINGS
at lsKernelName,	dw msg.chain_kernel_name
at lsAddName,		dw msg.addname_empty
at lsMinPara,		dw paras(512)
at lsMaxPara,		dw paras(8192)
at lsOptions,		dw LOAD_SET_DL_UNIT | LOAD_SET_DSSI_PARTINFO \
			 | LOAD_NO_BPB
at lsSegment,		dw 7C0h
at lsEntry,		dw 7C00h, -7C0h
at lsBPB,		dw 7C00h, 0
at lsCheckOffset,	dw 510
at lsCheckValue,	dw 0AA55h
at lsName,		asciz "CHAIN"
	iend
			dw 0
%endif


dskerrs:	db dskerr0-dskerrs,dskerr1-dskerrs
		db dskerr2-dskerrs,dskerr3-dskerrs
		db dskerr4-dskerrs,dskerr9-dskerrs
		db dskerr6-dskerrs,dskerr7-dskerrs
		db dskerr8-dskerrs,dskerr9-dskerrs
		db dskerra-dskerrs,dskerrb-dskerrs
		db dskerrc-dskerrs
dskerr0:	asciz "Write protect error"
dskerr1:	asciz "Unknown unit error"
dskerr2:	asciz "Drive not ready"
dskerr3:	asciz "Unknown command"
dskerr4:	asciz "Data error (CRC)"
dskerr6:	asciz "Seek error"
dskerr7:	asciz "Unknown media type"
dskerr8:	asciz "Sector not found"
dskerr9:	asciz "Unknown error"
dskerra:	asciz "Write fault"
dskerrb:	asciz "Read fault"
dskerrc:	asciz "General failure"
reading:	asciz " read"
writing:	asciz " writ"
drive:		db "ing drive "
driveno:	asciz "_"
msg8088:	asciz "8086/88"
msgx86:		asciz "x86"
no_copr:	asciz " without coprocessor"
has_copr:	asciz " with coprocessor"
has_287:	asciz " with 287"
tmodes:		db "trace mode is "
tmodev:		asciz "_ - interrupts are "
tmode1:		asciz "traced"
tmode0:		asciz "processed"
unused:		asciz " (unused)"
needsmsg:
.:		db "[needs "
.digit_x_ofs: equ $ - .
		db "x8"
.digit_6_ofs: equ $ - .
		db "6]"
needsmsg_L:	equ $-needsmsg
needsmath:	db "[needs math coprocessor]"
needsmath_L:	equ $-needsmath
obsolete:	db "[obsolete]"
obsolete_L:	equ $-obsolete
int0msg:	asciz "Divide error",13,10
int1msg:	asciz "Unexpected single-step interrupt",13,10
int3msg:	asciz "Unexpected breakpoint interrupt",13,10
%if _CATCHINT06
int6msg:	asciz "Invalid opcode",13,10
%endif
%if _CATCHINT08
int8msg:	asciz "Detected Control pressed for a while",13,10
int8_kbd_msg:	asciz "Detected Control pressed for a while (Keyboard enabled)",13,10
runint_ctrlc_msg:
		asciz "Detected double Control-C via serial",13,10
%endif
%if _CATCHINT07
int7msg:	asciz "No x87 present",13,10
%endif
%if _CATCHINT0C
int0Cmsg:	asciz "Stack fault (in R86M)",13,10
%endif
%if _CATCHINT0D
int0Dmsg:	asciz "General protection fault (in R86M)",13,10
%endif
%if _CATCHINT18
int18msg:	asciz "Diskless boot hook called",13,10
%endif
%if _CATCHINT19
int19msg:	asciz "Boot load called",13,10
%endif
%if _CATCHSYSREQ
sysreqmsg:	asciz "SysReq detected",13,10
%endif
%if _PM
 %if _CATCHEXC06
exc6msg:	asciz "Invalid opcode fault",13,10
 %endif
 %if _CATCHEXC0C
excCmsg:	asciz "Stack fault",13,10
 %endif
excDmsg:	asciz "General protection fault",13,10
%endif
%if _PM || _CATCHINT07 || _CATCHINT0C || _CATCHINT0D
 %if _EXCCSIP
excloc:		db "CS:IP="
exccsip:	asciz "    :    ",13,10
 %endif
 %if _AREAS
msg.area_hh_indirection_memory_access:
		asciz "Expression indirection fault: "
msg.area_rr_variable_read_access:
		asciz "Memory variable read access fault: "
msg.area_rr_variable_write_access:
		asciz "Memory variable write access fault: "
msg.area_uu_referenced_memory_access:
		asciz 13,10,"Disassembly referenced memory fault: "
msg.area_uu_simulate_scas:
		asciz "Disassembly SCAS simulation fault: "
msg.area_uu_simulate_cmps:
		asciz "Disassembly CMPS simulation fault: "
msg.area_aa_access:
		asciz "Assembly fault: "
msg.area_dd_access:
		asciz "Dump data fault: "
msg.area_ee_interactive_access:
		db 13,10
msg.area_ee_access:
		asciz "Enter data fault: "
msg.area_rr_access:
		asciz "Register command fault: "
msg.area_run_access:
		asciz "Run command fault: "
msg.area_uu_access:
		asciz "Disassembly fault: "
 %endif
%endif
%if _PM
excEmsg:	asciz "Page fault",13,10
 %if _BREAK_INSTALLDPMI
installdpmimsg:	asciz "Entered Protected Mode",13,10
 %endif
nodosext:	asciz "Command not supported in protected mode without a DOS extender",13,10
nopmsupp:	asciz "Command not supported in protected mode",13,10
 %if _DISPHOOK
dpmihook:	db "DPMI entry hooked, new entry="
dpmihookcs:	asciz "____:",_4digitshex(mydpmientry+DATASECTIONFIXUP),13,10
  %if _DEBUG
dpmihookamis:	asciz "DPMI entry hooked by other debugger with AMIS callout",13,10
  %endif
 %endif
msg.dpmi_no_hook:	asciz "DPMI entry cannot be hooked!",13,10
nodesc:		asciz "resource not accessible in real mode",13,10
;descwrong:	asciz "descriptor not accessible",13,10
gatewrong:	asciz "gate not accessible",13,10
msg.msdos:	asciz "MS-DOS"
descriptor:	db "---- base="
.base:		db "-------- limit="
.limit:		db "-------- attr="
.attrib:	db "----",13,10
		asciz
%endif	; _PM
ph_msg:		asciz "Error in sequence of calls to hack.",13,10

progtrm:	db 13,10,"Program terminated normally ("
progexit:	asciz "____)",13,10
nowhexe:	asciz "EXE and HEX files cannot be written",13,10
nownull:	asciz "Cannot write: no file name given",13,10
wwmsg1:		asciz "Writing "
wwmsg2:		asciz " bytes",13,10
diskful:	asciz "Disk full",13,10
openerr:	db "Error "
openerr1:	asciz "____ opening file",13,10
doserr2:	asciz "File not found",13,10
doserr3:	asciz "Path not found",13,10
doserr5:	asciz "Access denied",13,10
doserr8:	asciz "Insufficient memory",13,10
doserr11:	asciz "Invalid format",13,10

%if _EMS
;emmname:	db "EMMXXXX0"
emsnot:		asciz "EMS not installed",13,10
emserr1:	asciz "EMS internal error",13,10
emserr3:	asciz "Handle not found",13,10
emserr5:	asciz "No free handles",13,10
emserr7:	asciz "Total pages exceeded",13,10
emserr8:	asciz "Free pages exceeded",13,10
emserr9:	asciz "Parameter error",13,10
emserra:	asciz "Logical page out of range",13,10
emserrb:	asciz "Physical page out of range",13,10
	align 2, db 0
emserrs:	dw emserr1,emserr1,0,emserr3,0,emserr5,0,emserr7
		dw emserr8,emserr9,emserra,emserrb
emserrx:	asciz "EMS error "
xaans:		db "Handle created = "
xaans1:		asciz "____",13,10
xdans:		db "Handle "
xdans1:		asciz "____ deallocated",13,10
xrans:		asciz "Handle reallocated",13,10
xmans:		db "Logical page "
xmans1:		db "____ mapped to physical page "
xmans2:		asciz "__",13,10
xsstr1:		db "Handle "
xsstr1a:	db "____ has "
xsstr1b:	asciz "____ pages allocated",13,10
xsstr2:		db "phys. page "
xsstr2a:	db "__ = segment "
xsstr2b:	asciz "____  "
xsstr3:		db "____ of a total "
xsstr3a:	asciz "____ EMS "
xsstr4:		asciz "es have been allocated",13,10
xsstrpg:	asciz "pag"
xsstrhd:	asciz "handl"
xsnopgs:	asciz "no mappable pages",13,10,13,10
%endif

		align 4, db 0
flagbits:
.:		dw 800h,400h,200h, 80h,040h,010h,004h,001h
.amount: equ ($ - .) / 2
flagson:	dw "OV","DN","EI","NG","ZR","AC","PE","CY"
flagsoff:	dw "NV","UP","DI","PL","NZ","NA","PO","NC"
flagnames:	dw "OF","DF","IF","SF","ZF","AF","PF","CF"
%if _40COLUMNS
flagbits_for_40: equ 1____0____0____1____1____0____0____1b
flagbits_for_80: equ 1____1____1____1____1____1____1____1b
flagbits_for_shl: equ 16 - flagbits.amount

		align 2, db 0
shortflagbits:
.:		dw 400h, 200h
.amount: equ ($ - .) / 2
shortflagson:	dw "D ", "E "
shortflagsoff:	dw "U ", "D "
%endif
flagvaron:	db 1
flagvaroff:	db 0		; must be directly behind flagvaron

%if _COND
msg.condnotjump:db "not "
msg.condjump:	asciz "jumping"
%endif

msg.matches:	asciz " matches",13,10

		align 4, db 0
reg8names:	dw "AL","AH","BL","BH","CL","CH","DL","DH"
; Even entries are xL registers, odd ones the xH ones.
; Order matches that of the first four regs entries.

reg16names:	dw "AX","BX","CX","DX","SP","BP","SI","DI"
		dw "DS","ES","SS","CS","FS","GS","IP","FL"
; 32-bit registers are the first eight and last two entries of
;  reg16names with 'E', which are all non-segment registers.
; Segment registers can be detected by the 'S' as second letter.
; FS and GS are the fourth- and third-to-last entries.
; Order matches that of the sixteen regs entries.


		; Table of recognised default (unsigned) types.
		;
		; If any number of characters match, use the type.
		; If an additional "S" is found in front of a valid
		;  type, the type is set to signed. (Word and byte
		;  types are sign-extended to a dword value.)
		;
		; Each odd entry is an alternative name for the even
		;  entry preceding it.
types:
	countedb "BYTE"		; ("B" is hexadecimal)
	countedb "CHAR"		; ("C" is hexadecimal)
	countedb "WORD"
	countedb "SHORT"
	countedb "3BYTE"	; ("3" and "3B" are numeric)
	countedb "3BYTE"
	countedb "DWORD"	; ("D" is hexadecimal)
	countedb "LONG"
.addresses:
	countedb "POINTER"
	countedb "PTR"
	countedb "OFFSET"
	countedb "OFS"
	countedb "SEGMENT"
.end:

maxtypesize equ 7		; size of "SEGMENT" and "POINTER"
