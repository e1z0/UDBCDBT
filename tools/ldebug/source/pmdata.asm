
%if 0

lDebugX PM data

Copyright (C) 1995-2003 Paul Vojta
Copyright (C) 2008-2021 C. Masloch

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif


	usesection lDEBUG_DATA_ENTRY

	align 4, db 0
dpmientry:	dd 0	; DPMI entry point returned by DPMI host
dpmiwatch:	dd 0
dpmi_rm2pm:	dd 0
dpmi_rmsav:	dd 0
dpmi_pm2rm:	dw 0,0,0
dpmi_pmsav:	dw 0,0,0
	align 2, db 0
dssel:		dw 0	; debugger's (16-bit RW) data selector
cssel:		dw 0	; debugger's (16-bit RE) code selector
scratchsel:	dw 0	; scratch selector used for various purposes, limit -1
%if _SYMBOLIC
symsel1:	dw 0	; selector used to access symbol table access slices
symsel2:	dw 0	; another selector used to access symbol table
%endif
%if _IMMASM
immsel:		dw 0	; selector for immasm
%endif
dpmi32:		db 0	; 32-bit client if true
bCSAttr:	db 0	; current code attribute (D bit)
	align 2, db 0
bAddr32:	db 0	; Address attribute. if 1, hiword(edx) is valid
		db 0	; read/written when bAddr32 is pushed/popped


	align 2, db 0
convsegs:
.:
	dw pInDOS + so16aSegSel
%if _USESDA
	dw pSDA + so16aSegSel
%endif
	dw auxbuff_segorsel + soaSegSel
%if _HISTORY_SEPARATE_FIXED && _HISTORY
	dw history.segorsel + soaSegSel
%endif
.end_fixed:
.amount_fixed: equ (.end_fixed - .) / 2
	dw a_addr + saSegSel
	dw d_addr + saSegSel
	dw e_addr + saSegSel
.end:
.amount: equ (.end - .) / 2

exctab:
%if _CATCHEXC00
	db 00h
%endif
%if _CATCHEXC01
	db 01h
%endif
%if _CATCHEXC03
	db 03h
%endif
%if _CATCHEXC06
	db 06h
%endif
%if _CATCHEXC0C
	db 0Ch
%endif
%if _CATCHEXC0D
	db 0Dh
%endif
%if _CATCHEXC0E
	db 0Eh
%endif
endexctab:

%if _CATCHPMINT214C || _DEBUG
	align 8, db 0
excsave:
.:
%if _CATCHEXC00
	dw -1,-1,-1,0	; 0
%endif
%if _CATCHEXC01
	dw -1,-1,-1,0	; 1
%endif
%if _CATCHEXC03
	dw -1,-1,-1,0	; 3
%endif
%if _CATCHEXC06
	dw -1,-1,-1,0	; 6
%endif
%if _CATCHEXC0C
	dw -1,-1,-1,0	; 0C
%endif
%if _CATCHEXC0D
	dw -1,-1,-1,0	; 0D
%endif
%if _CATCHEXC0E
	dw -1,-1,-1,0	; 0E
%endif
.end:
.amount: equ (.end - .) / 8

 %if .amount != (endexctab - exctab)
  %error Wrong excsave length
 %endif
%endif

%if CATCHPMINTAMOUNT
pminttab:
.:
 %if _CATCHPMINT214C
	db 21h
	dw pmint21
 %endif
 %if _CATCHPMINT41
	db 41h
	dw pmint41
 %endif
.end:
.amount: equ (.end - .) / 3

	align 8, db 0
pmintsave:
.:
 %if _CATCHPMINT214C
.int21:	dw -1,-1,-1,0
 %endif
 %if _CATCHPMINT41
.int41:	dw -1,-1,-1,0
 %endif
.end:
.amount: equ (.end - .) / 8
%endif
