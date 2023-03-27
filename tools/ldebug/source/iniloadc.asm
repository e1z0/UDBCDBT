
%if 0

iniload config generation script

Usage of the works is permitted provided that this
instrument is retained with the works, so that any entity
that uses the works is notified of this instrument.

DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

%endif

%assign __lMACROS1_MAC__SHOW_EXPLICIT 0
%assign __lMACROS1_MAC__SHOW_EXPLICIT_DEFAULT 0
%assign __lMACROS1_MAC__SHOW_EXPLICIT_VALUE 0
%include "debug.mac"

	db '; File auto-generated, do not edit.',13,10
%if _DEBUG && _DEBUG_COND
	db 'strdef INILOAD_SIGNATURE, "bC"',13,10
%elif _DEBUG
	db 'strdef INILOAD_SIGNATURE, "Db"',13,10
%else
	db 'strdef INILOAD_SIGNATURE, "eb"',13,10
%endif
%if _DEVICE
	db '%assign _DEVICE 1',13,10
	db '%assign _DEVICE_ZERO_ENTRYPOINT 1',13,10
	db '%assign _DEVICE_ATTRIBUTE 8000h',13,10
	db '%define _DEVICE_NAME "LDEBUG$$"',13,10
%else
	db '%assign _DEVICE 0',13,10
%endif
