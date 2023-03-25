@echo off

rem for debug testing ram drive failure
rem goto NoFDRAMDRV

if not "%1" == "LAST" goto Normal
if not "%2" == "CHANCE" goto Normal
goto LastChance
:Normal
if "%1" == "ALTDRVR" goto Alternate
vinfo /m
if errorlevel 103 goto NotQEMU
REM only initialize QEMU with alternate driver
if errorlevel 102 goto Done
goto NotQEMU

:QEMU
:LastChance
REM Need to use other driver Under QEMU
REM However, regardless of options I've used SRDISK creates drive on the first
REM avalable drive letter (C: if HD isn't partitioned). Maybe, there is some
REM way to get it to skip drive letters. If so, it isn't obvious.
SET FDRAMDRV=
for %%d in ( C D E F G H ) do if "%FDRAMDRV%" == "" CALL %0 ALTDRVR %%d
goto Done

:Alternate
vinfo /d %2:
REM Error 15 - Not Present / Invalid Drive Letter
if errorlevel 16 goto Done
if not errorlevel 15 goto Done
devload srdxms.sys %2:
vinfo /d %2:
REM Error 5 - Not Formated
if errorlevel 6 goto Done
if not errorlevel 5 goto Done
REM Maybe switch this to just /FREEMEM:4M /MAXSIZE
if errorlevel 1 srdisk 8M>NUL
if errorlevel 1 srdisk 4M>NUL
if errorlevel 1 srdisk 2M>NUL
if errorlevel 1 srdisk 1M>NUL
if errorlevel 1 srdisk 512K>NUL
if errorlevel 1 srdisk 256K>NUL
if errorlevel 1 srdisk 128K>NUL
if errorlevel 1 srdisk 64K>NUL
if errorlevel 1 srdisk 32K>NUL
if errorlevel 1 goto Done
REM Error 2 - Removable (yes)
vinfo /d %2:
if errorlevel 3 goto Done
if not errorlevel 2 goto Done
vfdutil /u %2:\TEST????.$$$ >NUL
if errorlevel 1 goto Done
set FDRAMDRV=%2:
goto Done

:NotQEMU
SET RETRY=yes
SET FDRAMDRV=
:Attempt
SHSURDRV /QQ /U >NUL
if errorlevel 2 goto NotLoaded
rem if not errorlevel 1 goto NoFDRAMDRV
:NotLoaded
SHSURDRV /QQ /C /R:%2 /D:%1,R,$$OS_PREFIX$-RAMDRV >NUL
if errorlevel 27 goto NoFDRAMDRV
if errorlevel  1 set FDRAMDRV=A:
if errorlevel  2 set FDRAMDRV=B:
if errorlevel  3 set FDRAMDRV=C:
if errorlevel  4 set FDRAMDRV=D:
if errorlevel  5 set FDRAMDRV=E:
if errorlevel  6 set FDRAMDRV=F:
if errorlevel  7 set FDRAMDRV=G:
if errorlevel  8 set FDRAMDRV=H:
if errorlevel  9 set FDRAMDRV=I:
if errorlevel 10 set FDRAMDRV=J:
if errorlevel 11 set FDRAMDRV=K:
if errorlevel 12 set FDRAMDRV=L:
if errorlevel 13 set FDRAMDRV=M:
if errorlevel 14 set FDRAMDRV=N:
if errorlevel 15 set FDRAMDRV=O:
if errorlevel 16 set FDRAMDRV=P:
if errorlevel 17 set FDRAMDRV=Q:
REM Since RAM disk shouldn't be before R: we could skip 1-17
if errorlevel 18 set FDRAMDRV=R:
if errorlevel 19 set FDRAMDRV=S:
if errorlevel 20 set FDRAMDRV=T:
if errorlevel 21 set FDRAMDRV=U:
if errorlevel 22 set FDRAMDRV=V:
if errorlevel 23 set FDRAMDRV=W:
if errorlevel 24 set FDRAMDRV=X:
if errorlevel 25 set FDRAMDRV=Y:
if errorlevel 26 set FDRAMDRV=Z:
if "%FDRAMDRV%" == "" goto NoFDRAMDRV
goto Done
:NoFDRAMDRV
if "%RETRY%" == "" goto Done
SET RETRY=
goto Attempt
:Done
SET RETRY=

