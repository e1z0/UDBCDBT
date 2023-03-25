@echo off

REM Install fresh custom AUTOEXEC.BAT and FDCONFIG.SYS

if "%OCFG%" == "y" goto DoOption
goto AfterOption

:DoOption

set DMNU=1

REM DBX=DosBox, QEM=QEMU, VBX=VirtualBox, VMW=VMware, EMU=Other Emulated
set FEXT=DEF
vinfo /m
rem if errorlevel 1 set FEXT=186
rem if errorlevel 2 set FEXT=286
rem if errorlevel 3
set FEXT=386
if errorlevel 4 set FEXT=486
if errorlevel 5 set FEXT=586
if errorlevel 6 set FEXT=686
if errorlevel 101 set FEXT=DBX
if errorlevel 102 set FEXT=QEM
if errorlevel 103 set FEXT=VBX
if errorlevel 104 set FEXT=VMW
if errorlevel 200 set FEXT=EMU
if "%FEXT%" == "VBX" goto IsVirtualBox
goto NotDOSBox

:IsVirtualBox
set DMNU=2

:NotDOSBox
verrlvl 0

vcls /f %TSF% /b %TSB% /c %TSC% /y2 /h24
vframe /p0 /b %TFB% /f %TFF% %TFS% textbox /t %FLANG% ICONFIGS_FRAME
vecho /k0 /n /t %FLANG% ICONFIGS
vdelay %FDEL%

REM Simple AUTOEXEC.BAT creation

rem Embed Language configuration and codepage.
if not exist %FINSP%\AUTOEXEC.%FEXT% set FEXT=DEF
if not exist %FINSP%\AUTOEXEC.%FEXT% goto SkipAutoExec

type %FINSP%\AUTOEXEC.%FEXT% |grep -B 1000 -i \$LANG_SET\$|grep -iv \$LANG_SET\$>%TEMP%\AUTOEXEC.BAT
if errorlevel 1 goto Failed
if not exist %LANG%\SETLANG.BAT goto NoSetLangBat
echo rem codepage settings >>%TEMP%\AUTOEXEC.BAT
type %LANG%\SETLANG.BAT | grep -iv "@ECHO OFF" | vstr /b/n>>%TEMP%\AUTOEXEC.BAT
if errorlevel 1 goto Failed
:NoSetLangBat
if not exist %LANG%\KEYBOARD.LST goto NoKeyboards
echo rem keyboard settings >>%TEMP%\AUTOEXEC.BAT
type %LANG%\KEYBOARD.LST | grep -i "^VALUE.%FKB%=" | vstr /f = 2 | vstr /b/n>>%TEMP%\AUTOEXEC.BAT
:NoKeyboards
echo. >>%TEMP%\AUTOEXEC.BAT
if errorlevel 1 goto Failed
type %FINSP%\AUTOEXEC.%FEXT% |grep -A 1000 -i \$LANG_SET\$|grep -iv \$LANG_SET\$>>%TEMP%\AUTOEXEC.BAT
if errorlevel 1 goto Failed

rem if character at position 3 or 4 of FTARGET is less or equal to CR
REM then no path is provided
vinfo /k 4 %FTARGET%
if errorlevel 14 goto Sticky
vinfo /k 3 %FTARGET%
if errorlevel 14 goto Sticky
goto NoDDIR

:Sticky
echo %FTARGET% | vstr /n/f \ 2- | set /p DDIR=
if "%DDIR%" == "" goto Sticky
set DDIR=C:\%DDIR%
goto HaveDDIR
:NoDDIR
verrlvl 0
set DDIR=C:\
:HaveDDIR

rem set LANG, DOSDIR and etc in AUTOEXEC.BAT
CALL FDISET.BAT BEGIN %TEMP%\AUTOEXEC.BAT
CALL FDISET.BAT SET $TZ$ %TZ%
CALL FDISET.BAT SET $FLANG$ %LANG%
CALL FDISET.BAT SET $FTARGET$ %DDIR%
CALL FDISET.BAT SET $FDRIVE$ C:
rem CALL FDISET.BAT SET OSNAME "%OS_NAME%"
rem CALL FDISET.BAT SET OSVER "%OS_VERSION%"
rem CALL FDISET.BAT SET OS_NAME "%OS_NAME%"
rem CALL FDISET.BAT SET OS_VERSION "%OS_VERSION%"
CALL FDISET.BAT END %FDRIVE%\FDAUTO.BAT
if errorlevel 1 goto Failed
del %TEMP%\AUTOEXEC.BAT>NUL

:SkipAutoExec
rem set LANG, DOSDIR and etc in FDCONFIG.SYS

rem set FEXT=086
vinfo /m
rem if errorlevel 1 set FEXT=186
rem if errorlevel 2 set FEXT=286
rem if errorlevel 3
set FEXT=386
if errorlevel 4 set FEXT=486
if errorlevel 5 set FEXT=586
if errorlevel 6 set FEXT=686
if errorlevel 101 set FEXT=DBX
if errorlevel 102 set FEXT=QEM
if errorlevel 103 set FEXT=VBX
if errorlevel 104 set FEXT=VMW
if errorlevel 200 set FEXT=EMU

set FCCTY=001
set FCKEY=858
if exist %LANG%\COUNTRY.BAT call %LANG%\COUNTRY.BAT

if not exist %FINSP%\CONFIG.%FEXT% set FEXT=DEF
if not exist %FINSP%\CONFIG.%FEXT% goto SkipFDConfig
CALL FDISET.BAT BEGIN %FINSP%\CONFIG.%FEXT%
CALL FDISET.BAT SET $FLANG$ %LANG%
CALL FDISET.BAT SET $FTARGET$ %DDIR%
CALL FDISET.BAT SET $FCCC$ %FCCTY%
CALL FDISET.BAT SET $FCKC$ %FCKEY%
CALL FDISET.BAT SET $FDEFMENU$ %DMNU%
CALL FDISET.BAT SET $FDRIVE$ C:
CALL FDISET.BAT END %FDRIVE%\FDCONFIG.SYS
if errorlevel 1 goto Failed

:SkipFDConfig
verrlvl 0
set DDIR=

REM Create the installed version ID file.
echo PLATFORM=%OS_NAME%>%FTARGET%\VERSION.FDI
if errorlevel 1 goto Failed
echo VERSION=%OS_VERSION%>>%FTARGET%\VERSION.FDI
if errorlevel 1 goto Failed
grep -i "^RELEASE=" %FINSP%\VERSION.FDI>>%FTARGET%\VERSION.FDI
verrlvl 0

if not "%FVERB%" == "y" goto AfterOption
vcls /f %TSF% /b %TSB% /c %TSC% /y2 /h24
vframe /p0 /b %TFB% /f %TFF% %TFS% textbox /t %FLANG% ICONFIGS_FRAME
vgotoxy /k0 sop
vecho /k0 /n /e /t %FLANG% ICONFIGS_DONE
vdelay %FDEL%

goto AfterOption

:Failed
set FERROR="Copying configuration files."
call FDIFAIL.BAT cc ERROR_CONFIG
verrlvl 1

:AfterOption
set FEXT=
set DMNU=
set FCCTY=
set FCKEY=
