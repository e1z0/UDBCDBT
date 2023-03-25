@echo off

REM Check state of drive %FDRIVE% (C:), check if it needs formatted.
REM If so, prompt to run format, then retest. If fails again,
REM prompt to reboot.

vinfo /m
if errorlevel 102 goto NotDOSBox
if errorlevel 101 goto IsDOSBox
:NotDOSBox
verrlvl 0

vcls /f %TSF% /b %TSB% /c %TSC% /y2 /h24

if not "%1" == "" goto %1

vinfo /d %FDRIVE%

if errorlevel 5 goto NotFormatted
if errorlevel 2 goto WrongTypeDrive

:IsDOSBox
REM Drive %FDRIVE% is formatted.
verrlvl 0
goto Done

:WrongTypeDrive
goto Done

:NotFormatted
if "%FADV%" == "y" goto AdvancedFormat
vframe /p0 /b %TFB% /f %TFF% %TFS% textbox /t %FLANG% NOFORMAT_FRAME
vecho /k0 /t %FLANG% NOFORMAT %TFH% %FDRIVE% %TFF%
vecho /k0
vecho /k0 /t %FLANG% FORMAT?
vframe /p0 /b %TFB% /f %TFF% optionbox /t %FLANG% NOFORMAT_OPTS
vecho /k0 /t %FLANG% FORMAT_YES %FDRIVE%
vecho /k0 /n /t %FLANG% EXIT
vchoice /k0 /a %TFC% Ctrl-C /d 2

if errorlevel 200 FDICTRLC.BAT %0
if errorlevel 2 goto AbortBatch

set FDIDFMT=QuickFormat
goto DoFormat

:AdvancedFormat
vframe /p0 /b %TFB% /f %TFF% %TFS% textbox /t %FLANG% NOFORMATADV_FRAME
vecho /k0 /t %FLANG% NOFORMAT %TFH% %FDRIVE% %TFF%
vecho /k0
vecho /k0 /t %FLANG% FORMAT?
vframe /p0 /b %TFB% /f %TFF% optionbox /t %FLANG% NOFORMATADV_OPTS
vecho /k0 /t %FLANG% FORMATADV_QUICK %FDRIVE%
vecho /k0 /t %FLANG% FORMATADV_SLOW %FDRIVE%
vecho /k0 /n /t %FLANG% EXIT
vchoice /k0 /a %TFC% Ctrl-C /d 3

if errorlevel 200 FDICTRLC.BAT %0
if errorlevel 1 set FDIDFMT=QuickFormat
if errorlevel 2 set FDIDFMT=SlowFormat
if errorlevel 3 goto AbortBatch
goto DoFormat

:DoFormat
vcls /f %TSF% /b %TSB% /y2 /h24
vgotoxy down
vecho /k0 /t %FLANG% FORMATTING %FDRIVE%
vecho /k0
if "%FCURSOR%" == "" vcursor small
if not "%FCURSOR%" == "" vcursor %FCURSOR%
goto %FDIDFMT%

:QuickFormat
REM **** Launch Formatting Program ****
format %FDRIVE% /V:%OVOL% /Q /U /Z:seriously
REM **** Returned from Formatting ****
goto DoneFormat

:SlowFormat
REM **** Launch Formatting Program ****
format %FDRIVE% /V:%OVOL% /U /Z:seriously
REM **** Returned from Formatting ****
goto DoneFormat

:DoneFormat
set FDIDFMT=y

vcursor hide
vgotoxy eop sor
vecho /k0 /n /t %FLANG% PAUSE
vpause /fLightCyan /t %FWAIT% CTRL-C

if errorlevel 200 FDICTRLC.BAT %0 AfterFormat

:AfterFormat
vinfo /d %FDRIVE%

if errorlevel 2 goto StillCannotReadC

verrlvl 0
goto Done

:StillCannotReadC
set FERROR="Unable to read drive %FDRIVE% after format."
call FDIFAIL.BAT ERROR_READC

:AbortBatch
verrlvl 1

:Done
