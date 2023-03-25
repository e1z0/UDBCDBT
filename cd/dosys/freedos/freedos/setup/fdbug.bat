@echo off

REM Debug mode only. To display current environment variables.

if not "%FDEBUG%" == "y" goto Done

if "%1" == "cls" goto ClearScreen
if "%1" == "shell" goto StartShell
if "%1" == "" goto Default
goto %1

:Default
vcls /g /a 0x07
set | more

:Pause
vgotoxy /g eop sor
if not "%2" == "" vecho /k0 %2 %3 %4 %5 %6 %7 %8 %9
if "%FLANG%" == "" vecho /k0 /n "Press a key to continue... "
if not "%FLANG%" == "" vecho /k0 /n /t %FLANG% PRESSKEY
vpause /t 30 CTRL-C
if errorlevel 200 goto StartShell
goto Resume

:ClearScreen
vcls /g /a 0x07
goto Done

:StartShell
vgotoxy /g eop sor
if not "%2" == "" vecho /k0 %2 %3 %4 %5 %6 %7 %8 %9
if not "%COMSPEC%" == "" goto StartComspec
COMMAND.COM
goto Resume

:StartComspec
%COMSPEC%
goto Resume

:Resume
call FDICLS.BAT
goto Done

:Done
