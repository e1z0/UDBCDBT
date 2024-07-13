@echo off
SET DOSDIR=A:\FREEDOS
SET LANG=
SET TZ=
SET PATH=%dosdir%\BIN

SET DIRCMD=/P /OGN /Y
rem SET COPYCMD=/-Y

rem SET TEMP=%dosdir%\TEMP
rem SET TMP=%TEMP%

if exist %dosdir%\NLS\NUL set NLSPATH=%dosdir%\NLS
if exist %dosdir%\HELP\NUL set HELPPATH=%dosdir%\HELP

cls

if not exist %dosdir%\bin\uhdd.sys goto NoUHDD
DEVLOAD /H %dosdir%\bin\uhdd.sys /s5 /h
:NoUHDD

echo.
call CDROM.BAT

rem FDAPM APMDOS
rem DOSLFN

goto SkipLBACache
if not exist %dosdir%\bin\lbacache.com goto NoLBACache
vecho /g
verrlvl 1
LBACACHE.COM buf 20 flop
if errorlevel 1 goto NoLBACache
if not exist %dosdir%\bin\tickle.com goto NoLBACache
TICKLE.COM
:NoLBACache

:SkipLBACache

SET OS_NAME=FreeDOS
SET OS_VERSION=1.3
SET AUTOFILE=%0
SET CFGFILE=\FDCONFIG.SYS
alias reboot=fdapm warmboot
alias reset=fdisk /reboot
alias halt=fdapm poweroff
alias shutdown=fdapm poweroff

if exist %dosdir%\bin\fdnet.bat call %dosdir%\bin\fdnet.bat start
if exist %dosdir%\bin\fdassist.bat call %dosdir%\bin\fdassist.bat

if not exist SETUP.BAT goto Done
:RunSetup
rem vecho /k0 /p Done processing startup files /fCyan FDCONFIG.SYS /a7 and /fCyan FDAUTO.BAT /a7/p
rem vdelay 1500
rem CALL SETUP.BAT legacy
if exist %cdrom%\utils\pcms130\PCMS.COM goto ramdisk
goto Done

:ramdisk
echo Creating ram drive...
SRDISK /E 320 > NUL
mkdir %SRDISK1%:\temp
set TEMP=%SRDISK1%:\temp
set TMP=%SRDISK1%:\temp
mkdir %SRDISK1%:\pcmenu > NUL
echo Copying files to the ram drive...
copy %cdrom%\utils\pcms130\*.* %SRDISK1%:\pcmenu > NUL
vecho Ram drive was created with letter /fLightGreen %SRDISK1%: /a7
goto menu

:menu
if exist %cdrom%\menu.bat %cdrom%\menu.bat

:Done
if exist %dosdir%\bin\cdrom.bat call %dosdir%\bin\cdrom.bat display
rem if exist %dosdir%\bin\welcome.bat call %dosdir%\bin\welcome.bat
rem %cdrom%\menu.bat
