@echo off
rem initialize path
SET PATH=%cdrom%\bin;%srdisk1%:\pcmenu;%path%
SET PCMENU=%SRDISK1%:\pcmenu
ECHO.
ECHO.
ECHO.
ECHO.

:start
ECHO.
ECHO Please make your selection within 10 seconds or default option will load
ECHO.
VECHO 1. /fLightRed MENU (DEFAULT) /a7
VECHO 2. /fgreen Volcov Commander /a7
VECHO 3. /fgreen Reboot /a7
VECHO 4. /fgreen Grub boot loader /a7
VECHO 5. /fgreen Return to command prompt /a7
ECHO.
ECHO.


choice /C:123456q /N /T:1,10
IF ERRORLEVEL 5 goto :end
IF ERRORLEVEL 4 goto :grub
if ERRORLEVEL 3 goto :reboot
IF ERRORLEVEL 2 goto :vc
if ERRORLEVEL 1 goto :menu
ECHO.                                
goto start
:menu
rem %srdisk1%:
call pcmenu.bat
goto start
:grub
ECHO Running grub...
%cdrom%
\grub\grub.exe --config-file="#@\grub\menu.lst"
goto start
:shutdown                      
ECHO Shutting down...
shutdown
goto end
:reboot
ECHO Rebooting...
reset
exit
goto end
:vc
ECHO Running VC
%cdrom%\apps\vc\vc
goto start
:end
echo Returning to shell...

