@ECHO OFF
REM Copyright 2021-2022 Jerome Shidel
REM GNU General Public License, version 2 or later

set _CDROM.DEVID=FDCDX001
set _CDROM.OPTS=/D:%_CDROM.DEVID%
set _CDROM.ALTO=/D:FDCDR001
set _CDROM.CACHE=no

set _CDROM.LANG=%NLSPATH%\CDROM.%LANG%
if not exist %_CDROM.LANG% set _CDROM.LANG=%NLSPATH%\CDROM.EN
if not exist %_CDROM.LANG% set _CDROM.LANG=%DOSDIR%\BIN\CDROM.BAT
if "%1" == "/?" goto ShowHelp
if /I "%1" == "/H" goto ShowHelp
if /I "%1" == "HELP" goto ShowHelp
if /I "%1" == "/D" goto ShowStatus
if /I "%1" == "DISPLAY" goto ShowStatus
if /I "%1" == "STATUS" goto ShowStatus
goto Start

:ShowHelp
if not exist %HELPPATH%\CDROM.%LANG% goto ShowHelpEn
type %HELPPATH%\CDROM.%LANG%
goto End

:ShowHelpEn
if not exist %HELPPATH%\CDROM.EN goto ShowHelpless
%HELPPATH%\CDROM.%LANG%
goto End

:ShowHelpless
vecho/g /t %_CDROM.LANG% NO_HELP
goto End

:ShowStatus
if "%CDROM%" == "" vecho/g /t %_CDROM.LANG% CD.NONE
if not "%CDROM%" == "" vecho/g /t %_CDROM.LANG% CD.DRIVE %CDROM% %CDROMID%
goto End

rem Built-in English Translations
CD.INIT=CD-ROM initialization.
CD.GIVEUP=/p /fLightRed "unable to load an appropriate CD/DVD driver" /fGrey
CD.ERROR=, /fGrey error "#%1" - /fLightRed failed /fGrey /p
CD.NO_DRVR= /fGrey unable to locate /fYellow "%1" /fGrey CD driver - /fLightRed failed /fGrey
CD.TRY_DRVR= /g attempting to use the /fYellow "%1" /fGrey CD driver
CD.TRY_CACHE= /g attempting to load the /fYellow "%1" /fGrey CD caching
CD.TRY_EXT= attempting to load /fYellow "%1" /fGrey CD extenstions
CD.SUCCESS=, /fLightGreen success /fGrey
CD.STATUS=successfully started the CD driver and extenstions for drive /fLightGreen "%1" /fDarkGrey (%2) /fGrey
CD.DRIVE=CD-ROM configured as /fLightGreen %1 /fGrey drive
CD.NONE=/fLightRed CD-ROM not configured /fGrey
NO_HELP=unable to locate help file

:Start
vecho /g /t %_CDROM.LANG% CD.INIT

if /I "%1" == "SEARCH" goto SearchDriver
if "%1" == "" goto SearchDriver
set _CDROM.DRVR=%1
goto TryDriver

:SearchDriver
rem try un-supplied non-OSS drivers first, if present
set _CDROM.DRVR=AHCICD
goto TryDriver
:Not-AHCICD
set _CDROM.DRVR=VIDE-CDD
goto TryDriver
:Not-VIDE-CDD
set _CDROM.DRVR=OAKCDROM
goto TryDriver
:Not-OAKCDROM
set _CDROM.DRVR=GSCDROM
goto TryDriver
:Not-GSCDROM

rem try supplied OSS drivers
set _CDROM.DRVR=UDVD2
goto TryDriver
:Not-UDVD2
set _CDROM.DRVR=ELTORITO
set _CDROM.CACHE=yes
goto TryDriver
:Not-ELTORITO
set _CDROM.DRVR=GCDROM
goto TryDriver
:Not-GCDROM
set _CDROM.DRVR=UIDE
goto TryDriver
:Not-UIDE
set _CDROM.DRVR=ATAPICDD
goto TryDriver
:Not-ATAPICDD

:GiveUp
vecho /g /t %_CDROM.LANG% CD.GIVEUP
set _CDROM.DRVR=
vdelay 2000
verrlvl 1
goto End

:Failed
set _CDROM.CACHE=
vecho /g /n /t %_CDROM.LANG% CD.ERROR %ERRORLEVEL%
:FailedRetry
vdelay 1000
vecho /g /fGrey
if /I "%1" == "SEARCH" goto Not-%_CDROM.DRVR%
if "%1" == "" goto Not-%_CDROM.DRVR%
goto GiveUp

:MissingDriver
if /I "%1" == "SEARCH" goto Not-%_CDROM.DRVR%
if "%1" == "" goto Not-%_CDROM.DRVR%
vecho /g /n /t %_CDROM.LANG% CD.NO_DRVR %_CDROM.DRVR%
goto FailedRetry

:TryDriver
set _CDROM.TOPTS=%_CDROM.OPTS%
if not "%_CDROM.CACHE%" == "yes" goto TryNoCDRCache
if not exist %DOSDIR%\BIN\CDRCACHE.SYS goto TryNoCDRCache
set _CDROM.TOPTS=%_CDROM.ALTO%
:TryNoCDRCache
if not exist %DOSDIR%\BIN\%_CDROM.DRVR%.SYS goto AltLocation
vecho /g /n /t %_CDROM.LANG% CD.TRY_DRVR %_CDROM.DRVR%
DEVLOAD /H %DOSDIR%\BIN\%_CDROM.DRVR%.SYS %_CDROM.TOPTS% >NUL
if errorlevel 1 goto Failed
goto MaybeCache

:AltLocation
if not exist %_CDROM.DRVR%.SYS goto MissingDriver
vecho /g /n /t %_CDROM.LANG% CD.TRY_DRVR %_CDROM.DRVR%
DEVLOAD /H %_CDROM.DRVR%.SYS %_CDROM.TOPTS% >NUL
if errorlevel 1 goto Failed

:MaybeCache
vecho /g /t %_CDROM.LANG% CD.SUCCESS
if "%_CDROM.TOPTS%" == "%_CDROM.OPTS%" goto TryExtender
if not "%_CDROM.CACHE%" == "yes" goto TryNoCDRCache
if not exist %DOSDIR%\BIN\CDRCACHE.SYS goto TryExtender
vecho /g /n /t %_CDROM.LANG% CD.TRY_CACHE CDRCACHE.SYS
DEVLOAD /H %DOSDIR%\BIN\CDRCACHE.SYS FDCDR001 %_CDROM.DEVID% 1024 >NUL
if errorlevel 1 goto FailedCache
vecho /g /t %_CDROM.LANG% CD.SUCCESS
goto TryExtender

:FailedCache
vecho /g /n /t %_CDROM.LANG% CD.ERROR %ERRORLEVEL%
set _CDROM.DEVID=FDCDR001

:TryExtender
set CDROMID=%_CDROM.DEVID%
vecho /g /n /t %_CDROM.LANG% CD.TRY_EXT SHSUCDX
SHSUCDX /QQ /~ /D:?%CDROMID%,D >NUL
if errorlevel 27 goto FailedExtender
SHSUCDX /QQ /L:1
if errorlevel 27 goto FailedExtender
if not errorlevel 1 goto Failed

if errorlevel 1 set CDROM=A:
if errorlevel 2 set CDROM=B:
if errorlevel 3 set CDROM=C:
if errorlevel 4 set CDROM=D:
if errorlevel 5 set CDROM=E:
if errorlevel 6 set CDROM=F:
if errorlevel 7 set CDROM=G:
if errorlevel 8 set CDROM=H:
if errorlevel 9 set CDROM=I:
if errorlevel 10 set CDROM=J:
if errorlevel 11 set CDROM=K:
if errorlevel 12 set CDROM=L:
if errorlevel 13 set CDROM=M:
if errorlevel 14 set CDROM=N:
if errorlevel 15 set CDROM=O:
if errorlevel 16 set CDROM=P:
if errorlevel 17 set CDROM=Q:
if errorlevel 18 set CDROM=R:
if errorlevel 19 set CDROM=S:
if errorlevel 20 set CDROM=T:
if errorlevel 21 set CDROM=U:
if errorlevel 22 set CDROM=V:
if errorlevel 23 set CDROM=W:
if errorlevel 24 set CDROM=X:
if errorlevel 25 set CDROM=Y:
if errorlevel 26 set CDROM=Z:

vecho /g /t %_CDROM.LANG% CD.SUCCESS
vecho /g /t %_CDROM.LANG% CD.STATUS %CDROM% %CDROMID%
vdelay 500
goto End

:FailedExtender
vecho /g /n /t %_CDROM.LANG% CD.ERROR %ERRORLEVEL%

:EndError
verrlvl 1

:End
set _CDROM.DRVR=
set _CDROM.LANG=
set _CDROM.DEVID=
set _CDROM.OPTS=
set _CDROM.ALTO=
set _CDROM.CACHE=
set _CDROM.TOPTS=
