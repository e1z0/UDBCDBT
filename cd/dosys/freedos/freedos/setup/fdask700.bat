@echo off

REM Installation package selection.
if exist ..\BIN\FDIMPLES.COM set FDIM=..\BIN\FDIMPLES.COM
if exist ..\BIN\FDIMPLES.EXE set FDIM=..\BIN\FDIMPLES.EXE

:Beginning
set TPDC=3
if exist %FINSP%\FDPLFULL.LST goto HasAllPacs
if "%FADV%" == "y" goto BaseAdvOnly
:BaseOnly
set PF=PAC_FRAME_B
set PO=PAC_OPTS_B
set TPDC=1
goto PromptUser
:BaseAdvOnly
if "%FDIM%" == "" goto BaseOnly
set PF=PAC_FRAME_BD
set PO=PAC_OPTS_BD
set TPDC=1
goto PromptUser
:HasAllPacs
if "%FDIM%" == "" goto AllOnly
if "%FADV%" == "y" goto AllAdv
:AllOnly
set PF=PAC_FRAME
set PO=PAC_OPTS
goto PromptUser
:AllAdv
set PF=PAC_FRAME_D
set PO=PAC_OPTS_D
goto PromptUser

:PromptUser
vcls /f %TSF% /b %TSB% /c %TSC% /y2 /h24
vframe /p0 /b %TFB% /f %TFF% %TFS% textbox /t %FLANG% %PF%
vecho /k0 /t %FLANG% PACS? %TFH% %OS_NAME% %TFF%
vecho /k0
vframe /p0 /b %TFB% /f %TFF% optionbox /t %FLANG% %PO%
vecho /k0 /t %FLANG% PACBO
vecho /k0 /n /t %FLANG% PACBS
if not exist %FINSP%\FDPLFULL.LST goto HasNoAll
vecho /k0 /p
vecho /k0 /t %FLANG% PACAO
vecho /k0 /n /t %FLANG% PACAS
:HasNoAll
if "%FDIM%" == "" goto NotAdv
if not "%FADV%" == "y" goto NotAdv
vecho /k0 /p
vecho /k0 /t %FLANG% PACDO_ADV
vecho /k0 /n /t %FLANG% PACDS_ADV
:NotAdv
vchoice /k0 /a %TFC% Ctrl-C /d %TPDC%

if errorlevel 200 FDICTRLC.BAT %0

if errorlevel 6 goto OptDetailedSrc
if errorlevel 5 goto OptDetailed

if not exist %FINSP%\FDPLFULL.LST goto CheckOptShift
if errorlevel 4 goto OptAllSrc
if errorlevel 3 goto OptAll
goto CheckBase

:CheckOptShift
if errorlevel 4 goto OptDetailedSrc
if errorlevel 3 goto OptDetailed

:CheckBase
if errorlevel 2 goto OptBasicSrc
if errorlevel 1 goto OptBasic

:OptBasic
set OALL=n
set OSRC=0
goto Done

:OptBasicSrc
set OALL=n
set OSRC=1
goto Done

:OptAll
set OALL=y
set OSRC=0
goto Done

:OptAllSrc
set OALL=y
set OSRC=1
goto Done

:OptDetailed
set OALL=?
set OSRC=0
if exist %TEMP%\FDIMPLES.LST del %TEMP%\FDIMPLES.LST>nul
%FDIM% /FDI
if errorlevel 200 FDICTRLC.BAT %0
if errorlevel 1 goto Beginning
if not exist %TEMP%\FDIMPLES.LST goto Beginning
goto Done

:OptDetailedSrc
set OALL=?
set OSRC=1
if exist %TEMP%\FDIMPLES.LST del %TEMP%\FDIMPLES.LST>nul
%FDIM% /FDI
if errorlevel 200 FDICTRLC.BAT %0
if errorlevel 1 goto Beginning
if not exist %TEMP%\FDIMPLES.LST goto Beginning
goto Done

:Done
set PF=
set PO=
set FDIM=
set TPDC=
verrlvl 0
