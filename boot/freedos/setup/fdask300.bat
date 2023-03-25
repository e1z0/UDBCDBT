@echo off

REM if advanced mode prompt to replace system config files.

if "%FADV%" == "" goto AssumeYes

:AdvancedMode
if exist %FDRIVE%\AUTOEXEC.BAT goto AskAdvanced
if exist %FDRIVE%\FDAUTO.BAT goto AskAdvanced
if exist %FDRIVE%\CONFIG.SYS goto AskAdvanced
if exist %FDRIVE%\FDCONFIG.SYS goto AskAdvanced
goto AssumeYes

:AskAdvanced
vcls /f %TSF% /b %TSB% /c %TSC% /y2 /h24
vframe /p0 /b %TFB% /f %TFF% %TFS% textbox /t %FLANG% REPLACE_FRAME
vecho /k0 /t %FLANG% REPLACE?
vecho  /k0
vframe /p0 /b %TFB% /f %TFF% optionbox /t %FLANG% REPLACE_OPTS
vecho /k0 /t %FLANG% REPLACEY
vecho /k0 /n /t %FLANG% REPLACEN
vchoice /k0 /a %TFC% Ctrl-C /d 1

if errorlevel 200 FDICTRLC.BAT %0
if errorlevel 2 goto SkipOption

:AssumeYes
set OCFG=y
verrlvl 0
goto Done

:SkipOption
set OCFG=n
verrlvl 0

:Done
