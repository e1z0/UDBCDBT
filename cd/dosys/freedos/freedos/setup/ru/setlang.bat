@ECHO OFF

set LANG=RU

rem IF NOT EXIST %DOSDIR%\BIN\MKEYB.EXE GOTO NoKeyB
rem mkeyb ru
rem :NoKeyB

IF NOT EXIST %DOSDIR%\BIN\DISPLAY.EXE GOTO NoCodePage
IF NOT EXIST %DOSDIR%\BIN\MODE.COM GOTO NoCodePage
IF NOT EXIST %DOSDIR%\CPI\EGA3.CPX GOTO NoCodePage

LH DISPLAY CON=(EGA,,1)
MODE CON CP PREP=((866) %DOSDIR%\CPI\EGA3.CPX)
MODE CON CP SEL=866
MODE CON CP REFRESH
REM LH KEYB RU,,keybrd3.sys
REM LH CHCP 866

:NoCodePage
