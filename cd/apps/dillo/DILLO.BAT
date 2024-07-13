@echo off
REM Customize these lines for your system:
REM --------------------------------------
set DILLO=%cdrom%\apps\dillo
set NANOSCR=1024 768 8888

REM -------------------------------------------
REM You shouldn't need to edit below this line.
REM -------------------------------------------

rem set WATTCP.CFG=%DILLO%\BIN
set WATTCP.CFG=%ramd%:\

%DILLO%\bin\dillo.exe %1 %2 %3 %4 %5 %6 %7 %8 %9
rem >dillo.log
