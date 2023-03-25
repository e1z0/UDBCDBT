@ECHO OFF
if not exist %DOSDIR%\BIN\PKGINFO.EXE goto NotInstalled
%DOSDIR%\BIN\PKGINFO.EXE /p %1 %2 %3 %4 %5 %6 %7 %8 %9
goto End
:NotInstalled
echo PKGTOOLS not installed
:End