@echo off
echo SAMPLE LOGON SCRIPT 
echo.
echo *** You'll have to modify this script to suit your network. ***

if not %NTFS%==1 goto N
SHOWMENU Loading Read-Only NTFS Support...
REM *** NTFSDOS - EDIT BELOW TO SUIT YOUR LAN, ETC ***
net use x: \\server\share
ntfs
REM *** NTFSDOS - EDIT END
:N

REM --- EXAMPLE --- Automatically run Ghost from a network share ---
REM net use g: \\computer\share
REM g:
REM ghost.exe -rb
REM
REM Note: the -rb = reboot when done :)



