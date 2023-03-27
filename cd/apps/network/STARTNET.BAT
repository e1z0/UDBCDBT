set PROBLEM=0
if not %NETCARD%0==NDIS0 goto NV
SHOWMENU Loading Nvidia/Intel Support Driver
devload %ramd%:\NVNDIS2.DOS
if errorlevel==1 goto Abort
:NV

if exist %ramd%:\%NETCARD%.BAT SHOWMENU Executing NIC Driver Script
if exist %ramd%:\%NETCARD%.BAT call %ramd%:\%NETCARD%.BAT

SHOWMENU Initializing Network Client
%ramd%:\net initialize
if not errorlevel==0 goto Abort
SHOWMENU Starting NetBind
%ramd%:\netbind.com
if not errorlevel==0 goto Abort
SHOWMENU Starting UMB
%ramd%:\umb.com
if not errorlevel==0 goto Abort
SHOWMENU Starting TCP/IP
%ramd%:\tcptsr.exe
if not errorlevel==0 goto Abort
SHOWMENU Starting TinyRFC
%ramd%:\tinyrfc.exe
if not errorlevel==0 goto Abort
SHOWMENU Starting Ping Support
%ramd%:\nmtsr.exe
if not errorlevel==0 goto Abort
SHOWMENU Starting EMSbfr
%ramd%:\EMSBFR.EXE
if not errorlevel==0 goto Abort
if not %DNSENABLE%==1 goto NoDNS
SHOWMENU Starting DNS
if %DISABLEDHCP%==0 if not "%DNS%"=="" %ramd%:\dnr.exe
if not %DISABLEDHCP%==0 %ramd%:\dnr.exe
if not errorlevel==0 goto Abort
:NoDNS
SHOWMENU Logging on to Network
echo %password%>%temp%\pwd.txt
echo.>>%temp%\pwd.txt
if not %problem%==1 %ramd%:\net logon %username% %password% /yes /savepw:no<%temp%\pwd.txt
del %temp%\pwd.txt
goto end
:Abort
Set Problem=1
:end