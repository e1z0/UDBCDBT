@echo off
REM *** Continues from script.bat

if %PCIscan%==0 goto NoScan
SHOWMENU Autodecting PCI Networking Card
%ramd%\pciscan.exe %ramd%\ndis.map
if not %PCI0%0==0 goto Detected
REM *** Nothing detected, so dump PCI info ***
echo No known PCI Network Cards detected, running pciscan.exe/v for more info...
%ramd%\pciscan.exe -v
echo.
echo *** PLEASE NOTE: PCMCIA & ISA Cards can NOT be auto-detected! ***
echo.
echo *** Also, pciscan.exe can only autodetect PCI network cards that it knows about.
echo *** Some newer cards, or unpopular pci card drivers that have been sent to me,
echo *** do not have the autodection PCI vender/device ID's included. If you know
echo *** their ID's, please contact me @ www.netbootdisk.com/contact.htm :)
echo.
goto Detected
:NoScan
SHOWMENU *** WARNING *** Automatic PCI Network Card Detection has been disabled!
:Detected

REM *** Display Menu, showmenu.exe reads/writes MenuPref.bat + settemp.bat
SHOWMENU Executing Menu
%ramd%\ShowMenu.exe

SHOWMENU Setting Environment Variables
ctty nul
call %ramd%\MenuPref.bat >nul
call %ramd%\settemp.bat
del %ramd%\settemp.bat
ctty con
set path=%RAMD%\
SHOWMENU Saving Menu Perferences to Floppy Disk
echo Note: Settings will not be saved if disk is write protected or missing.
rem (execute copy command in a 'Fail-Continue' shell (undocumented /f) to prevent abort/retry/fail errors if the disk is write protected!)
ctty nul
%COMSPEC% /f /c copy %ramd%\menupref.bat %a%\NetBoot\menupref.bat
ctty con

SHOWMENU Saving LMHOSTS file to Floppy Disk
ctty nul
%COMSPEC% /f /c copy %ramd%\LMHOSTS %a%\NetBoot\FILECOPY\LMHOSTS
ctty con


SHOWMENU *** FLOPPY DISK CAN NOW BE REMOVED ***

SHOWMENU Loading IFSHLP.SYS
%ramd%\DEVLOAD /H %ramd%\ifshlp.sys


rem ** Hack for SiS900 autodetect speed/duplex issues **
if %NETCARD%==SIS900_1 copy %ramd%\SIS900_1.INI %ramd%\SIS900.INI
if %NETCARD%==SIS900_2 copy %ramd%\SIS900_2.INI %ramd%\SIS900.INI
if %NETCARD%==SIS900_3 copy %ramd%\SIS900_3.INI %ramd%\SIS900.INI
if %NETCARD%==SIS900_4 copy %ramd%\SIS900_4.INI %ramd%\SIS900.INI
if %NETCARD%==SIS900_1 SET NETCARD=SIS900
if %NETCARD%==SIS900_2 SET NETCARD=SIS900
if %NETCARD%==SIS900_3 SET NETCARD=SIS900
if %NETCARD%==SIS900_4 SET NETCARD=SIS900


SHOWMENU Building PROTOCOL.INI
type %ramd%\protocol.1 > %ramd%\PROTOCOL.INI
type %ramd%\%NETCARD%.ini >> %ramd%\PROTOCOL.INI
echo. >> %ramd%\PROTOCOL.INI
type %ramd%\protoADD.txt >> %ramd%\PROTOCOL.INI
echo. >> %ramd%\PROTOCOL.INI
type %ramd%\protocol.2 >> %ramd%\PROTOCOL.INI


SHOWMENU Updating SYSTEM.INI
REM ** RENAME REQUIRED PROTOCOL.INI FILE **

REM ** POKE SETTINGS IN TO SYSTEM.INI **
%ramd%\inifile set %ramd%\system.ini -sNetwork -klogondomain -v%WORKGROUP%
%ramd%\inifile set %ramd%\system.ini -sNetwork -kworkgroup -v%WORKGROUP%
%ramd%\inifile set %ramd%\system.ini -sNetwork -kcomputername -v%wkstn%
%ramd%\inifile set %ramd%\system.ini -sNetwork -klanroot -v%ramd%
%ramd%\inifile set %ramd%\system.ini -sNetwork -kpreferredredir -v%REDIRECTOR%
%ramd%\inifile set %ramd%\system.ini -s"network drivers" -kdevdir -v%ramd%
%ramd%\inifile set %ramd%\system.ini -s"network drivers" -knetcard -v%NETCARD%.DOS
 
if %packet%==1 goto P
SHOWMENU Unbinding Packet Driver
%ramd%\inifile set %ramd%\system.ini -s"network drivers" -ktransport -vtcpdrv.dos,nemm.dos
:P

if %DISABLEDHCP%==0 goto D
SHOWMENU Applying Static TCP/IP Settings
%ramd%\inifile set %ramd%\protocol.ini -stcpip -kDisableDHCP -v%DISABLEDHCP%
if not %DEFAULTGATEWAY0%0==0 %ramd%\inifile set %ramd%\protocol.ini -stcpip -kDEFAULTGATEWAY0 -v"%DEFAULTGATEWAY0%"
%ramd%\inifile set %ramd%\protocol.ini -stcpip -kIPADDRESS0 -v"%IPADDRESS0%"
if not %SUBNETMASK0%0==0 %ramd%\inifile set %ramd%\protocol.ini -stcpip -kSUBNETMASK0 -v"%SUBNETMASK0%"
if not %dns%0==0 %ramd%\inifile set %ramd%\tcputils.ini -sdnr -kNAMESERVER0 -v"%dns%"
if not %WINS%0==0 %ramd%\inifile set %ramd%\protocol.ini -stcpip -kWINS_SERVER0 -v"%WINS%"
:D

if not %CD%==1 goto C
SHOWMENU Loading IDE CD-ROM Driver...
call cdrom.bat
:C

SHOWMENU Loading Network Card Driver & Starting Networking...
@call %ramd%\startnet.bat

SHOWMENU Finding MAC Address...
%ramd%\nbmac.exe|%ramd%\set2var mac>%temp%\mac.bat
call %temp%\mac.bat

SHOWMENU Generating IP Config Summary...
echo @echo off > %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==0 echo echo *** DHCP IP CONFIGURATION *** >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==0 echo @ipconfg %ramd% >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo *** STATIC IP CONFIGURATION *** >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo. >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo IP Address     : %IPADDRESS0% >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo Subnet Mask    : %SUBNETMASK0% >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo Default Gateway: %DEFAULTGATEWAY0% >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo DNS Server     : %DNS% >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo WINS Server    : %WINS% >> %ramd%\IPCONFIG.BAT
if %DISABLEDHCP%==1 echo echo. >> %ramd%\IPCONFIG.BAT
if %DNSENABLE%==1 echo echo DNS Support: Enabled >> %ramd%\IPCONFIG.BAT 
if %DNSENABLE%==0 echo echo DNS Support: Disabled >> %ramd%\IPCONFIG.BAT
if %PACKET%==1 echo echo Packet Driver Support: Enabled >> %ramd%\IPCONFIG.BAT 
if %PACKET%==0 echo echo Packet Driver Support: Disabled >> %ramd%\IPCONFIG.BAT
echo echo Hardware MAC Address: %MAC% >> %ramd%\IPCONFIG.BAT

If not %UDMA%==1 goto U
SHOWMENU Loading UltraDMA hard-disk Driver...
if not %CD%==1 %ramd%\DEVLOAD /H UIDEJR.SYS /N2
if %CD%==1 %ramd%\DEVLOAD /H UIDEJR.SYS /D:CD
:U

If not %MOUSE%==1 goto M
SHOWMENU Loading Mouse Driver...
%ramd%\ctmouse.exe
:M

echo.
ECHO -------------------------------------------------------------------------------
if %PROBLEM%==1 goto Stuffed
echo UNIVERSAL TCP/IP NETWORK BOOTDISK - Script Complete!
ECHO -------------------------------------------------------------------------------
echo This Computer is called \\%WKSTN%.  Hardware MAC Address: %MAC%
if %DISABLEDHCP%==0 echo To view DHCP'd TCP/IP settings, type: IPCONFIG
if %DISABLEDHCP%==1 echo IP Address: %IPADDRESS0% SubNet: %SUBNETMASK0% Gateway: %DEFAULTGATEWAY0%
if %DISABLEDHCP%==1 echo.
if %DISABLEDHCP%==1 echo *** WARNING: Planning on using GHOST.EXE with a static IP address?         ***
if %DISABLEDHCP%==1 echo *** Be aware that wattcp.cfg + ghost.exe need to be in the same directory, ***
if %DISABLEDHCP%==1 echo *** otherwise Ghost will default to using DHCP configuration!              ***
if %DISABLEDHCP%==1 echo *** So copy %ramd%\wattcp.cfg to the directory that contains ghost.exe, or *** 
if %DISABLEDHCP%==1 echo *** alternatively copy your ghost.exe to the %ramd%\ ramdrive before you   ***
if %DISABLEDHCP%==1 echo *** run ghost.                                                             ***
if %DISABLEDHCP%==1 echo.

if %PCI0%0==0 echo.
if %PCI0%0==0 echo ===== NO PCI NETWORK CARD WAS AUTO-DETECTED ===================================
if %PCI0%0==0 echo Please type PCISCAN -V to discover what the PCI auto-detection PnP ID's in this
if %PCI0%0==0 echo computer are. You can report this information on the NetBootDisk.com/forums, so
if %PCI0%0==0 echo this can be included in the next version of the disk.
if %PCI0%0==0 echo ===============================================================================
if %PCI0%0==0 echo.

echo Type NET VIEW to browse the network, or NET VIEW \\COMPUTER to browse a PC.
echo Or to map to a network drive, type NET USE [drive]: \\SERVER\SHARE
echo.
if not %LOGONSCRIPT%0==0 SHOWMENU Executing Logon Script...
if not %LOGONSCRIPT%0==0 call %LOGONSCRIPT%
:end
del %ramd%\files.uha
Set WORKGROUP=
Set USERNAME=
Set PASSWORD=
Set DISABLEDHCP=
Set IPADDRESS0=
Set SUBNETMASK0=
Set DEFAULTGATEWAY0=
Set DNS=
Set DNSEnable=
Set Packet=
Set LOGONSCRIPT=
Set PCI0=
set CMDLINE=
set PROBLEM=
set MOUSE=
set REDIRECTOR=
set CD=
set UDMA=
set WINS=
goto TheEnd

:Stuffed
echo ERROR! Part of the Network Client and/or Drivers has failed to load. 
echo The scripted failed while executing this command: %CMDLINE%
echo Please note any error message(s) given above.
echo.
echo Now Aborting...
goto TheEnd
:TheEnd