@echo off
break off

rem Welcome to MS-DOS 7.10 Installation.
rem Copyright Microsoft Corp. 1981-1999. All rights reserved.
rem This program is Free Software under GNU GPL. WITHOUT ANY WARRANTY.
rem See the MS-DOS 7.10 License Agreement for more details.

if %1#==NextStep# goto ret1
if %1#==ChPath1# goto chpath1
if %1#==ChPath2# goto chpath2
if %1#==AddOns2# goto addons2
set tmpdest=
set swch=
if %1#==/?# goto help
if %1#==/D# set swch=1
if %1#==/d# set swch=1
if %1#==/R# set swch=2
if %1#==/r# set swch=2
if %1#==/N# set swch=3
if %1#==/n# set swch=3
if %swch%#==# set ins_path=%1
cls
echo.
echo  MS-DOS 7.10 Setup
echo ===================
echo.
ECHO Starting MS-DOS 7.10 Installation...
echo.
echo Setup is now checking files...
set error=
for %%x in (setup.inf w.bat wbat.com dready.com suscr.exe) do if not exist %%x set error=1
if not %error%#==1# goto noerror
echo Fatal Error: Cannot find the installation information!
echo Please make sure you start SETUP from the installation path.
echo.
pause
goto error

:noerror
echo Done.
echo.
suscr.exe /D " MS-DOS 7.10 Setup " /M "Initializing..." /F DOSÿSetup
echo Please wait while Setup initializes..
for %%x in (ret1 ret2 ret3 ret4) do set %%x=
set ret1=abcdefghijklmnopqrstuvwxyz_1234567890.
if not %ret1%#==abcdefghijklmnopqrstuvwxyz_1234567890.# goto novar
set ret2=%ret1%
if not %ret2%#==%ret1%# goto novar
set ret3=%ret1%
if not %ret3%#==%ret1%# goto novar
set ret4=%ret1%
if not %ret4%#==%ret1%# goto novar
for %%x in (ret1 ret2 ret3 ret4 ret wdir ins_dir) do set %%x=
if %os%#==Windows_NT# goto underwin
set windir=
if not %windir%#==# goto underwin

:sysboot
if %sysboot%#==YES# goto check386
call w.bat box @SETUP.INF:w_sysboot
if errorlevel 2 if not errorlevel 3 set ret=check386
if errorlevel 2 if not errorlevel 3 goto quit

:check386
cpudet.com
if errorlevel 3 goto i386
call w.bat box @SETUP.INF:w_no386 #2
if errorlevel 2 goto error

:i386
set xmss=
if not exist dos71_1s.pak goto ramd
dready.com a: > nul
if errorlevel 1 goto copypak
if not exist a:\dos71_1s.pak goto copypak
goto ramd

:copypak
call w.bat box @SETUP.INF:w_copypak
if errorlevel 1 if not errorlevel 2 set xmss=2880

:ramd
if not exist XMSXXXX0 goto noramd
if %swch%#==2# goto noramd
set tmpdest=r:
if not %xmss%#==2880# set xmss=768
if exist xmsdsk.exe xmsdsk.exe %tmpdest% %xmss% /y>nul
dready.com %tmpdest% /w
if errorlevel 1 goto noramd

:presetup
suscr.exe /M "Copying files needed for MS-DOS 7.10 Setup..."
copy COMMAND.COM %tmpdest% >nul
if not exist %tmpdest%\COMMAND.COM if %tmpdest%#==r:# goto noramd
if not exist %tmpdest%\COMMAND.COM if %tmpdest%#==c:\~dos71in.tmp# goto notmpdir
copy ATTR.COM %tmpdest% >nul
copy BOOTPART.EXE %tmpdest% >nul
copy CHANGE.EXE %tmpdest% >nul
copy CPUDET.COM %tmpdest% >nul
copy DELTREE.COM %tmpdest% >nul
copy DREADY.COM %tmpdest% >nul
copy FIND.EXE %tmpdest% >nul
copy FINDCD.EXE %tmpdest% >nul
copy GDISK.EXE %tmpdest% >nul
copy INIFILE.COM %tmpdest% >nul
copy MCD.EXE %tmpdest% >nul
copy MESSAGES.EXE %tmpdest% >nul
copy PKUNZIP.EXE %tmpdest% >nul
copy RESET1.STP %tmpdest%\RESET1.BAT >nul
copy RESET2.STP %tmpdest%\RESET2.BAT >nul
copy SHUTDOWN.COM %tmpdest% >nul
copy SETUP.INF %tmpdest% >nul
copy SETUP.BAT %tmpdest% >nul
copy SUSCR.EXE %tmpdest% >nul
copy W.BAT %tmpdest% >nul
copy WBAT.COM %tmpdest% >nul
copy XMSDSK.EXE %tmpdest% >nul
copy XFIND.COM %tmpdest% >nul
if %xmss%#==2880# if exist DOS71_1S.PAK copy DOS71_1S.PAK %tmpdest% >nul
if %xmss%#==2880# if exist DOS71_2S.PAK copy DOS71_2S.PAK %tmpdest% >nul
if not exist %tmpdest%\SETUP.BAT if %tmpdest%#==r:# goto noramd
if not exist %tmpdest%\SETUP.BAT if %tmpdest%#==c:\~dos71in.tmp# goto notmpdir
echo    Done.
if %COMSPEC%#==A:\COMMAND.COM# set COMSPEC=%tmpdest%\COMMAND.COM
set TEMP=%tmpdest%\
set TMP=%tmpdest%\
if %tmpdest%#==r:# r:
if %tmpdest%#==c:\~dos71in.tmp# c:
if %tmpdest%#==c:\~dos71in.tmp# cd \~dos71in.tmp
setup.bat NextStep

:noramd
if not %swch%#==2# echo.
if not %swch%#==2# echo    Warning: Cannot create RAM Drive properly!
suscr.exe /M "Creating a temporary folder for Setup files..."
dready.com c: /w
if errorlevel 1 goto notmpdir
echo.
set tmpdest=c:\~dos71in.tmp
if exist %tmpdest% attr.com -s -h -r %tmpdest% > nul
if exist %tmpdest% del %tmpdest%
if not exist %tmpdest%\nul md %tmpdest% > nul
if exist %tmpdest%\nul goto presetup

:notmpdir
set tmpdest=
call w.bat box @SETUP.INF:w_notmpdir
goto error

:ret1
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
call w.bat box @SETUP.INF:w_welcome
if errorlevel 2 set ret=ret1
if errorlevel 2 goto quit

:ret2
call w.bat box @SETUP.INF:w_intr
if errorlevel 2 set ret=ret2
if errorlevel 2 goto quit

:ret3
set ret=
set wrb=
call w.bat box @SETUP.INF:w_agree
if %wrb%#==2# set ret=ret3
if errorlevel 2 set ret=ret3
if %ret%#==ret3# goto quit
set missfile=
for %%x in (ATTR.COM BOOTPART.EXE CHANGE.EXE CPUDET.COM DELTREE.COM DREADY.COM FIND.EXE FINDCD.EXE) do if not exist %%x set missfile=%%x
for %%x in (GDISK.EXE INIFILE.COM MCD.EXE MESSAGES.EXE PKUNZIP.EXE SHUTDOWN.COM XFIND.COM SUSCR.EXE) do if not exist %%x set missfile=%%x
if not %missfile%#==# goto nofile
set cdnum=
if %swch%#==3# set addons=0
if not %addons%#==0# set addons=1
if %ins_path%#==# set ins_path=C:\DOS71
if %xmss%#==2880# set ins_dir=.
if %xmss%#==2880# if exist dos71_1s.pak goto detdc
set ins_dir=A:

:chkdir
dready.com %ins_dir% > nul
if not errorlevel 1 if exist %ins_dir%\diskvol.txt if exist %ins_dir%\dos71_1s.pak goto detdc
if not %cdnum%#==# goto insdir
set cdnum=1

:chkcd
if %cdnum%#==# goto insdir
set cdrom=
findcd.exe %cdnum% > nul
if %cdrom%#==# goto insdir
dready.com %cdrom% > nul
if not errorlevel 2 if exist %cdrom%\dos71\diskvol.txt if exist %cdrom%\dos71\dos71_1s.pak set ins_dir=%cdrom%\dos71
if %ins_dir%#==%cdrom%\dos71# goto chkdir
if %cdnum%#==4# goto insdir
if %cdnum%#==3# set cdnum=4
if %cdnum%#==2# set cdnum=3
if %cdnum%#==1# set cdnum=2
goto chkcd

:insdir
set cdrom=
set cdnum=
call w.bat box @SETUP.INF:w_insdir
if not errorlevel 2 goto chkdir
set ret=chkdir
set ins_dir= 
goto quit

:detdc
set cdrom=
set cdnum=
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
if %swch%#==1# goto path
call w.bat box @SETUP.INF:w_detdc
if errorlevel 2 if not errorlevel 3 goto mbr
echo Setup will now check the partitions on your hard drives.
echo.
echo    Please ensure that there is no error on your hard drives.
echo    You can verify this by running SCANDISK or similar first.
echo.
suscr.exe /M "Setup is checking partitions on your hard drives..."
gdisk.exe 1|find.exe "No partitions defined" > nul
if not errorlevel 1 goto invdc
gdisk.exe 1|find.exe " PRI DOS"|find.exe "DOS"|find.exe "FAT" > nul
if errorlevel 1 goto nofatdc
gdisk.exe 1|find.exe " PRI DOS"|find.exe "C:"|find.exe "Unformatted"|find.exe "FAT" > nul
if not errorlevel 1 goto nofmtdc
goto chkact

:invdc
call w.bat box @SETUP.INF:w_invdc
if errorlevel 2 goto error
gdisk.exe 1 /cre /pri /for /q /v:MSDOS710
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
call w.bat box @SETUP.INF:w_reboot
suscr.exe /M "Setup is now restarting your system..."
shutdown.com r

:nofatdc
call w.bat box @SETUP.INF:w_nofatdc
goto error

:nofmtdc
call w.bat box @SETUP.INF:w_nofmtdc
if errorlevel 2 goto error
pkunzip.exe -o -s:MSDOS %ins_dir%\DOS71_1S.PAK FORMAT.COM > nul
if not exist format.com goto noformat
format.com /?|find "Incorrect MS-DOS version" > nul
if not errorlevel 1 goto noformat
command.com /f /c format.com c: /v:MSDOS710 /autotest

:chkact
gdisk.exe 1|find.exe "H    PRI DOS"|find.exe "FAT" > nul
if not errorlevel 1 call w.bat box @SETUP.INF:w_hfatdc
gdisk.exe 1|find.exe "      PRI DOS"|find.exe "C:"|find.exe "FAT" > nul
if not errorlevel 1 goto noactdc
goto chkhd

:noformat
call w.bat box @SETUP.INF:w_noformat
goto error

:noactdc
call w.bat box @SETUP.INF:w_noactdc
if errorlevel 1 if not errorlevel 2 gdisk.exe 1 /act /p:1

:chkhd
dready.com c: /w > nul
if errorlevel 1 goto nohd
echo    Done.

:mbr
suscr.exe /M "Setup is processing MBR..."
call w.bat box @SETUP.INF:w_mbr #2
if errorlevel 1 if not errorlevel 2 gdisk.exe 1 /mbr > nul

:path
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
call w.bat box @SETUP.INF:w_path
if errorlevel 2 set ret=path
if errorlevel 2 goto quit
goto chpath1

:chins
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
set wcb1=%addons%
call w.bat box @SETUP.INF:w_chins
set addons=%wcb1%
set wcb1=
if errorlevel 2 set ret=chins
if errorlevel 2 goto quit
if %wrb%#==1# set chins=Full installation
if %wrb%#==2# set chins=DOS commands only
if %wrb%#==3# set chins=Minimal DOS system

:ados
set iad=N/A
if not #%chins%==#Full goto ready
call w.bat box @SETUP.INF:w_ados
if errorlevel 2 if not errorlevel 3 set iad=No
if not %iad%#==No# set iad=Yes

:ready
if %addons%#==1# set iao=Yes
if not %iao%#==Yes# set iao=No
call w.bat box @SETUP.INF:w_ready
set iao=
if errorlevel 3 set ret=ready
if errorlevel 3 goto quit
if errorlevel 2 goto path
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
echo MS-DOS 7.10 is now being installed to your system.
messages.exe a
suscr.exe /M "Setup is unpacking files on Installation Disk 1..."
set prog=1
messages.exe %prog%
if exist %ins_path%\~dosetmp attr.com -s -h -r %ins_path%\~dosetmp
if exist %ins_path%\~dosetmp del %ins_path%\~dosetmp
if not exist %ins_path%\~dosetmp\nul md %ins_path%\~dosetmp
if #%chins%==#Full set prog=26
if not #%chins%==#Full set prog=51
messages.exe %prog% 1
pkunzip.exe -d -o -s:MSDOS %ins_dir%\dos71_1s.pak %ins_path%\~dosetmp > nul
if not #%chins%==#Full goto checkzip
set noadd=

:chgdisk
suscr.exe /M ""
messages.exe 51 %prog%
set prog=51
if %xmss%#==2880# goto unpd2
if exist %ins_dir%\diskvol.txt if exist %ins_dir%\dos71_2s.pak goto chgdisk2
call w.bat box @SETUP.INF:w_changedisk2
if errorlevel 2 goto skipdisk2

:chgdisk2
dready.com %ins_dir% > nul
if errorlevel 2 goto disk2notready
if not exist %ins_dir%\diskvol.txt goto notdisk2

:unpd2
if not exist %ins_dir%\dos71_2s.pak goto notdisk2
suscr.exe /M "Setup is unpacking files on Installation Disk 2..."
messages.exe 76 %prog%
set prog=76
pkunzip.exe -d -o -s:MSDOS %ins_dir%\dos71_2s.pak %ins_path%\~dosetmp > nul
goto checkzip

:disk2notready
call w.bat box @SETUP.INF:w_disknotready
if errorlevel 2 goto skipdisk2
goto chgdisk2

:notdisk2
call w.bat box @SETUP.INF:w_notdisk2
if errorlevel 2 goto skipdisk2
goto chgdisk2

:skipdisk2
call w.bat box @SETUP.INF:w_skipdisk2 #2
if errorlevel 2 goto chgdisk
set noadd=1
goto checkzip

:checkcd
if %cdnum%#==# goto addons1
set cdrom=
findcd.exe %cdnum% > nul
if %cdrom%#==# goto addons1
dready.com %cdrom% > nul
if not errorlevel 2 if exist %cdrom%\dos71\addons\autorun\diskvol.txt if exist %cdrom%\dos71\addons\autorun\setup.dat set add_dir=%cdrom%\dos71\addons\autorun
if %add_dir%#==%cdrom%\dos71\addons\autorun# goto addons2
if %cdnum%#==4# goto addons1
if %cdnum%#==3# set cdnum=4
if %cdnum%#==2# set cdnum=3
if %cdnum%#==1# set cdnum=2
goto checkcd

:addons
if not %addons%#==1# goto cfgsys
messages.exe d
suscr.exe /M ""
set add_dir=A:
call w.bat box @SETUP.INF:w_addons
if errorlevel 2 goto cleanadd
suscr.exe /M "Setup is locating Add-Ons..."
dready.com %add_dir%: > nul
if not errorlevel 2 if exist %add_dir%\diskvol.txt if exist %add_dir%\setup.dat if exist %add_dir%\autorun.dat goto addons2
set cdnum=1
goto checkcd

:addons1
set cdrom=
set cdnum=
if %1#==AddOns2# goto exit
call w.bat box @SETUP.INF:w_addisk
if errorlevel 2 goto skipaddon

:addons2
set cdrom=
set cdnum=
dready.com %add_dir%: > nul
if errorlevel 2 goto disknotready
if not exist %add_dir%\diskvol.txt goto notaddisk
if not exist %add_dir%\setup.dat goto notaddisk
if exist %tmpdest%\~addons.bat attr.com -s -h -r %tmpdest%\~addons.bat > nul
set addname=
inifile.com %add_dir%\setup.dat [Setup] Addname > %tmpdest%\~ADDONS.BAT
if not exist %tmpdest%\~addons.bat goto invaddisk
call %tmpdest%\~addons.bat
if %addname%#==# goto invaddisk
set addvers=
inifile.com %add_dir%\setup.dat [Setup] Addvers > %tmpdest%\~ADDONS.BAT
if not exist %tmpdest%\~addons.bat goto invaddisk
call %tmpdest%\~addons.bat
if %addvers%#==# set addvers=N/A
set addtext=
inifile.com %add_dir%\setup.dat [Setup] Addtext > %tmpdest%\~ADDONS.BAT
if not exist %tmpdest%\~addons.bat goto invaddisk
call %tmpdest%\~addons.bat
if %addtext%#==# goto invaddisk
set destzip=
inifile.com %add_dir%\setup.dat [Setup] Destzip > %tmpdest%\~ADDONS.BAT
call %tmpdest%\~addons.bat
if %destzip%#==# goto invaddisk
if not exist %add_dir%\%destzip% goto invaddisk
set useaddon=
inifile.com %add_dir%\setup.dat [Setup] Useaddon > %tmpdest%\~ADDONS.BAT
call %tmpdest%\~addons.bat
if %useaddon%#==0# goto invaddisk
set useaddon=
if not exist %ins_path%\~dosetmp\addons\%addname%\nul goto firstadd
call w.bat box @SETUP.INF:w_readd #2
if errorlevel 2 goto addons1

:firstadd
if not exist %add_dir%\autorun.dat goto useaddon
set autorun=
inifile.com %add_dir%\autorun.dat [Auto] Autorun > %tmpdest%\~ADDONS.BAT
call %tmpdest%\~addons.bat
if %autorun%#==1# goto add
set autorun=

:useaddon
suscr.exe /M ""
call w.bat box @SETUP.INF:w_useaddon
if errorlevel 2 goto addons1

:add
suscr.exe /M "Setup is processing Add-Ons..."
set autorun=
mcd.exe /m /n %ins_path%\~dosetmp\addons\%addname% > nul
if not exist %ins_path%\~dosetmp\addons\%addname%\nul goto addfail
if exist %ins_path%\~dosetmp\addons\%addname%\autorun.bat attr.com -s -h -r %ins_path%\~dosetmp\addons\%addname%\autorun.bat > nul
if exist %ins_path%\~dosetmp\addons\%addname%\autorun.bat del %ins_path%\~dosetmp\addons\%addname%\autorun.bat
pkunzip.exe -o -d %add_dir%\%destzip% %ins_path%\~dosetmp\addons\%addname% > nul
if not exist %ins_path%\~dosetmp\addons\%addname%\autorun.bat goto addfail
call %ins_path%\~dosetmp\addons\%addname%\autorun.bat
goto setagain

:addfail
call w.bat box @SETUP.INF:w_addfail

:setagain
if %1#==AddOns2# goto exit
suscr.exe /M ""
call w.bat box @SETUP.INF:w_setagain
if errorlevel 2 goto cleanadd
goto addons1

:disknotready
call w.bat box @SETUP.INF:w_disknotready
if errorlevel 2 goto addons1
goto addons2

:notaddisk
call w.bat box @SETUP.INF:w_notaddisk
if errorlevel 2 goto addons1
goto addons2

:invaddisk
call w.bat box @SETUP.INF:w_invaddisk
if errorlevel 2 goto addons1
goto addons2

:skipaddon
call w.bat box @SETUP.INF:w_skipaddon #2
if errorlevel 2 goto addons1

:cleanadd
for %%x in (add_dir useaddon addname addvers addtext destzip) do set %%x=
goto cfgsys

:checkw9x
suscr.exe /M "Setup is checking Windows 9x/ME..."
xfind.com "winbootdir" c:\msdos.sys > nul
if errorlevel 1 goto copy
if exist %tmpdest%\~winpath.bat attr.com -s -h -r %tmpdest%\~winpath.bat > nul
inifile.com c:\msdos.sys [Paths] winbootdir > %tmpdest%\~WINPATH.BAT
if not exist %tmpdest%\~winpath.bat goto chkoldos
change.exe /i %tmpdest%\~winpath.bat "set winbootdir" "set wdir" > nul
call %tmpdest%\~winpath.bat
if %wdir%#==# set wdir=.
if %wdir%#==.# goto chkoldos
xfind.com "4.90.3000" c:\msdos.sys > nul
if not errorlevel 1 goto wmewarn
call w.bat box @SETUP.INF:w_w9xpath
if errorlevel 1 if not errorlevel 2 inifile.com %ins_path%\~dosetmp\msdos.sys [Paths] winbootdir=%wdir%

:chkoldos
for %%x in (io.dos msdos.dos command.dos) do if not exist c:\%%x goto copy
if exist %tmpdest%\~btoldos.bat attr.com -s -h -r %tmpdest%\~btoldos.bat > nul
inifile.com c:\msdos.sys [Options] BootMulti > %tmpdest%\~BTOLDOS.BAT
if not exist %tmpdest%\~btoldos.bat goto copy
call %tmpdest%\~btoldos.bat
if %BootMulti%#==1# set sysdos=4
set BootMulti=
goto copy

:wmewarn
call w.bat box @SETUP.INF:w_wmewarn
if errorlevel 2 set ret=copy
if errorlevel 2 goto quit
goto copy

:checknt
suscr.exe /M "Setup is checking Windows NT/2K/XP+..."
dready.com c: /w > nul
if errorlevel 1 goto nodc
if exist c:\ntldr if exist c:\boot.ini goto sysnt

:bpw
echo y|bootpart.exe dos71 boot:c: > nul

:checkdos
suscr.exe /M "Setup is checking your previous version of MS-DOS..."
set sysdos=
if exist c:\io.sys if exist c:\msdos.sys goto sysdos

:copy
messages.exe c
suscr.exe /M "Setup is copying files to the destination..."
if exist %ins_path%\*.* attr.com -s -h -r %ins_path%\*.* > nul
goto %chins%
goto error

:success
suscr.exe /D " MS-DOS 7.10 Setup " /M "Setup is now completing the installation..." /F DOSÿSetup
call w.bat box @SETUP.INF:w_success
call w.bat box @SETUP.INF:w_reqboot #2
if errorlevel 1 if not errorlevel 2 set ?=SD
if errorlevel 1 if not errorlevel 2 goto deltmp
cls
echo.
echo  MS-DOS 7.10 Setup
echo ===================
echo.
echo MS-DOS 7.10 Setup is now completed.
echo To use MS-DOS 7.10, please restart your system.
echo.
echo (C)Copyright Microsoft Corp. 1981-1999.
echo This program is Free Software under GNU GPL.
goto end

:full
copy %ins_path%\~dosetmp\*.* %ins_path% > nul
if %noadd%#==1# goto noadd
copy %ins_path%\~dosetmp\dosgui\*.* %ins_path% > nul
copy %ins_path%\~dosetmp\memmaker\*.* %ins_path% > nul
if exist %ins_path%\dosshell attr.com -s -h -r %ins_path%\dosshell > nul
if exist %ins_path%\dosshell del %ins_path%\dosshell
if not exist %ins_path%\dosshell\nul md %ins_path%\dosshell
copy %ins_path%\~dosetmp\dosshell\*.* %ins_path%\dosshell > nul
for %%x in (edit.com doshelp.hlp scandisk.exe memmaker.exe dosshell\dosshell.exe) do if not exist %ins_path%\%%x goto incom
if exist %ins_path%\doshelp.bat attr.com -s -h -r %ins_path%\doshelp.bat > nul
if exist %ins_path%\doshelp.bat del %ins_path%\doshelp.bat
change.exe /i %ins_path%\dosshell.bat "DOSSHELL.COM" "%ins_path%\DOSSHELL\DOSSHELL.COM" > nul
if not %iad%#==Yes# goto noadd
if exist %ins_path%\ados attr.com -s -h -r %ins_path%\ados > nul
if exist %ins_path%\ados del %ins_path%\ados
if not exist %ins_path%\ados\nul md %ins_path%\ados
copy %ins_path%\~dosetmp\ados\*.* %ins_path%\ados > nul
change.exe /i %ins_path%\ados.bat "ADOS.COM" "%ins_path%\ADOS\ADOS.COM" > nul

:noadd
if %noadd%#==1# if exist %ins_path%\dosshell.bat del %ins_path%\dosshell.bat
if %noadd%#==1# if exist %ins_path%\ados.bat del %ins_path%\ados.bat
if not %iad%#==Yes# if exist %ins_path%\ados.bat del %ins_path%\ados.bat
for %%x in (himem.sys doslfn.com xcopy.exe) do if not exist %ins_path%\%%x goto incom
goto addons

:dos
copy %ins_path%\~dosetmp\*.com %ins_path% > nul
copy %ins_path%\~dosetmp\*.exe %ins_path% > nul
copy %ins_path%\~dosetmp\*.tbl %ins_path% > nul
copy %ins_path%\~dosetmp\ega.cpi %ins_path% > nul
copy %ins_path%\~dosetmp\help.bat %ins_path% > nul
copy %ins_path%\~dosetmp\lockdrv.bat %ins_path% > nul
copy %ins_path%\~dosetmp\doshelp.bat %ins_path% > nul
copy %ins_path%\~dosetmp\doshelp.hlp %ins_path% > nul
copy %ins_path%\~dosetmp\lfnxlat.386 %ins_path% > nul
copy %ins_path%\~dosetmp\graphics.pro %ins_path% > nul
for %%x in (echo.sys ifshlp.sys country.sys) do if not exist %ins_path%\~dosetmp\%%x goto incom
copy %ins_path%\~dosetmp\echo.sys %ins_path% > nul
copy %ins_path%\~dosetmp\himem.sys %ins_path% > nul
copy %ins_path%\~dosetmp\ifshlp.sys %ins_path% > nul
copy %ins_path%\~dosetmp\dblbuff.sys %ins_path% > nul
copy %ins_path%\~dosetmp\country.sys %ins_path% > nul
copy %ins_path%\~dosetmp\display.sys %ins_path% > nul
copy %ins_path%\~dosetmp\keyboard.sys %ins_path% > nul
copy %ins_path%\~dosetmp\vide-cdd.sys %ins_path% > nul
for %%x in (himem.sys help.bat doslfn.com print.exe doshelp.hlp ega.cpi) do if not exist %ins_path%\%%x goto incom

:minimal
for %%x in (io.sys msdos.sys command.com config.sys autoexec.bat logo.sys) do copy %ins_path%\~dosetmp\%%x %ins_path% > nul
if exist %ins_path%\~dosetmp\echo.sys copy %ins_path%\~dosetmp\echo.sys %ins_path% > nul
if exist %ins_path%\~dosetmp\himem.sys copy %ins_path%\~dosetmp\himem.sys %ins_path% > nul
if exist %ins_path%\~dosetmp\ifshlp.sys copy %ins_path%\~dosetmp\ifshlp.sys %ins_path% > nul
goto addons

:cfgsys
messages.exe e
suscr.exe /M "Setup is preparing your system files..."
for %%x in (io.sys msdos.sys command.com config.sys autoexec.bat logo.sys himem.sys) do if not exist %ins_path%\%%x goto incom
if %sysdos%#==3# goto nosys
if exist c:\io.sys attr.com -s -h -r c:\io.sys > nul
if exist c:\msdos.sys attr.com -s -h -r c:\msdos.sys > nul
if exist c:\command.com attr.com -s -h -r c:\command.com > nul
if exist c:\config.sys attr.com -s -h -r c:\config.sys > nul
if exist c:\autoexec.bat attr.com -s -h -r c:\autoexec.bat > nul
if exist c:\logo.sys attr.com -s -h -r c:\logo.sys > nul
if exist c:\ifshlp.sys attr.com -s -h -r c:\ifshlp.sys > nul
if exist c:\dblbuff.sys attr.com -s -h -r c:\dblbuff.sys > nul
if exist c:\winboot.ini attr.com -s -h -r c:\winboot.ini > nul
if exist %ins_path%\drvspace.bin if exist c:\d??space.bin attr.com -s -h -r c:\d??space.bin > nul
if not %sysdos%#==2# goto makebak
if exist c:\io.dos attr.com -s -h -r c:\io.dos > nul
if exist c:\msdos.dos attr.com -s -h -r c:\msdos.dos > nul
if exist c:\command.dos attr.com -s -h -r c:\command.dos > nul
if exist c:\config.dos attr.com -s -h -r c:\config.dos > nul
if exist c:\autoexec.dos attr.com -s -h -r c:\autoexec.dos > nul
for %%x in (io.dos msdos.dos command.dos config.dos autoexec.dos) do if exist c:\%%x del c:\%%x
if exist c:\io.sys ren c:\io.sys io.dos
if exist c:\msdos.sys ren c:\msdos.sys msdos.dos
if exist c:\command.com ren c:\command.com command.dos
if exist c:\config.sys ren c:\config.sys config.dos
if exist c:\autoexec.bat ren c:\autoexec.bat autoexec.dos
if exist c:\io.dos if exist c:\msdos.dos if exist c:\command.dos call w.bat box @SETUP.INF:w_rendos
goto noren

:makebak
if exist c:\config.sys if exist c:\config.--- attr.com -s -h -r c:\config.--- > nul
if exist c:\config.sys copy c:\config.sys C:\CONFIG.--- > nul
if exist c:\config.--- attr.com +h c:\config.--- > nul
if exist c:\autoexec.bat if exist c:\autoexec.--- attr.com -s -h -r c:\autoexec.--- > nul
if exist c:\autoexec.bat copy c:\autoexec.bat C:\AUTOEXEC.--- > nul
if exist c:\autoexec.--- attr.com +h c:\autoexec.--- > nul
if exist c:\config.sys if exist c:\config.--- if exist c:\autoexec.bat if exist c:\autoexec.--- call w.bat box @SETUP.INF:w_makebak

:noren
for %%x in (io.sys msdos.sys command.com config.sys autoexec.bat logo.sys) do copy %ins_path%\%%x c:\ > nul
if not exist c:\ifshlp.sys\nul copy %ins_path%\ifshlp.sys c:\ > nul
if not exist c:\dblbuff.sys\nul copy %ins_path%\dblbuff.sys c:\ > nul
if exist c:\winboot.ini del c:\winboot.ini
if exist %ins_path%\drvspace.bin copy %ins_path%\drvspace.bin c:\ > nul
if exist %ins_path%\drvspace.bin copy %ins_path%\drvspace.bin C:\DBLSPACE.BIN > nul
for %%x in (io.sys msdos.sys logo.sys) do if not exist c:\%%x goto incom
suscr.exe /M "Setup is updating your system files..."
inifile.com c:\msdos.sys [Paths] windir=C:\
if %wdir%#==.# inifile.com c:\msdos.sys [Paths] winbootdir=.
inifile.com c:\msdos.sys [Options] Logo=1
inifile.com c:\msdos.sys [Options] BootGUI=0
inifile.com c:\msdos.sys [Options] BootMulti=0
inifile.com c:\msdos.sys [Options] DisableLog=1
inifile.com c:\msdos.sys [Options] SystemReg=0
for %%x in (2# 4#) do if %sysdos%#==%%x inifile.com c:\msdos.sys [Options] BootMulti=1
change.exe /i c:\config.sys "DEVICE=" "DEVICE=%ins_path%\\" > nul
change.exe /i c:\config.sys "DEVICEHIGH=" "DEVICEHIGH=%ins_path%\\" > nul
change.exe /i c:\autoexec.bat "PATH=" "PATH=%ins_path%;..;%wdir%;" > nul
echo SET PATH=%ins_path%;..;%wdir%>> c:\config.sys
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
call w.bat box @SETUP.INF:w_logo
if errorlevel 2 if not errorlevel 3 inifile.com c:\msdos.sys [Options] Logo=0
call w.bat box @SETUP.INF:w_bootlog #2
if errorlevel 1 if not errorlevel 2 inifile.com c:\msdos.sys [Options] DisableLog=0
attr.com +s +h +r c:\io.sys > nul
attr.com +s +h +r c:\msdos.sys > nul
if exist c:\d??space.bin attr.com +s +h +r c:\d??space.bin > nul
for %%x in (1 3 4) do set wcb%%x=1
for %%x in (2 5 6) do set wcb%%x=0
if not #%chins%==#Minimal goto nomin
for %%x in (1 2 3 4 5 6) do set wcb%%x=0
change.exe /i c:\config.sys "DEVICEHIGH=" "REM DEVICEHIGH=" > nul
change.exe /i c:\config.sys "DEVICE=%ins_path%\EMM386.EXE" "REM DEVICE=%ins_path%\EMM386.EXE" > nul
change.exe /i c:\autoexec.bat "MODE CON RATE" "REM MODE CON RATE" > nul
change.exe /i c:\autoexec.bat "LH KILLER" "REM LH KILLER" > nul
change.exe /i c:\autoexec.bat "LH DOSKEY" "REM LH DOSKEY" > nul
change.exe /i c:\autoexec.bat "LH MSCDEX" "REM LH MSCDEX" > nul
goto domin

:nomin
change.exe /i c:\autoexec.bat "MS-DOS 7.10 prompt." "MS-DOS 7.10 prompt. Type 'HELP' for help." > nul
call w.bat box @SETUP.INF:w_dlock
if errorlevel 1 if not errorlevel 2 change.exe /i c:\autoexec.bat "REM CALL LOCKDRV" "CALL LOCKDRV" > nul
call w.bat box @SETUP.INF:w_emm386
if errorlevel 2 if not errorlevel 3 goto emm1
if errorlevel 3 if not errorlevel 4 goto emm2
goto doscd

:emm1
change.exe /i c:\config.sys "DEVICE=%ins_path%\EMM386.EXE NOEMS" "DEVICE=%ins_path%\EMM386.EXE RAM" > nul
goto doscd

:emm2
change.exe /i c:\config.sys "DEVICE=%ins_path%\EMM386.EXE" "REM DEVICE=%ins_path%\EMM386.EXE" > nul
goto doscd

:doscd
call w.bat box @SETUP.INF:w_doscd
if not errorlevel 2 goto doscfg
if errorlevel 4 goto doscfg
if errorlevel 3 change.exe /i c:\config.sys "DEVICEHIGH=%ins_path%\VIDE-CDD.SYS" "REM DEVICEHIGH=%ins_path%\VIDE-CDD.SYS" > nul
change.exe /i c:\autoexec.bat "LH MSCDEX" "REM LH MSCDEX" > nul

:doscfg
call w.bat box @SETUP.INF:w_doscfg
if errorlevel 2 for %%x in (1 3 4) do set wcb%%x=1
if errorlevel 2 for %%x in (2 5 6) do set wcb%%x=0

:domin
if %wcb1%#==0# change.exe /i c:\autoexec.bat "LH SHARE" "REM LH SHARE" > nul
if %wcb2%#==0# change.exe /i c:\config.sys "DEVICEHIGH=%ins_path%\POWER.EXE" "REM DEVICEHIGH=%ins_path%\POWER.EXE" > nul
if %wcb3%#==0# change.exe /i c:\autoexec.bat "LH DOSLFN" "REM LH DOSLFN" > nul
if %wcb4%#==0# change.exe /i c:\autoexec.bat "LH CTMOUSE" "REM LH CTMOUSE" > nul
if %wcb5%#==0# change.exe /i c:\autoexec.bat "LH NLSFUNC" "REM LH NLSFUNC" >nul
if %wcb6%#==0# change.exe /i c:\autoexec.bat "LH SMARTDRV" "REM LH SMARTDRV" >nul
for %%x in (wrb cp ct kc) do set %%x=
if #%chins%==#Minimal goto nosys
call w.bat box @SETUP.INF:w_doscp
if %wrb%#==1# set cp=437
if %wrb%#==1# set ct=001
if %wrb%#==1# set kc=US
if %wrb%#==2# set cp=850
if %wrb%#==2# set ct=044
if %wrb%#==2# set kc=UX
if %wrb%#==3# set cp=852
if %wrb%#==3# set ct=042
if %wrb%#==3# set kc=CZ
if %wrb%#==4# set cp=863
if %wrb%#==4# set ct=002
if %wrb%#==4# set kc=CF
if %wrb%#==5# set cp=866
if %wrb%#==5# set ct=007
if %wrb%#==5# set kc=RU
if %wrb%#==6# set cp=869
if %wrb%#==6# set ct=030
if %wrb%#==6# set kc=GK
if %wrb%#==7# set cp=936
if %wrb%#==7# set ct=001
if %wrb%#==7# set kc=US
if %wrb%#==8# set cp=950
if %wrb%#==8# set ct=001
if %wrb%#==8# set kc=US
if %cp%#==# set cp=437
if %ct%#==# set ct=001
if %kc%#==# set kc=US
change.exe /i c:\config.sys "REM COUNTRY=001,,437,,COUNTRY.SYS" "COUNTRY=%ct%,,%cp%,,%ins_path%\COUNTRY.SYS" > nul
change.exe /i c:\autoexec.bat "DOSLFN /Z:CP437UNI.TBL" "DOSLFN /Z:%ins_path%\CP%cp%UNI.TBL" > nul
if %kc%#==US# goto nomode
if %kc%#==RU# goto nomode
if %kc%#==GK# goto nomode
if not %kc%#==UX# goto chmode
set wrb=
call w.bat box @SETUP.INF:w_keylay
if %wrb%#==1# set kc=UX
if %wrb%#==2# set kc=UK
if %wrb%#==3# set kc=FR
if %wrb%#==4# set kc=GR
if %wrb%#==5# set kc=DK
if %wrb%#==6# set kc=NL
if %wrb%#==7# set kc=IT
if %wrb%#==8# set kc=SP
if %wrb%#==9# set kc=PO

:chmode
change.exe /i c:\config.sys "REM DEVICEHIGH=%ins_path%\DISPLAY.SYS" "DEVICEHIGH=%ins_path%\DISPLAY.SYS" > nul
change.exe /i c:\autoexec.bat "REM MODE CON CP PREPARE=((437)EGA.CPI)" "MODE CON CP PREPARE=((%cp%)%ins_path%\EGA.CPI)" > nul
change.exe /i c:\autoexec.bat "REM MODE CON CP SELECT=437" "MODE CON CP SELECT=%cp%" > nul
change.exe /i c:\autoexec.bat "REM KEYB US,,,,KEYBOARD.SYS" "LH KEYB %kc%,,,,%ins_path%\KEYBOARD.SYS" > nul
:nomode
set cp=
set ct=
set kc=

:nosys
for %%x in (1 2 3 4 5 6) do set wcb%%x=
for %%x in (io.sys msdos.sys logo.sys) do del %ins_path%\%%x
goto success

:sysdos
xfind.com " WINA20.386 " c:\msdos.sys > nul
if errorlevel 1 goto checkw9x
call w.bat box @SETUP.INF:w_sysdos
if errorlevel 1 if not errorlevel 2 set sysdos=1
if errorlevel 2 if not errorlevel 3 set sysdos=2
if not errorlevel 3 goto copy
call w.bat box @SETUP.INF:w_nosys #2
if errorlevel 2 goto sysdos
set sysdos=3
goto copy

:sysnt
xfind.com "\ntdetect.com" c:\ntldr > nul
if errorlevel 1 goto bpw
call w.bat box @SETUP.INF:w_sysnt
if errorlevel 2 goto err2
attr.com -s -h -r c:\boot.ini > nul
inifile.com c:\boot.ini [operating systems] c:\=
inifile.com c:\boot.ini [operating systems] c:\bootsect.dos=
inifile.com c:\boot.ini [boot loader] default=c:\bootsect.dos
if exist %tmpdest%\~timeout.bat attr.com -s -h -r %tmpdest%\~timeout.bat > nul
set timeout=
inifile.com c:\boot.ini [boot loader] timeout > %tmpdest%\~TIMEOUT.BAT
if not exist %tmpdest%\~timeout.bat goto ntdboot
call %tmpdest%\~timeout.bat
if %timeout%#==# set timeout=0
if %timeout%#==0# inifile.com c:\boot.ini [boot loader] timeout=-1
set timeout=

:ntdboot
bootpart.exe dos71 c:\bootsect.dos "MS-DOS 7.10" > nul
attr.com +s +h +r c:\boot.ini > nul
if not exist c:\bootsect.dos call w.bat box "Cannot make dual-boot.",OK
if exist c:\bootsect.dos attr.com +s +h +r c:\bootsect.dos > nul
echo y|bootpart.exe dos71 boot:c: > nul
echo y|bootpart.exe winnt boot:c: > nul

:err2
if errorlevel 2 if not errorlevel 3 goto bpw
if not errorlevel 3 goto checkdos
call w.bat box @SETUP.INF:w_nobs #2
if errorlevel 2 goto sysnt
goto checkdos

:underwin
call w.bat box @SETUP.INF:w_underwin #2
if errorlevel 1 if not errorlevel 2 goto sysboot
goto error

:novar
for %%x in (ret1 ret2 ret3 ret4) do set %%x=
call w.bat box @SETUP.INF:w_novar
goto error

:nofile
call w.bat box @SETUP.INF:w_nofile
goto error

:nohd
call w.bat box @SETUP.INF:w_nohd #2
if errorlevel 2 goto error
echo    Done.
goto mbr

:nodc
call w.bat box @SETUP.INF:w_nodc #2
if errorlevel 1 if not errorlevel 2 set sysdos=3
if errorlevel 1 if not errorlevel 2 goto copy

:error
cls
echo.
echo  MS-DOS 7.10 Setup
echo ===================
echo.
echo MS-DOS 7.10 Setup has met an error and cannot continue.
echo Therefore, MS-DOS 7.10 is NOT installed successfully.
echo.
echo This program is Free Software under GNU GPL.
echo If the problem persists, please contact Support Center.
echo.
echo Press any key to quit.
pause > nul
set missfile=
set error=
goto end

:chpath1
suscr.exe /M "Setup is checking path..."
if not %1#==ChPath1# setup.bat ChPath1 %ins_path%

:chpath2
if %1#==ChPath1# set ins_path=%2
if %1#==ChPath1# setup.bat ChPath2
for %%x in (# #a: #A: #b: #B: #c: #C:) do if #%ins_path%==%%x goto inpath
dready.com %ins_path%\ /w > nul
if errorlevel 1 goto inpath
if exist %ins_path%\nul goto pathyes
goto pathno

:inpath
suscr.exe /D " MS-DOS 7.10 Setup " /F DOSÿSetup
call w.bat box @SETUP.INF:w_inpath
goto path

:pathyes
call w.bat box @SETUP.INF:w_pathyes
if errorlevel 2 goto path
goto copychk

:pathno
call w.bat box @SETUP.INF:w_pathno
if errorlevel 2 goto path
mcd.exe /m /n %ins_path%
if not exist %ins_path%\nul goto inpath

:copychk
if exist %ins_path%\~~W~~.TMP attr.com -s -h -r %ins_path%\~~W~~.TMP > nul
copy w.bat %ins_path%\~~W~~.TMP > nul
if not exist %ins_path%\~~W~~.TMP goto inpath
if exist %ins_path%\~~W~~.TMP del %ins_path%\~~W~~.TMP
goto chins

:checkzip
messages.exe 101 %prog%
messages.exe 102
messages.exe b
suscr.exe /M "Setup is checking unpacked files..."
for %%x in (io.sys msdos.sys command.com config.sys autoexec.bat logo.sys) do if not exist %ins_path%\~dosetmp\%%x goto incom
if not #%chins%==#Minimal if not exist %ins_path%\~dosetmp\subst.exe goto incom
if not #%chins%==#Minimal if not exist %ins_path%\~dosetmp\doslfn.com goto incom
if not #%chins%==#Minimal if not exist %ins_path%\~dosetmp\display.sys goto incom
if not #%chins%==#Full goto checknt
if not %noadd%#==1# if not exist %ins_path%\~dosetmp\drvspace.bin goto incom
if exist %ins_path%\~dosetmp\drvspace.* copy %ins_path%\~dosetmp\drvspace.* %ins_path%\~dosetmp\DBLSPACE.* > nul
if not %noadd%#==1# if not exist %ins_path%\~dosetmp\ansi.sys goto incom
if not %noadd%#==1# if not exist %ins_path%\~dosetmp\dosshell\dosshell.exe goto incom
if not %noadd%#==1# if not exist %ins_path%\~dosetmp\memmaker\memmaker.exe goto incom
if not %noadd%#==1# if not exist %ins_path%\~dosetmp\dosgui\msd.exe goto incom
goto checknt

:incom
call w.bat box @SETUP.INF:w_incom
goto error

:help
echo Installs MS-DOS 7.10.
echo.
echo This program will install MS-DOS 7.10 onto your system.
echo Usage: SETUP [path/switch]
echo.
echo Available switches:
echo.
echo /D: Skip hard disk and MBR detection.
echo /N: Don't install Add-Ons by default.
echo /R: Don't try to create a RAM drive for temporary files.
echo /?: Show this help.
echo.
echo This program is Free Software under GNU GPL.
goto exit

:quit
call w.bat box @SETUP.INF:w_quit #2,8
if errorlevel 2 goto %ret%
cls
echo.
echo  MS-DOS 7.10 Setup
echo ===================
echo.
echo You have chosen to quit from MS-DOS 7.10 Setup.
echo Please install it next time.
echo.
echo (C)Copyright Microsoft Corp. 1981-1999.
echo This program is Free Software under GNU GPL.

:end
for %%x in (noadd addons iad sysdos chins ret prog xmss swch wrb wbat wdir) do set %%x=
set ?=
dready.com a: > nul
if not errorlevel 1 if exist a:\diskvol.txt if exist a:\dready.com A:
if %ins_path%#==# goto nodeltmp

:deltmp
dready.com %ins_path% > nul
if errorlevel 1 goto nodeltmp
if %?%#==SD# if not exist %ins_path%\shutdown.com copy %ins_path%\~dosetmp\shutdown.com %ins_path% > nul
if exist %ins_path%\~dosetmp\nul deltree.com /y %ins_path%\~dosetmp > nul
if not %?%#==SD# goto nodeltmp
suscr.exe /M "Setup is now restarting your system..."
if exist r:\shutdown.com shutdown.com r
cd c:\
set TEMP=C:\
set TMP=C:\
if not exist %ins_path%\shutdown.com shutdown.com r
if exist c:\~dos71in.tmp\deltree.com c:\~dos71in.tmp\deltree.com /y c:\~dos71in.tmp|%ins_path%\shutdown.com r

:nodeltmp
set ins_dir=
set ins_path=
dready.com c: /w > nul
if errorlevel 1 goto reset
c:
cd c:\
set TEMP=C:\
set TMP=C:\

:reset
echo.
if %tmpdest%#==# goto exit
if %COMSPEC%#==%tmpdest%\COMMAND.COM# set COMSPEC=A:\COMMAND.COM
if %tmpdest%==r: goto reset1
if %tmpdest%==c:\~dos71in.tmp goto reset2
goto exit

:reset1
set tmpdest=
if not exist r:\reset1.bat echo Warning: RESET1.BAT doesn't exist. Cannot delete temporary files.
if exist r:\reset1.bat r:\reset1.bat
goto exit

:reset2
set tmpdest=
if not exist c:\~dos71in.tmp\reset2.bat echo Warning: RESET2.BAT doesn't exist. Cannot delete temporary files.
if exist c:\~dos71in.tmp\reset2.bat c:\~dos71in.tmp\reset2.bat

:exit
if not %1#==AddOns2# echo.
