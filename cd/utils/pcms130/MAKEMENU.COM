��!A�C�G2ҾJ�!�z�4 �<3��!s���M�غ8�0S�![<r��r�B� ���
 �>�!��� ��< tF����+ʴ@�!r;�uúz�A�!������L�!PCMENU.BAT  created  open error (read-only or out of directory entries)  write error (disk full?) 
PCMS %1 %2
IF NOT ERRORLEVEL 255 GOTO E
%PCM%
PCM_EXEC
:E
IF NOT ERRORLEVEL 1 GOTO X
IF ERRORLEVEL 4 ECHO Invalid option found
IF ERRORLEVEL 4 GOTO X
IF ERRORLEVEL 3 ECHO More than 66 items in menu
IF ERRORLEVEL 3 GOTO X
IF ERRORLEVEL 2 ECHO Error writing output
IF ERRORLEVEL 2 GOTO X
IF ERRORLEVEL 1 ECHO Error reading menu
:X
 @ECHO OFF
 :
CD\