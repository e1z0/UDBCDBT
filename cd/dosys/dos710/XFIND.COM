���������             ��������             XFIND Ver. 2.3 (c) 1990..2001, Horst Schaeffer
Reports all files where given string was found

Syntax: XFIND "string" [path] [file ...] [options]

String: in quote marks and/or decimal ASCII values
        linked with plus sign, i.e. "string"+9+"more"
Path:   to start search, default current dir
File:   file name(s), wildcards allowed, default: *.*
Options:
 /C	case sensitive search (case ignored by default)
 /S	include subdirectories
 /R	report files without given path
 /L	report LFN's (full path, unless /R)
 /".."	prefix for output lines
 +��
 �*��08�sF������<w<ar<zw$��S�  :�u����� tC��r�[Ê<"t<'uF�Ā<t�:�t���:��
 �Ȏ؉�  ���tA��� �@�!��� �@�!�������)Ѵ@��!û��?\u�/C;�r��Invalid file name ���*�  � �C<?t<*u�Ą�u��tʀy�\t'��u�}:t��u�� �N��!r�� t�A�\ �0�v�K��t�y�\t�y�:u�� �0�vËv�<\t<:u��'���u�< u迪�@ � ���~�Ft;��< uF�. �F
�N����`q��!s�f�ÿN�,� ��O~
�*.* �v�< u�����@ � �O�~����u��Subdir stack overflow +ɾ��< tAF���~OO&�\ 9�v��t��ON��GF�~Ëv�� �u���^������u��v�� +++ File read error ��������u�O� �H��z��� ��N��^ �?��!s��+Fr�¿�;�v�Á�N�r�� �u���N޹���^ ��B��!s��+ۊ :t�Fu�u�:uC;^r��F��F�������u�OW���Ft�N����`q��!s�f�����N
��N�Ft�_����u�O�
��F t��� ����� t����Fu!���@�=��!s���F ����^ �>��!�r�n�ú��N�!r�� �<.t�~����u��V�O��= t��Invalid search path �v�F���' �Fv��s������~� �~� � �~�= u��Ft�~������u�� �Fk�s��+�s��No string Invalid option ��< t�<,t�N</u�F�A�u� ���</u�N ��$�<Su�N��<Ru�N��<Lu�N�<Cu�N벾>��ì< t�<,t�N���< t�N���t0�<0r�<9w���� ����ĊD��=RCu��=FLu�
�FF�< t�N<+uF븋ρ��N��u�Nÿ�< t�<,t�N�<t</t< t<,t��� ��ܸ  �þ� �< t�N�</?t<u�-�r�� ����I����������Fu���= t����������	��Y�*��~ u���L�!     ��    �     ������J�!��Ls�!�������p�