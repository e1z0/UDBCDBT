%macro passdef 0-*.nolist
%rep %0
 %ifdef _%1
  %warning using define -D_%1=_%1
 %else
  %warning using define -U_%1
 %endif
 %rotate 1
%endrep
%endmacro
passdef _DATE__, _TIME__, _DATE_NUM__, _TIME_NUM__
passdef _UTC_DATE__, _UTC_TIME__, _UTC_DATE_NUM__, _UTC_TIME_NUM__
passdef _POSIX_TIME__
passdef REVISIONID
passdef REVISIONID_LMACROS
passdef REVISIONID_INSTSECT
passdef REVISIONID_SYMSNIP
passdef REVISIONID_SCANPTAB
passdef REVISIONID_INICOMP
passdef REVISIONID_INICHECK
passdef REVISIONID_LDOSBOOT
