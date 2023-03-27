:another
; another entry
y :yetanother
; another exit
goto :eof

:yetanother
; yet another
goto :eof

:entry
; self entry
y :another
; self exit
