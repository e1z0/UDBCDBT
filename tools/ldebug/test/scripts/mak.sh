#! /bin/bash

# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

[ -z "$NASM" ] && NASM=nasm
"$NASM" toolarge.asm -o toolarge.exe
