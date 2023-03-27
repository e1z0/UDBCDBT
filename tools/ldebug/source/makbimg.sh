#! /bin/bash

# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

rootpath="../"
miscpath=""

comext="com"
mapext="map"
lstext="lst"
binbigext="big"

lstdir="lst/"
mapdir="lst/"

ppre="p"
tpre="t"
usuf="u"
csuf="c"
lpre="l"

blzext="blz"
lz4ext="lz4"
szext="sz"
exoext="exo"
xext="x"
hsext="hs"
lzext="lz"
lzoext="lzo"
lzsa2ext="sa2"
aplext="apl"


. cfg.sh

if [ -n "$LMACROS_DIR" ]; then {
  options_i_lmacros=-I"${LMACROS_DIR%/}"/
} fi

if [ -n "$LDOSBOOT_DIR" ]; then {
  options_i_ldosboot=-I"${LDOSBOOT_DIR%/}"/
} fi

if [ -n "$SCANPTAB_DIR" ]; then {
  options_i_scanptab=-I"${SCANPTAB_DIR%/}"/
} fi

if [ -n "$BOOTIMG_DIR" ]; then {
  options_i_bootimg=-I"${BOOTIMG_DIR%/}"/
} fi

if [ -n "$INSTSECT_DIR" ]; then {
  options_i_instsect=-I"${INSTSECT_DIR%/}"/
} fi


[ -z "$build_name" ] && build_name=debug
scriptname=ldebug.sld
[[ "$build_name" == *dd* ]] && scriptname=lddebug.sld
[[ "$build_name" == *cdebug* ]] && scriptname=lcdebug.sld


            mkdir -p "${rootpath}tmp/bdbgtest"
	    echo -ne ":bootstartup\r\nr dco or= 4000\r\n" > "${rootpath}tmp/bdbgtest/$scriptname"
	    "$NASM" "${LDOSBOOT_DIR%/}"/boot.asm -w-user \
	     "$options_i_lmacros" \
	     -D_COMPAT_LDOS=1 \
	     -D_LOAD_NAME="'L${build_name^^}'" \
	     -D_MAP="${rootpath}tmp/bdbgtest/b$build_name".map \
	     -l "${rootpath}tmp/bdbgtest/b$build_name".lst \
	     -o "${rootpath}tmp/bdbgtest/b$build_name".bin &&
	    "$NASM" "${BOOTIMG_DIR%/}"/bootimg.asm \
	     -I ./ \
	     -I "${rootpath}tmp/bdbgtest/" \
	     -I "${rootpath}bin/" \
	     "$options_i_bootimg" \
	     "$options_i_lmacros" \
	     -o "${rootpath}tmp/bdbgtest/b$build_name.img" \
	     -D_PAYLOADFILE="$lpre$build_name.$comext,$scriptname,::chdir,scripts,$( \
		for file in "${rootpath}test/scripts/"*.sld; do echo -n "$file,"; done)::empty" \
	     -D_BOOTFILE="'b$build_name.bin'"
