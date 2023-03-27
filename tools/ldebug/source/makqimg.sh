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


            mkdir -p "${rootpath}tmp/qemutest"
	    cp -aL "$BOOT_KERNEL" "${rootpath}tmp/qemutest/${BOOT_KERNEL##*/}"
	    cp -aL "$BOOT_COMMAND" "${rootpath}tmp/qemutest/${BOOT_COMMAND##*/}"
	    echo -ne "@echo off\r\n$lpre$build_name.$comext /c='r dco or= 4000'\r\n" > "${rootpath}tmp/qemutest/a$build_name.bat"
	    echo -ne "quit.com\r\n" >> "${rootpath}tmp/qemutest/a$build_name.bat"
	    "$NASM" "${rootpath}misc/quit.asm" \
	     "$options_i_lmacros" \
	     -o "${rootpath}tmp/qemutest/"quit.com &&
	    "$NASM" "${LDOSBOOT_DIR%/}"/boot.asm -w-user \
	     "$options_i_lmacros" \
	     -D_COMPAT_"$BOOT_PROTOCOL"=1 \
	     -D_LBA=0 -D_USE_PART_INFO=0 -D_QUERY_GEOMETRY=0 \
	     $BOOT_OPTIONS \
	     -D_MAP="${rootpath}tmp/qemutest/"boot.map \
	     -l "${rootpath}tmp/qemutest/"boot.lst \
	     -o "${rootpath}tmp/qemutest/"boot.bin &&
	    "$NASM" "${BOOTIMG_DIR%/}"/bootimg.asm \
	     -I ./ \
	     -I "${rootpath}tmp/qemutest/" \
	     -I "${rootpath}bin/" \
	     "$options_i_bootimg" \
	     "$options_i_lmacros" \
	     -o "${rootpath}tmp/qemutest/$lpre$build_name.img" \
	     -D_PAYLOADFILE="${BOOT_KERNEL##*/},${BOOT_COMMAND##*/},::rename,a$build_name.bat,autoexec.bat,$lpre$build_name.$comext,quit.com,::chdir,scripts,$( \
		for file in "${rootpath}test/scripts/"*.{sld,com,exe}; do echo -n "$file,"; done)::empty" \
	     -D_BOOTFILE="'boot.bin'"
