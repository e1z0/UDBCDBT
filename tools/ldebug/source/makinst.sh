#! /bin/bash

# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

rootpath="../"

lstdir="lst/"
mapdir="lst/"

. cfg.sh

if [ -n "$LMACROS_DIR" ]; then {
  options_i_lmacros=-I"${LMACROS_DIR%/}"/
} fi

if [ -n "$LDOSBOOT_DIR" ]; then {
  options_i_ldosboot=-I"${LDOSBOOT_DIR%/}"/
} fi

if [ -n "$INSTSECT_DIR" ]; then {
  options_i_instsect=-I"${INSTSECT_DIR%/}"/
} fi


echo Building boot12, boot16, boot32
SUFFIX=""
TMPINC=""
if command -v "$MKTMPINC" &> /dev/null
then
	TMPINC="-D_TMPINC"
fi
if [[ -n "$TMPINC" ]]
then
	"$MKTMPINC" "${LDOSBOOT_DIR%/}"/boot.asm
fi
for FAT in 12 16
do
	"$NASM" "${LDOSBOOT_DIR%/}"/boot.asm \
	     $TMPINC \
	     "$options_i_lmacros" \
	     "$options_i_ldosboot" \
	     -D_COMPAT_"$INSTSECT_BOOT_PROTOCOL"=1 \
	     -D_LOAD_NAME="'$INSTSECT_BOOT_NAME'" \
	     -D_LBA=1 -D_CHS=1 -D_USE_PART_INFO=1 -D_QUERY_GEOMETRY=1 \
	     -D_LBA_SKIP_CHECK=1 \
	     -D_FAT$FAT=1 \
	     $INSTSECT_BOOT_OPTIONS \
	     -D_MAP="${rootpath}$mapdir"boot${FAT}${SUFFIX}.map \
	     -l "${rootpath}$lstdir"boot${FAT}${SUFFIX}.lst \
	     -o "${rootpath}tmp/"boot${FAT}${SUFFIX}.bin \
	     "$@"
	done
if [[ -n "$TMPINC" ]]
then
	"$MKTMPINC" "${LDOSBOOT_DIR%/}"/boot32.asm
fi
FAT=32
	"$NASM" "${LDOSBOOT_DIR%/}"/boot32.asm \
	     $TMPINC \
	     "$options_i_lmacros" \
	     "$options_i_ldosboot" \
	     -D_COMPAT_"$INSTSECT_BOOT_PROTOCOL"=1 \
	     -D_LOAD_NAME="'$INSTSECT_BOOT_NAME'" \
	     -D_LBA=1 -D_CHS=1 -D_USE_PART_INFO=1 -D_QUERY_GEOMETRY=1 \
	     -D_LBA_SKIP_CHECK=1 \
	     -D_FAT$FAT=1 \
	     $INSTSECT_BOOT_OPTIONS \
	     -D_MAP="${rootpath}$mapdir"boot${FAT}${SUFFIX}.map \
	     -l "${rootpath}$lstdir"boot${FAT}${SUFFIX}.lst \
	     -o "${rootpath}tmp/"boot${FAT}${SUFFIX}.bin \
	     "$@"
if [[ -n "$TMPINC" ]]
then
	rm -f *.tmp
fi

echo Building instsect.com
"$NASM" "${INSTSECT_DIR%/}"/instsect.asm \
	-D_MAP="${rootpath}$mapdir"instsect.map \
	-l "${rootpath}$lstdir"instsect.lst \
	-I "${rootpath}tmp/" \
	"$options_i_lmacros" \
	"$options_i_ldosboot" \
	-o "${rootpath}bin/"instsect.com "$@"
