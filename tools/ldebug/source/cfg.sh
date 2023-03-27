#! /bin/bash

# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

# If you want to override configuration without the
# hassle of having to exclude differences in this file
# (cfg.sh) from the SCM, you may provide ovr.sh.
# The meaning of this line allows you to copy cfg.sh
# to serve as a template for ovr.sh without changes.
[ -f ovr.sh ] && [[ "${BASH_SOURCE[0]##*/}" != ovr.sh ]] && . ovr.sh

# As the below only are set if no value is provided
# yet, any value can be overridden from the shell.
[ -z "$LMACROS_DIR" ] && LMACROS_DIR=../../lmacros/
[ -z "$SYMSNIP_DIR" ] && SYMSNIP_DIR=../../symsnip/
[ -z "$LDOSBOOT_DIR" ] && LDOSBOOT_DIR=../../ldosboot/
[ -z "$INSTSECT_DIR" ] && INSTSECT_DIR=../../instsect/
[ -z "$INSTSECT_BOOT_NAME" ] && INSTSECT_BOOT_NAME=LDEBUG
[ -z "$INSTSECT_BOOT_PROTOCOL" ] && INSTSECT_BOOT_PROTOCOL=LDOS
[ -z "$INSTSECT_BOOT_OPTIONS" ] && INSTSECT_BOOT_OPTIONS=" "
[ -z "$SCANPTAB_DIR" ] && SCANPTAB_DIR=../../scanptab/
[ -z "$INICHECK_DIR" ] && INICHECK_DIR=../../crc16-t/
[ -z "$BOOTIMG_DIR" ] && BOOTIMG_DIR=../../bootimg/
[ -z "$INICOMP_DIR" ] && INICOMP_DIR=../../inicomp/
[ -z "$INICOMP_METHOD" ] && INICOMP_METHOD="brieflz lz4 snappy exodecr x heatshrink lzd lzo lzsa2 apl bzp"
	# none or brieflz or lz4 or snappy or exodecr or x or heatshrink or lzd or lzo or lzsa2 or apl or bzp
[ -z "$INICOMP_WINNER" ] && INICOMP_WINNER=smallest
	# smallest or none or one of the method names
[ -z "$INICOMP_BLZPACK" ] && INICOMP_BLZPACK=blzpack
[ -z "$INICOMP_LZ4C" ] && INICOMP_LZ4C=lz4c
[ -z "$INICOMP_SNZIP" ] && INICOMP_SNZIP=snzip
[ -z "$INICOMP_EXOMIZER" ] && INICOMP_EXOMIZER=exomizer
[ -z "$INICOMP_X" ] && INICOMP_X=x
[ -z "$INICOMP_HEATSHRINK" ] && INICOMP_HEATSHRINK=heatshrink
[ -z "$INICOMP_LZIP" ] && INICOMP_LZIP=lzip
[ -z "$INICOMP_LZOP" ] && INICOMP_LZOP=lzop
[ -z "$INICOMP_LZSA" ] && INICOMP_LZSA=lzsa
[ -z "$INICOMP_APULTRA" ] && INICOMP_APULTRA=apultra
[ -z "$INICOMP_BZPACK" ] && INICOMP_BZPACK=bzpack

[ -z "$DOSEMU" ] && DOSEMU=dosemu
[ -z "$QEMU" ] && QEMU=qemu-system-i386
[ -z "$DEFAULT_MACHINE" ] && DEFAULT_MACHINE=dosemu
[ -z "$BOOT_KERNEL" ] && BOOT_KERNEL=~/.dosemu/drive_c/kernel.sys
[ -z "$BOOT_COMMAND" ] && BOOT_COMMAND=~/.dosemu/drive_c/command.com
[ -z "$BOOT_PROTOCOL" ] && BOOT_PROTOCOL=FREEDOS
[ -z "$BOOT_OPTIONS" ] && BOOT_OPTIONS=" "
[ -z "$TELLSIZE" ] && TELLSIZE=tellsize
[ -z "$MKTMPINC" ] && MKTMPINC=mktmpinc.pl
[ -z "$NASM" ] && NASM=nasm
[ -z "$CHECKSUM" ] && CHECKSUM="${INICHECK_DIR%/}"/iniload/checksum

[ -z "$use_build_revision_id" ] && use_build_revision_id=1
[ -z "$use_build_decomp_test" ] && use_build_decomp_test=0
[ -z "$use_build_inicheck" ] && use_build_inicheck=0
[ -z "$use_build_shim" ] && use_build_shim=0
[ -z "$use_build_qimg" ] && use_build_qimg=0
[ -z "$use_build_bimg" ] && use_build_bimg=0
[ -z "$use_build_bootable" ] && use_build_bootable=1
