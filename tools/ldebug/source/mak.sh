#! /bin/bash

# Usage of the works is permitted provided that this
# instrument is retained with the works, so that any entity
# that uses the works is notified of this instrument.
#
# DISCLAIMER: THE WORKS ARE WITHOUT WARRANTY.

if [ -n "$UPDATEDL" ]
then
  unset UPDATEDL
  if [ "$UPDATEDLNAME" == lcdebug ]
  then
    INICOMP_METHOD="lz4 lzd exodecr apl lzsa2" INICOMP_WINNER=lzsa2 \
      use_build_decomp_test=1 \
      build_name=cdebug build_options="-D_DEBUG -D_DEBUG_COND" ./mak.sh
    INICOMP_METHOD="lz4 lzd exodecr apl lzsa2" INICOMP_WINNER=lzsa2 \
      use_build_decomp_test=1 \
      build_name=cdebugx build_options="-D_DEBUG -D_DEBUG_COND -D_PM" ./mak.sh
    INSTSECT_BOOT_NAME=LCDEBUG ./makinst.sh
  elif [ "$UPDATEDLNAME" == lddebug ]
  then
    INICOMP_METHOD="lz4 lzd exodecr apl lzsa2" INICOMP_WINNER=lzsa2 \
      use_build_decomp_test=1 ./maked
    INSTSECT_BOOT_NAME=LDDEBUG ./makinst.sh
  else
    INICOMP_METHOD="lz4 lzd exodecr apl lzsa2" INICOMP_WINNER=lzsa2 \
      use_build_decomp_test=1 ./make
    INSTSECT_BOOT_NAME=LDEBUG ./makinst.sh
  fi
  exit $?
fi

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
bzpext="bzp"


function fun_delete_compressed {
  [ "$1" != "silent" ] && echo "Deleting $lpre${build_name}.$comext"
  rm -f ${rootpath}bin/$lpre"$build_name".$comext \
    ${rootpath}${lstdir}$lpre"$build_name"$csuf.$lstext ${rootpath}${mapdir}$lpre"$build_name"$csuf.$mapext
}

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

if [ -n "$SYMSNIP_DIR" ]; then {
  options_i_symsnip=-I"${SYMSNIP_DIR%/}"/
} fi


[ -z "$build_name" ] && build_name=debug
reproducedir=${rootpath}tmp/"$build_name".rep

function shellquote() {
  local sp=""
#  local ii=0
  for param; do
    echo -ne "$sp"
#    ((debug)) && echo -ne "$ii:"
#    ((ii += 1))
    printf "%q" "$param"
    sp=" "
  done
  echo
}

if [ "$1" == reproduce ]; then
  shift
  echo Reproducing the "$build_name" build
  if ! [ -f "$reproducedir"/nasmenv ] || \
      ! [ -f "$reproducedir"/options ] || \
      ! [ -f "$reproducedir"/usevars ] || \
      ! [ -f "$reproducedir"/cmdline ] || \
      ! [ -f "$reproducedir"/dt ]; then
    echo Error: Reproduce files not found
    exit 1
  fi
  altervar="havedt reproducing"
  if [ "$1" == alter ]; then
    shift
    altervar="havedt"
    echo Altering the reproduce files
    if [ "$1" == newdt ]; then
      shift
      altervar=""
    fi
  fi
  unset NASMENV
  eval "$(cat "$reproducedir"/nasmenv)"
  unset STACKSIZE
  unset INICOMP_METHOD
  unset INICOMP_WINNER
  unset INICOMP_SPEED_TEST
  unset INICOMP_SPEED_SCALE
  unset INICOMP_EXOMIZER_P
  unset build_options
  unset build_inicomp_options
  unset build_iniload_options
  eval "$(cat "$reproducedir"/options)"
  export use_build_revision_id=0
#  unset use_build_revision_id
  unset use_build_decomp_test
  unset use_build_inicheck
  unset use_build_shim
  unset use_build_bootable
  unset use_build_qimg
  unset use_build_bimg
  unset use_build_double_compressed
  unset use_build_compress_only
  eval "$(cat "$reproducedir"/usevars)"
  eval "$(printf "%q" "$0") $altervar $(cat "$reproducedir"/cmdline) $(shellquote "$@")"
  exit $?
fi

if (($use_build_revision_id)); then {
  [ -z "$build_revision_id" ] \
    && build_revision_id="hg $(hg id -i) ($( \
      hg log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)"
  [ -z "$build_revision_id_lmacros" -a -n "$LMACROS_DIR" ] \
    && var="hg $(hg -R "$LMACROS_DIR" id -i) ($( \
      hg -R "$LMACROS_DIR" log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)" \
    && [ "$var" != "$build_revision_id" ] \
    && build_revision_id_lmacros="'Uses lmacros:  Revision ID $var',13,10"
  [ -z "$build_revision_id_symsnip" -a -n "$SYMSNIP_DIR" -a -d "$SYMSNIP_DIR" ] \
    && var="hg $(hg -R "$SYMSNIP_DIR" id -i) ($( \
      hg -R "$SYMSNIP_DIR" log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)" \
    && [ "$var" != "$build_revision_id" ] \
    && build_revision_id_symsnip="'Uses symsnip:  Revision ID $var',13,10"
  [ -z "$build_revision_id_inicomp" ] \
    && var="hg $(hg -R "$INICOMP_DIR" id -i) ($( \
      hg -R "$INICOMP_DIR" log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)" \
    && [ "$var" != "$build_revision_id" ] \
    && build_revision_id_inicomp="'Uses inicomp:  Revision ID $var',13,10"
  [ -z "$build_revision_id_inicheck" ] \
    && var="hg $(hg -R "$INICHECK_DIR" id -i) ($( \
      hg -R "$INICHECK_DIR" log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)" \
    && [ "$var" != "$build_revision_id" ] \
    && build_revision_id_inicheck="'Uses inicheck: Revision ID $var',13,10"
  [ -z "$build_revision_id_scanptab" ] \
    && var="hg $(hg -R "$SCANPTAB_DIR" id -i) ($( \
      hg -R "$SCANPTAB_DIR" log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)" \
    && [ "$var" != "$build_revision_id" ] \
    && build_revision_id_scanptab="'Uses scanptab: Revision ID $var',13,10"
  [ -z "$build_revision_id_ldosboot" ] \
    && var="hg $(hg -R "$LDOSBOOT_DIR" id -i) ($( \
      hg -R "$LDOSBOOT_DIR" log -r 'ancestors(.)' --template '1' | wc --bytes) ancestors)" \
    && [ "$var" != "$build_revision_id" ] \
    && build_revision_id_ldosboot="'Uses ldosboot: Revision ID $var',13,10"
} fi

mkdir -p "$reproducedir"

if [ "$1" != havedt ]; then
  nasm ${miscpath}passdt.asm -o ${rootpath}tmp/scratch 2>&1 \
  -D_REVISIONID="'$build_revision_id'" \
  -D_REVISIONID_LMACROS="${build_revision_id_lmacros:-''}" \
  -D_REVISIONID_INSTSECT="${build_revision_id_instsect:-''}" \
  -D_REVISIONID_SYMSNIP="${build_revision_id_symsnip:-''}" \
  -D_REVISIONID_SCANPTAB="${build_revision_id_scanptab:-''}" \
  -D_REVISIONID_INICOMP="${build_revision_id_inicomp:-''}" \
  -D_REVISIONID_INICHECK="${build_revision_id_inicheck:-''}" \
  -D_CHECKSUM="$use_build_inicheck" \
  -D_REVISIONID_LDOSBOOT="${build_revision_id_ldosboot:-''}" \
  | perl -ne 'if (/.*(?:warning: using define )(-[UD].*?)( \[-w\+user\])?$/) { print $1."\n"; };' \
     > "$reproducedir"/dt
else
  shift
fi

if [ "$1" != reproducing ]; then
  export NASMENV
  export -p | grep -E "^declare -x NASMENV=" > "$reproducedir"/nasmenv
  export STACKSIZE
  export INICOMP_METHOD
  export INICOMP_WINNER
  export INICOMP_SPEED_TEST
  export INICOMP_SPEED_SCALE
  export INICOMP_EXOMIZER_P
  export build_options
  export build_inicomp_options
  export build_iniload_options
  export -p | grep -E "^declare -x (build_(|inicomp_|iniload_)options|INICOMP_(EXOMIZER_P|METHOD|WINNER|SPEED_TEST|SPEED_SCALE)|STACKSIZE)=" > "$reproducedir"/options
#  export use_build_revision_id
  export use_build_decomp_test
  export use_build_inicheck
  export use_build_shim
  export use_build_bootable
  export use_build_qimg
  export use_build_bimg
  export use_build_double_compressed
  export use_build_compress_only
  export -p | grep -E "^declare -x use_build_(decomp_test|inicheck|shim|bootable|qimg|bimg|double_compressed|compress_only)=" > "$reproducedir"/usevars
  shellquote "$@" > "$reproducedir"/cmdline
else
  shift
fi

# based on https://stackoverflow.com/a/22517974/738287
declare -a dtarray=()
while IFS='' read -r item; do
  dtarray+=("$item")
done < "$reproducedir"/dt

[ -z "$STACKSIZE" ] && STACKSIZE=2048

if ! (( $use_build_compress_only ))
then

echo "Creating $build_name.$binbigext"
((use_build_shim)) && [ -f ${rootpath}bin/"$build_name".$comext ] && rm ${rootpath}bin/"$build_name".$comext
((use_build_bootable)) && [ -f ${rootpath}bin/$lpre"$build_name".$comext ] && rm ${rootpath}bin/$lpre"$build_name".$comext
((use_build_bootable)) && [ -f ${rootpath}bin/$lpre"$build_name"$usuf.$comext ] && rm ${rootpath}bin/$lpre"$build_name"$usuf.$comext
# Delete the file first so in case the actual assembly fails,
#  there doesn't remain a stale result of the shim assembly.
"$NASM" -Ox iniloadc.asm -f bin \
  "$options_i_lmacros" \
  "$options_i_symsnip" \
  "$options_i_scanptab" \
  -o${rootpath}tmp/"$build_name"c.mac \
  -D_REVISIONID="'$build_revision_id'" \
  -D_REVISIONID_LMACROS="${build_revision_id_lmacros:-''}" \
  -D_REVISIONID_SCANPTAB="${build_revision_id_scanptab:-''}" \
  -D_REVISIONID_SYMSNIP="${build_revision_id_symsnip:-''}" \
  -D_REVISIONID_INICOMP="${build_revision_id_inicomp:-''}" \
  -D_REVISIONID_INICHECK="${build_revision_id_inicheck:-''}" \
  -D_CHECKSUM="$use_build_inicheck" \
  -D_REVISIONID_LDOSBOOT="${build_revision_id_ldosboot:-''}" \
  "${dtarray[@]}" $build_options "$@" && \
"$NASM" -Ox debug.asm -f bin \
  "$options_i_lmacros" \
  "$options_i_symsnip" \
  "$options_i_scanptab" \
  -o ${rootpath}tmp/"$build_name".$binbigext \
  -l ${rootpath}${lstdir}"$build_name".$lstext -D_MAP=${rootpath}${mapdir}"$build_name".$mapext \
  -D_REVISIONID="'$build_revision_id'" \
  -D_REVISIONID_LMACROS="${build_revision_id_lmacros:-''}" \
  -D_REVISIONID_SCANPTAB="${build_revision_id_scanptab:-''}" \
  -D_REVISIONID_SYMSNIP="${build_revision_id_symsnip:-''}" \
  -D_REVISIONID_INICOMP="${build_revision_id_inicomp:-''}" \
  -D_REVISIONID_INICHECK="${build_revision_id_inicheck:-''}" \
  -D_CHECKSUM="$use_build_inicheck" \
  -D_REVISIONID_LDOSBOOT="${build_revision_id_ldosboot:-''}" \
  "${dtarray[@]}" $build_options "$@"
rc=$?

else	# compress only
rc=0
fi

tellsize="$(( $("$TELLSIZE" ${rootpath}tmp/"$build_name".$binbigext) * 16 - $STACKSIZE ))"
if [ $? -eq 0 -a $rc -eq 0 ] && ((use_build_shim)); then {
  echo "Creating $build_name.com"
  "$NASM" mzshim.asm "$options_i_lmacros" "${dtarray[@]}" \
    -D_FILE=${rootpath}tmp/"$build_name".$binbigext -o${rootpath}bin/"$build_name".com \
    -D_IMAGE_EXE_AUTO_STACK="$STACKSIZE" -D_IMAGE_EXE_MAX=0 \
    -D_IMAGE_EXE_MIN_CALC="((( - (payload.actual_end - payload) \
      + $tellsize + _IMAGE_EXE_AUTO_STACK) + 15) & ~15)" \
  && perl -0777i -pe 's/(Uses (inicomp|inicheck|ldosboot|scanptab)[^\0]*\0)/"\0" x length $1/ge' \
    ${rootpath}bin/"$build_name".com
} fi

if [ -n "$LDOSBOOT_DIR" ] && ((use_build_bootable)); then {
  if [ -f "${rootpath}tmp/$build_name.$binbigext" ] \
  && grep -qaEi "^\s*Boot loader\s*$" "${rootpath}tmp/$build_name.$binbigext"; then {
    echo "Creating $lpre${build_name}$usuf.$comext"
    "$NASM" -Ox "${LDOSBOOT_DIR%/}"/iniload.asm -f bin \
      "$options_i_lmacros" \
      "$options_i_ldosboot" \
      "$options_i_scanptab" \
      -D_REVISIONID="'$build_revision_id'" \
      -D_REVISIONID_LMACROS="${build_revision_id_lmacros:-''}" \
      -D_REVISIONID_SCANPTAB="${build_revision_id_scanptab:-''}" \
      -D_REVISIONID_SYMSNIP="${build_revision_id_symsnip:-''}" \
      -D_REVISIONID_INICOMP="${build_revision_id_inicomp:-''}" \
      -D_REVISIONID_INICHECK="${build_revision_id_inicheck:-''}" \
      -D_CHECKSUM="$use_build_inicheck" \
      -D_REVISIONID_LDOSBOOT="${build_revision_id_ldosboot:-''}" \
      "${dtarray[@]}" \
      -D_INILOAD_CFG="'${rootpath}tmp/${build_name}c.mac'" \
      -I"${INICHECK_DIR%/}"/iniload/ -D_CHECKSUM="$use_build_inicheck" \
      $build_iniload_options -o${rootpath}bin/$lpre"$build_name"$usuf.$comext \
      -l${rootpath}${lstdir}$lpre"$build_name"$usuf.$lstext -D_MAP=${rootpath}${mapdir}$lpre"$build_name"$usuf.$mapext \
      -D_PAYLOAD_FILE="'${rootpath}tmp/$build_name.$binbigext'" -D_EXEC_OFFSET=32 \
      -D_IMAGE_EXE -D_IMAGE_EXE_AUTO_STACK="$STACKSIZE" -D_IMAGE_EXE_MAX=0 \
      -D_IMAGE_EXE_MIN_CALC="((( - (payload.actual_end - payload) \
        + $tellsize + _IMAGE_EXE_AUTO_STACK) + 15) & ~15)" \
    && perl -0777i -pe 's/(Uses inicomp[^\0]*\0)/"\0" x length $1/ge' \
      ${rootpath}bin/$lpre"$build_name"$usuf.$comext \
    && ((use_build_inicheck)) && "$CHECKSUM" ${rootpath}bin/$lpre"$build_name"$usuf.$comext
    ls -lgG ${rootpath}bin/$lpre"$build_name"$usuf.$comext
    if [ -z "$INICOMP_METHOD" -o "$INICOMP_METHOD" == "none" ]; then {
      fun_delete_compressed
    } else {
      if (( use_build_double_compressed ))
      then
	cp -a "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$build_name.one"
	binbigext=bi2
	cat "${rootpath}tmp/$build_name.one" "${rootpath}tmp/$build_name.one" > "${rootpath}tmp/$build_name.$binbigext"
      fi
      winmethod=""
      winduration=""
      rm -f "${rootpath}tmp/$build_name.spd"
      rm -f "${rootpath}tmp/$build_name.siz"
      nonesize="$(stat -c %s ${rootpath}bin/$lpre"$build_name"$usuf.$comext)"
      printf " %7u bytes (%6.2f%%), method %16s\n" \
	"$nonesize" \
	"$(echo "scale=2; $nonesize * 100 / $nonesize" | bc)" \
	"none" \
      | tee -a "${rootpath}tmp/$build_name.siz"
      fun_delete_compressed silent
      for method in $INICOMP_METHOD; do
	minsize="$tellsize"
	((debug)) && echo "tellsize=$tellsize"
      	inicomp_additional_memory=0
	inicomp_def_p=""
	inicomp_switch_p=""
      	if [ "$method" == "brieflz" ]; then {
      	  [ -z "$INICOMP_BLZPACK" ] && INICOMP_BLZPACK=blzpack
      	  inicomp_suffix=$blzext
      	  inicomp_option=-D_BRIEFLZ
      	} elif [ "$method" == "lz4" ]; then {
      	  [ -z "$INICOMP_LZ4C" ] && INICOMP_LZ4C=lz4c
      	  inicomp_suffix=$lz4ext
          inicomp_option=-D_LZ4
      	} elif [ "$method" == "snappy" ]; then {
      	  [ -z "$INICOMP_SNZIP" ] && INICOMP_SNZIP=snzip
      	  inicomp_suffix=$szext
      	  inicomp_option=-D_SNAPPY
      	} elif [ "$method" == "exodecr" ]; then {
      	  [ -z "$INICOMP_EXOMIZER" ] && INICOMP_EXOMIZER=exomizer
      	  inicomp_suffix=$exoext
      	  inicomp_option=-D_EXODECR
	  [ -n "$INICOMP_EXOMIZER_P" ] && inicomp_def_p="-D_P=$INICOMP_EXOMIZER_P"
	  [ -n "$INICOMP_EXOMIZER_P" ] && inicomp_switch_p="-P$INICOMP_EXOMIZER_P"
	  [ -n "$INICOMP_EXOMIZER_P" ] && echo "Using P = $INICOMP_EXOMIZER_P"
      	} elif [ "$method" == "x" ]; then {
      	  [ -z "$INICOMP_X" ] && INICOMP_X=x
      	  inicomp_suffix=$xext
      	  inicomp_option=-D_X
      	  inicomp_additional_memory="$(( 4 * 256 * 256 + 16384 ))"
		# 4 * 256 * 256 = CONTEXTSIZE
		# 16384 = threshold for multi-layer decompression
	} elif [ "$method" == "heatshrink" ]; then {
	  [ -z "$INICOMP_HEATSHRINK" ] && INICOMP_HEATSHRINK=heatshrink
	  inicomp_suffix=$hsext
	  inicomp_option=-D_HEATSHRINK
	} elif [ "$method" == "lzd" ]; then {
	  [ -z "$INICOMP_LZIP" ] && INICOMP_LZIP=lzip
	  inicomp_suffix=$lzext
	  inicomp_option=-D_LZD
      	  inicomp_additional_memory="$(( 32 * 1024 ))"
		# 32 KiB for the probability tables
	} elif [ "$method" == "lzo" ]; then {
	  [ -z "$INICOMP_LZOP" ] && INICOMP_LZOP=lzop
	  inicomp_suffix=$lzoext
	  inicomp_option=-D_LZO
	} elif [ "$method" == "lzsa2" ]; then {
	  [ -z "$INICOMP_LZSA" ] && INICOMP_LZSA=lzsa
	  inicomp_suffix=$lzsa2ext
	  inicomp_option=-D_LZSA2
	} elif [ "$method" == "apl" ]; then {
	  [ -z "$INICOMP_APULTRA" ] && INICOMP_APULTRA=apultra
	  inicomp_suffix=$aplext
	  inicomp_option=-D_APL
      	} elif [ "$method" == "bzp" ]; then {
      	  [ -z "$INICOMP_BZPACK" ] && INICOMP_BZPACK=bzpack
      	  inicomp_suffix=$bzpext
          inicomp_option=-D_BZP
	} elif [ "$method" == "none" ]; then {
	  continue
	} else {
	  echo "Invalid compression method selected: $method"
	  exit 1
	} fi
      mkdir -p ${rootpath}tmp/"$inicomp_suffix"
      rm -f ${rootpath}tmp/"$inicomp_suffix"/"$build_name"."$inicomp_suffix" \
        ${rootpath}tmp/"$inicomp_suffix"/$ppre"$build_name".$binbigext
      echo "Creating $lpre${build_name}.$comext"
      if [ "$method" == "brieflz" ]; then {
        "$INICOMP_BLZPACK" "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/$build_name.$blzext"
      } elif [ "$method" == "lz4" ]; then {
        "$INICOMP_LZ4C" -9zfk -hc "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/$build_name.$lz4ext"
      } elif [ "$method" == "snappy" ]; then {
        "$INICOMP_SNZIP" -ck "${rootpath}tmp/$build_name.$binbigext" > "${rootpath}tmp/$inicomp_suffix/$build_name.$szext"
      } elif [ "$method" == "exodecr" ]; then {
        "$INICOMP_EXOMIZER" raw $inicomp_switch_p "${rootpath}tmp/$build_name.$binbigext" -o "${rootpath}tmp/$inicomp_suffix/$build_name.$exoext"
      } elif [ "$method" == "x" ]; then {
        "$INICOMP_X" -9zfk "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/$build_name.$xext"
      } elif [ "$method" == "heatshrink" ]; then {
	rm -f "${rootpath}tmp/$inicomp_suffix/$build_name.$hsext"
	winmsg=""
	for w in 10 11 12 13 14; do
	  for l in 4 5 6 7 8 9 10 11 12 13 14; do
	    ((l >= w)) && continue
	    if msg="$("$INICOMP_HEATSHRINK" -ve -w $w -l $l "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/w${w}l$l.bin")"; then
	      if [ ! -f "${rootpath}tmp/$inicomp_suffix/$build_name.$hsext" ] \
		|| (( ( $(stat -c %s "${rootpath}tmp/$inicomp_suffix/w${w}l$l.bin") + 2 ) <= $(stat -c %s "${rootpath}tmp/$inicomp_suffix/$build_name.$hsext") )); then
		echo -ne "!"
		winmsg="$msg"
		printf "\x$(printf "%02X" "$w")\x$(printf "%02X" "$l")" > "${rootpath}tmp/$inicomp_suffix/$build_name.$hsext"
		cat "${rootpath}tmp/$inicomp_suffix/w${w}l$l.bin" >> "${rootpath}tmp/$inicomp_suffix/$build_name.$hsext"
	      else
		echo -ne "."
	      fi
	    fi
	  done
	done
	echo ""
	if [ -z "$winmsg" ]; then
	  echo "Error: Compression failure!"
	  false
	else
	  echo "$winmsg"
	  true
	fi
      } elif [ "$method" == "lzd" ]; then {
        "$INICOMP_LZIP" -9vvfkc "${rootpath}tmp/$build_name.$binbigext" > "${rootpath}tmp/$inicomp_suffix/$build_name.$lzext"
      } elif [ "$method" == "lzo" ]; then {
        "$INICOMP_LZOP" -9vf -o "${rootpath}tmp/$inicomp_suffix/$build_name.$lzoext" "${rootpath}tmp/$build_name.$binbigext"
      } elif [ "$method" == "lzsa2" ]; then {
        "$INICOMP_LZSA" -c -f2 --prefer-ratio -v "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/$build_name.$lzsa2ext"
      } elif [ "$method" == "apl" ]; then {
        "$INICOMP_APULTRA" -c -v "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/$build_name.$aplext"
      } elif [ "$method" == "bzp" ]; then {
        "$INICOMP_BZPACK" "${rootpath}tmp/$build_name.$binbigext" "${rootpath}tmp/$inicomp_suffix/$build_name.$bzpext" -e
      } else {
        echo "Internal error!"
        exit 1
      } fi
      rc=$?
      decompsize=0
      rc2=0
      if (($use_build_decomp_test)) && [ $rc -eq 0 ]
      then
        if "$NASM" "${INICOMP_DIR%/}"/inicomp.asm "$options_i_lmacros" \
        -I"${INICOMP_DIR%/}"/ "$inicomp_option" \
        "${dtarray[@]}" \
        -D_INILOAD_CFG="'${rootpath}tmp/${build_name}c.mac'" \
	$inicomp_def_p \
        $build_inicomp_options -o${rootpath}tmp/"$inicomp_suffix"/$tpre"$build_name".$binbigext \
        -l ${rootpath}tmp/"$inicomp_suffix"/$tpre"$build_name".$lstext \
        -D_MAP="${rootpath}tmp/$inicomp_suffix/$tpre$build_name.$mapext" \
        -D_PAYLOAD_FILE="'${rootpath}tmp/$inicomp_suffix/$build_name.$inicomp_suffix'" -D_EXEC_OFFSET=32 \
        -D_IMAGE_EXE -D_IMAGE_EXE_AUTO_STACK="$STACKSIZE" -D_IMAGE_EXE_MAX=0 \
        -D_TEST_PROGRAM \
        -D_TEST_PROGRESS \
        -D_TEST_PROGRAM_DECOMPRESSED_SIZE="$(stat -c %s "${rootpath}tmp/$build_name.$binbigext")" \
        -D_INCLUDE_UNCOMPRESSED -D_UNCOMPRESSED_FILE="'${rootpath}tmp/$build_name.$binbigext'" \
      && "$NASM" "${LDOSBOOT_DIR%/}"/iniload.asm "$options_i_lmacros" \
	"$options_i_ldosboot" \
	"$options_i_scanptab" \
        "${dtarray[@]}" \
	-D_INILOAD_CFG="'${rootpath}tmp/${build_name}c.mac'" \
        -I"${INICHECK_DIR%/}"/iniload/ -D_CHECKSUM="$use_build_inicheck" \
        $build_iniload_options -o${rootpath}tmp/"$inicomp_suffix"/$tpre"$build_name".$comext \
        -D_PAYLOAD_FILE="'${rootpath}tmp/$inicomp_suffix/$tpre$build_name.$binbigext'" -D_EXEC_OFFSET=32 \
        -D_IMAGE_EXE -D_IMAGE_EXE_AUTO_STACK="$STACKSIZE" -D_IMAGE_EXE_MAX=0 \
        -D_IMAGE_EXE_MIN_CALC="((-1024 \
          + $(stat -c %s "${rootpath}tmp/$inicomp_suffix/$build_name.$inicomp_suffix") \
          + $(stat -c %s "${rootpath}tmp/$build_name.$binbigext") \
          + _IMAGE_EXE_AUTO_STACK + 15 \
	  + $inicomp_additional_memory) & ~15)"
            # -1024 in the iniload build is for inicomp's INIT0
	then
	  if [[ "$DEFAULT_MACHINE" == dosemu ]]
	  then
	    decompsize="$(( ( $( \
            "$DOSEMU" < /dev/null -K "${PWD}/${rootpath}tmp/$inicomp_suffix" \
            -E $tpre"$build_name".$comext -dumb -quiet -te 2> /dev/null \
            | tee /dev/stderr | sed -re '/^(about to execute|error:|note:|info:)/Id;s/[\r\n]+$//g' \
            | sed -re 's/^Allocation of DOS memory failed.$/0/' \
            ) + 0 ) * 16 ))"
            rc2=$?
	    if (( $decompsize == 0 ))
	    then
	      echo "Error: Decompression test failed" >&2
	      ((pass)) || rc2=1
	    else
	      if (( $INICOMP_SPEED_TEST ))
	      then
		start="$(date +%s.%3N)"
		"$DOSEMU" < /dev/null -K "${PWD}/${rootpath}tmp/$inicomp_suffix" \
		-E $tpre"$build_name".$comext" b $INICOMP_SPEED_TEST" -dumb -quiet -te 2> /dev/null \

#		| sed -re '/^(about to execute|error:|note:|info:)/Id;s/[\r\n]+$//g' \

		end="$(date +%s.%3N)"
		duration="$(echo "scale=3; $end - $start" | bc)"
		printf " %7.2fs for %3u runs (%$((5 + ${INICOMP_SPEED_SCALE:-0}))sms / run), method %16s\n" \
		  "$duration" "$INICOMP_SPEED_TEST" \
		  "$(echo "scale=${INICOMP_SPEED_SCALE:-0}; $duration * 1000 / $INICOMP_SPEED_TEST" | bc)" \
		  "$method" \
		| tee -a "${rootpath}tmp/$build_name.spd"
	      fi
	    fi
          elif [[ "$DEFAULT_MACHINE" == qemu ]]
          then
            mkdir -p "${rootpath}tmp/boottest"
	    cp -aL "$BOOT_KERNEL" "${rootpath}tmp/boottest/${BOOT_KERNEL##*/}"
	    cp -aL "$BOOT_COMMAND" "${rootpath}tmp/boottest/${BOOT_COMMAND##*/}"
	    echo -ne "@echo off\r\n$tpre$build_name.$comext > output.txt\r\n" > "${rootpath}tmp/boottest/"autoexec.bat
	    echo -ne "if errorlevel 1 goto :end\r\n" >> "${rootpath}tmp/boottest/"autoexec.bat
	    echo -ne "echo success> result.txt\r\n" >> "${rootpath}tmp/boottest/"autoexec.bat
	    echo -ne ":end\r\nquit.com\r\n" >> "${rootpath}tmp/boottest/"autoexec.bat
	    "$NASM" "${rootpath}misc/quit.asm" \
	     "$options_i_lmacros" \
	     -o "${rootpath}tmp/boottest/"quit.com &&
	    "$NASM" "${LDOSBOOT_DIR%/}"/boot.asm -w-user \
	     "$options_i_lmacros" \
	     -D_COMPAT_"$BOOT_PROTOCOL"=1 \
	     -D_LBA=0 -D_USE_PART_INFO=0 -D_QUERY_GEOMETRY=0 \
	     $BOOT_OPTIONS \
	     -D_MAP="${rootpath}tmp/boottest/"boot.map \
	     -l "${rootpath}tmp/boottest/"boot.lst \
	     -o "${rootpath}tmp/boottest/"boot.bin &&
	    "$NASM" "${BOOTIMG_DIR%/}"/bootimg.asm \
	     -I ./ \
	     -I "${rootpath}tmp/boottest/" \
	     -I "${rootpath}tmp/$inicomp_suffix/" \
	     "$options_i_bootimg" \
	     "$options_i_lmacros" \
	     -o "${rootpath}tmp/boottest/"diskette.img \
	     -l "${rootpath}tmp/boottest/"diskette.lst \
	     -D_PAYLOADFILE="${BOOT_KERNEL##*/},${BOOT_COMMAND##*/},autoexec.bat,$tpre$build_name.$comext,quit.com" \
	     -D_BOOTFILE="'boot.bin'"
	    (($?)) && exit $?
	    "$QEMU" -fda "${rootpath}tmp/boottest/diskette.img" -boot order=a -display none 2> /dev/null
	    if [[ "$(mtype -t -i "${rootpath}tmp/boottest/"diskette.img ::RESULT.TXT 2> /dev/null)" != success ]]
	    then
	      echo "Error: Decompression test failed" >&2
	      decompsize=0
	      ((pass)) || rc2=1
	    else
	      decompsize="$(( $(mtype -t -i "${rootpath}tmp/boottest/"diskette.img ::OUTPUT.TXT 2> /dev/null) * 16))"
	      if (( $INICOMP_SPEED_TEST ))
	      then
	    echo -ne "@echo off\r\n$tpre$build_name.$comext b $INICOMP_SPEED_TEST\r\n" > "${rootpath}tmp/boottest/"autoexec.bat
	    echo -ne "if errorlevel 1 goto :end\r\n" >> "${rootpath}tmp/boottest/"autoexec.bat
	    echo -ne "echo success> result.txt\r\n" >> "${rootpath}tmp/boottest/"autoexec.bat
	    echo -ne ":end\r\nquit.com\r\n" >> "${rootpath}tmp/boottest/"autoexec.bat
	    "$NASM" "${BOOTIMG_DIR%/}"/bootimg.asm \
	     -I ./ \
	     -I "${rootpath}tmp/boottest/" \
	     -I "${rootpath}tmp/$inicomp_suffix/" \
	     "$options_i_bootimg" \
	     "$options_i_lmacros" \
	     -o "${rootpath}tmp/boottest/"diskette.img \
	     -l "${rootpath}tmp/boottest/"diskette.lst \
	     -D_PAYLOADFILE="${BOOT_KERNEL##*/},${BOOT_COMMAND##*/},autoexec.bat,$tpre$build_name.$comext,quit.com" \
	     -D_BOOTFILE="'boot.bin'"
	    (($?)) && exit $?
		start="$(date +%s.%3N)"
		"$QEMU" -fda "${rootpath}tmp/boottest/diskette.img" -boot order=a -display none 2> /dev/null
		end="$(date +%s.%3N)"
		duration="$(echo "scale=3; $end - $start" | bc)"
		printf " %7.2fs for %3u runs (%$((5 + ${INICOMP_SPEED_SCALE:-0}))sms / run), method %16s\n" \
		  "$duration" "$INICOMP_SPEED_TEST" \
		  "$(echo "scale=${INICOMP_SPEED_SCALE:-0}; $duration * 1000 / $INICOMP_SPEED_TEST" | bc)" \
		  "$method" \
		| tee -a "${rootpath}tmp/$build_name.spd"
	      fi
	    fi
          else
	    echo "Error: invalid machine \"$DEFAULT_MACHINE\" selected" >&2
	    exit 1
          fi
        else
          rc2=1
        fi
      fi
      if (( $decompsize == 0 ))
      then
        decompsize="$(( \
        + $(stat -c %s "${rootpath}tmp/$inicomp_suffix/$build_name.$inicomp_suffix") \
        + $(stat -c %s "${rootpath}tmp/$build_name.$binbigext") \
	+ $inicomp_additional_memory \
        + 4096 ))"
        # 4096 is a heuristic guess for larger than size of the decompressor
      fi
      ((debug)) && echo "decompsize=$decompsize"
      [ "$decompsize" -gt "$minsize" ] && minsize="$decompsize"
      ((debug)) && echo "minsize=$minsize"
      rm -f ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name".$comext
      [ $rc2 -eq 0 -a $rc -eq 0 ] \
      && "$NASM" "${INICOMP_DIR%/}"/inicomp.asm "$options_i_lmacros" \
        -I"${INICOMP_DIR%/}"/ "$inicomp_option" \
        "${dtarray[@]}" \
        -D_INILOAD_CFG="'${rootpath}tmp/${build_name}c.mac'" \
	$inicomp_def_p \
        $build_inicomp_options -o${rootpath}tmp/"$inicomp_suffix"/$ppre"$build_name".$binbigext \
        -l ${rootpath}tmp/"$inicomp_suffix"/$ppre"$build_name".$lstext \
        -D_PAYLOAD_FILE="'${rootpath}tmp/$inicomp_suffix/$build_name.$inicomp_suffix'" -D_EXEC_OFFSET=32 \
        -D_IMAGE_EXE -D_IMAGE_EXE_AUTO_STACK="$STACKSIZE" -D_IMAGE_EXE_MAX=0 \
      && "$NASM" -Ox "${LDOSBOOT_DIR%/}"/iniload.asm -f bin \
	"$options_i_lmacros" \
	"$options_i_ldosboot" \
	"$options_i_scanptab" \
	-D_REVISIONID="'$build_revision_id'" \
	-D_REVISIONID_LMACROS="${build_revision_id_lmacros:-''}" \
	-D_REVISIONID_SCANPTAB="${build_revision_id_scanptab:-''}" \
	-D_REVISIONID_SYMSNIP="${build_revision_id_symsnip:-''}" \
	-D_REVISIONID_INICOMP="${build_revision_id_inicomp:-''}" \
	-D_REVISIONID_INICHECK="${build_revision_id_inicheck:-''}" \
	-D_CHECKSUM="$use_build_inicheck" \
	-D_REVISIONID_LDOSBOOT="${build_revision_id_ldosboot:-''}" \
        "${dtarray[@]}" \
	-D_INILOAD_CFG="'${rootpath}tmp/${build_name}c.mac'" \
        -I"${INICHECK_DIR%/}"/iniload/ -D_CHECKSUM="$use_build_inicheck" \
        $build_iniload_options -o${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name".$comext \
        -l ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name"$csuf.$lstext -D_MAP=${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name"$csuf.$mapext \
        -D_PAYLOAD_FILE="'${rootpath}tmp/$inicomp_suffix/$ppre$build_name.$binbigext'" -D_EXEC_OFFSET=32 \
        -D_IMAGE_EXE -D_IMAGE_EXE_AUTO_STACK="$STACKSIZE" -D_IMAGE_EXE_MAX=0 \
        -D_IMAGE_EXE_MIN_CALC="(( \
          - (payload.actual_end - payload) \
          + "$minsize" \
          + _IMAGE_EXE_AUTO_STACK + 15) & ~15)" \
      && ((use_build_inicheck)) && "$CHECKSUM" ${rootpath}tmp/$lpre"$inicomp_suffix"/"$build_name".$comext
	if [ -f ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name".$comext ]; then
	  compsize="$(stat -c %s ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name".$comext)"
	  printf " %7u bytes (%6.2f%%), method %16s\n" \
	    "$compsize" \
	    "$(echo "scale=2; $compsize * 100 / $nonesize" | bc)" \
	    "$method" \
	  | tee -a "${rootpath}tmp/$build_name.siz"
	  if [[ -z "$INICOMP_WINNER" || "$INICOMP_WINNER" == "smallest" ]] && \
	    { [ ! -f "${rootpath}bin/$lpre$build_name.$comext" ] \
	      || (( $compsize < $(stat -c %s "${rootpath}bin/$lpre$build_name.$comext") ))
	      } || \
	    [[ "$INICOMP_WINNER" == "$method" ]] || \
	    { [[ "$INICOMP_WINNER" == "fastest" ]] && \
	      (( $use_build_decomp_test )) && \
	      (( $INICOMP_SPEED_TEST )) && \
	      [[ -z "$winduration" || "$(echo "$winduration > $duration" | bc)" == 1 ]]
	    }; then
	    winduration="$duration"
	    winmethod="$method"
	    cp -a ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name".$comext ${rootpath}bin
	    cp -a ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name"$csuf.$lstext ${rootpath}${lstdir}
	    cp -a ${rootpath}tmp/"$inicomp_suffix"/$lpre"$build_name"$csuf.$mapext ${rootpath}${mapdir}
	  fi
	fi
      done
	if [ -n "$winmethod" ]; then
	  echo "Note: Method $winmethod selected."
	  if (( $use_build_decomp_test )) && (( $INICOMP_SPEED_TEST ))
	  then
	    printf "Note: Duration is %7.2fs for %3u runs (%$((5 + ${INICOMP_SPEED_SCALE:-0}))sms / run).\n" \
	      "$winduration" "$INICOMP_SPEED_TEST" \
	      "$(echo "scale=${INICOMP_SPEED_SCALE:-0}; $winduration * 1000 / $INICOMP_SPEED_TEST" | bc)"
	  fi
	  ls -lgG ${rootpath}bin/$lpre"$build_name".$comext
	else
	  echo Error: No compression method succeeded.
	  false
	fi
    } fi
    ((use_build_qimg)) && ./makqimg.sh
    ((use_build_bimg)) && ./makbimg.sh
  } else {
    echo "Deleting $lpre${build_name}$usuf.$comext"
    rm -f ${rootpath}bin/$lpre"$build_name"$usuf.$comext \
      ${rootpath}${lstdir}$lpre"$build_name"$usuf.$lstext ${rootpath}${mapdir}$lpre"$build_name"$usuf.$mapext
    fun_delete_compressed
  } fi
} fi
