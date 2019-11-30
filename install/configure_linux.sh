#!/bin/bash

echo Running blc setup...
realpath() {
    OURPWD=$PWD
    cd "$(dirname "$1")"
    LINK=$(readlink "$(basename "$1")")
    while [ "$LINK" ]; do
      cd "$(dirname "$LINK")"
      LINK=$(readlink "$(basename "$1")")
    done
    REALPATH="$PWD/$(basename "$1")"
    cd "$OURPWD"
    echo "$REALPATH"
}

SCRIPT=$(readlink -f $0)
WDIR=$(dirname $SCRIPT)
echo working directory: $WDIR
cd $WDIR

CONFIG_FILE="../etc/bl.conf"
RT_ENTRY_POINT="__start"
STATUS=0

echo "- Looking for bl APIs"
LIB_DIR="../lib/bl"
if [ -d "$LIB_DIR" ]; then
    LIB_DIR=$(realpath $LIB_DIR)
    echo "  FOUND - $LIB_DIR"
else
    echo "  error: Cannot find bl APIs. You can try to set correct path manually in etc/bl.conf file."
    $STATUS=1
fi

echo "- Looking for ld"
LINKER_EXEC=$(which ld run)
if [ -z "$LINKER_EXEC" ]; then
    echo "  error: Cannot find ld linker. You can try to set correct path manually in etc/bl.conf file."
    $STATUS=1
else
    echo "  FOUND - $LINKER_EXEC"
fi

#echo "- Looking for C runtime objects"
#CRT1_O="/usr/lib/x86_64-linux-gnu/crt1.o"
#if [ -e "$CRT1_O" ]; then
#    echo "  FOUND - $CRT1_O"
#else
#    echo "  error: Cannot find '$CRT1_O'. You can try to set correct path manually in etc/bl.conf file."
#    $STATUS=1
#fi

#CRTI_O="/usr/lib/x86_64-linux-gnu/crti.o"
#if [ -e "$CRTI_O" ]; then
#    echo "  FOUND - $CRTI_O"
#else
#    echo "  error: Cannot find '$CRTI_O'. You can try to set correct path manually in etc/bl.conf file."
#    $STATUS=1
#fi

#CRTN_O="/usr/lib/x86_64-linux-gnu/crtn.o"
#if [ -e "$CRTN_O" ]; then
#    echo "  FOUND - $CRTN_O"
#else
#    echo "  error: Cannot find '$CRTN_O'. You can try to set correct path manually in etc/bl.conf file."
#    $STATUS=1
#fi

#LDLIB="/lib64/ld-linux-x86-64.so.2"
#if [ -e "$LDLIB" ]; then
#    echo "  FOUND - $LDLIB"
#else
#    echo "  error: Cannot find '$LDLIB'. You can try to set correct path manually in etc/bl.conf file."
#    $STATUS=1
#fi

RT_O=$LIB_DIR/rt/blrt_x86_64_linux.o
LINKER_OPT="-e $RT_ENTRY_POINT $RT_O --hash-style=gnu --no-add-needed --build-id --eh-frame-hdr -dynamic-linker $LDLIB -lc -lm"

rm -f $CONFIG_FILE
mkdir -p ../etc
printf "/*\n * blc config file\n */\n\n" >> $CONFIG_FILE
echo LIB_DIR \"$LIB_DIR/api\" >> $CONFIG_FILE
echo LINKER_EXEC \"$LINKER_EXEC\" >> $CONFIG_FILE
echo LINKER_OPT \"$LINKER_OPT\" >> $CONFIG_FILE
echo LINKER_LIB_PATH \"/usr/lib:/usr/local/lib:/lib64\" >> $CONFIG_FILE

if [ $STATUS -eq 0 ]; then
    CONFIG_FILE=$(realpath $CONFIG_FILE)
    echo Configuration finnished without errors and written to $CONFIG_FILE file.
else
    echo Configuration finnished with errors.
fi


exit $STATUS
