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
RT_ENTRY_POINT="_start"
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

LDLIB="/lib64/ld-linux-x86-64.so.2"
if [ -e "$LDLIB" ]; then
    echo "  FOUND - $LDLIB"
else
    echo "  error: Cannot find '$LDLIB'. You can try to set correct path manually in etc/bl.conf file."
    $STATUS=1
fi

RT_O=$LIB_DIR/rt/blrt_x86_64_linux.o
LINKER_OPT="-e $RT_ENTRY_POINT $RT_O --hash-style=gnu --no-add-needed --build-id --eh-frame-hdr -dynamic-linker $LDLIB -lc -lm -ldl -lpthread"

rm -f $CONFIG_FILE
mkdir -p ../etc
printf "/*\n * blc config file\n */\n\n" >> $CONFIG_FILE
echo LIB_DIR \"$LIB_DIR/api\" >> $CONFIG_FILE
echo LINKER_EXEC \"$LINKER_EXEC\" >> $CONFIG_FILE
echo LINKER_OPT \"$LINKER_OPT\" >> $CONFIG_FILE
echo LINKER_LIB_PATH \"/usr/lib:/usr/local/lib:/lib64:/usr/lib/x86_64-linux-gnu\" >> $CONFIG_FILE

if [ $STATUS -eq 0 ]; then
    CONFIG_FILE=$(realpath $CONFIG_FILE)
    echo Configuration finished without errors and written to $CONFIG_FILE file.
else
    echo Configuration finished with errors.
fi


exit $STATUS
