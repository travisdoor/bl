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

CMD_TOOLS="/Library/Developer/CommandLineTools"
WDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
echo working directory: $WDIR
cd $WDIR

CONFIG_FILE="../etc/bl.conf"
MACOS_VER="$(sw_vers -productVersion)"
RT_ENTRY_POINT="___os_start"
STATUS=0

echo "- Looking for bl APIs"
LIB_DIR="../lib/bl/api"
if [ -d "$LIB_DIR" ]; then
    LIB_DIR=$(realpath $LIB_DIR)
    echo "  FOUND - $LIB_DIR"
else
    echo "  error: Cannot find bl APIs. You can try to set correct path manually in etc/bl.conf file."
    $STATUS=1
fi

echo "- Looking XCode command line tools"
if [ -z "$CMD_TOOLS" ]; then
    echo "  error: Cannot find XCode command line tools. Use 'xcode-select --install' to fix this."
    $STATUS=1
else
    echo "  FOUND - $CMD_TOOLS"
fi

echo "- Looking for ld"
LINKER_EXEC="$CMD_TOOLS/usr/bin/ld"
if [ -z "$LINKER_EXEC" ]; then
    echo "  error: Cannot find 'ld' linker. You can try to set correct path manually in etc/bl.conf file."
    $STATUS=1
else
    echo "  FOUND - $LINKER_EXEC"
fi

#echo "- Looking for C runtime objects"
#CRT_O="$CMD_TOOLS/SDKs/MacOSX.sdk/usr/lib/crt1.o"
#if [ -e "$CRT_O" ]; then
#    echo "  FOUND - $CRT_O"
#else
#    echo "  error: Cannot find '$CRT_O'. You can try to set correct path manually in etc/bl.conf file."
#    $STATUS=1
#fi

LINKER_OPT="-e $RT_ENTRY_POINT -macosx_version_min $MACOS_VER -lc"

rm -f $CONFIG_FILE
mkdir -p ../etc
printf "/*\n * blc config file\n */\n\n" >> $CONFIG_FILE
echo LIB_DIR \"$LIB_DIR\" >> $CONFIG_FILE
echo LINKER_EXEC \"$LINKER_EXEC\" >> $CONFIG_FILE
echo LINKER_OPT \"$LINKER_OPT\" >> $CONFIG_FILE
echo LINKER_LIB_PATH \"/usr/lib:/usr/local/lib\" >> $CONFIG_FILE

if [ $STATUS -eq 0 ]; then
    CONFIG_FILE=$(realpath $CONFIG_FILE)
    echo Configuration finished without errors and writtent to $CONFIG_FILE file.
else
    echo Configuration finished with errors.
fi


exit $STATUS
