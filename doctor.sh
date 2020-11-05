#!/bin/bash
WDIR=$(pwd)
cd tests
if [ "$(uname)" == "Darwin" ]; then
    ./doctor-macos "$@"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    ./doctor-linux "$@"
else
    ./doctor-win64.exe "$@"
fi
# Cleanup
cd ${WDIR}
