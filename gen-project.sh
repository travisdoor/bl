#!/bin/bash
WDIR=$(pwd)
if [ -z "$1" ]
  then
    echo "Expected build type. <Release|Debug>"
    exit
fi
BUILD_TYPE=$1
mkdir -p build
cd build
rm -f CMakeCache.txt
if [ "$(uname)" == "Darwin" ]; then
    echo "Generate project in " $(pwd) "for macOS."
    cmake .. -DCMAKE_BUILD_TYPE=${BUILD_TYPE} 
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    echo "Generate project in " $(pwd) "for Linux."
    cmake .. -DCMAKE_BUILD_TYPE=${BUILD_TYPE} 
else 
    echo "Generate project in " $(pwd) "for Windows."
    cmake .. -G "Visual Studio 16 2019" -Thost=x64 -DCMAKE_BUILD_TYPE=${BUILD_TYPE} 
fi
echo done
# Cleanup
cd ${WDIR}
