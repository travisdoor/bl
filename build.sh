#!/bin/bash

mkdir -p build/release
mkdir -p build/debug

# cd build/release
# cmake -G"Ninja" ../../ -DCMAKE_BUILD_TYPE=Release -DLLVM_DIR=/opt/homebrew/Cellar/llvm/16.0.6/lib/cmake/llvm -DBL_RPMALLOC_ENABLE=ON
# ninja

cd build/debug
cmake -G"Ninja" ../../ -DCMAKE_BUILD_TYPE=Debug -DLLVM_DIR=/opt/homebrew/Cellar/llvm/16.0.6/lib/cmake/llvm -DBL_RPMALLOC_ENABLE=ON
ninja

cp compile_commands.json ../../compile_commands.json
