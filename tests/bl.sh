#!/bin/bash  
exec_name=${1%.*}
../bin/bl $1 && llvm-dis $1.bc && llc -filetype=obj $1.bc && clang $1.bc -o $exec_name
