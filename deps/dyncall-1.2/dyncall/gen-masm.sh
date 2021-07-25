#!/bin/sh
cd `dirname $0`

../portasm/gen-masm.sh dyncall_call_x86 _generic_masm
../portasm/gen-masm.sh dyncall_call_x64 _generic_masm

