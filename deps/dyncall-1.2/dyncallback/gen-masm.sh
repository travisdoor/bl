#!/bin/sh
cd `dirname $0`

../portasm/gen-masm.sh dyncall_callback_x86 _masm
../portasm/gen-masm.sh dyncall_callback_x64 _masm

