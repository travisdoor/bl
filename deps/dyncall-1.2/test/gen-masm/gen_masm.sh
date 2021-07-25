#!/bin/sh
cd `dirname $0`

../../portasm/gen-masm.sh call_x86
../../portasm/gen-masm.sh call_x64

