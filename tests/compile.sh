#!/usr/bin/env bash

../bin/bl test.bl
cp ../bin/bl/main.c .
clang -o test main.c
