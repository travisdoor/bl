//*****************************************************************************
// blc
//
// File:   bldebug.h
// Author: Martin Dorazil
// Date:   26.1.18
//
// Copyright 2018 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//*****************************************************************************

#ifndef BLDEBUG_H_VYI9AXGT
#define BLDEBUG_H_VYI9AXGT

#include <stdlib.h>
#include <stdio.h>

#define BL_YELLOW(str) \
    "\x1b[33m" str "\x1b[0m"

#define BL_RED(str) \
    "\x1b[31m" str "\x1b[0m"

#define BL_GREEN(str) \
    "\x1b[32m" str "\x1b[0m"

#define bl_assert(expr, format, ...) \
    if ((expr) == 0) { \
        fprintf(stderr, BL_RED("assert: ") format "\n", ##__VA_ARGS__); \
        abort(); \
    }

#define bl_abort(format, ...) \
    { \
        fprintf(stderr, BL_RED("abort: ") format "\n", ##__VA_ARGS__); \
        abort(); \
    }

#define bl_error(format, ...) \
    { \
        fprintf(stderr, BL_RED("error: ") format "\n", ##__VA_ARGS__); \
    }

#define bl_warning(format, ...) \
    { \
        fprintf(stdout, BL_YELLOW("warning: ") format "\n", ##__VA_ARGS__); \
    }

#define bl_log(format, ...) \
    { \
        fprintf(stdout, format "\n", ##__VA_ARGS__); \
    }

#endif /* end of include guard: BLDEBUG_H_VYI9AXGT */

