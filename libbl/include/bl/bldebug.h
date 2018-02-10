//*****************************************************************************
// bl
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

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_RESET   "\x1b[0m"
#define ANSI_COLOR_GREEN   "\x1b[32m"

#define bl_assert(expr, format, ...) \
    if ((expr) == 0) { \
        fprintf(stderr, ANSI_COLOR_RED "[ASSERT]  " format ANSI_COLOR_RESET "\n", ##__VA_ARGS__); \
        abort(); \
    }

#define bl_abort(format, ...) \
    { \
        fprintf(stderr, ANSI_COLOR_RED "[ABORT] " format ANSI_COLOR_RESET "\n", ##__VA_ARGS__); \
        abort(); \
    }

#define bl_error(format, ...) \
    { \
        fprintf(stderr, ANSI_COLOR_RED "[ERROR]   " format ANSI_COLOR_RESET "\n", ##__VA_ARGS__); \
    }

#define bl_warning(format, ...) \
    { \
        fprintf(stdout, ANSI_COLOR_YELLOW "[WARNING] " format ANSI_COLOR_RESET "\n", ##__VA_ARGS__); \
    }

#define bl_log(format, ...) \
    { \
        fprintf(stdout, format "\n", ##__VA_ARGS__); \
    }

#endif /* end of include guard: BLDEBUG_H_VYI9AXGT */

