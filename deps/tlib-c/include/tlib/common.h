// =================================================================================================
// tlib-c
//
// File:   common.h
// Author: Martin Dorazil
// Date:   29/9/2019
//
// Copyright 2019 Martin Dorazil
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
// =================================================================================================

#ifndef T_COMMON_H
#define T_COMMON_H

#include "tlib/config.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Check whether two iterators are equal.
#define TITERATOR_EQUAL(_first, _second) ((_first).opaque == (_second).opaque)

// Return size of of static array.
#define TARRAY_SIZE(_array) (sizeof((_array)) / sizeof((_array)[0]))

// Abort execution with message printed out to the stderr stream.
#define TABORT(_msg, ...)                                                                          \
    {                                                                                              \
        fprintf(stderr, (_msg), ##__VA_ARGS__);                                                    \
        abort();                                                                                   \
    }

#ifdef _MSC_VER
#define TAPI __declspec(dllexport)
#pragma warning(disable : 4706)
#else
#define TAPI __attribute__((__visibility__("default")))
#endif

// Common tlib types.
typedef char               s8;
typedef short              s16;
typedef int                s32;
typedef long long          s64;
typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;
typedef size_t             usize;
typedef float              f32;
typedef double             f64;

// Iterator handle.
typedef struct TIterator {
    void *opaque;
} TIterator;

typedef void *(*TAllocFn)(usize bytes, const char *filename, s32 line);
typedef void (*TFreeFn)(void *ptr);

// This setter can be used to specify custom malloc and free handling.
TAPI void tlib_set_allocator(TAllocFn malloc_fn, TFreeFn free_fn);

#endif
