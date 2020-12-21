//*****************************************************************************
// tlib-c
//
// File:   common.c
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
//*****************************************************************************

#include "tlib/common.h"
#include "tmemory.h"

static void *default_malloc(usize size, const char *filename, s32 line)
{
    (void)filename;
    (void)line;
    return malloc(size);
}

TAllocFn _tmalloc = &default_malloc;
TFreeFn  _tfree   = &free;

void tlib_set_allocator(TAllocFn malloc_fn, TFreeFn free_fn)
{
    _tmalloc = malloc_fn;
    _tfree   = free_fn;
}
