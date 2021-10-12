// =================================================================================================
// bl
//
// File:   blmemory.c
// Author: Martin Dorazil
// Date:   21/6/20
//
// Copyright 2020 Martin Dorazil
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

#include "blmemory.h"
#include "TracyC.h"
#include "common.h"
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

void *_bl_realloc(void *ptr, const size_t size, const char UNUSED(*filename), s32 UNUSED(line))
{
    void *mem = realloc(ptr, size);
    if (!mem) abort();
    TracyCFree(ptr);
    TracyCAlloc(mem, size);
    return mem;
}

void *_bl_malloc(const size_t size, const char UNUSED(*filename), s32 UNUSED(line))
{
    void *mem = malloc(size);
    if (!mem) abort();
    TracyCAlloc(mem, size);
    return mem;
}

void _bl_free(void *ptr)
{
    TracyCFree(ptr);
    free(ptr);
}
