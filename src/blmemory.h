// =================================================================================================
// bl
//
// File:   blmemory.h
// Author: Martin Dorazil
// Date:   3/1/18
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
// =================================================================================================

#ifndef BL_BLMEMORY_H
#define BL_BLMEMORY_H

#include "config.h"

#if BL_PLATFORM_WIN && BL_DEBUG
#define BL_CRTDBG_ALLOC 0
#else
// This is available only on windows.
#define BL_CRTDBG_ALLOC 0
#endif

#include <tlib/tlib.h>

#define bmalloc(size) _bl_malloc(size, __FILE__, __LINE__)
#define brealloc(ptr, size) _bl_realloc(ptr, size, __FILE__, __LINE__)
#define bfree(ptr) _bl_free(ptr)

void *_bl_realloc(void *ptr, const size_t size, const char *filename, s32 line);
void *_bl_malloc(const size_t size, const char *filename, s32 line);
void  _bl_free(void *ptr);

#endif // BL_BLMEMORY_H
