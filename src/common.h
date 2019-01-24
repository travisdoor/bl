//************************************************************************************************
// bl
//
// File:   common.h
// Author: Martin Dorazil
// Date:   03/03/2018
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
//************************************************************************************************

#ifndef BL_COMMON_H
#define BL_COMMON_H

#include <limits.h>
#include "bldebug.h"
#include "config.h"
#include "error.h"
#include "messages.h"
#include "blmemory.h"

#if defined(BL_COMPILER_CLANG) || defined(BL_COMPILER_GNUC)
#define DEPRECATED __attribute__((deprecated))
#else
#define DEPRECATED
#endif

#define ARRAY_SIZE(array) (sizeof(array) / sizeof(array[0]))

#define barray_foreach(arr, it)                                                                    \
  if (bo_array_size((arr)))                                                                        \
    for (size_t i = 0; i < bo_array_size((arr)) && ((it) = bo_array_at((arr), i, void *)); ++i)

#define array_foreach(arr, it)                                                                     \
  for (size_t _keep = 1, i = 0, _size = ARRAY_SIZE((arr)); _keep && i != _size;                    \
       _keep = !_keep, i++)                                                                        \
    for (it = (arr)[i]; _keep; _keep = !_keep)

#define bhtbl_foreach(htbl, it)                                                                    \
  (it) = bo_htbl_begin((htbl));                                                                    \
  for (bo_iterator_t end = bo_htbl_end((htbl)); !bo_iterator_equal(&(it), &end);                   \
       bo_htbl_iter_next((htbl), &(it)))

#define blist_foreach(list, it)                                                                    \
  (it) = bo_list_begin((list));                                                                    \
  for (bo_iterator_t end = bo_list_end((list)); !bo_iterator_equal(&(it), &end);                   \
       bo_list_iter_next((list), &(it)))

bool
file_exists(const char *filepath);

const char *
brealpath(const char *file, char *out, int out_len);

void
date_time(char *buf, int len, const char *format);

bool
is_aligned(const void *p, int32_t alignment);

void
align_ptr_up(void **p, size_t alignment, ptrdiff_t *adjustment);

#endif
