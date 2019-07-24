//*****************************************************************************
// Biscuit Object
//
// File:   hash.h
// Author: Martin Dorazil
// Date:   23.8.17
//
// Copyright 2017 Martin Dorazil
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

#ifndef BOBJECT_HASH_H
#define BOBJECT_HASH_H

#include <stdio.h>
#include "bobject/types.h"
#include "bobject/utils.h"

/* TODO: move to utils */

BO_BEGIN_DECLS

static inline uint32_t
bo_hash_from_str(const char *str)
{
  uint32_t hash = 5381;
  int c;

  while ((c = *str++))
    hash = ((hash << 5) + hash) + c;

  return hash;
}

static inline uint32_t
bo_hash_from_strn(const char *str,
                  size_t      len)
{
  uint32_t hash = 5381;
  size_t i = 0;
  int c;

  while ((c = *str++) && (i < len)) {
    ++i;
    hash = ((hash << 5) + hash) + c;
  }

  return hash;
}

BO_END_DECLS

#endif // BOBJECT_HASH_H
