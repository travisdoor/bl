//************************************************************************************************
// bl
//
// File:   common.c
// Author: Martin Dorazil
// Date:   11.8.18
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

#include "common.h"

#ifndef BL_COMPILER_MSVC
#include "unistd.h"
#endif

bool
file_exists(const char *filepath)
{
#ifdef BL_COMPILER_MSVC
  return PathFileExistsA(filepath);
#else
  return access(filepath, F_OK) != -1;
#endif
}

const char *
brealpath(const char *file, char *out, int out_len)
{
  const char *resolved = NULL;
  assert(out);
  assert(out_len);
  if (!file) return resolved;

#ifdef BL_COMPILER_MSVC
  if (GetFullPathNameA(file, out_len, out, NULL) && file_exists(out)) return &out[0];
  return NULL;
#else
  return realpath(file, out);
#endif
}
