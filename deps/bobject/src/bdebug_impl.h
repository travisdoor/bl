//*****************************************************************************
// Biscuit Object
//
// File:   bdebug_impl.h
// Author: Martin Dorazil
// Date:   27/10/2017
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

#ifndef BDEBUG_IMPL_H_8E1MLQF4
#define BDEBUG_IMPL_H_8E1MLQF4

#include "bobject/bdebug.h"
#include "bobject/config.h"

void
_error(const char *format, ...);

#define bo_assert(exp, format, ...) \
  if ((exp) == 0) { \
    _error((format), ##__VA_ARGS__); \
    abort(); \
  }

#define bo_abort(format, ...) \
  { \
    _error((format), ##__VA_ARGS__); \
    abort(); \
  } \


#endif /* end of include guard: BDEBUG_IMPL_H_8E1MLQF4 */
