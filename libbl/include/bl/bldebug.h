//************************************************************************************************
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
//************************************************************************************************

#ifndef BLDEBUG_H_VYI9AXGT
#define BLDEBUG_H_VYI9AXGT

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <bobject/bobject.h>
#include "bl/config.h"

BO_BEGIN_DECLS

#define BL_YELLOW(str) "\x1b[33m" str "\x1b[0m"
#define BL_RED(str) "\x1b[31m" str "\x1b[0m"
#define BL_GREEN(str) "\x1b[32m" str "\x1b[0m"
#define BL_MAGENTA(str) "\x1b[35m" str "\x1b[0m"
#define BL_CYAN(str) "\x1b[36m" str "\x1b[0m"

#if defined(BL_COMPILER_GNUC) || defined(BL_COMPILER_CLANG)
#ifndef __FILENAME__
#define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)
#endif
#elif defined(BL_COMPILER_MSVC)
#ifndef __FILENAME__
#define __FILENAME__ (strrchr(__FILE__, '\\') ? strrchr(__FILE__, '\\') + 1 : __FILE__)
#endif
#else
#pragma message("WARNING: Cannot parse filename with this compiler")
#define __FILENAME__
#endif

typedef enum { BL_LOG_ASSERT, BL_LOG_ABORT, BL_LOG_WARNING, BL_LOG_MSG } bl_log_msg_type_e;

extern BO_EXPORT void
_bl_log(bl_log_msg_type_e t, const char *file, int line, const char *msg, ...);

#ifdef BL_DEBUG
#define bl_assert(expr, format, ...)                                                               \
  if ((expr) == 0) {                                                                               \
    _bl_log(BL_LOG_ASSERT, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                         \
    assert(false);                                                                                       \
  }

#define bl_log(format, ...)                                                                        \
  {                                                                                                \
    _bl_log(BL_LOG_MSG, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                            \
  }

#define bl_warning(format, ...)                                                                    \
  {                                                                                                \
    _bl_log(BL_LOG_WARNING, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                        \
  }

#else
#define bl_assert(expr, format, ...)                                                               \
  while (0) {                                                                                      \
  }

#define bl_log(format, ...)                                                                        \
  while (0) {                                                                                      \
  }

#define bl_warning(format, ...)                                                                    \
  while (0) {                                                                                      \
  }
#endif

#define bl_abort(format, ...)                                                                      \
  {                                                                                                \
    _bl_log(BL_LOG_ABORT, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                          \
    abort();                                                                                       \
  }

BO_END_DECLS

#endif /* end of include guard: BLDEBUG_H_VYI9AXGT */
