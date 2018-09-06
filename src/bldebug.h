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

#ifndef BL_DEBUG_H
#define BL_DEBUG_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <bobject/bobject.h>
#include "config.h"

#ifdef BL_NO_COLOR
#define BL_COLOR_END
#define BL_MAGENTA_BEGIN
#define BL_CYAN_BEGIN
#define BL_RED_BEGIN
#define BL_YELLOW_BEGIN
#define BL_GREEN_BEGIN
#define BL_BLUE_BEGIN
#else
#define BL_COLOR_END "\x1b[0m"
#define BL_MAGENTA_BEGIN "\x1b[35m"
#define BL_CYAN_BEGIN "\x1b[36m"
#define BL_RED_BEGIN "\x1b[31m"
#define BL_YELLOW_BEGIN "\x1b[33m"
#define BL_BLUE_BEGIN "\x1b[34m"
#define BL_GREEN_BEGIN "\x1b[32m"
#endif

#define BL_YELLOW(str) BL_YELLOW_BEGIN str BL_COLOR_END
#define BL_RED(str) BL_RED_BEGIN str BL_COLOR_END
#define BL_GREEN(str) BL_GREEN_BEGIN str BL_COLOR_END
#define BL_MAGENTA(str) BL_MAGENTA_BEGIN str BL_COLOR_END
#define BL_CYAN(str) BL_CYAN_BEGIN str BL_COLOR_END
#define BL_BLUE(str) BL_BLUE_BEGIN str BL_COLOR_END

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

typedef enum
{
  BL_LOG_ASSERT,
  BL_LOG_ABORT,
  BL_LOG_WARNING,
  BL_LOG_MSG
} bl_log_msg_type_e;

void
_bl_log(bl_log_msg_type_e t, const char *file, int line, const char *msg, ...);

#ifdef BL_DEBUG
#define bl_log(format, ...)                                                                        \
  {                                                                                                \
    _bl_log(BL_LOG_MSG, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                            \
  }

#define bl_warning(format, ...)                                                                    \
  {                                                                                                \
    _bl_log(BL_LOG_WARNING, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                        \
  }

#else
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

#endif 
