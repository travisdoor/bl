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

#include "config.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <tlib/tlib.h>

#if defined(BL_COMPILER_MSVC)
#define BL_DEBUG_BREAK __debugbreak()
#else
#define BL_DEBUG_BREAK
#endif

#ifdef __cplusplus
extern "C" {
#endif

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

typedef enum { LOG_ASSERT, LOG_ABORT, LOG_WARNING, LOG_MSG } bl_log_msg_type_e;

void _log(bl_log_msg_type_e t, const char *file, s32 line, const char *msg, ...);

void print_trace(void);

void *_assert_invalid_expr(const char *expr, const char *file, s32 line);

#if BL_ASSERT_ENABLE || BL_DEBUG
#define BL_ASSERT(e)                                                                               \
	if (!(e)) {                                                                                \
		_log(LOG_ASSERT, __FILENAME__, __LINE__, #e);                                      \
		print_trace();                                                                     \
		BL_DEBUG_BREAK;                                                                    \
		abort();                                                                           \
	}
#define BL_MAGIC_ASSERT(O)                                                                         \
	{                                                                                          \
		BL_ASSERT(O && "Invalid reference!");                                              \
		BL_ASSERT((O)->_magic == (void *)&(O)->_magic && "Invalid magic!");                \
	}
#define BL_MAGIC_ADD void *_magic
#define BL_MAGIC_SET(O) (O)->_magic = (void *)&(O)->_magic
#else
#define BL_ASSERT(e)                                                                               \
	while (0) {                                                                                \
	}
#define BL_MAGIC_ASSERT(O)
#define BL_MAGIC_ADD
#define BL_MAGIC_SET(O)
#endif

#ifdef BL_DEBUG
#define BL_LOG(format, ...)                                                                        \
	{                                                                                          \
		_log(LOG_MSG, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                      \
	}

#define BL_WARNING(format, ...)                                                                    \
	{                                                                                          \
		_log(LOG_WARNING, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                  \
	}

#else /* !BL_DEBUG */

#define BL_LOG(format, ...)                                                                        \
	while (0) {                                                                                \
	}

#define BL_WARNING(format, ...)                                                                    \
	while (0) {                                                                                \
	}

#endif /* BL_DEBUG */

#define BL_ABORT(format, ...)                                                                      \
	{                                                                                          \
		_log(LOG_ABORT, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                    \
		print_trace();                                                                     \
		BL_DEBUG_BREAK;                                                                    \
		abort();                                                                           \
	}

#define BL_ABORT_ISSUE(N)                                                                          \
	{                                                                                          \
		_log(LOG_ABORT,                                                                    \
		     __FILENAME__,                                                                 \
		     __LINE__,                                                                     \
		     "Issue: https://github.com/travisdoor/bl/issues/" #N);                        \
		print_trace();                                                                     \
		abort();                                                                           \
	}

#define BL_WARNING_ISSUE(N)                                                                        \
	{                                                                                          \
		_log(LOG_WARNING,                                                                  \
		     __FILENAME__,                                                                 \
		     __LINE__,                                                                     \
		     "Issue: https://github.com/travisdoor/bl/issues/" #N);                        \
	}

#define BL_UNIMPLEMENTED                                                                           \
	{                                                                                          \
		_log(LOG_ABORT, __FILENAME__, __LINE__, "unimplemented");                          \
		print_trace();                                                                     \
		BL_DEBUG_BREAK;                                                                    \
		abort();                                                                           \
	}

#define BL_UNIMPLEMENTED_REGION(R)                                                                 \
	{                                                                                          \
		_log(LOG_ABORT, __FILENAME__, __LINE__, "unimplemented region '" #R "'");          \
		print_trace();                                                                     \
		BL_DEBUG_BREAK;                                                                    \
		abort();                                                                           \
	}

#if TRACY_ENABLE
#define BL_TRACY_MESSAGE(tag, format, ...)                                                         \
	{                                                                                          \
		char buf[256];                                                                     \
		snprintf(buf, ARRAY_SIZE(buf), "#%s " format, tag, ##__VA_ARGS__);                 \
		TracyCMessageC(buf, strlen(buf), thash_from_str(tag));                             \
	}
#else
#define BL_TRACY_MESSAGE(format, ...)                                                              \
	while (0) {                                                                                \
	}
#endif
#ifdef __cplusplus
}
#endif
#endif
