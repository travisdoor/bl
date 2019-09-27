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
#include <bobject/bobject.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef BL_NO_COLOR
#define COLOR_END ""
#define MAGENTA_BEGIN ""
#define CYAN_BEGIN ""
#define RED_BEGIN ""
#define RED_BG_BEGIN ""
#define YELLOW_BEGIN ""
#define GREEN_BEGIN ""
#define BLUE_BEGIN ""
#else
#define COLOR_END "\x1b[0m"
#define MAGENTA_BEGIN "\x1b[35m"
#define CYAN_BEGIN "\x1b[36m"
#define RED_BEGIN "\x1b[31m"
#define RED_BG_BEGIN "\x1b[41m"
#define YELLOW_BEGIN "\x1b[33m"
#define BLUE_BEGIN "\x1b[34m"
#define GREEN_BEGIN "\x1b[32m"
#endif

#define YELLOW(str) YELLOW_BEGIN str COLOR_END
#define RED(str) RED_BEGIN str COLOR_END
#define GREEN(str) GREEN_BEGIN str COLOR_END
#define MAGENTA(str) MAGENTA_BEGIN str COLOR_END
#define CYAN(str) CYAN_BEGIN str COLOR_END
#define BLUE(str) BLUE_BEGIN str COLOR_END

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

void
_log(bl_log_msg_type_e t, const char *file, int32_t line, const char *msg, ...);

void
print_trace(void);

#ifdef BL_DEBUG
#define BL_LOG(format, ...)                                                                        \
	{                                                                                          \
		_log(LOG_MSG, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                      \
	}

#define BL_WARNING(format, ...)                                                                    \
	{                                                                                          \
		_log(LOG_WARNING, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                  \
	}

#define BL_ASSERT(e)                                                                               \
	if (!(e)) {                                                                                \
		print_trace();                                                                     \
		assert(e);                                                                         \
	}

#else /* !BL_DEBUG */

#define BL_LOG(format, ...)                                                                        \
	while (0) {                                                                                \
	}

#define BL_WARNING(format, ...)                                                                    \
	while (0) {                                                                                \
	}

#define BL_ASSERT(e)                                                                               \
	while (0) {                                                                                \
	}

#endif /* BL_DEBUG */

#define BL_ABORT(format, ...)                                                                      \
	{                                                                                          \
		_log(LOG_ABORT, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                    \
		print_trace();                                                                     \
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

#define BL_ASSERT_MAGIC(O, M, MSG) assert((O)->_magic == &(M) && MSG)

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
		abort();                                                                           \
	}
#ifdef __cplusplus
}
#endif
#endif
