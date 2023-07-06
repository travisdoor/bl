// =================================================================================================
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
// =================================================================================================

#ifndef BL_DEBUG_H
#define BL_DEBUG_H

#include "basic_types.h"
#include "config.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#if BL_PLATFORM_LINUX
#	include <signal.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if BL_COMPILER_GNUC || BL_COMPILER_CLANG
#	ifndef __FILENAME__
#		define __FILENAME__ (strrchr(__FILE__, '/') ? strrchr(__FILE__, '/') + 1 : __FILE__)
#	endif
#elif BL_COMPILER_MSVC
#	ifndef __FILENAME__
#		define __FILENAME__ (strrchr(__FILE__, '\\') ? strrchr(__FILE__, '\\') + 1 : __FILE__)
#	endif
#else
#	pragma message("WARNING: Cannot parse filename with this compiler")
#	define __FILENAME__
#endif

typedef enum { LOG_ASSERT, LOG_ABORT, LOG_ABORT_ISSUE, LOG_WARNING, LOG_MSG } log_msg_kind_t;

void log_impl(log_msg_kind_t t, const char *file, s32 line, const char *msg, ...);
void print_trace_impl(void);

#if defined(BL_DEBUG)
#	define print_trace() print_trace_impl()
#else
#	define print_trace() (void)0
#endif

#if defined(BL_DEBUG) || BL_ASSERT_ENABLE
// =================================================================================================
static inline void bl_debug_break(void)
{
#	if BL_PLATFORM_WIN
	__debugbreak();
#	elif BL_PLATFORM_MACOS
	__builtin_debugtrap();
#	else
	raise(SIGTRAP);
#	endif
}

#	define BL_DEBUG_BREAK bl_debug_break()

#	define bassert(e)                                                                             \
		if (!(e)) {                                                                                \
			log_impl(LOG_ASSERT, __FILENAME__, __LINE__, #e);                                      \
			print_trace();                                                                         \
			BL_DEBUG_BREAK;                                                                        \
			abort();                                                                               \
		}                                                                                          \
		(void)0

#	define bmagic_assert(O)                                                                       \
		{                                                                                          \
			bassert(O && "Invalid reference!");                                                    \
			bassert((O)->_magic == (void *)&(O)->_magic && "Invalid magic!");                      \
		}                                                                                          \
		(void)0
#	define bmagic_member void *_magic;
#	define bmagic_set(O) (O)->_magic = (void *)&(O)->_magic

#	define bcalled_once_member(name) s32 _##name##_call_count;
#	define bcalled_once_assert(obj, name)                                                         \
		bassert((obj)->_##name##_call_count++ == 0 && "Expected to be called only once!")

// =================================================================================================
#else
// =================================================================================================
#	define BL_DEBUG_BREAK                                                                         \
		while (0) {                                                                                \
		}

#	define bassert(e)                                                                             \
		while (0) {                                                                                \
		}                                                                                          \
		(void)0

#	define bmagic_assert(O)                                                                       \
		while (0) {                                                                                \
		}                                                                                          \
		(void)0

#	define bmagic_member
#	define bmagic_set(O)                                                                          \
		while (0) {                                                                                \
		}                                                                                          \
		(void)0

#	define bcalled_once_member(name)
#	define bcalled_once_assert(obj, name) (void)0

// =================================================================================================
#endif

#if defined(BL_DEBUG)
// =================================================================================================
#	define blog(format, ...)                                                                      \
		{                                                                                          \
			log_impl(LOG_MSG, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                      \
		}                                                                                          \
		(void)0

#	define bwarn(format, ...)                                                                     \
		{                                                                                          \
			log_impl(LOG_WARNING, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                  \
		}                                                                                          \
		(void)0
// =================================================================================================
#else
// =================================================================================================
#	define blog(format, ...)                                                                      \
		while (0) {                                                                                \
		}                                                                                          \
		(void)0

#	define bwarn(format, ...)                                                                     \
		while (0) {                                                                                \
		}                                                                                          \
		(void)0
// =================================================================================================
#endif

#define babort(format, ...)                                                                        \
	{                                                                                              \
		log_impl(LOG_ABORT, __FILENAME__, __LINE__, format, ##__VA_ARGS__);                        \
		print_trace();                                                                             \
		BL_DEBUG_BREAK;                                                                            \
		abort();                                                                                   \
	}                                                                                              \
	(void)0

#define babort_issue(N)                                                                            \
	{                                                                                              \
		log_impl(LOG_ABORT_ISSUE,                                                                  \
		         __FILENAME__,                                                                     \
		         __LINE__,                                                                         \
		         "Issue: https://github.com/travisdoor/bl/issues/%d",                              \
		         N);                                                                               \
		print_trace();                                                                             \
		BL_DEBUG_BREAK;                                                                            \
		abort();                                                                                   \
	}                                                                                              \
	(void)0

#define BL_UNIMPLEMENTED                                                                           \
	{                                                                                              \
		log_impl(LOG_ABORT, __FILENAME__, __LINE__, "unimplemented");                              \
		print_trace();                                                                             \
		BL_DEBUG_BREAK;                                                                            \
		abort();                                                                                   \
	}                                                                                              \
	(void)0

#define BL_UNREACHABLE                                                                             \
	{                                                                                              \
		log_impl(LOG_ABORT, __FILENAME__, __LINE__, "unreachable");                                \
		print_trace();                                                                             \
		BL_DEBUG_BREAK;                                                                            \
		abort();                                                                                   \
	}                                                                                              \
	(void)0

#ifdef TRACY_ENABLE
#	define BL_TRACY_MESSAGE(tag, format, ...)                                                     \
		{                                                                                          \
			char buf[256];                                                                         \
			snprintf(buf, static_arrlenu(buf), "#%s " format, tag, ##__VA_ARGS__);                 \
			TracyCMessageC(buf, strlen(buf), strhash(make_str_from_c(tag)));                      \
		}                                                                                          \
		(void)0
#else
#	define BL_TRACY_MESSAGE(tag, format, ...)                                                     \
		while (0) {                                                                                \
		}                                                                                          \
		(void)0
#endif

#define zone()                                                                                     \
	TracyCZone(_tctx, true);                                                                       \
	(void)0

#define _BL_VARGS(...) __VA_ARGS__
#define return_zone(...)                                                                           \
	{                                                                                              \
		TracyCZoneEnd(_tctx) return _BL_VARGS(__VA_ARGS__);                                        \
	}                                                                                              \
	(void)0

#ifdef __cplusplus
}
#endif
#endif
