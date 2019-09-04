//************************************************************************************************
// bl
//
// File:   builder.h
// Author: Martin Dorazil
// Date:   02/03/2018
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

#ifndef BL_BUILDER_H
#define BL_BUILDER_H

#include "assembly.h"
#include "conf_data.h"
#include "error.h"
#include "mir.h"
#include <bobject/containers/array.h>
#include <bobject/containers/string.h>

#define COMPILE_OK 0
#define COMPILE_FAIL 1

typedef enum {
	BUILDER_FLAG_RUN             = 1 << 1,
	BUILDER_FLAG_PRINT_TOKENS    = 1 << 2,
	BUILDER_FLAG_PRINT_AST       = 1 << 3,
	BUILDER_FLAG_LOAD_FROM_FILE  = 1 << 4,
	BUILDER_FLAG_SYNTAX_ONLY     = 1 << 5,
	BUILDER_FLAG_EMIT_LLVM       = 1 << 6,
	BUILDER_FLAG_RUN_TESTS       = 1 << 7,
	BUILDER_FLAG_NO_BIN          = 1 << 8,
	BUILDER_FLAG_NO_WARN         = 1 << 9,
	BUILDER_FLAG_VERBOSE         = 1 << 10,
	BUILDER_FLAG_NO_API          = 1 << 11,
	BUILDER_FLAG_EMIT_MIR        = 1 << 12,
	BUILDER_FLAG_FORCE_TEST_LLVM = 1 << 13,
	BUILDER_FLAG_DEBUG_BUILD     = 1 << 14,
	BUILDER_FLAG_NO_LLVM         = 1 << 15,
} BuilderFlags;

typedef struct Builder {
	uint32_t  flags;
	int32_t   total_lines;
	int32_t   errorc;
	BArray *  str_cache;
	ConfData *conf;
} Builder;

typedef enum {
	BUILDER_MSG_ERROR,
	BUILDER_MSG_WARNING,
	BUILDER_MSG_NOTE,
	BUILDER_MSG_LOG,
} BuilderMsgType;

typedef enum { BUILDER_CUR_AFTER, BUILDER_CUR_WORD, BUILDER_CUR_BEFORE, BUILDER_CUR_NONE } BuilderCurPos;

struct Location;

Builder *
builder_new(void);

void
builder_delete(Builder *builder);

int
builder_load_conf_file(Builder *builder, const char *filepath);

int
builder_compile(Builder *builder, Assembly *assembly, uint32_t flags, OptLvl opt_lvl);

void
builder_error(Builder *builder, const char *format, ...);

void
builder_warning(Builder *builder, const char *format, ...);

void
builder_msg(Builder *        builder,
            BuilderMsgType   type,
            int32_t          code,
            struct Location *src,
            BuilderCurPos    pos,
            const char *     format,
            ...);

BString *
builder_create_cached_str(Builder *builder);

#endif
