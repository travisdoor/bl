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

#include <bobject/containers/array.h>
#include <bobject/containers/string.h>
#include "assembly.h"
#include "error.h"
#include "arena.h"
#include "mir.h"

// clang-format off
#define BUILDER_RUN              0x00000002
#define BUILDER_PRINT_TOKENS     0x00000004
#define BUILDER_PRINT_AST        0x00000008
#define BUILDER_LOAD_FROM_FILE   0x00000010
#define BUILDER_SYNTAX_ONLY      0x00000020
#define BUILDER_EMIT_LLVM        0x00000040
#define BUILDER_RUN_TESTS        0x00000080
#define BUILDER_NO_BIN           0x00000100
#define BUILDER_NO_WARN          0x00000200
#define BUILDER_VERBOSE          0x00000400
#define BUILDER_VERBOSE_MIR_PRE  0x00000800
#define BUILDER_VERBOSE_MIR_POST 0x00001000
#define BUILDER_NO_API           0x00002000
// clang-format on

#define COMPILE_OK 0
#define COMPILE_FAIL 1

typedef void (*diag_handler_f)(const char *, void *);

typedef struct Builder
{
  MirArenas      mir_arenas;
  Arena          ast_arena;
  Arena          scope_arena;
  diag_handler_f on_error;
  diag_handler_f on_warning;
  diag_handler_f on_note;
  void *         on_error_cnt;
  void *         on_warning_cnt;
  void *         on_note_cnt;
  uint32_t       flags;
  int            total_lines;
  int            errorc;
  BArray *       str_cache;
} Builder;

typedef enum
{
  BUILDER_MSG_ERROR,
  BUILDER_MSG_WARNING,
  BUILDER_MSG_NOTE,
} BuilderMsgType;

typedef enum
{
  BUILDER_CUR_AFTER,
  BUILDER_CUR_WORD,
  BUILDER_CUR_BEFORE
} BuilderCurPos;

struct Src;

Builder *
builder_new(void);

void
builder_delete(Builder *builder);

int
builder_compile(Builder *builder, Assembly *assembly, uint32_t flags);

void
builder_set_error_diag_handler(Builder *builder, diag_handler_f handler, void *context);

void
builder_set_warning_diag_handler(Builder *builder, diag_handler_f handler, void *context);

void
builder_set_note_diag_handler(Builder *builder, diag_handler_f handler, void *context);

void
builder_error(Builder *builder, const char *format, ...);

void
builder_warning(Builder *builder, const char *format, ...);

void
builder_msg(Builder *builder, BuilderMsgType type, int code, struct Src *src, BuilderCurPos pos,
            const char *format, ...);

BString *
builder_create_cached_str(Builder *builder);

#endif
