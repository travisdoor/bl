//************************************************************************************************
// blc
//
// File:   builder.c
// Author: Martin Dorazil
// Date:   14.2.18
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

#include <stdarg.h>
#include <time.h>
#include <bobject/containers/string.h>

#include "common.h"
#include "builder.h"
#include "assembly.h"
#include "unit.h"
#include "stages.h"
#include "token.h"

#define MAX_MSG_LEN 1024
#define MAX_ERROR_REPORTED 10

static const char *reserved_names[RESERVED_COUNT] = {"u8", "u16", "u32", "u64", "usize",
                                                     "s8", "s16", "s32", "s64", "main"};

static Ast entry_u8 = {.kind                   = AST_TYPE,
                       .src                    = NULL,
                       .next                   = NULL,
                       .type.kind              = AST_TYPE_INT,
                       .type.integer.name      = "u8",
                       .type.integer.bitcount  = 8,
                       .type.integer.is_signed = false};

static Ast entry_u16 = {.kind                   = AST_TYPE,
                        .src                    = NULL,
                        .next                   = NULL,
                        .type.kind              = AST_TYPE_INT,
                        .type.integer.name      = "u16",
                        .type.integer.bitcount  = 16,
                        .type.integer.is_signed = false};

static Ast entry_u32 = {.kind                   = AST_TYPE,
                        .src                    = NULL,
                        .next                   = NULL,
                        .type.kind              = AST_TYPE_INT,
                        .type.integer.name      = "u32",
                        .type.integer.bitcount  = 32,
                        .type.integer.is_signed = false};

static Ast entry_u64 = {.kind                   = AST_TYPE,
                        .src                    = NULL,
                        .next                   = NULL,
                        .type.kind              = AST_TYPE_INT,
                        .type.integer.name      = "u64",
                        .type.integer.bitcount  = 64,
                        .type.integer.is_signed = false};

static Ast entry_usize = {.kind                   = AST_TYPE,
                          .src                    = NULL,
                          .next                   = NULL,
                          .type.kind              = AST_TYPE_INT,
                          .type.integer.name      = "u64",
                          .type.integer.bitcount  = 64,
                          .type.integer.is_signed = false};

static Ast entry_s8 = {.kind                   = AST_TYPE,
                       .src                    = NULL,
                       .next                   = NULL,
                       .type.kind              = AST_TYPE_INT,
                       .type.integer.name      = "s8",
                       .type.integer.bitcount  = 8,
                       .type.integer.is_signed = true};

static Ast entry_s16 = {.kind                   = AST_TYPE,
                        .src                    = NULL,
                        .next                   = NULL,
                        .type.kind              = AST_TYPE_INT,
                        .type.integer.name      = "s16",
                        .type.integer.bitcount  = 16,
                        .type.integer.is_signed = true};

static Ast entry_s32 = {.kind                   = AST_TYPE,
                        .src                    = NULL,
                        .next                   = NULL,
                        .type.kind              = AST_TYPE_INT,
                        .type.integer.name      = "s32",
                        .type.integer.bitcount  = 32,
                        .type.integer.is_signed = true};

static Ast entry_s64 = {.kind                   = AST_TYPE,
                        .src                    = NULL,
                        .next                   = NULL,
                        .type.kind              = AST_TYPE_INT,
                        .type.integer.name      = "s64",
                        .type.integer.bitcount  = 64,
                        .type.integer.is_signed = true};

static int
compile_unit(Builder *builder, Unit *unit, Assembly *assembly, uint32_t flags);

static int
compile_assembly(Builder *builder, Assembly *assembly, uint32_t flags);

static void
default_error_handler(const char *msg, void *context)
{
#if BL_ABORT_ON_CMP_ERROR
  bl_abort(false, "%s", msg);
#else
  msg_error("%s", msg);
#endif
}

static void
default_warning_handler(const char *msg, void *context)
{
  msg_warning("%s", msg);
}

static void
default_note_handler(const char *msg, void *context)
{
  msg_note("%s", msg);
}

static bool llvm_initialized = false;

static void
llvm_init(void)
{
  if (llvm_initialized) return;

  LLVMInitializeAllTargetInfos();
  LLVMInitializeAllTargets();
  LLVMInitializeAllTargetMCs();
  LLVMInitializeAllAsmParsers();
  LLVMInitializeAllAsmPrinters();
  LLVMLinkInMCJIT();
  llvm_initialized = true;
}

#define interrupt_on_error(_builder)                                                               \
  if ((_builder)->errorc) return COMPILE_FAIL;

int
compile_unit(Builder *builder, Unit *unit, Assembly *assembly, uint32_t flags)
{
  if (flags & BUILDER_VERBOSE) {
    if (unit->loaded_from) {
      msg_log("compile: %s (loaded from '%s')", unit->name, unit->loaded_from->src.unit->name);
    } else {
      msg_log("compile: %s", unit->name);
    }
  }

  if (flags & BUILDER_LOAD_FROM_FILE) {
    file_loader_run(builder, unit);
    interrupt_on_error(builder);
  }

  lexer_run(builder, unit);
  interrupt_on_error(builder);

  if (flags & BUILDER_PRINT_TOKENS) {
    token_printer_run(unit);
    interrupt_on_error(builder);
  }

  parser_run(builder, assembly, unit);

  return COMPILE_OK;
}

int
compile_assembly(Builder *builder, Assembly *assembly, uint32_t flags)
{
  if (!builder->errorc) checker_run(builder, assembly);
  // if (!builder->errorc) post_run(builder, assembly);

  if (flags & BUILDER_PRINT_AST) {
    ast_printer_run(assembly);
  }

  interrupt_on_error(builder);

  if (!(flags & BUILDER_SYNTAX_ONLY)) {
    ir_run(builder, assembly);

    if (flags & BUILDER_EMIT_LLVM) {
      bc_writer_run(builder, assembly);
      interrupt_on_error(builder);
    }

    if (flags & BUILDER_RUN) jit_exec_run(builder, assembly);
    if (flags & BUILDER_RUN_TESTS) test_exec_run(builder, assembly);

    if (!(flags & BUILDER_NO_BIN)) {
      linker_run(builder, assembly);
      interrupt_on_error(builder);
      native_bin_run(builder, assembly);
      interrupt_on_error(builder);
    }
  }

  return COMPILE_OK;
}

/* public */
Builder *
builder_new(void)
{
  Builder *builder = bl_calloc(1, sizeof(Builder));
  if (!builder) bl_abort("bad alloc");

  builder->on_error    = default_error_handler;
  builder->on_warning  = default_warning_handler;
  builder->on_note     = default_note_handler;
  builder->flags       = 0;
  builder->errorc      = 0;
  builder->uname_cache = bo_array_new_bo(bo_typeof(BString), true);

  /* initialize LLVM statics */
  llvm_init();

  /* initialize reserved names hashes */
  builder->reserved = bo_htbl_new(sizeof(ReservedNames), RESERVED_COUNT);
  for (int i = 0; i < RESERVED_COUNT; ++i) {
    bo_htbl_insert(builder->reserved, bo_hash_from_str(reserved_names[i]), i);
  }

  /* SETUP BUILDINS */
  struct Buildin *b = &builder->buildin;
  b->entry_u8       = &entry_u8;
  b->entry_u16      = &entry_u16;
  b->entry_u32      = &entry_u32;
  b->entry_u64      = &entry_u64;
  b->entry_usize    = &entry_usize;
  b->entry_s8       = &entry_s8;
  b->entry_s16      = &entry_s16;
  b->entry_s32      = &entry_s32;
  b->entry_s64      = &entry_s64;
  /* SETUP BUILDINS */

  return builder;
}

void
builder_delete(Builder *builder)
{
  bo_unref(builder->uname_cache);
  bo_unref(builder->reserved);
  bl_free(builder);
}

int
builder_compile(Builder *builder, Assembly *assembly, uint32_t flags)
{
  clock_t begin = clock();
  Unit *  unit;
  int     state = COMPILE_OK;

  builder->flags = flags;
  msg_log("compile assembly: %s", assembly->name);

  barray_foreach(assembly->units, unit)
  {
    /* IDEA: can run in separate thread */
    if ((state = compile_unit(builder, unit, assembly, flags)) != COMPILE_OK) {
      break;
    }
  }

  if (state == COMPILE_OK) state = compile_assembly(builder, assembly, flags);

  clock_t end        = clock();
  double  time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

  msg_log("compiled %i lines in %f seconds", builder->total_lines, time_spent);
  if (state != COMPILE_OK) {
    msg_log("there were errors, sorry...");
  }

  return state;
}

void
builder_set_error_diag_handler(Builder *builder, diag_handler_f handler, void *context)
{
  builder->on_error     = handler;
  builder->on_error_cnt = context;
}

void
builder_set_warning_diag_handler(Builder *builder, diag_handler_f handler, void *context)
{
  builder->on_warning     = handler;
  builder->on_warning_cnt = context;
}

void
builder_set_note_diag_handler(Builder *builder, diag_handler_f handler, void *context)
{
  builder->on_note     = handler;
  builder->on_note_cnt = context;
}

void
bl_diag_delete_msg(char *msg)
{
  free(msg);
}

void
builder_error(Builder *builder, const char *format, ...)
{
  if (builder->errorc > MAX_ERROR_REPORTED) return;
  char error[MAX_MSG_LEN] = {0};

  va_list args;
  va_start(args, format);
  vsnprintf(error, MAX_MSG_LEN, format, args);
  va_end(args);

  builder->on_error(&error[0], builder->on_error_cnt);
  builder->errorc++;
}

void
builder_warning(Builder *builder, const char *format, ...)
{
  char warning[MAX_MSG_LEN] = {0};

  va_list args;
  va_start(args, format);
  vsnprintf(warning, MAX_MSG_LEN, format, args);
  va_end(args);

  builder->on_warning(&warning[0], builder->on_error_cnt);
}

void
builder_msg(Builder *builder, BuilderMsgType type, int code, Src *src, BuilderCurPos pos,
            const char *format, ...)
{
  if (type == BUILDER_MSG_ERROR && builder->errorc > MAX_ERROR_REPORTED) return;
  if ((builder->flags & BUILDER_NO_WARN) && type == BUILDER_MSG_WARNING) return;

  BString *tmp              = bo_string_new(MAX_MSG_LEN);
  char     msg[MAX_MSG_LEN] = {0};

  if (src) {
    int line = src->line;
    int col  = src->col;
    int len  = src->len;

    switch (pos) {
    case BUILDER_CUR_AFTER:
      col += len;
      len = 1;
      break;

    case BUILDER_CUR_WORD:
      break;

    case BUILDER_CUR_BEFORE:
      col -= col < 1 ? 0 : 1;
      len = 1;
      break;
    }

    if (code == 0) {
      snprintf(msg, MAX_MSG_LEN, "%s:%d:%d ", src->unit->filepath, line, col);
    } else {
      const char *mark = "E";
      if (type == BUILDER_MSG_NOTE) mark = "N";
      if (type == BUILDER_MSG_WARNING) mark = "W";

      snprintf(msg, MAX_MSG_LEN, "[%s%04d] %s:%d:%d ", mark, code, src->unit->filepath, line, col);
    }

    va_list args;
    va_start(args, format);
    vsnprintf(msg + strlen(msg), MAX_MSG_LEN - strlen(msg), format, args);
    va_end(args);
    bo_string_append(tmp, &msg[0]);

    int         pad      = sprintf(msg, "%d", src->line) + 2;
    long        line_len = 0;
    const char *line_str = unit_get_src_ln(src->unit, src->line - 1, &line_len);
    if (line_str && line_len) {
      sprintf(msg, "\n%*d", pad, src->line - 1);
      bo_string_append(tmp, &msg[0]);
      bo_string_append(tmp, " | ");
      bo_string_appendn(tmp, line_str, line_len);
    }

    line_str = unit_get_src_ln(src->unit, src->line, &line_len);
    if (line_str && line_len) {
      sprintf(msg, CYAN("\n%*d"), pad, src->line);
      bo_string_append(tmp, &msg[0]);
      bo_string_append(tmp, " | ");
      bo_string_appendn(tmp, line_str, line_len);
      sprintf(msg, "\n%*s", pad, "");
      bo_string_append(tmp, &msg[0]);
      bo_string_append(tmp, " | ");
    }

    for (int i = 0; i < col + len - 1; ++i) {
      if (i < col - 1)
        bo_string_append(tmp, " ");
      else {
        const char *marker = RED("^");
        if (type == BUILDER_MSG_NOTE) marker = BLUE("^");
        if (type == BUILDER_MSG_WARNING) marker = YELLOW("^");
        bo_string_append(tmp, marker);
      }
    }

    line_str = unit_get_src_ln(src->unit, src->line + 1, &line_len);
    if (line_str && line_len) {
      sprintf(msg, "\n%*d", pad, src->line + 1);
      bo_string_append(tmp, &msg[0]);
      bo_string_append(tmp, " | ");
      bo_string_appendn(tmp, line_str, line_len);
    }
  } else {
    va_list args;
    va_start(args, format);
    vsnprintf(msg + strlen(msg), MAX_MSG_LEN - strlen(msg), format, args);
    va_end(args);
    bo_string_append(tmp, &msg[0]);
  }

  if (type == BUILDER_MSG_ERROR) {
    builder->errorc++;
    builder->on_error(bo_string_get(tmp), builder->on_error_cnt);
  } else if (type == BUILDER_MSG_WARNING) {
    builder->on_warning(bo_string_get(tmp), builder->on_warning_cnt);
  } else {
    builder->on_note(bo_string_get(tmp), builder->on_note_cnt);
  }

  bo_unref(tmp);

#if ASSERT_ON_CMP_ERROR
  if (type == BUILDER_MSG_ERROR) assert(false);
#endif
}

uint64_t
builder_get_unique_id(Builder *builder)
{
  static uint64_t i = 0;
  return i++;
}

const char *
builder_get_unique_name(Builder *builder, const char *base)
{
  BString *s = bo_string_new(64);
  bo_array_push_back(builder->uname_cache, s);

  bo_string_append(s, base);
  uint64_t ui = builder_get_unique_id(builder);
  char     ui_str[21];
  sprintf(ui_str, "%llu", (unsigned long long)ui);
  bo_string_append(s, ui_str);
  return bo_string_get(s);
}

int
builder_is_reserved(Builder *builder, uint64_t hash)
{
  if (!bo_htbl_has_key(builder->reserved, hash)) return -1;
  return bo_htbl_at(builder->reserved, hash, ReservedNames);
}
