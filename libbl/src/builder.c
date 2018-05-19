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

#include "common_impl.h"

#include "builder_impl.h"
#include "assembly_impl.h"
#include "unit_impl.h"

#include "stages_impl.h"

#define MAX_MSG_LEN 1024

static bl_error_e
compile_unit(bl_builder_t *builder, bl_unit_t *unit, bl_assembly_t *assembly, uint32_t flags);

static bl_error_e
compile_assembly(bl_builder_t *builder, bl_assembly_t *assembly, uint32_t flags);

static void
default_error_handler(const char *msg, void *context)
{
#if BL_ABORT_ON_CMP_ERROR
  bl_abort(false, "%s", msg);
#else
  bl_msg_error("%s", msg);
#endif
}

static void
default_warning_handler(const char *msg, void *context)
{
  bl_msg_warning("%s", msg);
}

bl_error_e
compile_unit(bl_builder_t *builder, bl_unit_t *unit, bl_assembly_t *assembly, uint32_t flags)
{
  bl_msg_log("processing unit: " BL_GREEN("%s"), unit->name);
  bl_error_e error;

  if (flags & BL_BUILDER_LOAD_FROM_FILE && (error = bl_file_loader_run(builder, unit)) != BL_NO_ERR)
    return error;

  if ((error = bl_lexer_run(builder, unit)) != BL_NO_ERR)
    return error;

  if (flags & BL_BUILDER_PRINT_TOKENS && (error = bl_token_printer_run(unit)) != BL_NO_ERR)
    return error;

  if ((error = bl_parser_run(builder, unit)) != BL_NO_ERR)
    return error;

  if ((error = bl_preproc_run(builder, unit, assembly)) != BL_NO_ERR)
    return error;

  return BL_NO_ERR;
}

bl_error_e
compile_assembly(bl_builder_t *builder, bl_assembly_t *assembly, uint32_t flags)
{
  bl_error_e error;

  /*
  if ((error = bl_linker_run(builder, assembly)) != BL_NO_ERR)
    return error;
  */

  /*
  if ((error = bl_merge_run(builder, assembly)) != BL_NO_ERR)
    return error;
  */

  if ((error = bl_connect_run(builder, assembly)) != BL_NO_ERR)
    return error;

  if (flags & BL_BUILDER_PRINT_AST && (error = bl_ast_printer_run(assembly)) != BL_NO_ERR)
    return error;

  if ((error = bl_check_run(builder, assembly)) != BL_NO_ERR)
    return error;

  if ((error = bl_evaluator_run(builder, assembly)) != BL_NO_ERR)
    return error;

  if (!(flags & BL_BUILDER_SYNTAX_ONLY)) {
    if ((error = bl_llvm_gen_run(builder, assembly)) != BL_NO_ERR)
      return error;

    if (flags & BL_BUILDER_RUN && (error = bl_llvm_jit_exec_run(builder, assembly)) != BL_NO_ERR)
      return error;

    if (flags & BL_BUILDER_EMIT_LLVM &&
        (error = bl_llvm_bc_writer_run(builder, assembly)) != BL_NO_ERR)
      return error;

    if (!(flags & BL_BUILDER_RUN)) {
      if ((error = bl_llvm_linker_run(builder, assembly)) != BL_NO_ERR)
        return error;

      if ((error = bl_llvm_native_bin_run(builder, assembly)) != BL_NO_ERR)
        return error;
    }
  }

  return BL_NO_ERR;
}

/* public */
bl_builder_t *
bl_builder_new(void)
{
  bl_builder_t *builder = bl_calloc(1, sizeof(bl_builder_t));

  builder->on_error   = default_error_handler;
  builder->on_warning = default_warning_handler;

  return builder;
}

void
bl_builder_delete(bl_builder_t *builder)
{
  bl_free(builder);
}

bl_error_e
bl_builder_compile(bl_builder_t *builder, bl_assembly_t *assembly, uint32_t flags)
{
  clock_t      begin = clock();
  bl_unit_t *  unit;
  bl_error_e   error;

  for (size_t i = 0; i < bo_array_size(assembly->units); ++i) {
    unit = bo_array_at(assembly->units, i, bl_unit_t *);

    /* IDEA: can run in separate thread */
    if ((error = compile_unit(builder, unit, assembly, flags)) != BL_NO_ERR) {
      return error;
    }
  }

  error = compile_assembly(builder, assembly, flags);

  clock_t end        = clock();
  double  time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

  bl_msg_log("compiled " BL_GREEN("%i") " lines in " BL_GREEN("%f") " seconds", builder->total_lines,
         time_spent);

  return error;
}

void
bl_builder_set_error_diag_handler(bl_builder_t *builder, bl_diag_handler_f handler, void *context)
{
  builder->on_error     = handler;
  builder->on_error_cnt = context;
}

void
bl_builder_set_warning_diag_handler(bl_builder_t *builder, bl_diag_handler_f handler, void *context)
{
  builder->on_warning     = handler;
  builder->on_warning_cnt = context;
}

void
bl_diag_delete_msg(char *msg)
{
  free(msg);
}

void
bl_builder_error(bl_builder_t *builder, const char *format, ...)
{
  char error[MAX_MSG_LEN] = {0};

  va_list args;
  va_start(args, format);
  vsnprintf(error, MAX_MSG_LEN, format, args);
  va_end(args);

  builder->on_error(&error[0], builder->on_error_cnt);
}

void
bl_builder_warning(bl_builder_t *builder, const char *format, ...)
{
  char warning[MAX_MSG_LEN] = {0};

  va_list args;
  va_start(args, format);
  vsnprintf(warning, MAX_MSG_LEN, format, args);
  va_end(args);

  builder->on_warning(&warning[0], builder->on_error_cnt);
}
