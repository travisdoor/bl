//*****************************************************************************
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
//*****************************************************************************

#include <stdarg.h>
#include <time.h>

#include "bl/blmemory.h"
#include "bl/bldebug.h"

#include "builder_impl.h"
#include "assembly_impl.h"
#include "unit_impl.h"

#include "stages_impl.h"

#define MAX_MSG_LEN 1024

static bool
compile_unit(bl_builder_t *builder,
             bl_unit_t *unit,
             uint32_t flags);

static bool
compile_assembly(bl_builder_t *builder,
                 bl_assembly_t *assembly,
                 uint32_t flags);

static void
default_error_handler(const char *msg,
                      void *context)
{
  bl_log(BL_RED("error: ")
           "%s", msg);
}

static void
default_warning_handler(const char *msg,
                        void *context)
{
  bl_log(BL_YELLOW("warning: ")
           "%s", msg);
}

bool
compile_unit(bl_builder_t *builder,
             bl_unit_t *unit,
             uint32_t flags)
{
  bl_log("processing unit: " BL_GREEN("%s"), unit->name);

  if (flags & BL_BUILDER_LOAD_FROM_FILE && !bl_file_loader_run(builder, unit))
    return false;

  if (!bl_lexer_run(builder, unit))
    return false;

  if (flags & BL_BUILDER_PRINT_TOKENS && !bl_token_printer_run(unit))
    return false;

  if (!bl_parser_run(builder, unit))
    return false;

  if (flags & BL_BUILDER_PRINT_AST && !bl_ast_printer_run(unit))
    return false;

  if (!bl_llvm_backend_run(builder, unit))
    return false;

  if (flags & BL_BUILDER_EXPORT_BC && !bl_llvm_bc_writer_run(builder, unit))
    return false;

  return true;
}

bool
compile_assembly(bl_builder_t *builder,
                 bl_assembly_t *assembly,
                 uint32_t flags)
{
  if (!bl_llvm_linker_run(builder, assembly))
    return false;

  if (flags & BL_BUILDER_RUN && !bl_llvm_jit_exec_run(builder, assembly))
    return false;

  return true;
}

/* public */
bl_builder_t *
bl_builder_new(void)
{
  bl_builder_t *builder = bl_calloc(1, sizeof(bl_builder_t));

  builder->on_error = default_error_handler;
  builder->on_warning = default_warning_handler;

  return builder;
}

void
bl_builder_delete(bl_builder_t *builder)
{
  bl_free(builder);
}

bool
bl_builder_compile(bl_builder_t *builder,
                   bl_assembly_t *assembly,
                   uint32_t flags)
{
  clock_t begin = clock();
  const size_t c = bo_array_size(assembly->units);
  bl_unit_t *unit;
  for (size_t i = 0; i < c; i++) {
    unit = bo_array_at(assembly->units, i, bl_unit_t *);
    /* IDEA: can run in separate thread */
    if (!compile_unit(builder, unit, flags)) {
      return false;
    }
  }

  bool result = compile_assembly(builder, assembly, flags);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;

  bl_log("compiled in " BL_GREEN("%f") " seconds", time_spent);

  return result;
}

void
bl_builder_set_error_diag_handler(bl_builder_t *builder,
                                  bl_diag_handler_f handler,
                                  void *context)
{
  builder->on_error = handler;
  builder->on_error_cnt = context;
}

void
bl_builder_set_warning_diag_handler(bl_builder_t *builder,
                                    bl_diag_handler_f handler,
                                    void *context)
{
  builder->on_warning = handler;
  builder->on_warning_cnt = context;
}

void
bl_diag_delete_msg(char *msg)
{
  free(msg);
}

void
bl_builder_error(bl_builder_t *builder,
                 const char *format,
                 ...)
{
  char error[MAX_MSG_LEN] = {0};

  va_list args;
  va_start(args, format);
  vsnprintf(error, MAX_MSG_LEN, format, args);
  va_end(args);

  builder->on_error(&error[0], builder->on_error_cnt);
}

void
bl_builder_warning(bl_builder_t *builder,
                   const char *format,
                   ...)
{
  char warning[MAX_MSG_LEN] = {0};

  va_list args;
  va_start(args, format);
  vsnprintf(warning, MAX_MSG_LEN, format, args);
  va_end(args);

  builder->on_warning(&warning[0], builder->on_error_cnt);
}
