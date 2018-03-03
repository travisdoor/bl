//*****************************************************************************
// bl
//
// File:   unit.c
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
//*****************************************************************************

#include <string.h>
#include "unit_impl.h"
#include "blmemory_impl.h"
#include "bl/bldebug.h"

static void
init(bl_unit_t *unit)
{
  bl_sym_tbl_init(&unit->sym_tbl);
  bl_tokens_init(&unit->tokens);
  bl_ast_init(&unit->ast);
}

/* public */
bl_unit_t *
bl_unit_new_file(const char *filepath)
{
  bl_unit_t *unit = bl_calloc(1, sizeof(bl_unit_t));
  unit->filepath = strdup(filepath);
  unit->name     = strrchr(unit->filepath, '/');
  if (unit->name == NULL)
    unit->name   = unit->filepath;
  else
    unit->name++;

  init(unit);
  return unit;
}

bl_unit_t *
bl_unit_new_str(const char *name,
                const char *src)
{
  bl_unit_t *unit = bl_calloc(1, sizeof(bl_unit_t));
  unit->filepath = strdup(name);
  unit->name     = strdup(name);

  if (src)
    unit->src = strdup(src);
  else bl_abort("invalid source for %s unit", unit->name);

  init(unit);
  return unit;
}

void
bl_unit_delete(bl_unit_t *unit)
{
  free(unit->filepath);
  free(unit->src);
  bl_tokens_terminate(&unit->tokens);
  bl_ast_terminate(&unit->ast);
  bl_sym_tbl_terminate(&unit->sym_tbl);

  LLVMDisposeModule(unit->llvm_module);
  bl_free(unit);
}

const char *
bl_unit_get_src_file(bl_unit_t *unit)
{
  return unit->filepath;
}

const char *
bl_unit_get_src(bl_unit_t *unit)
{
  return unit->src;
}

const char *
bl_unit_get_name(bl_unit_t *unit)
{
  return unit->name;
}

