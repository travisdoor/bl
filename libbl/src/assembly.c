//************************************************************************************************
// blc
//
// File:   assembly.c
// Author: Martin Dorazil
// Date:   09/02/2018
//
// Copyright 2017 Martin Dorazil
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

#include <string.h>
#include "blmemory_impl.h"
#include "assembly_impl.h"
#include "unit_impl.h"

/* public */

bl_assembly_t *
bl_assembly_new(const char *name)
{
  bl_assembly_t *assembly = bl_calloc(1, sizeof(bl_assembly_t));
  assembly->name          = strdup(name);
  assembly->units         = bo_array_new(sizeof(bl_unit_t *));
  assembly->scope_cache   = bl_scope_cache_new();
  assembly->scope         = bl_scope_new(assembly->scope_cache);

  return assembly;
}

void
bl_assembly_delete(bl_assembly_t *assembly)
{
  free(assembly->name);
  bl_scope_cache_delete(assembly->scope_cache);
  LLVMDisposeModule(assembly->llvm_module);
  LLVMContextDispose(assembly->llvm_cnt);

  const size_t c = bo_array_size(assembly->units);
  bl_unit_t *  unit;
  for (size_t i = 0; i < c; ++i) {
    unit = bo_array_at(assembly->units, i, bl_unit_t *);
    bl_unit_delete(unit);
  }
  bo_unref(assembly->units);

  bl_free(assembly);
}

void
bl_assembly_add_unit(bl_assembly_t *assembly, bl_unit_t *unit)
{
  /* TODO: handle duplicity */
  bo_array_push_back(assembly->units, unit);
}

const char *
bl_assembly_get_name(bl_assembly_t *assembly)
{
  return assembly->name;
}

int
bl_assembly_get_unit_count(bl_assembly_t *assembly)
{
  return (int)bo_array_size(assembly->units);
}

bl_unit_t *
bl_assembly_get_unit(bl_assembly_t *assembly, int i)
{
  return bo_array_at(assembly->units, (size_t)i, bl_unit_t *);
}
