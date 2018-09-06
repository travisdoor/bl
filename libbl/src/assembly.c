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
#include <bobject/containers/hash.h>
#include "blmemory_impl.h"
#include "assembly_impl.h"
#include "unit_impl.h"

#define EXPECTED_UNIT_COUNT 512
#define EXPECTED_LINK_COUNT 32
#define EXPECTED_GSCOPE_SIZE 4096

/* public */

assembly_t *
bl_assembly_new(const char *name)
{
  assembly_t *assembly = bl_calloc(1, sizeof(assembly_t));
  if (!assembly) bl_abort("bad alloc");
  assembly->name         = strdup(name);
  assembly->units        = bo_array_new(sizeof(unit_t *));
  assembly->unique_cache = bo_htbl_new(0, EXPECTED_UNIT_COUNT);
  assembly->ir_queue     = bo_list_new(sizeof(node_t *));
  assembly->link_cache   = bo_htbl_new(sizeof(char *), EXPECTED_LINK_COUNT);

  scope_cache_init(&assembly->scope_cache);
  assembly->gscope = scope_new(assembly->scope_cache, EXPECTED_GSCOPE_SIZE);

  bo_array_reserve(assembly->units, EXPECTED_UNIT_COUNT);

  return assembly;
}

void
bl_assembly_delete(assembly_t *assembly)
{
  free(assembly->name);

  unit_t *unit;
  barray_foreach(assembly->units, unit)
  {
    bl_unit_delete(unit);
  }
  bo_unref(assembly->units);
  bo_unref(assembly->unique_cache);
  bo_unref(assembly->ir_queue);
  bo_unref(assembly->link_cache);

  scope_cache_terminate(assembly->scope_cache);

  /* LLVM cleanup */
  /* execution engine owns llvm_module after creation */
  LLVMDisposeExecutionEngine(assembly->llvm_jit);
  if (assembly->llvm_run_engine)
    LLVMDisposeExecutionEngine(assembly->llvm_run_engine);
  else
    LLVMDisposeModule(assembly->llvm_module);

  LLVMContextDispose(assembly->llvm_cnt);

  bl_free(assembly);
}

void
bl_assembly_add_unit(assembly_t *assembly, unit_t *unit)
{
  bo_array_push_back(assembly->units, unit);
}

bool
bl_assembly_add_unit_unique(bl_assembly_ref assembly, bl_unit_ref unit)
{
  uint64_t hash = 0;
  if (unit->filepath)
    hash = bo_hash_from_str(unit->filepath);
  else
    hash = bo_hash_from_str(unit->name);

  if (bo_htbl_has_key(assembly->unique_cache, hash)) return false;

  bo_htbl_insert_empty(assembly->unique_cache, hash);
  bl_assembly_add_unit(assembly, unit);
  return true;
}

void
bl_assembly_add_link(bl_assembly_ref assembly, const char *lib)
{
  if (!lib) return;
  uint64_t hash = bo_hash_from_str(lib);
  if (bo_htbl_has_key(assembly->link_cache, hash)) return;

  bo_htbl_insert(assembly->link_cache, hash, lib);
}

const char *
bl_assembly_get_name(assembly_t *assembly)
{
  return assembly->name;
}

int
bl_assembly_get_unit_count(assembly_t *assembly)
{
  return (int)bo_array_size(assembly->units);
}

unit_t *
bl_assembly_get_unit(assembly_t *assembly, int i)
{
  return bo_array_at(assembly->units, (size_t)i, unit_t *);
}
