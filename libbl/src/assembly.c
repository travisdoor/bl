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

bl_assembly_t *
bl_assembly_new(const char *name)
{
  bl_assembly_t *assembly = bl_calloc(1, sizeof(bl_assembly_t));
  assembly->name          = strdup(name);
  assembly->units         = bo_array_new(sizeof(bl_unit_t *));
  assembly->unique_cache  = bo_htbl_new(0, EXPECTED_UNIT_COUNT);
  assembly->ir_queue      = bo_list_new(sizeof(bl_node_t *));
  assembly->link_cache    = bo_htbl_new(sizeof(char *), EXPECTED_LINK_COUNT);

  bl_scope_cache_init(&assembly->scope_cache);
  assembly->gscope = bl_scope_new(assembly->scope_cache, EXPECTED_GSCOPE_SIZE);

  bo_array_reserve(assembly->units, EXPECTED_UNIT_COUNT);

  return assembly;
}

void
bl_assembly_delete(bl_assembly_t *assembly)
{
  free(assembly->name);

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    bl_unit_delete(unit);
  }
  bo_unref(assembly->units);
  bo_unref(assembly->unique_cache);
  bo_unref(assembly->ir_queue);
  bo_unref(assembly->link_cache);

  bl_scope_cache_terminate(assembly->scope_cache);

  /* LLVM cleanup */
  /* execution engine owns llvm_module after creation */
  if (assembly->llvm_runtime_engine)
    LLVMDisposeExecutionEngine(assembly->llvm_runtime_engine);
  else
    LLVMDisposeModule(assembly->llvm_module);

  LLVMContextDispose(assembly->llvm_cnt);

  bl_free(assembly);
}

void
bl_assembly_add_unit(bl_assembly_t *assembly, bl_unit_t *unit)
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

  bo_htbl_insert(assembly->link_cache, hash, (char *) lib);
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

void
bl_assembly_add_into_ir(bl_assembly_t *assembly, bl_node_t *node)
{
  assert(node);
  bo_list_push_back(assembly->ir_queue, node);
}
