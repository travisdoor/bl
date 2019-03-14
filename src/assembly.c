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
#include "blmemory.h"
#include "assembly.h"
#include "unit.h"
#include "mir.h"

#define EXPECTED_UNIT_COUNT 512
#define EXPECTED_LINK_COUNT 32

/* public */

Assembly *
assembly_new(const char *name)
{
  Assembly *assembly = bl_calloc(1, sizeof(Assembly));
  if (!assembly) bl_abort("bad alloc");
  assembly->name         = strdup(name);
  assembly->units        = bo_array_new(sizeof(Unit *));
  assembly->unique_cache = bo_htbl_new(0, EXPECTED_UNIT_COUNT);
  assembly->link_cache   = bo_htbl_new(sizeof(char *), EXPECTED_LINK_COUNT);

  assembly->mir_module = mir_new_module(assembly->name);

  bo_array_reserve(assembly->units, EXPECTED_UNIT_COUNT);
  return assembly;
}

void
assembly_delete(Assembly *assembly)
{
  free(assembly->name);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    unit_delete(unit);
  }
  bo_unref(assembly->units);
  bo_unref(assembly->unique_cache);
  bo_unref(assembly->link_cache);

  mir_delete_module(assembly->mir_module);

  bl_free(assembly);
}

void
assembly_add_unit(Assembly *assembly, Unit *unit)
{
  bo_array_push_back(assembly->units, unit);
}

bool
assembly_add_unit_unique(Assembly *assembly, Unit *unit)
{
  uint64_t hash = 0;
  if (unit->filepath)
    hash = bo_hash_from_str(unit->filepath);
  else
    hash = bo_hash_from_str(unit->name);

  if (bo_htbl_has_key(assembly->unique_cache, hash)) return false;

  bo_htbl_insert_empty(assembly->unique_cache, hash);
  assembly_add_unit(assembly, unit);
  return true;
}

void
assembly_add_link(Assembly *assembly, const char *lib)
{
  if (!lib) return;
  uint64_t hash = bo_hash_from_str(lib);
  if (bo_htbl_has_key(assembly->link_cache, hash)) return;

  char *tmp = (char *)lib;
  bo_htbl_insert(assembly->link_cache, hash, tmp);
}
