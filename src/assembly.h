//************************************************************************************************
// Biscuit Engine
//
// File:   assembly.h
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

#ifndef BL_ASSEMBLY_BL
#define BL_ASSEMBLY_BL

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/list.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Core.h>
#include "scope.h"
#include "unit.h"

typedef struct bl_assembly
{
  BArray *               units;           /* array of all units in assembly */
  BHashTable *           unique_cache;    /* cache for loading only unique units */
  BHashTable *           link_cache;      /* all linked externals libraries passed to linker */
  char *                 name;            /* assembly name */
  scope_cache_t *        scope_cache;     /* cache for scopes */
  scope_t *              gscope;          /* cache for global scope */
  BList *                ir_queue;        /* generated into IR (entry functions 'main' etc.)*/
  LLVMContextRef         llvm_cnt;        /* llvm context */
  LLVMModuleRef          llvm_module;     /* final llvm module */
  LLVMExecutionEngineRef llvm_jit;        /* used in ir.c for compile-time execution */
  LLVMExecutionEngineRef llvm_run_engine; /* used when compiler is called with '-run' */
} assembly_t;

typedef struct bl_assembly *bl_assembly_ref;

bl_assembly_ref
bl_assembly_new(const char *name);

void
bl_assembly_delete(bl_assembly_ref assembly);

void
bl_assembly_add_unit(bl_assembly_ref assembly, bl_unit_ref unit);

void
bl_assembly_add_link(bl_assembly_ref assembly, const char *lib);

bool
bl_assembly_add_unit_unique(bl_assembly_ref assembly, bl_unit_ref unit);

int
bl_assembly_get_unit_count(bl_assembly_ref assembly);

bl_unit_ref
bl_assembly_get_unit(bl_assembly_ref assembly, int i);

const char *
bl_assembly_get_name(bl_assembly_ref assembly);

#endif
