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
#include "types.h"
#include "arena.h"

typedef struct
{
  Arena                  ast_arena;
  Arena                  type_arena;
  BArray *               units;           /* array of all units in assembly */
  BHashTable *           unique_cache;    /* cache for loading only unique units */
  BHashTable *           link_cache;      /* all linked externals libraries passed to linker */
  char *                 name;            /* assembly name */
  ScopeCache *           scope_cache;     /* cache for scopes */
  Scope *                gscope;          /* cache for global scope */
  BList *                ir_queue;        /* generated into IR (entry functions 'main' etc.)*/
  BArray *               test_cases;      /* array of all test cases in assembly */
  bool                   has_main;        /* has main method implemented */
  LLVMContextRef         llvm_cnt;        /* llvm context */
  LLVMModuleRef          llvm_module;     /* final llvm module */
  LLVMExecutionEngineRef llvm_jit;        /* used in ir.c for compile-time execution */
  LLVMExecutionEngineRef llvm_run_engine; /* used when compiler is called with '-run' */
} Assembly;

Assembly *
assembly_new(const char *name);

void
assembly_delete(Assembly *assembly);

void
assembly_add_unit(Assembly *assembly, Unit *unit);

void
assembly_add_link(Assembly *assembly, const char *lib);

bool
assembly_add_unit_unique(Assembly *assembly, Unit *unit);

#endif
