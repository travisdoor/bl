//************************************************************************************************
// blc
//
// File:   test_exec.c
// Author: Martin Dorazil
// Date:   14/02/2018
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

#include <setjmp.h>
#include "stages.h"
#include "bldebug.h"

#if 0
typedef struct
{
  Builder *              builder;
  Assembly *             assembly;
  LLVMExecutionEngineRef llvm_jit;
} Context;

static jmp_buf assert_jmp;

static void
test_case(Context *cnt, TestCase *tc);

/* impl */
static void
test_case(Context *cnt, TestCase *tc)
{
  bool result = true;
  assert(tc->fn);
  assert(tc->name);
  assert(cnt->llvm_jit);

  AstDecl *    _decl   = tc->fn;
  const char * fn_name = _decl->name->str;
  LLVMValueRef llvm_fn = NULL;

  if (LLVMFindFunction(cnt->llvm_jit, fn_name, &llvm_fn)) {
    bl_abort("test case function '%s' has not been linked into compile time execution engine!",
             fn_name);
  }

  /* asserting in this way is not good idea, LLVMRunFunction never returns result value so this
   * value cannot be freed later -> cause memory leak */
  if (!(result = (bool)setjmp(assert_jmp))) {
    LLVMGenericValueRef ret = LLVMRunFunction(cnt->llvm_jit, llvm_fn, 0, NULL);
    LLVMDisposeGenericValue(ret);
  }

  const char *unit_name = ((Ast *)tc->fn)->src->unit->name;
  const int   line      = ((Ast *)tc->fn)->src->line;
  msg_log("[%s] %s:%d %s", result ? RED(" FAIL ") : GREEN("  OK  "), unit_name, line, tc->name);
}

/* public */
void
test_exec_run(Builder *builder, Assembly *assembly)
{
  msg_log("\nrunning test cases:");
  assert(assembly->llvm_jit);

  Context cnt = {.builder = builder, .assembly = assembly, .llvm_jit = assembly->llvm_jit};

  TestCase     tc;
  const size_t c = bo_array_size(assembly->test_cases);
  for (size_t i = 0; i < c; ++i) {
    tc = bo_array_at(assembly->test_cases, i, TestCase);
    test_case(&cnt, &tc);
  }

  msg_log("testing done\n");
}

void
__bl_assert_failure(const char *file, int line)
{
  msg_error("assertion failed in %s:%d", file, line);
  longjmp(assert_jmp, false);
}
#endif

/* public */
void
test_exec_run(Builder *builder, Assembly *assembly)
{}
