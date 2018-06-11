//************************************************************************************************
// blc
//
// File:   test_runner.c
// Author: Martin Dorazil
// Date:   8.6.18
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

#include "common_impl.h"
#include "stages_impl.h"

#define MAX_REPORT_LEN 512
#define REPORT_COL_W 80

bl_error_e
bl_test_runner_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bl_msg_log(BL_GREEN("Running utests:"));

  const size_t c                   = bo_array_size(assembly->utest_methods);
  bl_utest_t * utest               = NULL;
  bl_src_t *   src                 = NULL;
  bool         status              = false;
  char         tmp[MAX_REPORT_LEN] = {0};

  if (!c) {
    bl_msg_log(BL_YELLOW("no tests found"));
    return BL_NO_ERR;
  }
  
  for (size_t i = 0; i < c; ++i) {
    utest = &bo_array_at(assembly->utest_methods, i, bl_utest_t);
    src   = utest->func->src;
    LLVMGenericValueRef result =
        LLVMRunFunction(assembly->llvm_compiletime_engine, utest->llvm_func, 0, NULL);
    status = !(int)LLVMGenericValueToInt(result, 0);

    snprintf(tmp, MAX_REPORT_LEN, "  " BL_GREEN("%s") " %s:%d",
             bl_peek_decl_func(utest->func)->id.str, src->unit->name, src->line);

    const int dcount = REPORT_COL_W - strlen(tmp) - 6 + 2 * strlen(BL_GREEN_BEGIN);
    for (int j = 0; j < dcount; ++j) {
      sprintf(tmp + strlen(tmp), ".");
    }

    bl_msg_log("%s%s", tmp, status ? "[" BL_GREEN(" OK ") "]" : "[" BL_RED("FAIL") "]");
  }

  return BL_NO_ERR;
}
