//************************************************************************************************
// blc
//
// File:   evaluator.c
// Author: Martin Dorazil
// Date:   26.4.18
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

/*************************************************************************************************
 * Constant expressions in static array declaration and enum variant declaration need
 * to be evaluated due to correct llvm generation. All expresions processed here must
 * be checked in type check already.
 *
 * Ex.: var arr i32[10 + 2]; is evaluated to var arr i32[12];
 *************************************************************************************************/

#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

/*************************************************************************************************
 * visitors
 *************************************************************************************************/

static void
eval_enum_variant(bl_visitor_t *visitor, bl_node_t *var)
{
  bl_log("trying to evaluate enum variant");
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_evaluator_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bl_visitor_t visitor_eval;
  bl_visitor_init(&visitor_eval, NULL);
  bl_visitor_add(&visitor_eval, eval_enum_variant, BL_VISIT_ENUM_VARIANT);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;
  for (int i = 0; i < c; ++i) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_eval, unit->ast.root);
  }

  return BL_NO_ERR;
}
