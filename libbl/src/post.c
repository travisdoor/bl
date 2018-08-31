//************************************************************************************************
// bl
//
// File:   post.c
// Author: Martin Dorazil
// Date:   31/8/18
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

#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"
#include "visitor_impl.h"

#define peek_cnt(visitor) ((context_t)(visitor)->context)

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
} context_t;

static void
visit_decl_value(bl_visitor_t *visitor, bl_node_t *decl_value);

void
visit_decl_value(bl_visitor_t *visitor, bl_node_t *decl_value)
{
  bl_log("decl value");
  bl_visitor_walk_decl_value(visitor, decl_value);
}

void
bl_post_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {
      .builder  = builder,
      .assembly = assembly,
      .unit     = NULL,
  };

  bl_visitor_t visitor;
  bl_visitor_init(&visitor, &cnt);

  bl_visitor_add(&visitor, visit_decl_value, BL_VISIT_DECL_VALUE);

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    bl_visitor_walk_decl_ublock(&visitor, unit->ast.root);
  }
}
