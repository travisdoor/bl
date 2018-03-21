//*****************************************************************************
// bl
//
// File:   visitor.c
// Author: Martin Dorazil
// Date:   3/20/18
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
//*****************************************************************************

#include <string.h>
#include "visitor_impl.h"
#include "common_impl.h"

static void
visit_module(bl_visitor_t *visitor, bl_module_t *module, bl_src_t *src)
{
  bl_visitor_walk_module(visitor, module);
}

static void
visit_func(bl_visitor_t *visitor, bl_func_t *func, bl_src_t *src)
{
  bl_visitor_walk_func(visitor, func);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context = context;
  visitor->nesting = 0;

  visitor->visitors[BL_VISIT_MODULE] = visit_module;
  visitor->visitors[BL_VISIT_FUNC]   = visit_func;
}

void
bl_visitor_add(bl_visitor_t *visitor, void *visit, bl_visit_e type)
{
  visitor->visitors[type] = visit;
}

void
bl_visitor_walk_module(bl_visitor_t *visitor, bl_module_t *module)
{
  visitor->nesting++;
  const size_t c    = bl_ast_module_node_count(module);
  bl_node_t *  node = NULL;
  for (size_t i = 0; i < c; i++) {
    node = bl_ast_module_get_node(module, i);

    switch (node->t) {
    case BL_NODE_MODULE: {
      bl_visit_module_f v = visitor->visitors[BL_VISIT_MODULE];
      v(visitor, &BL_MODULE(node), &node->src);
      break;
    }
    case BL_NODE_FUNC: {
      bl_visit_fn_f v = visitor->visitors[BL_VISIT_FUNC];
      v(visitor, &BL_FUNC(node), &node->src);
      break;
    }
    default:
      bl_abort("unknown node");
    }
  }
  visitor->nesting--;
}

void
bl_visitor_walk_func(bl_visitor_t *visitor, bl_func_t *module)
{
  // TODO
}
