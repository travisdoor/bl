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

static void
visit_module(bl_visitor_t *visitor, bl_module_t *module)
{
  bl_visitor_walk_module(visitor, module);
}

static void
visit_item(bl_visitor_t *visitor, bl_item_t *item)
{
  bl_visitor_walk_item(visitor, item);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context                   = context;
  visitor->visitors[BL_VISIT_MODULE] = visit_module;
  visitor->visitors[BL_VISIT_ITEM]   = visit_item;
}

void
bl_visitor_add(bl_visitor_t *visitor, void *visit, bl_visit_e type)
{
  visitor->visitors[type] = visit;
}

void
bl_visitor_walk_root(bl_visitor_t *visitor, bl_node_t *root)
{
  if (root->t == BL_NODE_MODULE) {
    bl_visit_module_f v = visitor->visitors[BL_VISIT_MODULE];
    v(visitor, (bl_module_t *)root);
  }
}

void
bl_visitor_walk_module(bl_visitor_t *visitor, bl_module_t *module)
{
  bl_visit_item_f v    = visitor->visitors[BL_VISIT_ITEM];
  const size_t    c    = bl_ast_module_item_count(module);
  bl_item_t *     item = NULL;

  for (size_t i = 0; i < c; i++) {
    item = bl_ast_module_get_item(module, i);
    v(visitor, item);
  }
}

void
bl_visitor_walk_item(bl_visitor_t *visitor, bl_item_t *item)
{
  switch (item->t) {
  case BL_ITEM_MODULE: {
    bl_visit_module_f v = visitor->visitors[BL_VISIT_MODULE];
    v(visitor, item->node.module);
    break;
  }

  case BL_ITEM_FUNC:
  case BL_ITEM_STRUCT:
  case BL_ITEM_ENUM:
  case BL_ITEM_EXTERN:
    break;
  }
}
