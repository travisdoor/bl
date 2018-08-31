//************************************************************************************************
// bl
//
// File:   visitor.c
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

#include "visitor_impl.h"

static inline void
call_visit(bl_visitor_t *visitor, bl_node_t *node, bl_visit_e type)
{
  if (visitor->visitors[type] != NULL) visitor->visitors[type](visitor, node);
}

static void
visit_decl(bl_visitor_t *visitor, bl_node_t *decl)
{
  bl_visitor_walk_decl(visitor, decl);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context = context;
  visitor->nesting = 0;

  visitor->visitors[BL_VISIT_DECL]  = visit_decl;
}

void
bl_visitor_add(bl_visitor_t *visitor, bl_visit_f callback, bl_visit_e type)
{
  visitor->visitors[type] = callback;
}

void
bl_visitor_walk_ublock(bl_visitor_t *visitor, bl_node_t *ublock)
{
  assert(ublock);
  visitor->nesting++;
  bl_node_ublock_t *_ublock = bl_peek_ublock(ublock);

  bl_node_t *node;
  bl_node_foreach(_ublock->nodes, node)
  {
    switch (bl_node_code(node)) {
    case BL_NODE_DECL:
      call_visit(visitor, node, BL_VISIT_DECL);
      break;
    case BL_NODE_LINK:
    case BL_NODE_LOAD:
      break;

    default:
      bl_abort("invalid node in ublock");
    }
  }
}

void
bl_visitor_walk_decl(bl_visitor_t *visitor, bl_node_t *decl)
{
  assert(decl);
  //bl_node_decl_t *_decl = bl_peek_decl(decl);
}
