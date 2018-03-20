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
visit_module(bl_visitor_t *visitor, bl_module_t *module, bl_src_t *src)
{
  bl_visitor_walk_module(visitor, module);
}

static void
visit_item(bl_visitor_t *visitor, bl_item_t *item, bl_src_t *src)
{
  bl_visitor_walk_item(visitor, item);
}

static void
visit_func_decl(bl_visitor_t *visitor, bl_func_decl_t *func_decl, bl_src_t *src)
{
  bl_visitor_walk_func_decl(visitor, func_decl);
}

static void
visit_block(bl_visitor_t *visitor, bl_block_t *block, bl_src_t *src)
{
  bl_visitor_walk_block(visitor, block);
}

static void
visit_struct_decl(bl_visitor_t *visitor, bl_struct_decl_t *struct_decl, bl_src_t *src)
{
  bl_visitor_walk_struct_decl(visitor, struct_decl);
}

static void
visit_enum_decl(bl_visitor_t *visitor, bl_enum_decl_t *enum_decl, bl_src_t *src)
{
  bl_visitor_walk_enum_decl(visitor, enum_decl);
}

static void
visit_stmt(bl_visitor_t *visitor, bl_stmt_t *stmt, bl_src_t *src)
{
  bl_visitor_walk_stmt(visitor, stmt);
}

static void
visit_decl(bl_visitor_t *visitor, bl_decl_t *decl, bl_src_t *src)
{
  bl_visitor_walk_decl(visitor, decl);
}

static void
visit_expr(bl_visitor_t *visitor, bl_expr_t *expr, bl_src_t *src)
{
  bl_visitor_walk_expr(visitor, expr);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context                        = context;
  visitor->nesting                        = 0;
  visitor->visitors[BL_VISIT_MODULE]      = visit_module;
  visitor->visitors[BL_VISIT_ITEM]        = visit_item;
  visitor->visitors[BL_VISIT_FUNC_DECL]   = visit_func_decl;
  visitor->visitors[BL_VISIT_BLOCK]       = visit_block;
  visitor->visitors[BL_VISIT_ENUM_DECL]   = visit_enum_decl;
  visitor->visitors[BL_VISIT_STRUCT_DECL] = visit_struct_decl;
  visitor->visitors[BL_VISIT_STMT]        = visit_stmt;
  visitor->visitors[BL_VISIT_DECL]        = visit_decl;
  visitor->visitors[BL_VISIT_EXPR]        = visit_expr;
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
    v(visitor, (bl_module_t *)root, &root->src);
  }
}

void
bl_visitor_walk_module(bl_visitor_t *visitor, bl_module_t *module)
{
  visitor->nesting++;
  bl_visit_item_f v    = visitor->visitors[BL_VISIT_ITEM];
  const size_t    c    = bl_ast_module_item_count(module);
  bl_item_t *     item = NULL;

  for (size_t i = 0; i < c; i++) {
    item = bl_ast_module_get_item(module, i);
    v(visitor, item, &item->base_.src);
  }
  visitor->nesting--;
}

void
bl_visitor_walk_item(bl_visitor_t *visitor, bl_item_t *item)
{
  visitor->nesting++;
  switch (item->t) {
  case BL_ITEM_MODULE: {
    bl_visit_module_f v = visitor->visitors[BL_VISIT_MODULE];
    v(visitor, item->node.module, &item->node.module->base_.src);
    break;
  }

  case BL_ITEM_STRUCT: {
    bl_visit_struct_decl_f v = visitor->visitors[BL_VISIT_STRUCT_DECL];
    v(visitor, item->node.struct_decl, &item->node.struct_decl->base_.src);
    break;
  }

  case BL_ITEM_ENUM: {
    bl_visit_enum_decl_f v = visitor->visitors[BL_VISIT_ENUM_DECL];
    v(visitor, item->node.enum_decl, &item->node.enum_decl->base_.src);
    break;
  }

  case BL_ITEM_FUNC: {
    bl_visit_func_decl_f vf = visitor->visitors[BL_VISIT_FUNC_DECL];
    vf(visitor, item->node.func.func_decl, &item->node.func.func_decl->base_.src);

    bl_visit_block_f vb = visitor->visitors[BL_VISIT_BLOCK];
    vb(visitor, item->node.func.block, &item->node.func.block->base_.src);
    break;
  }

  case BL_ITEM_EXTERN:
    break;
  }

  visitor->nesting--;
}

void
bl_visitor_walk_func_decl(bl_visitor_t *visitor, bl_func_decl_t *func_decl)
{
}

void
bl_visitor_walk_block(bl_visitor_t *visitor, bl_block_t *block)
{
  visitor->nesting++;

  bl_visit_stmt_f v    = visitor->visitors[BL_VISIT_STMT];
  const size_t    c    = bl_ast_block_stmt_count(block);
  bl_stmt_t *     stmt = NULL;

  for (size_t i = 0; i < c; i++) {
    stmt = bl_ast_block_get_stmt(block, i);
    v(visitor, stmt, &stmt->base_.src);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_struct_decl(bl_visitor_t *visitor, bl_struct_decl_t *struct_decl)
{
  // TODO
}

void
bl_visitor_walk_enum_decl(bl_visitor_t *visitor, bl_enum_decl_t *enum_decl)
{
  // TODO
}

void
bl_visitor_walk_stmt(bl_visitor_t *visitor, bl_stmt_t *stmt)
{
  visitor->nesting++;

  switch (stmt->t) {
  case BL_STMT_EXPR: {
    bl_visit_expr_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, stmt->stmt.expr, &stmt->stmt.expr->base_.src);
    break;
  }

  case BL_STMT_DECL: {
    bl_visit_decl_f v = visitor->visitors[BL_VISIT_DECL];
    v(visitor, stmt->stmt.decl, &stmt->stmt.decl->base_.src);
    break;
  }

  case BL_STMT_BLOCK: {
    bl_visit_block_f v = visitor->visitors[BL_VISIT_BLOCK];
    v(visitor, stmt->stmt.block, &stmt->stmt.block->base_.src);
    break;
  }
  }

  visitor->nesting--;
}

void
bl_visitor_walk_decl(bl_visitor_t *visitor, bl_decl_t *decl)
{
  visitor->nesting++;
  if (decl->init_expr != NULL) {
    bl_visit_expr_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, decl->init_expr, &decl->init_expr->base_.src);
  }
  visitor->nesting--;
}

void
bl_visitor_walk_expr(bl_visitor_t *visitor, bl_expr_t *expr)
{
}
