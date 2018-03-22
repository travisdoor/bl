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

static void
visit_type(bl_visitor_t *visitor, bl_type_t *type, bl_src_t *src)
{
  bl_visitor_walk_type(visitor, type);
}

static void
visit_arg(bl_visitor_t *visitor, bl_arg_t *arg, bl_src_t *src)
{
  bl_visitor_walk_arg(visitor, arg);
}

static void
visit_struct(bl_visitor_t *visitor, bl_struct_t *strct, bl_src_t *src)
{
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_enum_t *enm, bl_src_t *src)
{
  bl_visitor_walk_enum(visitor, enm);
}

static void
visit_var(bl_visitor_t *visitor, bl_var_t *var, bl_src_t *src)
{
  bl_visitor_walk_var(visitor, var);
}

static void
visit_block(bl_visitor_t *visitor, bl_block_t *block, bl_src_t *src)
{
  bl_visitor_walk_block(visitor, block);
}

static void
visit_expr(bl_visitor_t *visitor, bl_expr_t *expr, bl_src_t *src)
{
  bl_visitor_walk_expr(visitor, expr);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context = context;
  visitor->nesting = 0;

  visitor->visitors[BL_VISIT_MODULE] = visit_module;
  visitor->visitors[BL_VISIT_FUNC]   = visit_func;
  visitor->visitors[BL_VISIT_TYPE]   = visit_type;
  visitor->visitors[BL_VISIT_ARG]    = visit_arg;
  visitor->visitors[BL_VISIT_STRUCT] = visit_struct;
  visitor->visitors[BL_VISIT_ENUM]   = visit_enum;
  visitor->visitors[BL_VISIT_VAR]    = visit_var;
  visitor->visitors[BL_VISIT_BLOCK]  = visit_block;
  visitor->visitors[BL_VISIT_EXPR]   = visit_expr;
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
      v(visitor, &bl_peek_module(node), &node->src);
      break;
    }

    case BL_NODE_FUNC: {
      bl_visit_fn_f v = visitor->visitors[BL_VISIT_FUNC];
      v(visitor, &bl_peek_func(node), &node->src);
      break;
    }

    case BL_NODE_STRUCT: {
      bl_visit_struct_f v = visitor->visitors[BL_VISIT_STRUCT];
      v(visitor, &bl_peek_struct(node), &node->src);
      break;
    }

    case BL_NODE_ENUM: {
      bl_visit_enum_f v = visitor->visitors[BL_VISIT_ENUM];
      v(visitor, &bl_peek_enum(node), &node->src);
      break;
    }

    default:
      bl_abort("unknown node in visitor");
    }
  }
  visitor->nesting--;
}

void
bl_visitor_walk_func(bl_visitor_t *visitor, bl_func_t *func)
{
  visitor->nesting++;

  bl_visit_type_f  vt  = visitor->visitors[BL_VISIT_TYPE];
  bl_visit_arg_f   va  = visitor->visitors[BL_VISIT_ARG];
  bl_visit_block_f vb  = visitor->visitors[BL_VISIT_BLOCK];
  const size_t     c   = bl_ast_func_arg_count(func);
  bl_node_t *      arg = NULL;

  for (size_t i = 0; i < c; i++) {
    arg = bl_ast_func_get_arg(func, i);
    va(visitor, &bl_peek_arg(arg), &arg->src);
  }

  vt(visitor, &bl_peek_type(func->ret_type), &func->ret_type->src);
  if (func->block)
    vb(visitor, &bl_peek_block(func->block), &func->block->src);

  visitor->nesting--;
}

void
bl_visitor_walk_type(bl_visitor_t *visitor, bl_type_t *type)
{
  /* nothing to do, terminal node */
}

void
bl_visitor_walk_arg(bl_visitor_t *visitor, bl_arg_t *arg)
{
  visitor->nesting++;

  bl_visit_type_f vt = visitor->visitors[BL_VISIT_TYPE];
  vt(visitor, &bl_peek_type(arg->type), &arg->type->src);

  visitor->nesting--;
}

void
bl_visitor_walk_struct(bl_visitor_t *visitor, bl_struct_t *strct)
{
  // TODO
}

void
bl_visitor_walk_enum(bl_visitor_t *visitor, bl_enum_t *enm)
{
  // TODO
}

void
bl_visitor_walk_var(bl_visitor_t *visitor, bl_var_t *var)
{
  visitor->nesting++;

  bl_visit_type_f vt = visitor->visitors[BL_VISIT_TYPE];
  vt(visitor, &bl_peek_type(var->type), &var->type->src);

  visitor->nesting--;
}

void
bl_visitor_walk_block(bl_visitor_t *visitor, bl_block_t *block)
{
  visitor->nesting++;
  const size_t c    = bl_ast_block_node_count(block);
  bl_node_t *  node = NULL;

  for (size_t i = 0; i < c; i++) {
    node = bl_ast_block_get_node(block, i);

    switch (node->t) {
    case BL_NODE_VAR: {
      bl_visit_var_f v = visitor->visitors[BL_VISIT_VAR];
      v(visitor, &bl_peek_var(node), &node->src);
      break;
    }

    case BL_NODE_EXPR: {
      bl_visit_expr_f v = visitor->visitors[BL_VISIT_EXPR];
      v(visitor, &bl_peek_expr(node), &node->src);
      break;
    }

    case BL_NODE_BLOCK: {
      bl_visit_block_f v = visitor->visitors[BL_VISIT_BLOCK];
      v(visitor, &bl_peek_block(node), &node->src);
      break;
    }

    default:
      bl_abort("unknown node in block visit");
    }
  }

  visitor->nesting--;
}

void
bl_visitor_walk_expr(bl_visitor_t *visitor, bl_expr_t *expr)
{
  visitor->nesting++;

  switch (expr->t) {
  case BL_EXPR_BINOP: {
    bl_visit_expr_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, &bl_peek_expr(expr->expr.binop.lhs), &expr->expr.binop.lhs->src);
    v(visitor, &bl_peek_expr(expr->expr.binop.rhs), &expr->expr.binop.rhs->src);
  }
  case BL_EXPR_CONST:
    break;

  default:
    bl_abort("unknown node in expr visit");
  }

  visitor->nesting--;
}
