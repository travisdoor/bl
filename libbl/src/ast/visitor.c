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
walk_block_content(bl_visitor_t *visitor, bl_node_t *stmt)
{
  if (stmt == NULL)
    return;

  switch (bl_node_code(stmt)) {
  case BL_DECL_BLOCK: {
    bl_visit_f v = visitor->visitors[BL_VISIT_BLOCK];
    v(visitor, stmt);
    break;
  }
  case BL_DECL_VAR: {
    bl_visit_f v = visitor->visitors[BL_VISIT_VAR];
    v(visitor, stmt);
    break;
  }

  case BL_EXPR_CALL:
  case BL_EXPR_VAR_REF:
  case BL_EXPR_BINOP:
  case BL_EXPR_CONST:
  case BL_EXPR_PATH: {
    bl_visit_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, stmt);
    break;
  }

  case BL_STMT_IF: {
    bl_visit_f v = visitor->visitors[BL_VISIT_IF];
    v(visitor, stmt);
    break;
  }

  case BL_STMT_LOOP: {
    bl_visit_f v = visitor->visitors[BL_VISIT_LOOP];
    v(visitor, stmt);
    break;
  }

  case BL_STMT_WHILE: {
    bl_visit_f v = visitor->visitors[BL_VISIT_WHILE];
    v(visitor, stmt);
    break;
  }

  case BL_STMT_BREAK: {
    bl_visit_f v = visitor->visitors[BL_VISIT_BREAK];
    v(visitor, stmt);
    break;
  }

  case BL_STMT_CONTINUE: {
    bl_visit_f v = visitor->visitors[BL_VISIT_CONTINUE];
    v(visitor, stmt);
    break;
  }

  case BL_STMT_RETURN: {
    bl_visit_f v = visitor->visitors[BL_VISIT_RETURN];
    v(visitor, stmt);
    break;
  }

  default:
    bl_abort("unknown statement");
  }
}

static void
walk_block_content(bl_visitor_t *visitor, bl_node_t *stmt);

static void
visit_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_visitor_walk_module(visitor, module);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_visitor_walk_func(visitor, func);
}

static void
visit_type(bl_visitor_t *visitor, bl_node_t *type)
{
  bl_visitor_walk_type(visitor, type);
}

static void
visit_arg(bl_visitor_t *visitor, bl_node_t *arg)
{
  bl_visitor_walk_arg(visitor, arg);
}

static void
visit_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  bl_visitor_walk_enum(visitor, enm);
}

static void
visit_var(bl_visitor_t *visitor, bl_node_t *var)
{
  bl_visitor_walk_var(visitor, var);
}

static void
visit_block(bl_visitor_t *visitor, bl_node_t *block)
{
  bl_visitor_walk_block(visitor, block);
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  bl_visitor_walk_expr(visitor, expr);
}

static void
visit_if(bl_visitor_t *visitor, bl_node_t *if_stmt)
{
  bl_visitor_walk_if(visitor, if_stmt);
}

static void
visit_loop(bl_visitor_t *visitor, bl_node_t *stmt_loop)
{
  bl_visitor_walk_loop(visitor, stmt_loop);
}

static void
visit_while(bl_visitor_t *visitor, bl_node_t *stmt_while)
{
  bl_visitor_walk_while(visitor, stmt_while);
}

static void
visit_break(bl_visitor_t *visitor, bl_node_t *stmt_break)
{
  bl_visitor_walk_break(visitor, stmt_break);
}

static void
visit_continue(bl_visitor_t *visitor, bl_node_t *stmt_continue)
{
  bl_visitor_walk_continue(visitor, stmt_continue);
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t *stmt_return)
{
  bl_visitor_walk_return(visitor, stmt_return);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context = context;
  visitor->nesting = 0;

  visitor->visitors[BL_VISIT_MODULE]   = visit_module;
  visitor->visitors[BL_VISIT_FUNC]     = visit_func;
  visitor->visitors[BL_VISIT_TYPE]     = visit_type;
  visitor->visitors[BL_VISIT_ARG]      = visit_arg;
  visitor->visitors[BL_VISIT_STRUCT]   = visit_struct;
  visitor->visitors[BL_VISIT_ENUM]     = visit_enum;
  visitor->visitors[BL_VISIT_VAR]      = visit_var;
  visitor->visitors[BL_VISIT_BLOCK]    = visit_block;
  visitor->visitors[BL_VISIT_EXPR]     = visit_expr;
  visitor->visitors[BL_VISIT_IF]       = visit_if;
  visitor->visitors[BL_VISIT_LOOP]     = visit_loop;
  visitor->visitors[BL_VISIT_WHILE]    = visit_while;
  visitor->visitors[BL_VISIT_BREAK]    = visit_break;
  visitor->visitors[BL_VISIT_CONTINUE] = visit_continue;
  visitor->visitors[BL_VISIT_RETURN]   = visit_return;
}

void
bl_visitor_add(bl_visitor_t *visitor, bl_visit_f visit, bl_visit_e type)
{
  visitor->visitors[type] = visit;
}

void
bl_visitor_walk_module(bl_visitor_t *visitor, bl_node_t *module)
{
  visitor->nesting++;
  const size_t c    = bl_ast_module_node_count(module);
  bl_node_t *  node = NULL;
  for (size_t i = 0; i < c; i++) {
    node = bl_ast_module_get_node(module, i);

    switch (bl_node_code(node)) {
    case BL_DECL_MODULE: {
      bl_visit_f v = visitor->visitors[BL_VISIT_MODULE];
      v(visitor, node);
      break;
    }

    case BL_DECL_FUNC: {
      bl_visit_f v = visitor->visitors[BL_VISIT_FUNC];
      v(visitor, node);
      break;
    }

    case BL_DECL_STRUCT: {
      bl_visit_f v = visitor->visitors[BL_VISIT_STRUCT];
      v(visitor, node);
      break;
    }

    case BL_DECL_ENUM: {
      bl_visit_f v = visitor->visitors[BL_VISIT_ENUM];
      v(visitor, node);
      break;
    }

    default:
      bl_abort("unknown node in module");
    }
  }
  visitor->nesting--;
}

void
bl_visitor_walk_func(bl_visitor_t *visitor, bl_node_t *func)
{
  visitor->nesting++;

  bl_visit_f   vt  = visitor->visitors[BL_VISIT_TYPE];
  bl_visit_f   va  = visitor->visitors[BL_VISIT_ARG];
  bl_visit_f   vb  = visitor->visitors[BL_VISIT_BLOCK];
  const size_t c   = bl_ast_func_arg_count(func);
  bl_node_t *  arg = NULL;

  for (size_t i = 0; i < c; i++) {
    arg = bl_ast_func_get_arg(func, i);
    va(visitor, arg);
  }

  vt(visitor, bl_peek_decl_func(func)->ret_type);
  if (bl_peek_decl_func(func)->block)
    vb(visitor, bl_peek_decl_func(func)->block);

  visitor->nesting--;
}

void
bl_visitor_walk_type(bl_visitor_t *visitor, bl_node_t *type)
{
  /* nothing to do, terminal node */
}

void
bl_visitor_walk_arg(bl_visitor_t *visitor, bl_node_t *arg)
{
  visitor->nesting++;

  bl_visit_f vt = visitor->visitors[BL_VISIT_TYPE];
  vt(visitor, bl_peek_decl_arg(arg)->type);

  visitor->nesting--;
}

void
bl_visitor_walk_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  // TODO
}

void
bl_visitor_walk_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  // TODO
}

void
bl_visitor_walk_var(bl_visitor_t *visitor, bl_node_t *var)
{
  visitor->nesting++;

  bl_visit_f vt = visitor->visitors[BL_VISIT_TYPE];
  vt(visitor, bl_peek_decl_var(var)->type);

  if (bl_peek_decl_var(var)->init_expr) {
    bl_visit_f ve = visitor->visitors[BL_VISIT_EXPR];
    ve(visitor, bl_peek_decl_var(var)->init_expr);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_block(bl_visitor_t *visitor, bl_node_t *block)
{
  visitor->nesting++;
  const size_t c    = bl_ast_block_node_count(block);
  bl_node_t *  node = NULL;

  for (size_t i = 0; i < c; i++) {
    node = bl_ast_block_get_node(block, i);
    walk_block_content(visitor, node);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  visitor->nesting++;

  switch (bl_node_code(expr)) {

  case BL_EXPR_BINOP: {
    bl_visit_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, bl_peek_expr_binop(expr)->lhs);
    v(visitor, bl_peek_expr_binop(expr)->rhs);
    break;
  }

  case BL_EXPR_PATH: {
    bl_visit_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, bl_peek_expr_path(expr)->next);
    break;
  }

  case BL_EXPR_CALL: {
    bl_visit_f   v   = visitor->visitors[BL_VISIT_EXPR];
    const size_t c   = bl_ast_call_arg_count(expr);
    bl_node_t *  arg = NULL;
    for (size_t i = 0; i < c; i++) {
      arg = bl_ast_call_get_arg(expr, i);
      v(visitor, arg);
    }
    break;
  }

  case BL_EXPR_CONST:
  case BL_EXPR_VAR_REF:
    break;

  default:
    bl_abort("unknown node in expr visit");
  }

  visitor->nesting--;
}

void
bl_visitor_walk_if(bl_visitor_t *visitor, bl_node_t *if_stmt)
{
  visitor->nesting++;
  bl_visit_f vexpr = visitor->visitors[BL_VISIT_EXPR];
  vexpr(visitor, bl_peek_stmt_if(if_stmt)->test);
  walk_block_content(visitor, bl_peek_stmt_if(if_stmt)->true_stmt);
  walk_block_content(visitor, bl_peek_stmt_if(if_stmt)->false_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_loop(bl_visitor_t *visitor, bl_node_t *stmt_loop)
{
  visitor->nesting++;
  walk_block_content(visitor, bl_peek_stmt_loop(stmt_loop)->true_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_while(bl_visitor_t *visitor, bl_node_t *stmt_while)
{
  visitor->nesting++;
  bl_visit_f vexpr = visitor->visitors[BL_VISIT_EXPR];
  vexpr(visitor, bl_peek_stmt_while(stmt_while)->test);
  walk_block_content(visitor, bl_peek_stmt_while(stmt_while)->true_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_break(bl_visitor_t *visitor, bl_node_t *stmt_break)
{
  // terminal
}

void
bl_visitor_walk_continue(bl_visitor_t *visitor, bl_node_t *stmt_continue)
{
  // terminal
}

void
bl_visitor_walk_return(bl_visitor_t *visitor, bl_node_t *stmt_return)
{
  visitor->nesting++;
  if (bl_peek_stmt_return(stmt_return)->expr) {
    bl_visit_f vexpr = visitor->visitors[BL_VISIT_EXPR];
    vexpr(visitor, bl_peek_stmt_return(stmt_return)->expr);
  }
  visitor->nesting--;
}
