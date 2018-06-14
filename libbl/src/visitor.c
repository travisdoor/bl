//************************************************************************************************
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
//************************************************************************************************

#include <string.h>
#include "visitor_impl.h"
#include "common_impl.h"

/* call visit method if it's not set to NULL */
#define call_visit(visitor, node, type)                                                            \
  if ((visitor)->visitors[(type)] != NULL) {                                                       \
    (visitor)->visitors[(type)]((visitor), (node));                                                \
  }

static void
walk_block_content(bl_visitor_t *visitor, bl_node_t **stmt)
{
  if (*stmt == NULL)
    return;

  switch (bl_node_code(*stmt)) {
  case BL_DECL_BLOCK: {
    call_visit(visitor, stmt, BL_VISIT_BLOCK);
    break;
  }

  case BL_DECL_MUT: {
    call_visit(visitor, stmt, BL_VISIT_MUT);
    break;
  }

  case BL_DECL_CONST: {
    call_visit(visitor, stmt, BL_VISIT_CONST);
    break;
  }

  case BL_EXPR_CALL:
  case BL_EXPR_DECL_REF:
  case BL_EXPR_MEMBER_REF:
  case BL_EXPR_ARRAY_REF:
  case BL_EXPR_BINOP:
  case BL_EXPR_UNARY:
  case BL_EXPR_SIZEOF:
  case BL_EXPR_CONST: {
    call_visit(visitor, stmt, BL_VISIT_EXPR);
    break;
  }

  case BL_STMT_IF: {
    call_visit(visitor, stmt, BL_VISIT_IF);
    break;
  }

  case BL_STMT_USING: {
    call_visit(visitor, stmt, BL_VISIT_USING);
    break;
  }

  case BL_STMT_LOOP: {
    call_visit(visitor, stmt, BL_VISIT_LOOP);
    break;
  }

  case BL_STMT_BREAK: {
    call_visit(visitor, stmt, BL_VISIT_BREAK);
    break;
  }

  case BL_STMT_CONTINUE: {
    call_visit(visitor, stmt, BL_VISIT_CONTINUE);
    break;
  }

  case BL_STMT_RETURN: {
    call_visit(visitor, stmt, BL_VISIT_RETURN);
    break;
  }

  default:
    bl_abort("unknown statement");
  }
}

static void
walk_block_content(bl_visitor_t *visitor, bl_node_t **stmt);

static void
visit_module(bl_visitor_t *visitor, bl_node_t **module)
{
  bl_visitor_walk_module(visitor, module);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t **func)
{
  bl_visitor_walk_func(visitor, func);
}

static void
visit_type(bl_visitor_t *visitor, bl_node_t **type)
{
  bl_visitor_walk_type(visitor, type);
}

static void
visit_arg(bl_visitor_t *visitor, bl_node_t **arg)
{
  bl_visitor_walk_arg(visitor, arg);
}

static void
visit_struct(bl_visitor_t *visitor, bl_node_t **strct)
{
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t **enm)
{
  bl_visitor_walk_enum(visitor, enm);
}

static void
visit_mut(bl_visitor_t *visitor, bl_node_t **mut)
{
  bl_visitor_walk_mut(visitor, mut);
}

static void
visit_const(bl_visitor_t *visitor, bl_node_t **cnst)
{
  bl_visitor_walk_const(visitor, cnst);
}

static void
visit_block(bl_visitor_t *visitor, bl_node_t **block)
{
  bl_visitor_walk_block(visitor, block);
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t **expr)
{
  bl_visitor_walk_expr(visitor, expr);
}

static void
visit_if(bl_visitor_t *visitor, bl_node_t **if_stmt)
{
  bl_visitor_walk_if(visitor, if_stmt);
}

static void
visit_loop(bl_visitor_t *visitor, bl_node_t **stmt_loop)
{
  bl_visitor_walk_loop(visitor, stmt_loop);
}

static void
visit_break(bl_visitor_t *visitor, bl_node_t **stmt_break)
{
  bl_visitor_walk_break(visitor, stmt_break);
}

static void
visit_continue(bl_visitor_t *visitor, bl_node_t **stmt_continue)
{
  bl_visitor_walk_continue(visitor, stmt_continue);
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t **stmt_return)
{
  bl_visitor_walk_return(visitor, stmt_return);
}

static void
visit_struct_member(bl_visitor_t *visitor, bl_node_t **member)
{
  bl_visitor_walk_struct_member(visitor, member);
}

static void
visit_enum_variant(bl_visitor_t *visitor, bl_node_t **variant)
{
  bl_visitor_walk_enum_variant(visitor, variant);
}

static void
visit_pre_load(bl_visitor_t *visitor, bl_node_t **pre_load)
{
  bl_visitor_walk_load(visitor, pre_load);
}

static void
visit_pre_link(bl_visitor_t *visitor, bl_node_t **pre_link)
{
  bl_visitor_walk_link(visitor, pre_link);
}

static void
visit_using(bl_visitor_t *visitor, bl_node_t **using)
{
  bl_visitor_walk_using(visitor, using);
}

void
bl_visitor_init(bl_visitor_t *visitor, void *context)
{
  visitor->context = context;
  visitor->nesting = 0;

  visitor->visitors[BL_VISIT_MODULE]        = visit_module;
  visitor->visitors[BL_VISIT_FUNC]          = visit_func;
  visitor->visitors[BL_VISIT_TYPE]          = visit_type;
  visitor->visitors[BL_VISIT_ARG]           = visit_arg;
  visitor->visitors[BL_VISIT_STRUCT]        = visit_struct;
  visitor->visitors[BL_VISIT_ENUM]          = visit_enum;
  visitor->visitors[BL_VISIT_MUT]           = visit_mut;
  visitor->visitors[BL_VISIT_CONST]         = visit_const;
  visitor->visitors[BL_VISIT_BLOCK]         = visit_block;
  visitor->visitors[BL_VISIT_EXPR]          = visit_expr;
  visitor->visitors[BL_VISIT_IF]            = visit_if;
  visitor->visitors[BL_VISIT_LOOP]          = visit_loop;
  visitor->visitors[BL_VISIT_BREAK]         = visit_break;
  visitor->visitors[BL_VISIT_CONTINUE]      = visit_continue;
  visitor->visitors[BL_VISIT_RETURN]        = visit_return;
  visitor->visitors[BL_VISIT_STRUCT_MEMBER] = visit_struct_member;
  visitor->visitors[BL_VISIT_ENUM_VARIANT]  = visit_enum_variant;
  visitor->visitors[BL_VISIT_LOAD]          = visit_pre_load;
  visitor->visitors[BL_VISIT_LINK]          = visit_pre_link;
  visitor->visitors[BL_VISIT_USING]         = visit_using;
}

void
bl_visitor_add(bl_visitor_t *visitor, bl_visit_f visit, bl_visit_e type)
{
  visitor->visitors[type] = visit;
}

void
bl_visitor_walk_module(bl_visitor_t *visitor, bl_node_t **module)
{
  visitor->nesting++;
  bl_decl_module_t *_module = bl_peek_decl_module(*module);
  bl_node_t **       node    = &_module->nodes;
  while (*node) {
    switch (bl_node_code(*node)) {
    case BL_DECL_MODULE: {
      call_visit(visitor, node, BL_VISIT_MODULE);
      break;
    }

    case BL_DECL_FUNC: {
      call_visit(visitor, node, BL_VISIT_FUNC);
      break;
    }

    case BL_STMT_USING: {
      call_visit(visitor, node, BL_VISIT_USING);
      break;
    }

    case BL_DECL_STRUCT: {
      call_visit(visitor, node, BL_VISIT_STRUCT);
      break;
    }

    case BL_DECL_ENUM: {
      call_visit(visitor, node, BL_VISIT_ENUM);
      break;
    }

    case BL_DECL_CONST: {
      call_visit(visitor, node, BL_VISIT_CONST);
      break;
    }

    case BL_PRE_LOAD: {
      call_visit(visitor, node, BL_VISIT_LOAD);
      break;
    }

    case BL_PRE_LINK: {
      call_visit(visitor, node, BL_VISIT_LINK);
      break;
    }

    default:
      bl_abort("unknown node in module");
    }

    node = &(*node)->next;
  }
  visitor->nesting--;
}

void
bl_visitor_walk_gscope(bl_visitor_t *visitor, bl_node_t **root)
{
  call_visit(visitor, root, BL_VISIT_MODULE);
}

void
bl_visitor_walk_func(bl_visitor_t *visitor, bl_node_t **func)
{
  visitor->nesting++;

  bl_decl_func_t *fn = bl_peek_decl_func(*func);

  bl_node_t **arg = &fn->args;
  while (*arg) {
    call_visit(visitor, arg, BL_VISIT_ARG);
    arg = &(*arg)->next;
  }

  call_visit(visitor, &fn->ret_type, BL_VISIT_TYPE);

  if (bl_peek_decl_func(*func)->block) {
    call_visit(visitor, &bl_peek_decl_func(*func)->block, BL_VISIT_BLOCK);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_type(bl_visitor_t *visitor, bl_node_t **type)
{
  visitor->nesting++;
  bl_node_t **dim = bl_ast_get_type_dim(*type);

  if (*dim)
    call_visit(visitor, dim, BL_VISIT_EXPR);
  visitor->nesting--;
}

void
bl_visitor_walk_arg(bl_visitor_t *visitor, bl_node_t **arg)
{
  visitor->nesting++;
  call_visit(visitor, &bl_peek_decl_arg(*arg)->type, BL_VISIT_TYPE);
  visitor->nesting--;
}

void
bl_visitor_walk_struct(bl_visitor_t *visitor, bl_node_t **strct)
{
  visitor->nesting++;
  bl_decl_struct_t *_strct = bl_peek_decl_struct(*strct);
  bl_node_t **       member = &_strct->members;
  while (*member) {
    call_visit(visitor, member, BL_VISIT_STRUCT_MEMBER);
    member = &(*member)->next;
  }

  visitor->nesting--;
}

void
bl_visitor_walk_enum(bl_visitor_t *visitor, bl_node_t **enm)
{
  visitor->nesting++;
  bl_decl_enum_t *_enm = bl_peek_decl_enum(*enm);
  call_visit(visitor, &_enm->type, BL_VISIT_TYPE);

  bl_node_t **variant = &_enm->variants;

  while (*variant) {
    call_visit(visitor, variant, BL_VISIT_ENUM_VARIANT);
    variant = &(*variant)->next;
  }

  visitor->nesting--;
}

void
bl_visitor_walk_mut(bl_visitor_t *visitor, bl_node_t **mut)
{
  visitor->nesting++;

  bl_decl_mut_t *_mut = bl_peek_decl_mut(*mut);
  call_visit(visitor, &_mut->type, BL_VISIT_TYPE);

  if (_mut->init_expr) {
    call_visit(visitor, &bl_peek_decl_mut(*mut)->init_expr, BL_VISIT_EXPR);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_const(bl_visitor_t *visitor, bl_node_t **cnst)
{
  visitor->nesting++;

  bl_decl_const_t *_cnst = bl_peek_decl_const(*cnst);
  call_visit(visitor, &_cnst->type, BL_VISIT_TYPE);

  if (_cnst->init_expr) {
    call_visit(visitor, &bl_peek_decl_const(*cnst)->init_expr, BL_VISIT_EXPR);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_block(bl_visitor_t *visitor, bl_node_t **block)
{
  visitor->nesting++;
  bl_decl_block_t *_block = bl_peek_decl_block(*block);
  bl_node_t **      node   = &_block->nodes;

  while (*node) {
    walk_block_content(visitor, node);
    node = &(*node)->next;
  }

  visitor->nesting--;
}

void
bl_visitor_walk_expr(bl_visitor_t *visitor, bl_node_t **expr)
{
  visitor->nesting++;

  switch (bl_node_code(*expr)) {

  case BL_EXPR_BINOP: {
    call_visit(visitor, &bl_peek_expr_binop(*expr)->lhs, BL_VISIT_EXPR);
    call_visit(visitor, &bl_peek_expr_binop(*expr)->rhs, BL_VISIT_EXPR);
    break;
  }

  case BL_EXPR_MEMBER_REF: {
    call_visit(visitor, &bl_peek_expr_member_ref(*expr)->next, BL_VISIT_EXPR);
    break;
  }

  case BL_EXPR_SIZEOF: {
    call_visit(visitor, &bl_peek_expr_sizeof(*expr)->des_type, BL_VISIT_TYPE);
    break;
  }

  case BL_EXPR_CAST: {
    call_visit(visitor, &bl_peek_expr_cast(*expr)->type, BL_VISIT_TYPE);
    call_visit(visitor, &bl_peek_expr_cast(*expr)->next, BL_VISIT_EXPR);
    break;
  }

  case BL_EXPR_UNARY: {
    call_visit(visitor, &bl_peek_expr_unary(*expr)->next, BL_VISIT_EXPR);
    break;
  }

  case BL_EXPR_ARRAY_REF: {
    call_visit(visitor, &bl_peek_expr_array_ref(*expr)->index, BL_VISIT_EXPR);
    call_visit(visitor, &bl_peek_expr_array_ref(*expr)->next, BL_VISIT_EXPR);
    break;
  }

  case BL_EXPR_INIT: {
    bl_expr_init_t *_init = bl_peek_expr_init(*expr);

    if (_init->type)
      call_visit(visitor, &_init->type, BL_VISIT_TYPE);

    bl_node_t **expr = &_init->exprs;
    while (*expr) {
      call_visit(visitor, expr, BL_VISIT_EXPR);
      expr = &(*expr)->next;
    }
    break;
  }

  case BL_EXPR_CALL: {
    bl_expr_call_t *_call = bl_peek_expr_call(*expr);
    bl_node_t **     arg   = &_call->args;

    while (*arg) {
      call_visit(visitor, arg, BL_VISIT_EXPR);
      arg = &(*arg)->next;
    }
    break;
  }

  case BL_EXPR_CONST:
  case BL_EXPR_DECL_REF:
  case BL_EXPR_NULL:
    break;

  default:
    bl_abort("unknown node in expr visit");
  }

  visitor->nesting--;
}

void
bl_visitor_walk_if(bl_visitor_t *visitor, bl_node_t **if_stmt)
{
  visitor->nesting++;
  call_visit(visitor, &bl_peek_stmt_if(*if_stmt)->test, BL_VISIT_EXPR);
  walk_block_content(visitor, &bl_peek_stmt_if(*if_stmt)->true_stmt);
  walk_block_content(visitor, &bl_peek_stmt_if(*if_stmt)->false_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_if_true(bl_visitor_t *visitor, bl_node_t **if_stmt)
{
  visitor->nesting++;
  walk_block_content(visitor, &bl_peek_stmt_if(*if_stmt)->true_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_if_false(bl_visitor_t *visitor, bl_node_t **if_stmt)
{
  visitor->nesting++;
  walk_block_content(visitor, &bl_peek_stmt_if(*if_stmt)->false_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_loop(bl_visitor_t *visitor, bl_node_t **stmt_loop)
{
  visitor->nesting++;
  call_visit(visitor, &bl_peek_stmt_loop(*stmt_loop)->test, BL_VISIT_EXPR);
  walk_block_content(visitor, &bl_peek_stmt_loop(*stmt_loop)->true_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_loop_body(bl_visitor_t *visitor, bl_node_t **stmt_loop)
{
  visitor->nesting++;
  walk_block_content(visitor, &bl_peek_stmt_loop(*stmt_loop)->true_stmt);
  visitor->nesting--;
}

void
bl_visitor_walk_break(bl_visitor_t *visitor, bl_node_t **stmt_break)
{
  // terminal
}

void
bl_visitor_walk_continue(bl_visitor_t *visitor, bl_node_t **stmt_continue)
{
  // terminal
}

void
bl_visitor_walk_return(bl_visitor_t *visitor, bl_node_t **stmt_return)
{
  visitor->nesting++;
  if (bl_peek_stmt_return(*stmt_return)->expr) {
    call_visit(visitor, &bl_peek_stmt_return(*stmt_return)->expr, BL_VISIT_EXPR);
  }
  visitor->nesting--;
}

void
bl_visitor_walk_struct_member(bl_visitor_t *visitor, bl_node_t **member)
{
  visitor->nesting++;
  bl_decl_struct_member_t *_member = bl_peek_decl_struct_member(*member);

  call_visit(visitor, &_member->type, BL_VISIT_TYPE);
  visitor->nesting--;
}

void
bl_visitor_walk_enum_variant(bl_visitor_t *visitor, bl_node_t **variant)
{
  visitor->nesting++;
  bl_decl_enum_variant_t *_variant = bl_peek_decl_enum_variant(*variant);

  if (_variant->expr) {
    call_visit(visitor, &_variant->expr, BL_VISIT_EXPR);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_load(bl_visitor_t *visitor, bl_node_t **pre_load)
{
  // terminal
}

void
bl_visitor_walk_link(bl_visitor_t *visitor, bl_node_t **pre_link)
{
  // terminal
}

void
bl_visitor_walk_using(bl_visitor_t *visitor, bl_node_t **using)
{
  // terminal
}
