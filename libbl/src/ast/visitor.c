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

static void
visit_const_expr(bl_visitor_t *visitor, bl_const_expr_t *expr, bl_src_t *src)
{
  bl_visitor_walk_const_expr(visitor, expr);
}

static void
visit_binop(bl_visitor_t *visitor, bl_binop_t *binop, bl_src_t *src)
{
  bl_visitor_walk_binop(visitor, binop);
}

static void
visit_call(bl_visitor_t *visitor, bl_call_t *call, bl_src_t *src)
{
  bl_visitor_walk_call(visitor, call);
}

static void
visit_var_ref(bl_visitor_t *visitor, bl_var_ref_t *ref, bl_src_t *src)
{
  bl_visitor_walk_var_ref(visitor, ref);
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
visit_path(bl_visitor_t *visitor, bl_path_t *path, bl_src_t *src)
{
  bl_visitor_walk_path(visitor, path);
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
  visitor->visitors[BL_VISIT_CONST]       = visit_const_expr;
  visitor->visitors[BL_VISIT_BINOP]       = visit_binop;
  visitor->visitors[BL_VISIT_CALL]        = visit_call;
  visitor->visitors[BL_VISIT_VAR_REF]     = visit_var_ref;
  visitor->visitors[BL_VISIT_ARG]         = visit_arg;
  visitor->visitors[BL_VISIT_TYPE]        = visit_type;
  visitor->visitors[BL_VISIT_PATH]        = visit_path;
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
  default:
    bl_abort("invalid item");
  }

  visitor->nesting--;
}

void
bl_visitor_walk_func_decl(bl_visitor_t *visitor, bl_func_decl_t *func_decl)
{
  visitor->nesting++;

  bl_visit_arg_f  va  = visitor->visitors[BL_VISIT_ARG];
  bl_visit_type_f vt  = visitor->visitors[BL_VISIT_TYPE];
  const size_t    c   = bl_ast_func_arg_count(func_decl);
  bl_arg_t *      arg = NULL;

  for (size_t i = 0; i < c; i++) {
    arg = bl_ast_func_get_arg(func_decl, i);
    va(visitor, arg, &arg->base_.src);
  }

  vt(visitor, func_decl->ret, &func_decl->ret->base_.src);

  visitor->nesting--;
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
  default:
    bl_abort("invalid stmt");
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

  bl_visit_type_f vt = visitor->visitors[BL_VISIT_TYPE];
  vt(visitor, decl->type, &decl->type->base_.src);
  visitor->nesting--;
}

void
bl_visitor_walk_expr(bl_visitor_t *visitor, bl_expr_t *expr)
{
  visitor->nesting++;

  switch (expr->t) {
  case BL_EXPR_CALL: {
    bl_visit_call_f v = visitor->visitors[BL_VISIT_CALL];
    v(visitor, expr->expr.call, &expr->expr.call->base_.src);
    break;
  }

  case BL_EXPR_VAR_REF: {
    bl_visit_var_ref_f v = visitor->visitors[BL_VISIT_VAR_REF];
    v(visitor, expr->expr.var_ref, &expr->expr.var_ref->base_.src);
    break;
  }

  case BL_EXPR_BINOP: {
    bl_visit_const_binop_f v = visitor->visitors[BL_VISIT_BINOP];
    v(visitor, expr->expr.binop, &expr->expr.binop->base_.src);
    break;
  }

  case BL_EXPR_CONST: {
    bl_visit_const_expr_f v = visitor->visitors[BL_VISIT_CONST];
    v(visitor, expr->expr.cnst, &expr->expr.cnst->base_.src);
    break;
  }

  case BL_EXPR_NESTED: {
    bl_visit_expr_f v = visitor->visitors[BL_VISIT_EXPR];
    v(visitor, expr->expr.nested, &expr->expr.nested->base_.src);
    break;
  }

  case BL_EXPR_PATH: {
    bl_visit_path_f v = visitor->visitors[BL_VISIT_PATH];
    v(visitor, expr->expr.path, &expr->expr.path->base_.src);
    break;
  }

  default:
    bl_abort("invalid expression");
  }

  visitor->nesting--;
}

void
bl_visitor_walk_const_expr(bl_visitor_t *visitor, bl_const_expr_t *expr)
{
  visitor->nesting++;
  bl_visit_type_f vt = visitor->visitors[BL_VISIT_TYPE];
  vt(visitor, expr->type, &expr->type->base_.src);
  visitor->nesting--;
}

void
bl_visitor_walk_binop(bl_visitor_t *visitor, bl_binop_t *binop)
{
  visitor->nesting++;
  bl_visit_expr_f v = visitor->visitors[BL_VISIT_EXPR];
  v(visitor, binop->lhs, &binop->lhs->base_.src);
  v(visitor, binop->rhs, &binop->rhs->base_.src);
  visitor->nesting--;
}

void
bl_visitor_walk_call(bl_visitor_t *visitor, bl_call_t *call)
{
  visitor->nesting++;

  bl_visit_expr_f v    = visitor->visitors[BL_VISIT_EXPR];
  const size_t    c    = bl_ast_call_arg_count(call);
  bl_expr_t *     expr = NULL;

  for (size_t i = 0; i < c; i++) {
    expr = bl_ast_call_get_arg(call, i);
    v(visitor, expr, &expr->base_.src);
  }

  visitor->nesting--;
}

void
bl_visitor_walk_var_ref(bl_visitor_t *visitor, bl_var_ref_t *ref)
{
}

void
bl_visitor_walk_arg(bl_visitor_t *visitor, bl_arg_t *arg)
{
  visitor->nesting++;
  if (arg->type) {
    bl_visit_type_f vt = visitor->visitors[BL_VISIT_TYPE];
    vt(visitor, arg->type, &arg->type->base_.src);
  }
  visitor->nesting--;
}

void
bl_visitor_walk_type(bl_visitor_t *visitor, bl_type_t *type)
{
}

void
bl_visitor_walk_path(bl_visitor_t *visitor, bl_path_t *path)
{
  visitor->nesting++;

  switch (path->t) {
  case BL_PATH_PATH:{
    bl_visit_path_f vt = visitor->visitors[BL_VISIT_PATH];
    vt(visitor, path->next.path, &path->next.path->base_.src);
    break;
  }

  case BL_PATH_CALL:{
    bl_visit_call_f vt = visitor->visitors[BL_VISIT_CALL];
    vt(visitor, path->next.call, &path->next.call->base_.src);
    break;
  }

  case BL_PATH_VAR_REF:{
    bl_visit_var_ref_f vt = visitor->visitors[BL_VISIT_VAR_REF];
    vt(visitor, path->next.var_ref, &path->next.var_ref->base_.src);
    break;
  }

  default:
    bl_abort("invalid path");
  }

  visitor->nesting--;
}
