//*****************************************************************************
// blc
//
// File:   ast_printer.c
// Author: Martin Dorazil
// Date:   04/02/2018
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

#include <stdio.h>
#include "stages_impl.h"
#include "common_impl.h"
#include "ast/ast2_impl.h"
#include "ast/visitor_impl.h"

static void
print_node(bl_node_t *node, int pad)
{
  if (!node)
    return;

  fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<%d:%d> ") BL_YELLOW("%p "), pad, "",
          bl_ast2_node_to_str(node), node->src.line, node->src.col, node);

  switch (node->t) {
  case BL_NODE_MODULE: {
    pad += 2;
    bl_item_t *  item;
    bl_module_t *module = (bl_module_t *)node;
    const size_t c      = bl_ast_module_item_count(module);
    for (size_t i = 0; i < c; i++) {
      item = bl_ast_module_get_item(module, i);
      print_node((bl_node_t *)item, pad);
    }

    break;
  }
  case BL_NODE_FUNC_DECL: {
    pad += 2;
    bl_arg_t *      arg;
    bl_func_decl_t *func = (bl_func_decl_t *)node;
    const size_t    c    = bl_ast_func_arg_count(func);
    for (size_t i = 0; i < c; i++) {
      arg = bl_ast_func_get_arg(func, i);
      print_node((bl_node_t *)arg, pad);
    }

    print_node((bl_node_t *)func->ret, pad);

    break;
  }
  case BL_NODE_BINOP: {
    pad += 2;
    bl_binop_t *binop = (bl_binop_t *)node;
    fprintf(stdout, "op: " BL_YELLOW("%s"), bl_sym_strings[binop->op]);
    print_node((bl_node_t *)binop->lhs, pad);
    print_node((bl_node_t *)binop->rhs, pad);
    break;
  }
  case BL_NODE_CALL: {
    pad += 2;
    bl_call_t *call = (bl_call_t *)node;
    fprintf(stdout, "callee: " BL_YELLOW("%s:%p"), call->id.str, call->callee);

    bl_expr_t *  arg;
    const size_t c = bl_ast_call_arg_count(call);
    for (size_t i = 0; i < c; i++) {
      arg = bl_ast_call_get_arg(call, i);
      print_node((bl_node_t *)arg, pad);
    }

    break;
  }
  case BL_NODE_VAR_REF: {
    bl_var_ref_t *var_ref = (bl_var_ref_t *)node;
    fprintf(stdout, "ref: " BL_YELLOW("%s:%p"), var_ref->id.str, var_ref->ref);
    break;
  }
  case BL_NODE_EXPR: {
    pad += 2;
    bl_expr_t *expr = (bl_expr_t *)node;

    switch (expr->t) {
    case BL_EXPR_CONST:
    case BL_EXPR_BINOP:
    case BL_EXPR_NESTED:
    case BL_EXPR_CALL:
    case BL_EXPR_VAR_REF:
      print_node((bl_node_t *)expr->expr.cnst, pad);
      break;
      print_node((bl_node_t *)expr->expr.binop, pad);
      break;
    default:
      break;
    }

    break;
  }
  case BL_NODE_CONST_EXPR: {
    pad += 2;
    bl_const_expr_t *expr = (bl_const_expr_t *)node;

    switch (expr->type->type.fund) {
    case BL_FTYPE_I8:
    case BL_FTYPE_I32:
    case BL_FTYPE_I64:
    case BL_FTYPE_U8:
    case BL_FTYPE_U32:
    case BL_FTYPE_U64:
      fprintf(stdout, "value: " BL_MAGENTA("%lld"), expr->value.s);
      break;
    case BL_FTYPE_F32:
    case BL_FTYPE_F64:
      fprintf(stdout, "value: " BL_MAGENTA("%f"), expr->value.f);
      break;
    case BL_FTYPE_CHAR:
      fprintf(stdout, "value: " BL_MAGENTA("%c"), expr->value.c);
      break;
    case BL_FTYPE_STRING:
      fprintf(stdout, "value: " BL_MAGENTA("%s"), expr->value.str);
      break;
    case BL_FTYPE_BOOL:
      if (expr->value.b == true)
        fprintf(stdout, "value: " BL_MAGENTA("true"));
      else
        fprintf(stdout, "value: " BL_MAGENTA("false"));
      break;
    default:
      break;
    }

    print_node((bl_node_t *)expr->type, pad);

    break;
  }
  case BL_NODE_DECL: {
    pad += 2;
    bl_decl_t *decl = (bl_decl_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), decl->id.str);
    print_node((bl_node_t *)decl->type, pad);
    print_node((bl_node_t *)decl->init_expr, pad);
    break;
  }
  case BL_NODE_BLOCK: {
    pad += 2;
    bl_stmt_t *  stmt;
    bl_block_t * block = (bl_block_t *)node;
    const size_t c     = bl_ast_block_stmt_count(block);
    for (size_t i = 0; i < c; i++) {
      stmt = bl_ast_block_get_stmt(block, i);
      print_node((bl_node_t *)stmt, pad);
    }
    break;
  }
  case BL_NODE_STMT: {
    pad += 2;
    bl_stmt_t *stmt = (bl_stmt_t *)node;
    switch (stmt->t) {
    case BL_STMT_DECL:
      print_node((bl_node_t *)stmt->stmt.decl, pad);
      break;
    case BL_STMT_EXPR:
      print_node((bl_node_t *)stmt->stmt.expr, pad);
      break;
    case BL_STMT_BLOCK:
      print_node((bl_node_t *)stmt->stmt.block, pad);
      break;
    default:
      break;
    }
    break;
  }
  case BL_NODE_TYPE: {
    bl_type_t *type = (bl_type_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), type->id.str);
    break;
  }
  case BL_NODE_ARG: {
    pad += 2;
    bl_arg_t *arg = (bl_arg_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), arg->id.str);
    print_node((bl_node_t *)arg->type, pad);
    break;
  }
  case BL_NODE_ITEM: {
    pad += 2;
    bl_item_t *item = (bl_item_t *)node;
    fprintf(stdout, "name: " BL_YELLOW("%s"), item->id.str);

    switch (item->t) {
    case BL_ITEM_FUNC:
      print_node((bl_node_t *)item->node.func.func_decl, pad);
      print_node((bl_node_t *)item->node.func.block, pad);
      break;
    case BL_ITEM_MODULE:
      break;
    default:
      break;
    }
    break;
  }
  default:
    break;
  }
}

static void
visit_module(bl_visitor_t *visitor, bl_module_t *module)
{
  fprintf(stdout, "\nmodule");
  bl_visitor_walk_module(visitor, module);
}

static void
visit_item(bl_visitor_t *visitor, bl_item_t *item)
{
  fprintf(stdout, "\nitem: %s", item->id.str);
  bl_visitor_walk_item(visitor, item);
}

bl_error_e
bl_ast_printer_run(bl_assembly_t *assembly)
{
  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    print_node((bl_node_t *)unit->ast.root, 0);

    bl_visitor_t visitor;

    bl_visitor_add(&visitor, visit_module, BL_VISIT_MODULE);
    bl_visitor_add(&visitor, visit_item, BL_VISIT_ITEM);

    bl_visitor_walk_root(&visitor, (bl_node_t *)unit->ast.root);
  }

  fprintf(stdout, "\n\n");

  return BL_NO_ERR;
}
