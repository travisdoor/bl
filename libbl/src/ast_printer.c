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

static void
print_node(bl_node_t *node,
           int pad)
{
  if (!node)
    return;

  fprintf(stdout,
          "\n%*s" BL_GREEN("%s ") BL_MAGENTA("<%d:%d> "),
          pad,
          "",
          bl_node_to_str(node),
          node->line,
          node->col);

  int c = 0;
  bl_node_t *child = NULL;

  switch (node->type) {
    case BL_NODE_GLOBAL_STMT:
      c = bl_node_glob_stmt_get_children_count(node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_glob_stmt_get_child(node, i);
        print_node(child, pad);
      }
      break;
    case BL_NODE_FUNC_DECL:
      fprintf(stdout, "type: " BL_YELLOW("%s"), node->value.decl.type.name);
      fprintf(stdout, ", name: " BL_YELLOW("%s"), node->value.decl.ident.name);
      c = bl_node_func_decl_get_param_count(node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_func_decl_get_param(node, i);
        print_node(child, pad);
      }
      print_node(node->value.func_decl.cmp_stmt, pad);
      break;
    case BL_NODE_CALL_EXPR:
      c = bl_node_call_expr_get_arg_count(node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_call_expr_get_arg(node, i);
        print_node(child, pad);
      }
      break;
    case BL_NODE_STRUCT_DECL:
      fprintf(stdout, "name: " BL_YELLOW("%s"), node->value.decl.type.name);
      c = bl_node_struct_decl_get_member_count(node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_struct_decl_get_member(node, i);
        print_node(child, pad);
      }
      break;
    case BL_NODE_LOOP_STMT:
      pad += 2;
      print_node(node->value.loop_stmt.cmp_stmt, pad);
      print_node(node->value.loop_stmt.expr, pad);
      break;
    case BL_NODE_BINOP:
      pad += 2;
      fprintf(stdout, "op: " BL_YELLOW("%s"), bl_sym_strings[node->value.binop.operator]);
      print_node(node->value.binop.lhs, pad);
      print_node(node->value.binop.rhs, pad);
      break;
    case BL_NODE_IF_STMT:
      pad += 2;
      print_node(node->value.if_stmt.expr, pad);
      print_node(node->value.if_stmt.then_stmt, pad);
      print_node(node->value.if_stmt.else_stmt, pad);
      print_node(node->value.if_stmt.else_if_stmt, pad);
      break;
    case BL_NODE_DECL_REF_EXPR:
      fprintf(stdout, "name: " BL_YELLOW("%s"), node->value.decl_ref_expr.ident.name);
      break;
    case BL_NODE_CONST_EXPR: {
      bl_node_const_expr_t *const_expr = &node->value.const_expr;
      switch (const_expr->type) {
        case BL_CONST_ULONG:
        case BL_CONST_LONG:
        case BL_CONST_INT:
        case BL_CONST_BOOL:
          fprintf(stdout, "value: " BL_YELLOW("%lu"), const_expr->value.as_ulong);
          break;
        case BL_CONST_STRING:
          fprintf(stdout, "value: " BL_YELLOW("%s"), const_expr->value.as_string);
          break;
        case BL_CONST_CHAR:
          fprintf(stdout, "value: " BL_YELLOW("%c"), const_expr->value.as_char);
          break;
        case BL_CONST_DOUBLE:
          fprintf(stdout, "value: " BL_YELLOW("%f"), const_expr->value.as_double);
          break;
        case BL_CONST_FLOAT:
          fprintf(stdout, "value: " BL_YELLOW("%f"), const_expr->value.as_float);
          break;
      }
      break;
    }
    case BL_NODE_MEMBER_EXPR: {
      fprintf(stdout, "name: " BL_YELLOW("%s"), node->value.member_expr.ident.name);

      bl_node_t *member = node->value.member_expr.member;
      if (member) {
        fprintf(stdout, ", member: " BL_YELLOW("%s"), member->value.var_decl.base.ident.name);
      } else {
        fprintf(stdout, ", member: " BL_RED("UNKNOWN"));
      }
      pad += 2;
      print_node(node->value.member_expr.next, pad);
      break;
    }
    case BL_NODE_CMP_STMT:
      c = bl_node_cmp_stmt_get_children_count(node);
      pad += 2;
      for (int i = 0; i < c; i++) {
        child = bl_node_cmp_stmt_get_child(node, i);
        print_node(child, pad);
      }
      break;
    case BL_NODE_RETURN_STMT:
      pad += 2;
      print_node(node->value.return_stmt.expr, pad);
      break;
    case BL_NODE_VAR_DECL:
      fprintf(stdout, "order: " BL_YELLOW("%d "), node->value.var_decl.order);
    case BL_NODE_PARAM_VAR_DECL:
      fprintf(stdout, "type: " BL_YELLOW("%s"), node->value.decl.type.name);
      fprintf(stdout, ", name: " BL_YELLOW("%s"), node->value.decl.ident.name);
      pad += 2;
      print_node(node->value.var_decl.expr, pad);
    default:
      break;
  }
}

bl_error_e
bl_ast_printer_run(bl_unit_t *unit)
{
  print_node(unit->ast.root, 0);
  fprintf(stdout, "\n\n");
  return BL_NO_ERR;
}

