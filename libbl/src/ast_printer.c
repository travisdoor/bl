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

#define print_head(name, src, ptr, pad)                                                            \
  fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<%d:%d>") BL_YELLOW(" %p "), (pad)*2, "",       \
          (name), (src)->line, (src)->col, (ptr));

static void
visit_module(bl_visitor_t *visitor, bl_node_t *module)
{
  print_head("module", bl_peek_src(module), module, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_module(module)->id.str);
  bl_visitor_walk_module(visitor, module);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  print_head("function", bl_peek_src(func), func, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_func(func)->id.str);
  bl_visitor_walk_func(visitor, func);
}

static void
visit_type(bl_visitor_t *visitor, bl_node_t *type)
{
  print_head("type", bl_peek_src(type), type, visitor->nesting);
  if (bl_peek_type(type)->type_variant == BL_TYPE_REF) {
    fprintf(stdout, "name: " BL_YELLOW("'%s' -> %p"), bl_peek_type_ref(type)->id.str,
            bl_peek_type_ref(type)->ref);
  } else {
    fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_fund_type_strings[bl_peek_type_fund(type)->type]);
  }
  bl_visitor_walk_type(visitor, type);
}

static void
visit_arg(bl_visitor_t *visitor, bl_node_t *arg)
{
  print_head("arg", bl_peek_src(arg), arg, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_arg(arg)->id.str);
  bl_visitor_walk_arg(visitor, arg);
}

static void
visit_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  print_head("struct", bl_peek_src(strct), strct, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_struct(strct)->id.str);
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  print_head("enum", bl_peek_src(enm), enm, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_enum(enm)->id.str);
  bl_visitor_walk_enum(visitor, enm);
}

static void
visit_block(bl_visitor_t *visitor, bl_node_t *block)
{
  print_head("block", bl_peek_src(block), block, visitor->nesting);
  bl_visitor_walk_block(visitor, block);
}

static void
visit_var(bl_visitor_t *visitor, bl_node_t *var)
{
  print_head("variable", bl_peek_src(var), var, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_var(var)->id.str);
  bl_visitor_walk_var(visitor, var);
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  print_head("expr", bl_peek_src(expr), expr, visitor->nesting);
  switch (bl_peek_expr(expr)->expr_variant) {
  case BL_EXPR_CONST:
    fprintf(stdout, BL_CYAN("<const>"));
    break;
  case BL_EXPR_BINOP:
    fprintf(stdout, BL_CYAN("<binop>"));
    break;
  case BL_EXPR_VAR_REF:
    fprintf(stdout, BL_CYAN("<ref>") " name: " BL_YELLOW("'%s' -> %p"),
            bl_peek_expr_var_ref(expr)->id.str, bl_peek_expr_var_ref(expr)->ref);
    break;
  case BL_EXPR_CALL:
    fprintf(stdout, BL_CYAN("<call>") " name: " BL_YELLOW("'%s' -> %p"), bl_peek_expr_call(expr)->id.str,
            bl_peek_expr_call(expr)->ref);
    break;
  case BL_EXPR_PATH:
    fprintf(stdout, BL_CYAN("<path>") " name: " BL_YELLOW("'%s' -> %p"), bl_peek_expr_path(expr)->id.str,
            bl_peek_expr_path(expr)->ref);
    break;
  default:
    bl_abort("invalid expression");
  }
  bl_visitor_walk_expr(visitor, expr);
}

bl_error_e
bl_ast_printer_run(bl_assembly_t *assembly)
{
  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);

    fprintf(stdout, "\nAST for unit " BL_YELLOW("%s") ":", unit->name);

    bl_visitor_t visitor;
    bl_visitor_init(&visitor, NULL);

    bl_visitor_add(&visitor, visit_module, BL_VISIT_MODULE);
    bl_visitor_add(&visitor, visit_func, BL_VISIT_FUNC);
    bl_visitor_add(&visitor, visit_type, BL_VISIT_TYPE);
    bl_visitor_add(&visitor, visit_arg, BL_VISIT_ARG);
    bl_visitor_add(&visitor, visit_struct, BL_VISIT_STRUCT);
    bl_visitor_add(&visitor, visit_enum, BL_VISIT_ENUM);
    bl_visitor_add(&visitor, visit_block, BL_VISIT_BLOCK);
    bl_visitor_add(&visitor, visit_var, BL_VISIT_VAR);
    bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);

    bl_visitor_walk_module(&visitor, unit->ast.root);
  }

  fprintf(stdout, "\n\n");

  return BL_NO_ERR;
}
