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

static inline void
print_modif(int modif)
{
  if (modif & BL_MODIF_PUBLIC) {
    fprintf(stdout, BL_CYAN(" %s "), bl_sym_strings[BL_SYM_PUBLIC]);
  }

  if (modif & BL_MODIF_EXTERN) {
    fprintf(stdout, BL_CYAN(" %s"), bl_sym_strings[BL_SYM_EXTERN]);
  }
}

static inline void
print_path(BArray *path)
{
  if (!path)
    return;
  // bl_assert(path, "invalid path");
  const size_t c = bo_array_size(path);
  bl_node_t *  path_elem;
  for (size_t i = 0; i < c; i++) {
    path_elem = bo_array_at(path, i, bl_node_t *);
    fprintf(stdout, BL_CYAN("%s"), bl_peek_expr_path(path_elem)->id.str);
    if (i != c - 1)
      fprintf(stdout, BL_CYAN("::"));
  }
}

static void
visit_module(bl_visitor_t *visitor, bl_node_t *module)
{
  print_head("module", bl_peek_src(module), module, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), bl_peek_decl_module(module)->id.str);
  print_modif(bl_peek_decl_module(module)->modif);
  bl_visitor_walk_module(visitor, module);
}

static void
visit_func(bl_visitor_t *visitor, bl_node_t *func)
{
  print_head("function", bl_peek_src(func), func, visitor->nesting);
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), _func->id.str);
  print_modif(_func->modif);

  bl_visitor_walk_func(visitor, func);
}

static void
visit_type(bl_visitor_t *visitor, bl_node_t *type)
{
  print_head("type", bl_peek_src(type), type, visitor->nesting);
  if (bl_node_is(type, BL_TYPE_REF)) {
    print_path(bl_peek_type_ref(type)->path);
    fprintf(stdout, " -> " BL_YELLOW("%p"), bl_peek_type_ref(type)->ref);
  } else {
    fprintf(stdout, "fundamental: " BL_MAGENTA("%s"),
            bl_fund_type_strings[bl_peek_type_fund(type)->type]);
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
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), _strct->id.str);
  print_modif(_strct->modif);
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  print_head("enum", bl_peek_src(enm), enm, visitor->nesting);
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), _enm->id.str);
  print_modif(_enm->modif);
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
  switch (bl_node_code(expr)) {
  case BL_EXPR_CONST: {
    print_head("const", bl_peek_src(expr), expr, visitor->nesting);
    break;
  }
  case BL_EXPR_BINOP:
    print_head("binop", bl_peek_src(expr), expr, visitor->nesting);
    fprintf(stdout, " operation: " BL_YELLOW("'%s'"), bl_sym_strings[bl_peek_expr_binop(expr)->op]);
    break;
  case BL_EXPR_VAR_REF:
    print_head("var_ref", bl_peek_src(expr), expr, visitor->nesting);
    print_path(bl_peek_expr_var_ref(expr)->path);
    fprintf(stdout, " -> " BL_YELLOW("%p"), bl_peek_expr_var_ref(expr)->ref);
    break;
  case BL_EXPR_CALL:
    print_head("call", bl_peek_src(expr), expr, visitor->nesting);
    print_path(bl_peek_expr_call(expr)->path);
    fprintf(stdout, " -> " BL_YELLOW("%p"), bl_peek_expr_call(expr)->ref);
    break;
  default:
    bl_abort("invalid expression");
  }
  bl_visitor_walk_expr(visitor, expr);
}

static void
visit_if(bl_visitor_t *visitor, bl_node_t *if_stmt)
{
  print_head("if", bl_peek_src(if_stmt), if_stmt, visitor->nesting);
  bl_visitor_walk_if(visitor, if_stmt);
}

static void
visit_loop(bl_visitor_t *visitor, bl_node_t *stmt_loop)
{
  print_head("loop", bl_peek_src(stmt_loop), stmt_loop, visitor->nesting);
  bl_visitor_walk_loop(visitor, stmt_loop);
}

static void
visit_while(bl_visitor_t *visitor, bl_node_t *stmt_while)
{
  print_head("while", bl_peek_src(stmt_while), stmt_while, visitor->nesting);
  bl_visitor_walk_while(visitor, stmt_while);
}

static void
visit_break(bl_visitor_t *visitor, bl_node_t *stmt_break)
{
  print_head("break", bl_peek_src(stmt_break), stmt_break, visitor->nesting);
  bl_visitor_walk_break(visitor, stmt_break);
}

static void
visit_continue(bl_visitor_t *visitor, bl_node_t *stmt_continue)
{
  print_head("continue", bl_peek_src(stmt_continue), stmt_continue, visitor->nesting);
  bl_visitor_walk_continue(visitor, stmt_continue);
}

static void
visit_return(bl_visitor_t *visitor, bl_node_t *stmt_return)
{
  print_head("return", bl_peek_src(stmt_return), stmt_return, visitor->nesting);
  bl_visitor_walk_return(visitor, stmt_return);
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
    bl_visitor_add(&visitor, visit_if, BL_VISIT_IF);
    bl_visitor_add(&visitor, visit_loop, BL_VISIT_LOOP);
    bl_visitor_add(&visitor, visit_while, BL_VISIT_WHILE);
    bl_visitor_add(&visitor, visit_break, BL_VISIT_BREAK);
    bl_visitor_add(&visitor, visit_continue, BL_VISIT_CONTINUE);
    bl_visitor_add(&visitor, visit_return, BL_VISIT_RETURN);

    bl_visitor_walk_module(&visitor, unit->ast.root);
  }

  fprintf(stdout, "\n\n");

  return BL_NO_ERR;
}
