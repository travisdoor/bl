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
visit_module(bl_visitor_t *visitor, bl_module_t *module, bl_src_t *src)
{
  print_head("module", src, module, visitor->nesting);
  bl_visitor_walk_module(visitor, module);
}

static void
visit_item(bl_visitor_t *visitor, bl_item_t *item, bl_src_t *src)
{
  print_head("item", src, item, visitor->nesting);
  fprintf(stdout, " name: " BL_YELLOW("'%s'"), item->id.str);
  bl_visitor_walk_item(visitor, item);
}

static void
visit_func_decl(bl_visitor_t *visitor, bl_func_decl_t *func_decl, bl_src_t *src)
{
  print_head("func_decl", src, func_decl, visitor->nesting);
  bl_visitor_walk_func_decl(visitor, func_decl);
}

static void
visit_block(bl_visitor_t *visitor, bl_block_t *block, bl_src_t *src)
{
  print_head("block", src, block, visitor->nesting);
  bl_visitor_walk_block(visitor, block);
}

static void
visit_enum(bl_visitor_t *visitor, bl_enum_decl_t *enum_decl, bl_src_t *src)
{
  print_head("enum", src, enum_decl, visitor->nesting);
  bl_visitor_walk_enum_decl(visitor, enum_decl);
}

static void
visit_struct(bl_visitor_t *visitor, bl_struct_decl_t *struct_decl, bl_src_t *src)
{
  print_head("struct", src, struct_decl, visitor->nesting);
  bl_visitor_walk_struct_decl(visitor, struct_decl);
}

static void
visit_stmt(bl_visitor_t *visitor, bl_stmt_t *stmt, bl_src_t *src)
{
  print_head("stmt", src, stmt, visitor->nesting);
  bl_visitor_walk_stmt(visitor, stmt);
}

static void
visit_decl(bl_visitor_t *visitor, bl_decl_t *decl, bl_src_t *src)
{
  print_head("var", src, decl, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), decl->id.str);
  bl_visitor_walk_decl(visitor, decl);
}

static void
visit_expr(bl_visitor_t *visitor, bl_expr_t *expr, bl_src_t *src)
{
  print_head("expr", src, expr, visitor->nesting);
  bl_visitor_walk_expr(visitor, expr);
}

static void
visit_path(bl_visitor_t *visitor, bl_path_t *path, bl_src_t *src)
{
  print_head("path", src, path, visitor->nesting);
  fprintf(stdout, "id: " BL_YELLOW("'%s' -> %p"), path->id.str, path->ref);
  bl_visitor_walk_path(visitor, path);
}

static void
visit_const_expr(bl_visitor_t *visitor, bl_const_expr_t *expr, bl_src_t *src)
{
  print_head("constant", src, expr, visitor->nesting);

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

  bl_visitor_walk_const_expr(visitor, expr);
}

static void
visit_binop(bl_visitor_t *visitor, bl_binop_t *binop, bl_src_t *src)
{
  print_head("binop", src, binop, visitor->nesting);
  bl_visitor_walk_binop(visitor, binop);
}

static void
visit_call(bl_visitor_t *visitor, bl_call_t *call, bl_src_t *src)
{
  print_head("call", src, call, visitor->nesting);
  fprintf(stdout, "ref: " BL_YELLOW("'%s' -> %p"), call->id.str, call->callee);
  bl_visitor_walk_call(visitor, call);
}

static void
visit_var_ref(bl_visitor_t *visitor, bl_var_ref_t *ref, bl_src_t *src)
{
  print_head("ref", src, ref, visitor->nesting);
  fprintf(stdout, "ref: " BL_YELLOW("'%s' -> %p"), ref->id.str, ref->ref);
  bl_visitor_walk_var_ref(visitor, ref);
}

static void
visit_type(bl_visitor_t *visitor, bl_type_t *type, bl_src_t *src)
{
  print_head("type", src, type, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), type->id.str);
  bl_visitor_walk_type(visitor, type);
}

static void
visit_arg(bl_visitor_t *visitor, bl_arg_t *arg, bl_src_t *src)
{
  print_head("arg", src, arg, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), arg->id.str);
  bl_visitor_walk_arg(visitor, arg);
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
    bl_visitor_add(&visitor, visit_item, BL_VISIT_ITEM);
    bl_visitor_add(&visitor, visit_func_decl, BL_VISIT_FUNC_DECL);
    bl_visitor_add(&visitor, visit_block, BL_VISIT_BLOCK);
    bl_visitor_add(&visitor, visit_enum, BL_VISIT_ENUM_DECL);
    bl_visitor_add(&visitor, visit_struct, BL_VISIT_STRUCT_DECL);
    bl_visitor_add(&visitor, visit_stmt, BL_VISIT_STMT);
    bl_visitor_add(&visitor, visit_decl, BL_VISIT_DECL);
    bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
    bl_visitor_add(&visitor, visit_const_expr, BL_VISIT_CONST);
    bl_visitor_add(&visitor, visit_call, BL_VISIT_CALL);
    bl_visitor_add(&visitor, visit_binop, BL_VISIT_BINOP);
    bl_visitor_add(&visitor, visit_var_ref, BL_VISIT_VAR_REF);
    bl_visitor_add(&visitor, visit_arg, BL_VISIT_ARG);
    bl_visitor_add(&visitor, visit_type, BL_VISIT_TYPE);
    bl_visitor_add(&visitor, visit_path, BL_VISIT_PATH);

    bl_visitor_walk_root(&visitor, (bl_node_t *)unit->ast.root);
  }

  fprintf(stdout, "\n\n");

  return BL_NO_ERR;
}
