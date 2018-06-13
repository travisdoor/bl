//************************************************************************************************
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
//************************************************************************************************

#include <stdio.h>
#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"
#include "visitor_impl.h"

static inline void
print_head(const char *name, bl_src_t *src, void *ptr, int pad)
{
  if (src)
    fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<%d:%d>") BL_YELLOW(" %p "), pad * 2, "", name,
            src->line, src->col, ptr);
  else
    fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<generated>") BL_YELLOW(" %p "), pad * 2, "",
            name, ptr);
}

static inline void
print_modif(int modif)
{
  if (modif & BL_MODIF_PUBLIC) {
    fprintf(stdout, BL_CYAN(" %s"), bl_sym_strings[BL_SYM_PUBLIC]);
  }

  if (modif & BL_MODIF_EXTERN) {
    fprintf(stdout, BL_CYAN(" %s"), bl_sym_strings[BL_SYM_EXTERN]);
  }

  if (modif & BL_MODIF_EXPORT) {
    fprintf(stdout, BL_CYAN(" %s"), bl_sym_strings[BL_SYM_EXPORT]);
  }

  if (modif & BL_MODIF_UTEST) {
    fprintf(stdout, BL_CYAN(" %s"), bl_sym_strings[BL_SYM_TEST]);
  }

  if (modif & BL_MODIF_ENTRY) {
    fprintf(stdout, BL_CYAN(" %s"), "entry");
  }
}

static inline void
print_path(bl_node_t *path)
{
  while (path) {
    fprintf(stdout, BL_CYAN("%s"), bl_peek_path_elem(path)->id.str);
    path = path->next;

    if (path)
      fprintf(stdout, BL_CYAN("::"));
  }
}

static void
visit_using(bl_visitor_t *visitor, bl_node_t *using)
{
  print_head("using", bl_peek_src(using), using, visitor->nesting);
  print_path(bl_peek_stmt_using(using)->path);
  fprintf(stdout, " -> " BL_YELLOW("%p"), bl_peek_stmt_using(using)->ref);
}

static void
visit_load(bl_visitor_t *visitor, bl_node_t *load)
{
  print_head("load", bl_peek_src(load), load, visitor->nesting);
  fprintf(stdout, "path: " BL_YELLOW("'%s'"), bl_peek_pre_load(load)->filepath);
  bl_visitor_walk_load(visitor, load);
}

static void
visit_link(bl_visitor_t *visitor, bl_node_t *link)
{
  print_head("link", bl_peek_src(link), link, visitor->nesting);
  fprintf(stdout, "path: " BL_YELLOW("'%s'"), bl_peek_pre_link(link)->lib);
  bl_visitor_walk_link(visitor, link);
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
  fprintf(stdout, "name: " BL_YELLOW("'%s'") " usage: %d", _func->id.str, _func->used);
  print_modif(_func->modif);

  bl_visitor_walk_func(visitor, func);
}

static void
visit_type(bl_visitor_t *visitor, bl_node_t *type)
{
  print_head("type", bl_peek_src(type), type, visitor->nesting);
  if (bl_node_is(type, BL_TYPE_REF)) {
    bl_type_ref_t *_type = bl_peek_type_ref(type);
    for (int i = 0; i < _type->is_ptr; ++i)
      fprintf(stdout, BL_CYAN("*"));
    print_path(_type->path);
    fprintf(stdout, " -> " BL_YELLOW("%p"), _type->ref);
    fprintf(stdout, BL_CYAN("[%p]"), _type->dim);
  } else {
    bl_type_fund_t *_type = bl_peek_type_fund(type);
    fprintf(stdout, "fundamental: ");
    for (int i = 0; i < _type->is_ptr; ++i)
      fprintf(stdout, BL_CYAN("*"));

    fprintf(stdout, BL_CYAN("%s"), bl_fund_type_strings[_type->type]);
    fprintf(stdout, BL_CYAN("[%p]"), _type->dim);
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
  fprintf(stdout, "name: " BL_YELLOW("'%s'") " used: %d", _strct->id.str, _strct->used);
  print_modif(_strct->modif);
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  print_head("enum", bl_peek_src(enm), enm, visitor->nesting);
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);
  fprintf(stdout, "name: " BL_YELLOW("'%s'") ", used: %d", _enm->id.str, _enm->used);
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
visit_mut(bl_visitor_t *visitor, bl_node_t *mut)
{
  bl_decl_mut_t *_mut = bl_peek_decl_mut(mut);
  print_head("variable", bl_peek_src(mut), mut, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'") " used: %d", _mut->id.str, _mut->used);
  print_modif(_mut->modif);
  bl_visitor_walk_mut(visitor, mut);
}

static void
visit_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  bl_decl_const_t *_cnst = bl_peek_decl_const(cnst);
  print_head("constant", bl_peek_src(cnst), cnst, visitor->nesting);

  fprintf(stdout, "name: " BL_YELLOW("'%s'") " used: %d", _cnst->id.str, _cnst->used);
  bl_visitor_walk_const(visitor, cnst);
}

static void
visit_struct_member(bl_visitor_t *visitor, bl_node_t *member)
{
  print_head("member", bl_peek_src(member), member, visitor->nesting);
  bl_decl_struct_member_t *_member = bl_peek_decl_struct_member(member);
  fprintf(stdout, "name: " BL_YELLOW("'%s'") " order: %d", _member->id.str, _member->order);
  print_modif(_member->modif);
  bl_visitor_walk_struct_member(visitor, member);
}

static void
visit_enum_variant(bl_visitor_t *visitor, bl_node_t *variant)
{
  print_head("variant", bl_peek_src(variant), variant, visitor->nesting);
  bl_decl_enum_variant_t *_variant = bl_peek_decl_enum_variant(variant);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), _variant->id.str);
  bl_visitor_walk_enum_variant(visitor, variant);
}

static void
print_const_expr(bl_expr_const_t *expr)
{
  bl_type_kind_e kind = bl_type_get_kind(expr->type);
  switch (kind) {
  case BL_SINT_KIND:
    fprintf(stdout, "value: " BL_MAGENTA("%lld"), expr->value.s);
    break;
  case BL_UINT_KIND:
  case BL_SIZE_KIND:
    fprintf(stdout, "value: " BL_MAGENTA("%llu"), expr->value.u);
    break;
  case BL_REAL_KIND:
    fprintf(stdout, "value: " BL_MAGENTA("%f"), expr->value.f);
    break;
  case BL_BOOL_KIND:
    fprintf(stdout, "value: " BL_MAGENTA("%s"), expr->value.b ? "true" : "false");
    break;
  case BL_STR_KIND:
    fprintf(stdout, "value: " BL_MAGENTA("'%s'"), expr->value.str);
    break;
  case BL_CHAR_KIND:
    fprintf(stdout, "value: " BL_MAGENTA("%c"), expr->value.c);
    break;
  default:
    bl_abort("invalid constant type");
  }
}

static void
visit_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  switch (bl_node_code(expr)) {
  case BL_EXPR_CONST: {
    print_head("const", bl_peek_src(expr), expr, visitor->nesting);
    print_const_expr(bl_peek_expr_const(expr));
    break;
  }

  case BL_EXPR_UNARY: {
    print_head("unary", bl_peek_src(expr), expr, visitor->nesting);
    fprintf(stdout, "operation: " BL_YELLOW("'%s'"), bl_sym_strings[bl_peek_expr_unary(expr)->op]);
    break;
  }

  case BL_EXPR_SIZEOF: {
    print_head("sizeof", bl_peek_src(expr), expr, visitor->nesting);
    break;
  }

  case BL_EXPR_CAST: {
    print_head("cast", bl_peek_src(expr), expr, visitor->nesting);
    break;
  }

  case BL_EXPR_INIT: {
    print_head("init_list", bl_peek_src(expr), expr, visitor->nesting);
    break;
  }

  case BL_EXPR_BINOP:
    print_head("binop", bl_peek_src(expr), expr, visitor->nesting);
    fprintf(stdout, "operation: " BL_YELLOW("'%s'"), bl_sym_strings[bl_peek_expr_binop(expr)->op]);
    break;

  case BL_EXPR_DECL_REF:
    print_head("decl_ref", bl_peek_src(expr), expr, visitor->nesting);
    print_path(bl_peek_expr_decl_ref(expr)->path);
    fprintf(stdout, " -> " BL_YELLOW("%p"), bl_peek_expr_decl_ref(expr)->ref);
    break;

  case BL_EXPR_MEMBER_REF:
    print_head("member_ref", bl_peek_src(expr), expr, visitor->nesting);
    fprintf(stdout, BL_YELLOW("'%s'") " -> " BL_YELLOW("%p") BL_MAGENTA(" (%s)"),
            bl_peek_expr_member_ref(expr)->id.str, bl_peek_expr_member_ref(expr)->ref,
            bl_peek_expr_member_ref(expr)->is_ptr_ref ? "->" : ".");
    break;

  case BL_EXPR_ARRAY_REF:
    print_head("array_elem_ref", bl_peek_src(expr), expr, visitor->nesting);
    break;

  case BL_EXPR_NULL:
    print_head("null", bl_peek_src(expr), expr, visitor->nesting);
    break;

  case BL_EXPR_CALL:
    print_head("call", bl_peek_src(expr), expr, visitor->nesting);
    print_path(bl_peek_expr_call(expr)->path);
    fprintf(stdout, " -> " BL_YELLOW("%p") BL_CYAN(" %s"), bl_peek_expr_call(expr)->ref,
            bl_peek_expr_call(expr)->run_in_compile_time ? "#run" : "");
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

  for (int i = 0; i < c; ++i) {
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
    bl_visitor_add(&visitor, visit_mut, BL_VISIT_MUT);
    bl_visitor_add(&visitor, visit_const, BL_VISIT_CONST);
    bl_visitor_add(&visitor, visit_expr, BL_VISIT_EXPR);
    bl_visitor_add(&visitor, visit_if, BL_VISIT_IF);
    bl_visitor_add(&visitor, visit_loop, BL_VISIT_LOOP);
    bl_visitor_add(&visitor, visit_break, BL_VISIT_BREAK);
    bl_visitor_add(&visitor, visit_continue, BL_VISIT_CONTINUE);
    bl_visitor_add(&visitor, visit_return, BL_VISIT_RETURN);
    bl_visitor_add(&visitor, visit_struct_member, BL_VISIT_STRUCT_MEMBER);
    bl_visitor_add(&visitor, visit_enum_variant, BL_VISIT_ENUM_VARIANT);
    bl_visitor_add(&visitor, visit_load, BL_VISIT_LOAD);
    bl_visitor_add(&visitor, visit_link, BL_VISIT_LINK);
    bl_visitor_add(&visitor, visit_using, BL_VISIT_USING);

    bl_visitor_walk_module(&visitor, unit->ast.root);
  }

  fprintf(stdout, "\n\n");

  return BL_NO_ERR;
}
