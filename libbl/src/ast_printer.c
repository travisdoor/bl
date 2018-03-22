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
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), module->id.str);
  bl_visitor_walk_module(visitor, module);
}

static void
visit_func(bl_visitor_t *visitor, bl_func_t *func, bl_src_t *src)
{
  print_head("function", src, func, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), func->id.str);
  bl_visitor_walk_func(visitor, func);
}

static void
visit_type(bl_visitor_t *visitor, bl_type_t *type, bl_src_t *src)
{
  print_head("type", src, type, visitor->nesting);
  if (type->t == BL_TYPE_REF) {
    fprintf(stdout, "name: " BL_YELLOW("'%s' -> %p"), type->id.str, type->type.ref);
  } else {
    fprintf(stdout, "name: " BL_YELLOW("'%s'"), type->id.str);
  }
  bl_visitor_walk_type(visitor, type);
}

static void
visit_arg(bl_visitor_t *visitor, bl_arg_t *arg, bl_src_t *src)
{
  print_head("arg", src, arg, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), arg->id.str);
  bl_visitor_walk_arg(visitor, arg);
}

static void
visit_struct(bl_visitor_t *visitor, bl_struct_t *strct, bl_src_t *src)
{
  print_head("struct", src, strct, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), strct->id.str);
  bl_visitor_walk_struct(visitor, strct);
}

static void
visit_enum(bl_visitor_t *visitor, bl_enum_t *enm, bl_src_t *src)
{
  print_head("enum", src, enm, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), enm->id.str);
  bl_visitor_walk_enum(visitor, enm);
}

static void
visit_block(bl_visitor_t *visitor, bl_block_t *block, bl_src_t *src)
{
  print_head("block", src, block, visitor->nesting);
  bl_visitor_walk_block(visitor, block);
}

static void
visit_var(bl_visitor_t *visitor, bl_var_t *var, bl_src_t *src)
{
  print_head("variable", src, var, visitor->nesting);
  fprintf(stdout, "name: " BL_YELLOW("'%s'"), var->id.str);
  bl_visitor_walk_var(visitor, var);
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

    bl_visitor_walk_module(&visitor, &bl_peek_module(unit->ast.root));
  }

  fprintf(stdout, "\n\n");

  return BL_NO_ERR;
}
