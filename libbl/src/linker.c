//*****************************************************************************
// blc
//
// File:   linker.c
// Author: Martin Dorazil
// Date:   14.2.18
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

#include <setjmp.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define link_error(cnt, code, loc, format, ...)                                                    \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s %d:%d " format, loc->file, loc->line, loc->col,           \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  jmp_buf        jmp_error;
  bl_scope_t *   cscope;
  bl_scope_t *   gscope;
} context_t;

/* declaration merging */
/**************************************************************************************************/
static void
merge_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  context_t *     cnt   = peek_cnt(visitor);
  bl_scope_t *    scope = peek_cnt(visitor)->cscope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_func->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, func->src,
               "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s %d:%d",
               _func->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  bl_scope_insert_node(scope, func);
}

static void
merge_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  context_t *       cnt    = peek_cnt(visitor);
  bl_scope_t *      scope  = peek_cnt(visitor)->cscope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_strct->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, strct->src,
               "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s %d:%d",
               _strct->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  bl_scope_insert_node(scope, strct);
}

static void
merge_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  bl_decl_enum_t *_enm  = bl_peek_decl_enum(enm);
  context_t *     cnt   = peek_cnt(visitor);
  bl_scope_t *    scope = peek_cnt(visitor)->cscope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_enm->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, enm->src,
               "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s %d:%d",
               _enm->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  bl_scope_insert_node(scope, enm);
}

static void
merge_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_scope_t *prev_scope_tmp = peek_cnt(visitor)->cscope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");
  bl_decl_module_t *_module  = bl_peek_decl_module(module);
  bl_node_t *       conflict = bl_scope_get_node(prev_scope_tmp, &_module->id);

  if (conflict) {
    if (bl_ast_try_get_modif(module) != bl_ast_try_get_modif(conflict)) {
      link_error(peek_cnt(visitor), BL_ERR_UNCOMPATIBLE_MODIF, module->src,
                 "previous declaration of module " BL_YELLOW(
                     "'%s'") " has different access modifier, originally declared here: %s %d:%d",
                 _module->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    }

    peek_cnt(visitor)->cscope = bl_peek_decl_module(conflict)->scope;
    _module->scope            = bl_peek_decl_module(conflict)->scope;
    bl_log("reuse %s", _module->id.str);
  } else {
    _module->scope            = bl_scope_new();
    peek_cnt(visitor)->cscope = _module->scope;
    bl_scope_insert_node(prev_scope_tmp, module);
    bl_log("new %s", _module->id.str);
  }

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->cscope = prev_scope_tmp;
}
/**************************************************************************************************/

/* linking expressions */
/**************************************************************************************************/
static void
link_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_scope_t *prev_scope_tmp = peek_cnt(visitor)->cscope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");

  peek_cnt(visitor)->cscope = bl_peek_decl_module(module)->scope;
  bl_assert(peek_cnt(visitor)->cscope, "invalid next scope");

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->cscope = prev_scope_tmp;
}

typedef enum { LOOKUP_GSCOPE = 1, LOOKUP_CSCOPE = 2 } lookup_flag_e;

static bl_node_t *
lookup_node(context_t *cnt, BArray *path, bl_scope_t *cscope, int scope_flag, int iter)
{
  bl_node_t *found     = NULL;
  bl_node_t *path_elem = bo_array_at(path, iter, bl_node_t *);

  /* search symbol in current scope */
  if (scope_flag & LOOKUP_CSCOPE) {
    found = bl_scope_get_node(cscope, &bl_peek_expr_path(path_elem)->id);
  }

  /* search symbol in current scope */
  if (scope_flag & LOOKUP_GSCOPE && !found) {
    found = bl_scope_get_node(cnt->gscope, &bl_peek_expr_path(path_elem)->id);
  }

  if (found == NULL) {
    link_error(cnt, BL_ERR_UNKNOWN_SYMBOL, path_elem->src,
               "unknown module or enumerator " BL_YELLOW("'%s'") " in path expression",
               bl_peek_expr_path(path_elem)->id.str);
  }

  iter++;

  if (cscope != cnt->cscope && !(bl_ast_try_get_modif(found) & BL_MODIF_PUBLIC)) {
    link_error(cnt, BL_ERR_PRIVATE, path_elem->src,
               "symbol " BL_YELLOW("'%s'") " is private in this context, declared here: %s %d:%d",
               bl_peek_expr_path(path_elem)->id.str, found->src->file, found->src->line,
               found->src->col);
  }

  if (bl_node_code(found) == BL_DECL_MODULE) {
    bl_assert(iter < bo_array_size(path), "module cannot be last path element");
    return lookup_node(cnt, path, bl_peek_decl_module(found)->scope, LOOKUP_CSCOPE, iter);
  } else {
    bl_assert(iter == bo_array_size(path), "invalid path");
    return found;
  }

  return NULL;
}

static void
link_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  bl_node_t *found = NULL;
  context_t *cnt   = peek_cnt(visitor);

  switch (bl_node_code(expr)) {
  case BL_EXPR_CALL:
    found                        = lookup_node(cnt, bl_peek_expr_call(expr)->path, cnt->cscope,
                        LOOKUP_GSCOPE | LOOKUP_CSCOPE, 0);
    bl_peek_expr_call(expr)->ref = found;
    break;
  default:
    break;
  }
}

/**************************************************************************************************/

/* main entry function */
/**************************************************************************************************/
bl_error_e
bl_linker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder  = builder,
                   .assembly = assembly,
                   .cscope   = assembly->scope,
                   .gscope   = assembly->scope};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor_merge;
  bl_visitor_init(&visitor_merge, &cnt);
  bl_visitor_add(&visitor_merge, merge_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_merge, merge_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_merge, merge_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor_merge, merge_enum, BL_VISIT_ENUM);

  const int  c    = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit = NULL;

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_merge, unit->ast.root);
  }

  bl_visitor_t visitor_link;
  bl_visitor_init(&visitor_link, &cnt);
  bl_visitor_add(&visitor_link, link_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_link, link_expr, BL_VISIT_EXPR);

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_link, unit->ast.root);
  }

  return BL_NO_ERR;
}
