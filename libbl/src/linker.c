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
#include "block_scope_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define link_error(cnt, code, loc, format, ...)                                                    \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,           \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

typedef struct
{
  bl_block_scope_t block_scope;
  bl_builder_t *   builder;
  bl_assembly_t *  assembly;
  jmp_buf          jmp_error;
  bl_scope_t *     gscope;
  bl_scope_t *     mod_scope;
} context_t;

/* declaration merging */
/**************************************************************************************************/
static void
merge_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  context_t *     cnt   = peek_cnt(visitor);
  bl_scope_t *    scope = peek_cnt(visitor)->mod_scope;

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
  bl_scope_t *      scope  = peek_cnt(visitor)->mod_scope;

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
  bl_scope_t *    scope = peek_cnt(visitor)->mod_scope;

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
  context_t * cnt            = peek_cnt(visitor);
  bl_scope_t *prev_scope_tmp = cnt->mod_scope;
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

    peek_cnt(visitor)->mod_scope = bl_peek_decl_module(conflict)->scope;
    _module->scope               = bl_peek_decl_module(conflict)->scope;
  } else {
    _module->scope               = bl_scope_new(cnt->assembly->scope_cache);
    peek_cnt(visitor)->mod_scope = _module->scope;
    bl_scope_insert_node(prev_scope_tmp, module);
  }

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->mod_scope = prev_scope_tmp;
}
/**************************************************************************************************/

/* linking expressions */
/**************************************************************************************************/
static void
link_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_scope_t *prev_scope_tmp = peek_cnt(visitor)->mod_scope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");

  peek_cnt(visitor)->mod_scope = bl_peek_decl_module(module)->scope;
  bl_assert(peek_cnt(visitor)->mod_scope, "invalid next scope");

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->mod_scope = prev_scope_tmp;
}

typedef enum { LOOKUP_GSCOPE = 1, LOOKUP_MOD_SCOPE = 2, LOOKUP_BLOCK_SCOPE = 4 } lookup_flag_e;

static bl_node_t *
lookup_node_1(context_t *cnt, BArray *path, bl_scope_t *mod_scope, int scope_flag, int iter);

static bl_node_t *
lookup_node(context_t *cnt, BArray *path, int scope_flag)
{
  return lookup_node_1(cnt, path, cnt->mod_scope, scope_flag, 0);
}

bl_node_t *
lookup_node_1(context_t *cnt, BArray *path, bl_scope_t *mod_scope, int scope_flag, int iter)
{
  bl_node_t *found     = NULL;
  bl_node_t *path_elem = bo_array_at(path, iter, bl_node_t *);

  /* search symbol in block scope */
  if (scope_flag & LOOKUP_BLOCK_SCOPE) {
    found = bl_block_scope_get_node(&cnt->block_scope, &bl_peek_expr_path(path_elem)->id);
  }

  /* search symbol in module scope */
  if (scope_flag & LOOKUP_MOD_SCOPE && !found) {
    found = bl_scope_get_node(mod_scope, &bl_peek_expr_path(path_elem)->id);
  }

  /* search symbol in global scope */
  if (scope_flag & LOOKUP_GSCOPE && !found) {
    found = bl_scope_get_node(cnt->gscope, &bl_peek_expr_path(path_elem)->id);
  }

  if (found == NULL) {
    link_error(cnt, BL_ERR_UNKNOWN_SYMBOL, path_elem->src, "unknown symbol " BL_YELLOW("'%s'"),
               bl_peek_expr_path(path_elem)->id.str);
  }

  iter++;

  if (mod_scope != cnt->mod_scope && !(bl_ast_try_get_modif(found) & BL_MODIF_PUBLIC)) {
    link_error(cnt, BL_ERR_PRIVATE, path_elem->src,
               "symbol " BL_YELLOW("'%s'") " is private in this context, declared here: %s %d:%d",
               bl_peek_expr_path(path_elem)->id.str, found->src->file, found->src->line,
               found->src->col);
  }

  if (bl_node_code(found) == BL_DECL_MODULE) {
    if (iter == bo_array_size(path)) {
      link_error(cnt, BL_ERR_UNKNOWN_SYMBOL, path_elem->src,
                 "symbol " BL_YELLOW("'%s'") " is module, declared here: %s %d:%d",
                 bl_peek_expr_path(path_elem)->id.str, found->src->file, found->src->line,
                 found->src->col);
    }
    return lookup_node_1(cnt, path, bl_peek_decl_module(found)->scope, LOOKUP_MOD_SCOPE, iter);
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
    found = lookup_node(cnt, bl_peek_expr_call(expr)->path, LOOKUP_GSCOPE | LOOKUP_MOD_SCOPE);

    bl_peek_expr_call(expr)->ref = found;
    bl_peek_decl_func(found)->used++;
    break;
  case BL_EXPR_VAR_REF:
    found = lookup_node(cnt, bl_peek_expr_var_ref(expr)->path,
                        LOOKUP_GSCOPE | LOOKUP_MOD_SCOPE | LOOKUP_BLOCK_SCOPE);

    bl_peek_expr_var_ref(expr)->ref = found;
    if (bl_node_is(found, BL_DECL_VAR))
      bl_peek_decl_var(found)->used++;
    break;
  default:
    break;
  }

  bl_visitor_walk_expr(visitor, expr);
}

static void
link_type(bl_visitor_t *visitor, bl_node_t *type)
{
  bl_node_t *found = NULL;
  context_t *cnt   = peek_cnt(visitor);

  switch (bl_node_code(type)) {
  case BL_TYPE_REF:
    found = lookup_node(cnt, bl_peek_type_ref(type)->path, LOOKUP_GSCOPE | LOOKUP_MOD_SCOPE);

    bl_peek_type_ref(type)->ref = found;
    break;
  default:
    break;
  }

  bl_visitor_walk_type(visitor, type);
}

static void
link_func_args(context_t *cnt, bl_decl_func_t *func)
{
  /*
   * make all function parameters available in current block scope context
   */

  const size_t c = bl_ast_func_arg_count(func);
  bl_node_t *  arg;
  for (size_t i = 0; i < c; ++i) {
    arg = bl_ast_func_get_arg(func, i);

    bl_node_t *conflict = bl_block_scope_get_node(&cnt->block_scope, &bl_peek_decl_var(arg)->id);
    if (conflict) {
      link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, arg->src,
                 "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s %d:%d",
                 bl_peek_decl_var(arg)->id.str, conflict->src->file, conflict->src->line,
                 conflict->src->col);
    } else {
      bl_block_scope_insert_node(&cnt->block_scope, arg);
    }
  }
}

static void
link_block(bl_visitor_t *visitor, bl_node_t *block)
{
  context_t *      cnt    = peek_cnt(visitor);
  bl_decl_block_t *_block = bl_peek_decl_block(block);

  bl_block_scope_push(&cnt->block_scope);

  bl_assert(_block->parent, "block has no parent");
  if (bl_node_code(_block->parent) == BL_DECL_FUNC) {
    link_func_args(cnt, bl_peek_decl_func(_block->parent));
  }

  bl_visitor_walk_block(visitor, block);
  bl_block_scope_pop(&cnt->block_scope);
}

static void
link_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *    cnt  = peek_cnt(visitor);
  bl_decl_var_t *_var = bl_peek_decl_var(var);

  bl_node_t *conflict = bl_block_scope_get_node(&cnt->block_scope, &_var->id);
  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, var->src,
               "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s %d:%d",
               _var->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  } else {
    bl_block_scope_insert_node(&cnt->block_scope, var);
  }

  bl_visitor_walk_var(visitor, var);
}
/**************************************************************************************************/

/* main entry function */
/**************************************************************************************************/
bl_error_e
bl_linker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder   = builder,
                   .assembly  = assembly,
                   .mod_scope = assembly->scope,
                   .gscope    = assembly->scope};

  bl_block_scope_init(&cnt.block_scope);

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
  bl_visitor_add(&visitor_link, link_type, BL_VISIT_TYPE);
  bl_visitor_add(&visitor_link, link_block, BL_VISIT_BLOCK);
  bl_visitor_add(&visitor_link, link_var, BL_VISIT_VAR);

  for (int i = 0; i < c; i++) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_link, unit->ast.root);
  }

  bl_block_scope_terminate(&cnt.block_scope);

  return BL_NO_ERR;
}
