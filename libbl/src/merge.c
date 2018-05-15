//************************************************************************************************
// blc
//
// File:   merge.c
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
//************************************************************************************************

/* Modules with same name can be declared in multiple units, those need to be merged. We don't merge
 * modules directly on AST node level, only scope cache pointers are shared between modules on same
 * path instead. There can be some symbol conflicts (functions with same name in same module) and we
 * need to notify user about that. This stage must be sucessfully executed before reference
 * connection due to lack of header files and free declaration order across whole assembly.
 */

#include <setjmp.h>
#include <bobject/containers/htbl.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define merge_error(cnt, code, loc, format, ...)                                                   \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,           \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define merge_warning(cnt, loc, format, ...)                                                       \
  {                                                                                                \
    bl_builder_warning((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,         \
                       ##__VA_ARGS__);                                                             \
  }

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  jmp_buf        jmp_error;
  bl_scope_t *   curr_scope;
} context_t;

static void
merge_func(bl_visitor_t *visitor, bl_node_t *func);

static void
merge_const(bl_visitor_t *visitor, bl_node_t *cnst);

static void
merge_struct(bl_visitor_t *visitor, bl_node_t *strct);

static void
merge_enum(bl_visitor_t *visitor, bl_node_t *enm);

static void
merge_module(bl_visitor_t *visitor, bl_node_t *module);

/*************************************************************************************************
 * declaration merging
 * all declaration in corresponding modules must be merged due to lack of header files
 *************************************************************************************************/
void
merge_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  context_t *     cnt   = peek_cnt(visitor);
  bl_scope_t *    scope = peek_cnt(visitor)->curr_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_func->id);

  if (conflict) {
    merge_error(cnt, BL_ERR_DUPLICATE_SYMBOL, func->src,
                "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                _func->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  bl_scope_insert_node(scope, func);
}

void
merge_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  bl_decl_const_t *_cnst = bl_peek_decl_const(cnst);
  context_t *      cnt   = peek_cnt(visitor);
  bl_scope_t *     scope = peek_cnt(visitor)->curr_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_cnst->id);

  if (conflict) {
    merge_error(cnt, BL_ERR_DUPLICATE_SYMBOL, cnst->src,
                "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                _cnst->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  bl_scope_insert_node(scope, cnst);
}

void
merge_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  bl_decl_struct_t *_strct = bl_peek_decl_struct(strct);
  context_t *       cnt    = peek_cnt(visitor);
  bl_scope_t *      scope  = peek_cnt(visitor)->curr_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_strct->id);

  if (conflict) {
    merge_error(cnt, BL_ERR_DUPLICATE_SYMBOL, strct->src,
                "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                _strct->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  /* check for duplicit members */
  _strct->scope       = bl_scope_new(cnt->assembly->scope_cache);
  bl_node_t *  member = NULL;
  const size_t c      = bl_ast_struct_member_count(_strct);
  for (size_t i = 0; i < c; ++i) {
    member   = bl_ast_struct_get_member(_strct, i);
    conflict = bl_scope_get_node(_strct->scope, &bl_peek_decl_struct_member(member)->id);

    if (conflict) {
      merge_error(cnt, BL_ERR_DUPLICATE_SYMBOL, member->src,
                  "duplicate struct memeber " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  bl_peek_decl_struct_member(member)->id.str, conflict->src->file,
                  conflict->src->line, conflict->src->col);
    }

    bl_scope_insert_node(_strct->scope, member);
  }

  bl_scope_insert_node(scope, strct);
}

void
merge_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  bl_decl_enum_t *_enm  = bl_peek_decl_enum(enm);
  context_t *     cnt   = peek_cnt(visitor);
  bl_scope_t *    scope = peek_cnt(visitor)->curr_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_enm->id);

  if (conflict) {
    merge_error(cnt, BL_ERR_DUPLICATE_SYMBOL, enm->src,
                "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                _enm->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  /* check for duplicit members and prepare lookup scope cache */
  _enm->scope          = bl_scope_new(cnt->assembly->scope_cache);
  bl_node_t *  variant = NULL;
  const size_t c       = bl_ast_enum_get_count(_enm);
  for (size_t i = 0; i < c; ++i) {
    variant  = bl_ast_enum_get_variant(_enm, i);
    conflict = bl_scope_get_node(_enm->scope, &bl_peek_decl_enum_variant(variant)->id);

    if (conflict) {
      merge_error(cnt, BL_ERR_DUPLICATE_SYMBOL, variant->src,
                  "duplicate enum variant " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  bl_peek_decl_enum_variant(variant)->id.str, conflict->src->file,
                  conflict->src->line, conflict->src->col);
    }
    bl_scope_insert_node(_enm->scope, variant);
  }

  bl_scope_insert_node(scope, enm);
}

void
merge_module(bl_visitor_t *visitor, bl_node_t *module)
{
  context_t * cnt            = peek_cnt(visitor);
  bl_scope_t *prev_scope_tmp = cnt->curr_scope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");
  bl_decl_module_t *_module  = bl_peek_decl_module(module);
  bl_node_t *       conflict = bl_scope_get_node(prev_scope_tmp, &_module->id);

  if (conflict) {
    if (bl_ast_try_get_modif(module) != bl_ast_try_get_modif(conflict)) {
      merge_error(peek_cnt(visitor), BL_ERR_UNCOMPATIBLE_MODIF, module->src,
                  "previous declaration of module " BL_YELLOW(
                      "'%s'") " has different access modifier, originally declared here: %s:%d:%d",
                  _module->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    }

    peek_cnt(visitor)->curr_scope = bl_peek_decl_module(conflict)->scope;
    _module->scope                = bl_peek_decl_module(conflict)->scope;
  } else {
    _module->scope                = bl_scope_new(cnt->assembly->scope_cache);
    peek_cnt(visitor)->curr_scope = _module->scope;
    bl_scope_insert_node(prev_scope_tmp, module);
  }

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->curr_scope = prev_scope_tmp;
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_merge_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  bl_log("mergeing...");
  context_t cnt = {.builder = builder, .assembly = assembly, .curr_scope = assembly->scope};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    /* free allocated memory on error */
    return (bl_error_e)error;
  }

  /* merge all modules and check for duplicity */
  bl_visitor_t visitor_merge;
  bl_visitor_init(&visitor_merge, &cnt);
  bl_visitor_add(&visitor_merge, merge_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_merge, merge_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_merge, merge_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor_merge, merge_const, BL_VISIT_CONST);
  bl_visitor_add(&visitor_merge, merge_enum, BL_VISIT_ENUM);

  const int c = bl_assembly_get_unit_count(assembly);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_merge, cnt.unit->ast.root);
  }

  return BL_NO_ERR;
}
