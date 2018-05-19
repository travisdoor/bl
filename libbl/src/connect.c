//************************************************************************************************
// blc
//
// File:   connect.c
// Author: Martin Dorazil
// Date:   14.2.18
//
// Copyright 2018 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, first, publish, distribute, sublicense, and/or sell
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

/*************************************************************************************************
 * Connect will do 3 partial iterations of AST passed in.
 *
 * 2) connect type tree of custom types (mostly structures) so references to those types can be used
 *    later in compilation
 *    prepare file-global usings
 *
 * 3) connect rest of the source (mostly expressions referencing to some custom types)
 *************************************************************************************************/

#include <setjmp.h>
#include <bobject/containers/htbl.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define connect_error(cnt, code, loc, format, ...)                                                 \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,           \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define connect_warning(cnt, loc, format, ...)                                                     \
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

  /* Current compound node is pointer to current root for symbol lookup. It is typically block of
   * code inside curly brackets (module, function body, etc.). This node must have scope cache. When
   * needed symbol has not been found in scope of this node we need access to parent of this node
   * also. For example when we looking for type declaration of variable we first search in scope of
   * current compound node and than recursively in its parent nodes when type is declared
   * somewhere in global scope or parent module. */
  bl_node_t *curr_compound;
} context_t;

#define _VALIDATE_ARGS context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last
typedef void (*lookup_elem_valid_f)(_VALIDATE_ARGS);
#define VALIDATE_F(name) void validate_##name(_VALIDATE_ARGS)

/*************************************************************************************************
 * helpers
 *************************************************************************************************/
static void
include_using(context_t *cnt, bl_node_t *using);

static bl_node_t *
lookup(context_t *cnt, BArray *path, lookup_elem_valid_f validator, bool *found_in_curr_branch);

static bl_node_t *
lookup_in_tree(context_t *cnt, bl_id_t *id, bl_node_t *curr_compound, bl_node_t **linked_by,
               bl_node_t **prev_compound);

static bl_node_t *
lookup_in_scope(context_t *cnt, bl_id_t *id, bl_node_t *curr_compound, bl_node_t **linked_by);

static bl_node_t *
satisfy_decl_ref(context_t *cnt, bl_node_t *ref);

static bl_node_t *
satisfy_type(context_t *cnt, bl_node_t *type);

static void
satisfy_const(context_t *cnt, bl_node_t *cnst);

/*************************************************************************************************
 * First pass
 * - join all modules in one (in context meaning)
 * - insert all nodes in global scope into lookup tables
 *************************************************************************************************/

static void
first_pass_enum(bl_visitor_t *visitor, bl_node_t *enm);

static void
first_pass_const(bl_visitor_t *visitor, bl_node_t *cnst);

static void
first_pass_func(bl_visitor_t *visitor, bl_node_t *func);

static void
first_pass_module(bl_visitor_t *visitor, bl_node_t *module);

static void
first_pass_struct(bl_visitor_t *visitor, bl_node_t *strct);

/*************************************************************************************************
 * Second pass
 * - connect structure type tree
 *************************************************************************************************/

static void
second_pass_module(bl_visitor_t *visitor, bl_node_t *module);

static void
second_pass_using(bl_visitor_t *visitor, bl_node_t *using);

/*************************************************************************************************
 * Third pass
 * - connect everything in function scopes
 *************************************************************************************************/
static void
third_pass_module(bl_visitor_t *visitor, bl_node_t *module);

static void
third_pass_block(bl_visitor_t *visitor, bl_node_t *block);

static void
third_pass_func(bl_visitor_t *visitor, bl_node_t *func);

static void
third_pass_var(bl_visitor_t *visitor, bl_node_t *var);

static void
third_pass_expr(bl_visitor_t *visitor, bl_node_t *expr);

static void
third_pass_using(bl_visitor_t *visitor, bl_node_t *using);

static void
third_pass_type(bl_visitor_t *visitor, bl_node_t *type);

static void
third_pass_const(bl_visitor_t *visitor, bl_node_t *cnst);

/*************************************************************************************************
 * Validation
 *************************************************************************************************/

static VALIDATE_F(decl_ref);
static VALIDATE_F(call);
static VALIDATE_F(using);
static VALIDATE_F(type);

/*************************************************************************************************
 * Helpers impl
 *************************************************************************************************/
bl_node_t *
lookup(context_t *cnt, BArray *path, lookup_elem_valid_f validator, bool *found_in_curr_branch)
{
  const size_t c              = bo_array_size(path);
  bl_node_t *  path_elem      = bo_array_at(path, 0, bl_node_t *);
  bl_node_t *  linked_by      = NULL;
  bl_node_t *  prev_compound  = NULL;
  bool         in_curr_branch = false;
  bl_node_t *  found = lookup_in_tree(cnt, &bl_peek_path_elem(path_elem)->id, cnt->curr_compound,
                                    &linked_by, &prev_compound);

  if (validator)
    validator(cnt, path_elem, found, c == 1);
  bl_assert(found, "null found symbol unhandled by validator");

  if (!(bl_ast_try_get_modif(found) & BL_MODIF_PUBLIC) && bl_node_is(linked_by, BL_STMT_USING)) {
    connect_error(cnt, BL_ERR_PRIVATE, path_elem->src,
                  "symbol " BL_YELLOW("'%s'") " is private in this context",
                  bl_peek_path_elem(path_elem)->id.str);
  }

  in_curr_branch = prev_compound == found;

  for (size_t i = 1; i < c; ++i) {
    path_elem = bo_array_at(path, i, bl_node_t *);
    found     = lookup_in_scope(cnt, &bl_peek_path_elem(path_elem)->id, found, &linked_by);

    if (validator)
      validator(cnt, path_elem, found, c == i + 1);
    bl_assert(found, "null found symbol unhandled by validator");

    if (!(bl_ast_try_get_modif(found) & BL_MODIF_PUBLIC) && !in_curr_branch) {
      connect_error(cnt, BL_ERR_PRIVATE, path_elem->src,
                    "symbol " BL_YELLOW("'%s'") " is private in this context",
                    bl_peek_path_elem(path_elem)->id.str);
    }
  }

  if (found_in_curr_branch)
    (*found_in_curr_branch) = in_curr_branch;
  return found;
}

bl_node_t *
lookup_in_tree(context_t *cnt, bl_id_t *id, bl_node_t *curr_compound, bl_node_t **linked_by,
               bl_node_t **prev_compound)
{
  bl_node_t *  found             = NULL;
  bl_scopes_t *tmp_scopes        = NULL;
  bl_node_t *  tmp_curr_compound = curr_compound;

  while (found == NULL && tmp_curr_compound != NULL) {
    tmp_scopes = bl_ast_try_get_scopes(tmp_curr_compound);
    bl_assert(tmp_scopes, "invalid scopes");
    found = bl_scopes_get_node(tmp_scopes, id, linked_by);

    if (found == NULL && prev_compound != NULL)
      (*prev_compound) = tmp_curr_compound;

    tmp_curr_compound = bl_ast_try_get_parent(tmp_curr_compound);
  }

  return found;
}

bl_node_t *
lookup_in_scope(context_t *cnt, bl_id_t *id, bl_node_t *curr_compound, bl_node_t **linked_by)
{
  bl_scopes_t *scopes = bl_ast_try_get_scopes(curr_compound);
  return bl_scopes_get_node(scopes, id, linked_by);
}

bl_node_t *
satisfy_decl_ref(context_t *cnt, bl_node_t *ref)
{
  bl_expr_decl_ref_t *_ref  = bl_peek_expr_decl_ref(ref);
  bl_node_t *         found = lookup(cnt, _ref->path, validate_decl_ref, NULL); // TODO: validator
  _ref->ref                 = found;

  switch (bl_node_code(found)) {
  case BL_DECL_VAR:
    bl_peek_decl_var(found)->used++;
    break;
  case BL_DECL_CONST:
    bl_peek_decl_const(found)->used++;
    break;
  default:
    // TODO: handle as error
    break;
  }

  return found;
}

bl_node_t *
satisfy_type(context_t *cnt, bl_node_t *type)
{
  bl_node_t *found = NULL;
  if (bl_node_is(type, BL_TYPE_REF)) {
    found                       = lookup(cnt, bl_peek_type_ref(type)->path, validate_type, NULL);
    bl_peek_type_ref(type)->ref = found;

    switch (bl_node_code(found)) {
    case BL_DECL_STRUCT:
      bl_peek_decl_struct(found)->used++;
      break;
    case BL_DECL_ENUM:
      bl_peek_decl_enum(found)->used++;
      break;
    default:
      connect_error(cnt, BL_ERR_INVALID_TYPE, type->src,
                    "unknown type, struct or enum " BL_YELLOW("'%s'"),
                    bl_ast_try_get_id(found)->str);
    }
  }

  return found;
}

void
satisfy_const(context_t *cnt, bl_node_t *cnst)
{
  bl_decl_const_t *_cnst     = bl_peek_decl_const(cnst);
  bl_scopes_t *    scopes    = bl_ast_try_get_scopes(cnt->curr_compound);
  bl_node_t *      linked_by = NULL;
  bl_node_t *      conflict = lookup_in_tree(cnt, &_cnst->id, cnt->curr_compound, &linked_by, NULL);

  if (conflict) {
    if (linked_by == cnt->curr_compound) {
      connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, cnst->src,
                    "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                    _cnst->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    } else {
      connect_warning(cnt, cnst->src, BL_YELLOW("'%s'") " hides symbol declared here: %s:%d:%d",
                      _cnst->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    }
  }
  bl_scopes_insert_node(scopes, cnst);
}

void
include_using(context_t *cnt, bl_node_t *using)
{
  bl_node_t *conflict       = NULL;
  bool       in_curr_branch = false;
  bl_node_t *found = lookup(cnt, bl_peek_stmt_using(using)->path, validate_using, &in_curr_branch);
  bl_peek_stmt_using(using)->ref = found;

  if (in_curr_branch) {
    connect_warning(cnt, using->src, "using with no effect, trying to link current branch");
    return;
  }

  bl_scopes_t *found_scopes = bl_ast_try_get_scopes(found);
  bl_scopes_t *curr_scopes  = bl_ast_try_get_scopes(cnt->curr_compound);
  bl_assert(found_scopes, "invalid scopes");
  bl_assert(curr_scopes, "invalid scopes");

  /* check for scope conflicts */
  conflict = bl_scopes_get_linked_by(curr_scopes, found_scopes->main);
  if (conflict) {
    connect_warning(cnt, using->src, "using with no effect due previous one here: %d:%d",
                    conflict->src->line, conflict->src->col);
    return;
  }

  /* add found compound into cache */
  bl_scopes_include(curr_scopes, found_scopes->main, using);
}

/*************************************************************************************************
 * First impl
 *************************************************************************************************/

void
first_pass_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_decl_module_t *_module  = bl_peek_decl_module(module);
  context_t *       cnt      = peek_cnt(visitor);
  bl_node_t *       prev_cmp = cnt->curr_compound;
  bl_node_t *       conflict = lookup_in_scope(cnt, &_module->id, prev_cmp, NULL);

  cnt->curr_compound = module;

  if (conflict) {
    if (bl_ast_try_get_modif(module) != bl_ast_try_get_modif(conflict)) {
      connect_error(
          peek_cnt(visitor), BL_ERR_UNCOMPATIBLE_MODIF, module->src,
          "previous declaration of module " BL_YELLOW(
              "'%s'") " has different access modifier, originally declared here: %s:%d:%d",
          _module->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    }

    bl_scopes_t *conflict_scopes = bl_ast_try_get_scopes(conflict);
    bl_assert(conflict_scopes->main, "invalid main scope");
    bl_scopes_include_main(&_module->scopes, conflict_scopes->main, module);
  } else {
    bl_scopes_t *prev_scopes = bl_ast_try_get_scopes(prev_cmp);
    bl_scope_t * new_main    = bl_scope_new(cnt->assembly->scope_cache);
    bl_scopes_include_main(&_module->scopes, new_main, module);
    bl_scopes_insert_node(prev_scopes, module);
  }

  /* non-terminal */
  bl_visitor_walk_module(visitor, module);
  cnt->curr_compound = prev_cmp;
}

void
first_pass_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is_not(cnt->curr_compound, BL_DECL_MODULE))
    return;

  satisfy_const(cnt, cnst);
  /* terminal */
}

void
first_pass_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  bl_decl_enum_t *_enm     = bl_peek_decl_enum(enm);
  context_t *     cnt      = peek_cnt(visitor);
  bl_node_t *     conflict = lookup_in_tree(cnt, &_enm->id, cnt->curr_compound, NULL, NULL);

  if (conflict) {
    connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, enm->src,
                  "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  _enm->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  /* check for duplicit members and prepare lookup scope cache */
  bl_scope_t *scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_enm->scopes, scope, enm);

  bl_node_t *  variant = NULL;
  const size_t c       = bl_ast_enum_get_count(_enm);
  for (size_t i = 0; i < c; ++i) {
    variant  = bl_ast_enum_get_variant(_enm, i);
    conflict = bl_scope_get_node(scope, &bl_peek_decl_enum_variant(variant)->id);

    if (conflict) {
      connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, variant->src,
                    "duplicate enum variant " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                    bl_peek_decl_enum_variant(variant)->id.str, conflict->src->file,
                    conflict->src->line, conflict->src->col);
    }
    bl_scope_insert_node(scope, variant);
  }

  bl_scopes_t *scopes = bl_ast_try_get_scopes(cnt->curr_compound);
  bl_scopes_insert_node(scopes, enm);
  /* terminal */
}

void
first_pass_func(bl_visitor_t *visitor, bl_node_t *func)
{
  context_t *     cnt      = peek_cnt(visitor);
  bl_decl_func_t *_func    = bl_peek_decl_func(func);
  bl_node_t *     conflict = lookup_in_scope(cnt, &_func->id, cnt->curr_compound, NULL);

  if (conflict) {
    connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, func->src,
                  "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  _func->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  /* create new scope for function declaration */
  bl_scope_t *main_scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_func->scopes, main_scope, func);

  /* TODO: insert args into scope of function */

  bl_scopes_t *scopes = bl_ast_try_get_scopes(cnt->curr_compound);
  bl_scopes_insert_node(scopes, func);
  /* terminal */
}

void
first_pass_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  bl_decl_struct_t *_strct   = bl_peek_decl_struct(strct);
  context_t *       cnt      = peek_cnt(visitor);
  bl_node_t *       conflict = lookup_in_scope(cnt, &_strct->id, cnt->curr_compound, NULL);

  if (conflict) {
    connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, strct->src,
                  "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  _strct->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  }

  /* check for duplicit members */
  bl_scope_t *scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_strct->scopes, scope, strct);

  bl_node_t *member = NULL;

  const size_t c = bl_ast_struct_member_count(_strct);
  for (size_t i = 0; i < c; ++i) {
    member   = bl_ast_struct_get_member(_strct, i);
    conflict = bl_scope_get_node(scope, &bl_peek_decl_struct_member(member)->id);

    if (conflict) {
      connect_error(
          cnt, BL_ERR_DUPLICATE_SYMBOL, member->src,
          "duplicate struct memeber " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
          bl_peek_decl_struct_member(member)->id.str, conflict->src->file, conflict->src->line,
          conflict->src->col);
    }

    bl_scope_insert_node(scope, member);
  }

  bl_scopes_t *scopes = bl_ast_try_get_scopes(cnt->curr_compound);
  bl_scopes_insert_node(scopes, strct);

  /* terminal */
}

/*************************************************************************************************
 * Pre-connect impl
 *************************************************************************************************/
void
second_pass_module(bl_visitor_t *visitor, bl_node_t *module)
{
  context_t *cnt      = peek_cnt(visitor);
  bl_node_t *prev_cmp = cnt->curr_compound;
  cnt->curr_compound  = module;

  bl_visitor_walk_module(visitor, module);

  cnt->curr_compound = prev_cmp;
}

void
second_pass_using(bl_visitor_t *visitor, bl_node_t *using)
{
  /* solve only usings inside global scope */
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is_not(cnt->curr_compound, BL_DECL_MODULE))
    return;

  include_using(cnt, using);
}

/*************************************************************************************************
 * Connect impl
 *************************************************************************************************/
void
third_pass_using(bl_visitor_t *visitor, bl_node_t *using)
{
  /* solve only usings in other scopes than modules */
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is(cnt->curr_compound, BL_DECL_MODULE))
    return;

  include_using(cnt, using);
}

void
third_pass_type(bl_visitor_t *visitor, bl_node_t *type)
{
  context_t *cnt = peek_cnt(visitor);
  satisfy_type(cnt, type);
  bl_visitor_walk_type(visitor, type);
}

void
third_pass_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is(cnt->curr_compound, BL_DECL_MODULE)) {
    bl_visitor_walk_const(visitor, cnst);
    return;
  }

  satisfy_const(cnt, cnst);
  bl_visitor_walk_const(visitor, cnst);
}

void
third_pass_module(bl_visitor_t *visitor, bl_node_t *module)
{
  context_t *cnt      = peek_cnt(visitor);
  bl_node_t *prev_cmp = cnt->curr_compound;
  cnt->curr_compound  = module;

  bl_visitor_walk_module(visitor, module);

  cnt->curr_compound = prev_cmp;
}

void
third_pass_block(bl_visitor_t *visitor, bl_node_t *block)
{
  context_t *      cnt      = peek_cnt(visitor);
  bl_decl_block_t *_block   = bl_peek_decl_block(block);
  bl_node_t *      prev_cmp = cnt->curr_compound;

  bl_scope_t *main_scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_block->scopes, main_scope, block);
  cnt->curr_compound = block;

  bl_visitor_walk_block(visitor, block);

  cnt->curr_compound = prev_cmp;
}

void
third_pass_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  context_t *cnt = peek_cnt(visitor);
  switch (bl_node_code(expr)) {
  case BL_EXPR_CALL: {
    bl_node_t *found = lookup(cnt, bl_peek_expr_call(expr)->path, validate_call, NULL);

    bl_peek_expr_call(expr)->ref = found;
    bl_peek_decl_func(found)->used++;
    break;
  }

  case BL_EXPR_DECL_REF: {
    satisfy_decl_ref(cnt, expr);
    break;
  }

  case BL_EXPR_MEMBER_REF: {
    /* member access expression has not been linked yet -> solve it recursivelly */
    /* if (bl_peek_expr_member_ref(expr)->ref == NULL) */
    /*   satisfy_member(cnt, expr); */

    break;
  }

  default:
    break;
  }

  bl_visitor_walk_expr(visitor, expr);
}

void
third_pass_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *    cnt       = peek_cnt(visitor);
  bl_decl_var_t *_var      = bl_peek_decl_var(var);
  bl_node_t *    linked_by = NULL;

  bl_node_t *conflict = lookup_in_tree(cnt, &_var->id, cnt->curr_compound, &linked_by, NULL);
  if (conflict) {
    if (linked_by == cnt->curr_compound) {
      connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, var->src,
                    "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                    _var->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    } else {
      connect_warning(cnt, var->src, BL_YELLOW("'%s'") " hides symbol declared here: %s:%d:%d",
                      _var->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
    }
  }

  bl_scopes_t *scopes = bl_ast_try_get_scopes(cnt->curr_compound);
  bl_scopes_insert_node(scopes, var);

  bl_visitor_walk_var(visitor, var);
}

void
third_pass_func(bl_visitor_t *visitor, bl_node_t *func)
{
  context_t *cnt      = peek_cnt(visitor);
  bl_node_t *prev_cmp = cnt->curr_compound;
  cnt->curr_compound  = func;
  bl_visitor_walk_func(visitor, func);
  cnt->curr_compound = prev_cmp;
}

/*************************************************************************************************
 * Validate impl
 *************************************************************************************************/
VALIDATE_F(call)
{
  if (last) {
    if (found == NULL) {
      connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown function " BL_YELLOW("'%s'"),
                    bl_peek_path_elem(elem)->id.str);
    }
    if (bl_node_is_not(found, BL_DECL_FUNC))
      connect_error(cnt, BL_ERR_EXPECTED_FUNC, elem->src, "expected function name");
  } else {
    if (found == NULL) {
      connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown module " BL_YELLOW("'%s'"),
                    bl_peek_path_elem(elem)->id.str);
    }

    if (bl_node_is_not(found, BL_DECL_MODULE)) {
      connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src,
                    "expected module name in function call path");
    }
  }
}

VALIDATE_F(using)
{
  if (found == NULL) {
    connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown module " BL_YELLOW("'%s'"),
                  bl_peek_path_elem(elem)->id.str);
  }

  if (last) {
    if (bl_node_is_not(found, BL_DECL_MODULE) && bl_node_is_not(found, BL_DECL_ENUM)) {
      connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src,
                    "expected module or enum in using path");
    }
  } else {
    if (bl_node_is_not(found, BL_DECL_MODULE)) {
      connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src, "expected module name in using path");
    }
  }
}

VALIDATE_F(decl_ref)
{
  if (found == NULL) {
    connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown symbol " BL_YELLOW("'%s'"),
                  bl_peek_path_elem(elem)->id.str);
  }
}

VALIDATE_F(type)
{
  if (last) {
    if (found == NULL) {
      connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown type " BL_YELLOW("'%s'"),
                    bl_peek_path_elem(elem)->id.str);
    }

    switch (bl_node_code(found)) {
    case BL_DECL_STRUCT:
    case BL_DECL_ENUM:
      break;
    default:
      connect_error(cnt, BL_ERR_EXPECTED_TYPE, elem->src,
                    "expected type name, " BL_YELLOW("'%s'") " is invalid",
                    bl_peek_path_elem(elem)->id.str);
    }
  } else {
    if (found == NULL) {
      connect_error(cnt, BL_ERR_UNKNOWN_SYMBOL, elem->src, "unknown module " BL_YELLOW("'%s'"),
                    bl_peek_path_elem(elem)->id.str);
    }

    if (bl_node_is_not(found, BL_DECL_MODULE)) {
      connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src, "expected module name in type path");
    }
  }
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_connect_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder = builder, .assembly = assembly, .curr_compound = NULL};
  const int c   = bl_assembly_get_unit_count(assembly);
  /* all anonymous global modules needs shared cache */
  bl_scope_t *gscope = bl_scope_new(assembly->scope_cache);

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e)error;
  }

  bl_visitor_t visitor_first;
  bl_visitor_init(&visitor_first, &cnt);
  bl_visitor_add(&visitor_first, first_pass_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_first, first_pass_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_first, first_pass_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor_first, first_pass_enum, BL_VISIT_ENUM);
  bl_visitor_add(&visitor_first, first_pass_const, BL_VISIT_CONST);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    /* set shared global scope for all anonymous root modules of all units */
    bl_scopes_include_main(&bl_peek_decl_module(cnt.unit->ast.root)->scopes, gscope,
                           cnt.unit->ast.root);
    cnt.curr_compound = cnt.unit->ast.root;
    bl_visitor_walk_module(&visitor_first, cnt.unit->ast.root);
  }

  bl_visitor_t visitor_second;
  bl_visitor_init(&visitor_second, &cnt);
  bl_visitor_add(&visitor_second, second_pass_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_second, second_pass_using, BL_VISIT_USING);
  bl_visitor_add(&visitor_second, BL_SKIP_VISIT, BL_VISIT_CONST);
  bl_visitor_add(&visitor_second, BL_SKIP_VISIT, BL_VISIT_ENUM);
  bl_visitor_add(&visitor_second, BL_SKIP_VISIT, BL_VISIT_FUNC);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor_second, cnt.unit->ast.root);
  }

  bl_visitor_t visitor_third;
  bl_visitor_init(&visitor_third, &cnt);
  bl_visitor_add(&visitor_third, third_pass_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_third, third_pass_block, BL_VISIT_BLOCK);
  bl_visitor_add(&visitor_third, third_pass_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor_third, third_pass_using, BL_VISIT_USING);
  bl_visitor_add(&visitor_third, third_pass_var, BL_VISIT_VAR);
  bl_visitor_add(&visitor_third, third_pass_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_third, third_pass_type, BL_VISIT_TYPE);
  bl_visitor_add(&visitor_third, third_pass_const, BL_VISIT_CONST);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor_third, cnt.unit->ast.root);
  }

  return BL_NO_ERR;
}
