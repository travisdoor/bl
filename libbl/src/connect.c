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
lookup_in_scope(context_t *cnt, bl_node_t *path_elem, bl_node_t *curr_compound,
                bl_node_t **linked_by);

static bl_node_t *
satisfy_decl_ref(context_t *cnt, bl_node_t *ref);

static bl_node_t *
satisfy_type(context_t *cnt, bl_node_t *type);

static void
satisfy_const(context_t *cnt, bl_node_t *cnst);

/*************************************************************************************************
 * Pre-connect
 *************************************************************************************************/
static void
pre_connect_using(bl_visitor_t *visitor, bl_node_t *using);

static void
pre_connect_const(bl_visitor_t *visitor, bl_node_t *cnst);

/*************************************************************************************************
 * Connect
 *************************************************************************************************/
static void
connect_module(bl_visitor_t *visitor, bl_node_t *module);

static void
connect_block(bl_visitor_t *visitor, bl_node_t *block);

static void
connect_func(bl_visitor_t *visitor, bl_node_t *func);

static void
connect_var(bl_visitor_t *visitor, bl_node_t *var);

static void
connect_expr(bl_visitor_t *visitor, bl_node_t *expr);

static void
connect_using(bl_visitor_t *visitor, bl_node_t *using);

static void
connect_type(bl_visitor_t *visitor, bl_node_t *type);

static void
connect_const(bl_visitor_t *visitor, bl_node_t *cnst);

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
  const size_t c             = bo_array_size(path);
  bl_node_t *  path_elem     = bo_array_at(path, 0, bl_node_t *);
  bl_node_t *  linked_by     = NULL;
  bl_node_t *  prev_compound = NULL;
  bl_node_t *  prev_found    = NULL;
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

  if (found_in_curr_branch)
    (*found_in_curr_branch) = prev_compound == found;

  for (size_t i = 1; i < c; ++i) {
    prev_found = found;
    path_elem  = bo_array_at(path, i, bl_node_t *);
    found      = lookup_in_scope(cnt, path_elem, found, &linked_by);

    if (validator)
      validator(cnt, path_elem, found, c == i + 1);
    bl_assert(found, "null found symbol unhandled by validator");

    if (!(bl_ast_try_get_modif(found) & BL_MODIF_PUBLIC) && prev_compound != prev_found) {
      connect_error(cnt, BL_ERR_PRIVATE, path_elem->src,
                    "symbol " BL_YELLOW("'%s'") " is private in this context",
                    bl_peek_path_elem(path_elem)->id.str);
    }
    if (found_in_curr_branch)
      (*found_in_curr_branch) = prev_compound == prev_found;
    prev_compound = found;
  }

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
lookup_in_scope(context_t *cnt, bl_node_t *path_elem, bl_node_t *curr_compound,
                bl_node_t **linked_by)
{
  bl_scopes_t *scopes = bl_ast_try_get_scopes(curr_compound);
  return bl_scopes_get_node(scopes, &bl_peek_path_elem(path_elem)->id, linked_by);
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
  bl_decl_const_t *_cnst    = bl_peek_decl_const(cnst);
  bl_node_t *      conflict = lookup_in_tree(cnt, &_cnst->id, cnt->curr_compound, NULL, NULL);
  if (conflict) {
    connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, cnst->src,
                  "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  _cnst->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  } else {
    bl_scopes_t *scopes = bl_ast_try_get_scopes(cnt->curr_compound);
    bl_scopes_insert_node(scopes, cnst);
  }
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
 * Pre-connect impl
 *************************************************************************************************/
void
pre_connect_using(bl_visitor_t *visitor, bl_node_t *using)
{
  /* solve only usings inside global scope */
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is_not(cnt->curr_compound, BL_DECL_MODULE))
    return;

  include_using(cnt, using);
}

void
pre_connect_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is_not(cnt->curr_compound, BL_DECL_MODULE))
    return;

  satisfy_const(cnt, cnst);
  bl_visitor_walk_const(visitor, cnst);
}

/*************************************************************************************************
 * Connect impl
 *************************************************************************************************/
void
connect_using(bl_visitor_t *visitor, bl_node_t *using)
{
  /* solve only usings in other scopes than modules */
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is(cnt->curr_compound, BL_DECL_MODULE))
    return;

  include_using(cnt, using);
}

void
connect_type(bl_visitor_t *visitor, bl_node_t *type)
{
  context_t *cnt = peek_cnt(visitor);
  satisfy_type(cnt, type);
  bl_visitor_walk_type(visitor, type);
}

void
connect_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *cnt = peek_cnt(visitor);
  if (bl_node_is(cnt->curr_compound, BL_DECL_MODULE)) {
    bl_visitor_walk_const(visitor, cnst);
    return;
  }

  satisfy_const(cnt, cnst);
  bl_visitor_walk_const(visitor, cnst);
}

/* note: same method is used for pre_connect walking too!!! */
void
connect_module(bl_visitor_t *visitor, bl_node_t *module)
{
  context_t *cnt      = peek_cnt(visitor);
  bl_node_t *prev_cmp = cnt->curr_compound;
  cnt->curr_compound  = module;

  bl_visitor_walk_module(visitor, module);

  cnt->curr_compound = prev_cmp;
}

void
connect_block(bl_visitor_t *visitor, bl_node_t *block)
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
connect_func(bl_visitor_t *visitor, bl_node_t *func)
{
  context_t *     cnt      = peek_cnt(visitor);
  bl_decl_func_t *_func    = bl_peek_decl_func(func);
  bl_node_t *     prev_cmp = cnt->curr_compound;

  /* IDEA: extern functions without body can be leaved without scope cache becouse they have no
   * body and can be called only */
  bl_scope_t *main_scope = bl_scope_new(cnt->assembly->scope_cache);
  bl_scopes_include_main(&_func->scopes, main_scope, func);
  cnt->curr_compound = func;

  bl_visitor_walk_func(visitor, func);

  cnt->curr_compound = prev_cmp;
}

void
connect_expr(bl_visitor_t *visitor, bl_node_t *expr)
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
connect_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *    cnt  = peek_cnt(visitor);
  bl_decl_var_t *_var = bl_peek_decl_var(var);

  bl_node_t *conflict = lookup_in_tree(cnt, &_var->id, cnt->curr_compound, NULL, NULL);
  if (conflict) {
    connect_error(cnt, BL_ERR_DUPLICATE_SYMBOL, var->src,
                  "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                  _var->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  } else {
    bl_scopes_t *scopes = bl_ast_try_get_scopes(cnt->curr_compound);
    bl_scopes_insert_node(scopes, var);
  }

  bl_visitor_walk_var(visitor, var);
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

  if (bl_node_is_not(found, BL_DECL_MODULE)) {
    connect_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src, "expected module name in using path");
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
                    "expected type name " BL_YELLOW("'%s'") " is invalid",
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

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    /* free allocated memory on error */
    return (bl_error_e)error;
  }

  bl_visitor_t visitor_pre_connect;
  bl_visitor_init(&visitor_pre_connect, &cnt);
  bl_visitor_add(&visitor_pre_connect, connect_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_pre_connect, pre_connect_using, BL_VISIT_USING);
  bl_visitor_add(&visitor_pre_connect, pre_connect_const, BL_VISIT_CONST);
  bl_visitor_add(&visitor_pre_connect, BL_SKIP_VISIT, BL_VISIT_FUNC);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor_pre_connect, cnt.unit->ast.root);
  }

  bl_visitor_t visitor_connect;
  bl_visitor_init(&visitor_connect, &cnt);
  bl_visitor_add(&visitor_connect, connect_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_connect, connect_block, BL_VISIT_BLOCK);
  bl_visitor_add(&visitor_connect, connect_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_connect, connect_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor_connect, connect_using, BL_VISIT_USING);
  bl_visitor_add(&visitor_connect, connect_var, BL_VISIT_VAR);
  bl_visitor_add(&visitor_connect, connect_type, BL_VISIT_TYPE);
  bl_visitor_add(&visitor_connect, connect_const, BL_VISIT_CONST);

  for (int i = 0; i < c; ++i) {
    cnt.unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_gscope(&visitor_connect, cnt.unit->ast.root);
  }

  return BL_NO_ERR;
}
