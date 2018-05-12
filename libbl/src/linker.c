//************************************************************************************************
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
//************************************************************************************************

/*************************************************************************************************
 * Linker will do 3 partial iterations of AST passed in.
 *
 * 1) merge all modules and prepare scope buffers for symbol finding, it also notify user about
 *    duplicate symbols and name collisions in source after merge
 *
 * 2) connect type tree of custom types (mostly structures) so references to those types can be used
 *    later in compilation
 *    prepare file-global usings
 *
 * 3) link rest of the source (mostly expressions referencing to some custom types)
 *************************************************************************************************/

#include <setjmp.h>
#include <bobject/containers/htbl.h>
#include "common_impl.h"
#include "stages_impl.h"
#include "ast/visitor_impl.h"
#include "block_scope_impl.h"

#define STRUCT_QUEUE_RESERVE 1024
#define EXPECTED_USING_COUNT 32
#define peek_cnt(visitor) ((context_t *)(visitor)->context)

#define link_error(cnt, code, loc, format, ...)                                                    \
  {                                                                                                \
    bl_builder_error((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,           \
                     ##__VA_ARGS__);                                                               \
    longjmp((cnt)->jmp_error, (code));                                                             \
  }

#define link_warning(cnt, loc, format, ...)                                                        \
  {                                                                                                \
    bl_builder_warning((cnt)->builder, "%s:%d:%d " format, loc->file, loc->line, loc->col,         \
                       ##__VA_ARGS__);                                                             \
  }

typedef struct
{
  bl_block_scope_t block_scope;
  bl_builder_t *   builder;
  bl_assembly_t *  assembly;
  jmp_buf          jmp_error;
  bl_scope_t *     gscope;
  bl_scope_t *     mod_scope;
  BHashTable *     local_usings;
  bool             is_in_global_scope;
} context_t;

/* flags used in lookup methods for finding declarations in various scopes */
typedef enum
{
  LOOKUP_GSCOPE       = 1,  // search in global-scope
  LOOKUP_MOD_SCOPE    = 2,  // search in scope of current module
  LOOKUP_BLOCK_SCOPE  = 4,  // search in scope of surrent block (ex.: function body)
  LOOKUP_ENUM_VARIANT = 8,  // search in enum variants
  LOOKUP_USING        = 16, // search in all available usings
} lookup_flag_e;

typedef void (*lookup_elem_valid_f)(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last);

/*************************************************************************************************
 * forward declarations
 *************************************************************************************************/
static bl_node_t *
lookup_node_1(context_t *cnt, BArray *path, bl_scope_t *mod_scope, int scope_flag, int iter,
              bl_node_t *prev_node, lookup_elem_valid_f validator);

static bl_node_t *
lookup_node(context_t *cnt, BArray *path, int scope_flag, lookup_elem_valid_f validator);

static bl_node_t *
lookup_node_in_usings(context_t *cnt, bl_node_t *elem, bl_node_t *prev_found);

static bl_node_t *
satisfy_type(context_t *cnt, bl_node_t *type);

static bl_node_t *
satisfy_member(context_t *cnt, bl_node_t *expr);

static bl_node_t *
satisfy_decl_ref(context_t *cnt, bl_node_t *expr);

static void
pre_link_struct(bl_visitor_t *visitor, bl_node_t *strct);

static void
pre_link_module(bl_visitor_t *visitor, bl_node_t *module);

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

static void
link_module(bl_visitor_t *visitor, bl_node_t *module);

static void
link_expr(bl_visitor_t *visitor, bl_node_t *expr);

static void
link_enum(bl_visitor_t *visitor, bl_node_t *enm);

static void
link_type(bl_visitor_t *visitor, bl_node_t *type);

static void
link_func_args(context_t *cnt, bl_decl_func_t *func);

static void
link_block(bl_visitor_t *visitor, bl_node_t *block);

static void
link_var(bl_visitor_t *visitor, bl_node_t *var);

static void
link_const(bl_visitor_t *visitor, bl_node_t *cnst);

static void
link_fn(bl_visitor_t *visitor, bl_node_t *fn);

static void
link_using(bl_visitor_t *visitor, bl_node_t *using);

static void
valid_using_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last);

static void
valid_call_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last);

/*************************************************************************************************
 * util functions used on multiple places
 *************************************************************************************************/
bl_node_t *
satisfy_type(context_t *cnt, bl_node_t *type)
{
  bl_node_t *found = NULL;
  if (bl_node_is(type, BL_TYPE_REF)) {
    found                       = lookup_node(cnt, bl_peek_type_ref(type)->path,
                        LOOKUP_GSCOPE | LOOKUP_MOD_SCOPE | LOOKUP_USING, NULL);
    bl_peek_type_ref(type)->ref = found;

    switch (bl_node_code(found)) {
    case BL_DECL_STRUCT:
      bl_peek_decl_struct(found)->used++;
      break;
    case BL_DECL_ENUM:
      bl_peek_decl_enum(found)->used++;
      break;
    default:
      link_error(cnt, BL_ERR_INVALID_TYPE, type->src,
                 "unknown type, struct or enum " BL_YELLOW("'%s'"), bl_ast_try_get_id(found)->str);
    }
  }

  return found;
}

bl_node_t *
satisfy_decl_ref(context_t *cnt, bl_node_t *expr)
{
  bl_node_t *found =
      lookup_node(cnt, bl_peek_expr_decl_ref(expr)->path,
                  LOOKUP_GSCOPE | LOOKUP_MOD_SCOPE | LOOKUP_BLOCK_SCOPE | LOOKUP_USING, NULL);

  bl_peek_expr_decl_ref(expr)->ref = found;
  if (bl_node_is(found, BL_DECL_VAR))
    bl_peek_decl_var(found)->used++;
  else if (bl_node_is(found, BL_DECL_CONST))
    bl_peek_decl_const(found)->used++;

  return found;
}

bl_node_t *
satisfy_member(context_t *cnt, bl_node_t *expr)
{
  bl_assert(expr, "invalid expression");
  bl_node_t *found = NULL;
  switch (bl_node_code(expr)) {
  case BL_EXPR_MEMBER_REF: {
    bl_expr_member_ref_t *_member_ref = bl_peek_expr_member_ref(expr);
    bl_assert(_member_ref->next, "missing reference to next expr");
    bl_node_t *type = satisfy_member(cnt, _member_ref->next);
    type            = bl_peek_type_ref(type)->ref;

    /* try to find structure declaration in current module scope, when it is found we can access
     * private members also */
    bool access_priv = bl_scope_get_node(cnt->mod_scope, &bl_peek_decl_struct(type)->id);

    found = bl_scope_get_node(bl_peek_decl_struct(type)->scope, &_member_ref->id);
    if (found == NULL) {
      link_error(cnt, BL_ERR_UNKNOWN_SYMBOL, expr->src,
                 "no such member " BL_YELLOW("'%s'") " in structure " BL_YELLOW("'%s'"),
                 _member_ref->id.str, bl_ast_try_get_id(type)->str);
    }

    /* if current mod_scope contains found structure than private members of the struct can be
     * referenced, otherwise check if the member is public and generate error when it's not */

    bl_decl_struct_member_t *_member = bl_peek_decl_struct_member(found);
    if (!access_priv && _member->modif == BL_MODIF_NONE) {
      link_error(cnt, BL_ERR_PRIVATE, expr->src,
                 "member " BL_YELLOW("'%s'") " of the structure " BL_YELLOW(
                     "'%s'") " is private in context of current module, declared here: %s:%d:%d",
                 _member->id.str, bl_ast_try_get_id(type)->str, found->src->file, found->src->line,
                 found->src->col);
    }

    _member_ref->ref = found;
    found            = _member->type;
    break;
  }

  case BL_EXPR_DECL_REF: {
    /* link decl reference */
    bl_node_t *ref = satisfy_decl_ref(cnt, expr);

    if (bl_node_is_not(ref, BL_DECL_VAR)) {
      link_error(cnt, BL_ERR_INVALID_TYPE, expr->src,
                 BL_YELLOW("'%s'") " is not structure, declared here: %s:%d:%d",
                 bl_ast_try_get_id(ref)->str, ref->src->file, ref->src->line, ref->src->col);
    }

    /* get type */
    found = bl_peek_decl_var(ref)->type;

    if (bl_node_is_not(found, BL_TYPE_REF)) {
      link_error(cnt, BL_ERR_INVALID_TYPE, expr->src,
                 BL_YELLOW("'%s'") " is not structure, declared here: %s:%d:%d",
                 bl_ast_try_get_id(ref)->str, ref->src->file, ref->src->line, ref->src->col);
    }
    break;
  }

  case BL_EXPR_ARRAY_REF: {
    found = satisfy_member(cnt, bl_peek_expr_array_ref(expr)->next);
    break;
  }

  case BL_EXPR_UNARY: {
    found = satisfy_member(cnt, bl_peek_expr_unary(expr)->next);
    break;
  }

  default:
    bl_abort("invalid node %s", bl_node_name(expr));
  }

  return found;
}

/*
 * Search in all curent valid usings in file.
 * When prev_found is not NULL this method will generate warnings about ambiguous
 * symbols caused by using directives and return prev_found.
 * When pre_found is set to NULL method will find elem->id if there is one and return
 * first found.
 */
bl_node_t *
lookup_node_in_usings(context_t *cnt, bl_node_t *elem, bl_node_t *prev_found)
{
  /* local (function) usings */
  bl_log("looking for symbol in usings references");

  bo_iterator_t iter   = bo_htbl_begin(cnt->local_usings);
  bo_iterator_t end    = bo_htbl_end(cnt->local_usings);
  bl_node_t *   module = NULL;

  while (!bo_iterator_equal(&iter, &end)) {
    module = (bl_node_t *)bo_htbl_iter_peek_key(cnt->local_usings, &iter);
    bo_htbl_iter_next(cnt->local_usings, &iter);
    bl_assert(module, "invalid module in using cache");

    bl_node_t *found =
        bl_scope_get_node(bl_peek_decl_module(module)->scope, &bl_peek_path_elem(elem)->id);
    if (!prev_found && found) {
      prev_found = found;
    } else if (found) {
      link_warning(cnt, elem->src,
                   BL_YELLOW("'%s'") " is ambiguous, first found here: %s:%d:%d will be used. "
                                     "Colliding symbol with same name found here: %s:%d:%d. This "
                                     "can be caused by using directive.",
                   bl_peek_path_elem(elem)->id.str, prev_found->src->file, prev_found->src->line,
                   prev_found->src->col, found->src->file, found->src->line, found->src->col);
    }
  }

  return prev_found;
}

bl_node_t *
lookup_node_1(context_t *cnt, BArray *path, bl_scope_t *mod_scope, int scope_flag, int iter,
              bl_node_t *prev_node, lookup_elem_valid_f validator)
{
  bl_node_t *found     = NULL;
  bl_node_t *path_elem = bo_array_at(path, iter, bl_node_t *);

  /* search symbol in enum declaration */
  if (scope_flag & LOOKUP_ENUM_VARIANT) {
    bl_assert(prev_node, "invalid prev node");
    found =
        bl_scope_get_node(bl_peek_decl_enum(prev_node)->scope, &bl_peek_path_elem(path_elem)->id);
  }

  /* search symbol in block scope */
  if (scope_flag & LOOKUP_BLOCK_SCOPE && !found) {
    found = bl_block_scope_get_node(&cnt->block_scope, &bl_peek_path_elem(path_elem)->id);
  }

  /* search symbol in module scope */
  if (scope_flag & LOOKUP_MOD_SCOPE && !found) {
    found = bl_scope_get_node(mod_scope, &bl_peek_path_elem(path_elem)->id);
  }

  /* search symbol in global scope */
  if (scope_flag & LOOKUP_GSCOPE && !found) {
    found = bl_scope_get_node(cnt->gscope, &bl_peek_path_elem(path_elem)->id);
  }

  /* search symbol in using cache of local or global file scope if there is one */
  if (scope_flag & LOOKUP_USING) {
    found = lookup_node_in_usings(cnt, path_elem, found);
  }

  if (found == NULL) {
    link_error(cnt, BL_ERR_UNKNOWN_SYMBOL, path_elem->src, "unknown symbol " BL_YELLOW("'%s'"),
               bl_peek_path_elem(path_elem)->id.str);
  }

  iter++;

  if (mod_scope != cnt->mod_scope && !(bl_ast_try_get_modif(found) & BL_MODIF_PUBLIC)) {
    link_error(cnt, BL_ERR_PRIVATE, path_elem->src,
               "symbol " BL_YELLOW("'%s'") " is private, declared here: %s:%d:%d",
               bl_peek_path_elem(path_elem)->id.str, found->src->file, found->src->line,
               found->src->col);
  }

  if (validator != NULL) {
    validator(cnt, path_elem, found, iter == bo_array_size(path));
  }

  if (bl_node_is(found, BL_DECL_MODULE)) { /* found another module */
    if (iter == bo_array_size(path)) {
      return found;
    }

    return lookup_node_1(cnt, path, bl_peek_decl_module(found)->scope, LOOKUP_MOD_SCOPE, iter,
                         found, validator);

  } else if (bl_node_is(found, BL_DECL_ENUM)) { /* found enum */
    /* last in path -> return enum declaration */
    if (iter == bo_array_size(path))
      return found;

    /* not last in path -> we need to determinate enum variant */
    return lookup_node_1(cnt, path, mod_scope, LOOKUP_ENUM_VARIANT, iter, found, validator);
  } else {
    bl_assert(iter == bo_array_size(path), "invalid path, last found is %s", bl_node_name(found));
    return found;
  }

  return NULL;
}

/*************************************************************************************************
 * declaration merging
 * all declaration in corresponding modules must be merged due to lack of header files and
 * context free grammar
 *************************************************************************************************/
void
merge_func(bl_visitor_t *visitor, bl_node_t *func)
{
  bl_decl_func_t *_func = bl_peek_decl_func(func);
  context_t *     cnt   = peek_cnt(visitor);
  bl_scope_t *    scope = peek_cnt(visitor)->mod_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_func->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, func->src,
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
  bl_scope_t *     scope = peek_cnt(visitor)->mod_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_cnst->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, cnst->src,
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
  bl_scope_t *      scope  = peek_cnt(visitor)->mod_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_strct->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, strct->src,
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
      link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, member->src,
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
  bl_scope_t *    scope = peek_cnt(visitor)->mod_scope;

  bl_node_t *conflict = bl_scope_get_node(scope, &_enm->id);

  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, enm->src,
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
      link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, variant->src,
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
  bl_scope_t *prev_scope_tmp = cnt->mod_scope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");
  bl_decl_module_t *_module  = bl_peek_decl_module(module);
  bl_node_t *       conflict = bl_scope_get_node(prev_scope_tmp, &_module->id);

  if (conflict) {
    if (bl_ast_try_get_modif(module) != bl_ast_try_get_modif(conflict)) {
      link_error(peek_cnt(visitor), BL_ERR_UNCOMPATIBLE_MODIF, module->src,
                 "previous declaration of module " BL_YELLOW(
                     "'%s'") " has different access modifier, originally declared here: %s:%d:%d",
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

/*************************************************************************************************
 * satisfy structure types stored in cache
 * note: check all cached structures, link members with reference to other structures and
 * clear cache
 *************************************************************************************************/
void
pre_link_struct(bl_visitor_t *visitor, bl_node_t *strct)
{
  context_t *              cnt    = peek_cnt(visitor);
  bl_decl_struct_t *       _strct = bl_peek_decl_struct(strct);
  const size_t             c      = bl_ast_struct_member_count(_strct);
  bl_node_t *              member;
  bl_decl_struct_member_t *_member;
  for (size_t i = 0; i < c; ++i) {
    member  = bl_ast_struct_get_member(_strct, i);
    _member = bl_peek_decl_struct_member(member);
    satisfy_type(cnt, _member->type);
  }
}

void
pre_link_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_scope_t *prev_scope_tmp = peek_cnt(visitor)->mod_scope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");

  peek_cnt(visitor)->mod_scope = bl_peek_decl_module(module)->scope;
  bl_assert(peek_cnt(visitor)->mod_scope, "invalid next scope");

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->mod_scope = prev_scope_tmp;
}

bl_node_t *
lookup_node(context_t *cnt, BArray *path, int scope_flag, lookup_elem_valid_f validator)
{
  return lookup_node_1(cnt, path, cnt->mod_scope, scope_flag, 0, NULL, validator);
}

/*************************************************************************************************
 * link expresions
 *************************************************************************************************/
void
link_module(bl_visitor_t *visitor, bl_node_t *module)
{
  bl_scope_t *prev_scope_tmp = peek_cnt(visitor)->mod_scope;
  bl_assert(prev_scope_tmp, "invalid current scope in linker");

  peek_cnt(visitor)->mod_scope = bl_peek_decl_module(module)->scope;
  bl_assert(peek_cnt(visitor)->mod_scope, "invalid next scope");

  bl_visitor_walk_module(visitor, module);
  peek_cnt(visitor)->mod_scope = prev_scope_tmp;
}

void
link_expr(bl_visitor_t *visitor, bl_node_t *expr)
{
  bl_node_t *found = NULL;
  context_t *cnt   = peek_cnt(visitor);

  switch (bl_node_code(expr)) {
  case BL_EXPR_CALL:
    found = lookup_node(cnt, bl_peek_expr_call(expr)->path,
                        LOOKUP_GSCOPE | LOOKUP_MOD_SCOPE | LOOKUP_USING, valid_call_elem);

    bl_peek_expr_call(expr)->ref = found;
    bl_peek_decl_func(found)->used++;
    break;
  case BL_EXPR_DECL_REF:
    if (bl_peek_expr_decl_ref(expr)->ref == NULL)
      satisfy_decl_ref(cnt, expr);
    break;
  case BL_EXPR_MEMBER_REF:
    /* member access expression has not been linked yet -> solve it recursivelly */
    if (bl_peek_expr_member_ref(expr)->ref == NULL)
      satisfy_member(cnt, expr);

    break;
  default:
    break;
  }

  bl_visitor_walk_expr(visitor, expr);
}

void
link_enum(bl_visitor_t *visitor, bl_node_t *enm)
{
  context_t *     cnt  = peek_cnt(visitor);
  bl_decl_enum_t *_enm = bl_peek_decl_enum(enm);

  bl_scope_t *prev_scope_tmp = cnt->mod_scope;
  cnt->mod_scope             = _enm->scope;
  bl_assert(cnt->mod_scope, "invalid next scope");

  bl_visitor_walk_enum(visitor, enm);
  cnt->mod_scope = prev_scope_tmp;
}

void
link_type(bl_visitor_t *visitor, bl_node_t *type)
{
  context_t *cnt = peek_cnt(visitor);
  satisfy_type(cnt, type);
  bl_visitor_walk_type(visitor, type);
}

void
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
                 "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
                 bl_peek_decl_var(arg)->id.str, conflict->src->file, conflict->src->line,
                 conflict->src->col);
    } else {
      bl_block_scope_insert_node(&cnt->block_scope, arg);
    }
  }
}

void
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

void
link_var(bl_visitor_t *visitor, bl_node_t *var)
{
  context_t *    cnt  = peek_cnt(visitor);
  bl_decl_var_t *_var = bl_peek_decl_var(var);

  bl_node_t *conflict = bl_block_scope_get_node(&cnt->block_scope, &_var->id);
  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, var->src,
               "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
               _var->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  } else {
    bl_block_scope_insert_node(&cnt->block_scope, var);
  }

  bl_visitor_walk_var(visitor, var);
}

void
link_fn(bl_visitor_t *visitor, bl_node_t *fn)
{
  context_t *cnt = peek_cnt(visitor);
  bo_htbl_clear(cnt->local_usings);
  cnt->is_in_global_scope = false;
  bl_visitor_walk_func(visitor, fn);
  cnt->is_in_global_scope = true;
}

void
link_const(bl_visitor_t *visitor, bl_node_t *cnst)
{
  context_t *cnt = peek_cnt(visitor);
  if (cnt->is_in_global_scope) {
    bl_visitor_walk_const(visitor, cnst);
    return;
  }

  bl_decl_const_t *_cnst = bl_peek_decl_const(cnst);

  bl_node_t *conflict = bl_block_scope_get_node(&cnt->block_scope, &_cnst->id);
  if (conflict) {
    link_error(cnt, BL_ERR_DUPLICATE_SYMBOL, cnst->src,
               "duplicate symbol " BL_YELLOW("'%s'") " already declared here: %s:%d:%d",
               _cnst->id.str, conflict->src->file, conflict->src->line, conflict->src->col);
  } else {
    bl_block_scope_insert_node(&cnt->block_scope, cnst);
  }

  bl_visitor_walk_const(visitor, cnst);
}

/* validation of elements found during using path linking */
void
valid_using_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last)
{
  if (bl_node_is_not(found, BL_DECL_MODULE)) {
    link_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src,
               "expeced module name in using path, " BL_YELLOW(
                   "'%s'") " is invalid, declared here %s:%d:%d",
               bl_ast_try_get_id(found)->str, found->src->file, found->src->line, found->src->col);
  }
}

/* validation of elements found during expr-call path linking */
void
valid_call_elem(context_t *cnt, bl_node_t *elem, bl_node_t *found, bool last)
{
  if (last) {
    if (bl_node_is_not(found, BL_DECL_FUNC)) {
      /* accept functions only */
      link_error(cnt, BL_ERR_EXPECTED_FUNC, elem->src,
                 "expeced function name, " BL_YELLOW("'%s'") " is invalid, declared here %s:%d:%d",
                 bl_ast_try_get_id(found)->str, found->src->file, found->src->line,
                 found->src->col);
    }
  } else {
    /* accept modules only */
    if (bl_node_is_not(found, BL_DECL_MODULE)) {
      /* accept functions only */
      link_error(cnt, BL_ERR_EXPECTED_MODULE, elem->src,
                 "expeced module name, " BL_YELLOW("'%s'") " is invalid, declared here %s:%d:%d",
                 bl_ast_try_get_id(found)->str, found->src->file, found->src->line,
                 found->src->col);
    }
  }
}

void
link_using(bl_visitor_t *visitor, bl_node_t *using)
{
  context_t *      cnt    = peek_cnt(visitor);
  bl_stmt_using_t *_using = bl_peek_stmt_using(using);
  bl_assert(_using->path, "invalid path in using statement");
  bl_node_t *module = lookup_node(cnt, _using->path, LOOKUP_GSCOPE, valid_using_elem);
  bl_assert(module, "unhandled compiler error for module lookup fail");

  _using->ref = module;
  if (!cnt->is_in_global_scope && !bo_htbl_has_key(cnt->local_usings, (uint64_t)module)) {
    /* using is defined in function scope and should live only in function body, we also don't
     * want to have duplicit module references in curr_using cache due to performance */
    bo_htbl_insert_empty(cnt->local_usings, (uint64_t)module);
  }
  bl_log("global scope %d", cnt->is_in_global_scope);
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_linker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder   = builder,
                   .assembly  = assembly,
                   .mod_scope = assembly->scope,
                   .gscope    = assembly->scope};

  bl_block_scope_init(&cnt.block_scope);
  cnt.local_usings = bo_htbl_new(0, EXPECTED_USING_COUNT);

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    /* free allocated memory on error */
    bl_block_scope_terminate(&cnt.block_scope);
    return (bl_error_e)error;
  }

  /* 1) merge all modules and check for duplicity */
  bl_visitor_t visitor_merge;
  bl_visitor_init(&visitor_merge, &cnt);
  bl_visitor_add(&visitor_merge, merge_func, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_merge, merge_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_merge, merge_struct, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor_merge, merge_const, BL_VISIT_CONST);
  bl_visitor_add(&visitor_merge, merge_enum, BL_VISIT_ENUM);

  const int  c           = bl_assembly_get_unit_count(assembly);
  bl_unit_t *unit        = NULL;
  cnt.is_in_global_scope = true;

  for (int i = 0; i < c; ++i) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_merge, unit->ast.root);
  }

  /* 2) build structure type tree references */
  bl_visitor_t visitor_pre_link;
  bl_visitor_init(&visitor_pre_link, &cnt);
  bl_visitor_add(&visitor_pre_link, pre_link_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_pre_link, BL_SKIP_VISIT, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_pre_link, BL_SKIP_VISIT, BL_VISIT_ENUM);
  bl_visitor_add(&visitor_pre_link, pre_link_struct, BL_VISIT_STRUCT);

  cnt.is_in_global_scope = true;
  for (int i = 0; i < c; ++i) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_pre_link, unit->ast.root);
  }

  /* 3) link the rest */
  bl_visitor_t visitor_link;
  bl_visitor_init(&visitor_link, &cnt);
  bl_visitor_add(&visitor_link, link_module, BL_VISIT_MODULE);
  bl_visitor_add(&visitor_link, link_expr, BL_VISIT_EXPR);
  bl_visitor_add(&visitor_link, link_type, BL_VISIT_TYPE);
  bl_visitor_add(&visitor_link, link_block, BL_VISIT_BLOCK);
  bl_visitor_add(&visitor_link, link_var, BL_VISIT_VAR);
  bl_visitor_add(&visitor_link, link_fn, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_link, link_const, BL_VISIT_CONST);
  bl_visitor_add(&visitor_link, link_enum, BL_VISIT_ENUM);
  bl_visitor_add(&visitor_link, link_using, BL_VISIT_USING);

  cnt.is_in_global_scope = true;
  for (int i = 0; i < c; ++i) {
    unit = bl_assembly_get_unit(assembly, i);
    bl_visitor_walk_module(&visitor_link, unit->ast.root);
  }

  bl_block_scope_terminate(&cnt.block_scope);
  bo_unref(cnt.local_usings);

  return BL_NO_ERR;
}
