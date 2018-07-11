//************************************************************************************************
// bl
//
// File:   checker.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include <setjmp.h>
#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#define check_error(cnt, code, tok, pos, format, ...)                                              \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), &(tok)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_error_node(cnt, code, node, pos, format, ...)                                        \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_ERROR, (code), (node)->src, (pos), (format),         \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, &(tok)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

#define check_warning_node(cnt, node, pos, format, ...)                                            \
  {                                                                                                \
    bl_builder_msg((cnt)->builder, BL_BUILDER_WARNING, 0, (node)->src, (pos), (format),            \
                   ##__VA_ARGS__);                                                                 \
  }

typedef BHashTable waiting_t;
typedef BArray     waiting_queue_t;
typedef BArray     flatten_cache_t;
typedef BArray     flatten_t;

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;

  flatten_cache_t *flatten_cache;
  waiting_t *      waiting;
} context_t;

typedef struct
{
  flatten_t *flatten;
  size_t     i;
} fiter_t;

static bl_node_t *
lookup(bl_node_t *ident, bl_scope_t **out_scope);

static bl_node_t *
_lookup(bl_node_t *compound, bl_node_t *ident, bl_scope_t **out_scope);

static void
provide(bl_node_t *ident, bl_node_t *provided);

static waiting_t *
waiting_new(void);

static void
waiting_delete(waiting_t *waiting);

static inline void
waiting_push(waiting_t *waiting, uint64_t hash, fiter_t fiter);

static void
waiting_resume(context_t *cnt, uint64_t hash);

static flatten_cache_t *
flatten_cache_new(void);

static void
flatten_cache_delete(flatten_cache_t *cache);

static flatten_t *
flatten_new(flatten_cache_t *cache);

static inline void
flatten_push(flatten_t *flatten, bl_node_t *node);

static void
flatten_node(context_t *cnt, flatten_t *flatten, bl_node_t *node);

static void
check_flatten(context_t *cnt, bl_node_t *node);

static void
check_ublock(context_t *cnt, bl_node_t *node);

static void
check_block(context_t *cnt, bl_node_t *node);

static bool
check_node(context_t *cnt, bl_node_t *node);

static bool
check_ident(context_t *cnt, bl_node_t *ident);

static bool
check_decl_value(context_t *cnt, bl_node_t *decl);

static bool
check_lit_fn(context_t *cnt, bl_node_t *fn);

static void
check_unresolved(context_t *cnt);

// impl
bl_node_t *
lookup(bl_node_t *ident, bl_scope_t **out_scope)
{
  assert(ident);
  bl_node_t *compound = bl_peek_ident(ident)->parent_compound;
  assert(compound);
  return _lookup(compound, ident, out_scope);
}

bl_node_t *
_lookup(bl_node_t *compound, bl_node_t *ident, bl_scope_t **out_scope)
{
  bl_node_t * found = NULL;
  bl_scope_t *scope;

  while (compound && !found) {
    scope = bl_ast_get_scope(compound);
    assert(scope);

    found    = bl_scope_get(scope, ident);
    compound = bl_ast_get_parent_compound(compound);
  }

  if (out_scope) *out_scope = scope;
  return found;
}

void
provide(bl_node_t *ident, bl_node_t *provided)
{
  assert(ident && provided);
  bl_node_t *compound = bl_peek_ident(ident)->parent_compound;
  assert(compound);
  bl_scope_t *scope = bl_ast_get_scope(compound);
  assert(scope);

  bl_scope_insert(scope, ident, provided);
}

waiting_t *
waiting_new(void)
{
  return bo_htbl_new(sizeof(waiting_queue_t *), 2048);
}

void
waiting_delete(waiting_t *waiting)
{
  bo_iterator_t it;
  bl_bhtbl_foreach(waiting, it)
  {
    waiting_queue_t *q = bo_htbl_iter_peek_value(waiting, &it, waiting_queue_t *);
    bo_unref(q);
  }
  bo_unref(waiting);
}

void
waiting_push(waiting_t *waiting, uint64_t hash, fiter_t fiter)
{
  waiting_queue_t *queue = NULL;
  if (bo_htbl_has_key(waiting, hash)) {
    queue = bo_htbl_at(waiting, hash, waiting_queue_t *);
  } else {
    queue = bo_array_new(sizeof(fiter_t));
    bo_htbl_insert(waiting, hash, queue);
  }

  bo_array_push_back(queue, fiter);
}

void
waiting_resume(context_t *cnt, uint64_t hash)
{
  if (!bo_htbl_has_key(cnt->waiting, hash)) return;
  waiting_queue_t *q = bo_htbl_at(cnt->waiting, hash, waiting_queue_t *);
  assert(q);

  fiter_t fit;
  for (size_t i = 0; i < bo_array_size(q); ++i) {
    fit = bo_array_at(q, i, fiter_t);
    /* resume here */
    bl_node_t *tmp;
    for (; fit.i < bo_array_size(fit.flatten); ++fit.i) {
      tmp = bo_array_at(fit.flatten, fit.i, bl_node_t *);
      // bl_log("check %s (%p)", bl_node_name(tmp), tmp);
      if (!check_node(cnt, tmp)) {
        bl_abort("invalid check");
      }
    }
  }
  bo_htbl_erase_key(cnt->waiting, hash);
}

void
check_unresolved(context_t *cnt)
{
  bo_iterator_t    iter;
  waiting_queue_t *q;
  fiter_t          tmp;
  bl_node_t *      tmp_node;

  bl_bhtbl_foreach(cnt->waiting, iter)
  {
    q = bo_htbl_iter_peek_value(cnt->waiting, &iter, waiting_queue_t *);
    assert(q);

    for (size_t i = 0; i < bo_array_size(q); ++i) {
      tmp      = bo_array_at(q, i, fiter_t);
      tmp_node = bo_array_at(tmp.flatten, tmp.i, bl_node_t *);
      bl_scope_t *found_in_scope;
      bl_node_t * found = lookup(tmp_node, &found_in_scope);
      if (!found) {
        check_error_node(cnt, BL_ERR_UNKNOWN_SYMBOL, tmp_node, BL_BUILDER_CUR_WORD,
                         "unknown symbol");
      } else if ((found_in_scope != cnt->assembly->gscope) && found->serial > tmp_node->serial) {
        check_error_node(cnt, BL_ERR_UNKNOWN_SYMBOL, tmp_node, BL_BUILDER_CUR_WORD,
                         "symbol defined after place where it is used, declarations in local scope "
                         "must come before they are used");
      }
    }
  }
}

flatten_cache_t *
flatten_cache_new(void)
{
  return bo_array_new(sizeof(flatten_t *));
}

void
flatten_cache_delete(flatten_cache_t *cache)
{
  flatten_t *it;

  bl_barray_foreach(cache, it)
  {
    bo_unref(it);
  }

  bo_unref(cache);
}

flatten_t *
flatten_new(flatten_cache_t *cache)
{
  flatten_t *tmp = bo_array_new(sizeof(bl_node_t *));
  bo_array_push_back(cache, tmp);
  return tmp;
}

void
flatten_push(flatten_t *flatten, bl_node_t *node)
{
  bo_array_push_back(flatten, node);
}

void
flatten_node(context_t *cnt, flatten_t *flatten, bl_node_t *node)
{
  if (!node) return;

  switch (bl_node_code(node)) {
  case BL_NODE_DECL_VALUE: {
    bl_node_decl_value_t *_decl = bl_peek_decl_value(node);

    bl_node_t *conflict = lookup(_decl->name, NULL);
    if (conflict) {
      check_error_node(cnt, BL_ERR_DUPLICATE_SYMBOL, node, BL_BUILDER_CUR_WORD,
                       "symbol with same name already declared here: %s:%d",
                       conflict->src->unit->filepath, conflict->src->line);
    } else {
      provide(_decl->name, node);
    }

    flatten_node(cnt, flatten, _decl->type);
    flatten_node(cnt, flatten, _decl->value);
    break;
  }

  case BL_NODE_TYPE_FN: {
    bl_node_type_fn_t *_type_fn = bl_peek_type_fn(node);
    flatten_node(cnt, flatten, _type_fn->ret_type);
    bl_node_t *sub_type;
    bl_node_foreach(_type_fn->arg_types, sub_type)
    {
      flatten_node(cnt, flatten, sub_type);
    }
    break;
  }

  case BL_NODE_LIT_FN: {
    bl_node_lit_fn_t *_fn = bl_peek_lit_fn(node);
    flatten_node(cnt, flatten, _fn->type);
    flatten_node(cnt, flatten, _fn->block);
    break;
  }

  case BL_NODE_DECL_BLOCK: {
    check_block(cnt, node);
    return;
  }

  case BL_NODE_IDENT:
  case BL_NODE_STMT_BAD:
  case BL_NODE_STMT_RETURN:
  case BL_NODE_STMT_IF:
  case BL_NODE_STMT_LOOP:
  case BL_NODE_DECL_BAD:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_TYPE_STRUCT:
  case BL_NODE_TYPE_BAD:
  case BL_NODE_LIT:
  case BL_NODE_EXPR_BINOP:
  case BL_NODE_EXPR_CALL:
  case BL_NODE_EXPR_BAD:
  case BL_NODE_COUNT:
    break;
  default:
    bl_abort("invalid node %s", bl_node_name(node));
  }

  flatten_push(flatten, node);
}

void
check_flatten(context_t *cnt, bl_node_t *node)
{
  fiter_t fit;
  fit.flatten = flatten_new(cnt->flatten_cache);
  fit.i       = 0;

  flatten_node(cnt, fit.flatten, node);

  bl_node_t *tmp;
  for (; fit.i < bo_array_size(fit.flatten); ++fit.i) {
    tmp = bo_array_at(fit.flatten, fit.i, bl_node_t *);
    // bl_log("check %s (%p)", bl_node_name(tmp), tmp);
    if (!check_node(cnt, tmp)) {
      /* node has not been satisfied and need to be checked later when all it's references comes
       * out */
      bl_node_ident_t *_ident = bl_peek_ident(tmp);
      waiting_push(cnt->waiting, _ident->hash, fit);
      break;
    }
  }
}

void
check_block(context_t *cnt, bl_node_t *node)
{
  /* entry point */

  bl_node_decl_block_t *_block = bl_peek_decl_block(node);

  bl_node_t *child;
  bl_node_foreach(_block->nodes, child)
  {
    check_flatten(cnt, child);
  }
}

void
check_ublock(context_t *cnt, bl_node_t *node)
{
  /* entry point */
  bl_node_decl_ublock_t *_ublock = bl_peek_decl_ublock(node);

  bl_node_t *child;
  bl_node_foreach(_ublock->nodes, child)
  {
    check_flatten(cnt, child);
  }
}

bool
check_node(context_t *cnt, bl_node_t *node)
{
  assert(node);
  switch (bl_node_code(node)) {
  case BL_NODE_IDENT:
    return check_ident(cnt, node);

  case BL_NODE_DECL_VALUE:
    return check_decl_value(cnt, node);

  case BL_NODE_LIT_FN:
    return check_lit_fn(cnt, node);

  case BL_NODE_DECL_UBLOCK:
  case BL_NODE_STMT_BAD:
  case BL_NODE_STMT_RETURN:
  case BL_NODE_STMT_IF:
  case BL_NODE_STMT_LOOP:
  case BL_NODE_DECL_BLOCK:
  case BL_NODE_DECL_BAD:
  case BL_NODE_TYPE_FUND:
  case BL_NODE_TYPE_FN:
  case BL_NODE_TYPE_STRUCT:
  case BL_NODE_TYPE_BAD:
  case BL_NODE_LIT:
  case BL_NODE_EXPR_BINOP:
  case BL_NODE_EXPR_CALL:
  case BL_NODE_EXPR_BAD:
  case BL_NODE_COUNT:
    break;
  }

  return true; /* assert later? */
}

bool
check_lit_fn(context_t *cnt, bl_node_t *fn)
{
  if (fn->checked) return true;
  fn->checked = true;
  return true;
}

bool
check_ident(context_t *cnt, bl_node_t *ident)
{
  bl_node_ident_t *_ident = bl_peek_ident(ident);
  if (_ident->ref) return true;

  bl_node_t *found;
  const int  buildin = bl_ast_is_buildin_type(ident);
  if (buildin != -1) {
    found = &bl_ftypes[buildin];
  } else {
    found = lookup(ident, NULL);
    if (!found) return false;
    if (!found->checked) return false;
  }
  _ident->ref = found;

  return true;
}

bool
check_decl_value(context_t *cnt, bl_node_t *decl)
{
  if (decl->checked) return true;
  bl_node_decl_value_t *_decl      = bl_peek_decl_value(decl);
  bl_node_t *           value_type = NULL;

  assert(_decl->name);
  const bool is_in_gscope =
      bl_ast_get_scope(bl_peek_ident(_decl->name)->parent_compound) == cnt->assembly->gscope;

  assert(_decl->type || _decl->value);

  if (_decl->value) value_type = bl_ast_get_type(_decl->value);
  if (_decl->type) _decl->type = bl_ast_get_type(_decl->type);

  if (value_type && _decl->type && !bl_ast_type_cmp(value_type, _decl->type)) {
    char tmp_value[256];
    char tmp_decl[256];
    bl_ast_type_to_string(tmp_decl, 256, _decl->type);
    bl_ast_type_to_string(tmp_value, 256, value_type);
    check_error_node(cnt, BL_ERR_INVALID_TYPE, _decl->value, BL_BUILDER_CUR_WORD,
                     "no implicit cast for types '%s' and '%s'", tmp_decl, tmp_value);
  }

  if (value_type) {
    _decl->type = value_type;
  }

  assert(_decl->type);
  decl->checked = true;
  if (is_in_gscope) waiting_resume(cnt, bl_peek_ident(_decl->name)->hash);
  return true;
}

void
bl_checker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder       = builder,
                   .unit          = NULL,
                   .assembly      = assembly,
                   .ast           = NULL,
                   .waiting       = waiting_new(),
                   .flatten_cache = flatten_cache_new()};

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_ublock(&cnt, unit->ast.root);
  }

  check_unresolved(&cnt);

  flatten_cache_delete(cnt.flatten_cache);
  waiting_delete(cnt.waiting);
}
