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

typedef BArray flatten_t;
typedef BArray waiting_t;
typedef BArray flatten_cache_t;

typedef struct
{
  bl_builder_t * builder;
  bl_assembly_t *assembly;
  bl_unit_t *    unit;
  bl_ast_t *     ast;

  bl_node_t *      curr_compound;
  flatten_cache_t *flatten_cache;
  waiting_t *      waiting;
} context_t;

typedef struct
{
  flatten_t *flatten;
  size_t     i;
} fiter_t;

static waiting_t *
waiting_new(void);

static void
waiting_delete(waiting_t *waiting);

static inline void
waiting_push(waiting_t *waiting, fiter_t fiter);

static flatten_cache_t *
flatten_cache_new(void);

static void
flatten_cache_delete(flatten_cache_t *cache);

static flatten_t *
flatten_new(flatten_cache_t *cache);

static inline void
flatten_push(flatten_t *flatten, bl_node_t *node);

static void
flatten_node(flatten_t *flatten, bl_node_t *node);

static void
check_ublock(context_t *cnt, bl_node_t *node);

static void
check_waiting(context_t *cnt);

static bool
check_node(context_t *cnt, bl_node_t *node);

static bool
check_ident(context_t *cnt, bl_node_t *ident);

static bool
check_decl_value(context_t *cnt, bl_node_t *decl);

// impl
waiting_t *
waiting_new(void)
{
  return bo_array_new(sizeof(fiter_t));
}

static void
waiting_delete(waiting_t *waiting)
{
  bo_unref(waiting);
}

void
waiting_push(waiting_t *waiting, fiter_t fiter)
{
  bo_array_push_back(waiting, fiter);
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
flatten_node(flatten_t *flatten, bl_node_t *node)
{
  if (!node) return;

  switch (bl_node_code(node)) {
  case BL_NODE_IDENT:
    break;
  case BL_NODE_STMT_BAD:
    break;
  case BL_NODE_STMT_RETURN:
    break;
  case BL_NODE_STMT_IF:
    break;
  case BL_NODE_STMT_LOOP:
    break;
  case BL_NODE_DECL_VALUE: {
    bl_node_decl_value_t *_decl = bl_peek_decl_value(node);
    flatten_node(flatten, _decl->type);
    flatten_node(flatten, _decl->value);
    break;
  }
  case BL_NODE_DECL_BLOCK:
    break;
  case BL_NODE_DECL_BAD:
    break;
  case BL_NODE_TYPE_FUND:
    break;
  case BL_NODE_TYPE_FN:
    break;
  case BL_NODE_TYPE_STRUCT:
    break;
  case BL_NODE_TYPE_BAD:
    break;
  case BL_NODE_LIT_FN:
    break;
  case BL_NODE_LIT:
    break;
  case BL_NODE_EXPR_BINOP:
    break;
  case BL_NODE_EXPR_CALL:
    break;
  case BL_NODE_EXPR_BAD:
    break;
  case BL_NODE_COUNT:
    break;
  default:
    bl_abort("invalid node %s", bl_node_name(node));
  }

  flatten_push(flatten, node);
}

void
check_ublock(context_t *cnt, bl_node_t *node)
{
  bl_node_decl_ublock_t *_ublock = bl_peek_decl_ublock(node);
  _ublock->scope                 = cnt->assembly->gscope;
  cnt->curr_compound             = node;

  fiter_t    fit;
  bl_node_t *child;
  bl_node_foreach(_ublock->nodes, child)
  {
    fit.flatten = flatten_new(cnt->flatten_cache);
    fit.i       = 0;

    flatten_node(fit.flatten, child);

    bl_node_t *tmp;
    for (; fit.i < bo_array_size(fit.flatten); ++fit.i) {
      tmp = bo_array_at(fit.flatten, fit.i, bl_node_t *);
      bl_log("check %s (%p)", bl_node_name(tmp), tmp);
      if (!check_node(cnt, tmp)) {
        /* node has not been satisfied and need to be checked later when all it's references comes
         * out */
        bl_log("node not satisfied");
        waiting_push(cnt->waiting, fit);
        break;
      }
    }
  }
}

void
check_waiting(context_t *cnt)
{
  fiter_t fit;
  for (size_t i = bo_array_size(cnt->waiting); i-- > 0;) {
    fit = bo_array_at(cnt->waiting, i, fiter_t);
    /* resume here */
    bl_node_t *tmp;
    for (; fit.i < bo_array_size(fit.flatten); ++fit.i) {
      tmp = bo_array_at(fit.flatten, fit.i, bl_node_t *);
      bl_log("check %s (%p)", bl_node_name(tmp), tmp);
      if (!check_node(cnt, tmp)) {
        /* node has not been satisfied and need to be checked later when all it's references comes
         * out */
        bl_log("unknown symbol");
      }
    }
  }
}

bool
check_node(context_t *cnt, bl_node_t *node)
{
  assert(node);
  switch (bl_node_code(node)) {
  case BL_NODE_DECL_UBLOCK:
    break;
  case BL_NODE_IDENT:
    return check_ident(cnt, node);
  case BL_NODE_STMT_BAD:
    break;
  case BL_NODE_STMT_RETURN:
    break;
  case BL_NODE_STMT_IF:
    break;
  case BL_NODE_STMT_LOOP:
    break;
  case BL_NODE_DECL_VALUE:
    return check_decl_value(cnt, node);
  case BL_NODE_DECL_BLOCK:
    break;
  case BL_NODE_DECL_BAD:
    break;
  case BL_NODE_TYPE_FUND:
    break;
  case BL_NODE_TYPE_FN:
    break;
  case BL_NODE_TYPE_STRUCT:
    break;
  case BL_NODE_TYPE_BAD:
    break;
  case BL_NODE_LIT_FN:
    break;
  case BL_NODE_LIT:
    break;
  case BL_NODE_EXPR_BINOP:
    break;
  case BL_NODE_EXPR_CALL:
    break;
  case BL_NODE_EXPR_BAD:
    break;
  case BL_NODE_COUNT:
    break;
  }

  return true; /* assert later? */
}

bool
check_ident(context_t *cnt, bl_node_t *ident)
{
  bl_node_ident_t *_ident = bl_peek_ident(ident);
  if (_ident->ref) return false;

  bl_scope_t *scope = bl_ast_get_scope(cnt->curr_compound);
  assert(scope);

  bl_node_t *found = bl_scope_get(scope, ident);
  if (!found) return false;
  _ident->ref = found;

  return true;
}

bool
check_decl_value(context_t *cnt, bl_node_t *decl)
{
  bl_node_decl_value_t *_decl = bl_peek_decl_value(decl);
  bl_scope_t *          scope = bl_ast_get_scope(cnt->curr_compound);
  assert(scope);

  _decl->type = bl_ast_type_of(_decl->value);
  assert(_decl->type);

  bl_node_t *conflict = bl_scope_get(scope, _decl->name);
  if (conflict) {
    check_error_node(cnt, BL_ERR_DUPLICATE_SYMBOL, decl, BL_BUILDER_CUR_WORD,
                     "symbol with same name already declared");
  } else {
    bl_scope_insert(scope, _decl->name, decl);
  }

  return true;
}

void
bl_checker_run(bl_builder_t *builder, bl_assembly_t *assembly)
{
  context_t cnt = {.builder       = builder,
                   .unit          = NULL,
                   .assembly      = assembly,
                   .ast           = NULL,
                   .curr_compound = NULL,
                   .waiting       = waiting_new(),
                   .flatten_cache = flatten_cache_new()};

  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    cnt.unit = unit;
    cnt.ast  = &unit->ast;
    check_ublock(&cnt, unit->ast.root);
  }

  bl_log("*************************************");
  check_waiting(&cnt);

  flatten_cache_delete(cnt.flatten_cache);
  waiting_delete(cnt.waiting);
}
