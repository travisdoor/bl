//*****************************************************************************
// blc
//
// File:   sym_tbl.c
// Author: Martin Dorazil
// Date:   13/02/2018
//
// Copyright 2017 Martin Dorazil
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

#include <bobject/containers/htbl.h>
#include <bobject/containers/array.h>
#include "sym_tbl_impl.h"
#include "bl/bldebug.h"

#define EXPECTED_SYM_COUNT 512

void
bl_sym_tbl_init(bl_sym_tbl_t *tbl)
{
  tbl->syms        = bo_htbl_new(sizeof(bl_node_t *), EXPECTED_SYM_COUNT);
  tbl->unsatisfied = bo_array_new(sizeof(bl_node_t *));
}

void
bl_sym_tbl_terminate(bl_sym_tbl_t *tbl)
{
  bo_unref(tbl->syms);
  bo_unref(tbl->unsatisfied);
}

bool
bl_sym_tbl_register(bl_sym_tbl_t *tbl,
                    bl_node_t *node)
{
  bl_assert(node, "invalid node");
  uint32_t hash = node->value.decl.ident.hash;

  if (bo_htbl_has_key(tbl->syms, hash))
    return false;

  bo_htbl_insert(tbl->syms, hash, node);
  return true;
}

bl_node_t *
bl_sym_tbl_get_sym_of_type(bl_sym_tbl_t *tbl,
                           bl_ident_t *ident,
                           bl_node_type_e type)
{
  bl_assert(ident, "invalid identifier");
  bo_iterator_t iter = bo_htbl_find(tbl->syms, ident->hash);
  bo_iterator_t end  = bo_htbl_end(tbl->syms);
  if (bo_iterator_equal(&iter, &end))
    return NULL;

  bl_node_t *ret = bo_htbl_iter_peek_value(tbl->syms, &iter, bl_node_t *);
  if (ret->type != type)
    return NULL;

  return ret;
}

void
bl_sym_tbl_add_unsatisfied_expr(bl_sym_tbl_t *tbl,
                                bl_node_t *expr)
{
  bl_assert(expr, "invalid expression");
  bo_array_push_back(tbl->unsatisfied, expr);
}

bool
bl_sym_tbl_try_satisfy_all(bl_sym_tbl_t *tbl)
{
  size_t     c      = bo_array_size(tbl->unsatisfied);
  size_t     i      = 0;
  bool       ret    = true;
  bool       erase  = false;
  bl_node_t  *expr  = NULL;
  bl_node_t  *decl  = NULL;
  bl_ident_t *ident = NULL;

  while (i < c) {
    expr = bo_array_at(tbl->unsatisfied, i, bl_node_t *);

    switch (expr->type) {
      case BL_NODE_CALL_EXPR:
        ident = &expr->value.call_expr.ident;
        decl  = bl_sym_tbl_get_sym_of_type(tbl, ident, BL_NODE_FUNC_DECL);
        if (decl) {
          expr->value.call_expr.callee = decl;
          erase = true;
        }
        break;
      default: bl_abort("cannot satisfy this type");
    }

    /*
     * Delete when call was satisfied.
     */
    if (erase) {
      bo_array_erase(tbl->unsatisfied, i);
      --c;
      erase = false;
    } else {
      i++;
      ret = false;
    }
  }

  return ret;
}

