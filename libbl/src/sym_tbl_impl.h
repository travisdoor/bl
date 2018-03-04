//*****************************************************************************
// blc
//
// File:   sym_tbl_impl.h
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

#ifndef BL_SYM_TBL_IMPL_H
#define BL_SYM_TBL_IMPL_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include "ast/node_impl.h"
#include "identifier_impl.h"

typedef struct bl_sym_tbl
{
  BHashTable *syms;
  BArray     *unsatisfied;
} bl_sym_tbl_t;

void
bl_sym_tbl_init(bl_sym_tbl_t *tbl);

void
bl_sym_tbl_terminate(bl_sym_tbl_t *tbl);

/*
 * This will add new symbol into hash table and return
 * true on success.
 */
bool
bl_sym_tbl_register(bl_sym_tbl_t *tbl,
                    bl_node_t *node);

/*
 * Return symbol of excepted type or null when no such
 * symbol was found.
 */
bl_node_t *
bl_sym_tbl_get_sym_of_type(bl_sym_tbl_t *tbl,
                           bl_ident_t *ident,
                           bl_node_type_e type);

void
bl_sym_tbl_add_unsatisfied_expr(bl_sym_tbl_t *tbl,
                                bl_node_t *expr);

/*
 * Try to satisfy all calls and return true when all of them
 * were satisfied, false when there left some unsatisfied.
 */
bool
bl_sym_tbl_try_satisfy_all(bl_sym_tbl_t *tbl);

#endif //BL_SYM_TBL_H
