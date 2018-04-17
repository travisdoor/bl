//************************************************************************************************
// Biscuit Language 
//
// File:   block_scope_impl.h
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

#ifndef BLOCK_SCOPE_IMPL_H_C5LHTUYK
#define BLOCK_SCOPE_IMPL_H_C5LHTUYK

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include "ast/ast_impl.h"

typedef struct bl_block_scope {
  BArray *scopes; 
} bl_block_scope_t;

void
bl_block_scope_init(bl_block_scope_t *scope);

void
bl_block_scope_terminate(bl_block_scope_t *scope);

void
bl_block_scope_push(bl_block_scope_t *scope);

void
bl_block_scope_pop(bl_block_scope_t *scope);

/* insert new node or return conflicted node */
void
bl_block_scope_insert_node(bl_block_scope_t *scope, bl_node_t *node);

bl_node_t *
bl_block_scope_get_node(bl_block_scope_t *scope, bl_id_t *id);

#endif /* end of include guard: BLOCK_SCOPE_IMPL_H_C5LHTUYK */
