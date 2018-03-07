//*****************************************************************************
// bl
//
// File:   scope.h
// Author: Martin Dorazil
// Date:   3/6/18
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

#ifndef BL_SCOPE_H
#define BL_SCOPE_H

#include <bobject/containers/array.h>
#include "ast/node_impl.h"

typedef struct
{
  BArray *scopes;
} bl_scope_t;

void
bl_scope_init(bl_scope_t *cnt);

void
bl_scope_terminate(bl_scope_t *cnt);

void
bl_scope_push(bl_scope_t *cnt);

void
bl_scope_pop(bl_scope_t *cnt);

/*
 * Add new declaration into current scope.
 * When same declaration already exist in current scope
 * or in parent scopes method return conflicted node,
 * otherwise null will be returned.
 */
bl_node_t *
bl_scope_add(bl_scope_t *cnt,
             bl_node_t *node);

/*
 * Get declaration from current or parent scope.
 * When no such declaration exist function return
 * null.
 */
bl_node_t *
bl_scope_get(bl_scope_t *cnt,
             bl_ident_t *ident);

#endif //BL_SCOPE_H

