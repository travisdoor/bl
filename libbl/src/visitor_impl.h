//************************************************************************************************
// bl
//
// File:   visitor_impl.h
// Author: Martin Dorazil
// Date:   31/8/18
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

#ifndef BL_VISITOR_IMPL_H
#define BL_VISITOR_IMPL_H

#include "ast_impl.h"

typedef struct bl_visitor bl_visitor_t;

#define BL_VISIT_SKIP NULL

typedef enum
{
  BL_VISIT_DECL,
  BL_VISIT_COUNT
} bl_visit_e;

typedef void (*bl_visit_f)(bl_visitor_t *visitor, bl_node_t *node);

struct bl_visitor
{
  bl_visit_f visitors[BL_VISIT_COUNT];
  void *     context;
  int        nesting;
};

void
bl_visitor_init(bl_visitor_t *visitor, void *context);

void
bl_visitor_add(bl_visitor_t *visitor, bl_visit_f callback, bl_visit_e type);

void
bl_visitor_walk_ublock(bl_visitor_t *visitor, bl_node_t *ublock);

void
bl_visitor_walk_decl(bl_visitor_t *visitor, bl_node_t *decl);

#endif
