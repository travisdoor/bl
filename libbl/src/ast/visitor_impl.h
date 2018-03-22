//*****************************************************************************
// bl
//
// File:   visitor_impl.h
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
//*****************************************************************************

#ifndef VISITOR_IMPL_H_0IZSKUFY
#define VISITOR_IMPL_H_0IZSKUFY

#include "ast2_impl.h"

typedef struct bl_visitor bl_visitor_t;

typedef enum {
  BL_VISIT_MODULE,
  BL_VISIT_FUNC,
  BL_VISIT_TYPE,
  BL_VISIT_ARG,
  BL_VISIT_STRUCT,
  BL_VISIT_ENUM,
  BL_VISIT_VAR,
  BL_VISIT_BLOCK,
  BL_VISIT_COUNT
} bl_visit_e;

typedef void (*bl_visit_module_f)(bl_visitor_t *visitor, bl_module_t *module, bl_src_t *src);
typedef void (*bl_visit_fn_f)(bl_visitor_t *visitor, bl_func_t *func, bl_src_t *src);
typedef void (*bl_visit_type_f)(bl_visitor_t *visitor, bl_type_t *type, bl_src_t *src);
typedef void (*bl_visit_arg_f)(bl_visitor_t *visitor, bl_arg_t *arg, bl_src_t *src);
typedef void (*bl_visit_struct_f)(bl_visitor_t *visitor, bl_struct_t *strct, bl_src_t *src);
typedef void (*bl_visit_enum_f)(bl_visitor_t *visitor, bl_enum_t *enm, bl_src_t *src);
typedef void (*bl_visit_var_f)(bl_visitor_t *visitor, bl_var_t *var, bl_src_t *src);
typedef void (*bl_visit_block_f)(bl_visitor_t *visitor, bl_block_t *block, bl_src_t *src);

struct bl_visitor
{
  void *visitors[BL_VISIT_COUNT];
  void *context;
  int   nesting;
};

void
bl_visitor_init(bl_visitor_t *visitor, void *context);

void
bl_visitor_add(bl_visitor_t *visitor, void *visit, bl_visit_e type);

void
bl_visitor_walk_module(bl_visitor_t *visitor, bl_module_t *module);

void
bl_visitor_walk_func(bl_visitor_t *visitor, bl_func_t *func);

void
bl_visitor_walk_type(bl_visitor_t *visitor, bl_type_t *type);

void
bl_visitor_walk_arg(bl_visitor_t *visitor, bl_arg_t *arg);

void
bl_visitor_walk_struct(bl_visitor_t *visitor, bl_struct_t *strct);

void
bl_visitor_walk_enum(bl_visitor_t *visitor, bl_enum_t *enm);

void
bl_visitor_walk_var(bl_visitor_t *visitor, bl_var_t *var);

void
bl_visitor_walk_block(bl_visitor_t *visitor, bl_block_t *block);


#endif /* end of include guard: VISITOR_IMPL_H_0IZSKUFY */
