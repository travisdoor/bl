//*****************************************************************************
// blc
//
// File:   node_const.h
// Author: Martin Dorazil
// Date:   11/02/2018
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

#ifndef BL_NODE_CONST_H
#define BL_NODE_CONST_H

#include <bobject/bobject.h>
#include <stdbool.h>
#include "bl/ast/node_expr.h"

typedef enum _bl_node_conts_type_e
{
  BL_CONST_INT,
  BL_CONST_LONG,
  BL_CONST_ULONG,
  BL_CONST_BOOL,
  BL_CONST_STRING,
  BL_CONST_CHAR,
  BL_CONST_DOUBLE,
  BL_CONST_FLOAT
} bl_node_conts_type_e;

BO_BEGIN_DECLS

/* class NodeConst declaration */
bo_decl_type_begin(NodeConst, NodeExpr)
  /* virtuals */
bo_end();

extern BO_EXPORT int
bl_node_const_get_int(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_int(NodeConst *self,
                      int val);

extern BO_EXPORT long
bl_node_const_get_long(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_long(NodeConst *self,
                      long val);

extern BO_EXPORT unsigned long
bl_node_const_get_ulong(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_ulong(NodeConst *self,
                       unsigned long val);

extern BO_EXPORT double
bl_node_const_get_double(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_double(NodeConst *self,
                         double val);

extern BO_EXPORT float
bl_node_const_get_float(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_float(NodeConst *self,
                        float val);

extern BO_EXPORT bool
bl_node_const_get_bool(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_bool(NodeConst *self,
                       bool val);

extern BO_EXPORT const char *
bl_node_const_get_str(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_str(NodeConst *self,
                      const char *val);

extern BO_EXPORT char
bl_node_const_get_char(NodeConst *self);

extern BO_EXPORT void
bl_node_const_set_char(NodeConst *self,
                       char val);

extern BO_EXPORT bl_node_conts_type_e
bl_node_const_get_type(NodeConst *self);

BO_END_DECLS

#endif //BL_NODE_INT_CONST_H
