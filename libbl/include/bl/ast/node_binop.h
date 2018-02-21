//*****************************************************************************
// blc
//
// File:   node_binop.h
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

#ifndef BL_NODE_BINOP_H
#define BL_NODE_BINOP_H

#include <bobject/bobject.h>
#include "bl/ast/node.h"
#include "bl/ast/node_expr.h"
#include "bl/token.h"

BO_BEGIN_DECLS

/* class NodeBinop declaration */
bo_decl_type_begin(NodeBinop, Node)
  /* virtuals */
bo_end();

extern BO_EXPORT bl_sym_e
bl_node_binop_get_op(NodeBinop *self);

extern BO_EXPORT NodeExpr *
bl_node_binop_get_lhs(NodeBinop *self);

extern BO_EXPORT NodeExpr *
bl_node_binop_get_rhs(NodeBinop *self);

extern BO_EXPORT bool
bl_node_binop_set_lhs(NodeBinop *self,
                      NodeExpr *lvalue);

extern BO_EXPORT bool
bl_node_binop_set_rhs(NodeBinop *self,
                      NodeExpr *rvalue);

BO_END_DECLS

#endif //BL_NODE_BINOP_H
