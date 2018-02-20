//*****************************************************************************
// blc
//
// File:   node_binop.c
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

#include "ast/node_binop_impl.h"

/* class NodeBinop */
BString *
to_string(NodeBinop *self);

/* class NodeBinop object members */
bo_decl_members_begin(NodeBinop, Node)
  /* members */
  bl_sym_e operator;
  NodeExpr *lvalue;
  NodeExpr *rvalue;
bo_end();

bo_impl_type(NodeBinop, Node);

void
NodeBinopKlass_init(NodeBinopKlass *klass)
{
  bo_vtbl_cl(klass, Node)->to_string = (BString *(*)(Node *)) to_string;
}

void
NodeBinop_ctor(NodeBinop *self, NodeBinopParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Node, p);
  /* initialize self */
  self->operator = p->operator;
}

void
NodeBinop_dtor(NodeBinop *self)
{
}

bo_copy_result
NodeBinop_copy(NodeBinop *self, NodeBinop *other)
{
  return BO_NO_COPY;
}
/* class NodeBinop end */
BString *
to_string(NodeBinop *self)
{
  BString *ret = bo_string_new(128);
  bo_string_append(ret, "<");
  bo_string_append(ret, bl_node_strings[bo_members(self, Node)->type]);
  bo_string_append(ret, " ");
  bo_string_append(ret, bl_sym_strings[self->operator]);
  bo_string_append(ret, ">");
  return ret;
}

bl_sym_e
bl_node_binop_get_op(NodeBinop *self)
{
  return self->operator;
}

NodeExpr *
bl_node_binop_get_lvalue(NodeBinop *self)
{
  return self->lvalue;
}

NodeExpr *
bl_node_binop_get_rvalue(NodeBinop *self)
{
  return self->rvalue;
}

bool
bl_node_binop_set_lvalue(NodeBinop *self,
                         NodeExpr  *lvalue)
{
  if (lvalue == NULL)
    return false;

  self->lvalue = lvalue;
  return true;
}

bool
bl_node_binop_set_rvalue(NodeBinop *self,
                         NodeExpr  *rvalue)
{
  if (rvalue == NULL)
    return false;

  self->rvalue = rvalue;
  return true;
}

