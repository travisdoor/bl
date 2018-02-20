//*****************************************************************************
// blc
//
// File:   node_call.c
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

#include "ast/node_call_impl.h"
/* class NodeCall */
/* class NodeCall object members */
bo_decl_members_begin(NodeCall, NodeExpr)
  /* members */
  BArray *args;
  NodeFuncDecl *callee;
  Ident *ident;
bo_end();

bo_impl_type(NodeCall, NodeExpr);

void
NodeCallKlass_init(NodeCallKlass *klass)
{
}

void
NodeCall_ctor(NodeCall *self,
              NodeCallParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(NodeExpr, p);
  /* initialize self */
  self->ident = p->ident;
}

void
NodeCall_dtor(NodeCall *self)
{
  bo_unref(self->args);
  bo_unref(self->ident);
}

bo_copy_result
NodeCall_copy(NodeCall *self,
              NodeCall *other)
{
  return BO_NO_COPY;
}
/* class NodeCall end */

bool
bl_node_call_add_arg(NodeCall *self,
                     NodeExpr *arg)
{
  if (arg == NULL)
    return false;

  if (self->args == NULL)
    self->args = bo_array_new_bo(bo_typeof(NodeExpr), false);

  bo_array_push_back(self->args, arg);

  return true;
}

int
bl_node_call_get_arg_count(NodeCall *self)
{
  if (self->args == NULL)
    return 0;

  return (int) bo_array_size(self->args);
}

NodeExpr *
bl_node_call_get_arg(NodeCall *self,
                     int i)
{
  return bo_array_at(self->args, i, NodeExpr *);
}

NodeFuncDecl *
bl_node_call_get_callee(NodeCall *self)
{
  return self->callee;
}

void
bl_node_call_get_set_callee(NodeCall *self,
                            NodeFuncDecl *callee)
{
  self->callee = callee;
}

Ident *
bl_node_call_get_ident(NodeCall *self)
{
  return self->ident;
}
