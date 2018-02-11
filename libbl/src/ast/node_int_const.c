//*****************************************************************************
// bl
//
// File:   node_int_const.c
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

#include "ast/node_int_const_impl.h"
/* class NodeIntConst */
/* class NodeIntConst object members */
bo_decl_members_begin(NodeIntConst, NodeExpr)
  /* members */
  unsigned long long num;
bo_end();

bo_impl_type(NodeIntConst, NodeExpr);

void
NodeIntConstKlass_init(NodeIntConstKlass *klass)
{
}

void
NodeIntConst_ctor(NodeIntConst *self, NodeIntConstParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(NodeExpr, p);

  /* initialize self */
  self->num = p->num;
}

void
NodeIntConst_dtor(NodeIntConst *self)
{
}

bo_copy_result
NodeIntConst_copy(NodeIntConst *self, NodeIntConst *other)
{
  return BO_NO_COPY;
}
/* class NodeIntConst end */

unsigned long long
bl_node_int_const_get_num(NodeIntConst *self)
{
  return self->num;
}
