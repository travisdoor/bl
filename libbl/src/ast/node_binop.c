//*****************************************************************************
// bl
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
/* class NodeBinop object members */
bo_decl_members_begin(NodeBinop, Node)
  /* members */
bo_end();

bo_impl_type(NodeBinop, Node);

void
NodeBinopKlass_init(NodeBinopKlass *klass)
{
}

void
NodeBinop_ctor(NodeBinop *self, NodeBinopParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(Node, p);

  /* initialize self */
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

