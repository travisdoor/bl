//*****************************************************************************
// bl
//
// File:   node_decl_ref.c
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

#include "ast/node_decl_ref_impl.h"
/* class NodeDeclRef */
/* class NodeDeclRef object members */
bo_decl_members_begin(NodeDeclRef, NodeExpr)
  /* members */
  Ident *ident;
bo_end();

bo_impl_type(NodeDeclRef, NodeExpr);

void
NodeDeclRefKlass_init(NodeDeclRefKlass *klass)
{
}

void
NodeDeclRef_ctor(NodeDeclRef *self, NodeDeclRefParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(NodeExpr, p);

  /* initialize self */
  self->ident = p->ident;
}

void
NodeDeclRef_dtor(NodeDeclRef *self)
{
  bo_unref(self->ident);
}

bo_copy_result
NodeDeclRef_copy(NodeDeclRef *self, NodeDeclRef *other)
{
  return BO_NO_COPY;
}
/* class NodeDeclRef end */

Ident *
bl_node_decl_ref_get_ident(NodeDeclRef *self)
{
  return self->ident;
}
