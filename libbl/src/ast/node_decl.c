//*****************************************************************************
// bl
//
// File:   node_decl.c
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

#include "ast/node_decl_impl.h"
/* class NodeDecl */

bo_impl_type(NodeDecl, Node);

void
NodeDeclKlass_init(NodeDeclKlass *klass)
{
}

void
NodeDecl_ctor(NodeDecl *self,
              NodeDeclParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(Node, p);

  /* initialize self */
  self->ident = p->ident;
  self->type = p->type;
}

void
NodeDecl_dtor(NodeDecl *self)
{
  free(self->ident);
  bo_unref(self->type);
}

bo_copy_result
NodeDecl_copy(NodeDecl *self,
              NodeDecl *other)
{
  return BO_NO_COPY;
}

/* class NodeDecl end */

char *
bl_node_decl_get_ident(NodeDecl *self)
{
  return self->ident;
}

Type *
bl_node_decl_get_type(NodeDecl *self)
{
  return self->type;
}
