//*****************************************************************************
// bl
//
// File:   node_var_decl.c
// Author: Martin Dorazil
// Date:   10/02/2018
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

#include "ast/node_var_decl_impl.h"

static BString *
to_string(NodeVarDecl *self);

/* class NodeVarDecl */
/* class NodeVarDecl object members */
bo_decl_members_begin(NodeVarDecl, NodeDecl)
  /* members */
bo_end();

bo_impl_type(NodeVarDecl, NodeDecl);

void
NodeVarDeclKlass_init(NodeVarDeclKlass *klass)
{
  bo_vtbl_cl(klass, Node)->to_string
    = (BString *(*)(Node*)) to_string;
}

void
NodeVarDecl_ctor(NodeVarDecl *self, NodeVarDeclParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(NodeDecl, p);
  /* initialize self */
}

void
NodeVarDecl_dtor(NodeVarDecl *self)
{
}

bo_copy_result
NodeVarDecl_copy(NodeVarDecl *self, NodeVarDecl *other)
{
  return BO_NO_COPY;
}
/* class NodeVarDecl end */

BString *
to_string(NodeVarDecl *self)
{
  BString *ret = bo_string_new(128);
  bo_string_append(ret, "<");
  bo_string_append(ret, bl_node_strings[bo_members(self, Node)->type]);
  bo_string_append(ret, " ");
  bo_string_append(ret, bl_type_get_name(bo_members(self, NodeDecl)->type));
  bo_string_append(ret, " ");
  bo_string_append(ret, bo_members(self, NodeDecl)->ident);
  bo_string_append(ret, ">");
  return ret;
}

