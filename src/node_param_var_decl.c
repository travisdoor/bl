//*****************************************************************************
// bl 
//
// File:   node_param_var_decl.c
// Author: Martin Dorazil
// Date:   03/02/2018
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

#include <bobject/containers/array.h>
#include "node_param_var_decl.h"

static BString *
to_string(NodeParamVarDecl *self);

/* NodeParamVarDecl members */
bo_decl_members_begin(NodeParamVarDecl, Node)
  BString *type;
  BString *ident;
bo_end();

/* NodeParamVarDecl constructor parameters */
bo_decl_params_with_base_begin(NodeParamVarDecl, Node)
  BString *type;
  BString *ident;
bo_end();

bo_impl_type(NodeParamVarDecl, Node);

/* NodeParamVarDecl class init */
void
NodeParamVarDeclKlass_init(NodeParamVarDeclKlass *klass)
{
  bo_vtbl_cl(klass, Node)->to_string 
    = (BString *(*)(Node*)) to_string;
}

/* NodeParamVarDecl constructor */
void
NodeParamVarDecl_ctor(NodeParamVarDecl *self, NodeParamVarDeclParams *p)
{
  bo_parent_ctor(Node, p);
  self->type = p->type;
  self->ident = p->ident;
}

/* NodeParamVarDecl destructor */
void
NodeParamVarDecl_dtor(NodeParamVarDecl *self)
{
  bo_unref(self->type);
  bo_unref(self->ident);
}

/* NodeParamVarDecl copy constructor */
bo_copy_result
NodeParamVarDecl_copy(NodeParamVarDecl *self, NodeParamVarDecl *other)
{
  return BO_NO_COPY;
}

BString *
to_string(NodeParamVarDecl *self)
{
  BString *ret = bo_string_new(128);
  bo_string_append(ret, "<");
  bo_string_append(ret, bl_node_strings[bo_members(self, Node)->type]);
  bo_string_append(ret, " ");
  bo_string_append_str(ret, self->type);
  bo_string_append(ret, " ");
  bo_string_append_str(ret, self->ident);
  bo_string_append(ret, ">");
  return ret;
}

/* public */

NodeParamVarDecl *
bl_node_param_var_decl_new(BString *type,
                           BString *ident,
                           const char *generated_from,
                           int         line,
                           int         col)
{
  NodeParamVarDeclParams p = {
    .base.type = BL_NODE_PARAM_VAR_DECL,
    .base.generated_from = generated_from, 
    .base.line = line, 
    .base.col = col,
    .type = type,
    .ident = ident
  };
  
  return bo_new(NodeParamVarDecl, &p);
}

