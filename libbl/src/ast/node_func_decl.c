//*****************************************************************************
// bl 
//
// File:   node_func_decl.c
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

#include <bobject/containers/string.h>
#include "node_func_decl_impl.h"

static BString *
to_string(NodeFuncDecl *self);

/* NodeFuncDecl members */
bo_decl_members_begin(NodeFuncDecl, Node)
  BArray *params;
  NodeStmt *stmt;
  char *type;
  char *ident;
  bl_sym_e modificator;
bo_end();

bo_impl_type(NodeFuncDecl, Node);

/* NodeFuncDecl class init */
void
NodeFuncDeclKlass_init(NodeFuncDeclKlass *klass)
{
  bo_vtbl_cl(klass, Node)->to_string 
    = (BString *(*)(Node*)) to_string;
}

/* NodeFuncDecl constructor */
void
NodeFuncDecl_ctor(NodeFuncDecl *self, NodeFuncDeclParams *p)
{
  bo_parent_ctor(Node, p);
  self->type = p->type;
  self->ident = p->ident;
  self->modificator = p->modif;
}

/* NodeFuncDecl destructor */
void
NodeFuncDecl_dtor(NodeFuncDecl *self)
{
  free(self->type);
  free(self->ident);
  bo_unref(self->params);
}

/* NodeFuncDecl copy constructor */
bo_copy_result
NodeFuncDecl_copy(NodeFuncDecl *self, NodeFuncDecl *other)
{
  return BO_NO_COPY;
}

BString *
to_string(NodeFuncDecl *self)
{
  BString *ret = bo_string_new(128);
  bo_string_append(ret, "<");
  bo_string_append(ret, bl_node_strings[bo_members(self, Node)->type]);
  bo_string_append(ret, " ");
  bo_string_append(ret, self->type);
  bo_string_append(ret, " ");
  bo_string_append(ret, self->ident);
  bo_string_append(ret, " ");
  bo_string_append(ret, bl_sym_strings[self->modificator]);
  bo_string_append(ret, ">");
  return ret;
}

/* public */

bool
bl_node_func_decl_add_stmt(NodeFuncDecl *self,
                           NodeStmt     *stmt)
{
  if (!stmt)
    return false;

  self->stmt = stmt;
  return true;
}

bool
bl_node_func_decl_add_param(NodeFuncDecl     *self,
                            NodeParamVarDecl *param)
{
  if (param == NULL)
    return false;

  if (self->params == NULL)
    self->params = bo_array_new_bo(bo_typeof(Node), false);
  bo_array_push_back(self->params, param);
  return true;
}

NodeStmt *
bl_node_func_decl_get_stmt(NodeFuncDecl *self)
{
  return self->stmt;
}

int
bl_node_func_decl_param_count(NodeFuncDecl *self)
{
  if (self->params == NULL)
    return 0;

  return (int) bo_array_size(self->params);
}

NodeParamVarDecl * 
bl_node_func_decl_param(NodeFuncDecl *self,
                        int i)
{
  return bo_array_at(self->params, i, NodeParamVarDecl *);
}

const char *
bl_node_func_decl_ident(NodeFuncDecl *self)
{
  return self->ident;
}

const char *
bl_node_func_decl_type(NodeFuncDecl *self)
{
  return self->type;
}

bl_sym_e
bl_node_func_decl_modif(NodeFuncDecl *self)
{
  return self->modificator;
}

