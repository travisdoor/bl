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

#include "node_func_decl.h"

/* NodeFuncDecl members */
bo_decl_members_begin(NodeFuncDecl, Node)
bo_end();

/* NodeFuncDecl constructor parameters */
bo_decl_params_with_base_begin(NodeFuncDecl, Node)
bo_end();

bo_impl_type(NodeFuncDecl, Node);

/* NodeFuncDecl class init */
void
NodeFuncDeclKlass_init(NodeFuncDeclKlass *klass)
{
}

/* NodeFuncDecl constructor */
void
NodeFuncDecl_ctor(NodeFuncDecl *self, NodeFuncDeclParams *p)
{
  bo_parent_ctor(Node, p);
}

/* NodeFuncDecl destructor */
void
NodeFuncDecl_dtor(NodeFuncDecl *self)
{
}

/* NodeFuncDecl copy constructor */
bo_copy_result
NodeFuncDecl_copy(NodeFuncDecl *self, NodeFuncDecl *other)
{
  return BO_NO_COPY;
}

/* public */
NodeFuncDecl *
bl_node_func_decl_new(const char *generated_from,
                      int         line,
                      int         col)
{
  NodeFuncDeclParams p = {
    .base.type = BL_NODE_FUNCDECL,
    .base.generated_from = generated_from, 
    .base.line = line, 
    .base.col = col
  };
  
  return bo_new(NodeFuncDecl, &p);
}

