//*****************************************************************************
// blc
//
// File:   node_func_decl.h
// Author: Martin Dorazil
// Date:   8.2.18
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

#ifndef BL_NODE_FUNC_DECL_H
#define BL_NODE_FUNC_DECL_H

#include "bl/ast/node_decl.h"
#include "bl/ast/node_stmt.h"
#include "bl/ast/node_param_var_decl.h"
#include "bl/token.h"
#include "bl/type.h"

BO_BEGIN_DECLS
/* class declaration */
bo_decl_type_begin(NodeFuncDecl, NodeDecl)
  /* virtuals */
bo_end();

extern BO_EXPORT bl_sym_e
bl_node_func_decl_get_modif(NodeFuncDecl *self);

extern BO_EXPORT bool
bl_node_func_decl_set_stmt(NodeFuncDecl *self,
                           NodeStmt *stmt);

extern BO_EXPORT bool
bl_node_func_decl_add_param(NodeFuncDecl     *self,
                            NodeParamVarDecl *param);

extern BO_EXPORT NodeStmt *
bl_node_func_decl_get_stmt(NodeFuncDecl *self);

extern BO_EXPORT int
bl_node_func_decl_get_param_count(NodeFuncDecl *self);

extern BO_EXPORT NodeParamVarDecl * 
bl_node_func_decl_get_param(NodeFuncDecl *self,
                            int i);

BO_END_DECLS

#endif //BL_NODE_FUNC_DECL_H
