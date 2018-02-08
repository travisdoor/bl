//*****************************************************************************
// bl
//
// File:   ast.h
// Author: Martin Dorazil
// Date:   26.1.18
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

#ifndef AST_H_VGMYANDT
#define AST_H_VGMYANDT

#include <bobject/bobject.h>
#include "bl/ast/node.h"
#include "bl/ast/node_stmt.h"
#include "bl/ast/node_param_var_decl.h"
#include "bl/ast/node_func_decl.h"
#include "bl/ast/node_global_stmt.h"

/* TODO: cache nodes into array */

/* class Ast declaration */
bo_decl_type_begin(Ast, BObject)
  /* virtuals */
bo_end();

Ast *
bl_ast_new(void);

void
bl_ast_set_root(Ast *self,
                Node *root);

Node *
bl_ast_get_root(Ast *self);

NodeFuncDecl *
bl_ast_node_func_decl_new(Ast        *self,
                          char       *type,
                          char       *ident,
                          const char *generated_from,
                          int         line,
                          int         col);

NodeGlobalStmt *
bl_ast_node_global_stmt_new(Ast        *self,
                            const char *generated_from,
                            int         line,
                            int         col);

NodeStmt *
bl_ast_node_stmt_new(Ast        *self,
                     const char *generated_from,
                     int         line,
                     int         col);

NodeParamVarDecl *
bl_ast_node_param_var_decl_new(Ast        *self,
                               char       *type,
                               char       *ident,
                               const char *generated_from,
                               int         line,
                               int         col);

#endif /* end of include guard: AST_H_VGMYANDT */
