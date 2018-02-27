//*****************************************************************************
// blc
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
#include "bl/ast/node_expr.h"
#include "bl/ast/node_stmt.h"
#include "bl/ast/node_param_var_decl.h"
#include "bl/ast/node_func_decl.h"
#include "bl/ast/node_global_stmt.h"
#include "bl/ast/node_return_stmt.h"
#include "bl/ast/node_var_decl.h"
#include "bl/ast/node_binop.h"
#include "bl/ast/node_decl.h"
#include "bl/ast/node_const.h"
#include "bl/ast/node_call.h"
#include "bl/ast/node_decl_ref.h"
#include "bl/ast/node_if_stmt.h"
#include "bl/ast/node_loop_stmt.h"
#include "bl/ast/node_break_stmt.h"
#include "bl/ast/node_continue_stmt.h"

BO_BEGIN_DECLS

/* class Ast declaration */
bo_decl_type_begin(Ast, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT Ast *
bl_ast_new(void);

extern BO_EXPORT void
bl_ast_set_root(Ast *self,
                Node *root);

extern BO_EXPORT Node *
bl_ast_get_root(Ast *self);

extern BO_EXPORT NodeFuncDecl *
bl_ast_node_func_decl_new(Ast *self,
                          const char *type,
                          const char *ident,
                          bl_sym_e modif,
                          const char *generated_from,
                          int line,
                          int col);

extern BO_EXPORT NodeGlobalStmt *
bl_ast_node_global_stmt_new(Ast *self,
                            const char *generated_from,
                            int line,
                            int col);

extern BO_EXPORT NodeStmt *
bl_ast_node_stmt_new(Ast *self,
                     const char *generated_from,
                     int line,
                     int col);

extern BO_EXPORT NodeParamVarDecl *
bl_ast_node_param_var_decl_new(Ast *self,
                               const char *type,
                               const char *ident,
                               const char *generated_from,
                               int line,
                               int col);

extern BO_EXPORT NodeBinop *
bl_ast_node_binop_new(Ast *self,
                      bl_sym_e op,
                      const char *generated_from,
                      int line,
                      int col);

extern BO_EXPORT NodeReturnStmt *
bl_ast_node_return_stmt_new(Ast *self,
                            const char *generated_from,
                            int line,
                            int col);

extern BO_EXPORT NodeConst *
bl_ast_node_const_new(Ast *self,
                      const char *generated_from,
                      int line,
                      int col);

extern BO_EXPORT NodeCall *
bl_ast_node_call_new(Ast *self,
                     const char *ident,
                     const char *generated_from,
                     int line,
                     int col);

extern BO_EXPORT NodeVarDecl *
bl_ast_node_var_decl_new(Ast *self,
                         const char *type,
                         const char *ident,
                         const char *generated_from,
                         int line,
                         int col);

extern BO_EXPORT NodeDeclRef *
bl_ast_node_decl_ref_new(Ast *self,
                         const char *ident,
                         const char *generated_from,
                         int line,
                         int col);

extern BO_EXPORT NodeIfStmt *
bl_ast_node_if_stmt_new(Ast *self,
                        NodeExpr *cond,
                        NodeStmt *then_stmt,
                        NodeStmt *else_stmt,
                        const char *generated_from,
                        int line,
                        int col);

extern BO_EXPORT NodeLoopStmt *
bl_ast_node_loop_stmt_new(Ast *self,
                          NodeStmt *cmp_stmt,
                          const char *generated_from,
                          int line,
                          int col);

extern BO_EXPORT NodeBreakStmt *
bl_ast_node_break_stmt_new(Ast *self,
                           const char *generated_from,
                           int line,
                           int col);

extern BO_EXPORT NodeContinueStmt *
bl_ast_node_continue_stmt_new(Ast *self,
                              const char *generated_from,
                              int line,
                              int col);

BO_END_DECLS

#endif /* end of include guard: AST_H_VGMYANDT */
