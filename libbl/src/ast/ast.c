//*****************************************************************************
// bl
//
// File:   ast.c
// Author: Martin Dorazil
// Date:   6.2.18
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
#include "ast/ast_impl.h"

/* class Ast */
static void *
save_to_cache(Ast *self,
              void *node);

/* class Ast constructor params */
bo_decl_params_begin(Ast)
  /* constructor params */
bo_end();

/* class Ast object members */
bo_decl_members_begin(Ast, BObject)
  /* members */
  BArray *cache;
  Node   *root;
bo_end();

bo_impl_type(Ast, BObject);

void
AstKlass_init(AstKlass *klass)
{
}

void
Ast_ctor(Ast *self, AstParams *p)
{
  /* constructor */
  self->cache = bo_array_new_bo(bo_typeof(Node), true);
}

void
Ast_dtor(Ast *self)
{
  bo_unref(self->cache);
}

bo_copy_result
Ast_copy(Ast *self, Ast *other)
{
  return BO_NO_COPY;
}
/* class Ast end */

void *
save_to_cache(Ast *self,
              void *node)
{
  bo_array_push_back(self->cache, node);
  return node;
}

/* public */
Ast *
bl_ast_new(void)
{
  return bo_new(Ast, NULL);
}

void
bl_ast_set_root(Ast *self,
                Node *root)
{
  if (!root)
    return;
  self->root = root;
}

Node *
bl_ast_get_root(Ast *self)
{
  return self->root;
}


NodeFuncDecl *
bl_ast_node_func_decl_new(Ast        *self,
                          char       *type,
                          char       *ident,
                          bl_sym_e    modif,
                          const char *generated_from,
                          int         line,
                          int         col)
{
  NodeFuncDeclParams p = {
    .base = {
      .base = {
        .type           = BL_NODE_FUNC_DECL,
        .generated_from = generated_from,
        .line           = line,
        .col            = col,
      },
      .type           = bl_type_new(type),
      .ident          = bl_ident_new(ident),
    },
    .modif            = modif
  };

  return save_to_cache(self, bo_new(NodeFuncDecl, &p));
}

NodeGlobalStmt *
bl_ast_node_global_stmt_new(Ast        *self,
                            const char *generated_from,
                            int         line,
                            int         col)
{
  NodeGlobalStmtParams p = {
    .base.type = BL_NODE_GLOBAL_STMT,
    .base.generated_from = generated_from,
    .base.line = line,
    .base.col = col,
  };

  return save_to_cache(self, bo_new(NodeGlobalStmt, &p));
}

NodeStmt *
bl_ast_node_stmt_new(Ast        *self,
                     const char *generated_from,
                     int         line,
                     int         col)
{
  NodeStmtParams p = {
    .base.type = BL_NODE_STMT,
    .base.generated_from = generated_from,
    .base.line = line,
    .base.col = col,
  };

  return save_to_cache(self, bo_new(NodeStmt, &p));
}

NodeParamVarDecl *
bl_ast_node_param_var_decl_new(Ast        *self,
                               char       *type,
                               char       *ident,
                               const char *generated_from,
                               int         line,
                               int         col)
{
  NodeParamVarDeclParams p = {
    .base = {
      .base = {
        .type = BL_NODE_PARAM_VAR_DECL,
        .generated_from = generated_from,
        .line = line,
        .col = col
      },
      .type = bl_type_new(type),
      .ident = bl_ident_new(ident)
    }
  };

  return save_to_cache(self, bo_new(NodeParamVarDecl, &p));
}

NodeReturnStmt *
bl_ast_node_return_stmt_new(Ast        *self,
                            const char *generated_from,
                            int         line,
                            int         col)
{

  NodeReturnStmtParams p = {
    .base.type = BL_NODE_RETURN_STMT,
    .base.generated_from = generated_from,
    .base.line = line,
    .base.col = col,
  };

  return save_to_cache(self, bo_new(NodeReturnStmt, &p));
}

NodeIntConst *
bl_ast_node_int_const_new(Ast               *self,
                          unsigned long long num,
                          const char        *generated_from,
                          int                line,
                          int                col)
{
  NodeIntConstParams p = {
    .base = {
      .base = {
        .type = BL_NODE_INT_CONST,
        .generated_from = generated_from,
        .line = line,
        .col = col,
      }
    },
    .num = num
  };

  return save_to_cache(self, bo_new(NodeIntConst, &p));
}

NodeStringConst *
bl_ast_node_string_const_new(Ast        *self,
                             char       *string,
                             const char *generated_from,
                             int         line,
                             int         col)
{
  NodeStringConstParams p = {
    .base = {
      .base = {
        .type = BL_NODE_STRING_CONST,
        .generated_from = generated_from,
        .line = line,
        .col = col,
      }
    },
    .string = string
  };

  return save_to_cache(self, bo_new(NodeStringConst, &p));
}

NodeVarDecl *
bl_ast_node_var_decl_new(Ast        *self,
                         char       *type,
                         char       *ident,
                         const char *generated_from,
                         int         line,
                         int         col)
{
  NodeVarDeclParams p = {
    .base = {
      .base = {
        .type = BL_NODE_VAR_DECL,
        .generated_from = generated_from,
        .line = line,
        .col = col,
      },
      .type = bl_type_new(type),
      .ident = bl_ident_new(ident)
    }
  };

  return save_to_cache(self, bo_new(NodeVarDecl, &p));
}

NodeBinop *
bl_ast_node_binop_new(Ast        *self,
                      bl_sym_e    op,
                      const char *generated_from,
                      int         line,
                      int         col)
{
  NodeBinopParams p = {
    .base.type = BL_NODE_BINOP,
    .base.generated_from = generated_from,
    .base.line = line,
    .base.col = col,
    .operator = op
  };

  return save_to_cache(self, bo_new(NodeBinop, &p));
}

NodeCall*
bl_ast_node_call_new(Ast        *self,
                     char       *calle,
                     const char *generated_from,
                     int         line,
                     int         col)
{
  NodeCallParams p = {
    .base = {
      .base = {
        .type = BL_NODE_CALL,
        .generated_from = generated_from,
        .line = line,
        .col = col,
      }
    },
    .calle = bl_ident_new(calle)
  };

  return save_to_cache(self, bo_new(NodeCall, &p));
}

NodeDeclRef *
bl_ast_node_decl_ref_new(Ast        *self,
                         char       *ident,
                         const char *generated_from,
                         int         line,
                         int         col)
{
  NodeDeclRefParams p = {
    .base = {
      .base = {
        .type = BL_NODE_DECL_REF,
        .generated_from = generated_from,
        .line = line,
        .col = col,
      }
    },
    .ident = ident
  };

  return save_to_cache(self, bo_new(NodeDeclRef, &p));
}

