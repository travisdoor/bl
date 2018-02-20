//*****************************************************************************
// blc
//
// File:   node_return_stmt.c
// Author: Martin Dorazil
// Date:   08/02/2018
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

#include "ast/node_return_stmt_impl.h"

/* NodeReturnStmt members */

bo_decl_members_begin(NodeReturnStmt, Node)
  NodeExpr *expr;
bo_end();

bo_impl_type(NodeReturnStmt, Node);

/* NodeReturnStmt class init */
void
NodeReturnStmtKlass_init(NodeReturnStmtKlass *klass)
{
}

/* NodeReturnStmt constructor */
void
NodeReturnStmt_ctor(NodeReturnStmt *self, NodeReturnStmtParams *p)
{
  bo_parent_ctor(Node, p);
}

/* NodeReturnStmt destructor */
void
NodeReturnStmt_dtor(NodeReturnStmt *self)
{
}

/* NodeReturnStmt copy constructor */
bo_copy_result
NodeReturnStmt_copy(NodeReturnStmt *self, NodeReturnStmt *other)
{
  return BO_NO_COPY;
}

bool
bl_node_return_stmt_add_expr(NodeReturnStmt *self,
                             NodeExpr       *expr)
{
  if (expr == NULL)
    return false;

  self->expr = expr;
  return true;
}

NodeExpr *
bl_node_return_stmt_get_expr(NodeReturnStmt *self)
{
  return self->expr;
}

