//*****************************************************************************
// bl
//
// File:   node_if_stmt.c
// Author: Martin Dorazil
// Date:   18/02/2018
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

#include "ast/node_if_stmt_impl.h"
/* class NodeIfStmt */

/* class NodeIfStmt object members */
bo_decl_members_begin(NodeIfStmt, Node)
  /* members */
  NodeExpr *condition;
  NodeStmt *then_stmt;
  NodeStmt *else_stmt;
bo_end();

bo_impl_type(NodeIfStmt, Node);

void
NodeIfStmtKlass_init(NodeIfStmtKlass *klass)
{
}

void
NodeIfStmt_ctor(NodeIfStmt *self, NodeIfStmtParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Node, p);

  /* initialize self */
  self->then_stmt = p->then_stmt;
  self->condition = p->condition;
  self->else_stmt = p->else_stmt;
}

void
NodeIfStmt_dtor(NodeIfStmt *self)
{
}

bo_copy_result
NodeIfStmt_copy(NodeIfStmt *self, NodeIfStmt *other)
{
  return BO_NO_COPY;
}
/* class NodeIfStmt end */

NodeExpr *
bl_node_if_stmt_get_cond(NodeIfStmt *self)
{
  return self->condition;
}

NodeStmt *
bl_node_if_stmt_get_then_stmt(NodeIfStmt *self)
{
  return self->then_stmt;
}

NodeStmt *
bl_node_if_stmt_get_else_stmt(NodeIfStmt *self)
{
  return self->else_stmt;
}
