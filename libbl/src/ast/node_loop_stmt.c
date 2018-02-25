//*****************************************************************************
// bl
//
// File:   node_loop_stmt.c
// Author: Martin Dorazil
// Date:   25/02/2018
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

#include "ast/node_loop_stmt_impl.h"
/* class NodeLoopStmt */

/* class NodeLoopStmt constructor params */
/* class NodeLoopStmt object members */
bo_decl_members_begin(NodeLoopStmt, Node)
  /* members */
  NodeStmt *cmp_stmt;
bo_end();

bo_impl_type(NodeLoopStmt, Node);

void
NodeLoopStmtKlass_init(NodeLoopStmtKlass *klass)
{
}

void
NodeLoopStmt_ctor(NodeLoopStmt *self, NodeLoopStmtParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Node, p);

  /* initialize self */
  self->cmp_stmt = p->cmp_stmt;
}

void
NodeLoopStmt_dtor(NodeLoopStmt *self)
{
}

bo_copy_result
NodeLoopStmt_copy(NodeLoopStmt *self, NodeLoopStmt *other)
{
  return BO_NO_COPY;
}
/* class NodeLoopStmt end */

NodeStmt *
bl_node_loop_get_stmt(NodeLoopStmt *self)
{
  return self->cmp_stmt;
}
