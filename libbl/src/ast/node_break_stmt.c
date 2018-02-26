//*****************************************************************************
// bl
//
// File:   node_break_stmt.c
// Author: Martin Dorazil
// Date:   2/26/18
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

#include "ast/node_break_stmt_impl.h"
/* class NodeBreakStmt */

/* class NodeBreakStmt object members */
bo_decl_members_begin(NodeBreakStmt, Node)
  /* members */
bo_end();

bo_impl_type(NodeBreakStmt, Node);

void
NodeBreakStmtKlass_init(NodeBreakStmtKlass *klass)
{
}

void
NodeBreakStmt_ctor(NodeBreakStmt *self, NodeBreakStmtParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Node, p);

  /* initialize self */
}

void
NodeBreakStmt_dtor(NodeBreakStmt *self)
{
}

bo_copy_result
NodeBreakStmt_copy(NodeBreakStmt *self, NodeBreakStmt *other)
{
  return BO_NO_COPY;
}
/* class NodeBreakStmt end */
