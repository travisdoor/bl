//*****************************************************************************
// blc
//
// File:   node_cmp_stmt.c
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

#include <bobject/containers/array.h>
#include "node_cmp_stmt_impl.h"

/* NodeCmpStmt members */
bo_decl_members_begin(NodeCmpStmt, Node)
  BArray *nodes;
bo_end();

bo_impl_type(NodeCmpStmt, Node);

/* NodeCmpStmt class init */
void
NodeCmpStmtKlass_init(NodeCmpStmtKlass *klass)
{
}

/* NodeCmpStmt constructor */
void
NodeCmpStmt_ctor(NodeCmpStmt *self, NodeCmpStmtParams *p)
{
  bo_parent_ctor(Node, p);
}

/* NodeCmpStmt destructor */
void
NodeCmpStmt_dtor(NodeCmpStmt *self)
{
  bo_unref(self->nodes);
}

/* NodeCmpStmt copy constructor */
bo_copy_result
NodeCmpStmt_copy(NodeCmpStmt *self, NodeCmpStmt *other)
{
  return BO_NO_COPY;
}

bool
bl_node_stmt_add_child(NodeCmpStmt *self,
                       Node *node)
{
  if (node == NULL)
    return false;

  if (self->nodes == NULL)
    self->nodes = bo_array_new_bo(bo_typeof(Node), false);
  bo_array_push_back(self->nodes, node);
  return true;
}

int
bl_node_stmt_child_get_count(NodeCmpStmt *self)
{
  if (self->nodes == NULL) {
    return 0;
  }

  return (int)bo_array_size(self->nodes);
}

Node *
bl_node_stmt_get_child(NodeCmpStmt *self,
                       int i)
{
  return bo_array_at(self->nodes, (size_t)i, Node *);
}

