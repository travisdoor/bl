//*****************************************************************************
// bl 
//
// File:   node_global_stmt.c
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
#include "node_global_stmt_impl.h"

/* NodeGlobalStmt members */
bo_decl_members_begin(NodeGlobalStmt, Node)
  BArray *nodes;
bo_end();

bo_impl_type(NodeGlobalStmt, Node);

/* NodeGlobalStmt class init */
void
NodeGlobalStmtKlass_init(NodeGlobalStmtKlass *klass)
{
}

/* NodeGlobalStmt constructor */
void
NodeGlobalStmt_ctor(NodeGlobalStmt *self, NodeGlobalStmtParams *p)
{
  bo_parent_ctor(Node, p);
}

/* NodeGlobalStmt destructor */
void
NodeGlobalStmt_dtor(NodeGlobalStmt *self)
{
  bo_unref(self->nodes);
}

/* NodeGlobalStmt copy constructor */
bo_copy_result
NodeGlobalStmt_copy(NodeGlobalStmt *self, NodeGlobalStmt *other)
{
  return BO_NO_COPY;
}

int
bl_node_global_stmt_child_count(NodeGlobalStmt *self)
{
  if (self->nodes == NULL) {
    return 0;
  }

  return (int)bo_array_size(self->nodes);
}

Node * 
bl_node_global_stmt_child(NodeGlobalStmt *self,
                          int             i)
{
  return bo_array_at(self->nodes, i, Node *);   
}

bool
bl_node_global_stmt_add_child(NodeGlobalStmt *self,
                              Node *node)
{
  if (node == NULL)
    return false;

  if (self->nodes == NULL)
    self->nodes = bo_array_new_bo(bo_typeof(Node), false);
  bo_array_push_back(self->nodes, node);
  return true;
}


