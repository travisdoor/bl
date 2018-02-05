//*****************************************************************************
// bl 
//
// File:   node.c
// Author: Martin Dorazil
// Date:   02/02/2018
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

#include "node.h"
#include "bldebug.h"

bo_impl_type(Node, BObject);

/* Node class init */
void
NodeKlass_init(NodeKlass *klass)
{
  bo_vtbl_cl(klass, Node)->to_string = NULL;
}

/* Node constructor */
void
Node_ctor(Node *self, NodeParams *p)
{
  self->type = p->type;
  self->generated_from = p->generated_from;
  self->line = p->line;
  self->col = p->col;
}

/* Node destructor */
void
Node_dtor(Node *self)
{
}

/* Node copy constructor */
bo_copy_result
Node_copy(Node *self, Node *other)
{
  return BO_NO_COPY;
}

