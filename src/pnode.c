//*****************************************************************************
// bl
//
// File:   pnode.c
// Author: Martin Dorazil
// Date:   26/01/2018
//
// Copyright 2017 Martin Dorazil
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

#include "pnode.h"

/* class Pnode */
bo_decl_params_begin(Pnode)
  bl_ptype_e  type;
  bl_token_t *token;
bo_end();

bo_impl_type(Pnode, BObject);

void
PnodeKlass_init(PnodeKlass *klass)
{
}

void
Pnode_ctor(Pnode *self, PnodeParams *p)
{
  /* constructor */
  self->nodes = bo_array_new_bo(bo_typeof(Pnode), true);
  self->type  = p->type;
  self->tok   = p->token;
}

void
Pnode_dtor(Pnode *self)
{
  bo_unref(self->nodes);
}

bo_copy_result
Pnode_copy(Pnode *self, Pnode *other)
{
  return BO_NO_COPY;
}
/* class Pnode end */

Pnode *
bl_pnode_new(bl_ptype_e  type,
             bl_token_t *token)
{
  PnodeParams params = {
    .type  = type,
    .token = token
  };

  return bo_new(Pnode, &params);
}

Pnode *
bl_pnode_new_child(Pnode      *self,
                   bl_ptype_e  type,
                   bl_token_t *token)
{
  if (token == NULL) 
    return NULL;
  Pnode *child = bl_pnode_new(type, token);
  bo_array_push_back(self->nodes, child);
  return child;
}

bool
bl_pnode_push(Pnode *self,
              Pnode *child)
{
  if (!child)
    return false;
  bo_array_push_back(self->nodes, child);
  return true;
}

