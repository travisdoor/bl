//*****************************************************************************
// bl
//
// File:   pnode.h
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

#ifndef BL_PNODE_H
#define BL_PNODE_H

#include <bobject/bobject.h>
#include <bobject/containers/array.h>
#include "token.h"

typedef enum _bl_ptype {
  BL_PT_UNKNOWN,
} bl_ptype_e;

/* class PNode declaration */
bo_decl_type_begin(PNode, BObject)
  /* virtuals */
bo_end();

/* class PNode object members */
bo_decl_members_begin(PNode, BObject)
  /* members */
  BArray     *nodes;
  bl_ptype_e  type;
  bl_token_t *tok;
bo_end();

PNode *
bl_pnode_new(bl_ptype_e  type,
             bl_token_t *token);

PNode *
bl_pnode_new_child(PNode      *self,
                   bl_ptype_e  type,
                   bl_token_t *token);

bool
bl_pnode_push(PNode *self,
              PNode *child);

#endif //BL_PNODE_H

