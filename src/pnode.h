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
  BL_PT_GSCOPE,
  BL_PT_NSCOPE,
  BL_PT_SCOPE,
  BL_PT_DECL,
  BL_PT_FUNC,
  BL_PT_CALL,
  BL_PT_ASGN,
  BL_PT_NAMESPACE,
  BL_PT_EXP,
  BL_PT_TYPE,
  BL_PT_ID,
  BL_PT_ARG,
  BL_PT_ARGS,
  BL_PT_END
} bl_ptype_e;

/* class Pnode declaration */
bo_decl_type_begin(Pnode, BObject)
  /* virtuals */
bo_end();

/* class Pnode object members */
bo_decl_members_begin(Pnode, BObject)
  /* members */
  BArray     *nodes;
  bl_ptype_e  type;
  bl_token_t *tok;
bo_end();

Pnode *
bl_pnode_new(bl_ptype_e  type,
             bl_token_t *token);

Pnode *
bl_pnode_new_child(Pnode      *self,
                   bl_ptype_e  type,
                   bl_token_t *token);

bool
bl_pnode_push(Pnode *self,
              Pnode *child);

#endif //BL_PNODE_H
