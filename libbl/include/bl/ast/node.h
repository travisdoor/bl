//*****************************************************************************
// bl
//
// File:   node.h
// Author: Martin Dorazil
// Date:   8.2.18
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

#ifndef BL_NODE_H
#define BL_NODE_H

#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include <bobject/containers/array.h>

BO_BEGIN_DECLS

#define BL_NTYPE_LIST\
  nt(FUNC_DECL, "func_decl") \
  nt(GLOBAL_STMT, "global_stmt") \
  nt(STMT, "stmt") \
  nt(PARAM_VAR_DECL, "param_var_decl") \

typedef enum {
#define nt(tok, str) BL_NODE_##tok,
  BL_NTYPE_LIST
#undef nt
} bl_node_e;

/* class declaration */
bo_decl_type_begin(Node, BObject)
  /* virtuals */
  BString *(*to_string)(Node *);
bo_end();

extern BO_EXPORT bool
bl_node_add_child(Node *self,
                  Node *child);

extern BO_EXPORT bl_node_e
bl_node_type(Node *self);

extern BO_EXPORT BArray *
bl_node_children(Node *self);

BO_END_DECLS

#endif //BL_NODE_H
