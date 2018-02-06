//*****************************************************************************
// bl 
//
// File:   node.h
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

#ifndef BISCUIT_NODE_H
#define BISCUIT_NODE_H

#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include <bobject/containers/array.h>

#define NTYPES\
  nt(FUNC_DECL, "func_decl") \
  nt(GLOBAL_STMT, "global_stmt") \
  nt(PARAM_VAR_DECL, "param_var_decl") \

typedef enum {
#define nt(tok, str) BL_NODE_##tok,
  NTYPES 
#undef nt
} bl_node_e;

static char *bl_node_strings[] = {
#define nt(tok, str) str,
  NTYPES 
#undef nt
};

#undef SYMBOLS

/* class declaration */
bo_decl_type_begin(Node, BObject)
  /* virtuals */
  BString *(*to_string)(Node *);
bo_end();

/* Node constructor parameters */
bo_decl_params_begin(Node)
  bl_node_e type;
  const char *generated_from;
  int line;
  int col;
bo_end();

/* Node members */
bo_decl_members_begin(Node, BObject)
  /* not owning, references only!!! */
  BArray *nodes;
  bl_node_e type;
  const char *generated_from;
  int line;
  int col;
bo_end();

bool
bl_node_add_child(Node *self,
                  Node *child);

#endif /* end of include guard: BISCUIT_NODE_H */

