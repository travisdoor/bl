//*****************************************************************************
// blc
//
// File:   node_const.c
// Author: Martin Dorazil
// Date:   11/02/2018
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

#include "ast/node_const_impl.h"
#include "bl/bldebug.h"

static BString *
to_string(NodeConst *self);

/* class NodeConst */
/* class NodeConst object members */
bo_decl_members_begin(NodeConst, NodeExpr)
  /* members */
  union {
    const char *as_string;
    char as_char;
    int as_int;
    bool as_bool;
  } content;
  bl_node_conts_type_e type;
bo_end();

bo_impl_type(NodeConst, NodeExpr);

void
NodeConstKlass_init(NodeConstKlass *klass)
{
  bo_vtbl_cl(klass, Node)->to_string = (BString *(*)(Node *)) to_string;
}

void
NodeConst_ctor(NodeConst *self, NodeConstParams *p)
{
  /* constructor */

  /* initialize parent */
  bo_parent_ctor(NodeExpr, p);
}

void
NodeConst_dtor(NodeConst *self)
{
}

bo_copy_result
NodeConst_copy(NodeConst *self, NodeConst *other)
{
  return BO_NO_COPY;
}
/* class NodeConst end */

BString *
to_string(NodeConst *self)
{
  char str[512];
  switch (self->type) {
    case BL_CONST_INT:
      snprintf(str, 512, "int %d", self->content.as_int);
      break;
    case BL_CONST_BOOL:
      sprintf(str, "bool %d", self->content.as_bool);
      break;
    case BL_CONST_STRING:
      sprintf(str, "string %s", self->content.as_string);
      break;
    case BL_CONST_CHAR:
      sprintf(str, "char %c", self->content.as_char);
      break;
  }

  BString *ret = bo_string_new(128);
  bo_string_append(ret, "<");
  bo_string_append(ret, bl_node_strings[bo_members(self, Node)->type]);
  bo_string_append(ret, " ");
  bo_string_append(ret, &str[0]);
  bo_string_append(ret, ">");
  return ret;
}

int
bl_node_const_get_int(NodeConst *self)
{
  bl_assert(self->type == BL_CONST_INT, "invalid constant node type");
  return self->content.as_int;
}

void
bl_node_const_set_int(NodeConst *self,
                      int        val)
{
  self->type = BL_CONST_INT;
  self->content.as_int = val;
}

bool
bl_node_const_get_bool(NodeConst *self)
{
  bl_assert(self->type == BL_CONST_BOOL, "invalid constant node type");
  return self->content.as_bool;
}

void
bl_node_const_set_bool(NodeConst *self,
                       bool val)
{
  self->type = BL_CONST_BOOL;
  self->content.as_bool = val;
}

const char *
bl_node_const_get_str(NodeConst *self)
{
  bl_assert(self->type == BL_CONST_STRING, "invalid constant node type");
  return self->content.as_string;
}

void
bl_node_const_set_str(NodeConst *self,
                      const char *val)
{
  self->type = BL_CONST_STRING;
  self->content.as_string = val;
}

bl_node_conts_type_e
bl_node_const_get_type(NodeConst *self)
{
  return self->type;
}

char
bl_node_const_get_char(NodeConst *self)
{
  bl_assert(self->type == BL_CONST_CHAR, "invalid constant node type");
  return self->content.as_char;
}

void
bl_node_const_set_char(NodeConst *self,
                       char val)
{
  self->type = BL_CONST_CHAR;
  self->content.as_char = val;
}
