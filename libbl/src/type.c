//*****************************************************************************
// blc
//
// File:   type.c
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

#include <bobject/containers/hash.h>
#include <string.h>
#include "bl/type.h"

static const char *bl_type_strings[] = {
#define tp(tok, str) str,
  BL_TYPE_LIST
#undef tp
};

/* class Type */
bo_decl_params_begin(Type)
  char *name;
bo_end();

/* class Type object members */
bo_decl_members_begin(Type, BObject)
  /* members */
  char     *name;
  uint32_t  t;
bo_end();

bo_impl_type(Type, BObject);

void
TypeKlass_init(TypeKlass *klass)
{
}

void
Type_ctor(Type *self, TypeParams *p)
{
  /* constructor */
  self->name = p->name;

  for (uint32_t  i = 0; i < BL_TYPE_COUNT; i++) {
    if (strcmp(bl_type_strings[i], self->name) == 0)
      self->t = i;
  }

  if (self->t == BL_TYPE_NONE) {
    self->t = bo_hash_from_str(self->name);
  }
}

void
Type_dtor(Type *self)
{
  free(self->name);
}

bo_copy_result
Type_copy(Type *self, Type *other)
{
  return BO_NO_COPY;
}
/* class Type end */

Type *
bl_type_new(char *name)
{
  TypeParams p = {
    .name = name
  };

  return bo_new(Type, &p);
}

uint32_t
bl_type_get(Type *self)
{
  return self->t;
}

const char *
bl_type_get_name(Type *self)
{
  return self->name;
}

bool
bl_type_is(Type    *self,
           uint32_t t)
{
  return self->t == t;
}

bool
bl_type_is_fundamental(Type *self)
{
  return self->t < BL_TYPE_COUNT;
}

bool
bl_type_is_user_defined(Type *self)
{
  return !bl_type_is_fundamental(self);
}

bool
bl_type_is_not(Type    *self,
               uint32_t t)
{
  return self->t != t;
}
