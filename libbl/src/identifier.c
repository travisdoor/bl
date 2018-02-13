//*****************************************************************************
// bl
//
// File:   identificator.c
// Author: Martin Dorazil
// Date:   13.2.18
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

#include <bobject/containers/hash.h>
#include "bl/identifier.h"

/* class Ident */

/* class Ident constructor params */
bo_decl_params_begin(Ident)
  /* constructor params */
  char *name;
bo_end();

/* class Ident object members */
bo_decl_members_begin(Ident, BObject)
  /* members */
  char *name;
  uint32_t hash;
bo_end();

bo_impl_type(Ident, BObject);

void
IdentKlass_init(IdentKlass *klass)
{
}

void
Ident_ctor(Ident *self, IdentParams *p)
{
  /* constructor */
  self->name = p->name;
  self->hash = bo_hash_from_str(p->name);
}

void
Ident_dtor(Ident *self)
{
  free(self->name);
}

bo_copy_result
Ident_copy(Ident *self, Ident *other)
{
  return BO_NO_COPY;
}
/* class Ident end */

Ident *
bl_ident_new(char *name)
{
  IdentParams p = {
    .name = name
  };

  return bo_new(Ident, &p);
}

const char *
bl_ident_get_name(Ident *self)
{
  return self->name;
}

uint32_t
bl_ident_get_hash(Ident *self)
{
  return self->hash;
}
