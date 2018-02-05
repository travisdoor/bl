//*****************************************************************************
// bl
//
// File:   unit.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include "unit.h"
#include "bldebug.h"

/* class Unit */
bo_decl_params_begin(Unit)
  const char *filepath;
bo_end();

bo_impl_type(Unit, Actor);

void
UnitKlass_init(UnitKlass *klass)
{
}

void
Unit_ctor(Unit *self, UnitParams *p)
{
  /* constructor */
  bo_parent_ctor(Actor, p);
  self->filepath = bo_string_new_str(p->filepath);
}

void
Unit_dtor(Unit *self)
{
  bo_unref(self->filepath);
  bo_unref(self->src);
  bo_unref(self->tokens);
  bo_unref(self->ast);
}

bo_copy_result
Unit_copy(Unit *self, Unit *other)
{
  return BO_NO_COPY;
}
/* class Unit end */

/* public */
Unit *
bl_unit_new(const char *filepath)
{
  UnitParams params = {
    .filepath = filepath
  };
  return bo_new(Unit, &params);
}

const char*
bl_unit_src_file(Unit *self)
{
  return bo_string_get(self->filepath);
}

const char*
bl_unit_src(Unit *self)
{
  return bo_string_get(self->src);
}

