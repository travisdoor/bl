//*****************************************************************************
// blc
//
// File:   assembly.c
// Author: Martin Dorazil
// Date:   09/02/2018
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

#include <string.h>
#include "bl/bldebug.h"
#include "bl/assembly.h"

/* class Assembly object members */
bo_decl_members_begin(Assembly, BObject)
  /* members */
  BArray *units;
  char *name;
bo_end();

/* class Assembly */
bo_impl_type(Assembly, BObject);

bo_decl_params_begin(Assembly)
  const char *name;
bo_end();

void
AssemblyKlass_init(AssemblyKlass *klass)
{
}

void
Assembly_ctor(Assembly *self,
              AssemblyParams *p)
{
  /* constructor */
  self->name = strdup(p->name);
  self->units = bo_array_new_bo(bo_typeof(Unit), true);
}

void
Assembly_dtor(Assembly *self)
{
  free(self->name);
  bo_unref(self->units);
}

bo_copy_result
Assembly_copy(Assembly *self,
              Assembly *other)
{
  return BO_NO_COPY;
}

/* class Assembly end */

Assembly *
bl_assembly_new(const char *name)
{
  AssemblyParams p = {.name = name};
  return bo_new(Assembly, &p);
}

void
bl_assembly_add_unit(Assembly *self,
                     Unit *unit)
{
  /* TODO: handle duplicity */
  bo_array_push_back(self->units, unit);
}

const char *
bl_assembly_get_name(Assembly *self)
{
  return self->name;
}

int
bl_assembly_get_unit_count(Assembly *self)
{
  return (int) bo_array_size(self->units);
}

Unit *
bl_assembly_get_unit(Assembly *self,
                     int i)
{
  return bo_array_at(self->units, (size_t) i, Unit *);
}
