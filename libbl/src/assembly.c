//*****************************************************************************
// bl
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
#include "assembly_impl.h"

/* class Assembly */
static bool
compile_group(Assembly          *self,
              bl_compile_group_e group);

bo_impl_type(Assembly, BObject);

bo_decl_params_begin(Assembly)
  const char *name;
  Pipeline   *pipeline;
bo_end();

void
AssemblyKlass_init(AssemblyKlass *klass)
{
}

void
Assembly_ctor(Assembly *self, AssemblyParams *p)
{
  /* constructor */
  self->name = strdup(p->name);
  self->units = bo_array_new_bo(bo_typeof(Unit), true);
  self->pipeline = p->pipeline;
}

void
Assembly_dtor(Assembly *self)
{
  free(self->name);
  bo_unref(self->units);
}

bo_copy_result
Assembly_copy(Assembly *self, Assembly *other)
{
  return BO_NO_COPY;
}
/* class Assembly end */

bool
compile_group(Assembly          *self,
              bl_compile_group_e group)
{
  const size_t c = bo_array_size(self->units);
  Unit *unit = NULL;
  for (size_t i = 0; i < c; i++) {
    unit = bo_array_at(self->units, i, Unit *);
    if (!bl_pipeline_run(self->pipeline, (Actor *) unit, group)) {
      self->failed = (Unit *) bl_pipeline_get_failed(self->pipeline);
      return false;
    }
  }

  return true;
}

Assembly *
bl_assembly_new(const char *name,
                Pipeline   *pipeline)
{
  AssemblyParams p = {
    .name     = name,
    .pipeline = pipeline
  };

  return bo_new(Assembly, &p);
}

void
bl_assembly_add_unit(Assembly *self,
                     Unit     *unit)
{
  /* TODO: handle duplicity */
  bo_array_push_back(self->units, unit);
}

bool
bl_assembly_compile(Assembly *self)
{
  if (self->pipeline == NULL) {
    bl_error("no pipeline set for assembpy %s", self->name);
    return false;
  }

  self->failed = NULL;

  return
    compile_group(self, BL_CGROUP_PRE_ANALYZE) &&
    compile_group(self, BL_CGROUP_ANALYZE);
}

Unit *
bl_assembly_failed(Assembly *self)
{
  return self->failed;
}

