//*****************************************************************************
// bl 
//
// File:   pipeline.c
// Author: Martin Dorazil
// Date:   04/02/2018
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

#include <bobject/containers/array.h>
#include "bl/pipeline/pipeline.h"
#include "bl/bldebug.h"
#include "bl/pipeline/stage.h"
#include "bl/pipeline/actor.h"

#define EXPECTED_STAGE_COUNT 8

/* Pipeline members */
bo_decl_members_begin(Pipeline, BObject)
  BArray *stages[BL_PIPELINE_MAX_GROUP_COUNT];
  Actor *failed;
bo_end();

/* Pipeline constructor parameters */
bo_decl_params_begin(Pipeline)
bo_end();

bo_impl_type(Pipeline, BObject);

/* Pipeline class init */
void
PipelineKlass_init(PipelineKlass *klass)
{
}

/* Pipeline constructor */
void
Pipeline_ctor(Pipeline *self, PipelineParams *p)
{
  for (int i = 0; i < BL_PIPELINE_MAX_GROUP_COUNT; i++) {
    self->stages[i] = bo_array_new_bo(bo_typeof(Stage), true); 
    bo_array_reserve(self->stages[i], EXPECTED_STAGE_COUNT);
  }
}

/* Pipeline destructor */
void
Pipeline_dtor(Pipeline *self)
{
  for (int i = 0; i < BL_PIPELINE_MAX_GROUP_COUNT; i++) {
    bo_unref(self->stages[i]);
  }
}

/* Pipeline copy constructor */
bo_copy_result
Pipeline_copy(Pipeline *self, Pipeline *other)
{
  return BO_NO_COPY;
}

static bool 
run_group(Pipeline *self,
         Actor *actor,
         int group)
{
  const size_t c = bo_array_size(self->stages[group]);
  if (c == 0)
    return true;

  Stage *stage = NULL;
  for (size_t i = 0; i < c; i++) {
    stage = bo_array_at(self->stages[group], i, Stage *);
    if (!bo_vtbl(stage, Stage)->run(stage, actor)) {
      /* FAIL */
      bl_actor_set_state(actor, BL_ACTOR_STATE_FAILED);
      self->failed = actor;
      return false;
    }

    bl_actor_set_state(actor, BL_ACTOR_STATE_FINISHED);
  }

  return true;
}

/* public */
Pipeline *
bl_pipeline_new(void)
{
  return bo_new(Pipeline, NULL);
}

bool 
bl_pipeline_run(Pipeline *self,
                Actor    *actor,
                int       group)
{
  bl_assert(group < BL_PIPELINE_MAX_GROUP_COUNT, "invalid group %i", group);
  return run_group(self, actor, group);
}

void
bl_pipeline_add_stage(Pipeline *self,
                      Stage *stage)
{
  bo_array_push_back(self->stages[stage->group], stage);
}

Actor *
bl_pipeline_get_failed(Pipeline *self)
{
  return self->failed;
}
