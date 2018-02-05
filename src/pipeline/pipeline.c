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
#include "pipeline/pipeline.h"
#include "bldebug.h"

#define EXPECTED_STAGE_COUNT 8
#define MAX_DOMAIN_COUNT 4
#define STARTING_DOMAIN 0

/* Pipeline members */
bo_decl_members_begin(Pipeline, BObject)
  BArray *stages[MAX_DOMAIN_COUNT];
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
  for (int i = 0; i < MAX_DOMAIN_COUNT; i++) {
    self->stages[i] = bo_array_new_bo(bo_typeof(Stage), true); 
    bo_array_reserve(self->stages[i], EXPECTED_STAGE_COUNT);
  }
}

/* Pipeline destructor */
void
Pipeline_dtor(Pipeline *self)
{
  for (int i = 0; i < MAX_DOMAIN_COUNT; i++) {
    bo_unref(self->stages[i]);
  }

  puts("pipeline destroyed");
}

/* Pipeline copy constructor */
bo_copy_result
Pipeline_copy(Pipeline *self, Pipeline *other)
{
  return BO_NO_COPY;
}

static bool 
run_domain(Pipeline *self,
           Actor    *actor,
           int       domain)
{
  /* in case actor has been already ran */
  if (actor->state != BL_ACTOR_STATE_PENDING)
    return false;

  const size_t c = bo_array_size(actor->actors);
  if (c == 0) {
    const size_t cs = bo_array_size(self->stages[domain]);
    Stage *stage = NULL;
    for (int i = 0; i < cs; i++) {
      stage = bo_array_at(self->stages[domain], i, Stage *);
      /* IDEA: state can be managed inside every stage */
      if (!bo_vtbl(stage, Stage)->run(stage, actor)) {
        actor->state = BL_ACTOR_STATE_FAILED;
        self->failed = actor;
        return false;
      }

      actor->state = BL_ACTOR_STATE_FINISHED;
    }

    return true;
  } 

  /* not leaf */
  for (int i = 0; i < c; i++) {
    if (!run_domain(self, bo_array_at(actor->actors, i, Actor *), domain+1)) {
        actor->state = BL_ACTOR_STATE_FAILED;
        return false;
    }
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
                Actor    *actor)
{
  return run_domain(self, actor, STARTING_DOMAIN);
}

void
bl_pipeline_add_stage(Pipeline *self,
                      Stage    *stage)
{
  const int domain = bo_vtbl(stage, Stage)->domain(stage);
  bl_assert(domain < MAX_DOMAIN_COUNT, "cannot add domain %i, maximum is %i\n", domain, MAX_DOMAIN_COUNT);
  bo_array_push_back(self->stages[domain], stage);
}

Actor *
bl_pipeline_get_failed(Pipeline *self)
{
  return self->failed;
}
