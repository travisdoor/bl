//*****************************************************************************
// bl 
//
// File:   actor.c
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

#include <string.h>
#include "pipeline/actor.h"
#include "bldebug.h"

/* Actor constructor parameters */
bo_decl_params_begin(Actor)
bo_end();

bo_impl_type(Actor, BObject);

/* Actor class init */
void
ActorKlass_init(ActorKlass *klass)
{
}

/* Actor constructor */
void
Actor_ctor(Actor *self, ActorParams *p)
{
  self->state = BL_ACTOR_STATE_PENDING;
  self->actors = bo_array_new_bo(bo_typeof(Actor), true);
  self->error[0] = '\0';
}

/* Actor destructor */
void
Actor_dtor(Actor *self)
{
  bo_unref(self->actors);
  bo_unref(self->error);
}

/* Actor copy constructor */
bo_copy_result
Actor_copy(Actor *self, Actor *other)
{
  return BO_NO_COPY;
}

/* public */
Actor *
bl_actor_new(void)
{
  return bo_new(Actor, NULL);
}

bl_actor_state_e
bl_actor_state(Actor *self)
{
  return self->state;
}

void
bl_actor_add(Actor *self, Actor *child)
{
  bo_array_push_back(self->actors, child);
}

void
bl_actor_error(Actor *self,
               const char *format,
               ...)
{
  va_list args;
  va_start(args, format);
  vsnprintf(self->error, BL_ACTOR_MAX_ERROR_LEN, format, args);
  va_end(args);
}

const char *
bl_actor_get_error(Actor *self)
{
  if (strlen(&self->error[0]) == 0)
    return NULL;
  return &self->error[0];
}

void
bl_actor_error_reser(Actor *self)
{
  self->error[0] = '\0';
}

