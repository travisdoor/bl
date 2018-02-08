//*****************************************************************************
// bl 
//
// File:   actor.h
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

#ifndef ACTOR_H_TQV6OMSD
#define ACTOR_H_TQV6OMSD

#include <bobject/bobject.h>
#include <bobject/containers/array.h>
#include <bobject/containers/string.h>

#define BL_ACTOR_MAX_ERROR_LEN 1024

typedef enum _bl_actor_state_e
{
  BL_ACTOR_STATE_PENDING,
  BL_ACTOR_STATE_FINISHED,
  BL_ACTOR_STATE_FAILED
} bl_actor_state_e;

/* class declaration */
bo_decl_type_begin(Actor, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT bl_actor_state_e
bl_actor_state(Actor *self);

extern BO_EXPORT void
bl_actor_add(Actor *self, Actor *child);

extern BO_EXPORT const char *
bl_actor_get_error(Actor *self);

extern BO_EXPORT void
bl_actor_error_reset(Actor *self);

#endif /* end of include guard: ACTOR_H_TQV6OMSD */

