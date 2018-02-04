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

#ifndef BISCUIT_ACTOR_H
#define BISCUIT_ACTOR_H

#include <bobject/bobject.h>
#include <bobject/containers/array.h>

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

/* Actor members */
bo_decl_members_begin(Actor, BObject)
  bl_actor_state_e state;
  BArray *actors;
bo_end();

Actor *
bl_actor_new(void);

bl_actor_state_e
bl_actor_state(Actor *self);

void
bl_actor_add(Actor *self, Actor *child);

#endif /* end of include guard: BISCUIT_ACTOR_H */

