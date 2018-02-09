//*****************************************************************************
// bl 
//
// File:   pipeline.h
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

#ifndef PIPELINE_H_QHV37IME
#define PIPELINE_H_QHV37IME

#include <bobject/bobject.h>
#include "bl/pipeline/actor.h"
#include "bl/pipeline/stage.h"

#define BL_PIPELINE_MAX_GROUP_COUNT 8

BO_BEGIN_DECLS
/* class declaration */
bo_decl_type_begin(Pipeline, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT Pipeline *
bl_pipeline_new(void);

extern BO_EXPORT bool
bl_pipeline_run(Pipeline *self,
                Actor    *actor,
                int       group);

extern BO_EXPORT void
bl_pipeline_add_stage(Pipeline *self,
                      Stage *stage);

extern BO_EXPORT Actor *
bl_pipeline_get_failed(Pipeline *self);
BO_END_DECLS

#endif /* end of include guard: PIPELINE_H_QHV37IME */
