//*****************************************************************************
// bl 
//
// File:   stage.h
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

#ifndef STAGE_H_OGZN04CD
#define STAGE_H_OGZN04CD

#include <bobject/bobject.h>
#include "bl/pipeline/actor.h"
#include "bl/compile_group.h"

BO_BEGIN_DECLS
/* class declaration */
bo_decl_type_begin(Stage, BObject)
  /* virtuals */
  /*
   * Run operation on an actor.
   */
  bool (*run)(Stage*, Actor *);
bo_end();

/* Stage members */
bo_decl_members_begin(Stage, BObject)
  bl_compile_group_e group;
bo_end();

bo_decl_params_begin(Stage)
  bl_compile_group_e group;
bo_end();

BO_END_DECLS

#endif /* end of include guard: STAGE_H_OGZN04CD */
