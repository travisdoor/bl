//*****************************************************************************
// bl
//
// File:   main.c
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

#include <stdio.h>
#include "lexer.h"
#include "unit.h"
#include "module.h"
#include "pipeline/pipeline.h"
#include "bldebug.h"

#define ENABLE_LOG 0

int main(int argc, char *argv[])
{
  if (argc < 2)
    return 1;

  /* init actors */
  Actor *module = (Actor *)bl_module_new();

  for (int i = 1; i < argc; i++) {
    Actor *unit = (Actor *)bl_unit_new(argv[i]);
    bl_actor_add(module, unit);
  }

  /* init pipeline */
  Pipeline *pipeline = bl_pipeline_new();
  Stage *lexer = (Stage *)bl_lexer_new(); 

  bl_pipeline_add_stage(pipeline, lexer);
  
  bl_log("pipeline start\n");
  if (!bl_pipeline_run(pipeline, module)) {
    bl_error("pipeline run failed\n");
  } else 
    bl_log("pipeline finished without errors\n");

  bo_unref(module);
  bo_unref(pipeline);

  return 0;
}
