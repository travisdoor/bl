//*****************************************************************************
// bl
//
// File:   linker.c
// Author: Martin Dorazil
// Date:   3/7/18
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

#include <setjmp.h>
#include "stages_impl.h"

#define link_error(cnt, code, format, ...) \
  { \
    bl_builder_error((cnt)->builder, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, (code)); \
  }

typedef struct {
  jmp_buf jmp_error;
} context_t;

bl_error_e
bl_linker_run(bl_builder_t *builder,
              bl_assembly_t *assembly)
{
  /*
   * Solve unsatisfied expressions and declaration collisions
   * between units.
   */
  return BL_NO_ERR;
}


