//*****************************************************************************
// bl
//
// File:   parser2.c
// Author: Martin Dorazil
// Date:   3/15/18
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
#include "ast/ast2_impl.h"

typedef struct
{
  bl_builder_t *builder;
  bl_unit_t *unit;

  jmp_buf jmp_error;
} context_t;

static bl_item_t *
parse_item(context_t *cnt);

static bl_item_t *
parse_item(context_t *cnt)
{
  return NULL;
}

bl_error_e
bl_parser2_run(bl_builder_t *builder,
               bl_unit_t *unit)
{
  context_t
    cnt =
    {.builder = builder, .unit = unit};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error))) {
    return (bl_error_e) error;
  }

  parse_item(&cnt);

  return BL_NO_ERR;
}
