//*****************************************************************************
// bl
//
// File:   evaluator.c
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
#include <evaluator.h>
#include <string.h>
#include "src_context.h"
#include "token.h"
#include "snippets.h"
#include "bldebug.h"

#define SCOPE_PAD 2

static const char *header = "/* Biscuit generated source code, do not modify. */\n\n";
static const char *gprefix = "_bl_";

/* decl */

/* public */
BString *
bl_evaluator_evaluate(Pnode *node)
{
  // TODO: cache context for other sources???
  SrcContext *cnt = bl_src_context_new();

  if (node->type != BL_PT_GSCOPE)
    bl_parse_error("invalid node, expected global scope");

  BString * src = bo_string_new(10);  
  bo_unref(cnt);
  return src;
}
