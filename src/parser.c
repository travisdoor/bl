//*****************************************************************************
// bl
//
// File:   parser.c
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
#include "parser.h"
#include "token.h"

typedef enum _ptype {
  BL_PT_BODY,
  BL_PT_SCOPE,
  BL_PT_METHOD
} ptype_e;

typedef struct _pnode {
  struct _pnode **next;
  size_t          count;
  ptype_e         type;
} pnode_t;

static int
parse_type(BArray  *tokens,
           size_t  *i)
{
  bl_token_t *tok = &bo_array_at(tokens, *i, bl_token_t);

  switch (tok->sym) {
    case BL_SYM_LPAREN:
      if (parse_method(tokens, &i)) break;
    default:
      puts("error: identifier expected");
      return 0;
  }

  return 1;
}

static int
parse_gscope(BArray  *tokens)
{
  size_t c = bo_array_size(tokens);
  for (size_t i = 0; i < c; ++i) {
    bl_token_t *tok = &bo_array_at(tokens, i, bl_token_t);

    switch (tok->sym) {
      case BL_SYM_IDENT:
        if (parse_type(tokens, &i)) break;
      default:
        puts("error: identifier expected");
        return 0;
    }
  }
  return 1;
}

/* public */

int
bl_parser_parse(BArray *tokens)
{
  if (bo_array_size(tokens) == 0)
    return 0;

  return parse_gscope(tokens);
}
