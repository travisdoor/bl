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

#define ENABLE_LOG 0

#if ENABLE_LOG
void log_tokens(Tokens *tokens)
{
  // log lexer output
  bl_token_t *tok;
  size_t i = 0;
  while ((tok = bl_tokens_consume(tokens))) {
    switch (tok->sym) {
    case BL_SYM_STRING:
    case BL_SYM_IDENT:
      printf("T[%zu %d:%d]: %s:%.*s\n", i, tok->line, tok->col,
          bl_sym_strings[tok->sym], (int)tok->len, tok->content.as_string);
      break;
    case BL_SYM_NUM:
      printf("T[%zu %d:%d]: %s:%d\n", i, tok->line, tok->col,
          bl_sym_strings[tok->sym], tok->content.as_int);
      break;
    default:
      printf("T[%zu %d:%d]: %s\n", i, tok->line, tok->col,
          bl_sym_strings[tok->sym]);
    }
    i++;
  }
  bl_tokens_resert_iter(tokens);
}

#endif

int main(int argc, char *argv[])
{
  if (argc < 2)
    return 1;

//  printf ("Input file: %s\n", argv[1]);

  Unit *unit = bl_unit_new(argv[1]);
  bl_unit_compile(unit);
  bo_unref(unit);
  return 0;
}
