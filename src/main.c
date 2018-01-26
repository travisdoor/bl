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
#include <stdlib.h>
#include <string.h>
#include <bobject/containers/string.h>
#include "lexer.h"
#include "parser.h"

#define error(m, ...) \
  { fprintf(stderr, m, ##__VA_ARGS__); abort(); }

int main(int argc, char *argv[])
{
  BString *in_src;
  BString *out_src;

  if (argc < 2)
    return 1;

  printf ("Input file: %s\n", argv[1]);

  FILE *f = fopen(argv[1], "r");
  if (f == NULL)
    return 2;
  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t) ftell(f);
  fseek(f, 0, SEEK_SET);

  in_src = bo_string_new(fsize);
  fread((char *)bo_string_get(in_src), fsize, 1, f);
  fclose(f);

  out_src = bo_string_new(1024);
  bo_string_append(out_src,
    "/* This is biscuit generated source, do not modify. */\n\n#include <stdio.h>\n\n");

  printf ("Source: \n%s\n\n", bo_string_get(in_src));

  BArray *tokens = bo_array_new(sizeof(bl_token_t));
  if (bl_lexer_scan(in_src, tokens)) {
    size_t c = bo_array_size(tokens);
    bl_token_t *tok;
    for (size_t i = 0; i < c; i++) {
      tok = &bo_array_at(tokens, i, bl_token_t);

      switch (tok->sym) {
      case BL_SYM_STRING:
      case BL_SYM_IDENT:
        printf("T: %s:%.*s\n", bl_sym_strings[tok->sym], (int)tok->len, tok->content.as_string);
        break;
      case BL_SYM_NUM:
        printf("T: %s:%d\n", bl_sym_strings[tok->sym], tok->content.as_int);
        break;
      default:
        printf("T: %s\n", bl_sym_strings[tok->sym]);
      }
    }

    bl_parser_parse(tokens);
  }


  bo_unref(in_src);
  bo_unref(out_src);
  return 0;
}
