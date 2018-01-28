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
#include "evaluator.h"

#define ENABLE_LOG 0

#define error(m, ...) \
  { fprintf(stderr, m, ##__VA_ARGS__); abort(); }

#if ENABLE_LOG
void log_tokens(BArray *tokens)
{
  // log lexer output
  size_t c = bo_array_size(tokens);
  bl_token_t *tok;
  for (size_t i = 0; i < c; i++) {
     tok = &bo_array_at(tokens, i, bl_token_t);

    switch (tok->sym) {
    case BL_SYM_STRING:
    case BL_SYM_IDENT:
      printf("T[%zu]: %s:%.*s\n", i, bl_sym_strings[tok->sym], (int)tok->len, tok->content.as_string);
      break;
    case BL_SYM_NUM:
      printf("T[%zu]: %s:%d\n", i, bl_sym_strings[tok->sym], tok->content.as_int);
      break;
    default:
      printf("T[%zu]: %s\n", i, bl_sym_strings[tok->sym]);
    }
  }
}

void log_parsed(Pnode *node, int lpad)
{
  switch (node->type) {
    case BL_PT_GSCOPE:
      printf("%.*s[gscope]\n", lpad, "  ");
      break;
    case BL_PT_SCOPE:
      printf("%*s[scope]\n", lpad, "  ");
      break;
    case BL_PT_METHOD:
      printf("%*s[method] %.*s %.*s\n", lpad, "  ",
             (int) node->content.as_method.ret->len,
             node->content.as_method.ret->content.as_string,
             (int) node->content.as_method.name->len,
             node->content.as_method.name->content.as_string
      );
      break;
    case BL_PT_DECL:
      printf("%*s[decl] %.*s %.*s\n", lpad, "  ",
             (int) node->content.as_decl.type->len,
             node->content.as_decl.type->content.as_string,
             (int) node->content.as_decl.name->len,
             node->content.as_decl.name->content.as_string
      );
      break;
    case BL_PT_ATTRIBUTE:
      printf("%*s[attribute] %.*s %.*s\n", lpad, "  ",
             (int) node->content.as_attribute.header->len,
             node->content.as_attribute.header->content.as_string,
             (int) node->content.as_attribute.entry_point->len,
             node->content.as_attribute.entry_point->content.as_string
      );
      break;
    case BL_PT_CALL:
      printf("%*s[call] %.*s %.*s\n", lpad, "  ",
             (int) node->content.as_call.name->len,
             node->content.as_call.name->content.as_string,
             (int) node->content.as_call.param1->len,
             node->content.as_call.param1->content.as_string
      );
      break;
    default:
      abort();
  }

  if (node->nodes == NULL)
    return;

  size_t c = bo_array_size(node->nodes);
  Pnode *child;
  lpad++;
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(node->nodes, i, Pnode *);
    log_parsed(child, lpad);
  }
}
#endif

int main(int argc, char *argv[])
{
  BString *in_src;

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

#if ENABLE_LOG
  printf ("Source: \n%s\n\n", bo_string_get(in_src));
#endif

  puts("parsing...");
  BArray *tokens = bo_array_new(sizeof(bl_token_t));
  if (bl_lexer_scan(in_src, tokens)) {
#if ENABLE_LOG
    puts("lexer output:");
    log_tokens(tokens);
#endif

    Pnode *program = bl_parser_parse(tokens);
#if ENABLE_LOG
    // log parser output
    puts("parser output:");
    log_parsed(program, 0);
#endif

    if (program) {
      BString *out_src = bl_evaluator_evaluate(program);

#if ENABLE_LOG
      printf("\ngenerated source: \n%s", bo_string_get(out_src));
#endif

      FILE *fo = fopen("main.c", "w");
      if (fo == NULL)
        return 3;
      fwrite((char *)bo_string_get(out_src), bo_string_len(out_src), 1, f);
      fclose(f);

      bo_unref(out_src);
    }

    bo_unref(program);
  }

  bo_unref(in_src);
  return 0;
}
