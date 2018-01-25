#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

#define error(m, ...) \
  { fprintf(stderr, m, ##__VA_ARGS__); abort(); }

static Lexer *lexer;

int main(int argc, char *argv[])
{
  if (argc < 2)
    return 1;

  lexer = bl_lexer_new();

  printf ("Input file: %s\n", argv[1]);

  FILE *f = fopen(argv[1], "r");
  if (f == NULL)
    return 2;
  fseek(f, 0, SEEK_END);
  size_t fsize = (size_t) ftell(f);
  fseek(f, 0, SEEK_SET);

  char *src = malloc(fsize + 1);
  fread(src, fsize, 1, f);
  fclose(f);

  src[fsize] = '\0';

  printf ("Source: \n%s\n\n", src);
  bl_lexer_init(lexer, src);
  bl_token_t *tok;
  while (bl_lexer_scan(lexer)) {
    tok = bl_lexer_tok(lexer);

    switch (tok->sym) {
    case BL_SYM_IDENT:
      break;
    default:
      error("syntax error");
    }

    printf("T: %s %.*s\n", bl_sym_strings[tok->sym], (int) tok->len, tok->content.as_string);
  }

  bo_unref(lexer);
  return 0;
}
