#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

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

  BArray *tokens = bo_array_new(sizeof(bl_token));
  BArray *cnt_buf = bo_array_new_bo(bo_typeof(BString), true);
  bl_lexer_process_str(src, tokens, cnt_buf);

  bl_token token;
  for (size_t i = 0; i < bo_array_size(tokens); ++i) {
    token = bo_array_at(tokens, i, bl_token);
    printf("token: %s:%s\n", token.symbol, token.content != NULL ? bo_string_get(token.content) : "-");
  }

  bo_unref(tokens);
  bo_unref(cnt_buf);
  bo_unref(lexer);
  return 0;
}
