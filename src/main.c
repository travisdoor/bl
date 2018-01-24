#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

static Lexer *lexer;

int main(int argc, char *argv[])
{
  if (argc < 2)
    return 1;

  lexer = bl_lexer_new();

  printf ("Parsing: %s\n", argv[1]);

  FILE *f = fopen(argv[1], "rb");
  fclose(f);

  bo_unref(lexer);
  return 0;
}
