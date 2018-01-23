#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main(int argc, char *argv[])
{
  if (argc < 2)
    return 1;

  printf ("Parsing: %s\n", argv[1]);

  FILE *f = fopen(argv[1], "rb");
  fclose(f);

  return 0;
}
