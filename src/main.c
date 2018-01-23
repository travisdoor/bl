#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"

int main(int argc, char *argv[])
{
  if (argc < 2)
    return 1;

  printf ("Parsing: %s\n", argv[1]);

  FILE *f = fopen(argv[1], "rb");
  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  fseek(f, 0, SEEK_SET); 
  char *src = malloc(fsize + 1);
  fread(src, fsize, 1, f);
  fclose(f);

  data_type_e type;
  data_u data;
  char *iter = src;

  // test
  char out[100000];
  out[0] = '\0';

  while(parse(&iter, &type, &data)) {
  }

  printf("out: \n%s", out);
  free(src);

  return 0;
}
