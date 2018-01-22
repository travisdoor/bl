#include <stdio.h>
#include <stdlib.h>
#include "parser.h"

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

  type_t type;
  data_u data;
  char *iter = src;
  while(parse(&iter, &type, &data)) {
    if (type == DELIMITER)
      printf("parsed: %s:%c\n", "DELIMITER", (char) data.delimiter);
    else
      printf("parsed: %s:%s\n", "TOKEN", data.token);
  }

  free(src);

  return 0;
}
