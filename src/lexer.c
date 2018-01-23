#include <string.h>
#include <stdbool.h>
#include "lexer.h"

static const char *dstr[COUNT] = {
  " ", // space
  "==", // compare
  "//", // comment
  "/*", // comment_begin
  "*/", // comment_end
  "\n", // new line
  "(", // lb
  ")", // rb
  "{", // begin
  "}", // end
  "=", // set
  ":", // colon
  ";", // semicolon
  "#", // cmp time
  ",", // comma
  "[", // al
  "]", // ar
  "<", // less
  ">", // greater
  "*", // star
  "\"", // quote
  "%", // perc
  ".", // dot
};

static size_t
parse_delimiter(char *src, delimiter_e *d)
{
  size_t dl;
  for (int i = 0; i < COUNT; ++i) {
    dl = strlen(dstr[i]);
    if (strncmp(src, dstr[i], dl) == 0) {
      *d = (delimiter_e) i;
      return dl;
    }
  }
  return 0;
}

static size_t
parse_token(char *src,
            char *token)
{
  char *iter = src;
  delimiter_e d;

  while (*iter != '\0' && parse_delimiter(iter, &d) == 0) {
    iter++;
  }

  size_t tl = iter - src;
  strncpy(token, src, tl);
  token[tl] = '\0';

  return tl;
}

size_t
parse(char        **src,
      data_type_e  *type,
      data_u       *data)
{
  if ((**src) == '\0')
    return 0;

  delimiter_e d;
  size_t pl;

  pl = parse_delimiter(*src, &d);
  if (pl > 0) {
    *type = DELIMITER;
    (*data).delimiter = d;
    *src += pl;
    return pl;
  }

  *type = TOKEN;
  pl = parse_token(*src, &(*data).token[0]);
  *src += pl;

  return pl;
}

const char *
delimiter_to_str(delimiter_e d)
{
  if (d < COUNT)
    return dstr[d];

  return "unknown delimiter";
}
