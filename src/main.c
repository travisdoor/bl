#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bobject/containers/string.h>
#include "lexer.h"

#define error(m, ...) \
  { fprintf(stderr, m, ##__VA_ARGS__); abort(); }

static Lexer *lexer;

static inline bool
is_valid_ctype(bl_token_t *tok)
{
  if (tok->len == 0 || tok->content.as_int == 0)
    return false;

  return strncmp(tok->content.as_string, "int", 3) == 0;
}

static bool
parse_end(Lexer   *lexer,
          BString *out)
{
  bl_lexer_scan(lexer);
  bl_token_t *tok = bl_lexer_tok(lexer);

  if (tok->sym == BL_SYM_SEMICOLON) {
    bo_string_append(out, ";\n");
    return true;
  }

  fprintf(stderr, "invalid token: missing semicolon\n");
  return false;
}

static bool
parse_asign(Lexer   *lexer,
            BString *out)
{
  bl_lexer_scan(lexer);
  bl_token_t *tok = bl_lexer_tok(lexer);
  char n[64];

  switch (tok->sym) {
    case BL_SYM_ASIGN:
      bo_string_append(out, " ");
      bo_string_append(out, "=");
      
      bl_lexer_scan(lexer);
      tok = bl_lexer_tok(lexer);

      switch (tok->sym) {
        case BL_SYM_NUM:
          sprintf(n, "%d", tok->content.as_int);
          bo_string_append(out, " ");
          bo_string_append(out, n);
          return parse_end(lexer, out);
        default:
          fprintf(stderr, "invalid token: number expected\n");
          return false;
      }

      break;
    default:
      fprintf(stderr, "invalid token: assignment expected\n");
      return false;
  }
  return true;
}

static bool
parse_decl(Lexer   *lexer,
           BString *out)
{
  bl_lexer_scan(lexer);
  bl_token_t *tok = bl_lexer_tok(lexer);

  switch (tok->sym) {
    case BL_SYM_IDENT:
      bo_string_append(out, " ");
      bo_string_appendn(out, tok->content.as_string, tok->len);
      parse_asign(lexer, out);
      break;
    default:
      fprintf(stderr, "invalid token: name expected\n");
      return false;
  }
  return true;
}

static bool
parse_scope(Lexer   *lexer,
            BString *out)
{
  bl_lexer_scan(lexer);
  bl_token_t *tok = bl_lexer_tok(lexer);
  printf("T: %s %.*s\n", bl_sym_strings[tok->sym], (int) tok->len, tok->content.as_string);

  switch (tok->sym) {
    case BL_SYM_IDENT:
      bo_string_appendn(out, tok->content.as_string, tok->len);
      if (parse_decl(lexer, out)) break; 
    default:
      fprintf(stderr, "invalid token: type expected\n");
      return false;
  }
  return true;
}

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
  BString *out_src = bo_string_new(1024);

  bo_string_append(
    out_src,
    "/* This is biscuit generated source, do not modify. */\n\n#include <stdio.h>\n\n"
  );


  printf ("Source: \n%s\n\n", src);
  bl_lexer_init(lexer, src);
  bl_token_t *tok;
  if (parse_scope(lexer, out_src))
    printf("\nOUT:\n%s\n", bo_string_get(out_src));

  bo_unref(lexer);
  bo_unref(out_src);
  return 0;
}
