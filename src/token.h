#ifndef BL_TOKEN_H
#define BL_TOKEN_H

BO_BEGIN_DECLS

#define SYMBOLS \
  sm(EOF, "end") \
  sm(LINE_COMMENT, "line_comment") \
  sm(IDENT, "identifier") \
  sm(STRING, "string") \
  sm(NUM, "number") \
  sm(RET, "return") \
  sm(IF, "if") \
  sm(ELSE, "else") \
  sm(LBLOCK, "{") \
  sm(RBLOCK, "}") \
  sm(LPAREN, "(") \
  sm(RPAREN, ")") \
  sm(SEMICOLON, ";") \
  sm(ASIGN, "=") \
  sm(SLASH, "/") \

typedef enum {
#define sm(tok, str) BL_SYM_##tok,
  SYMBOLS
#undef sm
} bl_sym_e;

static char *bl_sym_strings[] = {
#define sm(tok, str) str,
  SYMBOLS
#undef sm
};

#undef SYMBOLS

typedef struct
{
  bl_sym_e sym;
  size_t len;
  union content_u {
    const char *as_string;
    double      as_double;
    int         as_int;
  } content;
} bl_token_t;

BO_END_DECLS

#endif //BL_TOKEN_H
