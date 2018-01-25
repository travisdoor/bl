#include <stdio.h>
#include <string.h>
#include "lexer.h"

/* class Lexer */
#define is_intend_c(c) \
  ((c) >= 'a' && (c) <= 'z' || \
   (c) >= 'A' && (c) <= 'Z' || \
   (c) >= '0' && (c) <= '9' || \
   (c) == '_' || (c) == '-')

#define is_number_c(c) \
  ((c) >= '0' && (c) <= '9')

/* class Lexer constructor params */
bo_decl_params_begin(Lexer)
  /* constructor params */
bo_end();

/* class Lexer object members */
bo_decl_members_begin(Lexer, BObject)
  /* members */
  char      *stream;
  size_t     offset;
  bl_token_t tok;
bo_end();

bo_impl_type(Lexer, BObject);

void
LexerKlass_init(LexerKlass *klass)
{
}

void
Lexer_ctor(Lexer *self, LexerParams *p)
{
  /* constructor */
}

void
Lexer_dtor(Lexer *self)
{
}

bo_copy_result
Lexer_copy(Lexer *self, Lexer *other)
{
  return BO_NO_COPY;
}
/* class Lexer end */

static inline char *
nextc(Lexer *self)
{
  return &self->stream[self->offset++];
}

static inline char *
prevc(Lexer *self)
{
  return &self->stream[--self->offset];
}

static int
scan_string(Lexer *self,
            char   term)
{
  self->tok.content.as_string = nextc(self);

  size_t len = 1;
  while (*(nextc(self)) != term) {
    len++;
  }

  self->tok.len = len;
  return 1;
}

static int
scan_ident(Lexer *self,
           char  *c)
{
  if (strncmp(c, "return", 5) == 0) {
    self->offset += 5;
    return self->tok.sym = BL_SYM_RET;
  }

  if (strncmp(c, "int", 3) == 0) {
    self->offset += 3;
    return self->tok.sym = BL_SYM_INT;
  }

  self->tok.content.as_string = c;
  size_t len = 0;
  while (is_intend_c(*c)) {
    c = nextc(self);
    len++;
  }

  prevc(self);
  self->tok.len = len;
  self->tok.sym = BL_SYM_IDENT;
  return self->tok.sym;
}

static int
scan_number(Lexer *self,
            char  *c)
{
  if (!is_number_c(*c))
    return 0;

  int n = 0;
  do {
    n = n * 10 + *c - '0';
    c = nextc(self);
  } while (is_number_c(*c));
  prevc(self);

  self->tok.sym = BL_SYM_NUM;
  self->tok.content.as_int = n;
  return self->tok.sym;
}

static inline void
reset_tok(Lexer *self)
{
  memset(&self->tok, 0, sizeof(bl_token_t));
}

/* public */
Lexer *
bl_lexer_new(void)
{
  return bo_new(Lexer, NULL);
}

void
bl_lexer_init(Lexer *self,
              char  *stream)
{
  self->stream = stream;
}

bl_token_t *
bl_lexer_tok(Lexer *self)
{
  return &self->tok;
}

int
bl_lexer_scan(Lexer *self)
{
  char *c;
skip:
  reset_tok(self);
  switch (*(c = nextc(self))) {
  case '\0':
    return self->tok.sym = BL_SYM_EOF;
  case ' ':
  case '\n':
  case '\t':
    goto skip;
  case '{':
    return self->tok.sym = BL_SYM_LBLOCK;
  case '}':
    return self->tok.sym = BL_SYM_RBLOCK;
  case '(':
    return self->tok.sym = BL_SYM_LPAREN;
  case ')':
    return self->tok.sym = BL_SYM_RPAREN;
  case '=':
    return self->tok.sym = BL_SYM_ASIGN;
  case ';':
    return self->tok.sym = BL_SYM_SEMICOLON;
  case '"':
    scan_string(self, '"');
    return self->tok.sym = BL_SYM_STRING;
  case '/':
    switch (*(c = nextc(self))) {
    case '/':
      scan_string(self, '\n');
      self->tok.sym = BL_SYM_LINE_COMMENT;
      goto skip;
    }
    return self->tok.sym = BL_SYM_SLASH;
  default:
    if (scan_number(self, c))
      return self->tok.sym;

    return scan_ident(self, c);
  }
}

