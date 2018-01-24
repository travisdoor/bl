#include <stdio.h>
#include <string.h>
#include "lexer.h"

/* class Lexer */

/* class Lexer constructor params */
bo_decl_params_begin(Lexer)
  /* constructor params */
bo_end();

/* class Lexer object members */
bo_decl_members_begin(Lexer, BObject)
  /* members */
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

static bool
cmatchset(char c, char *set)
{
  for (char *curr = set; *curr != '\0'; curr++) {
    if (*curr == c)
      return true;
  }

  return false;
}

static void
push_token(BArray     *tokens,
           BArray     *cnt_buf,
           const char *symbol,
           size_t      symbol_len,
           const char *content)
{
  bl_token tok = {0};
  if (symbol_len == 0)
    symbol_len = strlen(symbol);
  strncpy(tok.symbol, symbol, symbol_len);

  if (content != NULL) {
    BString *cs = bo_string_new_str(content);
    tok.content = cs;

    bo_array_push_back(cnt_buf, cs);
  } else {
    tok.content = NULL;
  }

  bo_array_push_back(tokens, tok);
}

/* public */
Lexer *
bl_lexer_new(void)
{
  LexerParams p = {};
  return bo_new(Lexer, &p);
}

/*
 * operator +-/*
 * string
 * number 0123456789
 * symbol
 * block {}
 */

void
bl_lexer_process_str(char    *str,
                     BArray  *tokens,
                     BArray  *cnt_buf)
{
  char seq[1024] = {0};
  size_t i = 0;
  for (char *curr = str; *curr != '\0'; curr++) {
    if (cmatchset(*curr, " \n")) {
      if (i != 0) {
        push_token(tokens, cnt_buf, "symbol", 0, seq);
        i = 0;
      }
    } else if (cmatchset(*curr, "+-/*")) {
      if (i != 0) {
        push_token(tokens, cnt_buf, "symbol", 0, seq);
        i = 0;
      }
      push_token(tokens, cnt_buf, "operator", 0, NULL);
    } else if (cmatchset(*curr, "{}()[],:=;")) {
      if (i != 0) {
        push_token(tokens, cnt_buf, "symbol", 0, seq);
        i = 0;
      }
      push_token(tokens, cnt_buf, curr, 1, NULL);
    } else {
      seq[i] = *curr;
      i++;
      seq[i] = '\0';
    }
  }
}
