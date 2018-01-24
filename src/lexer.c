#include <stdio.h>
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
  puts("new lexer");
}

void
Lexer_dtor(Lexer *self)
{
  puts("delete lexer");
}

bo_copy_result
Lexer_copy(Lexer *self, Lexer *other)
{
  return BO_NO_COPY;
}
/* class Lexer end */

Lexer *
bl_lexer_new(void)
{
  LexerParams p = {};
  return bo_new(Lexer, &p);
}


