#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include "token.h"

BO_BEGIN_DECLS

/* class Lexer declaration */
bo_decl_type_begin(Lexer, BObject)
  /* virtuals */
bo_end();

Lexer *
bl_lexer_new(void);

void
bl_lexer_init(Lexer *self,
              char  *stream);

int
bl_lexer_scan(Lexer *self);

bl_token_t *
bl_lexer_tok(Lexer *self);

BO_END_DECLS
