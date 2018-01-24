#include <bobject/bobject.h>

BO_BEGIN_DECLS

/* class Lexer declaration */
bo_decl_type_begin(Lexer, BObject)
  /* virtuals */
bo_end();

extern BO_EXPORT Lexer *
bl_lexer_new(void);

BO_END_DECLS
