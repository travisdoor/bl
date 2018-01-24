#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include <bobject/containers/array.h>

#define BL_MAX_SYMBOL_LENTH 64

BO_BEGIN_DECLS

typedef struct _bl_token
{
  char symbol[BL_MAX_SYMBOL_LENTH];
  BString *content;
} bl_token;


/* class Lexer declaration */
bo_decl_type_begin(Lexer, BObject)
  /* virtuals */
bo_end();

Lexer *
bl_lexer_new(void);

void
bl_lexer_process_str(char   *str,
                     BArray *tokens,
                     BArray *cnt_buf);

BO_END_DECLS
