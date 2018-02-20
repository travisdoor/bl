//*****************************************************************************
// bl
//
// File:   lexer_new.c
// Author: Martin Dorazil
// Date:   19/02/2018
//
// Copyright 2018 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//*****************************************************************************

#include <string.h>
#include <setjmp.h>
#include "bl/lexer_new.h"
#include "bl/unit.h"
#include "bl/bldebug.h"

#define is_intend_c(c) \
  (((c) >= 'a' && (c) <= 'z') || \
   ((c) >= 'A' && (c) <= 'Z')|| \
   ((c) >= '0' && (c) <= '9')|| \
   (c) == '_' || (c) == '-')

#define is_number_c(c) \
  ((c) >= '0' && (c) <= '9')

#define scan_error(cnt, format, ...) \
  { \
    bl_actor_error((Actor *)(cnt)->unit, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct _context_t
{
  Unit *unit;
  Tokens *tokens;
  jmp_buf jmp_error;
  char *c;
  int line;
  int col;
} context_t;

/* class LexerNew */
static bool
run(LexerNew *self,
    Unit *unit);

static void
scan(context_t *cnt);

static bool
scan_comment(context_t *cnt,
             const char *term);

static bool
scan_ident(context_t *cnt,
           bl_token_t *tok);

static bool
scan_string(context_t *cnt,
            bl_token_t *tok);

bool
scan_number(context_t *cnt,
            bl_token_t *tok);

/* class LexerNew constructor params */
bo_decl_params_with_base_begin(LexerNew, Stage)
  /* constructor params */
bo_end();

/* class LexerNew object members */
bo_decl_members_begin(LexerNew, Stage)
  /* members */
bo_end();

bo_impl_type(LexerNew, Stage);

void
LexerNewKlass_init(LexerNewKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
LexerNew_ctor(LexerNew *self,
              LexerNewParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);

  /* initialize self */
}

void
LexerNew_dtor(LexerNew *self)
{
}

bo_copy_result
LexerNew_copy(LexerNew *self,
              LexerNew *other)
{
  return BO_NO_COPY;
}

/* class LexerNew end */

bool
scan_comment(context_t *cnt,
             const char *term)
{
  const size_t len = strlen(term);
  while (true) {
    if (*cnt->c == '\n') {
      cnt->line++;
      cnt->col = 1;
    } else if (*cnt->c == '\0' && strcmp(term, "\n") != 0) {
      /*
       * Unterminated comment
       */
      scan_error(cnt,
                 "%s %d:%d unterminated comment block.",
                 bl_unit_get_name(cnt->unit),
                 cnt->line,
                 cnt->col);
    }
    if (strncmp(cnt->c, term, len) == 0) {
      break;
    }
    cnt->c++;
  }

  /* skip terminator */
  cnt->c += len;
  return true;
}

bool
scan_ident(context_t *cnt,
           bl_token_t *tok)
{
  tok->src_loc = cnt->c;
  tok->line = cnt->line;
  tok->col = cnt->col;
  tok->sym = BL_SYM_IDENT;
  tok->content.as_string = cnt->c;

  int len = 0;
  while (true) {
    if (!is_intend_c(*cnt->c)) {
      break;
    }

    len++;
    cnt->c++;
  }

  if (len == 0)
    return false;

  tok->len = len;
  cnt->col += len;
  return true;
}

bool
scan_string(context_t *cnt,
            bl_token_t *tok)
{
  if (*cnt->c != '\"') {
    return false;
  }

  tok->src_loc = cnt->c;
  tok->line = cnt->line;
  tok->col = cnt->col;
  tok->sym = BL_SYM_STRING;

  /* eat " */
  cnt->c++;
  tok->content.as_string = cnt->c;

  int len = 0;
  while (true) {
    if (*cnt->c == '\"') {
      cnt->c++;
      break;
    } else if (*cnt->c == '\0') {
      scan_error(cnt,
                 "%s %d:%d unterminated string.",
                 bl_unit_get_name(cnt->unit),
                 cnt->line,
                 cnt->col);
    }

    len++;
    cnt->c++;
  }

  tok->len = len;
  cnt->col += len + 2;
  return true;
}

bool
scan_number(context_t *cnt,
            bl_token_t *tok)
{
  tok->src_loc = cnt->c;
  tok->line = cnt->line;
  tok->col = cnt->col;
  tok->sym = BL_SYM_NUM;
  tok->content.as_string = cnt->c;

  unsigned long long n = 0;
  int len = 0;
  while (true) {
    if (!is_number_c(*(cnt->c))) {
      break;
    }

    n = n * 10 + (*cnt->c) - '0';
    len++;
    cnt->c++;
  }

  if (len == 0)
    return false;

  tok->len = len;
  cnt->col += len;
  tok->content.as_ull = n;

  return true;
}

void
scan(context_t *cnt)
{
  bl_token_t tok;
scan:
  tok.src_loc = cnt->c;
  tok.line = cnt->line;
  tok.col = cnt->col;

  /*
   * Ignored characters
   */
  switch (*cnt->c) {
    case '\0':
      tok.sym = BL_SYM_EOF;
      bl_tokens_push(cnt->tokens, &tok);
      return;
    case '\r':
    case '\n':
      cnt->line++;
      cnt->col = 1;
      cnt->c++;
      goto scan;
    case '\t':
      /* TODO: can be set by user */
      cnt->col += 2;
      cnt->c++;
      goto scan;
    case ' ':
      cnt->col++;
      cnt->c++;
      goto scan;
    default:
      break;
  }

  /*
   * Scan symbols described directly as strings.
   */
  size_t len = 0;
  for (int i = BL_SYM_IF; i < BL_SYM_NONE; i++) {
    len = strlen(bl_sym_strings[i]);
    if (strncmp(cnt->c, bl_sym_strings[i], len) == 0) {
      cnt->c += len;
      tok.sym = (bl_sym_e) i;

      /*
       * Two joined symbols will be parsed as identifier.
       */
      if (i >= BL_SYM_IF && i <= BL_SYM_NAMESPACE && is_intend_c(*cnt->c)) {
        /* roll back */
        cnt->c -= len;
        break;
      }

      switch (tok.sym) {
        case BL_SYM_LCOMMENT:
          /* begin of line comment */
          scan_comment(cnt, "\n");
          goto scan;
        case BL_SYM_LBCOMMENT:
          /* begin of block comment */
          scan_comment(cnt, bl_sym_strings[BL_SYM_RBCOMMENT]);
          goto scan;
        case BL_SYM_RBCOMMENT:
          goto scan;
        default:
          cnt->col += len;
          goto push_token;
      }
    }
  }

  /*
   * Scan special tokens.
   */
  if (scan_number(cnt, &tok))
    goto push_token;

  if (scan_ident(cnt, &tok))
    goto push_token;

  if (scan_string(cnt, &tok))
    goto push_token;

  /* When symbol is unknown report error */
  scan_error(cnt, "%s %d:%d unexpected token.", bl_unit_get_name(cnt->unit), cnt->line, cnt->col);
push_token:
  bl_tokens_push(cnt->tokens, &tok);
  goto scan;
}

bool
run(LexerNew *self,
    Unit *unit)
{
  Tokens *tokens = bl_tokens_new();
  bl_unit_set_tokens(unit, tokens);

  char *src = (char *) bl_unit_get_src(unit);

  bl_log(BL_GREEN("processing unit: %s"), bl_unit_get_name(unit));

  if (src == NULL) {
    bl_actor_error((Actor *) unit,
                   "No source loaded for unit " BL_YELLOW("'%s'") ", use builder flag "
                     BL_YELLOW("BL_BUILDER_LOAD_FROM_FILE") " or create unit from loaded source.",
                   bl_unit_get_name(unit));
    return false;
  }

  context_t cnt = {.c = src, .line = 1, .col = 1, .unit = unit, .tokens = tokens};

  if (setjmp(cnt.jmp_error))
    return false;

  scan(&cnt);

  return true;
}

/* public */
LexerNew *
bl_lexer_new_new(bl_compile_group_e group)
{
  LexerNewParams p = {.base.group = group};

  return bo_new(LexerNew, &p);
}
