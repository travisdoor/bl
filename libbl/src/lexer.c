//*****************************************************************************
// blc
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
#include "stages_impl.h"

#define is_intend_c(c) \
  (((c) >= 'a' && (c) <= 'z') || \
   ((c) >= 'A' && (c) <= 'Z')|| \
   ((c) >= '0' && (c) <= '9')|| \
   (c) == '_' || (c) == '-')

#define is_number_c(c) \
  ((c) >= '0' && (c) <= '9')

#define scan_error(cnt, code, format, ...) \
  { \
    bl_builder_error((cnt)->builder, (format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, code); \
  }

typedef struct context
{
  bl_builder_t *builder;
  bl_unit_t    *unit;
  bl_tokens_t  *tokens;
  jmp_buf      jmp_error;
  char         *c;
  int          line;
  int          col;
} context_t;

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

static bool
scan_number(context_t *cnt,
            bl_token_t *tok);

static char
scan_specch(char c);

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
                 BL_ERR_UNTERMINATED_COMMENT,
                 "%s %d:%d unterminated comment block.",
                 cnt->unit->name,
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
  tok->line    = cnt->line;
  tok->col     = cnt->col;
  tok->sym     = BL_SYM_IDENT;

  char *begin = cnt->c;

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

  BString *cstr = bl_tokens_create_cached_str(cnt->tokens);
  bo_string_appendn(cstr, begin, len);
  tok->value.as_string = bo_string_get(cstr);

  tok->len = len;
  cnt->col += len;
  return true;
}

char
scan_specch(char c)
{
  switch (c) {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    default:
      return c;
  }
}

bool
scan_string(context_t *cnt,
            bl_token_t *tok)
{
  if (*cnt->c != '\"') {
    return false;
  }

  tok->src_loc = cnt->c;
  tok->line    = cnt->line;
  tok->col     = cnt->col;
  tok->sym     = BL_SYM_STRING;

  /* eat " */
  cnt->c++;

  BString *cstr = bl_tokens_create_cached_str(cnt->tokens);
  char    c;
  int     len   = 0;

scan:
  while (true) {
    switch (*cnt->c) {
      case '\"': {
        cnt->c++;
        char *tmp_c = cnt->c;
        /* check multiline string */
        while (true) {
          if (*tmp_c == '\"') {
            /* skip " */
            cnt->c = tmp_c + 1;
            goto scan;
          } else if ((*tmp_c != ' ' && *tmp_c != '\n' && *tmp_c != '\t') || (*tmp_c == '\0')) {
            goto exit;
          }

          tmp_c++;
        }
      }
      case '\0': {
        scan_error(cnt,
                   BL_ERR_UNTERMINATED_STRING,
                   "%s %d:%d unterminated string.",
                   cnt->unit->name,
                   cnt->line,
                   cnt->col);
      }
      case '\\':
        /* special character */
        c = scan_specch(*(cnt->c + 1));
        cnt->c += 2;
        len++;
        break;
      default:
        c = *cnt->c;
        len++;
        cnt->c++;
    }
    bo_string_appendn(cstr, &c, 1);
  }
exit:
  tok->value.as_string = bo_string_get(cstr);
  tok->len             = len;
  cnt->col += len + 2;
  return true;
}

bool
scan_number(context_t *cnt,
            bl_token_t *tok)
{
  tok->src_loc         = cnt->c;
  tok->line            = cnt->line;
  tok->col             = cnt->col;
  tok->value.as_string = cnt->c;

  unsigned long n   = 0;
  int           len = 0;
  while (true) {
    if (*(cnt->c) == '.') {
      len++;
      cnt->c++;
      goto scan_double;
    }

    if (!is_number_c(*(cnt->c))) {
      break;
    }

    n = n * 10 + (*cnt->c) - '0';
    len++;
    cnt->c++;
  }

  if (len == 0)
    return false;

  tok->len          = len;
  cnt->col += len;
  tok->sym          = BL_SYM_NUM;
  tok->value.as_ull = n;
  return true;

scan_double:
  {
    unsigned long e = 1;

    while (true) {
      if (!is_number_c(*(cnt->c))) {
        break;
      }

      n = n * 10 + (*cnt->c) - '0';
      e *= 10;
      len++;
      cnt->c++;
    }

    /*
     * valid d. or .d -> minimal 2 characters
     */
    if (len < 2)
      return false;

    if (*(cnt->c) == 'f') {
      len++;
      cnt->c++;
      tok->sym            = BL_SYM_FLOAT;
      tok->value.as_float = n / (float) e;
    } else {
      tok->sym             = BL_SYM_DOUBLE;
      tok->value.as_double = n / (double) e;
    }

    tok->len = len;
    cnt->col += len;

    return true;
  }
}

void
scan(context_t *cnt)
{
  bl_token_t tok;
scan:
  tok.src_loc = cnt->c;
  tok.line    = cnt->line;
  tok.col     = cnt->col;

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
  size_t   len = 0;
  for (int i   = BL_SYM_IF; i < BL_SYM_NONE; i++) {
    len = strlen(bl_sym_strings[i]);
    if (strncmp(cnt->c, bl_sym_strings[i], len) == 0) {
      cnt->c += len;
      tok.sym = (bl_sym_e) i;

      /*
       * Two joined symbols will be parsed as identifier.
       */
      if (i >= BL_SYM_IF && i <= BL_SYM_IMPL && is_intend_c(*cnt->c)) {
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
        case BL_SYM_RBCOMMENT: {
          scan_error(cnt,
                     BL_ERR_INVALID_TOKEN,
                     "%s %d:%d unexpected token.",
                     cnt->unit->name,
                     cnt->line,
                     cnt->col);
        }
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
  scan_error(cnt,
             BL_ERR_INVALID_TOKEN,
             "%s %d:%d unexpected token.",
             cnt->unit->name,
             cnt->line,
             cnt->col);
push_token:
  bl_tokens_push(cnt->tokens, &tok);
  goto scan;
}

bl_error_e
bl_lexer_run(bl_builder_t *builder,
             bl_unit_t *unit)
{
  context_t
    cnt =
    {.builder = builder, .tokens = &unit->tokens, .unit = unit, .c = unit->src, .line = 1, .col = 1,};

  int error = 0;
  if ((error = setjmp(cnt.jmp_error)))
    return (bl_error_e) error;

  scan(&cnt);

  return BL_NO_ERR;
}

