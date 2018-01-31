//*****************************************************************************
// bl
//
// File:   lexer.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include <stdio.h>
#include <string.h>
#include "lexer.h"
#include "bldebug.h"

/* class Lexer */
#define is_intend_c(c) \
  (((c) >= 'a' && (c) <= 'z') || \
   ((c) >= 'A' && (c) <= 'Z')|| \
   ((c) >= '0' && (c) <= '9')|| \
   (c) == '_' || (c) == '-')

#define is_number_c(c) \
  ((c) >= '0' && (c) <= '9')

#define token(s, c) \
  { \
    tok.sym  = (s); \
    tok.line = ((c).line); \
    tok.col  = ((c).col); \
    bl_tokens_push(unit->tokens, &tok); \
  }

typedef struct _cursor {
  char *iter;
  int line;
  int col;
} cursor;

static void
init_cursor(cursor *cnt,
            char *begin)
{
  cnt->col  = 1;
  cnt->line = 1;
  cnt->iter = begin; 
}

static int
scan_string(cursor *cnt,
            char     term,
            Tokens  *out)
{
  bl_token_t tok;
  cnt->iter++;
  cnt->col++;

  tok.line = cnt->line;
  tok.col = cnt->col;
  tok.content.as_string = cnt->iter;

  size_t len = 0;
  while (*cnt->iter != term) {
    if (*cnt->iter == '\0')
      return 0;
    cnt->iter++;
    cnt->col++;
    len++;
  }

  tok.len = len;
  tok.sym = BL_SYM_STRING;

  bl_tokens_push(out, &tok);
  return 1;
}

static int
ignore_till(cursor *cnt,
            char     term)
{
  while (*cnt->iter != term) {
    if (*cnt->iter == '\n') {
      cnt->col = 1;
      cnt->line++;
    }
    if (*cnt->iter == '\0')
      return 0;
    cnt->iter++;
    cnt->col++;
  }

  if (term == '\n') {
    cnt->col = 1;
    cnt->line++;
  }
  return 1;
}

static int
scan_ident(cursor *cnt,
           Tokens  *out)
{
  bl_token_t tok;
  size_t len = 2;
  tok.content.as_string = cnt->iter;

  if (strncmp(cnt->iter, "if", len) == 0) {
    token(BL_SYM_IF, *cnt);
    cnt->iter += len;
    cnt->col += len;
    return 1;
  }

  len = 4;
  if (strncmp(cnt->iter, "else", len) == 0) {
    token(BL_SYM_ELSE, *cnt);
    cnt->iter += len;
    cnt->col += len;
    return 1;
  }

  len = 5;
  if (strncmp(cnt->iter, "return", len) == 0) {
    token(BL_SYM_RET, *cnt);
    cnt->iter += len;
    cnt->col += len;
    return 1;
  }

  len = 6;
  if (strncmp(cnt->iter, "extern", len) == 0) {
    token(BL_SYM_EXTERN, *cnt);
    cnt->iter += len;
    cnt->col += len;
    return 1;
  }

  len = 0;
  while (is_intend_c(*cnt->iter)) {
    (cnt->iter)++;
    len++;
  }

  tok.len = len;
  token(BL_SYM_IDENT, *cnt);

  cnt->iter--;
  cnt->col += len;
  return 1;
}

static int
scan_number(cursor *cnt,
            Tokens  *out)
{
  if (!is_number_c(*cnt->iter))
    return 0;

  int n = 0;
  bl_token_t tok;
  tok.col  = cnt->col;
  tok.line = cnt->line;

  while (true) {
    n = n * 10 + (*cnt->iter) - '0';
    cnt->col++;
    if (is_number_c(*(cnt->iter+1))) {
      cnt->iter++;
    } else
      break;
  }

  tok.sym = BL_SYM_NUM;
  tok.content.as_int = n;
  
  bl_tokens_push(out, &tok);
  return 1;
}

/* public */
bool
bl_lexer_scan(Unit *unit)
{
  unit->tokens = bl_tokens_new(in);

  bl_token_t tok = {
    .sym  = BL_SYM_EOF,
    .len  = 0,
    .line = 0,
    .col  = 0
  };

  cursor cnt;
  for (init_cursor(&cnt, (char *) bo_string_get(unit->src));
       *cnt.iter != '\0'; cnt.iter++) {
    switch (*cnt.iter) {
      case '\n':
        cnt.line++;
        cnt.col = 1;
        continue;
      case ' ':
      case '\t':
        cnt.col++;
        continue;
      case '{':
        token(BL_SYM_LBLOCK, cnt);
        cnt.col++;
        continue;
      case '}':
        token(BL_SYM_RBLOCK, cnt);
        cnt.col++;
        continue;
      case '[':
        token(BL_SYM_LBRACKET, cnt);
        cnt.col++;
        continue;
      case ']':
        token(BL_SYM_RBRACKET, cnt);
        cnt.col++;
        continue;
      case '(':
        token(BL_SYM_LPAREN, cnt);
        cnt.col++;
        continue;
      case ')':
        token(BL_SYM_RPAREN, cnt);
        cnt.col++;
        continue;
      case ',':
        token(BL_SYM_COMMA, cnt);
        cnt.col++;
        continue;
      case '=':
        token(BL_SYM_ASIGN, cnt);
        cnt.col++;
        continue;
      case ';':
        token(BL_SYM_SEMICOLON, cnt);
        cnt.col++;
        continue;
      case '"':
        scan_string(&cnt, '"', unit);
        continue;
      case '/':
        switch (*(cnt.iter + 1)) {
        case '/':
          ignore_till(&cnt, '\n');
          continue;
        default:
          token(BL_SYM_SLASH, cnt);
          cnt.col++;
          continue;
        }
      default:
        if (scan_number(&cnt, unit))
          continue;

        if (scan_ident(&cnt, unit))
          continue;

        bl_parse_error("unknown character\n");
    }
  }
  tok.sym = BL_SYM_EOF;
  bl_tokens_push(unit->tokens, &tok);
  return true;
}

