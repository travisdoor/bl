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
    bl_tokens_push(tokens, &tok); \
  }

typedef struct _cursor {
  char *iter;
  int line;
  int col;
} cursor;

static void
init_cursor(cursor *cur,
            char *begin)
{
  cur->col  = 1;
  cur->line = 1;
  cur->iter = begin;
}

static int
scan_string(cursor *cur,
            char     term,
            Tokens  *tokens)
{
  bl_token_t tok;
  cur->iter++;
  cur->col++;

  tok.line = cur->line;
  tok.col = cur->col;
  char *str = cur->iter;

  size_t len = 0;
  while (*cur->iter != term) {
    if (*cur->iter == '\0')
      return 0;
    cur->iter++;
    cur->col++;
    len++;
  }

  str = strndup(str, len);
  tok.content.as_string = str;
  tok.sym = BL_SYM_STRING;

  bl_tokens_cache_str(tokens, str);
  bl_tokens_push(tokens, &tok);
  return 1;
}

static int
ignore_till(cursor *cur,
            char     term)
{
  while (*cur->iter != term) {
    if (*cur->iter == '\n') {
      cur->col = 1;
      cur->line++;
    }
    if (*cur->iter == '\0')
      return 0;
    cur->iter++;
    cur->col++;
  }

  if (term == '\n') {
    cur->col = 1;
    cur->line++;
  }
  return 1;
}

static int
scan_ident(cursor *cur,
           Tokens  *tokens)
{
  bl_token_t tok;
  size_t len = 2;

  if (strncmp(cur->iter, "if", len) == 0) {
    token(BL_SYM_IF, *cur);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 4;
  if (strncmp(cur->iter, "else", len) == 0) {
    token(BL_SYM_ELSE, *cur);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 5;
  if (strncmp(cur->iter, "return", len) == 0) {
    token(BL_SYM_RET, *cur);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 6;
  if (strncmp(cur->iter, "extern", len) == 0) {
    token(BL_SYM_EXTERN, *cur);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 9;
  if (strncmp(cur->iter, "namespace", len) == 0) {
    token(BL_SYM_NAMESPACE, *cur);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  char *str = cur->iter;
  len = 0;
  while (is_intend_c(*cur->iter)) {
    (cur->iter)++;
    len++;
  }

  str = strndup(str, len);
  tok.content.as_string = str;
  tok.sym = BL_SYM_IDENT;

  bl_tokens_cache_str(tokens, str);
  bl_tokens_push(tokens, &tok);

  cur->iter--;
  cur->col += len;
  return 1;
}

static int
scan_number(cursor *cur,
            Tokens  *tokens)
{
  if (!is_number_c(*cur->iter))
    return 0;

  int n = 0;
  bl_token_t tok;
  tok.col  = cur->col;
  tok.line = cur->line;

  while (true) {
    n = n * 10 + (*cur->iter) - '0';
    cur->col++;
    if (is_number_c(*(cur->iter+1))) {
      cur->iter++;
    } else
      break;
  }

  tok.sym = BL_SYM_NUM;
  tok.content.as_int = n;
  
  bl_tokens_push(tokens, &tok);
  return 1;
}

/* public */
Tokens *
bl_lexer_scan(Unit *unit)
{
  Tokens *tokens = bl_tokens_new(unit->src);

  bl_token_t tok = {
    .sym  = BL_SYM_EOF,
    .line = 0,
    .col  = 0
  };

  cursor cur;
  for (init_cursor(&cur, (char *) bo_string_get(unit->src));
       *cur.iter != '\0'; cur.iter++) {
    switch (*cur.iter) {
      case '\n':
        cur.line++;
        cur.col = 1;
        continue;
      case ' ':
      case '\t':
        cur.col++;
        continue;
      case '{':
        token(BL_SYM_LBLOCK, cur);
        cur.col++;
        continue;
      case '}':
        token(BL_SYM_RBLOCK, cur);
        cur.col++;
        continue;
      case '[':
        token(BL_SYM_LBRACKET, cur);
        cur.col++;
        continue;
      case ']':
        token(BL_SYM_RBRACKET, cur);
        cur.col++;
        continue;
      case '(':
        token(BL_SYM_LPAREN, cur);
        cur.col++;
        continue;
      case ')':
        token(BL_SYM_RPAREN, cur);
        cur.col++;
        continue;
      case ',':
        token(BL_SYM_COMMA, cur);
        cur.col++;
        continue;
      case '=':
        token(BL_SYM_ASIGN, cur);
        cur.col++;
        continue;
      case ';':
        token(BL_SYM_SEMICOLON, cur);
        cur.col++;
        continue;
      case '"':
        scan_string(&cur, '"', tokens);
        continue;
      case '/':
        switch (*(cur.iter + 1)) {
        case '/':
          ignore_till(&cur, '\n');
          continue;
        default:
          token(BL_SYM_SLASH, cur);
          cur.col++;
          continue;
        }
      default:
        if (scan_number(&cur, tokens))
          continue;

        if (scan_ident(&cur, tokens))
          continue;

        bl_error_at(bo_string_get(unit->filepath), cur.line, cur.col, "unknown character");
    }
  }
  tok.sym = BL_SYM_EOF;
  bl_tokens_push(tokens, &tok);
  return tokens;
}

