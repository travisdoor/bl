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
#include <stdarg.h>
#include <bobject/containers/array.h>
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

typedef struct _cursor_t
{
  char *iter;
  int line;
  int col;
} cursor_t;

/* Lexer members */
bo_decl_members_begin(Lexer, BObject)
  BArray  *tokens;
  size_t   iter;
  size_t   marker;
  bl_notify_f notif;
bo_end();

/* Lexer constructor parameters */
bo_decl_params_begin(Lexer)
  bl_notify_f notif;
bo_end();

bo_impl_type(Lexer, BObject);

/* Lexer class init */
void
LexerKlass_init(LexerKlass *klass)
{
}

/* Lexer constructor */
void
Lexer_ctor(Lexer *self, LexerParams *p)
{
  self->tokens = bo_array_new(sizeof(bl_token_t));
  self->notif = p->notif;
}

/* Lexer destructor */
void
Lexer_dtor(Lexer *self)
{
  bo_unref(self->tokens);
}

/* Lexer copy constructor */
bo_copy_result
Lexer_copy(Lexer *self, Lexer *other)
{
  return BO_NO_COPY;
}

static int
scan_string(Lexer  *self,
            char    term,
            cursor_t *cur)
{
  bl_token_t tok;
  cur->iter++;
  cur->col++;

  bl_token_init(&tok, BL_SYM_STRING, cur->line, cur->col, 0, cur->iter);
  tok.content.as_string = cur->iter;

  while (*cur->iter != term) {
    if (*cur->iter == '\0')
      return 0;
    cur->iter++;
    cur->col++;
    tok.len++;
  }

  bo_array_push_back(self->tokens, tok);
  return 1;
}

static int
ignore_till(Lexer *self,
            char   term,
            cursor_t *cur)
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
scan_ident(Lexer *self,
           cursor_t *cur)
{
  bl_token_t tok;
  size_t len = 2;

  if (strncmp(cur->iter, "if", len) == 0) {
    bl_token_init(&tok, BL_SYM_IF, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 4;
  if (strncmp(cur->iter, "else", len) == 0) {
    bl_token_init(&tok, BL_SYM_ELSE, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 5;
  if (strncmp(cur->iter, "return", len) == 0) {
    bl_token_init(&tok, BL_SYM_RETURN, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 6;
  if (strncmp(cur->iter, "extern", len) == 0) {
    bl_token_init(&tok, BL_SYM_EXTERN, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  len = 9;
  if (strncmp(cur->iter, "namespace", len) == 0) {
    bl_token_init(&tok, BL_SYM_NAMESPACE, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    return 1;
  }

  bl_token_init(&tok, BL_SYM_IDENT, cur->line, cur->col, 0, cur->iter);
  tok.content.as_string = cur->iter;
  while (is_intend_c(*cur->iter)) {
    (cur->iter)++;
    tok.len++;
  }


  bo_array_push_back(self->tokens, tok);
  cur->iter--;
  cur->col += len;
  return 1;
}

static int
scan_number(Lexer *self,
            cursor_t *cur)
{
  if (!is_number_c(*cur->iter))
    return 0;

  int n = 0;
  bl_token_t tok;
  bl_token_init(&tok, BL_SYM_NUM, cur->line, cur->col, 0, cur->iter);

  while (true) {
    n = n * 10 + (*cur->iter) - '0';
    cur->col++;
    tok.len++;
    if (is_number_c(*(cur->iter+1))) {
      cur->iter++;
    } else
      break;
  }

  tok.content.as_int = n;
  bo_array_push_back(self->tokens, tok);
  return 1;
}

/* public */
Lexer *
bl_lexer_new(bl_notify_f notif)
{
  LexerParams p = {
    .notif = notif
  };
  
  return bo_new(Lexer, &p);
}

/* public */
bool 
bl_lexer_scan(Lexer *self,
              BString *src)
{
  bl_token_t tok = {
    .sym  = BL_SYM_EOF,
    .line = 0,
    .col  = 0
  };

  cursor_t cur = {
    .iter = (char *)bo_string_get(src),
    .line = 0,
    .col = 0
  };

  for (;*cur.iter != '\0'; cur.iter++) {
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
        bl_token_init(&tok ,BL_SYM_LBLOCK, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case '}':
        bl_token_init(&tok ,BL_SYM_RBLOCK, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case '[':
        bl_token_init(&tok ,BL_SYM_LBRACKET, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case ']':
        bl_token_init(&tok ,BL_SYM_RBRACKET, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case '(':
        bl_token_init(&tok ,BL_SYM_LPAREN, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case ')':
        bl_token_init(&tok ,BL_SYM_RPAREN, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case ',':
        bl_token_init(&tok ,BL_SYM_COMMA, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case '=':
        bl_token_init(&tok ,BL_SYM_ASIGN, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case ';':
        bl_token_init(&tok ,BL_SYM_SEMICOLON, cur.line, cur.col, 1, cur.iter);
        bo_array_push_back(self->tokens, tok);
        cur.col++;
        continue;
      case '"':
        scan_string(self, '"', &cur);
        continue;
      case '/':
        switch (*(cur.iter + 1)) {
        case '/':
          ignore_till(self, '\n', &cur);
          continue;
        default:
          bl_token_init(&tok ,BL_SYM_SLASH, cur.line, cur.col, 1, cur.iter);
          bo_array_push_back(self->tokens, tok);
          cur.col++;
          continue;
        }
      default:
        if (scan_number(self, &cur))
          continue;

        if (scan_ident(self, &cur))
          continue;

        self->notif("", cur.iter, cur.line, cur.col, "unknown character"); 
        return false;
    }
  }
  tok.sym = BL_SYM_EOF;
  bo_array_push_back(self->tokens, tok);
  return true;
}

bl_token_t *
bl_lexer_peek(Lexer *self)
{
  return bl_lexer_peek_nth(self, 1);
}

bl_token_t *
bl_lexer_peek_2nd(Lexer *self)
{
  return bl_lexer_peek_nth(self, 2);
}

bl_token_t *
bl_lexer_peek_nth(Lexer *self,
                   size_t  n)
{
  const size_t i = self->iter + n - 1;
  if (i < bo_array_size(self->tokens))
    return &bo_array_at(self->tokens, i, bl_token_t);

  return NULL;
}

bl_token_t *
bl_lexer_consume(Lexer *self)
{
  if (self->iter < bo_array_size(self->tokens))
    return &bo_array_at(self->tokens, self->iter++, bl_token_t);

  return NULL;
}

bl_token_t *
bl_lexer_consume_if(Lexer  *self,
                     bl_sym_e sym)
{
  bl_token_t *tok;
  if (self->iter < bo_array_size(self->tokens)) {
    tok = &bo_array_at(self->tokens, self->iter, bl_token_t);
    if (tok->sym == sym) {
      self->iter++;
      return tok;
    }
  }

  return NULL;

}

bool
bl_lexer_current_is(Lexer  *self,
                     bl_sym_e sym)
{
  return (&bo_array_at(self->tokens, self->iter, bl_token_t))->sym == sym;
}

bool
bl_lexer_next_is(Lexer  *self,
                  bl_sym_e sym)
{
  return (&bo_array_at(self->tokens, self->iter+1, bl_token_t))->sym == sym;
}

bool
bl_lexer_current_is_not(Lexer  *self,
                         bl_sym_e sym)
{
  return (&bo_array_at(self->tokens, self->iter, bl_token_t))->sym != sym;
}

bool
bl_lexer_next_is_not(Lexer  *self,
                      bl_sym_e sym)
{
  return (&bo_array_at(self->tokens, self->iter+1, bl_token_t))->sym != sym;
}

bool
bl_lexer_is_seq(Lexer *self,
                 int     cnt,
                 ...)
{
  bool ret     = true;
  size_t c     = bo_array_size(self->tokens);
  bl_sym_e sym = BL_SYM_EOF;
  cnt         += self->iter;

  va_list valist;
  va_start(valist, cnt);

  for (size_t i = self->iter; i < cnt && i < c; i++) {
    sym = va_arg(valist, bl_sym_e);
    if ((&bo_array_at(self->tokens, i, bl_token_t))->sym != sym) {
      ret = false;
      break;
    }
  }

  va_end(valist);
  return ret;
}

void
bl_lexer_set_marker(Lexer *self)
{
  self->marker = self->iter;
}

void
bl_lexer_back_to_marker(Lexer *self)
{
  self->iter = self->marker;
}

void
bl_lexer_resert_iter(Lexer *self)
{
  self->iter = 0;
}

