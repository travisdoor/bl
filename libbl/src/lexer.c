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
#include "bl/lexer.h"
#include "bl/bldebug.h"
#include "bl/pipeline/stage.h"
#include "unit_impl.h"

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

static bool
run(Lexer *self,
    Unit  *unit);

static bool
scan_string(Lexer  *self,
            Unit   *unit,
            char    term,
            cursor_t *cur);

static bool 
ignore_till(Lexer *self,
            Unit   *unit,
            char   term,
            cursor_t *cur);

static bool 
scan_ident(Lexer *self,
           Unit   *unit,
           cursor_t *cur);

static bool 
scan_number(Lexer *self,
            Unit   *unit,
            cursor_t *cur);

/* Lexer members */
bo_decl_members_begin(Lexer, Stage)
bo_end();

/* Lexer constructor parameters */
bo_decl_params_with_base_begin(Lexer, Stage)
bo_end();

bo_impl_type(Lexer, Stage);

/* Lexer class init */
void
LexerKlass_init(LexerKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
}

/* Lexer constructor */
void
Lexer_ctor(Lexer *self, LexerParams *p)
{
  bo_parent_ctor(Stage, p);
}

/* Lexer destructor */
void
Lexer_dtor(Lexer *self)
{
}

/* Lexer copy constructor */
bo_copy_result
Lexer_copy(Lexer *self, Lexer *other)
{
  return BO_NO_COPY;
}

bool
run(Lexer *self,
    Unit  *unit)
{
  bo_unref(unit->tokens);
  unit->tokens = bl_tokens_new();

  bl_token_t tok = {
    .sym  = BL_SYM_EOF,
    .line = 0,
    .col  = 0
  };

  cursor_t cur = {
    .iter = unit->src,
    .line = 1,
    .col = 1
  };

  for (;*cur.iter != '\0'; cur.iter++) {
    switch (*cur.iter) {
      /* TODO: windows line endings */
      case '\r':
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
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '}':
        bl_token_init(&tok ,BL_SYM_RBLOCK, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '[':
        bl_token_init(&tok ,BL_SYM_LBRACKET, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case ']':
        bl_token_init(&tok ,BL_SYM_RBRACKET, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '(':
        bl_token_init(&tok ,BL_SYM_LPAREN, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case ')':
        bl_token_init(&tok ,BL_SYM_RPAREN, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case ',':
        bl_token_init(&tok ,BL_SYM_COMMA, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '=':
        bl_token_init(&tok ,BL_SYM_ASIGN, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '+':
        bl_token_init(&tok ,BL_SYM_PLUS, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '-':
        bl_token_init(&tok ,BL_SYM_MINUS, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case ';':
        bl_token_init(&tok ,BL_SYM_SEMICOLON, cur.line, cur.col, 1, cur.iter);
        bl_tokens_push(unit->tokens, &tok);
        cur.col++;
        continue;
      case '"':
        scan_string(self, unit, '"', &cur);
        continue;
      case '/':
        switch (*(cur.iter + 1)) {
        case '/':
          ignore_till(self, unit, '\n', &cur);
          continue;
        default:
          bl_token_init(&tok ,BL_SYM_SLASH, cur.line, cur.col, 1, cur.iter);
          bl_tokens_push(unit->tokens, &tok);
          cur.col++;
          continue;
        }
      default:
        if (scan_number(self, unit, &cur))
          continue;

        if (scan_ident(self, unit, &cur))
          continue;

        /* notify error */
        bl_actor_error((Actor *)unit, "invalid character %c %i:%i", 
            cur.iter[0], cur.line, cur.col); 
        return false;
    }
  }
  tok.sym = BL_SYM_EOF;
  bl_tokens_push(unit->tokens, &tok);
//  bl_log("* lexing done\n");
  return true;
}

bool
scan_string(Lexer  *self,
            Unit   *unit,
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
      return false;
    cur->iter++;
    cur->col++;
    tok.len++;
  }

  bl_tokens_push(unit->tokens, &tok);
  return true;
}

bool
ignore_till(Lexer *self,
            Unit   *unit,
            char   term,
            cursor_t *cur)
{
  while (*cur->iter != term) {
    if (*cur->iter == '\n') {
      cur->col = 1;
      cur->line++;
    }
    if (*cur->iter == '\0')
      return false;
    cur->iter++;
    cur->col++;
  }

  if (term == '\n') {
    cur->col = 1;
    cur->line++;
  }
  return true;
}

bool
scan_ident(Lexer *self,
           Unit   *unit,
           cursor_t *cur)
{
  bl_token_t tok;
  size_t len = 2;

  if (strncmp(cur->iter, "if", len) == 0) {
    bl_token_init(&tok, BL_SYM_IF, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  len = 4;
  if (strncmp(cur->iter, "else", len) == 0) {
    bl_token_init(&tok, BL_SYM_ELSE, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  if (strncmp(cur->iter, "true", len) == 0) {
    bl_token_init(&tok, BL_SYM_TRUE, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  len = 5;
  if (strncmp(cur->iter, "return", len) == 0) {
    bl_token_init(&tok, BL_SYM_RETURN, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  if (strncmp(cur->iter, "false", len) == 0) {
    bl_token_init(&tok, BL_SYM_FALSE, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  if (strncmp(cur->iter, "class", len) == 0) {
    bl_token_init(&tok, BL_SYM_CLASS, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  len = 6;
  if (strncmp(cur->iter, "extern", len) == 0) {
    bl_token_init(&tok, BL_SYM_EXTERN, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  if (strncmp(cur->iter, "struct", len) == 0) {
    bl_token_init(&tok, BL_SYM_STRUCT, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  len = 9;
  if (strncmp(cur->iter, "namespace", len) == 0) {
    bl_token_init(&tok, BL_SYM_NAMESPACE, cur->line, cur->col, len, cur->iter);
    cur->iter += len;
    cur->col += len;
    bl_tokens_push(unit->tokens, &tok);
    return true;
  }

  bl_token_init(&tok, BL_SYM_IDENT, cur->line, cur->col, 0, cur->iter);
  tok.content.as_string = cur->iter;
  while (is_intend_c(*cur->iter)) {
    (cur->iter)++;
    tok.len++;
  }

  if (tok.len == 0)
    return false;

  bl_tokens_push(unit->tokens, &tok);
  cur->iter--;
  cur->col += tok.len;
  return true;
}

bool
scan_number(Lexer *self,
            Unit   *unit,
            cursor_t *cur)
{
  if (!is_number_c(*cur->iter))
    return false;

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
  bl_tokens_push(unit->tokens, &tok);
  return true;
}

/* public */
Lexer *
bl_lexer_new(bl_compile_group_e group)
{
  LexerParams p = {
    .base.group = group
  };

  return bo_new(Lexer, &p);
}

