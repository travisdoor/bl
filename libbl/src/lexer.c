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
#include "bl/lexer.h"
#include "bl/unit.h"
#include "bl/bldebug.h"

#define is_intend_c(c) \
  (((c) >= 'a' && (c) <= 'z') || \
   ((c) >= 'A' && (c) <= 'Z')|| \
   ((c) >= '0' && (c) <= '9')|| \
   (c) == '_' || (c) == '-')

#define is_number_c(c) \
  ((c) >= '0' && (c) <= '9')

#define scan_error(self, format, ...) \
  { \
    bl_actor_error((Actor *)(self)->unit, (format), ##__VA_ARGS__); \
    longjmp((self)->jmp_error, 1); \
  }

/* class Lexer */
static bool
run(Lexer *self,
    Unit *unit);

static void
scan(Lexer *self);

static bool
scan_comment(Lexer *self,
             const char *term);

static bool
scan_ident(Lexer *self,
           bl_token_t *tok);

static bool
scan_string(Lexer *self,
            bl_token_t *tok);

static bool
scan_number(Lexer *self,
            bl_token_t *tok);

static char
scan_specch(char c);

static void
reset(Lexer *self,
      Unit *unit);

/* class Lexer constructor params */
bo_decl_params_with_base_begin(Lexer, Stage)
  /* constructor params */
bo_end();

/* class Lexer object members */
bo_decl_members_begin(Lexer, Stage)
  /* members */
  Unit *unit;
  Tokens *tokens;
  jmp_buf jmp_error;
  char *c;
  int line;
  int col;
bo_end();

bo_impl_type(Lexer, Stage);

void
LexerKlass_init(LexerKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run =
    (bool (*)(Stage *,
              Actor *)) run;
}

void
Lexer_ctor(Lexer *self,
           LexerParams *p)
{
  /* constructor */
  /* initialize parent */
  bo_parent_ctor(Stage, p);

  /* initialize self */
}

void
Lexer_dtor(Lexer *self)
{
}

bo_copy_result
Lexer_copy(Lexer *self,
           Lexer *other)
{
  return BO_NO_COPY;
}

/* class Lexer end */

bool
scan_comment(Lexer *self,
             const char *term)
{
  const size_t len = strlen(term);
  while (true) {
    if (*self->c == '\n') {
      self->line++;
      self->col = 1;
    } else if (*self->c == '\0' && strcmp(term, "\n") != 0) {
      /*
       * Unterminated comment
       */
      scan_error(self,
                 "%s %d:%d unterminated comment block.",
                 bl_unit_get_name(self->unit),
                 self->line,
                 self->col);
    }
    if (strncmp(self->c, term, len) == 0) {
      break;
    }
    self->c++;
  }

  /* skip terminator */
  self->c += len;
  return true;
}

bool
scan_ident(Lexer *self,
           bl_token_t *tok)
{
  tok->src_loc = self->c;
  tok->line = self->line;
  tok->col = self->col;
  tok->sym = BL_SYM_IDENT;

  char *begin = self->c;

  int len = 0;
  while (true) {
    if (!is_intend_c(*self->c)) {
      break;
    }

    len++;
    self->c++;
  }

  if (len == 0)
    return false;

  BString *cstr = bl_tokens_create_cached_str(self->tokens);
  bo_string_appendn(cstr, begin, len);
  tok->content.as_string = bo_string_get(cstr);

  tok->len = len;
  self->col += len;
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
scan_string(Lexer *self,
            bl_token_t *tok)
{
  if (*self->c != '\"') {
    return false;
  }

  tok->src_loc = self->c;
  tok->line = self->line;
  tok->col = self->col;
  tok->sym = BL_SYM_STRING;

  /* eat " */
  self->c++;

  BString *cstr = bl_tokens_create_cached_str(self->tokens);
  char c;
  int len = 0;

scan:
  while (true) {
    switch (*self->c) {
      case '\"': {
        self->c++;
        char *tmp_c = self->c;
        /* check multiline string */
        while (true) {
          if (*tmp_c == '\"') {
            /* skip " */
            self->c = tmp_c + 1;
            goto scan;
          } else if ((*tmp_c != ' ' && *tmp_c != '\n' && *tmp_c != '\t') || (*tmp_c == '\0')) {
            goto exit;
          }

          tmp_c++;
        }
      }
      case '\0': {
        scan_error(self,
                   "%s %d:%d unterminated string.",
                   bl_unit_get_name(self->unit),
                   self->line,
                   self->col);
      }
      case '\\':
        /* special character */
        c = scan_specch(*(self->c + 1));
        self->c += 2;
        len++;
        break;
      default:
        c = *self->c;
        len++;
        self->c++;
    }
    bo_string_appendn(cstr, &c, 1);
  }
exit:
  tok->content.as_string = bo_string_get(cstr);
  tok->len = len;
  self->col += len + 2;
  return true;
}

bool
scan_number(Lexer *self,
            bl_token_t *tok)
{
  tok->src_loc = self->c;
  tok->line = self->line;
  tok->col = self->col;
  tok->content.as_string = self->c;

  unsigned long n = 0;
  int len = 0;
  while (true) {
    if (*(self->c) == '.') {
      len++;
      self->c++;
      goto scan_double;
    }

    if (!is_number_c(*(self->c))) {
      break;
    }

    n = n * 10 + (*self->c) - '0';
    len++;
    self->c++;
  }

  if (len == 0)
    return false;

  tok->len = len;
  self->col += len;
  tok->sym = BL_SYM_NUM;
  tok->content.as_ull = n;
  return true;

scan_double: {
  unsigned long e = 1;

  while (true) {
    if (!is_number_c(*(self->c))) {
      break;
    }

    n = n * 10 + (*self->c) - '0';
    e *= 10;
    len++;
    self->c++;
  }

  /*
   * valid d. or .d -> minimal 2 characters
   */
  if (len < 2)
    return false;

  if (*(self->c) == 'f') {
    len++;
    self->c++;
    tok->sym = BL_SYM_FLOAT;
    tok->content.as_float = n / (float)e;
  } else {
    tok->sym = BL_SYM_DOUBLE;
    tok->content.as_double = n / (double)e;
  }

  tok->len = len;
  self->col += len;

  return true;
}
}

void
scan(Lexer *self)
{
  bl_token_t tok;
scan:
  tok.src_loc = self->c;
  tok.line = self->line;
  tok.col = self->col;

  /*
   * Ignored characters
   */
  switch (*self->c) {
    case '\0':
      tok.sym = BL_SYM_EOF;
      bl_tokens_push(self->tokens, &tok);
      return;
    case '\r':
    case '\n':
      self->line++;
      self->col = 1;
      self->c++;
      goto scan;
    case '\t':
      /* TODO: can be set by user */
      self->col += 2;
      self->c++;
      goto scan;
    case ' ':
      self->col++;
      self->c++;
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
    if (strncmp(self->c, bl_sym_strings[i], len) == 0) {
      self->c += len;
      tok.sym = (bl_sym_e) i;

      /*
       * Two joined symbols will be parsed as identifier.
       */
      if (i >= BL_SYM_IF && i <= BL_SYM_IMPL && is_intend_c(*self->c)) {
        /* roll back */
        self->c -= len;
        break;
      }

      switch (tok.sym) {
        case BL_SYM_LCOMMENT:
          /* begin of line comment */
          scan_comment(self, "\n");
          goto scan;
        case BL_SYM_LBCOMMENT:
          /* begin of block comment */
          scan_comment(self, bl_sym_strings[BL_SYM_RBCOMMENT]);
          goto scan;
        case BL_SYM_RBCOMMENT: {
          scan_error(self,
                     "%s %d:%d unexpected token.",
                     bl_unit_get_name(self->unit),
                     self->line,
                     self->col);
        }
        default:
          self->col += len;
          goto push_token;
      }
    }
  }

  /*
   * Scan special tokens.
   */
  if (scan_number(self, &tok))
    goto push_token;

  if (scan_ident(self, &tok))
    goto push_token;

  if (scan_string(self, &tok))
    goto push_token;

  /* When symbol is unknown report error */
  scan_error(self,
             "%s %d:%d unexpected token.",
             bl_unit_get_name(self->unit),
             self->line,
             self->col);
push_token:
  bl_tokens_push(self->tokens, &tok);
  goto scan;
}

void
reset(Lexer *self,
      Unit *unit)
{
  char *src = (char *) bl_unit_get_src(unit);
  self->col = 1;
  self->line = 1;
  self->c = src;
  self->unit = unit;

  if (src == NULL) {
    scan_error(self, "No source loaded for unit "
      BL_YELLOW("'%s'")
      ", use builder flag "
      BL_YELLOW("BL_BUILDER_LOAD_FROM_FILE")
      " or create unit from loaded source.", bl_unit_get_name(unit));
  }

  Tokens *tokens = bl_tokens_new();
  /* Unit owns tokens */
  bl_unit_set_tokens(unit, tokens);
  self->tokens = tokens;
}

bool
run(Lexer *self,
    Unit *unit)
{
  if (setjmp(self->jmp_error))
    return false;

  bl_log(BL_GREEN("processing unit: %s"), bl_unit_get_name(unit));

  reset(self, unit);
  scan(self);

  return true;
}

/* public */
Lexer *
bl_lexer_new(bl_compile_group_e group)
{
  LexerParams p = {.base.group = group};

  return bo_new(Lexer, &p);
}
