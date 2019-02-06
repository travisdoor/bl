//************************************************************************************************
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
//************************************************************************************************

#include <string.h>
#include <setjmp.h>
#include "stages.h"
#include "common.h"

#define is_intend_c(c)                                                                             \
  (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || ((c) >= '0' && (c) <= '9') ||       \
   (c) == '_')

#define scan_error(cnt, code, format, ...)                                                         \
  {                                                                                                \
    builder_error((cnt)->builder, (format), ##__VA_ARGS__);                                        \
    longjmp((cnt)->jmp_error, code);                                                               \
  }

typedef struct context
{
  Builder *builder;
  Unit *   unit;
  Tokens * tokens;
  jmp_buf  jmp_error;
  char *   c;
  int32_t  line;
  int32_t  col;
} Context;

static void
scan(Context *cnt);

static bool
scan_comment(Context *cnt, const char *term);

static bool
scan_ident(Context *cnt, Token *tok);

static bool
scan_string(Context *cnt, Token *tok);

static bool
scan_char(Context *cnt, Token *tok);

static bool
scan_number(Context *cnt, Token *tok);

static inline int
c_to_number(char c, int32_t base);

static char
scan_specch(char c);

bool
scan_comment(Context *cnt, const char *term)
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
      scan_error(cnt, ERR_UNTERMINATED_COMMENT, "%s %d:%d unterminated comment block.",
                 cnt->unit->name, cnt->line, cnt->col);
    }
    if (*cnt->c == SYM_EOF || strncmp(cnt->c, term, len) == 0) {
      break;
    }
    cnt->c++;
  }

  /* skip terminator */
  cnt->c += len;
  return true;
}

bool
scan_ident(Context *cnt, Token *tok)
{
  tok->src.line = cnt->line;
  tok->src.col  = cnt->col;
  tok->sym      = SYM_IDENT;

  char *begin = cnt->c;

  int32_t len = 0;
  while (true) {
    if (!is_intend_c(*cnt->c)) {
      break;
    }

    len++;
    cnt->c++;
  }

  if (len == 0) return false;

  BString *cstr = builder_create_cached_str(cnt->builder);
  bo_string_appendn(cstr, begin, len);
  tok->value.str = bo_string_get(cstr);

  tok->src.len = len;
  cnt->col += len;
  return true;
}

char
scan_specch(char c)
{
  switch (c) {
  case 'n':
    return '\n';
  case 'r':
    return '\r';
  case 't':
    return '\t';
  case '0':
    return '\0';
  case '\"':
    return '\"';
  case '\'':
    return '\'';
  case '\\':
    return '\\';
  default:
    return c;
  }
}

bool
scan_string(Context *cnt, Token *tok)
{
  if (*cnt->c != '\"') {
    return false;
  }

  tok->src.line = cnt->line;
  tok->src.col  = cnt->col;
  tok->sym      = SYM_STRING;

  /* eat " */
  cnt->c++;

  BString *cstr = builder_create_cached_str(cnt->builder);
  char     c;
  int32_t  len = 0;

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
      scan_error(cnt, ERR_UNTERMINATED_STRING, "%s %d:%d unterminated string.", cnt->unit->name,
                 cnt->line, cnt->col);
    }
    case '\\':
      /* special character */
      c = scan_specch(*(cnt->c + 1));
      cnt->c += 2;
      len += 2;
      break;
    default:
      c = *cnt->c;
      len++;
      cnt->c++;
    }
    bo_string_appendn(cstr, &c, 1);
  }
exit:
  tok->value.str = bo_string_get(cstr);
  tok->src.len   = len;
  tok->src.col   = tok->src.col + 1;
  cnt->col += len + 2;
  return true;
}

bool
scan_char(Context *cnt, Token *tok)
{
  if (*cnt->c != '\'') return false;
  tok->src.line = cnt->line;
  tok->src.col  = cnt->col;
  tok->src.len  = 0;
  tok->sym      = SYM_CHAR;

  /* eat ' */
  cnt->c++;

  switch (*cnt->c) {
  case '\'': {
    scan_error(cnt, ERR_EMPTY, "%s %d:%d expected character in ''.", cnt->unit->name, cnt->line,
               cnt->col);
  }
  case '\0': {
    scan_error(cnt, ERR_UNTERMINATED_STRING, "%s %d:%d unterminated character.", cnt->unit->name,
               cnt->line, cnt->col);
  }
  case '\\':
    /* special character */
    tok->value.c = scan_specch(*(cnt->c + 1));
    cnt->c += 2;
    tok->src.len = 2;
    break;
  default:
    tok->value.c = *cnt->c;
    tok->src.len = 1;
    cnt->c++;
  }

  /* eat ' */
  if (*cnt->c != '\'') {
    scan_error(cnt, ERR_UNTERMINATED_STRING, "%s %d:%d unterminated character expected '.",
               cnt->unit->name, cnt->line, cnt->col);
  }
  cnt->c++;

  return true;
}

int
c_to_number(char c, int32_t base)
{
  switch (base) {
  case 16:
    if (c >= 'a' && c <= 'f') {
      return c - 'a' + 10;
    }

    if (c >= 'A' && c <= 'F') {
      return c - 'A' + 10;
    }
  case 10:
    if (c >= '2' && c <= '9') {
      return c - '0';
    }
  case 2:
    if (c == '0' || c == '1') {
      return c - '0';
    }
    break;
  default:
    bl_abort("invalid number base");
  }

  return -1;
}

bool
scan_number(Context *cnt, Token *tok)
{
  tok->src.line  = cnt->line;
  tok->src.col   = cnt->col;
  tok->value.str = cnt->c;

  unsigned long n    = 0;
  int32_t       len  = 0;
  int32_t       base = 10;
  int32_t       buf  = 0;

  if (strncmp(cnt->c, "0x", 2) == 0) {
    base = 16;
    cnt->c += 2;
    len += 2;
  } else if (strncmp(cnt->c, "0b", 2) == 0) {
    base = 2;
    cnt->c += 2;
    len += 2;
  }

  while (true) {
    if (*(cnt->c) == '.') {

      if (base != 10) {
        scan_error(cnt, ERR_INVALID_TOKEN, "%s %d:%d invalid suffix.", cnt->unit->name, cnt->line,
                   cnt->col + len);
      }

      len++;
      cnt->c++;
      goto scan_double;
    }

    buf = c_to_number(*(cnt->c), base);
    if (buf == -1) {
      break;
    }

    n = n * base + buf;
    len++;
    cnt->c++;
  }

  if (len == 0) return false;

  tok->src.len = len;
  cnt->col += len;
  tok->sym     = SYM_NUM;
  tok->value.u = n;
  return true;

scan_double : {
  unsigned long e = 1;

  while (true) {
    buf = c_to_number(*(cnt->c), 10);
    if (buf == -1) {
      break;
    }

    n = n * 10 + buf;
    e *= 10;
    len++;
    cnt->c++;
  }

  /*
   * valid d. or .d -> minimal 2 characters
   */
  if (len < 2) return false;

  if (*(cnt->c) == 'f') {
    len++;
    cnt->c++;
    tok->sym = SYM_FLOAT;
  } else {
    tok->sym = SYM_DOUBLE;
  }

  tok->value.d = n / (double)e;

  tok->src.len = len;
  cnt->col += len;

  return true;
}
}

void
scan(Context *cnt)
{
  Token tok;
scan:
  tok.src.line = cnt->line;
  tok.src.col  = cnt->col;

  /*
   * Ignored characters
   */
  switch (*cnt->c) {
  case '\0':
    tok.sym = SYM_EOF;
    tokens_push(cnt->tokens, &tok);
    return;
  case '\r':
    cnt->c++;
    goto scan;
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
  for (int32_t i = SYM_IF; i < SYM_NONE; ++i) {
    len = strlen(sym_strings[i]);
    if (strncmp(cnt->c, sym_strings[i], len) == 0) {
      cnt->c += len;
      tok.sym     = (Sym)i;
      tok.src.len = len;

      /*
       * Two joined symbols will be parsed as identifier.
       */
      if (i >= SYM_IF && i <= SYM_CONTINUE && is_intend_c(*cnt->c)) {
        /* roll back */
        cnt->c -= len;
        break;
      }

      switch (tok.sym) {
      case SYM_LCOMMENT:
        /* begin of line comment */
        scan_comment(cnt, "\n");
        goto scan;
      case SYM_LBCOMMENT:
        /* begin of block comment */
        scan_comment(cnt, sym_strings[SYM_RBCOMMENT]);
        goto scan;
      case SYM_RBCOMMENT: {
        scan_error(cnt, ERR_INVALID_TOKEN, "%s %d:%d unexpected token.", cnt->unit->name, cnt->line,
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
  if (scan_number(cnt, &tok)) goto push_token;
  if (scan_ident(cnt, &tok)) goto push_token;
  if (scan_string(cnt, &tok)) goto push_token;
  if (scan_char(cnt, &tok)) goto push_token;

  /* When symbol is unknown report error */
  scan_error(cnt, ERR_INVALID_TOKEN, "%s %d:%d unexpected token.", cnt->unit->name, cnt->line,
             cnt->col);
push_token:
  tok.src.unit = cnt->unit;
  tokens_push(cnt->tokens, &tok);
  goto scan;
}

void
lexer_run(Builder *builder, Unit *unit)
{
  Context cnt = {
      .builder = builder,
      .tokens  = &unit->tokens,
      .unit    = unit,
      .c       = unit->src,
      .line    = 1,
      .col     = 1,
  };

  int32_t error = 0;
  if ((error = setjmp(cnt.jmp_error))) return;

  scan(&cnt);

  builder->total_lines += cnt.line;
}
