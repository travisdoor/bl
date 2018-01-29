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
  ((c) >= 'a' && (c) <= 'z' || \
   (c) >= 'A' && (c) <= 'Z' || \
   (c) >= '0' && (c) <= '9' || \
   (c) == '_' || (c) == '-')

#define is_number_c(c) \
  ((c) >= '0' && (c) <= '9')

static int
scan_string(char  **iter,
            char    term,
            Tokens *out)
{
  bl_token_t tok;
  (*iter)++;
  tok.content.as_string = *iter;

  size_t len = 0;
  while (**iter != term) {
    if (**iter == '\0')
      return 0;
    (*iter)++;
    len++;
  }

  tok.len = len;
  tok.sym = BL_SYM_STRING;

  bl_tokens_push(out, &tok);
  return 1;
}

static int
ignore_till(char **iter,
            char   term)
{
  while (**iter != term) {
    if (**iter == '\0')
      return 0;
    (*iter)++;
  }
  return 1;
}

static int
scan_ident(char  **iter,
           Tokens *out)
{
  bl_token_t tok;
  size_t len = 2;
  tok.content.as_string = *iter;

  if (strncmp(*iter, "if", len) == 0) {
    *iter += len;
    tok.sym = BL_SYM_IF;
    bl_tokens_push(out, &tok);
    return 1;
  }

  len = 4;
  if (strncmp(*iter, "else", len) == 0) {
    *iter += len;
    tok.sym = BL_SYM_IF;
    bl_tokens_push(out, &tok);
    return 1;
  }

  len = 5;
  if (strncmp(*iter, "return", len) == 0) {
    *iter += len;
    tok.sym = BL_SYM_RET;
    bl_tokens_push(out, &tok);
    return 1;
  }

  len = 6;
  if (strncmp(*iter, "extern", len) == 0) {
    *iter += len;
    tok.sym = BL_SYM_EXTERN;
    bl_tokens_push(out, &tok);
    return 1;
  }

  len = 0;
  while (is_intend_c(**iter)) {
    (*iter)++;
    len++;
  }

  (*iter)--;
  tok.len = len;
  tok.sym = BL_SYM_IDENT;

  bl_tokens_push(out, &tok);
  return 1;
}

static int
scan_number(char  **iter,
            Tokens *out)
{
  if (!is_number_c(**iter))
    return 0;

  int n = 0;
  bl_token_t tok;

  while (true) {
    n = n * 10 + (**iter) - '0';
    if (is_number_c(*(*iter+1)))
      (*iter)++;
    else
      break;
  }

  tok.sym = BL_SYM_NUM;
  tok.content.as_int = n;
  
  bl_tokens_push(out, &tok);
  return 1;
}

/* public */
Tokens *
bl_lexer_scan(BString *in)
{
  Tokens *ret = bl_tokens_new(in);

  bl_token_t tok = {
    .sym = BL_SYM_EOF,
    .len = 0
  };

  for (char *iter = (char *)bo_string_get(in); *iter != '\0'; iter++) {
    switch (*iter) {
      case ' ':
      case '\n':
      case '\t':
        continue;
      case '{':
        tok.sym = BL_SYM_LBLOCK;
        bl_tokens_push(ret, &tok);
        continue;
      case '}':
        tok.sym = BL_SYM_RBLOCK;
        bl_tokens_push(ret, &tok);
        continue;
      case '[':
        tok.sym = BL_SYM_LBRACKET;
        bl_tokens_push(ret, &tok);
        continue;
      case ']':
        tok.sym = BL_SYM_RBRACKET;
        bl_tokens_push(ret, &tok);
        continue;
      case '(':
        tok.sym = BL_SYM_LPAREN;
        bl_tokens_push(ret, &tok);
        continue;
      case ')':
        tok.sym = BL_SYM_RPAREN;
        bl_tokens_push(ret, &tok);
        continue;
      case ',':
        tok.sym = BL_SYM_COMMA;
        bl_tokens_push(ret, &tok);
        continue;
      case '=':
        tok.sym = BL_SYM_ASIGN;
        bl_tokens_push(ret, &tok);
        continue;
      case ';':
        tok.sym = BL_SYM_SEMICOLON;
        bl_tokens_push(ret, &tok);
        continue;
      case '"':
        scan_string(&iter, '"', ret);
        continue;
      case '/':
        switch (*(iter + 1)) {
        case '/':
          ignore_till(&iter, '\n');
          continue;
        default:
          tok.sym = BL_SYM_SLASH;
          bl_tokens_push(ret, &tok);
          continue;
        }
      default:
        if (scan_number(&iter, ret))
          continue;

        if (scan_ident(&iter, ret))
          continue;

        bl_parse_error("unknown character\n");
    }
  }
  return ret;
}

