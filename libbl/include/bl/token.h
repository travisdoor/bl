//*****************************************************************************
// bl
//
// File:   token.h
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

#ifndef BL_TOKEN_H
#define BL_TOKEN_H

#include <stdio.h>
#include <bobject/bobject.h>

BO_BEGIN_DECLS
#define BL_SYMBOLS_LIST \
  sm(EOF, "end") \
  sm(IDENT, "identifier") \
  sm(STRING, "string") \
  sm(NUM, "number") \
  sm(RETURN, "return") \
  sm(IF, "if") \
  sm(ELSE, "else") \
  sm(EXTERN, "extern") \
  sm(NAMESPACE, "namespace") \
  sm(CLASS, "class") \
  sm(STRUCT, "struct") \
  sm(TRUE, "true") \
  sm(FALSE, "false") \
  sm(LBLOCK, "{") \
  sm(RBLOCK, "}") \
  sm(LBRACKET, "[") \
  sm(RBRACKET, "]") \
  sm(LPAREN, "(") \
  sm(RPAREN, ")") \
  sm(COMMA, ",") \
  sm(SEMICOLON, ";") \
  sm(ASIGN, "=") \
  sm(PLUS, "+") \
  sm(MINUS, "-") \
  sm(ASTERISK, "*") \
  sm(SLASH, "/") \
  sm(NONE, "") \

typedef enum
{
#define sm(tok, str) BL_SYM_##tok,
  BL_SYMBOLS_LIST
#undef sm
} bl_sym_e;

extern BO_EXPORT char *bl_sym_strings[];

typedef struct
{
  bl_sym_e sym;
  int line;
  int col;
  int len;
  const char *src_loc;
  union content_u
  {
    const char *as_string;
    double as_double;
    int as_int;
  } content;
} bl_token_t;

/* content must be set manually */
extern BO_EXPORT void
bl_token_init(bl_token_t *token,
              bl_sym_e symbol,
              int line,
              int col,
              int len,
              const char *src_loc);

extern BO_EXPORT bool
bl_token_is_binop(bl_token_t *token);

BO_END_DECLS

#endif //BL_TOKEN_H
