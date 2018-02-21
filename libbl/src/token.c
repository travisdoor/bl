//*****************************************************************************
// blc
//
// File:   token.c
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

#include "bl/token.h"

char *bl_sym_strings[] = {
#define sm(tok, str) str,
  BL_SYMBOLS_LIST
#undef sm
};

void
bl_token_init(bl_token_t *token,
              bl_sym_e symbol,
              int line,
              int col,
              int len,
              const char *src_loc)
{
  (*token) = (bl_token_t) {.sym = symbol, .line = line, .col = col, .len = len, .src_loc = src_loc};
}

bool
bl_token_is_binop(bl_token_t *token)
{
  return token->sym >= BL_SYM_EQ && token->sym <= BL_SYM_LESS;
}

int
bl_token_binop_precedence(bl_token_t *token)
{
  switch (token->sym) {
    case BL_SYM_IDENT:
      return 50;
    case BL_SYM_NUM:
      return 50;
    case BL_SYM_ASTERISK:
      return 40;
    case BL_SYM_SLASH:
      return 40;
    case BL_SYM_MODULO:
      return 40;
    case BL_SYM_PLUS:
      return 20;
    case BL_SYM_MINUS:
      return 20;
    case BL_SYM_LESS_EQ:
      return 10;
    default:
      return -1;
  }
}
