//************************************************************************************************
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
//************************************************************************************************

#include "token_impl.h"

char *bl_sym_strings[] = {
#define sm(tok, str) str,
    BL_SYMBOLS_LIST
#undef sm
};

bool
bl_token_is_binop(bl_token_t *token)
{
  return token->sym >= BL_SYM_EQ && token->sym <= BL_SYM_LESS;
}

bool
bl_token_is_unary(bl_token_t *token)
{
  switch (token->sym) {
  case BL_SYM_AND:
  case BL_SYM_ASTERISK:
  case BL_SYM_MINUS:
  case BL_SYM_PLUS:
  case BL_SYM_NOT:
    return true;
  default:
    return false;
  }

  return false;
}

bool
bl_token_is_logic_op(bl_token_t *token)
{
  switch (token->sym) {
  case BL_SYM_LESS:
  case BL_SYM_GREATER:
  case BL_SYM_LESS_EQ:
  case BL_SYM_GREATER_EQ:
  case BL_SYM_EQ:
  case BL_SYM_NEQ:
  case BL_SYM_LOGIC_AND:
  case BL_SYM_LOGIC_OR:
    return true;

  default:
    return false;
  }
}

int
bl_token_prec(bl_token_t *token, bool unary)
{
  switch (token->sym) {
    // . -> (
  case BL_SYM_DOT:
  case BL_SYM_ARROW:
  case BL_SYM_LBRACKET:
    return 60;

    // ident number cast + -
  case BL_SYM_IDENT:
  case BL_SYM_NUM:
  case BL_SYM_CAST:
    return 50;

    // * / %
  case BL_SYM_ASTERISK:
  case BL_SYM_SLASH:
  case BL_SYM_MODULO:
    return 40;

    // + -
  case BL_SYM_PLUS:
  case BL_SYM_MINUS:
    return unary ? 50 : 20;

    // .
  case BL_SYM_NOT:
    return 20;

    // < > <= >=
  case BL_SYM_LESS:
  case BL_SYM_GREATER:
  case BL_SYM_LESS_EQ:
  case BL_SYM_GREATER_EQ:
    return 15;

    // == !=
  case BL_SYM_EQ:
  case BL_SYM_NEQ:
    return 10;

    // &
  case BL_SYM_AND:
    return 9;

    // ~
  case BL_SYM_XOR:
    return 8;

    // |
  case BL_SYM_OR:
    return 7;

    // &&
  case BL_SYM_LOGIC_AND:
    return 6;

    // ||
  case BL_SYM_LOGIC_OR:
    return 5;

    // =
  case BL_SYM_ASSIGN:
    return 4;
  default:
    return -1;
  }
}

bool
bl_token_is(bl_token_t *token, bl_sym_e sym)
{
  if (!token) return false;
  return token->sym == sym;
}

bool
bl_token_is_not(bl_token_t *token, bl_sym_e sym)
{
  return !bl_token_is(token, sym);
}
