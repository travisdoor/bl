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

#include "token.h"

char *sym_strings[] = {
#define sm(tok, str) str,
    _SYMBOLS_LIST
#undef sm
};

bool
token_is_binop(Token *token)
{
  return token->sym >= SYM_EQ && token->sym <= SYM_LESS;
}

bool
token_is_unary(Token *token)
{
  switch (token->sym) {
  case SYM_AND:
  case SYM_ASTERISK:
  case SYM_MINUS:
  case SYM_PLUS:
  case SYM_NOT:
    return true;
  default:
    return false;
  }

  return false;
}

bool
token_is_logic_op(Token *token)
{
  switch (token->sym) {
  case SYM_LESS:
  case SYM_GREATER:
  case SYM_LESS_EQ:
  case SYM_GREATER_EQ:
  case SYM_EQ:
  case SYM_NEQ:
  case SYM_LOGIC_AND:
  case SYM_LOGIC_OR:
    return true;

  default:
    return false;
  }
}

int
token_prec(Token *token, bool unary)
{
  switch (token->sym) {
    // . -> [ (
  case SYM_DOT:
  case SYM_ARROW:
  case SYM_LBRACKET:
  case SYM_LPAREN:
    return 60;

    // ident number cast + -
  case SYM_IDENT:
  case SYM_NUM:
  case SYM_CAST:
    return 50;

    // * / %
  case SYM_ASTERISK:
  case SYM_SLASH:
  case SYM_MODULO:
    return 40;

    // + -
  case SYM_PLUS:
  case SYM_MINUS:
    return unary ? 50 : 20;

    // .
  case SYM_NOT:
    return 20;

    // < > <= >=
  case SYM_LESS:
  case SYM_GREATER:
  case SYM_LESS_EQ:
  case SYM_GREATER_EQ:
    return 15;

    // == !=
  case SYM_EQ:
  case SYM_NEQ:
    return 10;

    // &
  case SYM_AND:
    return 9;

    // ~
  case SYM_XOR:
    return 8;

    // |
  case SYM_OR:
    return 7;

    // &&
  case SYM_LOGIC_AND:
    return 6;

    // ||
  case SYM_LOGIC_OR:
    return 5;

    // = += -= *= /=
  case SYM_ASSIGN:
  case SYM_PLUS_ASSIGN:
  case SYM_MINUS_ASSIGN:
  case SYM_MUL_ASSIGN:
  case SYM_DIV_ASSIGN:
  case SYM_MOD_ASSIGN:
    return 4;

  default:
    return -1;
  }
}

bool
token_is(Token *token, Sym sym)
{
  if (!token) return false;
  return token->sym == sym;
}

bool
token_is_not(Token *token, Sym sym)
{
  return !token_is(token, sym);
}
