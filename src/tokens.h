//************************************************************************************************
// blc
//
// File:   tokens.h
// Author: Martin Dorazil
// Date:   29.1.18
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

#ifndef BL_TOKENS_H
#define BL_TOKENS_H

#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include <bobject/containers/array.h>
#include "token.h"

typedef struct Tokens
{
  BArray *buf;
  BArray *string_cache;
  size_t  iter;
  size_t  marker;
} Tokens;

void
tokens_init(Tokens *tokens);

void
tokens_terminate(Tokens *tokens);

BString *
tokens_create_cached_str(Tokens *tokens);

int
tokens_count(Tokens *tokens);

void
tokens_push(Tokens *tokens, Token *t);

Token *
tokens_peek(Tokens *tokens);

Token *
tokens_peek_last(Tokens *tokens);

Token *
tokens_peek_2nd(Tokens *tokens);

Token *
tokens_peek_nth(Tokens *tokens, size_t n);

Token *
tokens_peek_prev(Tokens *tokens);

Token *
tokens_consume(Tokens *tokens);

Token *
tokens_consume_if(Tokens *tokens, Sym sym);

bool
tokens_current_is(Tokens *tokens, Sym sym);

bool
tokens_previous_is(Tokens *tokens, Sym sym);

bool
tokens_next_is(Tokens *tokens, Sym sym);

bool
tokens_current_is_not(Tokens *tokens, Sym sym);

bool
tokens_next_is_not(Tokens *tokens, Sym sym);

bool
tokens_is_seq(Tokens *tokens, int cnt, ...);

void
tokens_reset_iter(Tokens *tokens);

void
tokens_set_marker(Tokens *tokens);

void
tokens_back_to_marker(Tokens *tokens);

void
tokens_consume_till(Tokens *tokens, Sym sym);

bool
tokens_lookahead_till(Tokens *tokens, Sym lookup, Sym terminal);

BArray *
tokens_get_all(Tokens *tokens);

#endif
