//*****************************************************************************
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
//*****************************************************************************

#ifndef TOKENS_H_Z3NM7BJC
#define TOKENS_H_Z3NM7BJC

#include <bobject/bobject.h>
#include <bobject/containers/string.h>
#include <bobject/containers/array.h>
#include "token_impl.h"

typedef struct bl_tokens
{
  BArray *buf;
  BArray *string_cache;
  size_t iter;
  size_t marker;
} bl_tokens_t;

void
bl_tokens_init(bl_tokens_t *tokens);

void
bl_tokens_terminate(bl_tokens_t *tokens);

BString *
bl_tokens_create_cached_str(bl_tokens_t *tokens);

int
bl_tokens_count(bl_tokens_t *tokens);

void
bl_tokens_push(bl_tokens_t *tokens, bl_token_t *t);

bl_token_t *
bl_tokens_peek(bl_tokens_t *tokens);

bl_token_t *
bl_tokens_peek_last(bl_tokens_t *tokens);

bl_token_t *
bl_tokens_peek_2nd(bl_tokens_t *tokens);

bl_token_t *
bl_tokens_peek_nth(bl_tokens_t *tokens, size_t n);

bl_token_t *
bl_tokens_peek_prev(bl_tokens_t *tokens);

bl_token_t *
bl_tokens_consume(bl_tokens_t *tokens);

bl_token_t *
bl_tokens_consume_if(bl_tokens_t *tokens, bl_sym_e sym);

bool
bl_tokens_current_is(bl_tokens_t *tokens, bl_sym_e sym);

bool
bl_tokens_previous_is(bl_tokens_t *tokens, bl_sym_e sym);

bool
bl_tokens_next_is(bl_tokens_t *tokens, bl_sym_e sym);

bool
bl_tokens_current_is_not(bl_tokens_t *tokens, bl_sym_e sym);

bool
bl_tokens_next_is_not(bl_tokens_t *tokens, bl_sym_e sym);

bool
bl_tokens_is_seq(bl_tokens_t *tokens, int cnt, ...);

void
bl_tokens_reset_iter(bl_tokens_t *tokens);

void
bl_tokens_set_marker(bl_tokens_t *tokens);

void
bl_tokens_back_to_marker(bl_tokens_t *tokens);

BArray *
bl_tokens_get_all(bl_tokens_t *tokens);

#endif /* end of include guard: TOKENS_H_Z3NM7BJC */
