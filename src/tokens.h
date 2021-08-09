// =================================================================================================
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
// =================================================================================================

#ifndef BL_TOKENS_H
#define BL_TOKENS_H

#include "token.h"

struct tokens {
    TArray buf;
    usize  iter;
};

enum tokens_lookahead_state {
    TOK_LOOK_HIT,
    TOK_LOOK_CONTINUE,
    TOK_LOOK_TERMINAL,
};

typedef enum tokens_lookahead_state (*token_cmp_func_t)(struct token *curr);

void          tokens_init(struct tokens *tokens);
void          tokens_terminate(struct tokens *tokens);
int           tokens_count(struct tokens *tokens);
void          tokens_push(struct tokens *tokens, struct token *t);
struct token *tokens_peek(struct tokens *tokens);
struct token *tokens_peek_last(struct tokens *tokens);
struct token *tokens_peek_2nd(struct tokens *tokens);
struct token *tokens_peek_nth(struct tokens *tokens, usize n);
struct token *tokens_peek_prev(struct tokens *tokens);
struct token *tokens_consume(struct tokens *tokens);
struct token *tokens_consume_if(struct tokens *tokens, enum sym sym);
bool          tokens_current_is(struct tokens *tokens, enum sym sym);
bool          tokens_previous_is(struct tokens *tokens, enum sym sym);
bool          tokens_next_is(struct tokens *tokens, enum sym sym);
bool          tokens_current_is_not(struct tokens *tokens, enum sym sym);
bool          tokens_next_is_not(struct tokens *tokens, enum sym sym);
bool          tokens_is_seq(struct tokens *tokens, usize argc, ...);
void          tokens_reset_iter(struct tokens *tokens);
usize         tokens_get_marker(struct tokens *tokens);
void          tokens_back_to_marker(struct tokens *tokens, usize marker);
void          tokens_consume_till(struct tokens *tokens, enum sym sym);
void          tokens_consume_till2(struct tokens *tokens, usize argc, enum sym *args);
bool          tokens_lookahead_till(struct tokens *tokens, enum sym lookup, enum sym terminal);
bool          tokens_lookahead(struct tokens *tokens, token_cmp_func_t cmp);

TArray *tokens_get_all(struct tokens *tokens);

#endif
