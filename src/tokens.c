// =================================================================================================
// blc
//
// File:   tokens.c
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

#include "tokens.h"
#include "common.h"
#include <stdarg.h>

void tokens_init(Tokens *tokens)
{
    tarray_init(&tokens->buf, sizeof(struct token));
    tarray_reserve(&tokens->buf, 512);
}

void tokens_terminate(Tokens *tokens)
{
    tarray_terminate(&tokens->buf);
}

void tokens_push(Tokens *tokens, struct token *t)
{
    tarray_push(&tokens->buf, *t);
}

struct token *tokens_peek(Tokens *tokens)
{
    return tokens_peek_nth(tokens, 1);
}

struct token *tokens_peek_2nd(Tokens *tokens)
{
    return tokens_peek_nth(tokens, 2);
}

struct token *tokens_peek_last(Tokens *tokens)
{
    const usize i = tokens->buf.size;
    if (i == 0) BL_ABORT("Peeking empty tokens!");
    return &tarray_at(struct token, &tokens->buf, i - 1);
}

struct token *tokens_peek_prev(Tokens *tokens)
{
    if (tokens->iter > 0) {
        return &tarray_at(struct token, &tokens->buf, tokens->iter - 1);
    }
    return NULL;
}

struct token *tokens_peek_nth(Tokens *tokens, usize n)
{
    const usize i = tokens->iter + n - 1;
    if (i < tokens->buf.size) return &tarray_at(struct token, &tokens->buf, i);
    return tokens_peek_last(tokens);
}

struct token *tokens_consume(Tokens *tokens)
{
    if (tokens->iter < tokens->buf.size)
        return &tarray_at(struct token, &tokens->buf, tokens->iter++);
    return NULL;
}

struct token *tokens_consume_if(Tokens *tokens, Sym sym)
{
    struct token *tok;
    if (tokens->iter < tokens->buf.size) {
        tok = &tarray_at(struct token, &tokens->buf, tokens->iter);
        if (tok->sym == sym) {
            tokens->iter++;
            return tok;
        }
    }

    return NULL;
}

bool tokens_current_is(Tokens *tokens, Sym sym)
{
    return (&tarray_at(struct token, &tokens->buf, tokens->iter))->sym == sym;
}

bool tokens_previous_is(Tokens *tokens, Sym sym)
{
    if (tokens->iter > 0)
        return (&tarray_at(struct token, &tokens->buf, tokens->iter - 1))->sym == sym;
    return false;
}

bool tokens_next_is(Tokens *tokens, Sym sym)
{
    return (&tarray_at(struct token, &tokens->buf, tokens->iter + 1))->sym == sym;
}

bool tokens_current_is_not(Tokens *tokens, Sym sym)
{
    return (&tarray_at(struct token, &tokens->buf, tokens->iter))->sym != sym;
}

bool tokens_next_is_not(Tokens *tokens, Sym sym)
{
    return (&tarray_at(struct token, &tokens->buf, tokens->iter + 1))->sym != sym;
}

bool tokens_is_seq(Tokens *tokens, usize argc, ...)
{
    bool  ret = true;
    usize c   = tokens->buf.size;
    Sym   sym = SYM_EOF;
    argc += (int)tokens->iter;

    va_list valist;
    va_start(valist, argc);

    for (usize i = tokens->iter; i < argc && i < c; ++i) {
        sym = va_arg(valist, Sym);
        if ((&tarray_at(struct token, &tokens->buf, i))->sym != sym) {
            ret = false;
            break;
        }
    }

    va_end(valist);
    return ret;
}

usize tokens_get_marker(Tokens *tokens)
{
    return tokens->iter;
}

void tokens_back_to_marker(Tokens *tokens, usize marker)
{
    tokens->iter = marker;
}

void tokens_reset_iter(Tokens *tokens)
{
    tokens->iter = 0;
}

TArray *tokens_get_all(Tokens *tokens)
{
    return &tokens->buf;
}

int tokens_count(Tokens *tokens)
{
    return (int)tokens->buf.size;
}

void tokens_consume_till(Tokens *tokens, Sym sym)
{
    while (tokens_current_is_not(tokens, sym) && tokens_current_is_not(tokens, SYM_EOF)) {
        tokens_consume(tokens);
    }
}

void tokens_consume_till2(Tokens *tokens, usize argc, Sym *args)
{
    BL_ASSERT(argc && args);
    while (tokens_current_is_not(tokens, SYM_EOF)) {
        for (usize i = 0; i < argc; ++i) {
            if (tokens_current_is(tokens, args[i])) return;
        }
        tokens_consume(tokens);
    }
}

bool tokens_lookahead_till(Tokens *tokens, Sym lookup, Sym terminal)
{
    bool  found  = false;
    usize marker = tokens_get_marker(tokens);
    while (tokens_current_is_not(tokens, terminal) && tokens_current_is_not(tokens, SYM_EOF)) {
        if (tokens_current_is(tokens, lookup)) {
            found = true;
            break;
        }

        tokens_consume(tokens);
    }
    tokens_back_to_marker(tokens, marker);
    return found;
}

bool tokens_lookahead(Tokens *tokens, TokenCmpFunc cmp)
{
    BL_ASSERT(cmp);
    bool                 found  = false;
    usize                marker = tokens_get_marker(tokens);
    struct token *       curr   = NULL;
    TokensLookaheadState state  = TOK_LOOK_TERMINAL;
    while (true) {
        curr  = tokens_peek(tokens);
        state = cmp(curr);
        if (curr->sym == SYM_EOF || state == TOK_LOOK_TERMINAL) {
            break;
        } else if (state == TOK_LOOK_HIT) {
            found = true;
            break;
        }
        tokens_consume(tokens);
    }
    tokens_back_to_marker(tokens, marker);
    return found;
}
