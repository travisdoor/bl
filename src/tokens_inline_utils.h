// =================================================================================================
// bl
//
// File:   tokens_inline_utils.h
// Author: Martin Dorazil
// Date:   12/9/21
//
// Copyright 2021 Martin Dorazil
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

#ifndef BL_TOKENS_INLINE_UTILS_H
#define BL_TOKENS_INLINE_UTILS_H

#include "tokens.h"

enum tokens_lookahead_state {
    TOK_LOOK_HIT,
    TOK_LOOK_CONTINUE,
    TOK_LOOK_TERMINAL,
};

typedef enum tokens_lookahead_state (*token_cmp_func_t)(struct token *curr);

static INLINE void tokens_push(struct tokens *tokens, struct token *t)
{
    tarray_push(&tokens->buf, *t);
}

static INLINE struct token *tokens_peek_last(struct tokens *tokens)
{
    const usize i = tokens->buf.size;
    if (i == 0) BL_ABORT("Peeking empty tokens!");
    return &tarray_at(struct token, &tokens->buf, i - 1);
}

static INLINE struct token *tokens_peek_nth(struct tokens *tokens, usize n)
{
    const usize i = tokens->iter + n - 1;
    if (i < tokens->buf.size) return &tarray_at(struct token, &tokens->buf, i);
    return tokens_peek_last(tokens);
}

static INLINE struct token *tokens_peek(struct tokens *tokens)
{
    return tokens_peek_nth(tokens, 1);
}

static INLINE struct token *tokens_peek_2nd(struct tokens *tokens)
{
    return tokens_peek_nth(tokens, 2);
}

static INLINE struct token *tokens_peek_prev(struct tokens *tokens)
{
    if (tokens->iter > 0) {
        return &tarray_at(struct token, &tokens->buf, tokens->iter - 1);
    }
    return NULL;
}

static INLINE struct token *tokens_consume(struct tokens *tokens)
{
    if (tokens->iter >= tokens->buf.size) return NULL;
    return &tarray_at(struct token, &tokens->buf, tokens->iter++);
}

static INLINE bool tokens_current_is(struct tokens *tokens, enum sym sym)
{
    return tarray_at(struct token, &tokens->buf, tokens->iter).sym == sym;
}

static INLINE bool tokens_previous_is(struct tokens *tokens, enum sym sym)
{
    if (tokens->iter == 0) return false;
    return tarray_at(struct token, &tokens->buf, tokens->iter - 1).sym == sym;
}

static INLINE bool tokens_next_is(struct tokens *tokens, enum sym sym)
{
    return tarray_at(struct token, &tokens->buf, tokens->iter + 1).sym == sym;
}

static INLINE bool tokens_current_is_not(struct tokens *tokens, enum sym sym)
{
    return !tokens_current_is(tokens, sym);
}

static INLINE bool tokens_next_is_not(struct tokens *tokens, enum sym sym)
{
    return !tokens_next_is(tokens, sym);
}

static INLINE usize tokens_get_marker(struct tokens *tokens)
{
    return tokens->iter;
}

static INLINE void tokens_back_to_marker(struct tokens *tokens, usize marker)
{
    tokens->iter = marker;
}

static INLINE void tokens_reset_iter(struct tokens *tokens)
{
    tokens->iter = 0;
}

static INLINE TArray *tokens_get_all(struct tokens *tokens)
{
    return &tokens->buf;
}

static INLINE int tokens_count(struct tokens *tokens)
{
    return (int)tokens->buf.size;
}

static INLINE void tokens_consume_till(struct tokens *tokens, enum sym sym)
{
    while (tokens_current_is_not(tokens, sym) && tokens_current_is_not(tokens, SYM_EOF)) {
        tokens_consume(tokens);
    }
}

static INLINE void tokens_consume_till2(struct tokens *tokens, usize argc, enum sym *args)
{
    BL_ASSERT(argc && args);
    while (tokens_current_is_not(tokens, SYM_EOF)) {
        for (usize i = 0; i < argc; ++i) {
            if (tokens_current_is(tokens, args[i])) return;
        }
        tokens_consume(tokens);
    }
}

static INLINE struct token *tokens_consume_if(struct tokens *tokens, enum sym sym)
{
    if (tokens->iter >= tokens->buf.size) return NULL;
    struct token *token = &tarray_at(struct token, &tokens->buf, tokens->iter);
    if (token->sym != sym) return NULL;
    tokens->iter++;
    return token;
}

static bool tokens_is_seq(struct tokens *tokens, usize argc, ...)
{
    bool     ret = true;
    usize    c   = tokens->buf.size;
    enum sym sym = SYM_EOF;
    argc += (int)tokens->iter;

    va_list valist;
    va_start(valist, argc);

    for (usize i = tokens->iter; i < argc && i < c; ++i) {
        sym = va_arg(valist, enum sym);
        if ((&tarray_at(struct token, &tokens->buf, i))->sym != sym) {
            ret = false;
            break;
        }
    }

    va_end(valist);
    return ret;
}

static bool tokens_lookahead_till(struct tokens *tokens, enum sym lookup, enum sym terminal)
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

static bool tokens_lookahead(struct tokens *tokens, token_cmp_func_t cmp)
{
    BL_ASSERT(cmp);
    bool                        found  = false;
    usize                       marker = tokens_get_marker(tokens);
    struct token *              curr   = NULL;
    enum tokens_lookahead_state state  = TOK_LOOK_TERMINAL;
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
#endif
