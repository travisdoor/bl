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

#include "stb_ds.h"
#include "tokens.h"
#include <stdarg.h>

enum tokens_lookahead_state {
    TOK_LOOK_HIT,
    TOK_LOOK_CONTINUE,
    TOK_LOOK_TERMINAL,
};

typedef enum tokens_lookahead_state (*token_cmp_func_t)(struct token *curr);

#define tokens_len(tokens) arrlen((tokens)->buf)
#define tokens_push(tokens, t) arrput((tokens)->buf, t)
#define tokens_peek_nth(tokens, n)                                                                 \
    ((tokens)->iter + (n) < tokens_len(tokens) && (tokens)->iter + (n) >= 0                        \
         ? &(tokens)->buf[(tokens)->iter + (n)]                                                    \
         : token_end)
#define tokens_peek(tokens) tokens_peek_nth(tokens, 0)
#define tokens_peek_sym(tokens) ((tokens)->buf[(tokens)->iter].sym)
#define tokens_peek_2nd(tokens) tokens_peek_nth(tokens, 1)
#define tokens_peek_prev(tokens) tokens_peek_nth(tokens, -1)
#define tokens_consume(tokens)                                                                     \
    ((tokens)->iter < tokens_len(tokens) ? &(tokens)->buf[(tokens)->iter++] : token_end)
#define tokens_current_is(tokens, s) (tokens_peek(tokens)->sym == (s))
#define tokens_current_is_not(tokens, s) (tokens_peek(tokens)->sym != (s))

static INLINE void tokens_consume_till(struct tokens *tokens, enum sym sym)
{
    while (tokens_current_is_not(tokens, sym) && tokens_current_is_not(tokens, SYM_EOF)) {
        tokens_consume(tokens);
    }
}

static INLINE void tokens_consume_till2(struct tokens *tokens, s64 argc, enum sym *args)
{
    BL_ASSERT(argc && args);
    while (tokens_current_is_not(tokens, SYM_EOF)) {
        for (s64 i = 0; i < argc; ++i) {
            if (tokens_current_is(tokens, args[i])) return;
        }
        tokens_consume(tokens);
    }
}

static INLINE struct token *tokens_consume_if(struct tokens *tokens, enum sym sym)
{
    if (tokens->iter >= arrlen(tokens->buf)) return NULL;
    struct token *token = &tokens->buf[tokens->iter];
    if (token->sym != sym) return NULL;
    tokens->iter++;
    return token;
}

static INLINE bool tokens_is_seq(struct tokens *tokens, s64 argc, ...)
{
    bool     ret = true;
    s64      c   = arrlen(tokens->buf);
    enum sym sym = SYM_EOF;
    argc += tokens->iter;

    va_list valist;
    va_start(valist, argc);

    for (s64 i = tokens->iter; i < argc && i < c; ++i) {
        sym = va_arg(valist, enum sym);
        if ((&tokens->buf[i])->sym != sym) {
            ret = false;
            break;
        }
    }

    va_end(valist);
    return ret;
}

static INLINE bool tokens_lookahead_till(struct tokens *tokens, enum sym lookup, enum sym terminal)
{
    bool      found  = false;
    const s64 marker = tokens->iter;
    while (tokens_current_is_not(tokens, terminal) && tokens_current_is_not(tokens, SYM_EOF)) {
        if (tokens_current_is(tokens, lookup)) {
            found = true;
            break;
        }
        tokens_consume(tokens);
    }
    tokens->iter = marker;
    return found;
}

static INLINE bool tokens_lookahead(struct tokens *tokens, token_cmp_func_t cmp)
{
    BL_ASSERT(cmp);
    bool                        found  = false;
    const s64                   marker = tokens->iter;
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
    tokens->iter = marker;
    return found;
}
#endif
