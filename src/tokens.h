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

#include "common.h"

#define TOKEN_OPTIONAL_LOCATION(tok) ((tok) ? &(tok)->location : NULL)

enum sym {
#define sm(tok, str, len) SYM_##tok,
#include "tokens.inc"
#undef sm
};

extern char *        sym_strings[];
extern s32           sym_lens[];
extern struct token *token_end;

struct unit;
struct location {
    u16          line;
    u16          col;
    u32          len;
    struct unit *unit;
};

union token_value {
    const char *str;
    char        c;
    f64         d;
    u64         u;
};

struct token {
    enum sym          sym;
    bool              overflow;
    struct location   location;
    union token_value value;
};

enum token_associativity {
    TOKEN_ASSOC_NONE,
    TOKEN_ASSOC_RIGHT,
    TOKEN_ASSOC_LEFT,
};

struct token_precedence {
    s32                      priority;
    enum token_associativity associativity;
};

struct tokens {
    struct token *buf;
    usize         iter;
};

static INLINE bool sym_is_binop(enum sym sym)
{
    return sym >= SYM_EQ && sym <= SYM_ASTERISK;
}

static INLINE bool token_is_binop(struct token *token)
{
    return sym_is_binop(token->sym);
}

static INLINE bool token_is(struct token *token, enum sym sym)
{
    if (!token) return false;
    return token->sym == sym;
}

static INLINE bool token_is_not(struct token *token, enum sym sym)
{
    return !token_is(token, sym);
}

void                    tokens_init(struct tokens *tokens);
void                    tokens_terminate(struct tokens *tokens);
bool                    token_is_unary(struct token *token);
struct token_precedence token_prec(struct token *token);

#endif
