// =================================================================================================
// blc
//
// File:   token.h
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
// =================================================================================================

#ifndef BL_TOKEN_H
#define BL_TOKEN_H

#include "common.h"
#include <stdio.h>

#define TOKEN_OPTIONAL_LOCATION(tok) ((tok) ? &(tok)->location : NULL)

typedef enum {
#define sm(tok, str) SYM_##tok,
#include "token.inc"
#undef sm
} Sym;

enum token_associativity {
    TOKEN_ASSOC_NONE,
    TOKEN_ASSOC_RIGHT,
    TOKEN_ASSOC_LEFT,
};

extern char *sym_strings[];

struct unit;
struct location {
    s32          line;
    s32          col;
    s32          len;
    struct unit *unit;
};

union token_value {
    const char *str;
    char        c;
    f64         d;
    u64         u;
};

struct token {
    struct location   location;
    union token_value value;
    Sym               sym;
    bool              overflow;
};

// sizeof this structure is 8 bytes so it can be passed by value
struct token_precedence {
    s32                      priority;
    enum token_associativity associativity;
};

static INLINE bool sym_is_binop(Sym sym)
{
    return sym >= SYM_EQ && sym <= SYM_ASTERISK;
}

static INLINE bool token_is_binop(struct token *token)
{
    return sym_is_binop(token->sym);
}

static INLINE bool token_is(struct token *token, Sym sym)
{
    if (!token) return false;
    return token->sym == sym;
}

static INLINE bool token_is_not(struct token *token, Sym sym)
{
    return !token_is(token, sym);
}

bool                    token_is_unary(struct token *token);
struct token_precedence token_prec(struct token *token);

#endif // BL_TOKEN_H
