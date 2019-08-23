//************************************************************************************************
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
//************************************************************************************************

#ifndef BL_TOKEN_H
#define BL_TOKEN_H

#include <bobject/bobject.h>
#include <stdio.h>

typedef enum {
#define sm(tok, str) SYM_##tok,
#include "token.def"
#undef sm
} Sym;

typedef enum {
	TOKEN_ASSOC_NONE,
	TOKEN_ASSOC_RIGHT,
	TOKEN_ASSOC_LEFT,
} TokenAssociativity;

extern char *sym_strings[];

struct Unit;
typedef struct Location {
	int32_t      line;
	int32_t      col;
	int32_t      len;
	struct Unit *unit;
} Location;

typedef union {
	const char *       str;
	char               c;
	double             d;
	unsigned long long u;
} TokenValue;

typedef struct Token {
	Sym        sym;
	Location   location;
	TokenValue value;
} Token;

/* sizeof this structure is 8 bytes so it can be passed by value */
typedef struct {
	int32_t            priority;
	TokenAssociativity associativity;
} TokenPrecedence;

static inline bool
sym_is_binop(Sym sym)
{
	return sym >= SYM_EQ && sym <= SYM_ASTERISK;
}

static inline bool
token_is_binop(Token *token)
{
	return sym_is_binop(token->sym);
}

bool
token_is_unary(Token *token);

TokenPrecedence
token_prec(Token *token);

static inline bool
token_is(Token *token, Sym sym)
{
	if (!token) return false;
	return token->sym == sym;
}

static inline bool
token_is_not(Token *token, Sym sym)
{
	return !token_is(token, sym);
}

#endif // BL_TOKEN_H
