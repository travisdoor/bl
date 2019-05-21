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

// clang-format off
#define _SYMBOLS_LIST \
  sm(EOF = 0,        "end") \
  sm(IDENT,          "identifier") \
  sm(STRING,         "string") \
  sm(CHAR,           "char") \
  sm(NUM,            "number") \
  sm(FLOAT,          "float") \
  sm(DOUBLE,         "double") \
  sm(IF,             "if") /* must be first */ \
  sm(FN,             "fn") \
  sm(CAST,           "cast") \
  sm(ELSE,           "else") \
  sm(TRUE,           "true") \
  sm(NULL,           "null") \
  sm(ENUM,           "enum") \
  sm(LOOP,           "loop")\
  sm(LOAD,           "load") \
  sm(LINK,           "link") \
  sm(TEST,           "test") \
  sm(FALSE,          "false") \
  sm(BREAK,          "break")\
  sm(EXTERN,         "extern") \
  sm(RETURN,         "return") \
  sm(SIZEOF,         "sizeof") \
  sm(STRUCT,         "struct") \
  sm(ALIGNOF,        "alignof") \
  sm(COMPILER,       "compiler") \
  sm(CONTINUE,       "continue") \
  sm(TYPEINFO,       "typeinfo") \
  sm(UNREACHABLE,    "unreachable") \
  sm(AT,             "@") \
  sm(LCOMMENT,       "//") \
  sm(LBCOMMENT,      "/*") \
  sm(RBCOMMENT,      "*/") \
  sm(HASH,           "#") \
  sm(LBLOCK,         "{") \
  sm(RBLOCK,         "}") \
  sm(LBRACKET,       "[") \
  sm(RBRACKET,       "]") \
  sm(LPAREN,         "(") \
  sm(RPAREN,         ")") \
  sm(COMMA,          ",") \
  sm(SEMICOLON,      ";") \
  sm(COLON,          ":") \
  sm(VARGS,          "...") \
  sm(EQ,             "==") /* logical begin */ \
  sm(NEQ,            "!=") \
  sm(GREATER_EQ,     ">=") \
  sm(LESS_EQ,        "<=") \
  sm(LOGIC_AND,      "&&") \
  sm(GREATER,        ">") \
  sm(LESS,           "<") \
  sm(LOGIC_OR,       "||") /* logical end */ \
  sm(PLUS_ASSIGN,    "+=") /* assign bebin */ \
  sm(MINUS_ASSIGN,   "-=") \
  sm(ASTERISK_ASSIGN,"*=") \
  sm(SLASH_ASSIGN,   "/=") \
  sm(PERCENT_ASSIGN, "%=") \
  sm(ASSIGN,         "=") /* assign end */\
  sm(AND,            "&") \
  sm(OR,             "|") \
  sm(CARET,          "^") \
  sm(NOT,            "!") \
  sm(SLASH,          "/") \
  sm(PERCENT,        "%") \
  sm(PLUS,           "+") \
  sm(MINUS,          "-") \
  sm(ASTERISK,       "*") \
  sm(DOT,            ".") \
  sm(NONE,           "") \

typedef enum {
#define sm(tok, str) SYM_##tok,
  _SYMBOLS_LIST
#undef sm
} Sym;
// clang-format on

typedef enum {
	TOKEN_ASSOC_NONE,
	TOKEN_ASSOC_RIGHT,
	TOKEN_ASSOC_LEFT,
} TokenAssociativity;

extern char *sym_strings[];

struct Unit;
typedef struct Src {
	int32_t      line;
	int32_t      col;
	int32_t      len;
	struct Unit *unit;
} Src;

typedef union {
	const char *       str;
	char               c;
	double             d;
	unsigned long long u;
} TokenValue;

typedef struct Token {
	Sym        sym;
	Src        src;
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
