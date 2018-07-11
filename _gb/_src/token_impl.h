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

#include <stdio.h>
#include <bobject/bobject.h>

// clang-format off
#define BL_SYMBOLS_LIST \
  sm(EOF = 0,     "end") \
  sm(IDENT,       "identifier") \
  sm(STRING,      "string") \
  sm(CHAR,        "char") \
  sm(NUM,         "number") \
  sm(FLOAT,       "float") \
  sm(DOUBLE,      "double") \
  sm(IF,          "if") /* must be first */ \
  sm(FN,          "fn") \
  sm(MUT,         "mut") \
  sm(CAST,        "cast") \
  sm(ELSE,        "else") \
  sm(TRUE,        "true") \
  sm(NULL,        "null") \
  sm(ENUM,        "enum") \
  sm(LOOP,        "loop")\
  sm(RUN,         "#run")\
  sm(FALSE,       "false") \
  sm(USING,       "using") \
  sm(CONST,       "const") \
  sm(WHILE,       "while") \
  sm(BREAK,       "break")\
  sm(TEST,        "#test") \
  sm(LOAD,        "#load") \
  sm(LINK,        "#link") \
  sm(LINE,        "#line") \
  sm(FILE,        "#file") \
  sm(PUBLIC,      "public")\
  sm(EXTERN,      "extern") \
  sm(MODULE,      "module") \
  sm(EXPORT,      "export") \
  sm(RETURN,      "return") \
  sm(STRUCT,      "struct") \
  sm(SIZEOF,      "sizeof") \
  sm(CONTINUE,    "continue") /* must be last */ \
  sm(PATH,        "::") \
  sm(ARROW,       "->") \
  sm(LCOMMENT,    "//") \
  sm(LBCOMMENT,   "/*") \
  sm(RBCOMMENT,   "*/") \
  sm(LBLOCK,      "{") \
  sm(RBLOCK,      "}")\
  sm(LBRACKET,    "[") \
  sm(RBRACKET,    "]")\
  sm(LPAREN,      "(") \
  sm(RPAREN,      ")") \
  sm(COMMA,       ",")\
  sm(SEMICOLON,   ";") \
  sm(COLON,       ":") \
  sm(EQ,          "==") /* logical begin */ \
  sm(NEQ,         "!=") \
  sm(GREATER_EQ,  ">=") \
  sm(LESS_EQ,     "<=")\
  sm(LOGIC_AND,   "&&") \
  sm(LOGIC_OR,    "||") /* logical end */ \
  sm(AND,         "&") \
  sm(OR,          "|") \
  sm(XOR,         "^") \
  sm(NOT,         "!") \
  sm(SLASH,       "/") \
  sm(MODULO,      "%") \
  sm(ASSIGN,      "=") \
  sm(PLUS,        "+") \
  sm(MINUS,       "-")\
  sm(ASTERISK,    "*") \
  sm(GREATER,     ">") \
  sm(LESS,        "<") \
  sm(DOT,         ".") \
  sm(NONE,        "")\

typedef enum {
#define sm(tok, str) BL_SYM_##tok,
  BL_SYMBOLS_LIST
#undef sm
} bl_sym_e;
// clang-format on

extern char *bl_sym_strings[];

struct bl_unit;
typedef struct bl_src
{
  int             line;
  int             col;
  int             len;
  struct bl_unit *unit;
} bl_src_t;

typedef struct bl_token
{
  bl_sym_e sym;
  bl_src_t src;
  union
  {
    const char *       str;
    char               c;
    double             d;
    unsigned long long u;
  } value;
} bl_token_t;

/* is token any known binary operation? */
bool
bl_token_is_binop(bl_token_t *token);

bool
bl_token_is(bl_token_t *token, bl_sym_e sym);

/* is token logical operation which result type should be boolean */
bool
bl_token_is_logic_op(bl_token_t *token);

bool
bl_token_is_unary(bl_token_t *token);

/*
 * Return token precedence or -1 when token is not binary operation, identifier or constant.
 */
int
bl_token_prec(bl_token_t *token);

#endif // BL_TOKEN_H
