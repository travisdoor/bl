//************************************************************************************************
// blc
//
// File:   token.c
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

#include "token.h"

char *sym_strings[] = {
#define sm(tok, str) str,
#include "token.inc"
#undef sm
};

bool
token_is_unary(Token *token)
{
	switch (token->sym) {
	case SYM_MINUS:
	case SYM_PLUS:
	case SYM_NOT:
		return true;
	default:
		return false;
	}

	return false;
}

TokenPrecedence
token_prec(Token *token)
{
	switch (token->sym) {
		/* . [ ( */
	case SYM_DOT:
	case SYM_LBRACKET:
	case SYM_LPAREN:
		return (TokenPrecedence){.priority = 60, .associativity = TOKEN_ASSOC_LEFT};

		/* cast sizeof alignof typeinfo */
	case SYM_CAST:
	case SYM_SIZEOF:
	case SYM_ALIGNOF:
	case SYM_TYPEINFO:
		return (TokenPrecedence){.priority = 50, .associativity = TOKEN_ASSOC_RIGHT};

		/* * ^ / % */
	case SYM_ASTERISK:
	case SYM_CARET:
	case SYM_SLASH:
	case SYM_PERCENT:
		return (TokenPrecedence){.priority = 40, .associativity = TOKEN_ASSOC_LEFT};

		/* + - */
	case SYM_PLUS:
	case SYM_MINUS:
		return (TokenPrecedence){.priority = 20, .associativity = TOKEN_ASSOC_LEFT};

		/* >> << */
	case SYM_SHR:
	case SYM_SHL:
		return (TokenPrecedence){.priority = 18, .associativity = TOKEN_ASSOC_LEFT};

		/* < > <= >= */
	case SYM_LESS:
	case SYM_GREATER:
	case SYM_LESS_EQ:
	case SYM_GREATER_EQ:
		return (TokenPrecedence){.priority = 15, .associativity = TOKEN_ASSOC_LEFT};

		/* == != */
	case SYM_EQ:
	case SYM_NEQ:
		return (TokenPrecedence){.priority = 10, .associativity = TOKEN_ASSOC_LEFT};

		/* & */
	case SYM_AND:
		return (TokenPrecedence){.priority = 9, .associativity = TOKEN_ASSOC_LEFT};

		/* | */
	case SYM_OR:
		return (TokenPrecedence){.priority = 8, .associativity = TOKEN_ASSOC_LEFT};

		/* && */
	case SYM_LOGIC_AND:
		return (TokenPrecedence){.priority = 6, .associativity = TOKEN_ASSOC_LEFT};

		/* || */
	case SYM_LOGIC_OR:
		return (TokenPrecedence){.priority = 5, .associativity = TOKEN_ASSOC_LEFT};

		/* = += -= *= /= */
	case SYM_ASSIGN:
	case SYM_PLUS_ASSIGN:
	case SYM_MINUS_ASSIGN:
	case SYM_ASTERISK_ASSIGN:
	case SYM_SLASH_ASSIGN:
	case SYM_PERCENT_ASSIGN:
		return (TokenPrecedence){.priority = 4, .associativity = TOKEN_ASSOC_RIGHT};

	default:
		return (TokenPrecedence){.priority = 0, .associativity = TOKEN_ASSOC_NONE};
	}
}
