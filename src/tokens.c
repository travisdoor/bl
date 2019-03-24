//************************************************************************************************
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
//************************************************************************************************

#include "tokens.h"
#include "common.h"
#include <stdarg.h>

void tokens_init(Tokens *tokens) { tokens->buf = bo_array_new(sizeof(Token)); }

void tokens_terminate(Tokens *tokens) { bo_unref(tokens->buf); }

void tokens_push(Tokens *tokens, Token *t) { bo_array_push_back(tokens->buf, *t); }

Token *tokens_peek(Tokens *tokens) { return tokens_peek_nth(tokens, 1); }

Token *tokens_peek_2nd(Tokens *tokens) { return tokens_peek_nth(tokens, 2); }

Token *tokens_peek_last(Tokens *tokens)
{
	const size_t i = bo_array_size(tokens->buf);
	if (i == 0) {
		return NULL;
	}

	return &bo_array_at(tokens->buf, i, Token);
}

Token *tokens_peek_prev(Tokens *tokens)
{
	if (tokens->iter > 0) {
		return &bo_array_at(tokens->buf, tokens->iter - 1, Token);
	}
	return NULL;
}

Token *tokens_peek_nth(Tokens *tokens, size_t n)
{
	const size_t i = tokens->iter + n - 1;
	if (i < bo_array_size(tokens->buf))
		return &bo_array_at(tokens->buf, i, Token);

	return NULL;
}

Token *tokens_consume(Tokens *tokens)
{
	if (tokens->iter < bo_array_size(tokens->buf))
		return &bo_array_at(tokens->buf, tokens->iter++, Token);

	return NULL;
}

Token *tokens_consume_if(Tokens *tokens, Sym sym)
{
	Token *tok;
	if (tokens->iter < bo_array_size(tokens->buf)) {
		tok = &bo_array_at(tokens->buf, tokens->iter, Token);
		if (tok->sym == sym) {
			tokens->iter++;
			return tok;
		}
	}

	return NULL;
}

bool tokens_current_is(Tokens *tokens, Sym sym)
{
	return (&bo_array_at(tokens->buf, tokens->iter, Token))->sym == sym;
}

bool tokens_previous_is(Tokens *tokens, Sym sym)
{
	if (tokens->iter > 0)
		return (&bo_array_at(tokens->buf, tokens->iter - 1, Token))->sym == sym;
	return false;
}

bool tokens_next_is(Tokens *tokens, Sym sym)
{
	return (&bo_array_at(tokens->buf, tokens->iter + 1, Token))->sym == sym;
}

bool tokens_current_is_not(Tokens *tokens, Sym sym)
{
	return (&bo_array_at(tokens->buf, tokens->iter, Token))->sym != sym;
}

bool tokens_next_is_not(Tokens *tokens, Sym sym)
{
	return (&bo_array_at(tokens->buf, tokens->iter + 1, Token))->sym != sym;
}

bool tokens_is_seq(Tokens *tokens, int32_t cnt, ...)
{
	bool ret = true;
	size_t c = bo_array_size(tokens->buf);
	Sym sym = SYM_EOF;
	cnt += (int)tokens->iter;

	va_list valist;
	va_start(valist, cnt);

	for (size_t i = tokens->iter; i < cnt && i < c; ++i) {
		sym = va_arg(valist, Sym);
		if ((&bo_array_at(tokens->buf, i, Token))->sym != sym) {
			ret = false;
			break;
		}
	}

	va_end(valist);
	return ret;
}

size_t tokens_get_marker(Tokens *tokens) { return tokens->iter; }

void tokens_back_to_marker(Tokens *tokens, size_t marker) { tokens->iter = marker; }

void tokens_reset_iter(Tokens *tokens) { tokens->iter = 0; }

BArray *tokens_get_all(Tokens *tokens) { return tokens->buf; }

int tokens_count(Tokens *tokens) { return (int)bo_array_size(tokens->buf); }

void tokens_consume_till(Tokens *tokens, Sym sym)
{
	while (tokens_current_is_not(tokens, sym) && tokens_current_is_not(tokens, SYM_EOF)) {
		tokens_consume(tokens);
	}
}

bool tokens_lookahead_till(Tokens *tokens, Sym lookup, Sym terminal)
{
	bool found = false;
	size_t marker = tokens_get_marker(tokens);
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
	assert(cmp);
	bool found = false;
	size_t marker = tokens_get_marker(tokens);
	Token *curr = NULL;
	TokensLookaheadState state = TOK_LOOK_TERMINAL;
	while (true) {
		curr = tokens_peek(tokens);
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
