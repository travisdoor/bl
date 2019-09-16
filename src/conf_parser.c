//************************************************************************************************
// bl
//
// File:   conf_parser.c
// Author: Martin Dorazil
// Date:   24/07/2019
//
// Copyright 2019 Martin Dorazil
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

#include "common.h"
#include "stages.h"

typedef struct {
	Builder *builder;
	Tokens * tokens;
} Context;

static bool
parse_key_value_rq(Context *cnt)
{
	Token *tok_ident = tokens_consume(cnt->tokens);
	if (token_is_not(tok_ident, SYM_IDENT)) {
		builder_msg(cnt->builder,
		            BUILDER_MSG_ERROR,
		            ERR_UNEXPECTED_SYMBOL,
		            &tok_ident->location,
		            BUILDER_CUR_WORD,
		            "Expected key identificator.");
		return false;
	}

	Token *tok_value = tokens_consume(cnt->tokens);

	ConfDataValue tmp;

	switch (tok_value->sym) {
	case SYM_STRING:
		tmp.kind       = CDV_STRING;
		tmp.data.v_str = tok_value->value.str;
		break;
	case SYM_NUM:
		tmp.kind       = CDV_INT;
		tmp.data.v_int = (int)tok_value->value.u;
		break;
	default:
		builder_msg(cnt->builder,
		            BUILDER_MSG_ERROR,
		            ERR_UNEXPECTED_SYMBOL,
		            &tok_ident->location,
		            BUILDER_CUR_AFTER,
		            "Expected value after key identificator.");

		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return false;
	}

	const char *key = tok_ident->value.str;
	bl_assert(key);
	if (conf_data_has_key(cnt->builder->conf, key)) {
		builder_msg(cnt->builder,
		            BUILDER_MSG_ERROR,
		            ERR_DUPLICATE_SYMBOL,
		            &tok_ident->location,
		            BUILDER_CUR_WORD,
		            "Duplicate symbol in conf scope.");
	} else {
		conf_data_add(cnt->builder->conf, key, &tmp);
	}

	return true;
}

static void
parse_top_level(Context *cnt)
{
	while (token_is_not(tokens_peek(cnt->tokens), SYM_EOF)) {
		if (!parse_key_value_rq(cnt)) break;
	}
}

void
conf_parser_run(Builder *builder, Unit *unit)
{
	Context cnt = {.builder = builder, .tokens = &unit->tokens};

	parse_top_level(&cnt);
}
