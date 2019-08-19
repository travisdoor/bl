//************************************************************************************************
// bl
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include "common.h"
#include "stages.h"
#include <setjmp.h>

#define EXPECTED_GSCOPE_COUNT 4096
#define EXPECTED_PRIVATE_SCOPE_COUNT 256

#define parse_error(cnt, kind, tok, pos, format, ...)                                              \
	{                                                                                          \
		builder_msg((cnt)->builder,                                                        \
		            BUILDER_MSG_ERROR,                                                     \
		            (kind),                                                                \
		            &(tok)->location,                                                      \
		            (pos),                                                                 \
		            (format),                                                              \
		            ##__VA_ARGS__);                                                        \
	}

#define parse_warning(cnt, tok, pos, format, ...)                                                  \
	{                                                                                          \
		builder_msg((cnt)->builder,                                                        \
		            BUILDER_MSG_WARNING,                                                   \
		            0,                                                                     \
		            &(tok)->location,                                                      \
		            (pos),                                                                 \
		            (format),                                                              \
		            ##__VA_ARGS__);                                                        \
	}

#define parse_note(cnt, tok, pos, format, ...)                                                     \
	{                                                                                          \
		builder_msg((cnt)->builder,                                                        \
		            BUILDER_MSG_NOTE,                                                      \
		            0,                                                                     \
		            &(tok)->location,                                                      \
		            (pos),                                                                 \
		            (format),                                                              \
		            ##__VA_ARGS__);                                                        \
	}

/* swap current compound with _cmp and create temporary variable with previous one */

#define push_curr_decl(_cnt, _decl)                                                                \
	Ast *const _prev_decl = (_cnt)->curr_decl;                                                 \
	(_cnt)->curr_decl     = (_decl);

#define pop_curr_decl(_cnt) (_cnt)->curr_decl = _prev_decl;

#define push_inloop(_cnt)                                                                          \
	bool _prev_inloop   = (_cnt)->inside_loop;                                                 \
	(_cnt)->inside_loop = true;

#define pop_inloop(_cnt) (_cnt)->inside_loop = _prev_inloop;

#define push_scope(_cnt, _scope)                                                                   \
	Scope *_prev_scope = (_cnt)->scope;                                                        \
	(_cnt)->scope      = _scope;

#define pop_scope(_cnt) (_cnt)->scope = _prev_scope;

typedef enum {
	HD_NONE     = 1 << 0,
	HD_LOAD     = 1 << 1,
	HD_LINK     = 1 << 2,
	HD_TEST     = 1 << 3,
	HD_EXTERN   = 1 << 4,
	HD_COMPILER = 1 << 5,
	HD_PRIVATE  = 1 << 6,
} HashDirective;

typedef struct {
	Builder *    builder;
	Assembly *   assembly;
	Unit *       unit;
	Arena *      ast_arena;
	ScopeArenas *scope_arenas;
	Tokens *     tokens;

	/* tmps */
	Scope *scope;
	Ast *  curr_decl;
	bool   inside_loop;
	bool   inside_private_scope;
} Context;

/* helpers */
/* fw decls */
static BinopKind
sym_to_binop_kind(Sym sm);

static UnopKind
sym_to_unop_kind(Sym sm);

static void
parse_ublock_content(Context *cnt, Ast *ublock);

static Ast *
parse_hash_directive(Context *cnt, int32_t expected_mask, HashDirective *satisfied);

static void
parse_flags_for_curr_decl(Context *cnt);

static Ast *
parse_unrecheable(Context *cnt);

static Ast *
parse_ident(Context *cnt);

static Ast *
parse_block(Context *cnt);

static Ast *
parse_decl(Context *cnt);

static Ast *
parse_decl_member(Context *cnt, bool type_only);

static Ast *
parse_decl_arg(Context *cnt, bool type_only);

static Ast *
parse_decl_variant(Context *cnt, Ast *prev);

static Ast *
parse_type(Context *cnt);

static Ast *
parse_type_ref(Context *cnt);

static Ast *
parse_type_arr(Context *cnt);

static Ast *
parse_type_slice(Context *cnt);

static Ast *
parse_type_fn(Context *cnt, bool named_args);

static Ast *
parse_type_struct(Context *cnt);

static Ast *
parse_type_enum(Context *cnt);

static Ast *
parse_type_ptr(Context *cnt);

static Ast *
parse_type_vargs(Context *cnt);

static Ast *
parse_stmt_return(Context *cnt);

static Ast *
parse_stmt_if(Context *cnt);

static Ast *
parse_stmt_loop(Context *cnt);

static Ast *
parse_stmt_break(Context *cnt);

static Ast *
parse_stmt_continue(Context *cnt);

/* EXPRESSIONS */
static Ast *
parse_expr(Context *cnt);

static Ast *
_parse_expr(Context *cnt, int32_t p);

static Ast *
parse_expr_atom(Context *cnt);

static Ast *
parse_expr_primary(Context *cnt);

static Ast *
parse_expr_unary(Context *cnt);

static Ast *
parse_expr_binary(Context *cnt, Ast *lhs, Ast *rhs, Token *op);

static Ast *
parse_expr_addrof(Context *cnt);

static Ast *
parse_expr_deref(Context *cnt);

static Ast *
parse_expr_type(Context *cnt);

static Ast *
parse_expr_ref(Context *cnt);

static Ast *
parse_expr_nested(Context *cnt);

static Ast *
parse_expr_null(Context *cnt);

static Ast *
parse_expr_cast(Context *cnt);

static Ast *
parse_expr_lit(Context *cnt);

static Ast *
parse_expr_lit_fn(Context *cnt);

static Ast *
parse_expr_sizeof(Context *cnt);

static Ast *
parse_expr_type_info(Context *cnt);

static Ast *
parse_expr_alignof(Context *cnt);

static inline bool
parse_semicolon(Context *cnt);

static inline bool
parse_semicolon_rq(Context *cnt);

static Ast *
parse_expr_member(Context *cnt, Ast *prev);

static Ast *
parse_expr_call(Context *cnt, Ast *prev);

static Ast *
parse_expr_elem(Context *cnt, Ast *prev);

static Ast *
parse_expr_compound(Context *cnt);

// impl
BinopKind
sym_to_binop_kind(Sym sm)
{
	switch (sm) {
	case SYM_ASSIGN:
		return BINOP_ASSIGN;
	case SYM_PLUS_ASSIGN:
		return BINOP_ADD_ASSIGN;
	case SYM_MINUS_ASSIGN:
		return BINOP_SUB_ASSIGN;
	case SYM_ASTERISK_ASSIGN:
		return BINOP_MUL_ASSIGN;
	case SYM_SLASH_ASSIGN:
		return BINOP_DIV_ASSIGN;
	case SYM_PERCENT_ASSIGN:
		return BINOP_MOD_ASSIGN;
	case SYM_PLUS:
		return BINOP_ADD;
	case SYM_MINUS:
		return BINOP_SUB;
	case SYM_ASTERISK:
		return BINOP_MUL;
	case SYM_SLASH:
		return BINOP_DIV;
	case SYM_PERCENT:
		return BINOP_MOD;
	case SYM_EQ:
		return BINOP_EQ;
	case SYM_NEQ:
		return BINOP_NEQ;
	case SYM_GREATER:
		return BINOP_GREATER;
	case SYM_LESS:
		return BINOP_LESS;
	case SYM_GREATER_EQ:
		return BINOP_GREATER_EQ;
	case SYM_LESS_EQ:
		return BINOP_LESS_EQ;
	case SYM_LOGIC_AND:
		return BINOP_LOGIC_AND;
	case SYM_LOGIC_OR:
		return BINOP_LOGIC_OR;
	case SYM_AND:
		return BINOP_AND;
	case SYM_OR:
		return BINOP_OR;
	case SYM_SHR:
		return BINOP_SHR;
	case SYM_SHL:
		return BINOP_SHL;
	default:
		bl_abort("unknown binop operation!!!");
	}
}

UnopKind
sym_to_unop_kind(Sym sm)
{
	switch (sm) {
	case SYM_MINUS:
		return UNOP_NEG;
	case SYM_PLUS:
		return UNOP_POS;
	case SYM_NOT:
		return UNOP_NOT;
	default:
		bl_abort("unknown unop operation!!!");
	}
}

Ast *
parse_expr_ref(Context *cnt)
{
	Token *tok   = tokens_peek(cnt->tokens);
	Ast *  ident = parse_ident(cnt);
	if (!ident) return NULL;

	Ast *ref                 = ast_create_node(cnt->ast_arena, AST_EXPR_REF, tok, cnt->scope);
	ref->data.expr_ref.ident = ident;
	return ref;
}

void
parse_flags_for_curr_decl(Context *cnt)
{
	uint32_t flags = 0;

	HashDirective found = HD_NONE;

	const bool is_curr_decl_valid = cnt->curr_decl && cnt->curr_decl->kind == AST_DECL_ENTITY;

	/* flags are accepted only for named declarations */
	uint32_t accepted = is_curr_decl_valid ? HD_EXTERN | HD_COMPILER : HD_NONE;

	while (true) {
		parse_hash_directive(cnt, accepted, &found);

		if (found == HD_NONE) break;

		if (is_flag(found, HD_EXTERN)) {
			flags |= FLAG_EXTERN;
		} else if (is_flag(found, HD_COMPILER)) {
			flags |= FLAG_COMPILER;
		} else {
			bl_abort("Unexpected flag!!!");
		}

		/* Remove found flag from accepted mask (multiple flags of same type are not
		 * allowed). */
		accepted &= ~found;
	}

	if (is_curr_decl_valid) cnt->curr_decl->data.decl_entity.flags |= flags;
}

/*
 * Try to parse hash directive. List of enabled directives can be set by 'expected_mask',
 * 'satisfied' is optional output set to parsed directive id if there is one.
 *
 * <#><load|link|test|extern|compiler>
 */
Ast *
parse_hash_directive(Context *cnt, int32_t expected_mask, HashDirective *satisfied)
{
#define set_satisfied(_hd)                                                                         \
	{                                                                                          \
		if (satisfied) *satisfied = _hd;                                                   \
	}

	set_satisfied(HD_NONE);

	Token *tok_hash = tokens_consume_if(cnt->tokens, SYM_HASH);
	if (!tok_hash) return NULL;

	Token *tok_directive = tokens_consume(cnt->tokens);
	switch (tok_directive->sym) {

	case SYM_LOAD: {
		/* load <string> */
		set_satisfied(HD_LOAD);
		if (is_not_flag(expected_mask, HD_LOAD)) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		Token *tok_path = tokens_consume(cnt->tokens);
		if (!token_is(tok_path, SYM_STRING)) {
			parse_error(cnt,
			            ERR_INVALID_DIRECTIVE,
			            tok_path,
			            BUILDER_CUR_WORD,
			            "Expected path \"some/path\" after 'load' directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		Ast *load = ast_create_node(cnt->ast_arena, AST_LOAD, tok_directive, cnt->scope);
		load->data.load.filepath = tok_path->value.str;

		Unit *unit = unit_new_file(load->data.load.filepath, tok_path, cnt->unit);
		if (!assembly_add_unit_unique(cnt->assembly, unit)) {
			unit_delete(unit);
		}

		return load;
	}

	case SYM_LINK: {
		/* link <string> */
		set_satisfied(HD_LINK);
		if (is_not_flag(expected_mask, HD_LINK)) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		Token *tok_path = tokens_consume(cnt->tokens);
		if (!token_is(tok_path, SYM_STRING)) {
			parse_error(cnt,
			            ERR_INVALID_DIRECTIVE,
			            tok_path,
			            BUILDER_CUR_WORD,
			            "Expected path \"some/path\" after 'link' directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		Ast *link = ast_create_node(cnt->ast_arena, AST_LINK, tok_directive, cnt->scope);
		link->data.link.lib = tok_path->value.str;

		assembly_add_link(cnt->assembly, tok_path);

		return link;
	}

	case SYM_TEST: {
		/* test <string> {} */
		set_satisfied(HD_TEST);

		if (is_not_flag(expected_mask, HD_TEST)) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		Token *tok_desc = tokens_consume(cnt->tokens);
		if (tok_desc->sym != SYM_STRING) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		Ast *block = parse_block(cnt);
		if (!block) {
			parse_error(cnt,
			            ERR_INVALID_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_AFTER,
			            "Expected body of the test case '{...}'.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		parse_semicolon_rq(cnt);

		Ast *test =
		    ast_create_node(cnt->ast_arena, AST_TEST_CASE, tok_directive, cnt->scope);
		test->data.test_case.desc  = tok_desc->value.str;
		test->data.test_case.block = block;

		return test;
	}

	case SYM_EXTERN: {
		set_satisfied(HD_EXTERN);
		if (is_not_flag(expected_mask, HD_EXTERN)) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		return NULL;
	}

	case SYM_COMPILER: {
		set_satisfied(HD_COMPILER);
		if (is_not_flag(expected_mask, HD_COMPILER)) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		return NULL;
	}

	case SYM_PRIVATE: {
		set_satisfied(HD_PRIVATE);

		if (is_not_flag(expected_mask, HD_PRIVATE)) {
			parse_error(cnt,
			            ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		if (cnt->inside_private_scope) {
			parse_error(
			    cnt,
			    ERR_UNEXPECTED_DIRECTIVE,
			    tok_directive,
			    BUILDER_CUR_WORD,
			    "Unexpected directive. File already contains private scope block.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);
		}

		cnt->inside_private_scope = true;

		/*
		 * Here we create private scope for the current unit. (only when source file
		 * contains private block).
		 *
		 * Parent of this scope is a global-scope.
		 *
		 * This scope has also highest priority during symbol lookup inside the current unit
		 * and it is visible only from such unit.
		 * Private scope contains only global entity declarations with 'private' flag set
		 * in AST node.
		 */
		Scope *scope = scope_create(&cnt->builder->scope_arenas,
		                            SCOPE_PRIVATE,
		                            cnt->assembly->gscope,
		                            EXPECTED_PRIVATE_SCOPE_COUNT,
		                            &tok_directive->location);

		/* Make all other declarations in file nested in private scope */
		cnt->unit->private_scope = scope;
		cnt->scope               = scope;

		return ast_create_node(cnt->ast_arena, AST_PRIVATE, tok_directive, cnt->scope);
	}

	default:
		break;
	}

	parse_error(
	    cnt, ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unknown directive.");
	return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, cnt->scope);

#undef set_satisfied
}

/*
 * Compound expression syntax:
 *
 * {:<type>: <value>, ... }
 */
Ast *
parse_expr_compound(Context *cnt)
{
	if (!tokens_is_seq(cnt->tokens, 2, SYM_LBLOCK, SYM_COLON)) return NULL;
	/* eat { */
	Token *tok_begin = tokens_consume(cnt->tokens);
	/* eat : */
	tokens_consume(cnt->tokens);

	Ast *type = parse_type(cnt);
	if (!type) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected type.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	/* eat : */
	if (!tokens_consume_if(cnt->tokens, SYM_COLON)) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_TYPE,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Expected colon after type.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	Ast *compound = ast_create_node(cnt->ast_arena, AST_EXPR_COMPOUND, tok_begin, cnt->scope);
	compound->data.expr_compound.type = type;

	/* parse values */
	bool rq = false;
	Ast *tmp;

value:
	tmp = parse_expr(cnt);
	if (tmp) {
		if (!compound->data.expr_compound.values)
			compound->data.expr_compound.values = bo_array_new(sizeof(Ast *));
		bo_array_push_back(compound->data.expr_compound.values, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto value;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			parse_error(cnt,
			            ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected expression after comma ','.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
		}
	}

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBLOCK) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected end of initialization list '}' or another expression "
		            "separated by comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	return compound;
}

Ast *
parse_expr_sizeof(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_SIZEOF);
	if (!tok_begin) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_LPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after sizeof operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	Ast *szof = ast_create_node(cnt->ast_arena, AST_EXPR_SIZEOF, tok_begin, cnt->scope);
	szof->data.expr_sizeof.node = parse_expr(cnt);
	if (!szof->data.expr_sizeof.node) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(
		    cnt, ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after sizeof operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	return szof;
}

Ast *
parse_expr_type_info(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_TYPEINFO);
	if (!tok_begin) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_LPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after typeinfo operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	Ast *info = ast_create_node(cnt->ast_arena, AST_EXPR_TYPE_INFO, tok_begin, cnt->scope);
	info->data.expr_type_info.node = parse_expr(cnt);
	if (!info->data.expr_type_info.node) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(
		    cnt, ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after typeinfo operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	return info;
}

Ast *
parse_expr_alignof(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ALIGNOF);
	if (!tok_begin) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_LPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after cast operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	Ast *alof = ast_create_node(cnt->ast_arena, AST_EXPR_ALIGNOF, tok_begin, cnt->scope);
	alof->data.expr_alignof.node = parse_expr(cnt);
	if (!alof->data.expr_alignof.node) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(
		    cnt, ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after alignof operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	return alof;
}

Ast *
parse_expr_cast(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST);
	if (!tok_begin) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_LPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	Ast *cast = ast_create_node(cnt->ast_arena, AST_EXPR_CAST, tok_begin, cnt->scope);
	cast->data.expr_cast.type = parse_type(cnt);
	if (!cast->data.expr_cast.type) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_TYPE,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Expected type name as cast parameter.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after cast expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	cast->data.expr_cast.next = _parse_expr(cnt, token_prec(tok_begin).priority);
	if (!cast->data.expr_cast.next) {
		tok = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_EXPR,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after cast.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	return cast;
}

Ast *
parse_decl_member(Context *cnt, bool type_only)
{
	Token *tok_begin = tokens_peek(cnt->tokens);
	Ast *  name      = NULL;
	Ast *  type      = NULL;

	if (type_only) {
		type = parse_type(cnt);
	} else {
		name = parse_ident(cnt);
		if (name && !tokens_consume_if(cnt->tokens, SYM_COLON)) {
			builder_msg(cnt->builder,
			            BUILDER_MSG_ERROR,
			            ERR_EXPECTED_TYPE,
			            name->location,
			            BUILDER_CUR_AFTER,
			            "expected colon after struct member name");
		}
		type = parse_type(cnt);
	}

	if (!type && !name) return NULL;
	Ast *mem = ast_create_node(cnt->ast_arena, AST_DECL_MEMBER, tok_begin, cnt->scope);
	mem->data.decl.type = type;
	mem->data.decl.name = name;

	return mem;
}

Ast *
parse_decl_arg(Context *cnt, bool type_only)
{
	Token *tok_begin = tokens_peek(cnt->tokens);
	Ast *  name      = NULL;
	Ast *  type      = NULL;

	if (type_only) {
		type = parse_type(cnt);
	} else {
		name = parse_ident(cnt);
		if (name && !tokens_consume_if(cnt->tokens, SYM_COLON)) {
			builder_msg(cnt->builder,
			            BUILDER_MSG_ERROR,
			            ERR_EXPECTED_TYPE,
			            name->location,
			            BUILDER_CUR_AFTER,
			            "expected colon after argument name");
		}
		type = parse_type(cnt);
	}

	if (!type && !name) return NULL;
	Ast *arg            = ast_create_node(cnt->ast_arena, AST_DECL_ARG, tok_begin, cnt->scope);
	arg->data.decl.type = type;
	arg->data.decl.name = name;
	return arg;
}

Ast *
parse_decl_variant(Context *cnt, Ast *prev)
{
	Token *tok_begin = tokens_peek(cnt->tokens);
	Ast *  name      = parse_ident(cnt);
	if (!name) return NULL;

	Ast *var = ast_create_node(cnt->ast_arena, AST_DECL_VARIANT, tok_begin, cnt->scope);

	/* TODO: Validate correcly '::' */
	/* TODO: Automatic values set in MIR later??? */
	Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_COLON);
	tok_assign        = tokens_consume_if(cnt->tokens, SYM_COLON);
	if (tok_assign) {
		var->data.decl_variant.value = parse_expr(cnt);
		if (!var->data.decl_variant.value) bl_abort("Expected enum variant value");
	} else if (prev) {
		assert(prev->kind == AST_DECL_VARIANT);
		Ast *addition = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, NULL, cnt->scope);
		addition->data.expr_integer.val = 1;

		Ast *binop = ast_create_node(cnt->ast_arena, AST_EXPR_BINOP, NULL, cnt->scope);
		binop->data.expr_binop.kind = BINOP_ADD;
		binop->data.expr_binop.lhs  = prev->data.decl_variant.value;
		binop->data.expr_binop.rhs  = addition;

		var->data.decl_variant.value = binop;
	} else {
		/* first variant is allways 0 */
		var->data.decl_variant.value =
		    ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, NULL, cnt->scope);
		var->data.decl_variant.value->data.expr_integer.val = 0;
	}

	assert(var->data.decl_variant.value);
	var->data.decl.name = name;
	return var;
}

bool
parse_semicolon(Context *cnt)
{
	return (bool)tokens_consume_if(cnt->tokens, SYM_SEMICOLON);
}

bool
parse_semicolon_rq(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_SEMICOLON);
	if (!tok) {
		tok = tokens_peek_prev(cnt->tokens);
		parse_error(
		    cnt, ERR_MISSING_SEMICOLON, tok, BUILDER_CUR_AFTER, "Missing semicolon ';'.");
		return false;
	}
	return true;
}

Ast *
parse_stmt_return(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_RETURN);
	if (!tok_begin) return NULL;

	Ast *ret = ast_create_node(cnt->ast_arena, AST_STMT_RETURN, tok_begin, cnt->scope);
	assert(cnt->curr_decl);
	ret->data.stmt_return.fn_decl = cnt->curr_decl;
	ret->data.stmt_return.expr    = parse_expr(cnt);
	return ret;
}

Ast *
parse_stmt_if(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_IF);
	if (!tok_begin) return NULL;

	Ast *stmt_if = ast_create_node(cnt->ast_arena, AST_STMT_IF, tok_begin, cnt->scope);

	stmt_if->data.stmt_if.test = parse_expr(cnt);
	if (!stmt_if->data.stmt_if.test) {
		Token *tok_err = tokens_consume(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_EXPR,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Expected expression for the if statement.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	if (stmt_if->data.stmt_if.test->kind == AST_BAD) {
		tokens_consume_till(cnt->tokens, SYM_LBLOCK);
	}

	stmt_if->data.stmt_if.true_stmt = parse_block(cnt);
	if (!stmt_if->data.stmt_if.true_stmt) {
		Token *tok_err = tokens_consume(cnt->tokens);
		parse_error(
		    cnt,
		    ERR_EXPECTED_STMT,
		    tok_err,
		    BUILDER_CUR_WORD,
		    "Expected compound statement for true result of the if expression test.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	stmt_if->data.stmt_if.false_stmt = NULL;
	if (tokens_consume_if(cnt->tokens, SYM_ELSE)) {
		stmt_if->data.stmt_if.false_stmt = parse_stmt_if(cnt);
		if (!stmt_if->data.stmt_if.false_stmt)
			stmt_if->data.stmt_if.false_stmt = parse_block(cnt);
		if (!stmt_if->data.stmt_if.false_stmt) {
			Token *tok_err = tokens_consume(cnt->tokens);
			parse_error(
			    cnt,
			    ERR_EXPECTED_STMT,
			    tok_err,
			    BUILDER_CUR_WORD,
			    "Expected statement for false result of the if expression test.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
		}
	}

	return stmt_if;
}

static TokensLookaheadState
cmp_stmt_loop(Token *curr)
{
	if (token_is(curr, SYM_SEMICOLON))
		return TOK_LOOK_HIT;
	else if (token_is(curr, SYM_RPAREN))
		return TOK_LOOK_TERMINAL;
	else if (token_is(curr, SYM_LBLOCK))
		return TOK_LOOK_TERMINAL;

	return TOK_LOOK_CONTINUE;
}

Ast *
parse_stmt_loop(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LOOP);
	if (!tok_begin) return NULL;

	const bool while_true = tokens_current_is(cnt->tokens, SYM_LBLOCK);

	Ast *loop = ast_create_node(cnt->ast_arena, AST_STMT_LOOP, tok_begin, cnt->scope);
	push_inloop(cnt);

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_DEFAULT, cnt->scope, 8, &tok_begin->location);
	push_scope(cnt, scope);

	if (!while_true) {
		if (tokens_lookahead(cnt->tokens, cmp_stmt_loop)) {
			/* for loop construct loop [init]; [condition]; [increment] {} */
			loop->data.stmt_loop.init = parse_decl(cnt);
			if (!parse_semicolon_rq(cnt)) {
				assert(false);
			}

			loop->data.stmt_loop.condition = parse_expr(cnt);
			if (!parse_semicolon_rq(cnt)) {
				assert(false);
			}

			loop->data.stmt_loop.increment = parse_expr(cnt);
		} else {
			/* while construct with optional condition */
			loop->data.stmt_loop.condition = parse_expr(cnt);
		}
	}

	/* block */
	loop->data.stmt_loop.block = parse_block(cnt);
	if (!loop->data.stmt_loop.block) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(
		    cnt, ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "Expected loop body block.");
		pop_inloop(cnt);
		pop_scope(cnt);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, cnt->scope);
	}

	pop_inloop(cnt);
	pop_scope(cnt);
	return loop;
}

Ast *
parse_stmt_break(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_BREAK);
	if (!tok) return NULL;

	if (!cnt->inside_loop) {
		parse_error(cnt,
		            ERR_BREAK_OUTSIDE_LOOP,
		            tok,
		            BUILDER_CUR_WORD,
		            "Break statement outside a loop.");
	}
	return ast_create_node(cnt->ast_arena, AST_STMT_BREAK, tok, cnt->scope);
}

Ast *
parse_stmt_continue(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_CONTINUE);
	if (!tok) return NULL;

	if (!cnt->inside_loop) {
		parse_error(cnt,
		            ERR_CONTINUE_OUTSIDE_LOOP,
		            tok,
		            BUILDER_CUR_WORD,
		            "Continue statement outside a loop.");
	}

	return ast_create_node(cnt->ast_arena, AST_STMT_CONTINUE, tok, cnt->scope);
}

Ast *
parse_expr(Context *cnt)
{
	return _parse_expr(cnt, 0);
}

Ast *
_parse_expr(Context *cnt, int32_t p)
{
	Ast *lhs = parse_expr_atom(cnt);
	Ast *tmp = NULL;

	do {
		tmp = parse_expr_call(cnt, lhs);
		if (!tmp) tmp = parse_expr_elem(cnt, lhs);
		if (!tmp) tmp = parse_expr_member(cnt, lhs);
		lhs = tmp ? tmp : lhs;
	} while (tmp);

	while (token_is_binop(tokens_peek(cnt->tokens)) &&
	       token_prec(tokens_peek(cnt->tokens)).priority >= p) {
		Token *op = tokens_consume(cnt->tokens);

		const int32_t q = token_prec(op).associativity == TOKEN_ASSOC_LEFT
		                      ? token_prec(op).priority + 1
		                      : token_prec(op).priority;

		Ast *rhs = _parse_expr(cnt, q);
		if (!lhs || !rhs) {
			parse_error(cnt,
			            ERR_INVALID_EXPR,
			            op,
			            BUILDER_CUR_WORD,
			            "Invalid binary operation.");
		}

		lhs = parse_expr_binary(cnt, lhs, rhs, op);
	}

	return lhs;
}

/*
 * Primary expression parser
 *
 * ( expression )
 * <null>
 * <#run>
 * <identifier>
 * <fn () {}>
 * <type>
 * <literal>
 */
Ast *
parse_expr_primary(Context *cnt)
{
	Ast *expr = NULL;
	if ((expr = parse_expr_nested(cnt))) return expr;
	if ((expr = parse_expr_ref(cnt))) return expr;
	if ((expr = parse_expr_lit(cnt))) return expr;
	if ((expr = parse_expr_lit_fn(cnt))) return expr;
	if ((expr = parse_expr_type(cnt))) return expr;
	if ((expr = parse_expr_null(cnt))) return expr;
	if ((expr = parse_expr_compound(cnt))) return expr;

	return NULL;
}

/* <unary operator> <expression> */
Ast *
parse_expr_unary(Context *cnt)
{
	Token *op = tokens_peek(cnt->tokens);
	if (!token_is_unary(op)) return NULL;

	tokens_consume(cnt->tokens);
	Ast *unary = ast_create_node(cnt->ast_arena, AST_EXPR_UNARY, op, cnt->scope);
	unary->data.expr_unary.next = _parse_expr(cnt, token_prec(op).priority);
	unary->data.expr_unary.kind = sym_to_unop_kind(op->sym);

	if (unary->data.expr_unary.next == NULL) {
		Token *err_tok = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_EXPR,
		            err_tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after unary operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, op, cnt->scope);
	}

	if (unary->data.expr_unary.next->kind == AST_BAD) return unary->data.expr_unary.next;

	return unary;
}

Ast *
parse_expr_atom(Context *cnt)
{
	Ast *expr = NULL;

	if ((expr = parse_expr_primary(cnt))) return expr;
	if ((expr = parse_expr_unary(cnt))) return expr;
	if ((expr = parse_expr_deref(cnt))) return expr;
	if ((expr = parse_expr_addrof(cnt))) return expr;
	if ((expr = parse_expr_cast(cnt))) return expr;
	if ((expr = parse_expr_sizeof(cnt))) return expr;
	if ((expr = parse_expr_alignof(cnt))) return expr;
	if ((expr = parse_expr_type_info(cnt))) return expr;

	return NULL;
}

/* <expression> <binary operator> <expression>*/
Ast *
parse_expr_binary(Context *cnt, Ast *lhs, Ast *rhs, Token *op)
{
	if (!token_is_binop(op)) return NULL;

	Ast *binop = ast_create_node(cnt->ast_arena, AST_EXPR_BINOP, op, cnt->scope);
	binop->data.expr_binop.kind = sym_to_binop_kind(op->sym);
	binop->data.expr_binop.lhs  = lhs;
	binop->data.expr_binop.rhs  = rhs;

	return binop;
}

Ast *
parse_expr_addrof(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_AND);
	if (!tok) return NULL;

	Ast *addrof = ast_create_node(cnt->ast_arena, AST_EXPR_ADDROF, tok, cnt->scope);
	addrof->data.expr_addrof.next = _parse_expr(cnt, token_prec(tok).priority);

	if (addrof->data.expr_addrof.next == NULL) {
		Token *err_tok = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_EXPR,
		            err_tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after '&' operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	if (addrof->data.expr_addrof.next->kind == AST_BAD) return addrof->data.expr_addrof.next;
	return addrof;
}

Ast *
parse_expr_deref(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_CARET);
	if (!tok) return NULL;

	Ast *deref = ast_create_node(cnt->ast_arena, AST_EXPR_DEREF, tok, cnt->scope);
	deref->data.expr_deref.next = _parse_expr(cnt, token_prec(tok).priority);

	if (deref->data.expr_deref.next == NULL) {
		Token *err_tok = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_EXPR,
		            err_tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after '^' operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	if (deref->data.expr_deref.next->kind == AST_BAD) return deref->data.expr_deref.next;
	return deref;
}

Ast *
parse_expr_lit(Context *cnt)
{
	Token *tok = tokens_peek(cnt->tokens);
	Ast *  lit = NULL;

	switch (tok->sym) {
	case SYM_NUM:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, tok, cnt->scope);
		lit->data.expr_integer.val = tok->value.u;
		break;

	case SYM_CHAR:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_CHAR, tok, cnt->scope);
		lit->data.expr_character.val = (uint8_t)tok->value.c;

		break;

	case SYM_STRING:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_STRING, tok, cnt->scope);
		lit->data.expr_string.val = tok->value.str;
		break;

	case SYM_TRUE:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, cnt->scope);
		lit->data.expr_boolean.val = true;
		break;

	case SYM_FALSE:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, cnt->scope);
		lit->data.expr_boolean.val = false;
		break;

	case SYM_DOUBLE:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_DOUBLE, tok, cnt->scope);
		lit->data.expr_double.val = tok->value.d;
		break;

	case SYM_FLOAT:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FLOAT, tok, cnt->scope);
		lit->data.expr_float.val = (float)tok->value.d;
		break;

	default:
		return NULL;
	}

	tokens_consume(cnt->tokens);
	return lit;
}

Ast *
parse_expr_lit_fn(Context *cnt)
{
	Token *tok_fn = tokens_peek(cnt->tokens);
	if (token_is_not(tok_fn, SYM_FN)) return NULL;

	Ast *fn = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FN, tok_fn, cnt->scope);

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_DEFAULT, cnt->scope, 32, &tok_fn->location);
	push_scope(cnt, scope);

	Ast *type = parse_type_fn(cnt, true);
	assert(type);

	fn->data.expr_fn.type = type;

	/* parse flags */
	parse_flags_for_curr_decl(cnt);

	/* parse block (block is optional function body can be external) */
	fn->data.expr_fn.block = parse_block(cnt);

	pop_scope(cnt);
	return fn;
}

/* ( expression ) */
Ast *
parse_expr_nested(Context *cnt)
{
	Ast *  expr      = NULL;
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LPAREN);
	if (!tok_begin) return NULL;

	expr = parse_expr(cnt);
	if (expr == NULL) {
		parse_error(
		    cnt, ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_WORD, "Expected expression.");
	}

	/* eat ) */
	Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RPAREN);
	if (!tok_end) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Unterminated sub-expression, missing ')'.");
		parse_note(cnt, tok_begin, BUILDER_CUR_WORD, "starting here");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	return expr;
}

/* <expression>.<identifier> */
Ast *
parse_expr_member(Context *cnt, Ast *prev)
{
	if (!prev) return NULL;
	Token *tok = tokens_consume_if(cnt->tokens, SYM_DOT);
	if (!tok) return NULL;

	Ast *ident = parse_ident(cnt);
	if (!ident) {
		Token *tok_err = tokens_peek(cnt->tokens);
		parse_error(
		    cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected member name.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	Ast *mem = ast_create_node(cnt->ast_arena, AST_EXPR_MEMBER, tok, cnt->scope);
	mem->data.expr_member.ident = ident;
	mem->data.expr_member.next  = prev;
	mem->data.expr_member.i     = -1;
	return mem;
}

/* <expression>[<index>] */
Ast *
parse_expr_elem(Context *cnt, Ast *prev)
{
	if (!prev) return NULL;
	Token *tok_elem = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
	if (!tok_elem) return NULL;

	Ast *elem = ast_create_node(cnt->ast_arena, AST_EXPR_ELEM, tok_elem, cnt->scope);
	elem->data.expr_elem.index = parse_expr(cnt);
	elem->data.expr_elem.next  = prev;

	if (!elem->data.expr_elem.index) {
		parse_error(cnt,
		            ERR_EXPECTED_EXPR,
		            tok_elem,
		            BUILDER_CUR_WORD,
		            "Expected array index expression.");
	}

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBRACKET) {
		parse_error(
		    cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Missing bracket ']'.");
	}

	return elem;
}

Ast *
parse_ident(Context *cnt)
{
	Token *tok_ident = tokens_consume_if(cnt->tokens, SYM_IDENT);
	if (!tok_ident) return NULL;

	assert(cnt->scope);

	Ast *ident = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ident, cnt->scope);
	id_init(&ident->data.ident.id, tok_ident->value.str);

	return ident;
}

Ast *
parse_type_ptr(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ASTERISK);
	if (!tok_begin) return NULL;

	Ast *ptr = ast_create_node(cnt->ast_arena, AST_TYPE_PTR, tok_begin, cnt->scope);
	ptr->data.type_ptr.type = parse_type(cnt);
	assert(ptr->data.type_ptr.type);
	return ptr;
}

Ast *
parse_type_vargs(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_VARGS);
	if (!tok_begin) return NULL;

	Ast *ptr = ast_create_node(cnt->ast_arena, AST_TYPE_VARGS, tok_begin, cnt->scope);
	ptr->data.type_ptr.type = parse_type(cnt);
	return ptr;
}

Ast *
parse_type_enum(Context *cnt)
{
	Token *tok_enum = tokens_consume_if(cnt->tokens, SYM_ENUM);
	if (!tok_enum) return NULL;

	Ast *enm = ast_create_node(cnt->ast_arena, AST_TYPE_ENUM, tok_enum, cnt->scope);
	enm->data.type_enm.variants = bo_array_new(sizeof(Ast *));
	enm->data.type_enm.type     = parse_type(cnt);

	parse_flags_for_curr_decl(cnt);

	Token *tok = tokens_consume(cnt->tokens);
	if (token_is_not(tok, SYM_LBLOCK)) {
		parse_error(
		    cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected enum variant list.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_DEFAULT, cnt->scope, 512, &tok->location);
	enm->data.type_enm.scope = scope;
	push_scope(cnt, scope);

	/* parse enum varinats */
	bool rq = false;
	Ast *tmp;
	Ast *prev_tmp = NULL;

NEXT:
	tmp = parse_decl_variant(cnt, prev_tmp);
	if (tmp) {
		prev_tmp = tmp;
		bo_array_push_back(enm->data.type_enm.variants, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			parse_error(cnt,
			            ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected variant after comma ','.");
			pop_scope(cnt);
			return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBLOCK) {
		parse_error(
		    cnt,
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of variant list '}' or another variant separated by comma.");
		pop_scope(cnt);
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	pop_scope(cnt);
	return enm;
}

Ast *
parse_type_ref(Context *cnt)
{
	Token *tok   = tokens_peek(cnt->tokens);
	Ast *  ident = parse_ident(cnt);
	if (!ident) return NULL;

	Ast *type_ref = ast_create_node(cnt->ast_arena, AST_TYPE_REF, tok, cnt->scope);
	type_ref->data.type_ref.ident = ident;
	return type_ref;
}

Ast *
parse_type_arr(Context *cnt)
{
	/* slice or array??? */
	if (tokens_peek(cnt->tokens)->sym == SYM_LBRACKET &&
	    tokens_peek_2nd(cnt->tokens)->sym == SYM_RBRACKET)
		return NULL;

	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
	if (!tok_begin) return NULL;

	Ast *arr = ast_create_node(cnt->ast_arena, AST_TYPE_ARR, tok_begin, cnt->scope);
	arr->data.type_arr.len = parse_expr(cnt);
	assert(arr->data.type_arr.len);

	Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
	if (!tok_end) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok_end,
		            BUILDER_CUR_WORD,
		            "Expected ']' after array size expression.");
	}

	arr->data.type_arr.elem_type = parse_type(cnt);
	assert(arr->data.type_arr.elem_type);
	return arr;
}

Ast *
parse_type_slice(Context *cnt)
{
	/* slice or array??? []<type> */
	if (tokens_peek(cnt->tokens)->sym != SYM_LBRACKET) return NULL;
	if (tokens_peek_2nd(cnt->tokens)->sym != SYM_RBRACKET) return NULL;

	/* eat [] */
	Token *tok_begin = tokens_consume(cnt->tokens);
	tok_begin        = tokens_consume(cnt->tokens);

	Ast *slice = ast_create_node(cnt->ast_arena, AST_TYPE_SLICE, tok_begin, cnt->scope);

	slice->data.type_slice.elem_type = parse_type(cnt);

	if (!slice->data.type_slice.elem_type) {
		parse_error(cnt,
		            ERR_INVALID_TYPE,
		            tok_begin,
		            BUILDER_CUR_AFTER,
		            "Expected slice type after '[]'.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	return slice;
}

Ast *
parse_type(Context *cnt)
{
	Ast *type = NULL;

	type = parse_type_ptr(cnt);
	if (!type) type = parse_type_fn(cnt, false);
	if (!type) type = parse_type_struct(cnt);
	if (!type) type = parse_type_enum(cnt);
	if (!type) type = parse_type_vargs(cnt);
	if (!type) type = parse_type_arr(cnt);
	if (!type) type = parse_type_slice(cnt);
	if (!type) type = parse_type_ref(cnt);

	return type;
}

Ast *
parse_type_fn(Context *cnt, bool named_args)
{
	Token *tok_fn = tokens_consume_if(cnt->tokens, SYM_FN);
	if (!tok_fn) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_LPAREN) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected function parameter list.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, cnt->scope);
	}

	Ast *fn = ast_create_node(cnt->ast_arena, AST_TYPE_FN, tok_fn, cnt->scope);

	/* parse arg types */
	bool rq = false;
	Ast *tmp;

NEXT:
	tmp = parse_decl_arg(cnt, !named_args);
	if (tmp) {
		if (!fn->data.type_fn.args) fn->data.type_fn.args = bo_array_new(sizeof(Ast *));
		bo_array_push_back(fn->data.type_fn.args, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			parse_error(cnt,
			            ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected type after comma ','.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, cnt->scope);
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RPAREN) {
		parse_error(
		    cnt,
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of argument type list ')' or another type separated by comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, cnt->scope);
	}

	fn->data.type_fn.ret_type = parse_type(cnt);
	return fn;
}

Ast *
parse_type_struct(Context *cnt)
{
	Token *tok_struct = tokens_consume_if(cnt->tokens, SYM_STRUCT);
	if (!tok_struct) return NULL;

	parse_flags_for_curr_decl(cnt);

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_LBLOCK) {
		parse_error(cnt,
		            ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected struct member list.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, cnt->scope);
	}

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_DEFAULT, cnt->scope, 256, &tok->location);
	push_scope(cnt, scope);

	Ast *type_struct = ast_create_node(cnt->ast_arena, AST_TYPE_STRUCT, tok_struct, cnt->scope);
	type_struct->data.type_strct.scope   = scope;
	type_struct->data.type_strct.raw     = false;
	type_struct->data.type_strct.members = bo_array_new(sizeof(Ast *));

	/* parse members */
	bool       rq = false;
	Ast *      tmp;
	const bool type_only = tokens_peek_2nd(cnt->tokens)->sym == SYM_COMMA ||
	                       tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK;
	type_struct->data.type_strct.raw = type_only;
NEXT:
	tmp = parse_decl_member(cnt, type_only);
	if (tmp) {
		bo_array_push_back(type_struct->data.type_strct.members, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			parse_error(cnt,
			            ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected member after comma ','.");

			pop_scope(cnt);
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, cnt->scope);
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBLOCK) {
		parse_error(
		    cnt,
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of member list '}' or another memeber separated by comma.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		pop_scope(cnt);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, cnt->scope);
	}

	pop_scope(cnt);
	return type_struct;
}

Ast *
parse_decl(Context *cnt)
{
	/* is value declaration? */
	Token *tok_ident = tokens_peek(cnt->tokens);
	if (token_is_not(tok_ident, SYM_IDENT)) return NULL;

	Token *tok_2nd = tokens_peek_2nd(cnt->tokens);
	if (token_is_not(tok_2nd, SYM_COLON)) return NULL;

	Ast *ident = parse_ident(cnt);
	if (!ident) return NULL;

	/* eat : */
	tokens_consume(cnt->tokens);

	Ast *decl = ast_create_node(cnt->ast_arena, AST_DECL_ENTITY, tok_ident, cnt->scope);
	decl->data.decl.name       = ident;
	decl->data.decl_entity.mut = true;

	push_curr_decl(cnt, decl);

	decl->data.decl.type = parse_type(cnt);
	Token *tok_assign    = tokens_consume_if(cnt->tokens, SYM_ASSIGN);
	if (!tok_assign) tok_assign = tokens_consume_if(cnt->tokens, SYM_COLON);

	if (tok_assign) {
		decl->data.decl_entity.mut = token_is(tok_assign, SYM_ASSIGN);

		/* parse declaration expression */
		decl->data.decl_entity.value = parse_expr(cnt);

		if (!(decl->data.decl_entity.flags & (FLAG_EXTERN))) {
			if (!decl->data.decl_entity.value) {
				parse_error(cnt,
				            ERR_EXPECTED_INITIALIZATION,
				            tok_assign,
				            BUILDER_CUR_AFTER,
				            "Expected binding of declaration to some value.");
				pop_curr_decl(cnt);
				return ast_create_node(
				    cnt->ast_arena, AST_BAD, tok_ident, cnt->scope);
			}
		}
	}

	pop_curr_decl(cnt);
	return decl;
}

Ast *
parse_expr_call(Context *cnt, Ast *prev)
{
	if (!prev) return NULL;

	Token *tok = tokens_consume_if(cnt->tokens, SYM_LPAREN);
	if (!tok) return NULL;

	Ast *call                = ast_create_node(cnt->ast_arena, AST_EXPR_CALL, tok, cnt->scope);
	call->data.expr_call.ref = prev;
	call->data.expr_call.run = false;

	/* parse args */
	bool rq = false;
	Ast *tmp;

arg:
	tmp = parse_expr(cnt);
	if (tmp) {
		if (!call->data.expr_call.args)
			call->data.expr_call.args = bo_array_new(sizeof(Ast *));
		bo_array_push_back(call->data.expr_call.args, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto arg;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			parse_error(cnt,
			            ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected function argument after comma ','.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RPAREN) {
		parse_error(
		    cnt,
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of parameter list ')' or another parameter separated by comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, cnt->scope);
	}

	return call;
}

Ast *
parse_expr_null(Context *cnt)
{
	Token *tok_null = tokens_consume_if(cnt->tokens, SYM_NULL);
	if (!tok_null) return NULL;
	return ast_create_node(cnt->ast_arena, AST_EXPR_NULL, tok_null, cnt->scope);
}

Ast *
parse_unrecheable(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_UNREACHABLE);
	if (!tok) return NULL;

	return ast_create_node(cnt->ast_arena, AST_UNREACHABLE, tok, cnt->scope);
}

Ast *
parse_expr_type(Context *cnt)
{
	Token *tok  = tokens_peek(cnt->tokens);
	Ast *  type = NULL;

	type = parse_type_struct(cnt);
	if (!type) type = parse_type_arr(cnt);
	if (!type) type = parse_type_slice(cnt);
	if (!type) type = parse_type_enum(cnt);
	if (!type) type = parse_type_ptr(cnt);

	if (type) {
		Ast *expr = ast_create_node(cnt->ast_arena, AST_EXPR_TYPE, tok, cnt->scope);
		expr->data.expr_type.type = type;
		return expr;
	}

	return NULL;
}

Ast *
parse_block(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
	if (!tok_begin) return NULL;

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_DEFAULT, cnt->scope, 1024, &tok_begin->location);
	push_scope(cnt, scope);

	Ast *block = ast_create_node(cnt->ast_arena, AST_BLOCK, tok_begin, cnt->scope);

	Token *tok;
	Ast *  tmp;
	block->data.block.nodes = bo_array_new(sizeof(Ast *));

NEXT:
	if (tokens_current_is(cnt->tokens, SYM_SEMICOLON)) {
		tok = tokens_consume(cnt->tokens);
		parse_warning(cnt, tok, BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
		goto NEXT;
	}

	parse_hash_directive(cnt, 0, NULL);

	if ((tmp = (Ast *)parse_decl(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_return(cnt))) {
		if ((tmp)->kind != AST_BAD) parse_semicolon_rq(cnt);
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_if(cnt))) {
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_loop(cnt))) {
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_break(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_continue(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_expr(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_block(cnt))) {
		bo_array_push_back(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_unrecheable(cnt))) {
		bo_array_push_back(block->data.block.nodes, tmp);
		parse_semicolon_rq(cnt);
		goto NEXT;
	}

	tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
	if (!tok) {
		tok = tokens_peek_prev(cnt->tokens);
		parse_error(cnt,
		            ERR_EXPECTED_BODY_END,
		            tok,
		            BUILDER_CUR_AFTER,
		            "Expected end of block '}'.");
		parse_note(cnt, tok_begin, BUILDER_CUR_WORD, "Block starting here.");
		pop_scope(cnt);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, cnt->scope);
	}

	pop_scope(cnt);
	return block;
}

void
parse_ublock_content(Context *cnt, Ast *ublock)
{
	assert(ublock->kind == AST_UBLOCK);
	ublock->data.ublock.nodes = bo_array_new(sizeof(Ast *));
	Ast *tmp;
NEXT:
	if (parse_semicolon(cnt)) goto NEXT;

	if ((tmp = parse_decl(cnt))) {
		if (tmp->kind != AST_BAD) {
			parse_semicolon_rq(cnt);
			/* setup global scope flag for declaration */
			tmp->data.decl_entity.in_gscope = true;
			if (cnt->inside_private_scope) tmp->data.decl_entity.flags |= FLAG_PRIVATE;
		}

		bo_array_push_back(ublock->data.ublock.nodes, tmp);
		goto NEXT;
	}

	/* load, link, test, private - enabled in global scope */
	const int enabled_hd = HD_LOAD | HD_LINK | HD_TEST | HD_PRIVATE;
	if ((tmp = parse_hash_directive(cnt, enabled_hd, NULL))) {
		bo_array_push_back(ublock->data.ublock.nodes, tmp);
		goto NEXT;
	}

	Token *tok = tokens_peek(cnt->tokens);
	if (!token_is(tok, SYM_EOF)) {
		parse_error(cnt,
		            ERR_UNEXPECTED_SYMBOL,
		            tok,
		            BUILDER_CUR_WORD,
		            "Unexpected symbol in module body '%s'.",
		            sym_strings[tok->sym]);
	}
}

void
parser_run(Builder *builder, Assembly *assembly, Unit *unit)
{
	if (!assembly->gscope) {
		assembly->gscope = scope_create(
		    &builder->scope_arenas, SCOPE_GLOBAL, NULL, EXPECTED_GSCOPE_COUNT, NULL);
	}

	Context cnt = {.builder      = builder,
	               .assembly     = assembly,
	               .scope        = assembly->gscope,
	               .unit         = unit,
	               .ast_arena    = &builder->ast_arena,
	               .scope_arenas = &builder->scope_arenas,
	               .tokens       = &unit->tokens,
	               .curr_decl    = NULL,
	               .inside_loop  = false};

	Ast *root              = ast_create_node(&builder->ast_arena, AST_UBLOCK, NULL, cnt.scope);
	root->data.ublock.unit = unit;
	unit->ast              = root;

	parse_ublock_content(&cnt, unit->ast);
}
