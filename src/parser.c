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
#include "llvm_di.h"
#include "stages.h"
#include <setjmp.h>

TSMALL_ARRAY_TYPE(AstPtr64, Ast *, 64);
TSMALL_ARRAY_TYPE(ScopePtr64, Scope *, 64);

#define EXPECTED_PRIVATE_SCOPE_COUNT 256

#define PARSE_ERROR(kind, tok, pos, format, ...)                                                   \
	{                                                                                          \
		builder_msg(                                                                       \
		    BUILDER_MSG_ERROR, (kind), &(tok)->location, (pos), (format), ##__VA_ARGS__);  \
	}

#define PARSE_WARNING(tok, pos, format, ...)                                                       \
	{                                                                                          \
		builder_msg(                                                                       \
		    BUILDER_MSG_WARNING, 0, &(tok)->location, (pos), (format), ##__VA_ARGS__);     \
	}

#define PARSE_NOTE(tok, pos, format, ...)                                                          \
	{                                                                                          \
		builder_msg(                                                                       \
		    BUILDER_MSG_NOTE, 0, &(tok)->location, (pos), (format), ##__VA_ARGS__);        \
	}

/* swap current compound with _cmp and create temporary variable with previous one */

#define scope_push(_cnt, _scope) tsa_push_ScopePtr64(&(_cnt)->_scope_stack, (_scope))
#define scope_pop(_cnt) tsa_pop_ScopePtr64(&(_cnt)->_scope_stack)
#define scope_get(_cnt) tsa_last_ScopePtr64(&(_cnt)->_scope_stack)
#define scope_set(_cnt, _scope)                                                                    \
	((_cnt)->_scope_stack.data[(_cnt)->_scope_stack.size - 1]) = (_scope)

#define decl_push(_cnt, _decl) tsa_push_AstPtr64(&(_cnt)->_decl_stack, (_decl))
#define decl_pop(_cnt) tsa_pop_AstPtr64(&(_cnt)->_decl_stack)
#define decl_get(_cnt) ((_cnt)->_decl_stack.size ? tsa_last_AstPtr64(&(_cnt)->_decl_stack) : NULL)

typedef enum {
	HD_NONE        = 1 << 0,
	HD_LOAD        = 1 << 1,
	HD_LINK        = 1 << 2,
	HD_TEST        = 1 << 3,
	HD_EXTERN      = 1 << 4,
	HD_COMPILER    = 1 << 5,
	HD_PRIVATE     = 1 << 6,
	HD_INLINE      = 1 << 7,
	HD_NO_INLINE   = 1 << 8,
	HD_FILE        = 1 << 9,
	HD_LINE        = 1 << 10,
	HD_BASE        = 1 << 11,
	HD_META        = 1 << 12,
	HD_ENTRY       = 1 << 12,
	HD_BUILD_ENTRY = 1 << 13,
	HD_TAGS        = 1 << 14,
} HashDirective;

typedef struct {
	TSmallArray_AstPtr64   _decl_stack;
	TSmallArray_ScopePtr64 _scope_stack;
	Assembly *             assembly;
	Unit *                 unit;
	Arena *                ast_arena;
	ScopeArenas *          scope_arenas;
	Tokens *               tokens;

	/* tmps */
	bool inside_loop;
	bool inside_private_scope;
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
parse_hash_directive(Context *cnt, s32 expected_mask, HashDirective *satisfied);

static Ast *
parse_unrecheable(Context *cnt);

static Ast *
parse_ident(Context *cnt);

static Ast *
parse_block(Context *cnt, bool create_scope);

static Ast *
parse_decl(Context *cnt);

static Ast *
parse_decl_member(Context *cnt, bool type_only);

static Ast *
parse_decl_arg(Context *cnt, bool rq_named);

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
parse_type_fn(Context *cnt, bool rq_named_args);

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

static Ast *
parse_stmt_defer(Context *cnt);

static Ast *
parse_stmt_switch(Context *cnt);

static Ast *
parse_stmt_case(Context *cnt);

/* EXPRESSIONS */
static Ast *
parse_expr(Context *cnt);

static Ast *
_parse_expr(Context *cnt, s32 p);

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
parse_expr_cast_auto(Context *cnt);

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

static inline bool
hash_directive_to_flags(HashDirective hd, u32 *out_flags);

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
		BL_ABORT("unknown binop operation!!!");
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
		BL_ABORT("unknown unop operation!!!");
	}
}

Ast *
parse_expr_ref(Context *cnt)
{
	Token *tok   = tokens_peek(cnt->tokens);
	Ast *  ident = parse_ident(cnt);
	if (!ident) return NULL;

	Ast *ref = ast_create_node(cnt->ast_arena, AST_EXPR_REF, tok, scope_get(cnt));
	ref->data.expr_ref.ident = ident;
	return ref;
}

/*
 * Try to parse hash directive. List of enabled directives can be set by 'expected_mask',
 * 'satisfied' is optional output set to parsed directive id if there is one.
 */
Ast *
parse_hash_directive(Context *cnt, s32 expected_mask, HashDirective *satisfied)
{
#define set_satisfied(_hd)                                                                         \
	{                                                                                          \
		if (satisfied) *satisfied = _hd;                                                   \
	}

	set_satisfied(HD_NONE);

	Token *tok_hash = tokens_consume_if(cnt->tokens, SYM_HASH);
	if (!tok_hash) return NULL;

	Token *tok_directive = tokens_consume(cnt->tokens);
	if (tok_directive->sym != SYM_IDENT) goto INVALID;

	const char *directive = tok_directive->value.str;
	BL_ASSERT(directive);

	if (strcmp(directive, "load") == 0) {
		/* load <string> */
		set_satisfied(HD_LOAD);
		if (IS_NOT_FLAG(expected_mask, HD_LOAD)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Token *tok_path = tokens_consume(cnt->tokens);
		if (!token_is(tok_path, SYM_STRING)) {
			PARSE_ERROR(ERR_INVALID_DIRECTIVE,
			            tok_path,
			            BUILDER_CUR_WORD,
			            "Expected path \"some/path\" after 'load' directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Ast *load =
		    ast_create_node(cnt->ast_arena, AST_LOAD, tok_directive, scope_get(cnt));
		load->data.load.filepath = tok_path->value.str;

		Unit *unit = unit_new_file(load->data.load.filepath, tok_path, cnt->unit);
		if (!assembly_add_unit_unique(cnt->assembly, unit)) {
			unit_delete(unit);
		}

		return load;
	}

	if (strcmp(directive, "link") == 0) {
		/* link <string> */
		set_satisfied(HD_LINK);
		if (IS_NOT_FLAG(expected_mask, HD_LINK)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Token *tok_path = tokens_consume(cnt->tokens);
		if (!token_is(tok_path, SYM_STRING)) {
			PARSE_ERROR(ERR_INVALID_DIRECTIVE,
			            tok_path,
			            BUILDER_CUR_WORD,
			            "Expected path \"some/path\" after 'link' directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Ast *link =
		    ast_create_node(cnt->ast_arena, AST_LINK, tok_directive, scope_get(cnt));
		link->data.link.lib = tok_path->value.str;

		assembly_add_native_lib(cnt->assembly, tok_path->value.str, tok_path);

		return link;
	}

	if (strcmp(directive, "test") == 0) {
		/* test <string> {} */
		set_satisfied(HD_TEST);

		if (IS_NOT_FLAG(expected_mask, HD_TEST)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Token *tok_desc = tokens_consume(cnt->tokens);
		if (tok_desc->sym != SYM_STRING) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Scope *scope = scope_create(cnt->scope_arenas, SCOPE_FN, scope_get(cnt), 256, NULL);
		scope_push(cnt, scope);

		Ast *block = parse_block(cnt, false);
		if (!block) {
			PARSE_ERROR(ERR_INVALID_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_AFTER,
			            "Expected body of the test case '{...}'.");
			scope_pop(cnt);
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		scope->location = block->location;

		// parse_semicolon_rq(cnt);

		Ast *test =
		    ast_create_node(cnt->ast_arena, AST_TEST_CASE, tok_directive, scope_get(cnt));
		test->data.test_case.desc  = tok_desc->value.str;
		test->data.test_case.block = block;

		scope_pop(cnt);
		return test;
	}

	if (strcmp(directive, "file") == 0) {
		set_satisfied(HD_FILE);
		if (IS_NOT_FLAG(expected_mask, HD_FILE)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Ast *file =
		    ast_create_node(cnt->ast_arena, AST_EXPR_FILE, tok_directive, scope_get(cnt));

		file->data.expr_file.filename = tok_directive->location.unit->filepath;
		return file;
	}

	if (strcmp(directive, "base") == 0) {
		set_satisfied(HD_BASE);
		if (IS_NOT_FLAG(expected_mask, HD_BASE)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		return parse_type(cnt);
	}

	if (strcmp(directive, "tags") == 0) {
		set_satisfied(HD_TAGS);
		if (IS_NOT_FLAG(expected_mask, HD_TAGS)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		/*
		 * Tags can contain one or mover references separated by comma
		 */
		Ast *tag;
		bool rq = false;

		TSmallArray_AstPtr *values = create_sarr(TSmallArray_AstPtr, cnt->assembly);

	VALUE:
		tag = parse_expr_ref(cnt);
		if (tag) {
			tsa_push_AstPtr(values, tag);

			if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
				rq = true;
				goto VALUE;
			}
		} else if (rq) {
			Token *tok_err = tokens_peek(cnt->tokens);
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected another tag after comma ','.");

			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		if (!values->size) {
			Token *tok_err = tokens_peek(cnt->tokens);
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected tag value after #tags.");

			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Ast *tags =
		    ast_create_node(cnt->ast_arena, AST_TAGS, tok_directive, scope_get(cnt));

		tags->data.tags.values = values;
		return tags;
	}

#if 0
	if (strcmp(directive, "meta") == 0) {
		set_satisfied(HD_META);
		if (IS_NOT_FLAG(expected_mask, HD_META)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Token *tok_str = tokens_consume(cnt->tokens);
		if (tok_str->sym != SYM_STRING) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Expected string description after meta data.");

			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Ast *meta =
		    ast_create_node(cnt->ast_arena, AST_META_DATA, tok_directive, scope_get(cnt));

		meta->data.meta_data.str = tok_str->value.str;
		return meta;
	}
#endif

	if (strcmp(directive, "line") == 0) {
		set_satisfied(HD_LINE);
		if (IS_NOT_FLAG(expected_mask, HD_LINE)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		Ast *line =
		    ast_create_node(cnt->ast_arena, AST_EXPR_LINE, tok_directive, scope_get(cnt));

		line->data.expr_line.line = tok_directive->location.line;
		return line;
	}

	if (strcmp(directive, "entry") == 0) {
		set_satisfied(HD_ENTRY);
		if (IS_NOT_FLAG(expected_mask, HD_ENTRY)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		return NULL;
	}

	if (strcmp(directive, "build_entry") == 0) {
		set_satisfied(HD_BUILD_ENTRY);
		if (IS_NOT_FLAG(expected_mask, HD_BUILD_ENTRY)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		return NULL;
	}

	if (strcmp(directive, "extern") == 0) {
		set_satisfied(HD_EXTERN);
		if (IS_NOT_FLAG(expected_mask, HD_EXTERN)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		/* Extern flag extension could be linkage name as string */
		Token *tok_ext = tokens_consume_if(cnt->tokens, SYM_STRING);
		if (!tok_ext) return NULL;

		/* Parse extension token. */
		Ast *ext = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ext, scope_get(cnt));
		id_init(&ext->data.ident.id, tok_ext->value.str);
		return ext;
	}

	if (strcmp(directive, "compiler") == 0) {
		set_satisfied(HD_COMPILER);
		if (IS_NOT_FLAG(expected_mask, HD_COMPILER)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		return NULL;
	}

	if (strcmp(directive, "inline") == 0) {
		set_satisfied(HD_INLINE);
		if (IS_NOT_FLAG(expected_mask, HD_INLINE)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		return NULL;
	}

	if (strcmp(directive, "no_inline") == 0) {
		set_satisfied(HD_NO_INLINE);
		if (IS_NOT_FLAG(expected_mask, HD_NO_INLINE)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		return NULL;
	}

	if (strcmp(directive, "private") == 0) {
		set_satisfied(HD_PRIVATE);

		if (IS_NOT_FLAG(expected_mask, HD_PRIVATE)) {
			PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
			            tok_directive,
			            BUILDER_CUR_WORD,
			            "Unexpected directive.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
		}

		if (cnt->inside_private_scope) {
			PARSE_ERROR(
			    ERR_UNEXPECTED_DIRECTIVE,
			    tok_directive,
			    BUILDER_CUR_WORD,
			    "Unexpected directive. File already contains private scope block.");
			return ast_create_node(
			    cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
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
		Scope *scope = scope_create(cnt->scope_arenas,
		                            SCOPE_PRIVATE,
		                            cnt->assembly->gscope,
		                            EXPECTED_PRIVATE_SCOPE_COUNT,
		                            &tok_directive->location);

		scope->llvm_di_meta = scope->parent->llvm_di_meta;

		/* Make all other declarations in file nested in private scope */
		cnt->unit->private_scope = scope;
		scope_set(cnt, scope);

		return ast_create_node(cnt->ast_arena, AST_PRIVATE, tok_directive, scope_get(cnt));
	}

INVALID:
	PARSE_ERROR(
	    ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unknown directive.");
	return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, scope_get(cnt));
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
		PARSE_ERROR(ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected type.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	/* eat : */
	if (!tokens_consume_if(cnt->tokens, SYM_COLON)) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(
		    ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected colon after type.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	Ast *compound =
	    ast_create_node(cnt->ast_arena, AST_EXPR_COMPOUND, tok_begin, scope_get(cnt));
	compound->data.expr_compound.type = type;

	/* parse values */
	bool rq = false;
	Ast *tmp;

value:
	tmp = parse_expr(cnt);
	if (tmp) {
		if (!compound->data.expr_compound.values)
			compound->data.expr_compound.values =
			    create_sarr(TSmallArray_AstPtr, cnt->assembly);

		tsa_push_AstPtr(compound->data.expr_compound.values, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto value;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected expression after comma ','.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
		}
	}

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBLOCK) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected end of initialization list '}' or another expression "
		            "separated by comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
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
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after sizeof operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	Ast *szof = ast_create_node(cnt->ast_arena, AST_EXPR_SIZEOF, tok_begin, scope_get(cnt));
	szof->data.expr_sizeof.node = parse_expr(cnt);
	if (!szof->data.expr_sizeof.node) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after sizeof operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
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
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after typeinfo operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	Ast *info = ast_create_node(cnt->ast_arena, AST_EXPR_TYPE_INFO, tok_begin, scope_get(cnt));
	info->data.expr_type_info.node = parse_expr(cnt);
	if (!info->data.expr_type_info.node) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after typeinfo operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
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
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after cast operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	Ast *alof = ast_create_node(cnt->ast_arena, AST_EXPR_ALIGNOF, tok_begin, scope_get(cnt));
	alof->data.expr_alignof.node = parse_expr(cnt);
	if (!alof->data.expr_alignof.node) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after alignof operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	return alof;
}

Ast *
parse_expr_cast_auto(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST_AUTO);
	if (!tok_begin) return NULL;

	Ast *cast = ast_create_node(cnt->ast_arena, AST_EXPR_CAST, tok_begin, scope_get(cnt));
	cast->data.expr_cast.auto_cast = true;

	cast->data.expr_cast.next = _parse_expr(cnt, token_prec(tok_begin).priority);
	if (!cast->data.expr_cast.next) {
		Token *tok = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after auto cast.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	return cast;
}

Ast *
parse_expr_cast(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST);
	if (!tok_begin) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_LPAREN)) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok_begin,
		            BUILDER_CUR_WORD,
		            "Expected '(' after expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	Ast *cast = ast_create_node(cnt->ast_arena, AST_EXPR_CAST, tok_begin, scope_get(cnt));
	cast->data.expr_cast.type = parse_type(cnt);
	if (!cast->data.expr_cast.type) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_TYPE,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Expected type name as cast parameter.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	tok = tokens_consume(cnt->tokens);
	if (!token_is(tok, SYM_RPAREN)) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected ')' after cast expression.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	cast->data.expr_cast.next = _parse_expr(cnt, token_prec(tok_begin).priority);
	if (!cast->data.expr_cast.next) {
		tok = tokens_peek(cnt->tokens);
		PARSE_ERROR(
		    ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "Expected expression after cast.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
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
			builder_msg(BUILDER_MSG_ERROR,
			            ERR_EXPECTED_TYPE,
			            name->location,
			            BUILDER_CUR_AFTER,
			            "expected colon after struct member name");
		}
		type = parse_type(cnt);
	}

	if (!type && !name) return NULL;

	HashDirective found_hd = HD_NONE;
	Ast *         tags     = parse_hash_directive(cnt, HD_TAGS, &found_hd);

	Ast *mem = ast_create_node(cnt->ast_arena, AST_DECL_MEMBER, tok_begin, scope_get(cnt));
	mem->data.decl.type        = type;
	mem->data.decl.name        = name;
	mem->data.decl_member.tags = tags;

	return mem;
}

Ast *
parse_decl_arg(Context *cnt, bool rq_named)
{
	Token *tok_begin = tokens_peek(cnt->tokens);
	Ast *  name      = NULL;
	Ast *  type      = NULL;

	if (tokens_current_is(cnt->tokens, SYM_RPAREN)) return NULL;

	if (tokens_is_seq(cnt->tokens, 2, SYM_IDENT, SYM_COLON)) {
		name = parse_ident(cnt);
		tokens_consume(cnt->tokens);
	} else if (rq_named) {
		Token *tok_err = tokens_peek(cnt->tokens);
		builder_msg(BUILDER_MSG_ERROR,
		            ERR_EXPECTED_NAME,
		            &tok_err->location,
		            BUILDER_CUR_AFTER,
		            "Expected argument name.");

		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	type = parse_type(cnt);

	if (!type && !name) return NULL;
	Ast *arg = ast_create_node(cnt->ast_arena, AST_DECL_ARG, tok_begin, scope_get(cnt));
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

	Ast *var = ast_create_node(cnt->ast_arena, AST_DECL_VARIANT, tok_begin, scope_get(cnt));

	/* TODO: Validate correcly '::' */
	Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_COLON);
	tok_assign        = tokens_consume_if(cnt->tokens, SYM_COLON);
	if (tok_assign) {
		var->data.decl_variant.value = parse_expr(cnt);
		if (!var->data.decl_variant.value) BL_ABORT("Expected enum variant value");
	} else if (prev) {
		BL_ASSERT(prev->kind == AST_DECL_VARIANT);
		Ast *addition =
		    ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, tok_begin, scope_get(cnt));
		addition->data.expr_integer.val = 1;

		Ast *binop =
		    ast_create_node(cnt->ast_arena, AST_EXPR_BINOP, tok_begin, scope_get(cnt));
		binop->data.expr_binop.kind = BINOP_ADD;
		binop->data.expr_binop.lhs  = prev->data.decl_variant.value;
		binop->data.expr_binop.rhs  = addition;

		var->data.decl_variant.value = binop;
	} else {
		/* first variant is allways 0 */
		var->data.decl_variant.value =
		    ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, NULL, scope_get(cnt));
		var->data.decl_variant.value->data.expr_integer.val = 0;
	}

	BL_ASSERT(var->data.decl_variant.value);
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
		PARSE_ERROR(
		    ERR_MISSING_SEMICOLON, tok, BUILDER_CUR_AFTER, "Missing semicolon ';'.");
		return false;
	}
	return true;
}

bool
hash_directive_to_flags(HashDirective hd, u32 *out_flags)
{
#define FLAG_CASE(_c, _f)                                                                          \
	case (_c):                                                                                 \
		(*out_flags) |= (_f);                                                              \
		return true;

	switch (hd) {
		FLAG_CASE(HD_EXTERN, FLAG_EXTERN);
		FLAG_CASE(HD_ENTRY, FLAG_ENTRY);
		FLAG_CASE(HD_BUILD_ENTRY, FLAG_BUILD_ENTRY);
		FLAG_CASE(HD_COMPILER, FLAG_COMPILER);
		FLAG_CASE(HD_INLINE, FLAG_INLINE);
		FLAG_CASE(HD_NO_INLINE, FLAG_NO_INLINE);
	default:
		break;
	}

	return false;
}

Ast *
parse_stmt_return(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_RETURN);
	if (!tok_begin) return NULL;

	Ast *ret = ast_create_node(cnt->ast_arena, AST_STMT_RETURN, tok_begin, scope_get(cnt));
	ret->data.stmt_return.fn_decl = decl_get(cnt);
	ret->data.stmt_return.expr    = parse_expr(cnt);
	return ret;
}

Ast *
parse_stmt_if(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_IF);
	if (!tok_begin) return NULL;

	Ast *stmt_if = ast_create_node(cnt->ast_arena, AST_STMT_IF, tok_begin, scope_get(cnt));

	stmt_if->data.stmt_if.test = parse_expr(cnt);
	if (!stmt_if->data.stmt_if.test) {
		Token *tok_err = tokens_consume(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Expected expression for the if statement.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	if (stmt_if->data.stmt_if.test->kind == AST_BAD) {
		tokens_consume_till(cnt->tokens, SYM_LBLOCK);
	}

	stmt_if->data.stmt_if.true_stmt = parse_block(cnt, true);
	if (!stmt_if->data.stmt_if.true_stmt) {
		Token *tok_err = tokens_consume(cnt->tokens);
		PARSE_ERROR(
		    ERR_EXPECTED_STMT,
		    tok_err,
		    BUILDER_CUR_WORD,
		    "Expected compound statement for true result of the if expression test.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	stmt_if->data.stmt_if.false_stmt = NULL;
	if (tokens_consume_if(cnt->tokens, SYM_ELSE)) {
		stmt_if->data.stmt_if.false_stmt = parse_stmt_if(cnt);
		if (!stmt_if->data.stmt_if.false_stmt)
			stmt_if->data.stmt_if.false_stmt = parse_block(cnt, true);
		if (!stmt_if->data.stmt_if.false_stmt) {
			Token *tok_err = tokens_consume(cnt->tokens);
			PARSE_ERROR(
			    ERR_EXPECTED_STMT,
			    tok_err,
			    BUILDER_CUR_WORD,
			    "Expected statement for false result of the if expression test.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
		}
	}

	return stmt_if;
}

Ast *
parse_stmt_switch(Context *cnt)
{
	Token *tok_switch = tokens_consume_if(cnt->tokens, SYM_SWITCH);
	if (!tok_switch) return NULL;

	Ast *expr = parse_expr(cnt);
	BL_ASSERT(expr && "This should be an error!");

	Token *tok = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
	BL_ASSERT(tok && "This should be an error!");

	TSmallArray_AstPtr *cases        = create_sarr(TSmallArray_AstPtr, cnt->assembly);
	Ast *               stmt_case    = NULL;
	Ast *               default_case = NULL;
NEXT:
	stmt_case = parse_stmt_case(cnt);
	if (stmt_case && stmt_case->kind != AST_BAD) {
		if (stmt_case->data.stmt_case.is_default) {
			if (default_case) {
				builder_msg(
				    BUILDER_MSG_ERROR,
				    ERR_INVALID_SWITCH_CASE,
				    stmt_case->location,
				    BUILDER_CUR_WORD,
				    "Switch statement cannot have more than one default cases.");

				builder_msg(BUILDER_MSG_NOTE,
				            0,
				            default_case->location,
				            BUILDER_CUR_WORD,
				            "Previous found here.");
			} else {
				default_case = stmt_case;
			}
		}

		tsa_push_AstPtr(cases, stmt_case);
		if (tokens_current_is_not(cnt->tokens, SYM_RBLOCK)) goto NEXT;
	}

	tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
	BL_ASSERT(tok && "This should be an error!");

	Ast *stmt_switch =
	    ast_create_node(cnt->ast_arena, AST_STMT_SWITCH, tok_switch, scope_get(cnt));

	stmt_switch->data.stmt_switch.expr  = expr;
	stmt_switch->data.stmt_switch.cases = cases;
	return stmt_switch;
}

Ast *
parse_stmt_case(Context *cnt)
{
	TSmallArray_AstPtr *exprs = NULL;
	Ast *               block = NULL;
	Ast *               expr  = NULL;
	bool                rq    = false;

	if (tokens_current_is(cnt->tokens, SYM_RBLOCK)) return NULL;

	Token *tok_case = tokens_consume_if(cnt->tokens, SYM_DEFAULT);
	if (tok_case) goto SKIP_EXPRS;

	tok_case = tokens_peek(cnt->tokens);
	exprs    = create_sarr(TSmallArray_AstPtr, cnt->assembly);
NEXT:
	expr = parse_expr(cnt);
	if (expr) {
		tsa_push_AstPtr(exprs, expr);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_NAME,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Expected expression after comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

SKIP_EXPRS:
	block = parse_block(cnt, true);
	if (!block && !parse_semicolon_rq(cnt)) {
		Token *tok_err = tokens_peek(cnt->tokens);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	} else {
		parse_semicolon(cnt);
	}

	Ast *stmt_case = ast_create_node(cnt->ast_arena, AST_STMT_CASE, tok_case, scope_get(cnt));
	stmt_case->data.stmt_case.exprs      = exprs;
	stmt_case->data.stmt_case.is_default = !exprs;
	stmt_case->data.stmt_case.block      = block;

	return stmt_case;
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

	Ast *      loop = ast_create_node(cnt->ast_arena, AST_STMT_LOOP, tok_begin, scope_get(cnt));
	const bool prev_in_loop = cnt->inside_loop;
	cnt->inside_loop        = true;

	Scope *scope = scope_create(
	    cnt->scope_arenas, SCOPE_LEXICAL, scope_get(cnt), 128, &tok_begin->location);

	scope_push(cnt, scope);

	if (!while_true) {
		if (tokens_lookahead(cnt->tokens, cmp_stmt_loop)) {
			/* for loop construct loop [init]; [condition]; [increment] {} */
			loop->data.stmt_loop.init = parse_decl(cnt);
			if (!parse_semicolon_rq(cnt)) {
				BL_ASSERT(false);
			}

			loop->data.stmt_loop.condition = parse_expr(cnt);
			if (!parse_semicolon_rq(cnt)) {
				BL_ASSERT(false);
			}

			loop->data.stmt_loop.increment = parse_expr(cnt);
		} else {
			/* while construct with optional condition */
			loop->data.stmt_loop.condition = parse_expr(cnt);
		}
	}

	/* block */
	loop->data.stmt_loop.block = parse_block(cnt, false);
	if (!loop->data.stmt_loop.block) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(
		    ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "Expected loop body block.");
		cnt->inside_loop = prev_in_loop;
		scope_pop(cnt);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	cnt->inside_loop = prev_in_loop;
	scope_pop(cnt);
	return loop;
}

Ast *
parse_stmt_break(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_BREAK);
	if (!tok) return NULL;

	if (!cnt->inside_loop) {
		PARSE_ERROR(ERR_BREAK_OUTSIDE_LOOP,
		            tok,
		            BUILDER_CUR_WORD,
		            "Break statement outside a loop.");
	}
	return ast_create_node(cnt->ast_arena, AST_STMT_BREAK, tok, scope_get(cnt));
}

Ast *
parse_stmt_continue(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_CONTINUE);
	if (!tok) return NULL;

	if (!cnt->inside_loop) {
		PARSE_ERROR(ERR_CONTINUE_OUTSIDE_LOOP,
		            tok,
		            BUILDER_CUR_WORD,
		            "Continue statement outside a loop.");
	}

	return ast_create_node(cnt->ast_arena, AST_STMT_CONTINUE, tok, scope_get(cnt));
}

Ast *
parse_stmt_defer(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_DEFER);
	if (!tok) return NULL;

	Ast *expr = NULL;
	expr      = parse_expr(cnt);

	if (!expr) {
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after 'defer' statement.");

		Token *tok_err = tokens_peek(cnt->tokens);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, scope_get(cnt));
	}

	Ast *defer = ast_create_node(cnt->ast_arena, AST_STMT_DEFER, tok, scope_get(cnt));
	defer->data.stmt_defer.expr = expr;

	return defer;
}

Ast *
parse_expr(Context *cnt)
{
	return _parse_expr(cnt, 0);
}

Ast *
_parse_expr(Context *cnt, s32 p)
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

		const s32 q = token_prec(op).associativity == TOKEN_ASSOC_LEFT
		                  ? token_prec(op).priority + 1
		                  : token_prec(op).priority;

		Ast *rhs = _parse_expr(cnt, q);
		if (!lhs || !rhs) {
			PARSE_ERROR(
			    ERR_INVALID_EXPR, op, BUILDER_CUR_WORD, "Invalid binary operation.");
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
	if ((expr = parse_hash_directive(cnt, HD_FILE | HD_LINE, NULL))) return expr;

	return NULL;
}

/* <unary operator> <expression> */
Ast *
parse_expr_unary(Context *cnt)
{
	Token *op = tokens_peek(cnt->tokens);
	if (!token_is_unary(op)) return NULL;

	tokens_consume(cnt->tokens);
	Ast *unary = ast_create_node(cnt->ast_arena, AST_EXPR_UNARY, op, scope_get(cnt));
	unary->data.expr_unary.next = _parse_expr(cnt, token_prec(op).priority);
	unary->data.expr_unary.kind = sym_to_unop_kind(op->sym);

	if (unary->data.expr_unary.next == NULL) {
		Token *err_tok = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            err_tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after unary operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, op, scope_get(cnt));
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
	if ((expr = parse_expr_cast_auto(cnt))) return expr;
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

	Ast *binop = ast_create_node(cnt->ast_arena, AST_EXPR_BINOP, op, scope_get(cnt));
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

	Ast *addrof = ast_create_node(cnt->ast_arena, AST_EXPR_ADDROF, tok, scope_get(cnt));
	addrof->data.expr_addrof.next = _parse_expr(cnt, token_prec(tok).priority);

	if (addrof->data.expr_addrof.next == NULL) {
		Token *err_tok = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            err_tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after '&' operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	if (addrof->data.expr_addrof.next->kind == AST_BAD) return addrof->data.expr_addrof.next;
	return addrof;
}

Ast *
parse_expr_deref(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_CARET);
	if (!tok) return NULL;

	Ast *deref = ast_create_node(cnt->ast_arena, AST_EXPR_DEREF, tok, scope_get(cnt));
	deref->data.expr_deref.next = _parse_expr(cnt, token_prec(tok).priority);

	if (deref->data.expr_deref.next == NULL) {
		Token *err_tok = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            err_tok,
		            BUILDER_CUR_WORD,
		            "Expected expression after '^' operator.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
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
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, tok, scope_get(cnt));
		lit->data.expr_integer.val      = tok->value.u;
		lit->data.expr_integer.overflow = tok->overflow;
		break;

	case SYM_CHAR:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_CHAR, tok, scope_get(cnt));
		lit->data.expr_character.val = (u8)tok->value.c;

		break;

	case SYM_STRING:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_STRING, tok, scope_get(cnt));
		lit->data.expr_string.val = tok->value.str;
		break;

	case SYM_TRUE:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, scope_get(cnt));
		lit->data.expr_boolean.val = true;
		break;

	case SYM_FALSE:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, scope_get(cnt));
		lit->data.expr_boolean.val = false;
		break;

	case SYM_DOUBLE:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_DOUBLE, tok, scope_get(cnt));
		lit->data.expr_double.val      = tok->value.d;
		lit->data.expr_double.overflow = tok->overflow;
		break;

	case SYM_FLOAT:
		lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FLOAT, tok, scope_get(cnt));
		lit->data.expr_float.val      = (f32)tok->value.d;
		lit->data.expr_float.overflow = tok->overflow;
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

	Ast *fn = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FN, tok_fn, scope_get(cnt));

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_FN, scope_get(cnt), 256, &tok_fn->location);

	scope_push(cnt, scope);

	Ast *type = parse_type_fn(cnt, true);
	BL_ASSERT(type);

	fn->data.expr_fn.type = type;

	/* parse flags */
	Ast *curr_decl = decl_get(cnt);
	if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
		u32 accepted =
		    HD_EXTERN | HD_NO_INLINE | HD_INLINE | HD_COMPILER | HD_ENTRY | HD_BUILD_ENTRY;
		u32 flags = 0;
		while (true) {
			HashDirective found        = HD_NONE;
			Ast *         hd_extension = parse_hash_directive(cnt, accepted, &found);
			if (!hash_directive_to_flags(found, &flags)) break;

			if (found == HD_EXTERN && hd_extension) {
				/* Use extern flag extension on function declaration. */

				BL_ASSERT(hd_extension->kind == AST_IDENT &&
				          "Expected ident as #extern extension.");
				BL_ASSERT(curr_decl->data.decl_entity.explicit_linkage_name ==
				          NULL);
				curr_decl->data.decl_entity.explicit_linkage_name = hd_extension;
			}

			accepted &= ~found;
		}

		curr_decl->data.decl_entity.flags |= flags;
	}

	/* parse block (block is optional function body can be external) */
	fn->data.expr_fn.block = parse_block(cnt, false);

	scope_pop(cnt);
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
		PARSE_ERROR(ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_WORD, "Expected expression.");
	}

	/* eat ) */
	Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RPAREN);
	if (!tok_end) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok_err,
		            BUILDER_CUR_WORD,
		            "Unterminated sub-expression, missing ')'.");
		PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "starting here");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
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
		PARSE_ERROR(ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected member name.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	Ast *mem = ast_create_node(cnt->ast_arena, AST_EXPR_MEMBER, tok, scope_get(cnt));
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

	Ast *elem = ast_create_node(cnt->ast_arena, AST_EXPR_ELEM, tok_elem, scope_get(cnt));
	elem->data.expr_elem.index = parse_expr(cnt);
	elem->data.expr_elem.next  = prev;

	if (!elem->data.expr_elem.index) {
		PARSE_ERROR(ERR_EXPECTED_EXPR,
		            tok_elem,
		            BUILDER_CUR_WORD,
		            "Expected array index expression.");
	}

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBRACKET) {
		PARSE_ERROR(ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Missing bracket ']'.");
	}

	return elem;
}

Ast *
parse_ident(Context *cnt)
{
	Token *tok_ident = tokens_consume_if(cnt->tokens, SYM_IDENT);
	if (!tok_ident) return NULL;

	Ast *ident = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ident, scope_get(cnt));
	id_init(&ident->data.ident.id, tok_ident->value.str);

	return ident;
}

Ast *
parse_type_ptr(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ASTERISK);
	if (!tok_begin) return NULL;

	Ast *ptr = ast_create_node(cnt->ast_arena, AST_TYPE_PTR, tok_begin, scope_get(cnt));
	ptr->data.type_ptr.type = parse_type(cnt);
	BL_ASSERT(ptr->data.type_ptr.type);
	return ptr;
}

Ast *
parse_type_vargs(Context *cnt)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_VARGS);
	if (!tok_begin) return NULL;

	Ast *ptr = ast_create_node(cnt->ast_arena, AST_TYPE_VARGS, tok_begin, scope_get(cnt));
	ptr->data.type_ptr.type = parse_type(cnt);
	return ptr;
}

Ast *
parse_type_enum(Context *cnt)
{
	Token *tok_enum = tokens_consume_if(cnt->tokens, SYM_ENUM);
	if (!tok_enum) return NULL;

	Ast *enm = ast_create_node(cnt->ast_arena, AST_TYPE_ENUM, tok_enum, scope_get(cnt));
	enm->data.type_enm.variants = create_sarr(TSmallArray_AstPtr, cnt->assembly);
	enm->data.type_enm.type     = parse_type(cnt);

	/* parse flags */
	Ast *curr_decl = decl_get(cnt);
	if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
		u32 accepted = HD_COMPILER;
		u32 flags    = 0;
		while (true) {
			HashDirective found = HD_NONE;
			parse_hash_directive(cnt, accepted, &found);
			if (!hash_directive_to_flags(found, &flags)) break;
			accepted &= ~found;
		}

		curr_decl->data.decl_entity.flags |= flags;
	}

	Token *tok = tokens_consume(cnt->tokens);
	if (token_is_not(tok, SYM_LBLOCK)) {
		PARSE_ERROR(
		    ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected enum variant list.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_TYPE_ENUM, scope_get(cnt), 512, &tok->location);
	enm->data.type_enm.scope = scope;
	scope_push(cnt, scope);

	/* parse enum varinats */
	bool rq = false;
	Ast *tmp;
	Ast *prev_tmp = NULL;

NEXT:
	tmp = parse_decl_variant(cnt, prev_tmp);
	if (tmp) {
		prev_tmp = tmp;
		tsa_push_AstPtr(enm->data.type_enm.variants, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_SEMICOLON)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected variant after semicolon.");
			scope_pop(cnt);
			return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBLOCK) {
		PARSE_ERROR(
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of variant list '}' or another variant separated by semicolon.");
		scope_pop(cnt);
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	scope_pop(cnt);
	return enm;
}

Ast *
parse_type_ref(Context *cnt)
{
	Token *tok   = tokens_peek(cnt->tokens);
	Ast *  ident = parse_ident(cnt);
	if (!ident) return NULL;

	Ast *type_ref = ast_create_node(cnt->ast_arena, AST_TYPE_REF, tok, scope_get(cnt));
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

	Ast *arr = ast_create_node(cnt->ast_arena, AST_TYPE_ARR, tok_begin, scope_get(cnt));
	arr->data.type_arr.len = parse_expr(cnt);
	BL_ASSERT(arr->data.type_arr.len);

	Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
	if (!tok_end) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok_end,
		            BUILDER_CUR_WORD,
		            "Expected closing ']' after array size expression.");
	}

	arr->data.type_arr.elem_type = parse_type(cnt);
	if (!arr->data.type_arr.elem_type) {
		Token *tok_err = tokens_peek(cnt->tokens);
		PARSE_ERROR(
		    ERR_INVALID_TYPE, tok_err, BUILDER_CUR_WORD, "Expected array element type.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

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

	Ast *slice = ast_create_node(cnt->ast_arena, AST_TYPE_SLICE, tok_begin, scope_get(cnt));

	slice->data.type_slice.elem_type = parse_type(cnt);

	if (!slice->data.type_slice.elem_type) {
		PARSE_ERROR(
		    ERR_INVALID_TYPE, tok_begin, BUILDER_CUR_AFTER, "Expected slice element type.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
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
parse_type_fn(Context *cnt, bool rq_named_args)
{
	Token *tok_fn = tokens_consume_if(cnt->tokens, SYM_FN);
	if (!tok_fn) return NULL;

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_LPAREN) {
		PARSE_ERROR(ERR_MISSING_BRACKET,
		            tok,
		            BUILDER_CUR_WORD,
		            "Expected function parameter list.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, scope_get(cnt));
	}

	Ast *fn = ast_create_node(cnt->ast_arena, AST_TYPE_FN, tok_fn, scope_get(cnt));

	/* parse arg types */
	bool rq = false;
	Ast *tmp;

NEXT:
	tmp = parse_decl_arg(cnt, rq_named_args);
	if (tmp) {
		if (!fn->data.type_fn.args)
			fn->data.type_fn.args = create_sarr(TSmallArray_AstPtr, cnt->assembly);

		tsa_push_AstPtr(fn->data.type_fn.args, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected type after comma ','.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, scope_get(cnt));
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RPAREN) {
		PARSE_ERROR(
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of argument type list ')' or another type separated by comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, scope_get(cnt));
	}

	fn->data.type_fn.ret_type = parse_type(cnt);
	return fn;
}

Ast *
parse_type_struct(Context *cnt)
{
	Token *tok_struct = tokens_consume_if(cnt->tokens, SYM_STRUCT);
	if (!tok_struct) return NULL;

	/* parse flags */
	u32  accepted  = HD_COMPILER | HD_BASE;
	u32  flags     = 0;
	Ast *base_type = NULL;
	while (true) {
		Ast *         hd_extension;
		HashDirective found = HD_NONE;
		hd_extension        = parse_hash_directive(cnt, accepted, &found);
		if (found == HD_BASE) {
			BL_ASSERT(hd_extension);
			base_type = hd_extension;
		} else if (!hash_directive_to_flags(found, &flags)) {
			break;
		}
		accepted &= ~found;
	}

	Ast *curr_decl = decl_get(cnt);
	if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
		curr_decl->data.decl_entity.flags |= flags;
	}

	Token *tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_LBLOCK) {
		PARSE_ERROR(
		    ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected struct member list.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, scope_get(cnt));
	}

	Scope *scope =
	    scope_create(cnt->scope_arenas, SCOPE_TYPE_STRUCT, scope_get(cnt), 256, &tok->location);
	scope_push(cnt, scope);

	Ast *type_struct =
	    ast_create_node(cnt->ast_arena, AST_TYPE_STRUCT, tok_struct, scope_get(cnt));
	type_struct->data.type_strct.scope     = scope;
	type_struct->data.type_strct.raw       = false;
	type_struct->data.type_strct.members   = create_sarr(TSmallArray_AstPtr, cnt->assembly);
	type_struct->data.type_strct.base_type = base_type;

	/* parse members */
	bool       rq = false;
	Ast *      tmp;
	const bool type_only = tokens_peek_2nd(cnt->tokens)->sym == SYM_COMMA ||
	                       tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK;
	type_struct->data.type_strct.raw = type_only;
NEXT:
	tmp = parse_decl_member(cnt, type_only);
	if (tmp) {
		tsa_push_AstPtr(type_struct->data.type_strct.members, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_SEMICOLON)) {
			rq = true;
			goto NEXT;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected member after semicolon.");

			scope_pop(cnt);
			return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, scope_get(cnt));
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RBLOCK) {
		PARSE_ERROR(
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of member list '}' or another memeber separated by semicolon.");
		tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
		scope_pop(cnt);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, scope_get(cnt));
	}

	scope_pop(cnt);
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

	Ast *decl = ast_create_node(cnt->ast_arena, AST_DECL_ENTITY, tok_ident, scope_get(cnt));
	decl->data.decl.name       = ident;
	decl->data.decl_entity.mut = true;

	decl_push(cnt, decl);

	decl->data.decl.type = parse_type(cnt);
	Token *tok_assign    = tokens_consume_if(cnt->tokens, SYM_ASSIGN);
	if (!tok_assign) tok_assign = tokens_consume_if(cnt->tokens, SYM_COLON);

	if (tok_assign) {
		decl->data.decl_entity.mut = token_is(tok_assign, SYM_ASSIGN);

		/* parse declaration expression */
		decl->data.decl_entity.value = parse_expr(cnt);

		if (!(decl->data.decl_entity.flags & (FLAG_EXTERN))) {
			if (!decl->data.decl_entity.value) {
				PARSE_ERROR(ERR_EXPECTED_INITIALIZATION,
				            tok_assign,
				            BUILDER_CUR_AFTER,
				            "Expected binding of declaration to some value.");
				decl_pop(cnt);
				return ast_create_node(
				    cnt->ast_arena, AST_BAD, tok_ident, scope_get(cnt));
			}
		}
	}

	decl_pop(cnt);
	return decl;
}

Ast *
parse_expr_call(Context *cnt, Ast *prev)
{
	if (!prev) return NULL;

	Token *tok = tokens_consume_if(cnt->tokens, SYM_LPAREN);
	if (!tok) return NULL;

	Ast *call = ast_create_node(cnt->ast_arena, AST_EXPR_CALL, tok, scope_get(cnt));
	call->data.expr_call.ref = prev;
	call->data.expr_call.run = false;

	/* parse args */
	bool rq = false;
	Ast *tmp;

arg:
	tmp = parse_expr(cnt);
	if (tmp) {
		if (!call->data.expr_call.args)
			call->data.expr_call.args = create_sarr(TSmallArray_AstPtr, cnt->assembly);
		tsa_push_AstPtr(call->data.expr_call.args, tmp);

		if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
			rq = true;
			goto arg;
		}
	} else if (rq) {
		Token *tok_err = tokens_peek(cnt->tokens);
		if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
			PARSE_ERROR(ERR_EXPECTED_NAME,
			            tok_err,
			            BUILDER_CUR_WORD,
			            "Expected function argument after comma ','.");
			return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
		}
	}

	tok = tokens_consume(cnt->tokens);
	if (tok->sym != SYM_RPAREN) {
		PARSE_ERROR(
		    ERR_MISSING_BRACKET,
		    tok,
		    BUILDER_CUR_WORD,
		    "Expected end of parameter list ')' or another parameter separated by comma.");
		return ast_create_node(cnt->ast_arena, AST_BAD, tok, scope_get(cnt));
	}

	return call;
}

Ast *
parse_expr_null(Context *cnt)
{
	Token *tok_null = tokens_consume_if(cnt->tokens, SYM_NULL);
	if (!tok_null) return NULL;
	return ast_create_node(cnt->ast_arena, AST_EXPR_NULL, tok_null, scope_get(cnt));
}

Ast *
parse_unrecheable(Context *cnt)
{
	Token *tok = tokens_consume_if(cnt->tokens, SYM_UNREACHABLE);
	if (!tok) return NULL;

	return ast_create_node(cnt->ast_arena, AST_UNREACHABLE, tok, scope_get(cnt));
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
		Ast *expr = ast_create_node(cnt->ast_arena, AST_EXPR_TYPE, tok, scope_get(cnt));
		expr->data.expr_type.type = type;
		return expr;
	}

	return NULL;
}

Ast *
parse_block(Context *cnt, bool create_scope)
{
	Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
	if (!tok_begin) return NULL;

	if (create_scope) {
		Scope *scope = scope_create(
		    cnt->scope_arenas, SCOPE_LEXICAL, scope_get(cnt), 1024, &tok_begin->location);

		scope_push(cnt, scope);
	}

	Ast *block = ast_create_node(cnt->ast_arena, AST_BLOCK, tok_begin, scope_get(cnt));

	Token *tok;
	Ast *  tmp;
	block->data.block.nodes = tarray_new(sizeof(Ast *));

NEXT:
	if (tokens_current_is(cnt->tokens, SYM_SEMICOLON)) {
		tok = tokens_consume(cnt->tokens);
		PARSE_WARNING(tok, BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
		goto NEXT;
	}

	parse_hash_directive(cnt, 0, NULL);

	if ((tmp = (Ast *)parse_decl(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_return(cnt))) {
		if ((tmp)->kind != AST_BAD) parse_semicolon_rq(cnt);
		tarray_push(block->data.block.nodes, tmp);
		block->data.block.has_return      = true;
		tmp->data.stmt_return.owner_block = block;
		goto NEXT;
	}

	if ((tmp = parse_stmt_if(cnt))) {
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_switch(cnt))) {
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_loop(cnt))) {
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_break(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_continue(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_stmt_defer(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_expr(cnt))) {
		if (tmp->kind != AST_BAD) parse_semicolon_rq(cnt);
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_block(cnt, true))) {
		tarray_push(block->data.block.nodes, tmp);
		goto NEXT;
	}

	if ((tmp = parse_unrecheable(cnt))) {
		tarray_push(block->data.block.nodes, tmp);
		parse_semicolon_rq(cnt);
		goto NEXT;
	}

	tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
	if (!tok) {
		tok = tokens_peek_prev(cnt->tokens);
		PARSE_ERROR(
		    ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
		PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
		if (create_scope) scope_pop(cnt);
		return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, scope_get(cnt));
	}

	if (create_scope) scope_pop(cnt);
	return block;
}

void
parse_ublock_content(Context *cnt, Ast *ublock)
{
	/******************************************************************************************/
#define RQ_SEMICOLON_AFTER(_node)                                                                  \
	((_node)->data.decl_entity.value &&                                                        \
	 (_node)->data.decl_entity.value->kind != AST_EXPR_LIT_FN &&                               \
	 (_node)->data.decl_entity.value->kind != AST_TEST_CASE &&                                 \
	 (_node)->data.decl_entity.value->kind != AST_EXPR_TYPE)
	/******************************************************************************************/

	BL_ASSERT(ublock->kind == AST_UBLOCK);
	ublock->data.ublock.nodes = tarray_new(sizeof(Ast *));

	Ast *tmp;
NEXT:
	if (parse_semicolon(cnt)) goto NEXT;

	if ((tmp = parse_decl(cnt))) {
		if (tmp->kind != AST_BAD) {
			if (RQ_SEMICOLON_AFTER(tmp)) parse_semicolon_rq(cnt);
			/* setup global scope flag for declaration */
			tmp->data.decl_entity.in_gscope = true;
			if (cnt->inside_private_scope) tmp->data.decl_entity.flags |= FLAG_PRIVATE;
		}

		tarray_push(ublock->data.ublock.nodes, tmp);
		goto NEXT;
	}

	/* load, link, test, private - enabled in global scope */
	const int enabled_hd = HD_LOAD | HD_LINK | HD_TEST | HD_PRIVATE | HD_META;
	if ((tmp = parse_hash_directive(cnt, enabled_hd, NULL))) {
		if (tmp->kind == AST_META_DATA) {
			ublock->meta_node = tmp;
		} else {
			tarray_push(ublock->data.ublock.nodes, tmp);
		}
		goto NEXT;
	}

	Token *tok = tokens_peek(cnt->tokens);
	if (!token_is(tok, SYM_EOF)) {
		PARSE_ERROR(ERR_UNEXPECTED_SYMBOL,
		            tok,
		            BUILDER_CUR_WORD,
		            "Unexpected symbol in module body '%s'.",
		            sym_strings[tok->sym]);
	}

#undef RQ_SEMICOLON_AFTER
}

void
parser_run(Assembly *assembly, Unit *unit)
{
	BL_ASSERT(assembly->gscope && "Missing global scope for assembly.");

	Context cnt = {.assembly     = assembly,
	               .unit         = unit,
	               .ast_arena    = &assembly->arenas.ast,
	               .scope_arenas = &assembly->arenas.scope,
	               .tokens       = &unit->tokens,
	               .inside_loop  = false};

	tsa_init(&cnt._decl_stack);
	tsa_init(&cnt._scope_stack);

	scope_push(&cnt, assembly->gscope);

	Ast *root              = ast_create_node(cnt.ast_arena, AST_UBLOCK, NULL, scope_get(&cnt));
	root->data.ublock.unit = unit;
	unit->ast              = root;

	if (assembly->options.build_mode == BUILD_MODE_DEBUG) {
		unit->llvm_file_meta =
		    llvm_di_create_file(assembly->llvm.di_builder, unit->filename, unit->dirpath);
	}

	parse_ublock_content(&cnt, unit->ast);
	tsa_terminate(&cnt._decl_stack);
	tsa_terminate(&cnt._scope_stack);
}
