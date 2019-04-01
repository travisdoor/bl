//************************************************************************************************
// bl
//
// File:   ast.h
// Author: Martin Dorazil
// Date:   3/14/18
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

#ifndef BL_AST_H
#define BL_AST_H

#include "common.h"
#include "scope.h"
#include "token.h"
#include <bobject/containers/array.h>
#include <bobject/containers/hash.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/list.h>

struct Arena;
typedef struct Ast Ast;

typedef enum {
	AST_BAD,
	AST_LOAD,
	AST_LINK,
	AST_IDENT,
	AST_UBLOCK,
	AST_BLOCK,
	AST_TEST_CASE,
	AST_UNREACHABLE,
	_AST_DECL_FIRST,
	AST_DECL_ENTITY,
	AST_DECL_MEMBER,
	AST_DECL_ARG,
	AST_DECL_VARIANT,
	_AST_DECL_LAST,
	AST_STMT_RETURN,
	AST_STMT_IF,
	AST_STMT_LOOP,
	AST_STMT_BREAK,
	AST_STMT_CONTINUE,
	AST_STMT_DEFER,
	_AST_TYPE_FIRST,
	AST_TYPE_REF,
	AST_TYPE_ARR,
	AST_TYPE_SLICE,
	AST_TYPE_FN,
	AST_TYPE_STRUCT,
	AST_TYPE_ENUM,
	AST_TYPE_PTR,
	AST_TYPE_VARGS,
	_AST_TYPE_LAST,
	_AST_EXPR_FIRST,
	AST_EXPR_TYPE,
	AST_EXPR_REF,
	AST_EXPR_CAST,
	AST_EXPR_BINOP,
	AST_EXPR_CALL,
	AST_EXPR_MEMBER,
	AST_EXPR_ELEM,
	AST_EXPR_SIZEOF,
	AST_EXPR_ALIGNOF,
	AST_EXPR_TYPEOF,
	AST_EXPR_TYPE_INFO,
	AST_EXPR_UNARY,
	AST_EXPR_NULL,
	AST_EXPR_ADDROF,
	AST_EXPR_DEREF,
	AST_EXPR_COMPOUND,
	AST_EXPR_LIT_FN,
	AST_EXPR_LIT_INT,
	AST_EXPR_LIT_FLOAT,
	AST_EXPR_LIT_DOUBLE,
	AST_EXPR_LIT_CHAR,
	AST_EXPR_LIT_STRING,
	AST_EXPR_LIT_BOOL,
	_AST_EXPR_LAST,
} AstKind;

typedef enum {
	FLAG_EXTERN = 1 << 0, /* methods marked as extern */
	FLAG_TEST   = 1 << 1, /* test case */
} AstFlag;

/* map symbols to binary operation kind */
typedef enum {
	BINOP_INVALID = 0,
	BINOP_ASSIGN,
	BINOP_ADD_ASSIGN,
	BINOP_SUB_ASSIGN,
	BINOP_MUL_ASSIGN,
	BINOP_DIV_ASSIGN,
	BINOP_MOD_ASSIGN,
	BINOP_ADD,
	BINOP_SUB,
	BINOP_MUL,
	BINOP_DIV,
	BINOP_MOD,
	BINOP_EQ,
	BINOP_NEQ,
	BINOP_GREATER,
	BINOP_LESS,
	BINOP_GREATER_EQ,
	BINOP_LESS_EQ,
	BINOP_LOGIC_AND,
	BINOP_LOGIC_OR,
	BINOP_AND,
	BINOP_OR,
} BinopKind;

typedef enum {
	UNOP_INVALID = 0,
	UNOP_NEG,
	UNOP_POS,
	UNOP_NOT,
} UnopKind;

struct AstLoad {
	const char *filepath;
};

struct AstLink {
	const char *lib;
};

struct AstIdent {
	Scope *scope;
	ID     id;
};

struct AstUBlock {
	BArray *     nodes;
	struct Unit *unit;
};

struct AstBlock {
	BArray *nodes;
};

struct AstTestCase {
	const char *desc;
	Ast *       block;
};

struct AstStmtReturn {
	Ast *expr;
	Ast *fn_decl;
};

struct AstStmtDefer {
	Ast *expr;
};

struct AstStmtIf {
	Ast *test;
	Ast *true_stmt;
	Ast *false_stmt;
};

struct AstStmtLoop {
	Ast *init;
	Ast *condition;
	Ast *increment;
	Ast *block;
};

struct AstDecl {
	Ast *name;
	Ast *type;
};

struct AstDeclEntity {
	struct AstDecl base;
	Ast *          value;
	int32_t        flags;
	bool           in_gscope;
	bool mutable;
};

struct AstDeclMember {
	struct AstDecl base;
};

struct AstDeclArg {
	struct AstDecl base;
};

struct AstDeclVariant {
	struct AstDecl base;
	Ast *          value;
};

struct AstTypeArr {
	Ast *elem_type;
	Ast *len;
};

struct AstTypeSlice {
	Ast *elem_type;
};

struct AstTypeFn {
	Ast *   ret_type;
	BArray *args;
};

struct AstTypeStruct {
	Scope * scope;
	BArray *members;
	bool    raw;
};

struct AstTypeEnum {
	Scope * scope;
	Ast *   type;
	BArray *variants;
};

struct AstTypePtr {
	Ast *type;
};

struct AstTypeVargs {
	Ast *type;
};

struct AstTypeRef {
	Ast *ident;
};

struct AstExprType {
	Ast *type;
};

struct AstExprCompound {
	Ast *   type;
	BArray *values;
};

struct AstExprLitFn {
	Ast *type;
	Ast *block;
};

struct AstExprLitInt {
	uint64_t val;
};

struct AstExprLitFloat {
	float val;
};

struct AstExprLitDouble {
	double val;
};

struct AstExprLitChar {
	uint8_t val;
};

struct AstExprLitString {
	const char *val;
};

struct AstExprLitBool {
	bool val;
};

struct AstExprRef {
	Ast *ident;
};

struct AstExprCast {
	Ast *type;
	Ast *next;
};

struct AstExprBinop {
	Ast *     lhs;
	Ast *     rhs;
	BinopKind kind;
};

struct AstExprCall {
	Ast *   ref;
	BArray *args;
	bool    run;
};

struct AstExprMember {
	Ast *   ident;
	Ast *   next;
	int32_t i;
};

struct AstExprElem {
	Ast *next;
	Ast *index;
};

struct AstExprSizeof {
	Ast *node;
};

struct AstExprTypeInfo {
	Ast *node;
};

struct AstExprAlignof {
	Ast *node;
};

struct AstExprTypeof {
	Ast *node;
};

struct AstExprUnary {
	UnopKind kind;
	Ast *    next;
};

struct AstExprAddrOf {
	Ast *next;
};

struct AstExprDeref {
	Ast *next;
};

/* AST base type */
struct Ast {
	AstKind kind;
	Src *   src;

	union {
		struct AstLoad          load;
		struct AstLink          link;
		struct AstIdent         ident;
		struct AstUBlock        ublock;
		struct AstBlock         block;
		struct AstStmtReturn    stmt_return;
		struct AstStmtIf        stmt_if;
		struct AstStmtLoop      stmt_loop;
		struct AstStmtDefer     stmt_defer;
		struct AstTestCase      test_case;
		struct AstDecl          decl;
		struct AstDeclEntity    decl_entity;
		struct AstDeclArg       decl_arg;
		struct AstDeclMember    decl_member;
		struct AstDeclVariant   decl_variant;
		struct AstTypeRef       type_ref;
		struct AstTypeArr       type_arr;
		struct AstTypeSlice     type_slice;
		struct AstTypeFn        type_fn;
		struct AstTypeStruct    type_strct;
		struct AstTypeEnum      type_enm;
		struct AstTypePtr       type_ptr;
		struct AstTypeVargs     type_vargs;
		struct AstExprType      expr_type;
		struct AstExprLitFn     expr_fn;
		struct AstExprLitInt    expr_integer;
		struct AstExprLitFloat  expr_float;
		struct AstExprLitDouble expr_double;
		struct AstExprLitChar   expr_character;
		struct AstExprLitString expr_string;
		struct AstExprLitBool   expr_boolean;
		struct AstExprRef       expr_ref;
		struct AstExprCast      expr_cast;
		struct AstExprBinop     expr_binop;
		struct AstExprCall      expr_call;
		struct AstExprMember    expr_member;
		struct AstExprElem      expr_elem;
		struct AstExprSizeof    expr_sizeof;
		struct AstExprTypeInfo  expr_type_info;
		struct AstExprAlignof   expr_alignof;
		struct AstExprTypeof    expr_typeof;
		struct AstExprUnary     expr_unary;
		struct AstExprAddrOf    expr_addrof;
		struct AstExprDeref     expr_deref;
		struct AstExprCompound  expr_compound;
	} data;

#if BL_DEBUG
	int32_t _serial;
#endif
};

void ast_arena_init(struct Arena *arena);

static inline bool ast_binop_is_assign(BinopKind op)
{
	return op >= BINOP_ASSIGN && op <= BINOP_MOD_ASSIGN;
}

static inline bool ast_binop_is_logic(BinopKind op)
{
	return op >= BINOP_EQ && op <= BINOP_LOGIC_OR;
}

static inline bool ast_is_expr(Ast *node)
{
	assert(node);
	return node->kind > _AST_EXPR_FIRST && node->kind < _AST_EXPR_LAST;
}

static inline bool ast_is_decl(Ast *node)
{
	assert(node);
	return node->kind > _AST_DECL_FIRST && node->kind < _AST_DECL_LAST;
}

static inline bool ast_is_type(Ast *node)
{
	assert(node);
	return node->kind > _AST_TYPE_FIRST && node->kind < _AST_TYPE_LAST;
}

Ast *ast_create_node(struct Arena *arena, AstKind c, Token *tok);

const char *ast_binop_to_str(BinopKind op);

const char *ast_unop_to_str(UnopKind op);

const char *ast_get_name(const Ast *n);

#endif
