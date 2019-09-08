//************************************************************************************************
// bl
//
// File:   ast.c
// Author: Martin Dorazil
// Date:   15/03/2018
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

#include "ast.h"
#include "token.h"

#define ARENA_CHUNK_COUNT 256

static void
node_dtor(Ast *node)
{
	switch (node->kind) {
	case AST_UBLOCK:
		bo_unref(node->data.ublock.nodes);
		break;
	case AST_BLOCK:
		bo_unref(node->data.block.nodes);
		break;
	default:
		break;
	}
}

static void
small_array_dtor(SmallArrayAny *arr)
{
	sa_terminate(arr);
}

Ast *
ast_create_node(Arena *arena, AstKind c, struct Token *tok, struct Scope *parent_scope)
{
	Ast *node         = arena_alloc(arena);
	node->kind        = c;
	node->owner_scope = parent_scope;
	node->location    = tok ? &tok->location : NULL;

#if BL_DEBUG
	static uint64_t serial = 0;
	node->_serial          = serial++;
#endif
	return node;
}

/* public */
void
ast_arena_init(Arena *arena)
{
	arena_init(arena, sizeof(Ast), ARENA_CHUNK_COUNT, (ArenaElemDtor)node_dtor);
}

void
ast_arena_terminate(Arena *arena)
{
	arena_terminate(arena);
}

const char *
ast_get_name(const Ast *n)
{
	assert(n);
	switch (n->kind) {
	case AST_BAD:
		return "Bad";
	case AST_LOAD:
		return "Load";
	case AST_LINK:
		return "Link";
	case AST_PRIVATE:
		return "Private";
	case AST_IDENT:
		return "Ident";
	case AST_UBLOCK:
		return "UBlock";
	case AST_BLOCK:
		return "Block";
	case AST_TEST_CASE:
		return "TestCase";
	case AST_UNREACHABLE:
		return "Unreachable";
	case AST_STMT_RETURN:
		return "StmtReturn";
	case AST_STMT_DEFER:
		return "StmtDefer";
	case AST_STMT_IF:
		return "StmtIf";
	case AST_STMT_LOOP:
		return "StmtLoop";
	case AST_STMT_BREAK:
		return "StmtBreak";
	case AST_STMT_CONTINUE:
		return "StmtContinue";
	case AST_DECL_ENTITY:
		return "DeclEntity";
	case AST_DECL_MEMBER:
		return "DeclMember";
	case AST_DECL_ARG:
		return "DeclArg";
	case AST_DECL_VARIANT:
		return "DeclVariant";
	case AST_TYPE_REF:
		return "TypeRef";
	case AST_TYPE_ARR:
		return "TypeArr";
	case AST_TYPE_SLICE:
		return "TypeSlice";
	case AST_TYPE_FN:
		return "TypeFn";
	case AST_TYPE_STRUCT:
		return "TypeStruct";
	case AST_TYPE_ENUM:
		return "TypeEnum";
	case AST_TYPE_PTR:
		return "TypePtr";
	case AST_TYPE_VARGS:
		return "TypeVargs";
	case AST_EXPR_FILE:
		return "ExprFile";
	case AST_EXPR_LINE:
		return "ExprLine";
	case AST_EXPR_REF:
		return "ExprRef";
	case AST_EXPR_CAST:
		return "ExprCast";
	case AST_EXPR_BINOP:
		return "ExprBinop";
	case AST_EXPR_CALL:
		return "ExprCall";
	case AST_EXPR_MEMBER:
		return "ExprMember";
	case AST_EXPR_ELEM:
		return "ExprElem";
	case AST_EXPR_SIZEOF:
		return "ExprSizeof";
	case AST_EXPR_TYPE_INFO:
		return "ExprTypeInfo";
	case AST_EXPR_ALIGNOF:
		return "ExprAlignof";
	case AST_EXPR_UNARY:
		return "ExprUnary";
	case AST_EXPR_NULL:
		return "ExprNull";
	case AST_EXPR_ADDROF:
		return "ExprAddrOf";
	case AST_EXPR_DEREF:
		return "ExprDeref";
	case AST_EXPR_COMPOUND:
		return "ExprCompound";
	case AST_EXPR_LIT_FN:
		return "ExprLitFn";
	case AST_EXPR_LIT_INT:
		return "ExprLitInt";
	case AST_EXPR_LIT_FLOAT:
		return "ExprLitFloat";
	case AST_EXPR_LIT_DOUBLE:
		return "ExprLitDouble";
	case AST_EXPR_LIT_CHAR:
		return "ExprLitChar";
	case AST_EXPR_LIT_STRING:
		return "ExprLitString";
	case AST_EXPR_LIT_BOOL:
		return "ExprLitBool";

	default:
		bl_abort("invalid ast node");
	}
}

const char *
ast_binop_to_str(BinopKind op)
{
	switch (op) {
	case BINOP_INVALID:
		return "<invalid>";
	case BINOP_ASSIGN:
		return "=";
	case BINOP_ADD_ASSIGN:
		return "+=";
	case BINOP_SUB_ASSIGN:
		return "-=";
	case BINOP_MUL_ASSIGN:
		return "*=";
	case BINOP_DIV_ASSIGN:
		return "/=";
	case BINOP_MOD_ASSIGN:
		return "%=";
	case BINOP_ADD:
		return "+";
	case BINOP_SUB:
		return "-";
	case BINOP_MUL:
		return "*";
	case BINOP_DIV:
		return "/";
	case BINOP_MOD:
		return "%";
	case BINOP_EQ:
		return "==";
	case BINOP_NEQ:
		return "!=";
	case BINOP_GREATER:
		return ">";
	case BINOP_LESS:
		return "<";
	case BINOP_GREATER_EQ:
		return ">=";
	case BINOP_LESS_EQ:
		return "<=";
	case BINOP_LOGIC_AND:
		return "&&";
	case BINOP_LOGIC_OR:
		return "||";
	case BINOP_AND:
		return "&";
	case BINOP_OR:
		return "|";
	case BINOP_SHR:
		return ">>";
	case BINOP_SHL:
		return "<<";
	}

	return "invalid";
}

const char *
ast_unop_to_str(UnopKind op)
{
	switch (op) {
	case UNOP_NEG:
		return "-";
	case UNOP_POS:
		return "+";
	case UNOP_NOT:
		return "!";
	default:
		return "invalid";
	}
}
