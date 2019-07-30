//************************************************************************************************
// blc
//
// File:   ast_printer.c
// Author: Martin Dorazil
// Date:   04/02/2018
//
// Copyright 2018 Martin Dorazil
//
// Permissicopy
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
#include "common.h"
#include "stages.h"
#include <stdio.h>

#define MAX_STR_BUF 256
#define int_to_void_ptr(i) (void *)((intptr_t)(i))

static inline void
print_address(Ast *node, FILE *stream)
{
#if BL_DEBUG
	if (node)
		fprintf(stream, YELLOW(" %d "), node->_serial);
	else
		fprintf(stream, RED(" (null) "));
#else
	fprintf(stream, YELLOW(" %p "), node);
#endif
}

#define print_head(_node, _pad, _stream) _print_head((Ast *)(_node), (_pad), (_stream))

static inline void
_print_head(Ast *node, int32_t pad, FILE *stream)
{
	if (node->src)
		fprintf(stream,
		        "\n%*s" GREEN("%s ") CYAN("<%d:%d>"),
		        pad * 2,
		        "",
		        ast_get_name(node),
		        node->src->line,
		        node->src->col);
	else
		fprintf(stream,
		        "\n%*s" GREEN("%s ") CYAN("<IMPLICIT>"),
		        pad * 2,
		        "",
		        ast_get_name(node));

	print_address(node, stream);
}

static inline void
print_flags(int32_t flags, FILE *stream)
{
	if (!flags) return;
	if (is_flag(flags, FLAG_EXTERN)) fprintf(stream, " #extern");
	if (is_flag(flags, FLAG_TEST)) fprintf(stream, " #test");
	if (is_flag(flags, FLAG_COMPILER)) fprintf(stream, " #compiler");
	if (is_flag(flags, FLAG_PRIVATE)) fprintf(stream, " #private");
}

static void
print_node(Ast *node, int32_t pad, FILE *stream);

static void
print_ublock(Ast *ublock, int32_t pad, FILE *stream);

static void
print_test_case(Ast *test, int32_t pad, FILE *stream);

static void
print_load(Ast *load, int32_t pad, FILE *stream);

static void
print_link(Ast *link, int32_t pad, FILE *stream);

static void
print_private(Ast *private, int32_t pad, FILE *stream);

static void
print_block(Ast *block, int32_t pad, FILE *stream);

static void
print_unrecheable(Ast *unr, int32_t pad, FILE *stream);

static void
print_type_struct(Ast *strct, int32_t pad, FILE *stream);

static void
print_type_enum(Ast *enm, int32_t pad, FILE *stream);

static void
print_stmt_if(Ast *stmt_if, int32_t pad, FILE *stream);

static void
print_stmt_loop(Ast *loop, int32_t pad, FILE *stream);

static void
print_stmt_break(Ast *br, int32_t pad, FILE *stream);

static void
print_stmt_continue(Ast *cnt, int32_t pad, FILE *stream);

static void
print_stmt_return(Ast *ret, int32_t pad, FILE *stream);

static void
print_decl_entity(Ast *entity, int32_t pad, FILE *stream);

static void
print_decl_arg(Ast *arg, int32_t pad, FILE *stream);

static void
print_decl_member(Ast *member, int32_t pad, FILE *stream);

static void
print_decl_variant(Ast *variant, int32_t pad, FILE *stream);

static void
print_bad(Ast *bad, int32_t pad, FILE *stream);

static void
print_expr_unary(Ast *unary, int32_t pad, FILE *stream);

static void
print_expr_cast(Ast *cast, int32_t pad, FILE *stream);

static void
print_expr_member(Ast *member, int32_t pad, FILE *stream);

static void
print_expr_addrof(Ast *addrof, int32_t pad, FILE *stream);

static void
print_expr_sizeof(Ast *szof, int32_t pad, FILE *stream);

static void
print_expr_type_kind(Ast *type_kind, int32_t pad, FILE *stream);

static void
print_expr_type_info(Ast *type_info, int32_t pad, FILE *stream);

static void
print_expr_deref(Ast *deref, int32_t pad, FILE *stream);

static void
print_expr_binop(Ast *binop, int32_t pad, FILE *stream);

static void
print_expr_type(Ast *expr_type, int32_t pad, FILE *stream);

static void
print_expr_compound(Ast *expr_compound, int32_t pad, FILE *stream);

static void
print_expr_ref(Ast *ref, int32_t pad, FILE *stream);

static void
print_expr_lit_int(Ast *lit, int32_t pad, FILE *stream);

static void
print_expr_lit_float(Ast *lit, int32_t pad, FILE *stream);

static void
print_expr_lit_double(Ast *lit, int32_t pad, FILE *stream);

static void
print_expr_lit_char(Ast *lit, int32_t pad, FILE *stream);

static void
print_expr_lit_bool(Ast *lit, int32_t pad, FILE *stream);

static void
print_expr_lit_string(Ast *lit, int32_t pad, FILE *stream);

static void
print_expr_lit_fn(Ast *fn, int32_t pad, FILE *stream);

static void
print_expr_call(Ast *call, int32_t pad, FILE *stream);

static void
print_expr_elem(Ast *elem, int32_t pad, FILE *stream);

/* impl */
void
print_ublock(Ast *ublock, int32_t pad, FILE *stream)
{
	print_head(ublock, pad, stream);
	fprintf(stream, "%s", ublock->data.ublock.unit->name);

	Ast *tmp = NULL;
	barray_foreach(ublock->data.ublock.nodes, tmp) print_node(tmp, pad + 1, stream);
}

void
print_block(Ast *block, int32_t pad, FILE *stream)
{
	print_head(block, pad, stream);
	Ast *tmp = NULL;
	barray_foreach(block->data.block.nodes, tmp) print_node(tmp, pad + 1, stream);
}

void
print_test_case(Ast *test, int32_t pad, FILE *stream)
{
	print_head(test, pad, stream);
	fprintf(stream, "%s", test->data.test_case.desc);
	print_node(test->data.test_case.block, pad + 1, stream);
}

void
print_load(Ast *load, int32_t pad, FILE *stream)
{
	print_head(load, pad, stream);
	fprintf(stream, "%s", load->data.load.filepath);
}

void
print_link(Ast *link, int32_t pad, FILE *stream)
{
	print_head(link, pad, stream);
	fprintf(stream, "%s", link->data.link.lib);
}

void
print_private(Ast *private, int32_t pad, FILE *stream)
{
	print_head(private, pad, stream);
}

void
print_unrecheable(Ast *unr, int32_t pad, FILE *stream)
{
	print_head(unr, pad, stream);
}

void
print_type_struct(Ast *strct, int32_t pad, FILE *stream)
{
	print_head(strct, pad, stream);

	Ast *node;
	barray_foreach(strct->data.type_strct.members, node)
	{
		print_node(node, pad + 1, stream);
	}
}

void
print_type_enum(Ast *enm, int32_t pad, FILE *stream)
{
	print_head(enm, pad, stream);

	Ast *node;
	barray_foreach(enm->data.type_enm.variants, node)
	{
		print_node(node, pad + 1, stream);
	}
}

void
print_stmt_if(Ast *stmt_if, int32_t pad, FILE *stream)
{
	print_head(stmt_if, pad, stream);
	print_node(stmt_if->data.stmt_if.test, pad + 1, stream);
	print_node(stmt_if->data.stmt_if.true_stmt, pad + 1, stream);
	print_node(stmt_if->data.stmt_if.false_stmt, pad + 1, stream);
}

void
print_stmt_loop(Ast *loop, int32_t pad, FILE *stream)
{
	print_head(loop, pad, stream);
	print_node(loop->data.stmt_loop.init, pad + 1, stream);
	print_node(loop->data.stmt_loop.condition, pad + 1, stream);
	print_node(loop->data.stmt_loop.increment, pad + 1, stream);
	print_node(loop->data.stmt_loop.block, pad + 1, stream);
}

void
print_stmt_break(Ast *br, int32_t pad, FILE *stream)
{
	print_head(br, pad, stream);
}

void
print_stmt_continue(Ast *cnt, int32_t pad, FILE *stream)
{
	print_head(cnt, pad, stream);
}

void
print_stmt_return(Ast *ret, int32_t pad, FILE *stream)
{
	print_head(ret, pad, stream);
	print_node(ret->data.stmt_return.expr, pad + 1, stream);
}

void
print_decl_entity(Ast *entity, int32_t pad, FILE *stream)
{
	print_head(entity, pad, stream);

	fprintf(stream,
	        "'%s' '%s'",
	        entity->data.decl.name->data.ident.id.str,
	        entity->data.decl_entity.mutable ? "mutable" : "immutable");

	print_flags(entity->data.decl_entity.flags, stream);
	print_node((Ast *)entity->data.decl_entity.value, pad + 1, stream);
}

void
print_decl_arg(Ast *arg, int32_t pad, FILE *stream)
{
	print_head(arg, pad, stream);
	fprintf(stream, "'%s'", arg->data.decl.name->data.ident.id.str);
}

void
print_decl_member(Ast *member, int32_t pad, FILE *stream)
{
	print_head(member, pad, stream);
	fprintf(stream, "'%s'", member->data.decl.name->data.ident.id.str);
}

void
print_decl_variant(Ast *variant, int32_t pad, FILE *stream)
{
	print_head(variant, pad, stream);
	fprintf(stream, "'%s'", variant->data.decl.name->data.ident.id.str);
}

void
print_bad(Ast *bad, int32_t pad, FILE *stream)
{
	print_head(bad, pad, stream);
}

void
print_expr_cast(Ast *cast, int32_t pad, FILE *stream)
{
	print_head(cast, pad, stream);
	print_node(cast->data.expr_cast.type, pad + 1, stream);
	print_node(cast->data.expr_cast.next, pad + 1, stream);
}

void
print_expr_unary(Ast *unary, int32_t pad, FILE *stream)
{
	print_head(unary, pad, stream);

	const char *op = NULL;
	switch (unary->data.expr_unary.kind) {
	case UNOP_INVALID:
		op = "invalid";
		break;
	case UNOP_NEG:
		op = "-";
		break;
	case UNOP_POS:
		op = "+";
		break;
	case UNOP_NOT:
		op = "!";
		break;
	}

	fprintf(stream, "'%s' ", op);
	print_node(unary->data.expr_unary.next, pad + 1, stream);
}

void
print_expr_member(Ast *member, int32_t pad, FILE *stream)
{
	print_head(member, pad, stream);

	Ast *ident = member->data.expr_member.ident;
	if (ident) fprintf(stream, "'%s' ", ident->data.ident.id.str);
	print_node(member->data.expr_member.next, pad + 1, stream);
}

void
print_expr_addrof(Ast *addrof, int32_t pad, FILE *stream)
{
	print_head(addrof, pad, stream);
	print_node(addrof->data.expr_addrof.next, pad + 1, stream);
}

void
print_expr_sizeof(Ast *szof, int32_t pad, FILE *stream)
{
	print_head(szof, pad, stream);
	print_node(szof->data.expr_sizeof.node, pad + 1, stream);
}

void
print_expr_type_info(Ast *type_info, int32_t pad, FILE *stream)
{
	print_head(type_info, pad, stream);
	print_node(type_info->data.expr_type_info.node, pad + 1, stream);
}

void
print_expr_type_kind(Ast *type_kind, int32_t pad, FILE *stream)
{
	print_head(type_kind, pad, stream);
	print_node(type_kind->data.expr_type_kind.node, pad + 1, stream);
}

void
print_expr_deref(Ast *deref, int32_t pad, FILE *stream)
{
	print_head(deref, pad, stream);
	print_node(deref->data.expr_deref.next, pad + 1, stream);
}

void
print_expr_elem(Ast *elem, int32_t pad, FILE *stream)
{
	print_head(elem, pad, stream);
	print_node(elem->data.expr_elem.index, pad + 1, stream);
	print_node(elem->data.expr_elem.next, pad + 1, stream);
}

void
print_expr_binop(Ast *binop, int32_t pad, FILE *stream)
{
	print_head(binop, pad, stream);
	fprintf(stream, "'%s' ", ast_binop_to_str(binop->data.expr_binop.kind));
	print_node(binop->data.expr_binop.lhs, pad + 1, stream);
	print_node(binop->data.expr_binop.rhs, pad + 1, stream);
}

void
print_expr_type(Ast *expr_type, int32_t pad, FILE *stream)
{
	print_head(expr_type, pad, stream);
	print_node(expr_type->data.expr_type.type, pad + 1, stream);
}

void
print_expr_ref(Ast *ref, int32_t pad, FILE *stream)
{
	print_head(ref, pad, stream);
	fprintf(stream, "'%s' ", ref->data.expr_ref.ident->data.ident.id.str);
}

void
print_expr_lit_int(Ast *lit, int32_t pad, FILE *stream)
{
	print_head(lit, pad, stream);
	fprintf(stream, "%llu ", (long long unsigned)lit->data.expr_integer.val);
}

void
print_expr_lit_float(Ast *lit, int32_t pad, FILE *stream)
{
	print_head(lit, pad, stream);
	fprintf(stream, "%f ", lit->data.expr_float.val);
}

void
print_expr_lit_double(Ast *lit, int32_t pad, FILE *stream)
{
	print_head(lit, pad, stream);
	fprintf(stream, "%f ", lit->data.expr_double.val);
}

void
print_expr_lit_char(Ast *lit, int32_t pad, FILE *stream)
{
	print_head(lit, pad, stream);
	fprintf(stream, "%c ", lit->data.expr_character.val);
}

void
print_expr_lit_bool(Ast *lit, int32_t pad, FILE *stream)
{
	print_head(lit, pad, stream);
	fprintf(stream, "%s ", lit->data.expr_boolean.val ? "true" : "false");
}

void
print_expr_lit_string(Ast *lit, int32_t pad, FILE *stream)
{
	print_head(lit, pad, stream);

	char *tmp = strdup(lit->data.expr_string.val);
	fprintf(stream, "%s ", strtok(tmp, "\n"));
	char *next = strtok(NULL, "\n");
	if (next && strlen(next)) fprintf(stream, "... ");
	free(tmp);
}

void
print_expr_lit_fn(Ast *fn, int32_t pad, FILE *stream)
{
	print_head(fn, pad, stream);
	print_node(fn->data.expr_fn.block, pad + 1, stream);
}

void
print_expr_call(Ast *call, int32_t pad, FILE *stream)
{
	print_head(call, pad, stream);

	print_node(call->data.expr_call.ref, pad + 1, stream);

	if (call->data.expr_call.args) {
		Ast *arg;
		barray_foreach(call->data.expr_call.args, arg) print_node(arg, pad + 1, stream);
	}
}

void
print_expr_compound(Ast *expr_compound, int32_t pad, FILE *stream)
{
	print_head(expr_compound, pad, stream);

	BArray *exprs = expr_compound->data.expr_compound.values;
	if (exprs) {
		Ast *value;
		barray_foreach(exprs, value)
		{
			print_node(value, pad + 1, stream);
		}
	}
}

void
print_node(Ast *node, int32_t pad, FILE *stream)
{
	if (!node) return;
	switch (node->kind) {
	case AST_BAD:
		print_bad(node, pad, stream);
		break;

	case AST_LOAD:
		print_load(node, pad, stream);
		break;

	case AST_LINK:
		print_link(node, pad, stream);
		break;

	case AST_PRIVATE:
		print_private(node, pad, stream);
		break;

	case AST_IDENT:
		break;

	case AST_UBLOCK:
		print_ublock(node, pad, stream);
		break;

	case AST_BLOCK:
		print_block(node, pad, stream);
		break;

	case AST_TEST_CASE:
		print_test_case(node, pad, stream);
		break;

	case AST_UNREACHABLE:
		print_unrecheable(node, pad, stream);
		break;

	case AST_TYPE_STRUCT:
		print_type_struct(node, pad, stream);
		break;

	case AST_TYPE_ENUM:
		print_type_enum(node, pad, stream);
		break;

	case AST_DECL_ENTITY:
		print_decl_entity(node, pad, stream);
		break;

	case AST_DECL_ARG:
		print_decl_arg(node, pad, stream);
		break;

	case AST_DECL_MEMBER:
		print_decl_member(node, pad, stream);
		break;

	case AST_DECL_VARIANT:
		print_decl_variant(node, pad, stream);
		break;

	case AST_STMT_RETURN:
		print_stmt_return(node, pad, stream);
		break;

	case AST_STMT_IF:
		print_stmt_if(node, pad, stream);
		break;

	case AST_STMT_LOOP:
		print_stmt_loop(node, pad, stream);
		break;

	case AST_STMT_BREAK:
		print_stmt_break(node, pad, stream);
		break;

	case AST_STMT_CONTINUE:
		print_stmt_continue(node, pad, stream);
		break;

	case AST_EXPR_COMPOUND:
		print_expr_compound(node, pad, stream);
		break;

	case AST_EXPR_TYPE:
		print_expr_type(node, pad, stream);
		break;

	case AST_EXPR_REF:
		print_expr_ref(node, pad, stream);
		break;

	case AST_EXPR_CAST:
		print_expr_cast(node, pad, stream);
		break;

	case AST_EXPR_BINOP:
		print_expr_binop(node, pad, stream);
		break;

	case AST_EXPR_CALL:
		print_expr_call(node, pad, stream);
		break;

	case AST_EXPR_MEMBER:
		print_expr_member(node, pad, stream);
		break;

	case AST_EXPR_ELEM:
		print_expr_elem(node, pad, stream);
		break;

	case AST_EXPR_SIZEOF:
		print_expr_sizeof(node, pad, stream);
		break;

	case AST_EXPR_TYPE_KIND:
		print_expr_type_kind(node, pad, stream);
		break;

	case AST_EXPR_TYPE_INFO:
		print_expr_type_info(node, pad, stream);
		break;

	case AST_EXPR_UNARY:
		print_expr_unary(node, pad, stream);
		break;

	case AST_EXPR_ADDROF:
		print_expr_addrof(node, pad, stream);
		break;

	case AST_EXPR_DEREF:
		print_expr_deref(node, pad, stream);
		break;

	case AST_EXPR_NULL:
		break;

	case AST_EXPR_LIT_FN:
		print_expr_lit_fn(node, pad, stream);
		break;

	case AST_EXPR_LIT_INT:
		print_expr_lit_int(node, pad, stream);
		break;

	case AST_EXPR_LIT_FLOAT:
		print_expr_lit_float(node, pad, stream);
		break;

	case AST_EXPR_LIT_DOUBLE:
		print_expr_lit_double(node, pad, stream);
		break;

	case AST_EXPR_LIT_CHAR:
		print_expr_lit_char(node, pad, stream);
		break;

	case AST_EXPR_LIT_STRING:
		print_expr_lit_string(node, pad, stream);
		break;

	case AST_EXPR_LIT_BOOL:
		print_expr_lit_bool(node, pad, stream);
		break;

	default:
		break;
	}
}

void
ast_printer_run(Assembly *assembly, FILE *stream)
{
	Unit *unit;
	barray_foreach(assembly->units, unit)
	{
		print_node(unit->ast, 0, stream);
	}
	fprintf(stream, "\n\n");
}
