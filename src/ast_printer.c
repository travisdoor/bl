// =================================================================================================
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
// =================================================================================================

#include "ast.h"
#include "builder.h"
#include "common.h"
#include <stdio.h>

#define MAX_STR_BUF 256
#define int_to_void_ptr(i) (void *)((intptr_t)(i))

static INLINE void print_address(Ast *node, FILE *stream)
{
#if BL_DEBUG
    if (node)
        fprintf(stream, " %llu ", node->_serial);
    else
        fprintf(stream, " (null) ");
#else
    fprintf(stream, " %p ", node);
#endif
}

#define print_head(_node, _pad, _stream) _print_head((Ast *)(_node), (_pad), (_stream))

static INLINE void _print_head(Ast *node, s32 pad, FILE *stream)
{
    if (node->location)
        fprintf(stream,
                "\n%*s%s <%d:%d>",
                pad * 2,
                "",
                ast_get_name(node),
                node->location->line,
                node->location->col);
    else
        fprintf(stream, "\n%*s%s <IMPLICIT>", pad * 2, "", ast_get_name(node));

    print_address(node, stream);
}

static INLINE void print_flags(s32 flags, FILE *stream)
{
    if (!flags) return;
    if (IS_FLAG(flags, FLAG_EXTERN)) fprintf(stream, " #extern");
    if (IS_FLAG(flags, FLAG_TEST_FN)) fprintf(stream, " #test");
    if (IS_FLAG(flags, FLAG_COMPILER)) fprintf(stream, " #compiler");
    if (IS_FLAG(flags, FLAG_PRIVATE)) fprintf(stream, " #private");
}

static void print_node(Ast *node, s32 pad, FILE *stream);
static void print_ublock(Ast *ublock, s32 pad, FILE *stream);
static void print_load(Ast *load, s32 pad, FILE *stream);
static void print_import(Ast *import, s32 pad, FILE *stream);
static void print_link(Ast *link, s32 pad, FILE *stream);
static void print_private(Ast *private, s32 pad, FILE *stream);
static void print_scope(Ast *scope, s32 pad, FILE *stream);
static void print_call_loc(Ast *call_loc, s32 pad, FILE *stream);
static void print_block(Ast *block, s32 pad, FILE *stream);
static void print_unrecheable(Ast *unr, s32 pad, FILE *stream);
static void print_ref(Ast *ref, s32 pad, FILE *stream);
static void print_type_polymorph(Ast *poly, s32 pad, FILE *stream);
static void print_type_struct(Ast *strct, s32 pad, FILE *stream);
static void print_type_enum(Ast *enm, s32 pad, FILE *stream);
static void print_type_fn_group(Ast *group, s32 pad, FILE *stream);
static void print_type_fn(Ast *fn, s32 pad, FILE *stream);
static void print_stmt_if(Ast *stmt_if, s32 pad, FILE *stream);
static void print_stmt_switch(Ast *stmt_switch, s32 pad, FILE *stream);
static void print_stmt_case(Ast *stmt_case, s32 pad, FILE *stream);
static void print_stmt_loop(Ast *loop, s32 pad, FILE *stream);
static void print_stmt_break(Ast *br, s32 pad, FILE *stream);
static void print_stmt_continue(Ast *cnt, s32 pad, FILE *stream);
static void print_stmt_return(Ast *ret, s32 pad, FILE *stream);
static void print_stmt_defer(Ast *defer, s32 pad, FILE *stream);
static void print_decl_entity(Ast *entity, s32 pad, FILE *stream);
static void print_decl_arg(Ast *arg, s32 pad, FILE *stream);
static void print_decl_member(Ast *member, s32 pad, FILE *stream);
static void print_decl_variant(Ast *variant, s32 pad, FILE *stream);
static void print_bad(Ast *bad, s32 pad, FILE *stream);
static void print_expr_line(Ast *line, s32 pad, FILE *stream);
static void print_expr_file(Ast *file, s32 pad, FILE *stream);
static void print_expr_unary(Ast *unary, s32 pad, FILE *stream);
static void print_expr_cast(Ast *cast, s32 pad, FILE *stream);
static void print_expr_addrof(Ast *addrof, s32 pad, FILE *stream);
static void print_expr_sizeof(Ast *szof, s32 pad, FILE *stream);
static void print_expr_type_info(Ast *type_info, s32 pad, FILE *stream);
static void print_expr_test_cases(Ast *type_info, s32 pad, FILE *stream);
static void print_expr_deref(Ast *deref, s32 pad, FILE *stream);
static void print_expr_binop(Ast *binop, s32 pad, FILE *stream);
static void print_expr_type(Ast *expr_type, s32 pad, FILE *stream);
static void print_expr_compound(Ast *expr_compound, s32 pad, FILE *stream);
static void print_expr_lit_int(Ast *lit, s32 pad, FILE *stream);
static void print_expr_lit_float(Ast *lit, s32 pad, FILE *stream);
static void print_expr_lit_double(Ast *lit, s32 pad, FILE *stream);
static void print_expr_lit_char(Ast *lit, s32 pad, FILE *stream);
static void print_expr_lit_bool(Ast *lit, s32 pad, FILE *stream);
static void print_expr_lit_string(Ast *lit, s32 pad, FILE *stream);
static void print_expr_lit_fn(Ast *fn, s32 pad, FILE *stream);
static void print_expr_lit_fn_group(Ast *group, s32 pad, FILE *stream);
static void print_expr_call(Ast *call, s32 pad, FILE *stream);
static void print_expr_elem(Ast *elem, s32 pad, FILE *stream);

// impl
void print_ublock(Ast *ublock, s32 pad, FILE *stream)
{
    print_head(ublock, pad, stream);
    fprintf(stream, "%s", ublock->data.ublock.unit->name);

    Ast *tmp = NULL;
    TARRAY_FOREACH(Ast *, ublock->data.ublock.nodes, tmp) print_node(tmp, pad + 1, stream);
}

void print_block(Ast *block, s32 pad, FILE *stream)
{
    print_head(block, pad, stream);
    Ast *tmp = NULL;
    TSA_FOREACH(block->data.block.nodes, tmp) print_node(tmp, pad + 1, stream);
}

void print_load(Ast *load, s32 pad, FILE *stream)
{
    print_head(load, pad, stream);
    fprintf(stream, "%s", load->data.load.filepath);
}

void print_import(Ast *import, s32 pad, FILE *stream)
{
    print_head(import, pad, stream);
    fprintf(stream, "%s", import->data.load.filepath);
}

void print_link(Ast *link, s32 pad, FILE *stream)
{
    print_head(link, pad, stream);
    fprintf(stream, "%s", link->data.link.lib);
}

void print_private(Ast *private, s32 pad, FILE *stream)
{
    print_head(private, pad, stream);
}

void print_scope(Ast *scope, s32 pad, FILE *stream)
{
    print_head(scope, pad, stream);
    Ast *ident = scope->data.scope.ident;
    if (ident) fprintf(stream, "'%s' ", ident->data.ident.id.str);
}

void print_call_loc(Ast *call_loc, s32 pad, FILE *stream)
{
    print_head(call_loc, pad, stream);
}

void print_unrecheable(Ast *unr, s32 pad, FILE *stream)
{
    print_head(unr, pad, stream);
}

void print_type_polymorph(Ast *poly, s32 pad, FILE *stream)
{
    print_head(poly, pad, stream);
    Ast *ident = poly->data.type_polymorph.ident;
    if (ident) fprintf(stream, "'%s' ", ident->data.ident.id.str);
}

void print_type_struct(Ast *strct, s32 pad, FILE *stream)
{
    print_head(strct, pad, stream);

    Ast *node;
    TSA_FOREACH(strct->data.type_strct.members, node)
    {
        print_node(node, pad + 1, stream);
    }
}

void print_ref(Ast *ref, s32 pad, FILE *stream)
{
    print_head(ref, pad, stream);

    Ast *ident = ref->data.ref.ident;
    if (ident) fprintf(stream, "'%s' ", ident->data.ident.id.str);

    Ast *next = ref->data.ref.next;
    if (next) print_node(next, pad + 1, stream);
}

void print_type_fn_group(Ast *group, s32 pad, FILE *stream)
{
    print_head(group, pad, stream);

    Ast *node;
    TSA_FOREACH(group->data.type_fn_group.variants, node)
    {
        print_node(node, pad + 1, stream);
    }
}

void print_type_fn(Ast *fn, s32 pad, FILE *stream)
{
    print_head(fn, pad, stream);
    TSmallArray_AstPtr *args = fn->data.type_fn.args;
    if (args) {
        Ast *node;
        TSA_FOREACH(args, node)
        {
            print_node(node, pad + 1, stream);
        }
    }
    print_node(fn->data.type_fn.ret_type, pad + 1, stream);
}

void print_type_enum(Ast *enm, s32 pad, FILE *stream)
{
    print_head(enm, pad, stream);

    Ast *node;
    TSA_FOREACH(enm->data.type_enm.variants, node)
    {
        print_node(node, pad + 1, stream);
    }
}

void print_stmt_if(Ast *stmt_if, s32 pad, FILE *stream)
{
    print_head(stmt_if, pad, stream);
    print_node(stmt_if->data.stmt_if.test, pad + 1, stream);
    print_node(stmt_if->data.stmt_if.true_stmt, pad + 1, stream);
    print_node(stmt_if->data.stmt_if.false_stmt, pad + 1, stream);
}

void print_stmt_switch(Ast *stmt_switch, s32 pad, FILE *stream)
{
    print_head(stmt_switch, pad, stream);
    print_node(stmt_switch->data.stmt_switch.expr, pad + 1, stream);

    TSmallArray_AstPtr *cases = stmt_switch->data.stmt_switch.cases;
    Ast *               stmt_case;

    TSA_FOREACH(cases, stmt_case)
    {
        print_node(stmt_case, pad + 1, stream);
    }
}

void print_stmt_case(Ast *stmt_case, s32 pad, FILE *stream)
{
    print_head(stmt_case, pad, stream);
    if (stmt_case->data.stmt_case.is_default) fprintf(stream, "default");

    if (stmt_case->data.stmt_case.exprs) {
        TSmallArray_AstPtr *exprs = stmt_case->data.stmt_case.exprs;
        Ast *               expr;

        TSA_FOREACH(exprs, expr)
        {
            print_node(expr, pad + 1, stream);
        }
    }

    if (stmt_case->data.stmt_case.block) {
        print_node(stmt_case->data.stmt_case.block, pad + 1, stream);
    }
}

void print_stmt_loop(Ast *loop, s32 pad, FILE *stream)
{
    print_head(loop, pad, stream);
    print_node(loop->data.stmt_loop.init, pad + 1, stream);
    print_node(loop->data.stmt_loop.condition, pad + 1, stream);
    print_node(loop->data.stmt_loop.increment, pad + 1, stream);
    print_node(loop->data.stmt_loop.block, pad + 1, stream);
}

void print_stmt_break(Ast *br, s32 pad, FILE *stream)
{
    print_head(br, pad, stream);
}

void print_stmt_continue(Ast *cnt, s32 pad, FILE *stream)
{
    print_head(cnt, pad, stream);
}

void print_stmt_return(Ast *ret, s32 pad, FILE *stream)
{
    print_head(ret, pad, stream);
    TSmallArray_AstPtr *exprs = ret->data.stmt_return.exprs;
    if (exprs) {
        Ast *value;
        TSA_FOREACH(exprs, value)
        {
            print_node(value, pad + 1, stream);
        }
    }
}

void print_stmt_defer(Ast *defer, s32 pad, FILE *stream)
{
    print_head(defer, pad, stream);
    print_node(defer->data.stmt_defer.expr, pad + 1, stream);
}

void print_decl_entity(Ast *entity, s32 pad, FILE *stream)
{
    print_head(entity, pad, stream);

    fprintf(stream,
            "'%s' '%s'",
            entity->data.decl.name->data.ident.id.str,
            entity->data.decl_entity.mut ? "mutable" : "immutable");

    print_flags(entity->data.decl_entity.flags, stream);
    print_node((Ast *)entity->data.decl.type, pad + 1, stream);
    print_node((Ast *)entity->data.decl_entity.value, pad + 1, stream);
}

void print_decl_arg(Ast *arg, s32 pad, FILE *stream)
{
    print_head(arg, pad, stream);
    fprintf(stream, "'%s'", arg->data.decl.name->data.ident.id.str);
    print_node(arg->data.decl.type, pad + 1, stream);
}

void print_decl_member(Ast *member, s32 pad, FILE *stream)
{
    print_head(member, pad, stream);
    fprintf(stream, "'%s'", member->data.decl.name->data.ident.id.str);
    print_node(member->data.decl.type, pad + 1, stream);
}

void print_decl_variant(Ast *variant, s32 pad, FILE *stream)
{
    print_head(variant, pad, stream);
    fprintf(stream, "'%s'", variant->data.decl.name->data.ident.id.str);
    print_node(variant->data.decl.type, pad + 1, stream);
}

void print_bad(Ast *bad, s32 pad, FILE *stream)
{
    print_head(bad, pad, stream);
}

void print_expr_cast(Ast *cast, s32 pad, FILE *stream)
{
    print_head(cast, pad, stream);
    if (cast->data.expr_cast.auto_cast) {
        fprintf(stream, "<auto>");
    } else {
        print_node(cast->data.expr_cast.type, pad + 1, stream);
    }
    print_node(cast->data.expr_cast.next, pad + 1, stream);
}

void print_expr_line(Ast *line, s32 pad, FILE *stream)
{
    print_head(line, pad, stream);
}

void print_expr_file(Ast *file, s32 pad, FILE *stream)
{
    print_head(file, pad, stream);
}

void print_expr_unary(Ast *unary, s32 pad, FILE *stream)
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
    case UNOP_BIT_NOT:
        op = "~";
        break;
    }

    fprintf(stream, "'%s' ", op);
    print_node(unary->data.expr_unary.next, pad + 1, stream);
}

void print_expr_addrof(Ast *addrof, s32 pad, FILE *stream)
{
    print_head(addrof, pad, stream);
    print_node(addrof->data.expr_addrof.next, pad + 1, stream);
}

void print_expr_sizeof(Ast *szof, s32 pad, FILE *stream)
{
    print_head(szof, pad, stream);
    print_node(szof->data.expr_sizeof.node, pad + 1, stream);
}

void print_expr_type_info(Ast *type_info, s32 pad, FILE *stream)
{
    print_head(type_info, pad, stream);
    print_node(type_info->data.expr_type_info.node, pad + 1, stream);
}

void print_expr_test_cases(Ast *type_info, s32 pad, FILE *stream)
{
    print_head(type_info, pad, stream);
}

void print_expr_deref(Ast *deref, s32 pad, FILE *stream)
{
    print_head(deref, pad, stream);
    print_node(deref->data.expr_deref.next, pad + 1, stream);
}

void print_expr_elem(Ast *elem, s32 pad, FILE *stream)
{
    print_head(elem, pad, stream);
    print_node(elem->data.expr_elem.index, pad + 1, stream);
    print_node(elem->data.expr_elem.next, pad + 1, stream);
}

void print_expr_binop(Ast *binop, s32 pad, FILE *stream)
{
    print_head(binop, pad, stream);
    fprintf(stream, "'%s' ", ast_binop_to_str(binop->data.expr_binop.kind));
    print_node(binop->data.expr_binop.lhs, pad + 1, stream);
    print_node(binop->data.expr_binop.rhs, pad + 1, stream);
}

void print_expr_type(Ast *expr_type, s32 pad, FILE *stream)
{
    print_head(expr_type, pad, stream);
    print_node(expr_type->data.expr_type.type, pad + 1, stream);
}

void print_expr_lit_int(Ast *lit, s32 pad, FILE *stream)
{
    print_head(lit, pad, stream);
    fprintf(stream, "%llu ", (long long unsigned)lit->data.expr_integer.val);
}

void print_expr_lit_float(Ast *lit, s32 pad, FILE *stream)
{
    print_head(lit, pad, stream);
    fprintf(stream, "%f ", lit->data.expr_float.val);
}

void print_expr_lit_double(Ast *lit, s32 pad, FILE *stream)
{
    print_head(lit, pad, stream);
    fprintf(stream, "%f ", lit->data.expr_double.val);
}

void print_expr_lit_char(Ast *lit, s32 pad, FILE *stream)
{
    print_head(lit, pad, stream);
    fprintf(stream, "%c ", lit->data.expr_character.val);
}

void print_expr_lit_bool(Ast *lit, s32 pad, FILE *stream)
{
    print_head(lit, pad, stream);
    fprintf(stream, "%s ", lit->data.expr_boolean.val ? "true" : "false");
}

void print_expr_lit_string(Ast *lit, s32 pad, FILE *stream)
{
    print_head(lit, pad, stream);

    char *tmp = strdup(lit->data.expr_string.val);
    fprintf(stream, "%s ", strtok(tmp, "\n"));
    char *next = strtok(NULL, "\n");
    if (next && strlen(next)) fprintf(stream, "... ");
    free(tmp);
}

void print_expr_lit_fn(Ast *fn, s32 pad, FILE *stream)
{
    print_head(fn, pad, stream);
    print_node(fn->data.expr_fn.type, pad + 1, stream);
    print_node(fn->data.expr_fn.block, pad + 1, stream);
}

void print_expr_lit_fn_group(Ast *group, s32 pad, FILE *stream)
{
    print_head(group, pad, stream);
    Ast *tmp = NULL;
    TSA_FOREACH(group->data.expr_fn_group.variants, tmp)
    {
        print_node(tmp, pad + 1, stream);
    }
}

void print_expr_call(Ast *call, s32 pad, FILE *stream)
{
    print_head(call, pad, stream);

    print_node(call->data.expr_call.ref, pad + 1, stream);

    if (call->data.expr_call.args) {
        Ast *arg;
        TSA_FOREACH(call->data.expr_call.args, arg) print_node(arg, pad + 1, stream);
    }
}

void print_expr_compound(Ast *expr_compound, s32 pad, FILE *stream)
{
    print_head(expr_compound, pad, stream);

    TSmallArray_AstPtr *exprs = expr_compound->data.expr_compound.values;
    if (exprs) {
        Ast *value;
        TSA_FOREACH(exprs, value)
        {
            print_node(value, pad + 1, stream);
        }
    }
}

void print_node(Ast *node, s32 pad, FILE *stream)
{
    if (!node) return;
    switch (node->kind) {
    case AST_BAD:
        print_bad(node, pad, stream);
        break;

    case AST_LOAD:
        print_load(node, pad, stream);
        break;

    case AST_IMPORT:
        print_import(node, pad, stream);
        break;

    case AST_LINK:
        print_link(node, pad, stream);
        break;

    case AST_PRIVATE:
        print_private(node, pad, stream);
        break;

    case AST_SCOPE:
        print_scope(node, pad, stream);
        break;

    case AST_CALL_LOC:
        print_call_loc(node, pad, stream);
        break;

    case AST_IDENT:
        break;

    case AST_UBLOCK:
        print_ublock(node, pad, stream);
        break;

    case AST_BLOCK:
        print_block(node, pad, stream);
        break;

    case AST_UNREACHABLE:
        print_unrecheable(node, pad, stream);
        break;

    case AST_TYPE_STRUCT:
        print_type_struct(node, pad, stream);
        break;

    case AST_TYPE_FN:
        print_type_fn(node, pad, stream);
        break;

    case AST_TYPE_ENUM:
        print_type_enum(node, pad, stream);
        break;

    case AST_TYPE_FN_GROUP:
        print_type_fn_group(node, pad, stream);
        break;

    case AST_TYPE_POLYMORPH:
        print_type_polymorph(node, pad, stream);
        break;

    case AST_REF:
        print_ref(node, pad, stream);
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

    case AST_STMT_DEFER:
        print_stmt_defer(node, pad, stream);
        break;

    case AST_STMT_IF:
        print_stmt_if(node, pad, stream);
        break;

    case AST_STMT_SWITCH:
        print_stmt_switch(node, pad, stream);
        break;

    case AST_STMT_CASE:
        print_stmt_case(node, pad, stream);
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

    case AST_EXPR_CAST:
        print_expr_cast(node, pad, stream);
        break;

    case AST_EXPR_BINOP:
        print_expr_binop(node, pad, stream);
        break;

    case AST_EXPR_CALL:
        print_expr_call(node, pad, stream);
        break;

    case AST_EXPR_ELEM:
        print_expr_elem(node, pad, stream);
        break;

    case AST_EXPR_SIZEOF:
        print_expr_sizeof(node, pad, stream);
        break;

    case AST_EXPR_TYPE_INFO:
        print_expr_type_info(node, pad, stream);
        break;

    case AST_EXPR_TEST_CASES:
        print_expr_test_cases(node, pad, stream);
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

    case AST_EXPR_LIT_FN_GROUP:
        print_expr_lit_fn_group(node, pad, stream);
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

void ast_printer_run(Assembly *assembly)
{
    Unit *unit;
    TARRAY_FOREACH(Unit *, &assembly->units, unit)
    {
        print_node(unit->ast, 0, stdout);
    }
    fprintf(stdout, "\n\n");
}
