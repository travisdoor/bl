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

#include <stdio.h>
#include "stages.h"
#include "common.h"
#include "ast.h"

#define MAX_STR_BUF 256

static inline void
print_address(node_t *node)
{
#if BL_DEBUG
  if (node)
    fprintf(stdout, YELLOW(" %d "), node->_serial);
  else
    fprintf(stdout, RED(" (null) "));
#else
  fprintf(stdout, YELLOW(" %p "), node);
#endif
}

static inline void
print_head(const char *name, src_t *src, node_t *ptr, int pad)
{
  if (src)
    fprintf(stdout, "\n%*s" GREEN("%s ") CYAN("<%d:%d>"), pad * 2, "", name, src->line, src->col);
  else
    fprintf(stdout, "\n%*s" GREEN("%s ") CYAN("<IMPLICIT>"), pad * 2, "", name);

  print_address(ptr);
}

static inline void
print_type(node_t *type)
{
  if (!type) {
    fprintf(stdout, RED("{?}"));
    return;
  }
  char tmp[MAX_STR_BUF];
  ast_type_to_string(tmp, MAX_STR_BUF, type);
  fprintf(stdout, CYAN("{%s}"), tmp);
}

static inline void
print_flags(int flags)
{
  if (flags)
    fprintf(stdout, " #");
  else
    return;
  if (flags & FLAG_EXTERN) fprintf(stdout, "E");
  if (flags & FLAG_MAIN) fprintf(stdout, "M");
}

static void
print_load(visitor_t *visitor, node_t *node, void *pad);

static void
print_sizeof(visitor_t *visitor, node_t *node, void *pad);

static void
print_member(visitor_t *visitor, node_t *node, void *pad);

static void
print_elem(visitor_t *visitor, node_t *node, void *pad);

static void
print_unary(visitor_t *visitor, node_t *node, void *pad);

static void
print_break(visitor_t *visitor, node_t *node, void *pad);

static void
print_continue(visitor_t *visitor, node_t *node, void *pad);

static void
print_ublock(visitor_t *visitor, node_t *node, void *pad);

static void
print_type_struct(visitor_t *visitor, node_t *node, void *pad);

static void
print_decl(visitor_t *visitor, node_t *node, void *pad);

static void
print_block(visitor_t *visitor, node_t *node, void *pad);

static void
print_bad(visitor_t *visitor, node_t *node, void *pad);

static void
print_binop(visitor_t *visitor, node_t *node, void *pad);

static void
print_call(visitor_t *visitor, node_t *node, void *pad);

static void
print_lit(visitor_t *visitor, node_t *node, void *pad);

static void
print_lit_fn(visitor_t *visitor, node_t *node, void *pad);

static void
print_lit_struct(visitor_t *visitor, node_t *node, void *pad);

static void
print_lit_enum(visitor_t *visitor, node_t *node, void *pad);

static void
print_ident(visitor_t *visitor, node_t *node, void *pad);

static void
print_return(visitor_t *visitor, node_t *node, void *pad);

static void
print_if(visitor_t *visitor, node_t *node, void *pad);

static void
print_loop(visitor_t *visitor, node_t *node, void *pad);

static void
print_cast(visitor_t *visitor, node_t *node, void *pad);

static void
print_null(visitor_t *visitor, node_t *node, void *pad);

// impl
void
print_sizeof(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("sizeof", node->src, node, (intptr_t)pad);
  node_expr_sizeof_t *_sizeof = peek_expr_sizeof(node);
  print_type(_sizeof->in);
}

void
print_member(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("member", node->src, node, (intptr_t)pad);
  node_expr_member_t *_member = peek_expr_member(node);
  print_type(_member->type);
  fprintf(stdout, " (%s)", _member->ptr_ref ? "->" : ".");
  visitor_walk(visitor, node, pad + 1);
}

void
print_elem(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("elem", node->src, node, (intptr_t)pad);
  node_expr_elem_t *_elem = peek_expr_elem(node);
  print_type(_elem->type);
  visitor_walk(visitor, node, pad + 1);
}

void
print_load(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("load", node->src, node, (intptr_t)pad);
  node_load_t *_load = peek_load(node);
  fprintf(stdout, "'%s'", _load->filepath);
}

void
print_lit_struct(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("struct", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad + 1);
}

void
print_lit_enum(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("enum", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad + 1);
}

void
print_break(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("break", node->src, node, (intptr_t)pad);
}

void
print_continue(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("continue", node->src, node, (intptr_t)pad);
}

void
print_cast(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("cast", node->src, node, (intptr_t)pad);
  node_expr_cast_t *_cast = peek_expr_cast(node);
  print_type(_cast->type);
  visitor_walk(visitor, node, pad + 1);
}

void
print_null(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("null", node->src, node, (intptr_t)pad);
  node_expr_null_t *_null = peek_expr_null(node);
  print_type(_null->type);
}

void
print_unary(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("unary", node->src, node, (intptr_t)pad);
  node_expr_unary_t *_unary = peek_expr_unary(node);
  fprintf(stdout, "%s ", sym_strings[_unary->op]);
  print_type(_unary->type);
  visitor_walk(visitor, node, pad + 1);
}

void
print_if(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("if", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad + 1);
}

void
print_loop(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("loop", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad + 1);
}

void
print_decl(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("declaration", node->src, node, (intptr_t)pad);
  node_decl_t *_decl = peek_decl(node);
  fprintf(stdout, "[%d] ", _decl->kind);
  fprintf(stdout, "%s (%s) used: %d ", peek_ident(_decl->name)->str,
          _decl->mutable ? "mutable" : "immutable", _decl->used);

  print_type(_decl->type);
  print_flags(_decl->flags);
  visitor_visit(visitor, _decl->value, pad + 1);
}

void
print_type_struct(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("struct", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad + 1);
}

void
print_block(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("block", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad);
}

void
print_ident(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("ident", node->src, node, (intptr_t)pad);
  node_ident_t *_ident = peek_ident(node);
  fprintf(stdout, "%s ->", _ident->str);
  print_address(_ident->ref);
}

void
print_return(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("return", node->src, node, (intptr_t)pad);
  visitor_walk(visitor, node, pad + 1);
}

void
print_ublock(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("unit", node->src, node, (intptr_t)pad);
  node_ublock_t *_ublock = peek_ublock(node);
  fprintf(stdout, "%s", _ublock->unit->name);
  visitor_walk(visitor, node, pad + 1);
}

void
print_bad(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("INVALID", node->src, node, (intptr_t)pad);
}

void
print_binop(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("binop", node->src, node, (intptr_t)pad);
  node_expr_binop_t *_binop = peek_expr_binop(node);
  fprintf(stdout, "%s ", sym_strings[_binop->op]);
  print_type(_binop->type);
  visitor_walk(visitor, node, pad + 1);
}

void
print_lit(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("literal", node->src, node, (intptr_t)pad);
  node_lit_t *_lit = peek_lit(node);
  assert(_lit->type);

  node_type_fund_t *_type = peek_type_fund(ast_get_type(_lit->type));
  switch (_type->code) {
  case FTYPE_S8:
  case FTYPE_S16:
  case FTYPE_S32:
  case FTYPE_S64:
  case FTYPE_U8:
  case FTYPE_U16:
  case FTYPE_U32:
  case FTYPE_U64:
  case FTYPE_SIZE:
    fprintf(stdout, "%llu ", _lit->value.u);
    break;
  case FTYPE_F32:
  case FTYPE_F64:
    fprintf(stdout, "%f ", _lit->value.d);
    break;
  case FTYPE_CHAR:
    fprintf(stdout, "%c ", _lit->value.c);
    break;
  case FTYPE_STRING: {
    char *tmp = strdup(_lit->value.str);
    fprintf(stdout, "%s ", strtok(tmp, "\n"));
    char *next = strtok(NULL, "\n");
    if (next && strlen(next)) fprintf(stdout, "... ");
    free(tmp);
    break;
  }
  case FTYPE_BOOL:
    fprintf(stdout, "%s ", _lit->value.u ? "true" : "false");
    break;
  default:
    break;
  }

  print_type(_lit->type);
}

void
print_lit_fn(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("function", node->src, node, (intptr_t)pad);
  node_lit_fn_t *_fn = peek_lit_fn(node);

  print_type(_fn->type);
  visitor_walk(visitor, node, pad + 1);
}

void
print_call(visitor_t *visitor, node_t *node, void *pad)
{
  print_head("call", node->src, node, (intptr_t)pad);
  node_expr_call_t *_call = peek_expr_call(node);
  assert(_call->ref);
  if (node_is(_call->ref, NODE_IDENT)) {
    node_ident_t *_ident = peek_ident(_call->ref);
    fprintf(stdout, "%s ->", _ident->str);
    print_address(_ident->ref);
  }
  print_type(_call->type);
  if (_call->run) fprintf(stdout, " #run");

  visitor_walk(visitor, node, pad + 1);

  if (node_is(_call->ref, NODE_EXPR_MEMBER)) {
    visitor_visit(visitor, _call->ref, pad + 2);
  }
}

void
ast_printer_run(assembly_t *assembly)
{
  visitor_t visitor;
  visitor_init(&visitor);

  visitor_add(&visitor, print_bad, NODE_BAD);
  visitor_add(&visitor, print_ublock, NODE_UBLOCK);
  visitor_add(&visitor, print_block, NODE_BLOCK);
  visitor_add(&visitor, print_ident, NODE_IDENT);
  visitor_add(&visitor, print_decl, NODE_DECL);
  visitor_add(&visitor, print_load, NODE_LOAD);
  visitor_add(&visitor, print_lit_fn, NODE_LIT_FN);
  visitor_add(&visitor, print_lit_struct, NODE_LIT_STRUCT);
  visitor_add(&visitor, print_lit_enum, NODE_LIT_ENUM);
  visitor_add(&visitor, print_lit, NODE_LIT);
  visitor_add(&visitor, print_return, NODE_STMT_RETURN);
  visitor_add(&visitor, print_if, NODE_STMT_IF);
  visitor_add(&visitor, print_loop, NODE_STMT_LOOP);
  visitor_add(&visitor, print_break, NODE_STMT_BREAK);
  visitor_add(&visitor, print_continue, NODE_STMT_CONTINUE);
  visitor_add(&visitor, print_call, NODE_EXPR_CALL);
  visitor_add(&visitor, print_binop, NODE_EXPR_BINOP);
  visitor_add(&visitor, print_null, NODE_EXPR_NULL);
  visitor_add(&visitor, print_sizeof, NODE_EXPR_SIZEOF);
  visitor_add(&visitor, print_cast, NODE_EXPR_CAST);
  visitor_add(&visitor, print_member, NODE_EXPR_MEMBER);
  visitor_add(&visitor, print_elem, NODE_EXPR_ELEM);
  visitor_add(&visitor, print_unary, NODE_EXPR_UNARY);
  visitor_add(&visitor, print_type_struct, NODE_TYPE_STRUCT);

  unit_t *unit;
  barray_foreach(assembly->units, unit)
  {
    visitor_visit(&visitor, unit->ast.root, 0);
  }
  fprintf(stdout, "\n\n");
}
