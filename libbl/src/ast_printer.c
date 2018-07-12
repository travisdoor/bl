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
#include "stages_impl.h"
#include "common_impl.h"
#include "ast_impl.h"

#define MAX_STR_BUF 256

static inline void
print_address(bl_node_t *node)
{
#if BL_DEBUG
  fprintf(stdout, BL_YELLOW(" %d "), node->_serial);
#else
  fprintf(stdout, BL_YELLOW(" %p "), node);
#endif
}

static inline void
print_head(const char *name, bl_src_t *src, bl_node_t *ptr, int pad)
{
  if (src)
    fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<%d:%d>"), pad * 2, "", name, src->line,
            src->col);
  else
    fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<?>"), pad * 2, "", name);

  print_address(ptr);
}

static inline void
print_type(bl_node_t *type)
{
  if (!type) {
    fprintf(stdout, BL_RED("{?}"));
    return;
  }
  char tmp[MAX_STR_BUF];
  bl_ast_type_to_string(tmp, MAX_STR_BUF, type);
  fprintf(stdout, BL_CYAN("{%s}"), tmp);
}

static inline void
print_flags(int flags)
{
  if (flags)
    fprintf(stdout, " #");
  else
    return;
  if (flags & BL_FLAG_EXTERN) fprintf(stdout, "E");
  if (flags & BL_FLAG_MAIN) fprintf(stdout, "M");
  if (flags & BL_FLAG_ARG) fprintf(stdout, "A");
}

static void
print_node(bl_node_t *node, int pad);

static void
print_ublock(bl_node_t *node, int pad);

static void
print_type_struct(bl_node_t *node, int pad);

static void
print_decl_value(bl_node_t *node, int pad);

static void
print_decl_block(bl_node_t *node, int pad);

static void
print_decl_bad(bl_node_t *node, int pad);

static void
print_expr_bad(bl_node_t *node, int pad);

static void
print_expr_binop(bl_node_t *node, int pad);

static void
print_expr_call(bl_node_t *node, int pad);

static void
print_lit(bl_node_t *node, int pad);

static void
print_lit_fn(bl_node_t *node, int pad);

static void
print_ident(bl_node_t *node, int pad);

static void
print_return(bl_node_t *node, int pad);

static void
print_stmt_bad(bl_node_t *node, int pad);

static void
print_if(bl_node_t *node, int pad);

static void
print_loop(bl_node_t *node, int pad);

void
print_stmt_bad(bl_node_t *node, int pad)
{
  print_head("INVALID", node->src, node, pad);
}

void
print_if(bl_node_t *node, int pad)
{
  print_head("if", node->src, node, pad);
  bl_node_stmt_if_t *_if = bl_peek_stmt_if(node);
  print_node(_if->test, pad + 1);
  print_node(_if->true_stmt, pad + 1);
  print_node(_if->false_stmt, pad + 1);
}

void
print_loop(bl_node_t *node, int pad)
{
  print_head("loop", node->src, node, pad);
  bl_node_stmt_loop_t *_loop = bl_peek_stmt_loop(node);
  print_node(_loop->test, pad + 1);
  print_node(_loop->true_stmt, pad + 1);
}

void
print_decl_value(bl_node_t *node, int pad)
{
  print_head("declaration", node->src, node, pad);
  bl_node_decl_value_t *_decl = bl_peek_decl_value(node);
  fprintf(stdout, "%s (%s) used: %d ", bl_peek_ident(_decl->name)->str,
          _decl->mutable ? "mutable" : "immutable", _decl->used);

  print_type(_decl->type);
  print_flags(_decl->flags);
  print_node(_decl->value, pad + 1);
}

void
print_type_struct(bl_node_t *node, int pad)
{
  print_head("struct", node->src, node, pad);
  bl_node_type_struct_t *_ts = bl_peek_type_struct(node);

  bl_node_t *it;
  bl_node_foreach(_ts->types, it)
  {
    print_node(it, pad + 1);
  }
}

void
print_decl_block(bl_node_t *node, int pad)
{
  print_head("block", node->src, node, pad);
  bl_node_decl_block_t *_block = bl_peek_decl_block(node);

  bl_node_t *it;
  bl_node_foreach(_block->nodes, it)
  {
    print_node(it, pad + 1);
  }
}

void
print_decl_bad(bl_node_t *node, int pad)
{
  print_head("INVALID", node->src, node, pad);
}

void
print_ident(bl_node_t *node, int pad)
{
  print_head("ident", node->src, node, pad);
  bl_node_ident_t *_ident = bl_peek_ident(node);
  fprintf(stdout, "%s ->", _ident->str);
  print_address(_ident->ref);
}

void
print_return(bl_node_t *node, int pad)
{
  print_head("return", node->src, node, pad);
  bl_node_stmt_return_t *_return = bl_peek_stmt_return(node);
  print_node(_return->expr, pad + 1);
}

void
print_ublock(bl_node_t *node, int pad)
{
  print_head("unit", node->src, node, pad);
  bl_node_decl_ublock_t *_ublock = bl_peek_decl_ublock(node);
  fprintf(stdout, "%s", _ublock->unit->name);

  bl_node_t *it;
  bl_node_foreach(_ublock->nodes, it)
  {
    print_node(it, pad + 1);
  }
}

void
print_expr_bad(bl_node_t *node, int pad)
{
  print_head("INVALID", node->src, node, pad);
}

void
print_expr_binop(bl_node_t *node, int pad)
{
  print_head("binop", node->src, node, pad);
  bl_node_expr_binop_t *_binop = bl_peek_expr_binop(node);
  fprintf(stdout, "%s ", bl_sym_strings[_binop->op]);
  print_type(_binop->type);
  print_node(_binop->lhs, pad + 1);
  print_node(_binop->rhs, pad + 1);
}

void
print_lit(bl_node_t *node, int pad)
{
  print_head("literal", node->src, node, pad);
  bl_node_lit_t *_lit = bl_peek_lit(node);
  assert(_lit->type);

  bl_node_type_fund_t *_type = bl_peek_type_fund(_lit->type);
  switch (_type->code) {
  case BL_FTYPE_S8:
  case BL_FTYPE_S16:
  case BL_FTYPE_S32:
  case BL_FTYPE_S64:
  case BL_FTYPE_U8:
  case BL_FTYPE_U16:
  case BL_FTYPE_U32:
  case BL_FTYPE_U64:
  case BL_FTYPE_SIZE:
    fprintf(stdout, "%llu ", _lit->token->value.u);
    break;
  case BL_FTYPE_F32:
  case BL_FTYPE_F64:
    fprintf(stdout, "%f ", _lit->token->value.d);
    break;
  case BL_FTYPE_CHAR:
    fprintf(stdout, "%c ", _lit->token->value.c);
    break;
  case BL_FTYPE_STRING:
    fprintf(stdout, "%s ", _lit->token->value.str);
    break;
  case BL_FTYPE_BOOL:
    fprintf(stdout, "%s ", _lit->token->value.u ? "true" : "false");
    break;
  default:
    break;
  }
  print_type(_lit->type);
}

void
print_lit_fn(bl_node_t *node, int pad)
{
  print_head("function", node->src, node, pad);
  bl_node_lit_fn_t *_fn = bl_peek_lit_fn(node);

  print_type(_fn->type);

  bl_node_t *arg;
  bl_node_foreach(bl_peek_type_fn(_fn->type)->arg_types, arg)
  {
    print_node(arg, pad + 1);
  }

  print_node(_fn->block, pad + 1);
}

void
print_expr_call(bl_node_t *node, int pad)
{
  print_head("call", node->src, node, pad);
  bl_node_expr_call_t *_call = bl_peek_expr_call(node);
  assert(_call->ident);
  bl_node_ident_t *_ident = bl_peek_ident(_call->ident);

  fprintf(stdout, "%s ", _ident->str);
  print_type(_call->type);

  bl_node_t *it;
  bl_node_foreach(_call->args, it)
  {
    print_node(it, pad + 1);
  }
}

void
print_node(bl_node_t *node, int pad)
{
  if (!node) return;

  switch (node->code) {
  case BL_NODE_DECL_UBLOCK:
    print_ublock(node, pad);
    break;
  case BL_NODE_IDENT:
    print_ident(node, pad);
    break;
  case BL_NODE_STMT_RETURN:
    print_return(node, pad);
    break;
  case BL_NODE_STMT_IF:
    print_if(node, pad);
    break;
  case BL_NODE_STMT_LOOP:
    print_loop(node, pad);
    break;
  case BL_NODE_STMT_BAD:
    print_stmt_bad(node, pad);
    break;
  case BL_NODE_TYPE_STRUCT:
    print_type_struct(node, pad);
    break;
  case BL_NODE_DECL_VALUE:
    print_decl_value(node, pad);
    break;
  case BL_NODE_DECL_BLOCK:
    print_decl_block(node, pad);
    break;
  case BL_NODE_DECL_BAD:
    print_decl_bad(node, pad);
    break;
  case BL_NODE_LIT:
    print_lit(node, pad);
    break;
  case BL_NODE_LIT_FN:
    print_lit_fn(node, pad);
    break;
  case BL_NODE_EXPR_BAD:
    print_expr_bad(node, pad);
    break;
  case BL_NODE_EXPR_BINOP:
    print_expr_binop(node, pad);
    break;
  case BL_NODE_EXPR_CALL:
    print_expr_call(node, pad);
    break;
  default:
    bl_abort("invalid node %s", bl_node_name(node));
  }
}

void
bl_ast_printer_run(bl_assembly_t *assembly)
{
  bl_unit_t *unit;
  bl_barray_foreach(assembly->units, unit)
  {
    print_node(unit->ast.root, 0);
  }
  fprintf(stdout, "\n\n");
}
