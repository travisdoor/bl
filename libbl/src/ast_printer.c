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
print_head(const char *name, bl_src_t *src, void *ptr, int pad)
{
  if (src)
    fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<%d:%d>") BL_YELLOW(" %p "), pad * 2, "", name,
            src->line, src->col, ptr);
  else
    fprintf(stdout, "\n%*s" BL_GREEN("%s ") BL_CYAN("<?>") BL_YELLOW(" %p "), pad * 2, "", name,
            ptr);
}

static void
print_node(bl_node_t *node, int pad);

static void
print_ublock(bl_node_t *node, int pad);

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
print_lit(bl_node_t *node, int pad);

static void
print_lit_fn(bl_node_t *node, int pad);

static void
print_ident(bl_node_t *node, int pad);

void
print_decl_value(bl_node_t *node, int pad)
{
  print_head("declaration", node->src, node, pad);
  bl_node_decl_value_t *_decl = bl_peek_decl_value(node);
  fprintf(stdout, "%s (%s)", bl_peek_ident(_decl->name)->str,
          _decl->mutable ? "mutable" : "immutable");

  char tmp[MAX_STR_BUF];
  bl_ast_type_to_string(tmp, MAX_STR_BUF, _decl->type);

  if (_decl->type) fprintf(stdout, " %s", tmp);
  print_node(_decl->value, pad + 1);
}

void
print_decl_block(bl_node_t *node, int pad)
{
  print_head("block", node->src, node, pad);
  bl_node_decl_block_t *_block = bl_peek_decl_block(node);
  bl_node_t *           n      = _block->nodes;
  while (n) {
    print_node(n, pad + 1);
    n = n->next;
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
  fprintf(stdout, "%s -> %p", _ident->str, _ident->ref);
}

void
print_ublock(bl_node_t *node, int pad)
{
  print_head("unit", node->src, node, pad);
  bl_node_ublock_t *_ublock = bl_peek_ublock(node);
  bl_node_t *       tmp     = _ublock->nodes;
  ++pad;
  while (tmp) {
    print_node(tmp, pad);
    tmp = tmp->next;
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

  char tmp[MAX_STR_BUF];
  bl_ast_type_to_string(tmp, MAX_STR_BUF, _binop->type);

  fprintf(stdout, "%s %s", bl_sym_strings[_binop->op], tmp);
  print_node(_binop->lhs, pad + 1);
  print_node(_binop->rhs, pad + 1);
}

void
print_lit(bl_node_t *node, int pad)
{
  print_head("literal", node->src, node, pad);
  bl_node_lit_t *_lit = bl_peek_lit(node);
  assert(_lit->type);

  char tmp[MAX_STR_BUF];
  bl_ast_type_to_string(tmp, MAX_STR_BUF, _lit->type);
  
  bl_node_type_fund_t *_type = bl_peek_type_fund(_lit->type);
  switch (_type->code) {
  case BL_FTYPE_I8:
  case BL_FTYPE_I16:
  case BL_FTYPE_I32:
  case BL_FTYPE_I64:
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
  fprintf(stdout, "%s", tmp);
}

void
print_lit_fn(bl_node_t *node, int pad)
{
  print_head("function", node->src, node, pad);
  bl_node_lit_fn_t *_fn = bl_peek_lit_fn(node);

  char tmp[MAX_STR_BUF];
  bl_ast_type_to_string(tmp, MAX_STR_BUF, _fn->type);

  if (_fn->type) fprintf(stdout, "%s", tmp);
  print_node(_fn->block, pad + 1);
}

void
print_node(bl_node_t *node, int pad)
{
  if (!node) return;

  switch (node->code) {
  case BL_NODE_UBLOCK:
    print_ublock(node, pad);
    break;
  case BL_NODE_IDENT:
    print_ident(node, pad);
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
