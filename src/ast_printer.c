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
#define int_to_void_ptr(i) (void *)((intptr_t)(i))

static inline void
print_address(Ast *node)
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

#define print_head(_node, _pad) _print_head((Ast *)(_node), (pad))

static inline void
_print_head(Ast *node, int pad)
{
  if (node->src)
    fprintf(stdout, "\n%*s" GREEN("%s ") CYAN("<%d:%d>"), pad * 2, "", ast_get_name(node),
            node->src->line, node->src->col);
  else
    fprintf(stdout, "\n%*s" GREEN("%s ") CYAN("<IMPLICIT>"), pad * 2, "", ast_get_name(node));

  print_address(node);
}

static inline void
print_flags(int flags)
{
  if (flags)
    fprintf(stdout, " #");
  else
    return;
  if (flags & FLAG_EXTERN) fprintf(stdout, "E");
  if (flags & FLAG_TEST) fprintf(stdout, "T");
}

static void
print_node(Ast *node, int pad);

static void
print_ublock(Ast *ublock, int pad);

static void
print_block(Ast *block, int pad);

static void
print_stmt_if(Ast *stmt_if, int pad);

static void
print_decl_entity(Ast *entity, int pad);

static void
print_decl_arg(Ast *arg, int pad);

static void
print_bad(Ast *bad, int pad);

static void
print_expr_unary(Ast *unary, int pad);

static void
print_expr_binop(Ast *binop, int pad);

static void
print_expr_type(Ast *expr_type, int pad);

static void
print_expr_ref(Ast *ref, int pad);

static void
print_expr_lit_int(Ast *lit, int pad);

static void
print_expr_lit_float(Ast *lit, int pad);

static void
print_expr_lit_double(Ast *lit, int pad);

static void
print_expr_lit_char(Ast *lit, int pad);

static void
print_expr_lit_bool(Ast *lit, int pad);

void static print_expr_lit_string(Ast *lit, int pad);

static void
print_expr_lit_fn(Ast *fn, int pad);

static void
print_expr_call(Ast *call, int pad);

/* impl */
void
print_ublock(Ast *ublock, int pad)
{
  print_head(ublock, pad);
  fprintf(stdout, "%s", ublock->data.ublock.unit->name);

  Ast *tmp = NULL;
  barray_foreach(ublock->data.ublock.nodes, tmp) print_node(tmp, pad + 1);
}

void
print_block(Ast *block, int pad)
{
  print_head(block, pad);
  Ast *tmp = NULL;
  barray_foreach(block->data.block.nodes, tmp) print_node(tmp, pad + 1);
}

void
print_stmt_if(Ast *stmt_if, int pad)
{
  print_head(stmt_if, pad);
  print_node(stmt_if->data.stmt_if.test, pad + 1);
  print_node(stmt_if->data.stmt_if.true_stmt, pad + 1);
  print_node(stmt_if->data.stmt_if.false_stmt, pad + 1);
}

void
print_decl_entity(Ast *entity, int pad)
{
  print_head(entity, pad);

  fprintf(stdout, "'%s' '%s'", entity->data.decl.name->data.ident.str,
          entity->data.decl_entity.mutable ? "mutable" : "immutable");

  print_flags(entity->data.decl_entity.flags);
  print_node((Ast *)entity->data.decl_entity.value, pad + 1);
}

void
print_decl_arg(Ast *arg, int pad)
{
  print_head(arg, pad);
}

void
print_bad(Ast *bad, int pad)
{
  print_head(bad, pad);
}

void
print_expr_unary(Ast *unary, int pad)
{
  print_head(unary, pad);

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
  case UNOP_ADR:
    op = "&";
    break;
  case UNOP_DEREF:
    op = "*";
    break;
  }

  fprintf(stdout, "'%s' ", op);
  print_node(unary->data.expr_unary.next, pad + 1);
}

void
print_expr_binop(Ast *binop, int pad)
{
  print_head(binop, pad);
  fprintf(stdout, "'%s' ", ast_binop_to_str(binop->data.expr_binop.kind));
  print_node(binop->data.expr_binop.lhs, pad + 1);
  print_node(binop->data.expr_binop.rhs, pad + 1);
}

void
print_expr_type(Ast *expr_type, int pad)
{
  print_head(expr_type, pad);
}

void
print_expr_ref(Ast *ref, int pad)
{
  print_head(ref, pad);
  fprintf(stdout, "'%s' ", ref->data.expr_ref.ident->data.ident.str);
}

void
print_expr_lit_int(Ast *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%llu ", (long long unsigned)lit->data.expr_integer.val);
}

void
print_expr_lit_float(Ast *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%f ", lit->data.expr_float.val);
}

void
print_expr_lit_double(Ast *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%f ", lit->data.expr_double.val);
}

void
print_expr_lit_char(Ast *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%c ", lit->data.expr_character.val);
}

void
print_expr_lit_bool(Ast *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%s ", lit->data.expr_boolean.val ? "true" : "false");
}

void
print_expr_lit_string(Ast *lit, int pad)
{
  print_head(lit, pad);

  char *tmp = strdup(lit->data.expr_string.val);
  fprintf(stdout, "%s ", strtok(tmp, "\n"));
  char *next = strtok(NULL, "\n");
  if (next && strlen(next)) fprintf(stdout, "... ");
  free(tmp);
}

void
print_expr_lit_fn(Ast *fn, int pad)
{
  print_head(fn, pad);
  print_node(fn->data.expr_fn.block, pad + 1);
}

void
print_expr_call(Ast *call, int pad)
{
  print_head(call, pad);

  print_node(call->data.expr_call.ref, pad + 1);

  Ast *arg;
  barray_foreach(call->data.expr_call.args, arg) print_node(arg, pad + 1);
}

void
print_node(Ast *node, int pad)
{
  if (!node) return;
  switch (node->kind) {
  case AST_BAD:
    print_bad(node, pad);
    break;

  case AST_LOAD:
    break;

  case AST_LINK:
    break;

  case AST_IDENT:
    break;

  case AST_UBLOCK:
    print_ublock(node, pad);
    break;

  case AST_BLOCK:
    print_block(node, pad);
    break;

  case AST_DECL_ENTITY:
    print_decl_entity(node, pad);
    break;

  case AST_DECL_ARG:
    print_decl_arg(node, pad);
    break;

  case AST_DECL_MEMBER:
  case AST_DECL_VARIANT:
    break;

  case AST_STMT_RETURN:
    break;

  case AST_STMT_IF:
    print_stmt_if(node, pad);
    break;

  case AST_STMT_LOOP:
    break;

  case AST_STMT_BREAK:
    break;

  case AST_STMT_CONTINUE:
    break;

  case AST_EXPR_TYPE:
    print_expr_type(node, pad);
    break;

  case AST_EXPR_REF:
    print_expr_ref(node, pad);
    break;

  case AST_EXPR_CAST:
    break;

  case AST_EXPR_BINOP:
    print_expr_binop(node, pad);
    break;

  case AST_EXPR_CALL:
    print_expr_call(node, pad);
    break;

  case AST_EXPR_MEMBER:
    break;

  case AST_EXPR_ELEM:
    break;

  case AST_EXPR_SIZEOF:
    break;

  case AST_EXPR_TYPEOF:
    break;

  case AST_EXPR_UNARY:
    print_expr_unary(node, pad);
    break;

  case AST_EXPR_NULL:
    break;

  case AST_EXPR_LIT_FN:
    print_expr_lit_fn(node, pad);
    break;

  case AST_EXPR_LIT_INT:
    print_expr_lit_int(node, pad);
    break;

  case AST_EXPR_LIT_FLOAT:
    print_expr_lit_float(node, pad);
    break;

  case AST_EXPR_LIT_DOUBLE:
    print_expr_lit_double(node, pad);
    break;

  case AST_EXPR_LIT_CHAR:
    print_expr_lit_char(node, pad);
    break;

  case AST_EXPR_LIT_STRING:
    print_expr_lit_string(node, pad);
    break;

  case AST_EXPR_LIT_BOOL:
    print_expr_lit_bool(node, pad);

    break;

  default:
    break;
  }
}

void
ast_printer_run(Assembly *assembly)
{
  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    print_node(unit->ast, 0);
  }
  fprintf(stdout, "\n\n");
}
