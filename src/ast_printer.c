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
_print_head(Ast *node, int pad, FILE *stream)
{
  if (node->src)
    fprintf(stream, "\n%*s" GREEN("%s ") CYAN("<%d:%d>"), pad * 2, "", ast_get_name(node),
            node->src->line, node->src->col);
  else
    fprintf(stream, "\n%*s" GREEN("%s ") CYAN("<IMPLICIT>"), pad * 2, "", ast_get_name(node));

  print_address(node, stream);
}

static inline void
print_flags(int flags, FILE *stream)
{
  if (flags)
    fprintf(stream, " #");
  else
    return;
  if (flags & FLAG_EXTERN) fprintf(stream, "E");
  if (flags & FLAG_TEST) fprintf(stream, "T");
}

static void
print_node(Ast *node, int pad, FILE *stream);

static void
print_ublock(Ast *ublock, int pad, FILE *stream);

static void
print_test_case(Ast *test, int pad, FILE *stream);

static void
print_block(Ast *block, int pad, FILE *stream);

static void
print_unrecheable(Ast *unr, int pad, FILE *stream);

static void
print_stmt_if(Ast *stmt_if, int pad, FILE *stream);

static void
print_decl_entity(Ast *entity, int pad, FILE *stream);

static void
print_decl_arg(Ast *arg, int pad, FILE *stream);

static void
print_bad(Ast *bad, int pad, FILE *stream);

static void
print_expr_unary(Ast *unary, int pad, FILE *stream);

static void
print_expr_binop(Ast *binop, int pad, FILE *stream);

static void
print_expr_type(Ast *expr_type, int pad, FILE *stream);

static void
print_expr_ref(Ast *ref, int pad, FILE *stream);

static void
print_expr_lit_int(Ast *lit, int pad, FILE *stream);

static void
print_expr_lit_float(Ast *lit, int pad, FILE *stream);

static void
print_expr_lit_double(Ast *lit, int pad, FILE *stream);

static void
print_expr_lit_char(Ast *lit, int pad, FILE *stream);

static void
print_expr_lit_bool(Ast *lit, int pad, FILE *stream);

static void
print_expr_lit_string(Ast *lit, int pad, FILE *stream);

static void
print_expr_lit_fn(Ast *fn, int pad, FILE *stream);

static void
print_expr_call(Ast *call, int pad, FILE *stream);

/* impl */
void
print_ublock(Ast *ublock, int pad, FILE *stream)
{
  print_head(ublock, pad, stream);
  fprintf(stream, "%s", ublock->data.ublock.unit->name);

  Ast *tmp = NULL;
  barray_foreach(ublock->data.ublock.nodes, tmp) print_node(tmp, pad + 1, stream);
}

void
print_block(Ast *block, int pad, FILE *stream)
{
  print_head(block, pad, stream);
  Ast *tmp = NULL;
  barray_foreach(block->data.block.nodes, tmp) print_node(tmp, pad + 1, stream);
}

void
print_test_case(Ast *test, int pad, FILE *stream)
{
  print_head(test, pad, stream);
  fprintf(stream, "%s", test->data.test_case.desc);
  print_node(test->data.test_case.block, pad + 1, stream);
}

void
print_unrecheable(Ast *unr, int pad, FILE *stream)
{
  print_head(unr, pad, stream);
}

void
print_stmt_if(Ast *stmt_if, int pad, FILE *stream)
{
  print_head(stmt_if, pad, stream);
  print_node(stmt_if->data.stmt_if.test, pad + 1, stream);
  print_node(stmt_if->data.stmt_if.true_stmt, pad + 1, stream);
  print_node(stmt_if->data.stmt_if.false_stmt, pad + 1, stream);
}

void
print_decl_entity(Ast *entity, int pad, FILE *stream)
{
  print_head(entity, pad, stream);

  fprintf(stream, "'%s' '%s'", entity->data.decl.name->data.ident.str,
          entity->data.decl_entity.mutable ? "mutable" : "immutable");

  print_flags(entity->data.decl_entity.flags, stream);
  print_node((Ast *)entity->data.decl_entity.value, pad + 1, stream);
}

void
print_decl_arg(Ast *arg, int pad, FILE *stream)
{
  print_head(arg, pad, stream);
}

void
print_bad(Ast *bad, int pad, FILE *stream)
{
  print_head(bad, pad, stream);
}

void
print_expr_unary(Ast *unary, int pad, FILE *stream)
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
  case UNOP_ADR:
    op = "&";
    break;
  case UNOP_DEREF:
    op = "*";
    break;
  }

  fprintf(stream, "'%s' ", op);
  print_node(unary->data.expr_unary.next, pad + 1, stream);
}

void
print_expr_binop(Ast *binop, int pad, FILE *stream)
{
  print_head(binop, pad, stream);
  fprintf(stream, "'%s' ", ast_binop_to_str(binop->data.expr_binop.kind));
  print_node(binop->data.expr_binop.lhs, pad + 1, stream);
  print_node(binop->data.expr_binop.rhs, pad + 1, stream);
}

void
print_expr_type(Ast *expr_type, int pad, FILE *stream)
{
  print_head(expr_type, pad, stream);
}

void
print_expr_ref(Ast *ref, int pad, FILE *stream)
{
  print_head(ref, pad, stream);
  fprintf(stream, "'%s' ", ref->data.expr_ref.ident->data.ident.str);
}

void
print_expr_lit_int(Ast *lit, int pad, FILE *stream)
{
  print_head(lit, pad, stream);
  fprintf(stream, "%llu ", (long long unsigned)lit->data.expr_integer.val);
}

void
print_expr_lit_float(Ast *lit, int pad, FILE *stream)
{
  print_head(lit, pad, stream);
  fprintf(stream, "%f ", lit->data.expr_float.val);
}

void
print_expr_lit_double(Ast *lit, int pad, FILE *stream)
{
  print_head(lit, pad, stream);
  fprintf(stream, "%f ", lit->data.expr_double.val);
}

void
print_expr_lit_char(Ast *lit, int pad, FILE *stream)
{
  print_head(lit, pad, stream);
  fprintf(stream, "%c ", lit->data.expr_character.val);
}

void
print_expr_lit_bool(Ast *lit, int pad, FILE *stream)
{
  print_head(lit, pad, stream);
  fprintf(stream, "%s ", lit->data.expr_boolean.val ? "true" : "false");
}

void
print_expr_lit_string(Ast *lit, int pad, FILE *stream)
{
  print_head(lit, pad, stream);

  char *tmp = strdup(lit->data.expr_string.val);
  fprintf(stream, "%s ", strtok(tmp, "\n"));
  char *next = strtok(NULL, "\n");
  if (next && strlen(next)) fprintf(stream, "... ");
  free(tmp);
}

void
print_expr_lit_fn(Ast *fn, int pad, FILE *stream)
{
  print_head(fn, pad, stream);
  print_node(fn->data.expr_fn.block, pad + 1, stream);
}

void
print_expr_call(Ast *call, int pad, FILE *stream)
{
  print_head(call, pad, stream);

  print_node(call->data.expr_call.ref, pad + 1, stream);

  if (call->data.expr_call.args) {
    Ast *arg;
    barray_foreach(call->data.expr_call.args, arg) print_node(arg, pad + 1, stream);
  }
}

void
print_node(Ast *node, int pad, FILE *stream)
{
  if (!node) return;
  switch (node->kind) {
  case AST_BAD:
    print_bad(node, pad, stream);
    break;

  case AST_LOAD:
    break;

  case AST_LINK:
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

  case AST_DECL_ENTITY:
    print_decl_entity(node, pad, stream);
    break;

  case AST_DECL_ARG:
    print_decl_arg(node, pad, stream);
    break;

  case AST_DECL_MEMBER:
  case AST_DECL_VARIANT:
    break;

  case AST_STMT_RETURN:
    break;

  case AST_STMT_IF:
    print_stmt_if(node, pad, stream);
    break;

  case AST_STMT_LOOP:
    break;

  case AST_STMT_BREAK:
    break;

  case AST_STMT_CONTINUE:
    break;

  case AST_EXPR_TYPE:
    print_expr_type(node, pad, stream);
    break;

  case AST_EXPR_REF:
    print_expr_ref(node, pad, stream);
    break;

  case AST_EXPR_CAST:
    break;

  case AST_EXPR_BINOP:
    print_expr_binop(node, pad, stream);
    break;

  case AST_EXPR_CALL:
    print_expr_call(node, pad, stream);
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
    print_expr_unary(node, pad, stream);
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
