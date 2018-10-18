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

static inline void
print_head(Ast *node, int pad)
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
  if (flags & FLAG_MAIN) fprintf(stdout, "M");
  if (flags & FLAG_TEST) fprintf(stdout, "T");
}

static void
print_load(Visitor *visitor, Ast *node, int pad);

static void
print_sizeof(Visitor *visitor, Ast *node, int pad);

static void
print_lit_cmp(Visitor *visitor, Ast *node, int pad);

static void
print_member(Visitor *visitor, Ast *node, int pad);

static void
print_variant(Visitor *visitor, Ast *node, int pad);

static void
print_expr_member(Visitor *visitor, Ast *node, int pad);

static void
print_elem(Visitor *visitor, Ast *node, int pad);

static void
print_unary(Visitor *visitor, Ast *node, int pad);

static void
print_break(Visitor *visitor, Ast *node, int pad);

static void
print_continue(Visitor *visitor, Ast *node, int pad);

static void
print_ublock(Visitor *visitor, Ast *node, int pad);

static void
print_type_struct(Visitor *visitor, Ast *node, int pad);

static void
print_decl(Visitor *visitor, Ast *node, int pad);

static void
print_block(Visitor *visitor, Ast *node, int pad);

static void
print_bad(Visitor *visitor, Ast *node, int pad);

static void
print_binop(Visitor *visitor, Ast *node, int pad);

static void
print_call(Visitor *visitor, Ast *node, int pad);

static void
print_lit_int(Visitor *visitor, Ast *node, int pad);

static void
print_lit_float(Visitor *visitor, Ast *node, int pad);

static void
print_lit_char(Visitor *visitor, Ast *node, int pad);

static void
print_lit_bool(Visitor *visitor, Ast *node, int pad);

static void
print_lit_string(Visitor *visitor, Ast *node, int pad);

static void
print_lit_fn(Visitor *visitor, Ast *node, int pad);

static void
print_type_enum(Visitor *visitor, Ast *node, int pad);

static void
print_ident(Visitor *visitor, Ast *node, int pad);

static void
print_return(Visitor *visitor, Ast *node, int pad);

static void
print_if(Visitor *visitor, Ast *node, int pad);

static void
print_loop(Visitor *visitor, Ast *node, int pad);

static void
print_cast(Visitor *visitor, Ast *node, int pad);

static void
print_null(Visitor *visitor, Ast *node, int pad);

// impl
void
print_lit_cmp(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_sizeof(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
}

void
print_expr_member(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_member(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstMember *_mem = ast_peek_member(node);

  if (_mem->name) {
    fprintf(stdout, "%s ", _mem->name->str);
  }

  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_variant(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstVariant *_var = ast_peek_variant(node);

  fprintf(stdout, "%s ", _var->name->str);

  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_elem(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_load(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstLoad *_load = ast_peek_load(node);
  fprintf(stdout, "'%s'", _load->filepath);
}

void
print_type_enum(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_break(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
}

void
print_continue(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
}

void
print_cast(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_null(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
}

void
print_unary(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstExprUnary *_unary = ast_peek_expr_unary(node);
  fprintf(stdout, "%s ", sym_strings[_unary->kind]);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_if(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_loop(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_decl(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstDecl *_decl = ast_peek_decl(node);
  fprintf(stdout, "'%d' ", _decl->kind);
  fprintf(stdout, "'%s' '%s' used: %d ", _decl->name->str,
          _decl->mutable ? "mutable" : "immutable", _decl->used);

  print_flags(_decl->flags);
  visitor_visit(visitor, _decl->value, int_to_void_ptr(pad + 1));
}

void
print_type_struct(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_block(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_ident(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstIdent *_ident = ast_peek_ident(node);
  fprintf(stdout, "'%s'", _ident->str);
}

void
print_return(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_ublock(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstUBlock *_ublock = ast_peek_ublock(node);
  fprintf(stdout, "'%s'", _ublock->unit->name);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_bad(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
}

void
print_binop(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstExprBinop *_binop = ast_peek_expr_binop(node);
  fprintf(stdout, "'%s' ", sym_strings[_binop->kind]);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_lit_int(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstLitInt *_lit = ast_peek_lit_int(node);

  fprintf(stdout, "%llu ", _lit->i);
}

void
print_lit_float(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstLitFloat *_lit = ast_peek_lit_float(node);

  fprintf(stdout, "%f ", _lit->f);
}

void
print_lit_char(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstLitChar *_lit = ast_peek_lit_char(node);

  fprintf(stdout, "%c ", _lit->c);
}

void
print_lit_bool(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstLitBool *_lit = ast_peek_lit_bool(node);

  fprintf(stdout, "%s ", _lit->b ? "true" : "false");
}

void
print_lit_string(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstLitString *_lit = ast_peek_lit_string(node);

  char *tmp = strdup(_lit->s);
  fprintf(stdout, "%s ", strtok(tmp, "\n"));
  char *next = strtok(NULL, "\n");
  if (next && strlen(next)) fprintf(stdout, "... ");
  free(tmp);
}

void
print_lit_fn(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));
}

void
print_call(Visitor *visitor, Ast *node, int pad)
{
  print_head(node, pad);
  AstExprCall *_call = ast_peek_expr_call(node);
  assert(_call->ref);
  if (ast_is(_call->ref, AST_IDENT)) {
    AstIdent *_ident = ast_peek_ident(_call->ref);
    fprintf(stdout, "'%s'", _ident->str);
  }
  if (_call->run) fprintf(stdout, " #run");

  visitor_walk(visitor, node, int_to_void_ptr(pad + 1));

  if (ast_is(_call->ref, AST_EXPR_MEMBER)) {
    visitor_visit(visitor, _call->ref, int_to_void_ptr(pad + 2));
  }
}

void
ast_printer_run(Assembly *assembly)
{
  Visitor visitor;
  visitor_init(&visitor);

  visitor_add(&visitor, (VisitorFunc)print_bad, AST_BAD);
  visitor_add(&visitor, (VisitorFunc)print_ublock, AST_UBLOCK);
  visitor_add(&visitor, (VisitorFunc)print_block, AST_BLOCK);
  visitor_add(&visitor, (VisitorFunc)print_ident, AST_IDENT);
  visitor_add(&visitor, (VisitorFunc)print_decl, AST_DECL);
  visitor_add(&visitor, (VisitorFunc)print_load, AST_LOAD);
  visitor_add(&visitor, (VisitorFunc)print_lit_fn, AST_LIT_FN);
  visitor_add(&visitor, (VisitorFunc)print_type_enum, AST_TYPE_ENUM);
  visitor_add(&visitor, (VisitorFunc)print_lit_int, AST_LIT_INT);
  visitor_add(&visitor, (VisitorFunc)print_lit_float, AST_LIT_FLOAT);
  visitor_add(&visitor, (VisitorFunc)print_lit_char, AST_LIT_CHAR);
  visitor_add(&visitor, (VisitorFunc)print_lit_string, AST_LIT_STRING);
  visitor_add(&visitor, (VisitorFunc)print_lit_bool, AST_LIT_BOOL);
  visitor_add(&visitor, (VisitorFunc)print_return, AST_STMT_RETURN);
  visitor_add(&visitor, (VisitorFunc)print_if, AST_STMT_IF);
  visitor_add(&visitor, (VisitorFunc)print_loop, AST_STMT_LOOP);
  visitor_add(&visitor, (VisitorFunc)print_break, AST_STMT_BREAK);
  visitor_add(&visitor, (VisitorFunc)print_continue, AST_STMT_CONTINUE);
  visitor_add(&visitor, (VisitorFunc)print_lit_cmp, AST_LIT_CMP);
  visitor_add(&visitor, (VisitorFunc)print_call, AST_EXPR_CALL);
  visitor_add(&visitor, (VisitorFunc)print_binop, AST_EXPR_BINOP);
  visitor_add(&visitor, (VisitorFunc)print_null, AST_EXPR_NULL);
  visitor_add(&visitor, (VisitorFunc)print_sizeof, AST_EXPR_SIZEOF);
  visitor_add(&visitor, (VisitorFunc)print_cast, AST_EXPR_CAST);
  visitor_add(&visitor, (VisitorFunc)print_expr_member, AST_EXPR_MEMBER);
  visitor_add(&visitor, (VisitorFunc)print_elem, AST_EXPR_ELEM);
  visitor_add(&visitor, (VisitorFunc)print_unary, AST_EXPR_UNARY);
  visitor_add(&visitor, (VisitorFunc)print_type_struct, AST_TYPE_STRUCT);
  visitor_add(&visitor, (VisitorFunc)print_member, AST_MEMBER);
  visitor_add(&visitor, (VisitorFunc)print_variant, AST_VARIANT);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    visitor_visit(&visitor, unit->ast, 0);
  }
  fprintf(stdout, "\n\n");
}
