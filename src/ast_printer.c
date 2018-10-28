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
  if (flags & FLAG_MAIN) fprintf(stdout, "M");
  if (flags & FLAG_TEST) fprintf(stdout, "T");
  if (flags & FLAG_COMPILER) fprintf(stdout, "C");
}

static inline void
print_type(AstType *type)
{
  char tmp[256];
  ast_type_to_str(tmp, 256, type);
  fprintf(stdout, BLUE("{%s}"), tmp);
}

static void
print_load(Visitor *visitor, AstLoad *load, int pad);

static void
print_sizeof(Visitor *visitor, AstExprSizeof *szof, int pad);

static void
print_lit_cmp(Visitor *visitor, AstLitCmp *cmp, int pad);

static void
print_member(Visitor *visitor, AstMember *member, int pad);

static void
print_variant(Visitor *visitor, AstVariant *var, int pad);

static void
print_expr_member(Visitor *visitor, AstExprMember *mem, int pad);

static void
print_elem(Visitor *visitor, AstExprElem *elem, int pad);

static void
print_unary(Visitor *visitor, AstExprUnary *unary, int pad);

static void
print_break(Visitor *visitor, AstStmtBreak *brk, int pad);

static void
print_continue(Visitor *visitor, AstStmtContinue *cont, int pad);

static void
print_ublock(Visitor *visitor, AstUBlock *ublock, int pad);

static void
print_decl(Visitor *visitor, AstDecl *decl, int pad);

static void
print_block(Visitor *visitor, AstBlock *block, int pad);

static void
print_bad(Visitor *visitor, void *bad, int pad);

static void
print_binop(Visitor *visitor, AstExprBinop *binop, int pad);

static void
print_call(Visitor *visitor, AstExprCall *call, int pad);

static void
print_lit_int(Visitor *visitor, AstLitInt *lit, int pad);

static void
print_lit_float(Visitor *visitor, AstLitFloat *lit, int pad);

static void
print_lit_char(Visitor *visitor, AstLitChar *lit, int pad);

static void
print_lit_bool(Visitor *visitor, AstLitBool *lit, int pad);

static void
print_lit_string(Visitor *visitor, AstLitString *lit, int pad);

static void
print_lit_fn(Visitor *visitor, AstLitFn *fn, int pad);

static void
print_ident(Visitor *visitor, AstIdent *ident, int pad);

static void
print_return(Visitor *visitor, AstStmtReturn *ret, int pad);

static void
print_if(Visitor *visitor, AstStmtIf *stmt, int pad);

static void
print_loop(Visitor *visitor, AstStmtLoop *loop, int pad);

static void
print_cast(Visitor *visitor, AstExprCast *cast, int pad);

static void
print_null(Visitor *visitor, AstExprNull *null, int pad);

// impl
void
print_lit_cmp(Visitor *visitor, AstLitCmp *cmp, int pad)
{
  print_head(cmp, pad);
  visitor_walk(visitor, (Ast *)cmp, int_to_void_ptr(pad + 1));
}

void
print_sizeof(Visitor *visitor, AstExprSizeof *szof, int pad)
{
  print_head(szof, pad);
}

void
print_expr_member(Visitor *visitor, AstExprMember *member, int pad)
{
  print_head(member, pad);
  visitor_walk(visitor, (Ast *)member, int_to_void_ptr(pad + 1));
}

void
print_member(Visitor *visitor, AstMember *member, int pad)
{
  print_head(member, pad);

  if (member->name) {
    fprintf(stdout, "%s ", member->name->ident.str);
  }

  visitor_walk(visitor, (Ast *)member, int_to_void_ptr(pad + 1));
}

void
print_variant(Visitor *visitor, AstVariant *var, int pad)
{
  print_head(var, pad);
  fprintf(stdout, "%s ", var->name->ident.str);

  visitor_walk(visitor, (Ast *)var, int_to_void_ptr(pad + 1));
}

void
print_elem(Visitor *visitor, AstExprElem *elem, int pad)
{
  print_head(elem, pad);
  visitor_walk(visitor, (Ast *)elem, int_to_void_ptr(pad + 1));
}

void
print_load(Visitor *visitor, AstLoad *load, int pad)
{
  print_head(load, pad);
  fprintf(stdout, "'%s'", load->filepath);
}

void
print_break(Visitor *visitor, AstStmtBreak *brk, int pad)
{
  print_head(brk, pad);
}

void
print_continue(Visitor *visitor, AstStmtContinue *cont, int pad)
{
  print_head(cont, pad);
}

void
print_cast(Visitor *visitor, AstExprCast *cast, int pad)
{
  print_head(cast, pad);
  visitor_walk(visitor, (Ast *)cast, int_to_void_ptr(pad + 1));
}

void
print_null(Visitor *visitor, AstExprNull *null, int pad)
{
  print_head(null, pad);
}

void
print_unary(Visitor *visitor, AstExprUnary *unary, int pad)
{
  print_head(unary, pad);
  fprintf(stdout, "%s ", sym_strings[unary->kind]);
  visitor_walk(visitor, (Ast *)unary, int_to_void_ptr(pad + 1));
}

void
print_if(Visitor *visitor, AstStmtIf *sif, int pad)
{
  print_head(sif, pad);
  visitor_walk(visitor, (Ast *)sif, int_to_void_ptr(pad + 1));
}

void
print_loop(Visitor *visitor, AstStmtLoop *loop, int pad)
{
  print_head(loop, pad);
  visitor_walk(visitor, (Ast *)loop, int_to_void_ptr(pad + 1));
}

void
print_decl(Visitor *visitor, AstDecl *decl, int pad)
{
  print_head(decl, pad);

  switch (decl->kind) {
  case DECL_KIND_INVALID:
    fprintf(stdout, "[INVALID] ");
    break;
  case DECL_KIND_FIELD:
    fprintf(stdout, "[FIELD] ");
    break;
  case DECL_KIND_TYPE:
    fprintf(stdout, "[TYPE] ");
    break;
  case DECL_KIND_FN:
    fprintf(stdout, "[FN] ");
    break;
  case DECL_KIND_ENUM:
    fprintf(stdout, "[ENUM] ");
    break;
  }

  fprintf(stdout, "'%s' '%s' used: %d ", decl->name->ident.str,
          decl->mutable ? "mutable" : "immutable", decl->used);

  print_type(decl->type);

  print_flags(decl->flags);
  visitor_visit(visitor, decl->value, int_to_void_ptr(pad + 1));
}

void
print_block(Visitor *visitor, AstBlock *block, int pad)
{
  print_head(block, pad);
  visitor_walk(visitor, (Ast *)block, int_to_void_ptr(pad + 1));
}

void
print_ident(Visitor *visitor, AstIdent *ident, int pad)
{
  print_head(ident, pad);
  fprintf(stdout, "'%s'", ident->str);
}

void
print_return(Visitor *visitor, AstStmtReturn *ret, int pad)
{
  print_head(ret, pad);
  visitor_walk(visitor, (Ast *)ret, int_to_void_ptr(pad + 1));
}

void
print_ublock(Visitor *visitor, AstUBlock *ublock, int pad)
{
  print_head(ublock, pad);
  fprintf(stdout, "'%s'", ublock->unit->name);
  visitor_walk(visitor, (Ast *)ublock, int_to_void_ptr(pad + 1));
}

void
print_bad(Visitor *visitor, void *bad, int pad)
{
  print_head(bad, pad);
}

void
print_binop(Visitor *visitor, AstExprBinop *binop, int pad)
{
  print_head(binop, pad);
  fprintf(stdout, "'%s' ", sym_strings[binop->kind]);
  visitor_walk(visitor, (Ast *)binop, int_to_void_ptr(pad + 1));
}

void
print_lit_int(Visitor *visitor, AstLitInt *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%llu ", lit->i);
}

void
print_lit_float(Visitor *visitor, AstLitFloat *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%f ", lit->f);
}

void
print_lit_char(Visitor *visitor, AstLitChar *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%c ", lit->c);
}

void
print_lit_bool(Visitor *visitor, AstLitBool *lit, int pad)
{
  print_head(lit, pad);
  fprintf(stdout, "%s ", lit->b ? "true" : "false");
}

void
print_lit_string(Visitor *visitor, AstLitString *lit, int pad)
{
  print_head(lit, pad);

  char *tmp = strdup(lit->s);
  fprintf(stdout, "%s ", strtok(tmp, "\n"));
  char *next = strtok(NULL, "\n");
  if (next && strlen(next)) fprintf(stdout, "... ");
  free(tmp);
}

void
print_lit_fn(Visitor *visitor, AstLitFn *fn, int pad)
{
  print_head(fn, pad);
  visitor_walk(visitor, (Ast *)fn, int_to_void_ptr(pad + 1));
}

void
print_call(Visitor *visitor, AstExprCall *call, int pad)
{
  print_head(call, pad);
  assert(call->ref);
  if (call->run) fprintf(stdout, " #run");

  visitor_walk(visitor, (Ast *)call, int_to_void_ptr(pad + 1));

  if (ast_is(call->ref, AST_EXPR_MEMBER)) {
    visitor_visit(visitor, call->ref, int_to_void_ptr(pad + 2));
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
  visitor_add(&visitor, (VisitorFunc)print_member, AST_MEMBER);
  visitor_add(&visitor, (VisitorFunc)print_variant, AST_VARIANT);

  Unit *unit;
  barray_foreach(assembly->units, unit)
  {
    visitor_visit(&visitor, (Ast *)unit->ast, 0);
  }
  fprintf(stdout, "\n\n");
}
