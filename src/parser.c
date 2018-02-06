//*****************************************************************************
// Biscuit Engine
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   03/02/2018
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
//*****************************************************************************

#include <setjmp.h>
#include "parser.h"
#include "domains.h"
#include "unit.h"
#include "bldebug.h"
#include "ast/ast.h"

#define parse_error(format, ...) \
  { \
    bl_actor_error((Actor *)unit, (format), ##__VA_ARGS__); \
    longjmp(jmp_error, 1); \
  } 

static Node *
parse_global_stmt(Parser *self, 
                  Unit *unit,
                  jmp_buf jmp_error);

static Node *
parse_stmt(Parser *self, 
           Unit *unit,
           jmp_buf jmp_error);

static Node *
parse_func_decl(Parser *self, 
                Unit *unit,
                jmp_buf jmp_error);

static Node *
parse_param_var_decl(Parser *self, 
                     Unit *unit,
                     jmp_buf jmp_error);

static bool
run(Parser *self,
    Unit   *unit);

static int
domain(Parser *self);

static inline BString *
tok_to_str(bl_token_t *tok) {
  BString *str = bo_string_new(tok->len);
  bo_string_appendn(str, tok->content.as_string, tok->len);
  return str;
}

/* Parser members */
bo_decl_members_begin(Parser, Stage)
bo_end();

/* Parser constructor parameters */
bo_decl_params_begin(Parser)
bo_end();

bo_impl_type(Parser, Stage);

/* Parser class init */
void
ParserKlass_init(ParserKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
  bo_vtbl_cl(klass, Stage)->domain
    = (int (*)(Stage*)) domain;
}

/* Parser constructor */
void
Parser_ctor(Parser *self, ParserParams *p)
{
}

/* Parser destructor */
void
Parser_dtor(Parser *self)
{
}

/* Parser copy constructor */
bo_copy_result
Parser_copy(Parser *self, Parser *other)
{
  return BO_NO_COPY;
}

Node *
parse_global_stmt(Parser *self, 
                  Unit *unit,
                  jmp_buf jmp_error)
{
  NodeGlobalStmt *gstmt = bl_ast_node_global_stmt_new(unit->ast, bo_string_get(unit->src), 1, 0);
stmt:
  if (!bl_node_add_child((Node *)gstmt, parse_func_decl(self, unit, jmp_error))) {
    bl_token_t *tok = bl_tokens_peek(unit->tokens);
    parse_error("%s %d:%d expected function declaration",
                unit->filepath,
                tok->line,
                tok->col);
  }

  if (bl_tokens_current_is_not(unit->tokens, BL_SYM_EOF))
    goto stmt;

  return (Node *)gstmt;
}

Node *
parse_stmt(Parser *self, 
           Unit *unit,
           jmp_buf jmp_error)
{
  /* eat '{' */
  bl_token_t *tok = bl_tokens_consume(unit->tokens);
  if (tok->sym != BL_SYM_LBLOCK)
    parse_error("%s %d:%d expected scope body '{'",
                unit->filepath,
                tok->line,
                tok->col);

  NodeStmt *stmt = bl_ast_node_stmt_new(unit->ast, tok->src_loc, tok->line, tok->col);

  /* TODO: parse function scope */
  tok = bl_tokens_consume(unit->tokens);

  if (tok->sym != BL_SYM_RBLOCK)
    parse_error("%s %d:%d missing scope end '}'",
                unit->filepath,
                tok->line,
                tok->col);

  return (Node *)stmt;
}

Node *
parse_func_decl(Parser *self, 
                Unit *unit,
                jmp_buf jmp_error)
{ 
  NodeFuncDecl *func_decl = NULL;
  bl_token_t *tok;
  if (bl_tokens_is_seq(unit->tokens, 3, BL_SYM_IDENT, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    tok = bl_tokens_consume(unit->tokens);
    BString *type = tok_to_str(tok);

    tok = bl_tokens_consume(unit->tokens);
    BString *ident = tok_to_str(tok);

    func_decl = bl_ast_node_func_decl_new(
        unit->ast, type, ident, tok->src_loc, tok->line, tok->col);

    /* consume '(' */
    bl_tokens_consume(unit->tokens);

    if (bl_tokens_current_is_not(unit->tokens, BL_SYM_RPAREN)) {
param:
      bl_node_add_child((Node *)func_decl, parse_param_var_decl(self, unit, jmp_error));
      if (bl_tokens_consume_if(unit->tokens, BL_SYM_COMMA))
        goto param;
    }

    tok = bl_tokens_consume(unit->tokens);
    if (tok->sym != BL_SYM_RPAREN)
      parse_error("%s %d:%d expected ')' after function parameter declaration", 
          unit->filepath,
          tok->line,
          tok->col);

    bl_node_add_child((Node *)func_decl, parse_stmt(self, unit, jmp_error));
  }
  return (Node *)func_decl;
}

Node *
parse_param_var_decl(Parser *self, 
                     Unit *unit,
                     jmp_buf jmp_error)
{
  bl_token_t *tok = bl_tokens_consume(unit->tokens);
  if (tok->sym != BL_SYM_IDENT)
    parse_error("%s %d:%d expected parameter type", 
        unit->filepath,
        tok->line,
        tok->col);

  BString *type = tok_to_str(tok);

  tok = bl_tokens_consume(unit->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    bo_unref(type);
    parse_error("%s %d:%d expected parameter name", 
        unit->filepath,
        tok->line,
        tok->col);
  }
  
  BString *ident = tok_to_str(tok);
  return (Node *)bl_ast_node_param_var_decl_new(
      unit->ast, type, ident, tok->src_loc, tok->line, tok->col);
}

bool
run(Parser *self,
    Unit   *unit)
{
  jmp_buf jmp_error;
  
  if (unit->tokens == NULL) {
    bl_actor_error((Actor *)unit, "no tokens found for unit");
    return false;
  }

  if (setjmp(jmp_error))
    return false;

  bo_unref(unit->ast);
  unit->ast = bl_ast_new();
  bl_ast_set_root(unit->ast, parse_global_stmt(self, unit, jmp_error));
  bl_log("* parsing done\n");

  return true;
}

int
domain(Parser *self)
{
  return BL_DOMAIN_UNIT;
}

/* public */
Parser *
bl_parser_new(void)
{
  return bo_new(Parser, NULL);
}

/* public */
