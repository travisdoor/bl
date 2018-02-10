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
#include <string.h>
#include "bl/parser.h"
#include "bl/ast/node.h"
#include "bl/pipeline/stage.h"
#include "bl/bldebug.h"
#include "unit_impl.h"

#define parse_error(format, ...) \
  { \
    bl_actor_error((Actor *)unit, ("(parser) " format), ##__VA_ARGS__); \
    longjmp(jmp_error, 1); \
  } 

static Node *
parse_global_stmt(Parser *self, 
                  Unit *unit,
                  jmp_buf jmp_error);

static NodeStmt *
parse_stmt(Parser *self, 
           Unit *unit,
           jmp_buf jmp_error);

static NodeExpr *
parse_expr(Parser *self, 
           Unit *unit,
           jmp_buf jmp_error);

static NodeFuncDecl *
parse_func_decl(Parser *self, 
                Unit *unit,
                jmp_buf jmp_error);

static NodeParamVarDecl *
parse_param_var_decl(Parser *self, 
                     Unit *unit,
                     jmp_buf jmp_error);

static NodeReturnStmt *
parse_return_stmt(Parser *self,
                  Unit *unit,
                  jmp_buf jmp_error);

static bool
run(Parser *self,
    Unit   *unit);

/* Parser members */
bo_decl_members_begin(Parser, Stage)
bo_end();

/* Parser constructor parameters */
bo_decl_params_with_base_begin(Parser, Stage)
bo_end();

bo_impl_type(Parser, Stage);

/* Parser class init */
void
ParserKlass_init(ParserKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
}

/* Parser constructor */
void
Parser_ctor(Parser *self, ParserParams *p)
{
  bo_parent_ctor(Stage, p);
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
  NodeGlobalStmt *gstmt = bl_ast_node_global_stmt_new(unit->ast, unit->src, 1, 0);
stmt:
  if (bl_tokens_consume_if(unit->tokens, BL_SYM_SEMICOLON))
    goto stmt;

  if (!bl_node_global_stmt_add_child(gstmt, (Node *) parse_func_decl(self, unit, jmp_error))) {
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

NodeReturnStmt *
parse_return_stmt(Parser *self,
                  Unit *unit,
                  jmp_buf jmp_error)
{
  NodeReturnStmt *rstmt = NULL;
  if (bl_tokens_current_is(unit->tokens, BL_SYM_RETURN)) {
    bl_token_t *tok = bl_tokens_consume(unit->tokens);
    rstmt = bl_ast_node_return_stmt_new(unit->ast, tok->src_loc, tok->line, tok->col);

    /* HACK parse expression here */
    if (bl_tokens_current_is(unit->tokens, BL_SYM_NUM)) {
      if (!bl_node_return_stmt_add_expr(rstmt, parse_expr(self, unit, jmp_error))) {
        tok = bl_tokens_consume(unit->tokens);
        parse_error("%s %d:%d expected expression or nothing after return statement",
                    unit->filepath,
                    tok->line,
                    tok->col);
      }
    }

    tok = bl_tokens_consume(unit->tokens);
    if (tok->sym != BL_SYM_SEMICOLON) {
      parse_error("%s %d:%d missing semicolon ';' at the end of return statement",
                  unit->filepath,
                  tok->line,
                  tok->col);
    }
  }
  return rstmt;
}

NodeStmt *
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

stmt:
  if (bl_tokens_current_is(unit->tokens, BL_SYM_SEMICOLON)) {
    bl_tokens_consume(unit->tokens);
    goto stmt;
  }

  /* return */
  bl_node_stmt_add_child(stmt, (Node *) parse_return_stmt(self, unit, jmp_error));

  tok = bl_tokens_consume(unit->tokens);

  if (tok->sym != BL_SYM_RBLOCK)
    parse_error("%s %d:%d missing scope end '}'",
                unit->filepath,
                tok->line,
                tok->col);

  return stmt;
}

NodeFuncDecl *
parse_func_decl(Parser *self, 
                Unit *unit,
                jmp_buf jmp_error)
{ 
  NodeFuncDecl *func_decl = NULL;
  bl_token_t *tok;
  bl_sym_e modif = BL_SYM_NONE;

  /*
   * handle modificators
   */

  /* Store marker in case when current sequence of tokens is not function at all. */
  bl_tokens_set_marker(unit->tokens);
  if (bl_tokens_current_is(unit->tokens, BL_SYM_EXTERN)) {
    bl_tokens_consume(unit->tokens);
    modif = BL_SYM_EXTERN;
  }

  if (bl_tokens_is_seq(unit->tokens, 3, BL_SYM_IDENT, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    tok = bl_tokens_consume(unit->tokens);
    char *type = strndup(tok->content.as_string, tok->len);

    tok = bl_tokens_consume(unit->tokens);
    char *ident = strndup(tok->content.as_string, tok->len);

    func_decl = bl_ast_node_func_decl_new(
        unit->ast, type, ident, modif, tok->src_loc, tok->line, tok->col);

    /* consume '(' */
    bl_tokens_consume(unit->tokens);

    if (bl_tokens_current_is_not(unit->tokens, BL_SYM_RPAREN)) {
param:
      bl_node_func_decl_add_param(func_decl, parse_param_var_decl(self, unit, jmp_error));
      if (bl_tokens_consume_if(unit->tokens, BL_SYM_COMMA))
        goto param;
    }

    tok = bl_tokens_consume(unit->tokens);
    if (tok->sym != BL_SYM_RPAREN)
      parse_error("%s %d:%d expected ')' after function parameter declaration", 
          unit->filepath,
          tok->line,
          tok->col);

    if (modif == BL_SYM_EXTERN) {
      tok = bl_tokens_consume(unit->tokens);
      if (tok->sym != BL_SYM_SEMICOLON) {
        parse_error("%s %d:%d missing semicolon ';' at the end of extern function definition",
                    unit->filepath,
                    tok->line,
                    tok->col);
      }
    } else {
      bl_node_func_decl_add_stmt(func_decl, parse_stmt(self, unit, jmp_error));
    }
  } else {
    /* Roll back to marker. */
    bl_tokens_back_to_marker(unit->tokens);
  }

  return func_decl;
}

NodeParamVarDecl *
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

  char *type = strndup(tok->content.as_string, tok->len);

  tok = bl_tokens_consume(unit->tokens);
  if (tok->sym != BL_SYM_IDENT) {
    free(type);
    parse_error("%s %d:%d expected parameter name", 
        unit->filepath,
        tok->line,
        tok->col);
  }
  
  char *ident = strndup(tok->content.as_string, tok->len);
  return bl_ast_node_param_var_decl_new(
      unit->ast, type, ident, tok->src_loc, tok->line, tok->col);
}

NodeExpr *
parse_expr(Parser *self, 
           Unit *unit,
           jmp_buf jmp_error)
{
  NodeExpr *expr = NULL;

  /* HACK currently accept only numbers */
  if (bl_tokens_current_is(unit->tokens, BL_SYM_NUM)) {
    bl_token_t *tok = bl_tokens_consume(unit->tokens);
    expr = bl_ast_node_expr_new(
        unit->ast, tok->content.as_int, tok->src_loc, tok->line, tok->col);
  }

  return expr;
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
//  bl_log("* parsing done\n");

  return true;
}

/* public */
Parser *
bl_parser_new(bl_compile_group_e group)
{
  ParserParams p = {
    .base.group = group
  };

  return bo_new(Parser, &p);
}

/* public */
