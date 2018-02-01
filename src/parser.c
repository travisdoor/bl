//*****************************************************************************
// bl
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include <stdio.h>
#include <string.h>
#include "parser.h"
#include "token.h"
#include "bldebug.h"

/* forward decl */

static PNode *
parse_decl(Unit *unit,
           Tokens *tokens);

static PNode *
parse_func(Unit *unit,
           Tokens *tokens);
static PNode *
parse_return(Unit *unit,
             Tokens *tokens);

static PNode *
parse_call(Unit *unit,
           Tokens *tokens);

static PNode *
parse_call_args(Unit *unit,
                Tokens *tokens);

static PNode *
parse_call_arg(Unit *unit,
               Tokens *tokens);

static PNode *
parse_func_args(Unit *unit,
                Tokens *tokens);

static PNode *
parse_func_arg(Unit *unit,
               Tokens *tokens);

static PNode *
parse_exp(Unit *unit,
          Tokens *tokens);

static PNode *
parse_assignment(Unit *unit,
                 Tokens *tokens);

static PNode *
parse_gscope(Unit *unit,
             Tokens *tokens);

static PNode *
parse_namespace(Unit *unit,
                Tokens *tokens);

static PNode *
parse_scope(Unit *unit,
            Tokens *tokens);

static PNode *
parse_nscope(Unit *unit,
             Tokens *tokens);

static void
parse_error(Unit *unit,
            Tokens *tokens,
            const char *e);

/* impl */
void
parse_error(Unit *unit,
            Tokens *tokens,
            const char *msg)
{
  bl_token_t *tok = bl_tokens_peek(tokens);
  bl_error_at(bo_string_get(unit->filepath), tok->line, tok->col, msg);
}

PNode *
parse_exp(Unit *unit,
          Tokens *tokens)
{
  PNode *exp = NULL;
  if (bl_tokens_peek(tokens)->sym == BL_SYM_NUM) {
    exp = bl_pnode_new(BL_PT_EXP, bl_tokens_consume(tokens));
  }
  return exp;
}

PNode *
parse_assignment(Unit *unit,
                 Tokens *tokens)
{
  PNode *assign = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_ASIGN)) {
    assign = bl_pnode_new(BL_PT_ASGN, NULL);
    bl_pnode_new_child(assign, BL_PT_ID, bl_tokens_consume(tokens));
    // eat =
    bl_tokens_consume(tokens);

    PNode *exp = parse_exp(unit, tokens);
    if (!exp)
      parse_error(unit, tokens, "expected expression");
    bl_pnode_push(assign, exp);

    if (bl_tokens_consume_if(tokens, BL_SYM_SEMICOLON) == NULL)
      parse_error(unit, tokens, "missing semicolon");
  }
  return assign; 
}

PNode *
parse_call(Unit *unit,
           Tokens *tokens)
{
  PNode *call = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_LPAREN)) {
    call = bl_pnode_new(BL_PT_CALL, NULL);
    bl_pnode_new_child(call, BL_PT_ID, bl_tokens_consume(tokens));

    bl_pnode_push(call, parse_call_args(unit, tokens));

    if (bl_tokens_consume_if(tokens, BL_SYM_SEMICOLON) == NULL)
      parse_error(unit, tokens, "missing semicolon");
  }
  return call;
}

PNode *
parse_call_args(Unit *unit,
                Tokens *tokens)
{
  PNode *args = bl_pnode_new(BL_PT_ARGS, NULL);
  // eat (
  bl_tokens_consume(tokens);

  // empty argument list
  if (bl_tokens_current_is(tokens, BL_SYM_RPAREN)) {
    // eat last )
    bl_tokens_consume(tokens);
    return args;
  }

  while (true) {
    if (bl_tokens_current_is(tokens, BL_SYM_EOF) || bl_tokens_current_is(tokens, BL_SYM_LBLOCK)) {
      parse_error(unit, tokens, "missing ')' at the end of argument list");
    }

    bl_pnode_push(args, parse_call_arg(unit, tokens));

    if (bl_tokens_current_is(tokens, BL_SYM_RPAREN))
      break;

    if (bl_tokens_consume(tokens)->sym != BL_SYM_COMMA) {
      parse_error(unit, tokens, "missing ',' in argument list");
    }
  }
  // eat last )
  bl_tokens_consume(tokens);
  return args;
}

PNode *
parse_call_arg(Unit *unit,
               Tokens *tokens)
{
  PNode *arg = NULL;
  if (bl_tokens_current_is(tokens, BL_SYM_IDENT)) {
    arg = bl_pnode_new(BL_PT_ARG, NULL);
    bl_pnode_new_child(arg, BL_PT_ID, bl_tokens_consume(tokens));
  } else {
    parse_error(unit, tokens, "invalid argument");
  }
  return arg;
}

PNode *
parse_func_arg(Unit *unit,
               Tokens *tokens)
{
  PNode *arg = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    arg = bl_pnode_new(BL_PT_ARG, NULL);
    bl_pnode_new_child(arg, BL_PT_TYPE, bl_tokens_consume(tokens));
    bl_pnode_new_child(arg, BL_PT_ID, bl_tokens_consume(tokens));
  } else {
    parse_error(unit, tokens, "invalid argument");
  }
  return arg;
}

PNode *
parse_func_args(Unit *unit,
                Tokens *tokens)
{
  PNode *args = bl_pnode_new(BL_PT_ARGS, NULL);
  // eat (
  bl_tokens_consume(tokens);

  // empty argument list
  if (bl_tokens_current_is(tokens, BL_SYM_RPAREN)) {
    bl_tokens_consume(tokens);
    return args;
  }

  while (true) {
    if (bl_tokens_current_is(tokens, BL_SYM_EOF) || bl_tokens_current_is(tokens, BL_SYM_LBLOCK)) {
      parse_error(unit, tokens, "missing ')' at the end of argument list");
    }

    bl_pnode_push(args, parse_func_arg(unit, tokens));

    if (bl_tokens_current_is(tokens, BL_SYM_RPAREN))
      break;

    if (bl_tokens_consume(tokens)->sym != BL_SYM_COMMA) {
      parse_error(unit, tokens, "missing ',' in argument list");
    }
  }
  // eat last )
  bl_tokens_consume(tokens);
  return args;
}

PNode *
parse_return(Unit *unit,
             Tokens *tokens)
{
  // eat 'return'
  bl_tokens_consume(tokens);
  PNode *ret = bl_pnode_new(BL_PT_RET, NULL);

  PNode *exp = parse_exp(unit, tokens);
  if (exp)
    bl_pnode_push(ret, exp);

  if (bl_tokens_consume(tokens) == NULL)
    parse_error(unit, tokens, "missing semicolon");

  return ret;  
}

PNode *
parse_func(Unit *unit,
           Tokens *tokens)
{
  PNode *func = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    switch (bl_tokens_peek_nth(tokens, 3)->sym) {
      case BL_SYM_LPAREN:
        func = bl_pnode_new(BL_PT_FUNC, NULL);
        bl_pnode_new_child(func, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(func, BL_PT_ID, bl_tokens_consume(tokens));

        bl_pnode_push(func, parse_func_args(unit, tokens));

        if (bl_tokens_current_is_not(tokens, BL_SYM_LBLOCK)) {
          parse_error(unit, tokens, "expected '{' block after function declaration");
        }

        bl_pnode_push(func, parse_scope(unit, tokens));
        break;
      default:
        break;
    }
  }

  return func;
}

PNode *
parse_decl(Unit *unit,
           Tokens *tokens)
{
  PNode *decl = NULL;
  if (bl_tokens_is_seq(tokens, 2, BL_SYM_IDENT, BL_SYM_IDENT)) {
    switch (bl_tokens_peek_nth(tokens, 3)->sym) {
      case BL_SYM_ASIGN:
        decl = bl_pnode_new(BL_PT_DECL, NULL);
        bl_pnode_new_child(decl, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(decl, BL_PT_ID, bl_tokens_consume(tokens));

        // eat =
        bl_tokens_consume(tokens);
        PNode *exp = parse_exp(unit, tokens);
        if (!exp)
          parse_error(unit, tokens, "expected expression");
        bl_pnode_push(decl, exp);
        break;
      case BL_SYM_SEMICOLON:
        decl = bl_pnode_new(BL_PT_DECL, NULL);
        bl_pnode_new_child(decl, BL_PT_TYPE, bl_tokens_consume(tokens));
        bl_pnode_new_child(decl, BL_PT_ID, bl_tokens_consume(tokens));
        break;
      default:
        break;
    }
  }

  if (decl && bl_tokens_consume_if(tokens, BL_SYM_SEMICOLON) == NULL)
    parse_error(unit, tokens, "missing semicolon");

  return decl;
}

PNode *
parse_namespace(Unit *unit,
                Tokens *tokens)
{
  PNode *namespace = NULL;
  if (bl_tokens_current_is(tokens, BL_SYM_NAMESPACE)) {
    namespace = bl_pnode_new(BL_PT_NAMESPACE, bl_tokens_consume(tokens));
    bl_token_t *name = bl_tokens_consume_if(tokens, BL_SYM_IDENT);
    if (!bl_pnode_new_child(namespace, BL_PT_ID, name))
      parse_error(unit, tokens, "expected namespace name");

    bl_pnode_push(namespace, parse_nscope(unit, tokens));
  }

  return namespace;
}

PNode *
parse_scope(Unit *unit,
            Tokens *tokens)
{
  PNode *scope = bl_pnode_new(BL_PT_SCOPE, NULL);
  // eat {
  bl_tokens_consume(tokens);
  while (bl_tokens_current_is_not(tokens, BL_SYM_RBLOCK)) {
    if (bl_tokens_current_is(tokens, BL_SYM_EOF)) {
      parse_error(unit, tokens, "missing '}' at the end of scope");
    }
    if (bl_pnode_push(scope, parse_decl(unit, tokens)) ||
        bl_pnode_push(scope, parse_assignment(unit, tokens)) ||
        bl_pnode_push(scope, parse_return(unit, tokens)) ||
        bl_pnode_push(scope, parse_call(unit, tokens)))
      continue;
    else
      parse_error(unit, tokens, "unexpected symbol");
  }

  // eat }
  bl_tokens_consume(tokens);
  return scope;
}

PNode *
parse_nscope(Unit *unit,
             Tokens *tokens)
{
  PNode *nscope = bl_pnode_new(BL_PT_NSCOPE, NULL);
  // eat {
  bl_tokens_consume(tokens);
  while (bl_tokens_current_is_not(tokens, BL_SYM_RBLOCK)) {
    if (bl_tokens_current_is(tokens, BL_SYM_EOF)) {
      parse_error(unit, tokens, "missing '}' at the end of scope");
    }
    if (!bl_pnode_push(nscope, parse_namespace(unit, tokens)) &&
      !bl_pnode_push(nscope, parse_decl(unit, tokens)) &&
      !bl_pnode_push(nscope, parse_func(unit, tokens))) {
      parse_error(unit, tokens, "expected declaration or function");
    }
  }

  // eat }
  bl_tokens_consume(tokens);
  return nscope;
}

PNode *
parse_gscope(Unit *unit,
             Tokens *tokens)
{
  PNode *gscope = bl_pnode_new(BL_PT_GSCOPE, NULL);

  while (bl_tokens_current_is_not(tokens, BL_SYM_EOF)) {
    if (!bl_pnode_push(gscope, parse_namespace(unit, tokens)) &&
      !bl_pnode_push(gscope, parse_decl(unit, tokens)) &&
      !bl_pnode_push(gscope, parse_func(unit, tokens))) {
      parse_error(unit, tokens, "expected declaration or function");
    }
  }
  return gscope;
}

/* public */

PNode *
bl_parser_scan(Unit *unit,
               Tokens *tokens)
{
  /* parse global scope of the source */
  return parse_gscope(unit, tokens);
}
