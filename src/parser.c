//************************************************************************************************
// bl
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   3/15/18
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
//************************************************************************************************

#include <setjmp.h>
#include "stages.h"
#include "common.h"

#define FN_TEST_NAME "test@"

#define parse_error(cnt, kind, tok, pos, format, ...)                                              \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_ERROR, (kind), &(tok)->src, (pos), (format),           \
                ##__VA_ARGS__);                                                                    \
  }

#define parse_warning(cnt, tok, pos, format, ...)                                                  \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_WARNING, 0, &(tok)->src, (pos), (format),              \
                ##__VA_ARGS__);                                                                    \
  }

#define parse_note(cnt, tok, pos, format, ...)                                                     \
  {                                                                                                \
    builder_msg((cnt)->builder, BUILDER_MSG_NOTE, 0, &(tok)->src, (pos), (format), ##__VA_ARGS__); \
  }

/* swap current compound with _cmp and create temporary variable with previous one */

#define push_curr_decl(_cnt, _decl)                                                                \
  AstDeclEntity *const _prev_decl = (_cnt)->curr_decl;                                                   \
  (_cnt)->curr_decl         = (_decl);

#define pop_curr_decl(_cnt) (_cnt)->curr_decl = _prev_decl;

#define push_inloop(_cnt)                                                                          \
  bool _prev_inloop   = (_cnt)->inside_loop;                                                       \
  (_cnt)->inside_loop = true;

#define pop_inloop(_cnt) (_cnt)->inside_loop = _prev_inloop;

#define push_scope(_cnt, _scope)                                                                   \
  Scope *_prev_scope = (_cnt)->scope;                                                              \
  (_cnt)->scope      = _scope;

#define pop_scope(_cnt) (_cnt)->scope = _prev_scope;

typedef struct
{
  Builder * builder;
  Assembly *assembly;
  Unit *    unit;
  Arena *   ast_arena;
  Arena *   scope_arena;
  Tokens *  tokens;

  /* tmps */
  Scope *  scope;
  AstDeclEntity *curr_decl;
  bool     inside_loop;
  bool     core_loaded;
} Context;

/* helpers */
static inline void
insert_node(Ast ***node)
{
  *node = &(**node)->next;
}

/* fw decls */
static BinopKind
sym_to_binop_kind(Sym sm);

static UnopKind
sym_to_unop_kind(Sym sm);

static Ast *
load_core(Context *cnt);

static Ast *
parse_load(Context *cnt);

static Ast *
parse_link(Context *cnt);

static void
parse_ublock_content(Context *cnt, AstUBlock *ublock);

static int
parse_flags(Context *cnt, int allowed);

static AstIdent *
parse_ident(Context *cnt);

static Ast *
parse_block(Context *cnt);

static AstDecl *
parse_decl(Context *cnt);

static AstDecl *
parse_decl_member(Context *cnt, bool type_only, int order);

static AstDecl *
parse_decl_arg(Context *cnt, bool type_only);

static AstDecl *
parse_decl_variant(Context *cnt, AstType *base_type, AstDeclVariant *prev);

static AstType *
parse_type(Context *cnt);

static AstType *
parse_type_ref(Context *cnt);

static AstType *
parse_type_type(Context *cnt);

static AstType *
parse_type_arr(Context *cnt);

static AstType *
parse_type_vargs(Context *cnt);

static AstType *
parse_type_fn(Context *cnt, bool named_args);

static AstType *
parse_type_struct(Context *cnt);

static AstType *
parse_type_enum(Context *cnt);

static AstType *
parse_type_ptr(Context *cnt);

static Ast *
parse_stmt_return(Context *cnt);

static Ast *
parse_stmt_if(Context *cnt);

static Ast *
parse_stmt_loop(Context *cnt);

static Ast *
parse_stmt_break(Context *cnt);

static Ast *
parse_stmt_continue(Context *cnt);

static inline AstExpr *
parse_expr(Context *cnt);

static AstExpr *
_parse_expr(Context *cnt, AstExpr *lhs, int min_precedence);

static AstExpr *
parse_expr_type(Context *cnt);

static AstExpr *
parse_expr_run(Context *cnt);

static AstExpr *
parse_expr_unary(Context *cnt, Token *op);

static AstExpr *
parse_expr_member(Context *cnt, Token *op);

static AstExpr *
parse_expr_atom(Context *cnt, Token *op);

static AstExpr *
parse_expr_ref(Context *cnt);

static AstExpr *
parse_expr_nested(Context *cnt);

static AstExpr *
parse_expr_call(Context *cnt, AstExpr *prev);

static AstExpr *
parse_expr_null(Context *cnt);

static AstExpr *
parse_expr_sizeof(Context *cnt);

static AstExpr *
parse_expr_typeof(Context *cnt);

static AstExpr *
parse_expr_cast(Context *cnt);

static AstExpr *
parse_expr_elem(Context *cnt, Token *op);

static AstExpr *
parse_expr_line(Context *cnt);

static AstExpr *
parse_expr_file(Context *cnt);

static AstExpr *
parse_expr_lit_cmp(Context *cnt, AstExpr *prev);

static AstExpr *
parse_expr_lit(Context *cnt);

static AstExpr *
parse_expr_lit_fn(Context *cnt);

static inline bool
parse_semicolon_rq(Context *cnt);

// impl
BinopKind
sym_to_binop_kind(Sym sm)
{
  switch (sm) {
  case SYM_ASSIGN:
    return BINOP_ASSIGN;
  case SYM_PLUS_ASSIGN:
    return BINOP_ADD_ASSIGN;
  case SYM_MINUS_ASSIGN:
    return BINOP_SUB_ASSIGN;
  case SYM_ASTERISK_ASSIGN:
    return BINOP_MUL_ASSIGN;
  case SYM_SLASH_ASSIGN:
    return BINOP_DIV_ASSIGN;
  case SYM_PERCENT_ASSIGN:
    return BINOP_MOD_ASSIGN;
  case SYM_PLUS:
    return BINOP_ADD;
  case SYM_MINUS:
    return BINOP_SUB;
  case SYM_ASTERISK:
    return BINOP_MUL;
  case SYM_SLASH:
    return BINOP_DIV;
  case SYM_PERCENT:
    return BINOP_MOD;
  case SYM_EQ:
    return BINOP_EQ;
  case SYM_NEQ:
    return BINOP_NEQ;
  case SYM_GREATER:
    return BINOP_GREATER;
  case SYM_LESS:
    return BINOP_LESS;
  case SYM_GREATER_EQ:
    return BINOP_GREATER_EQ;
  case SYM_LESS_EQ:
    return BINOP_LESS_EQ;
  case SYM_LOGIC_AND:
    return BINOP_LOGIC_AND;
  case SYM_LOGIC_OR:
    return BINOP_LOGIC_OR;
  default:
    bl_abort("unknown binop operation!!!");
  }
}

UnopKind
sym_to_unop_kind(Sym sm)
{
  switch (sm) {
  case SYM_MINUS:
    return UNOP_NEG;
  case SYM_PLUS:
    return UNOP_POS;
  case SYM_NOT:
    return UNOP_NOT;
  case SYM_AND:
    return UNOP_ADR;
  case SYM_ASTERISK:
    return UNOP_DEREF;
  default:
    bl_abort("unknown unop operation!!!");
  }
}

AstExpr *
parse_expr_ref(Context *cnt)
{
  Token *   tok   = tokens_peek(cnt->tokens);
  AstIdent *ident = parse_ident(cnt);
  if (!ident) return NULL;

  AstExprRef *_ref = ast_create_expr(cnt->ast_arena, AST_EXPR_REF, tok, AstExprRef *);
  _ref->ident      = ident;
  return (AstExpr *)_ref;
}

AstExpr *
parse_expr_cast(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST);
  if (!tok_begin) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (!token_is(tok, SYM_LPAREN)) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_WORD,
                "expected '(' after cast expression");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_begin, AstExpr *);
  }

  AstExprCast *_cast = ast_create_expr(cnt->ast_arena, AST_EXPR_CAST, tok_begin, AstExprCast *);
  _cast->type        = parse_type(cnt);
  if (!_cast->type) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as cast parameter");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  tok = tokens_consume(cnt->tokens);
  if (!token_is(tok, SYM_RPAREN)) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected ')' after cast expression");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok, AstExpr *);
  }

  _cast->next = _parse_expr(cnt, parse_expr_atom(cnt, NULL), token_prec(tok_begin, false));
  if (!_cast->next) {
    tok = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "expected expression after cast");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok, AstExpr *);
  }

  return (AstExpr *)_cast;
}

AstDecl *
parse_decl_member(Context *cnt, bool type_only, int order)
{
  Token *   tok_begin = tokens_peek(cnt->tokens);
  AstIdent *name      = NULL;
  AstType * type      = NULL;

  if (type_only) {
    type = parse_type(cnt);
  } else {
    name = parse_ident(cnt);
    type = parse_type(cnt);
  }

  if (!type && !name) return NULL;
  AstDeclMember *mem      = ast_create_decl(cnt->ast_arena, AST_DECL_MEMBER, tok_begin, AstDeclMember *);
  mem->base.type         = type;
  mem->base.name         = name;
  mem->order = -1;

  return &mem->base;
}

AstDecl *
parse_decl_arg(Context *cnt, bool type_only)
{
  Token *   tok_begin = tokens_peek(cnt->tokens);
  AstIdent *name      = NULL;
  AstType * type      = NULL;

  if (type_only) {
    type = parse_type(cnt);
  } else {
    name = parse_ident(cnt);
    type = parse_type(cnt);
  }

  if (!type && !name) return NULL;
  AstDeclArg *arg = ast_create_decl(cnt->ast_arena, AST_DECL_ARG, tok_begin, AstDeclArg *);
  arg->base.type    = type;
  arg->base.name    = name;

  return &arg->base;
}

AstDecl *
parse_decl_variant(Context *cnt, AstType *base_type, AstDeclVariant *prev)
{
  Token *   tok_begin = tokens_peek(cnt->tokens);
  AstIdent *name      = parse_ident(cnt);
  if (!name) return NULL;

  AstDeclVariant *var = ast_create_decl(cnt->ast_arena, AST_DECL_VARIANT, tok_begin, AstDeclVariant *);

  Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_IMMDECL);
  if (tok_assign) {
    var->value = parse_expr(cnt);
    if (!var->value) bl_abort("expected enum variant value");
  } else if (prev) {
    AstExprLitInt *_addition =
        ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_INT, NULL, AstExprLitInt *);
    _addition->i = 1;

    AstExprBinop *_binop = ast_create_expr(cnt->ast_arena, AST_EXPR_BINOP, NULL, AstExprBinop *);
    _binop->kind         = BINOP_ADD;
    _binop->lhs          = prev->value;
    _binop->rhs          = (AstExpr *)_addition;

    var->value = (AstExpr *)_binop;
  } else {
    /* first variant is allways 0 */
    var->value = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_INT, NULL, AstExpr *);
    ((AstExprLitInt *)var->value)->i = 0;
  }

  assert(var->value);
  return &var->base;
}

AstExpr *
parse_expr_lit_cmp(Context *cnt, AstExpr *prev)
{
  if (!prev) return NULL;

  switch (prev->kind) {
  case AST_EXPR_TYPE:
    break;
  default:
    return NULL;
  }

  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok_begin) return NULL;

  AstExprLitCmp *lit_cmp =
      ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_CMP, tok_begin, AstExprLitCmp *);
  lit_cmp->base.type = prev->type;

  /* parse lit_cmp fields */
  bool  rq     = false;
  Ast **field  = (Ast **)&lit_cmp->fields;
  int * fieldc = &lit_cmp->fieldc;

next:
  *field = (Ast *)parse_expr(cnt);
  if (*field) {
    field = &(*field)->next;
    ++(*fieldc);

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected field after comma ','");
      return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
    }
  }

  /* eat } */
  if (!tokens_consume_if(cnt->tokens, SYM_RBLOCK)) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected end of initializer '}'");
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  return &lit_cmp->base;
}

AstExpr *
parse_expr_sizeof(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_SIZEOF);
  if (!tok_id) return NULL;

  /* eat ( */
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected '(' after sizeof buildin");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  AstExprSizeof *szof =
      ast_create_expr(cnt->ast_arena, AST_EXPR_SIZEOF, tok_id, AstExprSizeof *);

  //_sizeof->in = (Ast *)parse_type(cnt);
  if (!szof->in) szof->in = parse_expr(cnt);
  if (!szof->in) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as parameter");

    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  /* eat ) */
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected ')' after sizeof buildin argument");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  return &szof->base;
}

AstExpr *
parse_expr_typeof(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_TYPEOF);
  if (!tok_id) return NULL;

  /* eat ( */
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected '(' after typeof buildin");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  AstExprTypeof *tpof =
      ast_create_expr(cnt->ast_arena, AST_EXPR_TYPEOF, tok_id, AstExprTypeof *);

  if (!tpof->in) tpof->in = parse_expr(cnt);
  if (!tpof->in) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD,
                "expected type name as parameter");

    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  /* eat ) */
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected ')' after typeof buildin argument");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_err, AstExpr *);
  }

  return &tpof->base;
}

bool
parse_semicolon_rq(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_SEMICOLON);
  if (!tok) {
    tok = tokens_peek_prev(cnt->tokens);
    parse_error(cnt, ERR_MISSING_SEMICOLON, tok, BUILDER_CUR_AFTER, "missing semicolon ';'");
    return false;
  }
  return true;
}

Ast *
parse_stmt_return(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_RETURN);
  if (!tok_begin) return NULL;

  AstStmtReturn *ret =
      ast_create_node(cnt->ast_arena, AST_STMT_RETURN, tok_begin, AstStmtReturn *);

  assert(cnt->curr_decl);
  ret->fn_decl = cnt->curr_decl;
  ret->expr    = parse_expr(cnt);
  return &ret->base;
}

Ast *
parse_stmt_if(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_IF);
  if (!tok_begin) return NULL;

  // eat '('
  if (!tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_AFTER,
                "expected left parent '(' after 'if' statement");
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
  }

  AstStmtIf *_if = ast_create_node(cnt->ast_arena, AST_STMT_IF, tok_begin, AstStmtIf *);

  _if->test = parse_expr(cnt);
  if (_if->test == NULL) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD,
                "expected expression for the if statement");
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
  }

  if (_if->test->kind == AST_EXPR_BAD) {
    tokens_consume_till(cnt->tokens, SYM_LBLOCK);
  }

  // eat ')'
  if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "expected closing parent ')' after 'if' statement expression");
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
  }

  _if->true_stmt = parse_block(cnt);
  if (!_if->true_stmt) {
    Token *tok_err = tokens_consume(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_STMT, tok_err, BUILDER_CUR_WORD,
                "expected compound statement for true result of the if expression test");
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
  }

  _if->false_stmt = NULL;
  if (tokens_consume_if(cnt->tokens, SYM_ELSE)) {
    _if->false_stmt = parse_stmt_if(cnt);
    if (!_if->false_stmt) _if->false_stmt = parse_block(cnt);
    if (!_if->false_stmt) {
      Token *tok_err = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_EXPECTED_STMT, tok_err, BUILDER_CUR_WORD,
                  "expected statement for false result of the if expression test");
      return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
    }
  }

  return &_if->base;
}

static TokensLookaheadState
cmp_stmt_loop(Token *curr)
{
  if (token_is(curr, SYM_SEMICOLON))
    return TOK_LOOK_HIT;
  else if (token_is(curr, SYM_RPAREN))
    return TOK_LOOK_TERMINAL;
  else if (token_is(curr, SYM_LBLOCK))
    return TOK_LOOK_TERMINAL;

  return TOK_LOOK_CONTINUE;
}

Ast *
parse_stmt_loop(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LOOP);
  if (!tok_begin) return NULL;

  const bool while_true = tokens_current_is(cnt->tokens, SYM_LBLOCK);

  AstStmtLoop *loop = ast_create_node(cnt->ast_arena, AST_STMT_LOOP, tok_begin, AstStmtLoop *);
  push_inloop(cnt);

  Scope *scope = scope_create(cnt->scope_arena, cnt->scope, 8);
  push_scope(cnt, scope);

  if (!while_true) {
    // eat '('
    if (!while_true && !tokens_consume_if(cnt->tokens, SYM_LPAREN)) {
      Token *tok_err = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_AFTER,
                  "expected left parent '(' after 'loop' statement");
      pop_scope(cnt);
      return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
    }

    if (tokens_lookahead(cnt->tokens, cmp_stmt_loop)) {
      /* for loop construct loop [init]; [condition]; [increment] {} */
      loop->init = parse_decl(cnt);
      if (!parse_semicolon_rq(cnt)) {
        assert(false);
      }

      loop->condition = parse_expr(cnt);
      if (!parse_semicolon_rq(cnt)) {
        assert(false);
      }

      loop->increment = parse_expr(cnt);
    } else {
      /* while construct with optional condition */
      loop->condition = parse_expr(cnt);
    }

    // eat ')'
    if (!tokens_consume_if(cnt->tokens, SYM_RPAREN)) {
      Token *tok_err = tokens_consume(cnt->tokens);
      parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                  "expected closing parent ')'");
      pop_inloop(cnt);
      pop_scope(cnt);
      return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
    }
  }

  /* block */
  loop->block = parse_block(cnt);
  if (!loop->block) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "expected loop body block");
    pop_inloop(cnt);
    pop_scope(cnt);
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, Ast *);
  }

  pop_inloop(cnt);
  pop_scope(cnt);
  return &loop->base;
}

Ast *
parse_stmt_break(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_BREAK);
  if (!tok) return NULL;

  if (!cnt->inside_loop) {
    parse_error(cnt, ERR_BREAK_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD,
                "break statement outside a loop");
  }
  return ast_create_node(cnt->ast_arena, AST_STMT_BREAK, tok, Ast *);
}

Ast *
parse_stmt_continue(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_CONTINUE);
  if (!tok) return NULL;

  if (!cnt->inside_loop) {
    parse_error(cnt, ERR_CONTINUE_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD,
                "continue statement outside a loop");
  }

  return ast_create_node(cnt->ast_arena, AST_STMT_CONTINUE, tok, Ast *);
}

AstExpr *
parse_expr_lit(Context *cnt)
{
  Token *  tok = tokens_peek(cnt->tokens);
  AstExpr *lit = NULL;

  switch (tok->sym) {
  case SYM_NUM:
    lit                       = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_INT, tok, AstExpr *);
    ((AstExprLitInt *)lit)->i = tok->value.u;
    break;

  case SYM_CHAR:
    lit                        = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_CHAR, tok, AstExpr *);
    ((AstExprLitChar *)lit)->c = (uint8_t) tok->value.c;

    break;

  case SYM_STRING:
    lit = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_STRING, tok, AstExpr *);
    ((AstExprLitString *)lit)->s = tok->value.str;
    break;

  case SYM_TRUE:
    lit                        = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, AstExpr *);
    ((AstExprLitBool *)lit)->b = true;
    break;

  case SYM_FALSE:
    lit                        = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, AstExpr *);
    ((AstExprLitBool *)lit)->b = false;
    break;

  case SYM_DOUBLE:
  case SYM_FLOAT:
    // TODO: double!!!
    // TODO: double!!!
    // TODO: double!!!
    lit = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_FLOAT, tok, AstExpr *);
    ((AstExprLitFloat *)lit)->f = (float)tok->value.d;
    break;

  default:
    return NULL;
  }

  tokens_consume(cnt->tokens);
  return lit;
}

AstExpr *
parse_expr_lit_fn(Context *cnt)
{
  Token *tok_fn = tokens_peek(cnt->tokens);
  if (token_is_not(tok_fn, SYM_FN)) return NULL;

  AstExprLitFn *_fn = ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_FN, tok_fn, AstExprLitFn *);

  Scope *scope = scope_create(cnt->scope_arena, cnt->scope, 32);
  push_scope(cnt, scope);

  AstType *type = parse_type_fn(cnt, true);
  assert(type);

  ((AstExpr *)_fn)->type = type;

  /* parse block (block is optional function body can be external) */
  _fn->block = parse_block(cnt);

  pop_scope(cnt);
  return (AstExpr *)_fn;
}

AstType *
parse_type_enum(Context *cnt)
{
  Token *tok_enum = tokens_consume_if(cnt->tokens, SYM_ENUM);
  if (!tok_enum) return NULL;

  Scope *scope = scope_create(cnt->scope_arena, cnt->scope, 512);
  push_scope(cnt, scope);

  AstTypeEnum *_enm = ast_create_type(cnt->ast_arena, AST_TYPE_ENUM, tok_enum, AstTypeEnum *);

  _enm->type = parse_type(cnt);

  Token *tok = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "expected enm variant list");
    pop_scope(cnt);
    return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok, AstType *);
  }

  /* parse enum varinats */
  bool  rq           = false;
  Ast **variant      = &_enm->variants;
  AstDeclVariant * prev_variant = NULL;

next:
  *variant = (Ast *)parse_decl_variant(cnt, _enm->type, prev_variant);
  if (*variant) {
    prev_variant = (AstDeclVariant *)*variant;
    variant      = &(*variant)->next;

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected variant after comma ','");
      pop_scope(cnt);
      return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok, AstType *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of variant list  '}'  or another variant separated by comma");
    pop_scope(cnt);
    return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok, AstType *);
  }

  pop_scope(cnt);
  return (AstType *)_enm;
}

AstExpr *
parse_expr(Context *cnt)
{
  return _parse_expr(cnt, parse_expr_unary(cnt, NULL), 0);
}

AstExpr *
parse_expr_unary(Context *cnt, Token *op)
{
  Token *curr_op = tokens_peek(cnt->tokens);
  if (token_is_unary(curr_op)) {
    tokens_consume(cnt->tokens);
    AstExprUnary *_unary = ast_create_expr(cnt->ast_arena, AST_EXPR_UNARY, curr_op, AstExprUnary *);
    _unary->next         = _parse_expr(cnt, parse_expr_atom(cnt, NULL), token_prec(curr_op, true));
    _unary->kind         = sym_to_unop_kind(curr_op->sym);

    if (_unary->next == NULL) {
      Token *err_tok = tokens_peek(cnt->tokens);
      parse_error(cnt, ERR_EXPECTED_EXPR, err_tok, BUILDER_CUR_WORD,
                  "expected expression after unary operator");
      tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
      return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, curr_op, AstExpr *);
    }

    if (_unary->next->kind == AST_EXPR_BAD) return _unary->next;

    return (AstExpr *)_unary;
  } else {
    return parse_expr_atom(cnt, op);
  }
}

AstExpr *
parse_expr_nested(Context *cnt)
{
  AstExpr *expr      = NULL;
  Token *  tok_begin = tokens_consume_if(cnt->tokens, SYM_LPAREN);
  if (!tok_begin) return NULL;

  expr = parse_expr(cnt);
  if (expr == NULL) {
    parse_error(cnt, ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_WORD, "expected expression.");
  }

  /* eat ) */
  Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RPAREN);
  if (!tok_end) {
    Token *tok_err = tokens_peek(cnt->tokens);
    parse_error(cnt, ERR_MISSING_BRACKET, tok_err, BUILDER_CUR_WORD,
                "unterminated sub-expression, missing ')'");
    parse_note(cnt, tok_begin, BUILDER_CUR_WORD, "starting here");
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok_begin, AstExpr *);
  }

  return expr;
}

AstExpr *
parse_expr_member(Context *cnt, Token *op)
{
  if (!op) return NULL;
  if (token_is_not(op, SYM_DOT)) return NULL;

  AstIdent *ident = parse_ident(cnt);
  if (!ident) {
    parse_error(cnt, ERR_EXPECTED_NAME, op, BUILDER_CUR_WORD, "expected structure member name");
  }

  AstExprMember *_mem = ast_create_expr(cnt->ast_arena, AST_EXPR_MEMBER, op, AstExprMember *);
  _mem->i             = -1;
  return (AstExpr *)_mem;
}

AstExpr *
parse_expr_elem(Context *cnt, Token *op)
{
  if (!op) return NULL;
  if (token_is_not(op, SYM_LBRACKET)) return NULL;

  AstExprElem *_elem = ast_create_expr(cnt->ast_arena, AST_EXPR_ELEM, op, AstExprElem *);

  _elem->index = parse_expr(cnt);
  if (!_elem->index) {
    parse_error(cnt, ERR_EXPECTED_EXPR, op, BUILDER_CUR_WORD, "expected array index expression");
  }

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBRACKET) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "missing bracket ']'");
  }

  return (AstExpr *)_elem;
}

AstExpr *
parse_expr_atom(Context *cnt, Token *op)
{
  AstExpr *expr = NULL;
  AstExpr *tmp  = NULL;

  if ((expr = parse_expr_nested(cnt))) goto done;
  if ((expr = parse_expr_null(cnt))) goto done;
  if ((expr = parse_expr_sizeof(cnt))) goto done;
  if ((expr = parse_expr_typeof(cnt))) goto done;
  if ((expr = parse_expr_cast(cnt))) goto done;
  if ((expr = parse_expr_run(cnt))) goto done;
  if ((expr = parse_expr_lit_fn(cnt))) goto done;
  if ((expr = parse_expr_type(cnt))) goto done;
  if ((expr = parse_expr_elem(cnt, op))) goto done;
  if ((expr = parse_expr_lit(cnt))) goto done;
  if ((expr = parse_expr_member(cnt, op))) goto done;
  if ((expr = parse_expr_ref(cnt))) goto done;
  if ((expr = parse_expr_line(cnt))) goto done;
  if ((expr = parse_expr_file(cnt))) goto done;

done:
  tmp = parse_expr_call(cnt, expr);
  if (!tmp) tmp = parse_expr_lit_cmp(cnt, expr);
  return tmp ? tmp : expr;
}

AstExpr *
_parse_expr(Context *cnt, AstExpr *lhs, int min_precedence)
{
  AstExpr *rhs       = NULL;
  Token *  lookahead = tokens_peek(cnt->tokens);
  Token *  op        = NULL;

  while (token_prec(lookahead, false) >= min_precedence) {
    op = lookahead;
    tokens_consume(cnt->tokens);
    rhs = parse_expr_unary(cnt, op);
    if (rhs && rhs->kind == AST_EXPR_BAD) return rhs;
    lookahead = tokens_peek(cnt->tokens);

    while (token_prec(lookahead, false) > token_prec(op, false)) {
      rhs       = _parse_expr(cnt, rhs, token_prec(lookahead, false));
      lookahead = tokens_peek(cnt->tokens);
    }

    if (token_is_binop(op)) {
      AstExprBinop *_binop = ast_create_expr(cnt->ast_arena, AST_EXPR_BINOP, op, AstExprBinop *);
      _binop->kind         = sym_to_binop_kind(op->sym);
      _binop->lhs          = lhs;
      _binop->rhs          = rhs;

      lhs = &_binop->base;
    } else {
      parse_error(cnt, ERR_EXPECTED_BINOP, op, BUILDER_CUR_WORD, "expected binary operation");
      tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
      return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, op, AstExpr *);
    }
  }

  return lhs;
}

AstIdent *
parse_ident(Context *cnt)
{
  Token *tok_ident = tokens_consume_if(cnt->tokens, SYM_IDENT);
  if (!tok_ident) return NULL;

  assert(cnt->scope);

  AstIdent *_ident = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ident, AstIdent *);
  _ident->hash     = bo_hash_from_str(tok_ident->value.str);
  _ident->str      = tok_ident->value.str;
  _ident->scope    = cnt->scope;

  return _ident;
}

AstType *
parse_type_ptr(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ASTERISK);
  if (!tok_begin) return NULL;

  AstTypePtr *ptr = ast_create_type(cnt->ast_arena, AST_TYPE_PTR, tok_begin, AstTypePtr *);
  ptr->type       = parse_type(cnt);
  assert(ptr->type);
  return &ptr->base;
}

AstType *
parse_type_ref(Context *cnt)
{
  Token *   tok   = tokens_peek(cnt->tokens);
  AstIdent *ident = parse_ident(cnt);
  if (!ident) return NULL;

  AstTypeRef *type_ref = ast_create_type(cnt->ast_arena, AST_TYPE_REF, tok, AstTypeRef *);
  type_ref->ident      = ident;
  return &type_ref->base;
}

AstType *
parse_type_type(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_TYPE);
  if (!tok_begin) return NULL;

  return ast_create_type(cnt->ast_arena, AST_TYPE_TYPE, tok_begin, AstType *);
}

AstType *
parse_type_arr(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
  if (!tok_begin) return NULL;

  AstTypeArr *arr = ast_create_type(cnt->ast_arena, AST_TYPE_ARR, tok_begin, AstTypeArr *);
  arr->len        = parse_expr(cnt);

  Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
  if (!tok_end) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok_end, BUILDER_CUR_WORD,
                "expected ']' after array size expression");
  }

  arr->elem_type = parse_type(cnt);
  assert(arr->elem_type);
  return &arr->base;
}

AstType *
parse_type(Context *cnt)
{
  AstType *type = NULL;

  type = parse_type_ptr(cnt);
  if (!type) type = parse_type_type(cnt);
  if (!type) type = parse_type_fn(cnt, false);
  if (!type) type = parse_type_struct(cnt);
  if (!type) type = parse_type_enum(cnt);
  if (!type) type = parse_type_vargs(cnt);
  if (!type) type = parse_type_arr(cnt);
  if (!type) type = parse_type_ref(cnt);

  return type;
}

AstType *
parse_type_vargs(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_VARGS);
  if (!tok_begin) return NULL;

  AstTypeVArgs *vargs = ast_create_type(cnt->ast_arena, AST_TYPE_VARGS, tok_begin, AstTypeVArgs *);
  return &vargs->base;
}

AstType *
parse_type_fn(Context *cnt, bool named_args)
{
  Token *tok_fn = tokens_consume_if(cnt->tokens, SYM_FN);
  if (!tok_fn) return NULL;

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected function parameter list");
    return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok_fn, AstType *);
  }

  AstTypeFn *fn = ast_create_type(cnt->ast_arena, AST_TYPE_FN, tok_fn, AstTypeFn *);

  /* parse arg types */
  bool  rq   = false;
  Ast **arg  = &fn->args;
  int * argc = &fn->argc;

next:
  *arg = (Ast *)parse_decl_arg(cnt, !named_args);
  if (*arg) {
    arg = &(*arg)->next;
    ++(*argc);

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected type after comma ','");
      return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok_fn, AstType *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of argument type list  ')'  or another type separated by comma");
    return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok_fn, AstType *);
  }

  fn->ret_type = parse_type(cnt);
  /* implicitly use void for no-return functions */
  if (!fn->ret_type) fn->ret_type = cnt->builder->buildin.entry_void;

  return &fn->base;
}

AstType *
parse_type_struct(Context *cnt)
{
  Token *tok_struct = tokens_consume_if(cnt->tokens, SYM_STRUCT);
  if (!tok_struct) return NULL;

  Scope *scope = scope_create(cnt->scope_arena, cnt->scope, 256);
  push_scope(cnt, scope);

  Token *tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_LBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "expected struct member list");
    pop_scope(cnt);
    return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok_struct, AstType *);
  }

  AstTypeStruct *type_struct =
      ast_create_type(cnt->ast_arena, AST_TYPE_STRUCT, tok_struct, AstTypeStruct *);
  type_struct->raw = false;

  /* parse arg types */
  bool  rq       = false;
  Ast **member   = &type_struct->members;
  int * membersc = &type_struct->membersc;

  const bool type_only = tokens_peek_2nd(cnt->tokens)->sym == SYM_COMMA ||
                         tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK;
  type_struct->raw = type_only;
next:
  *member = (Ast *)parse_decl_member(cnt, type_only, *membersc);
  if (*member) {
    member = &(*member)->next;
    ++(*membersc);

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto next;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected member after comma ','");

      pop_scope(cnt);
      return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok_struct, AstType *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RBLOCK) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of member list  '}'  or another memeber separated by comma");
    tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
    pop_scope(cnt);
    return ast_create_type(cnt->ast_arena, AST_TYPE_BAD, tok_struct, AstType *);
  }

  pop_scope(cnt);
  return &type_struct->base;
}

AstDecl *
parse_decl(Context *cnt)
{
#define RETURN_BAD                                                                                 \
  {                                                                                                \
    pop_curr_decl(cnt);                                                                            \
    return ast_create_decl(cnt->ast_arena, AST_DECL_BAD, tok_ident, AstDecl *);                             \
  }

  Token *tok_ident = tokens_peek(cnt->tokens);
  if (token_is_not(tok_ident, SYM_IDENT)) return NULL;
  /* is value declaration? */
  if (token_is(tokens_peek_2nd(cnt->tokens), SYM_ASSIGN)) return NULL;
  Token *tok_lookehead = tokens_peek_2nd(cnt->tokens);
  switch (tok_lookehead->sym) {
  case SYM_IDENT:
  case SYM_ASTERISK:
  case SYM_FN:
  case SYM_STRUCT:
  case SYM_ENUM:
  case SYM_IMMDECL:
  case SYM_MDECL:
  case SYM_COMMA:
  case SYM_RBLOCK:
  case SYM_VARGS:
  case SYM_LBRACKET:
  case SYM_TYPE:
    break;
  default:
    return NULL;
  }

  AstIdent *ident = parse_ident(cnt);
  if (!ident) return NULL;

  AstDeclEntity *decl        = ast_create_decl(cnt->ast_arena, AST_DECL_ENTITY, tok_ident, AstDeclEntity *);
  decl->base.name           = ident;
  decl->mutable = true;

  push_curr_decl(cnt, decl);

  decl->base.type       = parse_type(cnt);
  Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_MDECL);
  if (!tok_assign) tok_assign = tokens_consume_if(cnt->tokens, SYM_IMMDECL);

  if (tok_assign) {
    decl->value   = parse_expr(cnt);
    decl->mutable = token_is(tok_assign, SYM_MDECL);
    decl->flags |= parse_flags(cnt, FLAG_EXTERN | FLAG_COMPILER);

    if (!(decl->flags & (FLAG_EXTERN | FLAG_COMPILER))) {
      if (!decl->value) {
        parse_error(cnt, ERR_EXPECTED_INITIALIZATION, tok_assign, BUILDER_CUR_AFTER,
                    "expected binding of declaration to some value");
        RETURN_BAD;
      }
    }
  }

  pop_curr_decl(cnt);
  return &decl->base;

#undef RETURN_BAD
}

AstExpr *
parse_expr_call(Context *cnt, AstExpr *prev)
{
  if (!prev) return NULL;

  Token *tok = tokens_consume_if(cnt->tokens, SYM_LPAREN);
  if (!tok) return NULL;

  AstExprCall *call = ast_create_expr(cnt->ast_arena, AST_EXPR_CALL, tok, AstExprCall *);
  call->ref         = prev;
  call->run         = false;

  /* parse args */
  bool  rq    = false;
  Ast **arg   = &call->args;
  int * argsc = &call->argsc;
arg:
  *arg = (Ast *)parse_expr(cnt);
  if (*arg) {
    ++(*argsc);
    arg = &(*arg)->next;

    if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
      rq = true;
      goto arg;
    }
  } else if (rq) {
    Token *tok_err = tokens_peek(cnt->tokens);
    if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
      parse_error(cnt, ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD,
                  "expected function argument after comma ','");
      return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok, AstExpr *);
    }
  }

  tok = tokens_consume(cnt->tokens);
  if (tok->sym != SYM_RPAREN) {
    parse_error(cnt, ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD,
                "expected end of parameter list ')' or another parameter separated by comma");
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok, AstExpr *);
  }

  return &call->base;
}

AstExpr *
parse_expr_null(Context *cnt)
{
  Token *tok_null = tokens_consume_if(cnt->tokens, SYM_NULL);
  if (!tok_null) return NULL;
  return ast_create_expr(cnt->ast_arena, AST_EXPR_NULL, tok_null, AstExpr *);
}

int
parse_flags(Context *cnt, int allowed)
{
#define CASE(_sym, _flag)                                                                          \
  case _sym: {                                                                                     \
    tokens_consume(cnt->tokens);                                                                   \
    if (!(allowed & _flag)) {                                                                      \
      parse_error(cnt, ERR_UNEXPECTED_MODIF, tok, BUILDER_CUR_WORD, "unexpected flag '%s'",        \
                  sym_strings[_sym]);                                                              \
    } else {                                                                                       \
      flags |= _flag;                                                                              \
    }                                                                                              \
    goto next;                                                                                     \
  }

  int    flags = 0;
  Token *tok;
next:
  tok = tokens_peek(cnt->tokens);
  switch (tok->sym) {
    CASE(SYM_EXTERN, FLAG_EXTERN)
    CASE(SYM_COMPILER, FLAG_COMPILER)
  default:
    break;
  }

  return flags;
#undef CASE
}

Ast *
parse_load(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_LOAD);
  if (!tok_id) return NULL;

  Token *tok_path = tokens_consume(cnt->tokens);
  if (!token_is(tok_path, SYM_STRING)) {
    parse_error(cnt, ERR_EXPECTED_STRING, tok_path, BUILDER_CUR_WORD,
                "expected path string after load preprocessor directive");
  }

  AstLoad *load  = ast_create_node(cnt->ast_arena, AST_LOAD, tok_id, AstLoad *);
  load->filepath = tok_path->value.str;

  Unit *unit = unit_new_file(load->filepath, tok_path);
  if (!assembly_add_unit_unique(cnt->assembly, unit)) {
    unit_delete(unit);
  }

  return &load->base;
}

AstExpr *
parse_expr_line(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LINE);
  if (!tok_begin) return NULL;

  AstExprLitInt *lit =
      ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_INT, tok_begin, AstExprLitInt *);
  lit->i = (uint64_t) tok_begin->src.line;
  return &lit->base;
}

AstExpr *
parse_expr_file(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_FILE);
  if (!tok_begin) return NULL;

  AstExprLitString *lit =
      ast_create_expr(cnt->ast_arena, AST_EXPR_LIT_STRING, tok_begin, AstExprLitString *);
  lit->s = tok_begin->src.unit->filepath;
  return &lit->base;
}

Ast *
parse_link(Context *cnt)
{
  Token *tok_id = tokens_consume_if(cnt->tokens, SYM_LINK);
  if (!tok_id) return NULL;

  Token *tok_path = tokens_consume(cnt->tokens);
  if (!token_is(tok_path, SYM_STRING)) {
    parse_error(cnt, ERR_EXPECTED_STRING, tok_path, BUILDER_CUR_WORD,
                "expected path string after link preprocessor directive");
  }

  AstLink *link = ast_create_node(cnt->ast_arena, AST_LINK, tok_id, AstLink *);
  link->lib     = tok_path->value.str;
  assembly_add_link(cnt->assembly, link->lib);

  return &link->base;
}

AstExpr *
parse_expr_type(Context *cnt)
{
  Token *  tok  = tokens_peek(cnt->tokens);
  AstType *type = NULL;

  type = parse_type_struct(cnt);
  if (!type) type = parse_type_arr(cnt);
  if (!type) type = parse_type_enum(cnt);

  if (type) {
    AstExpr *expr = ast_create_expr(cnt->ast_arena, AST_EXPR_TYPE, tok, AstExpr *);
    expr->type    = type;
    return expr;
  }

  return NULL;
}

AstExpr *
parse_expr_run(Context *cnt)
{
  Token *tok = tokens_consume_if(cnt->tokens, SYM_RUN);
  if (!tok) return NULL;
  bl_abort("unimplemeted");

  /* TODO: implement as separate node */
  /* TODO: implement as separate node */
  /* TODO: implement as separate node */
  /* TODO: implement as separate node */
  /* TODO: implement as separate node */
  /*

  AstExpr *expr = parse_expr(cnt);
  if (!expr) {
    parse_error(cnt, ERR_EXPECTED_EXPR, tok, BUILDER_CUR_AFTER,
                "expected call after '#run' directive");
    return ast_create_expr(cnt->ast_arena, AST_EXPR_BAD, tok, AstExpr *);
  }

  ((AstExprCall *)call)->run = true;
  return call;
  */
}

Ast *
parse_block(Context *cnt)
{
  Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
  if (!tok_begin) return NULL;

  Scope *scope = scope_create(cnt->scope_arena, cnt->scope, 1024);
  push_scope(cnt, scope);

  AstBlock *block = ast_create_node(cnt->ast_arena, AST_BLOCK, tok_begin, AstBlock *);

  Token *tok;
  Ast ** node = &block->nodes;
next:
  if (tokens_current_is(cnt->tokens, SYM_SEMICOLON)) {
    tok = tokens_consume(cnt->tokens);
    parse_warning(cnt, tok, BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
    goto next;
  }

  parse_flags(cnt, 0);

  if ((*node = parse_stmt_return(cnt))) {
    if ((*node)->kind != AST_BAD) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_if(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_loop(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_break(cnt))) {
    if ((*node)->kind != AST_BAD) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_stmt_continue(cnt))) {
    if ((*node)->kind != AST_BAD) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = (Ast *)parse_decl(cnt))) {
    if ((*node)->kind != AST_BAD) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_block(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = (Ast *)parse_expr(cnt))) {
    if ((*node)->kind != AST_BAD) parse_semicolon_rq(cnt);
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_load(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_link(cnt))) {
    insert_node(&node);
    goto next;
  }

  tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
  if (!tok) {
    tok = tokens_peek_prev(cnt->tokens);
    parse_error(cnt, ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "expected end of block '}'");
    parse_note(cnt, tok_begin, BUILDER_CUR_WORD, "block starting here");
    pop_scope(cnt);
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, Ast *);
  }

  pop_scope(cnt);
  return &block->base;
}

void
parse_ublock_content(Context *cnt, AstUBlock *ublock)
{
  Ast **node = &ublock->nodes;
next:
  parse_flags(cnt, 0);

  if ((*node = (Ast *)parse_decl(cnt))) {
    if ((*node) != AST_BAD) {
      parse_semicolon_rq(cnt);
      /* setup global scope flag for declaration */
      ((AstDeclEntity *)*node)->in_gscope = true;
    }
    insert_node(&node);
    goto next;
  }

  /* TODO: move to builder!!! */
  /* TODO: move to builder!!! */
  /* TODO: move to builder!!! */
  /* TODO: move to builder!!! */
  /* TODO: move to builder!!! */
  if (!(cnt->builder->flags & BUILDER_NO_API) && !cnt->core_loaded && (*node = load_core(cnt))) {
    insert_node(&node);
    cnt->core_loaded = true;
    goto next;
  }

  if ((*node = parse_load(cnt))) {
    insert_node(&node);
    goto next;
  }

  if ((*node = parse_link(cnt))) {
    insert_node(&node);
    goto next;
  }

  Token *tok = tokens_peek(cnt->tokens);
  if (!token_is(tok, SYM_EOF)) {
    parse_error(cnt, ERR_UNEXPECTED_SYMBOL, tok, BUILDER_CUR_WORD,
                "unexpected symbol in module body '%s'", sym_strings[tok->sym]);
  }
}

Ast *
load_core(Context *cnt)
{
  Unit *unit = unit_new_file(CORE_SOURCE_FILE, NULL);
  if (!assembly_add_unit_unique(cnt->assembly, unit)) {
    unit_delete(unit);
  }
  AstLoad *load  = ast_create_node(cnt->ast_arena, AST_LOAD, NULL, AstLoad *);
  load->filepath = CORE_SOURCE_FILE;
  return &load->base;
}

void
parser_run(Builder *builder, Assembly *assembly, Unit *unit)
{
  AstUBlock *_root = ast_create_node(&assembly->ast_arena, AST_UBLOCK, NULL, AstUBlock *);
  _root->unit      = unit;
  unit->ast        = _root;

  Context cnt = {.builder     = builder,
                 .assembly    = assembly,
                 .scope       = assembly->gscope,
                 .unit        = unit,
                 .ast_arena   = &assembly->ast_arena,
                 .scope_arena = &assembly->scope_arena,
                 .tokens      = &unit->tokens,
                 .curr_decl   = NULL,
                 .core_loaded = false,
                 .inside_loop = false};

  parse_ublock_content(&cnt, unit->ast);
}
