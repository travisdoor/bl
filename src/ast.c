//************************************************************************************************
// bl
//
// File:   ast.c
// Author: Martin Dorazil
// Date:   15/03/2018
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

#include <math.h>
#include "ast.h"
#include "arena.h"

#define ARENA_CHUNK_SIZE 256

typedef struct Chunk
{
  struct Chunk *next;
  int           count;
} chunk_t;

Node type_type = {.code           = NODE_TYPE_TYPE,
                  .src            = NULL,
                  .next           = NULL,
                  .type_type.name = NULL,
                  .type_type.spec = NULL};

Node ftypes[] = {
#define ft(name, str)                                                                              \
  {.code = NODE_TYPE_FUND, .src = NULL, .next = NULL, .type_fund.code = FTYPE_##name},

    _FTYPE_LIST
#undef ft
};

const char *ftype_strings[] = {
#define ft(code, name) #name,
    _FTYPE_LIST
#undef ft
};

const char *buildin_strings[] = {
#define bt(code, name) #name,
    _BUILDINS_LIST
#undef bt
};

const char *node_type_strings[] = {
#define nt(code, Name, name, data) #Name,
    _NODE_TYPE_LIST
#undef nt
};

uint64_t ftype_hashes[FTYPE_COUNT];
uint64_t buildin_hashes[BUILDIN_COUNT];

void
ast_node_terminate(Node *node)
{
  switch (node->code) {
  case NODE_DECL:
    bo_unref(ast_peek_decl(node)->deps);
    break;
  default:
    break;
  }
}

#define ast_create_node(arena, c, tok, t) (t) _ast_create_node((arena), (c), (tok));

static Node *
_ast_create_node(Arena *arena, NodeCode c, Token *tok)
{
  Node *node = arena_alloc(arena);
  node->code = c;
  node->src  = tok ? &tok->src : NULL;

#if BL_DEBUG
  static int serial = 0;
  node->_serial     = serial++;
  node->_state      = NOT_CHECKED;
#endif
  return node;
}

/* public */
void
ast_init_statics(void)
{
  const char *it;
  array_foreach(ftype_strings, it)
  {
    ftype_hashes[i] = bo_hash_from_str(it);
  }

  array_foreach(buildin_strings, it)
  {
    buildin_hashes[i] = bo_hash_from_str(it);
  }
}

/*************************************************************************************************
 * node constructors
 *************************************************************************************************/

_NODE_CTOR(bad)
{
  return ast_create_node(_arena, NODE_BAD, _tok, Node *);
}

_NODE_CTOR(load, const char *filepath)
{
  NodeLoad *_load = ast_create_node(_arena, NODE_LOAD, _tok, NodeLoad *);
  _load->filepath = filepath;
  return (Node *)_load;
}

_NODE_CTOR(link, const char *lib)
{
  NodeLink *_link = ast_create_node(_arena, NODE_LINK, _tok, NodeLink *);
  _link->lib      = lib;
  return (Node *)_link;
}

_NODE_CTOR(ublock, struct Unit *unit, Scope *scope)
{
  NodeUBlock *_ublock = ast_create_node(_arena, NODE_UBLOCK, _tok, NodeUBlock *);
  _ublock->scope      = scope;
  _ublock->unit       = unit;
  return (Node *)_ublock;
}

_NODE_CTOR(ident, const char *str, Node *ref, Scope *scope)
{
  NodeIdent *_ident = ast_create_node(_arena, NODE_IDENT, _tok, NodeIdent *);
  _ident->hash      = bo_hash_from_str(str);
  _ident->str       = str;
  _ident->ref       = ref;
  _ident->scope     = scope;
  return (Node *)_ident;
}

_NODE_CTOR(stmt_return, Node *expr, Node *fn)
{
  NodeStmtReturn *_ret = ast_create_node(_arena, NODE_STMT_RETURN, _tok, NodeStmtReturn *);
  _ret->expr           = expr;
  _ret->fn_decl        = fn;
  return (Node *)_ret;
}

_NODE_CTOR(stmt_break)
{
  return ast_create_node(_arena, NODE_STMT_BREAK, _tok, Node *);
}

_NODE_CTOR(stmt_continue)
{
  return ast_create_node(_arena, NODE_STMT_CONTINUE, _tok, Node *);
}

_NODE_CTOR(stmt_if, Node *test, Node *true_stmt, Node *false_stmt)
{
  NodeStmtIf *_if = ast_create_node(_arena, NODE_STMT_IF, _tok, NodeStmtIf *);
  _if->test       = test;
  _if->true_stmt  = true_stmt;
  _if->false_stmt = false_stmt;
  return (Node *)_if;
}

_NODE_CTOR(stmt_loop, Node *init, Node *condition, Node *increment, Node *block, Scope *scope)
{
  NodeStmtLoop *_loop = ast_create_node(_arena, NODE_STMT_LOOP, _tok, NodeStmtLoop *);
  _loop->init         = init;
  _loop->condition    = condition;
  _loop->increment    = increment;
  _loop->block        = block;
  _loop->scope        = scope;
  return (Node *)_loop;
}

_NODE_CTOR(block, Node *nodes, Scope *scope)
{
  NodeBlock *_block = ast_create_node(_arena, NODE_BLOCK, _tok, NodeBlock *);
  _block->nodes     = nodes;
  _block->scope     = scope;
  return (Node *)_block;
}

_NODE_CTOR(decl, DeclKind kind, Node *name, Node *type, Node *value, bool mutable, int flags,
           bool in_gscope)
{
  NodeDecl *_decl  = ast_create_node(_arena, NODE_DECL, _tok, NodeDecl *);
  _decl->kind      = kind;
  _decl->type      = type;
  _decl->name      = name;
  _decl->value     = value;
  _decl->mutable   = mutable;
  _decl->flags     = flags;
  _decl->in_gscope = in_gscope;
  return (Node *)_decl;
}

_NODE_CTOR(member, Node *name, Node *type, int order)
{
  NodeMember *_member = ast_create_node(_arena, NODE_MEMBER, _tok, NodeMember *);
  _member->name       = name;
  _member->type       = type;
  _member->order      = order;
  return (Node *)_member;
}

_NODE_CTOR(arg, Node *name, Node *type)
{
  NodeArg *_arg = ast_create_node(_arena, NODE_ARG, _tok, NodeArg *);
  _arg->name    = name;
  _arg->type    = type;
  return (Node *)_arg;
}

_NODE_CTOR(variant, Node *name, Node *type, Node *value)
{
  NodeVariant *_variant = ast_create_node(_arena, NODE_VARIANT, _tok, NodeVariant *);
  _variant->name        = name;
  _variant->type        = type;
  _variant->value       = value;
  return (Node *)_variant;
}

_NODE_CTOR(type_type, Node *name, Node *spec)
{
  NodeTypeType *_type = ast_create_node(_arena, NODE_TYPE_TYPE, _tok, NodeTypeType *);
  _type->name         = name;
  _type->spec         = spec;
  return (Node *)_type;
}

_NODE_CTOR(type_fund, FundType code)
{
  NodeTypeFund *_type_fund = ast_create_node(_arena, NODE_TYPE_FUND, _tok, NodeTypeFund *);
  _type_fund->code         = code;
  return (Node *)_type_fund;
}

_NODE_CTOR(type_vargs)
{
  NodeTypeVArgs *_type_vargs = ast_create_node(_arena, NODE_TYPE_VARGS, _tok, NodeTypeVArgs *);
  return (Node *)_type_vargs;
}

_NODE_CTOR(type_arr, Node *elem_type, Node *len)
{
  NodeTypeArr *_type_arr = ast_create_node(_arena, NODE_TYPE_ARR, _tok, NodeTypeArr *);
  _type_arr->elem_type   = elem_type;
  _type_arr->len         = len;
  return (Node *)_type_arr;
}

_NODE_CTOR(type_fn, Node *arg_types, int argc_types, Node *ret_type)
{
  NodeTypeFn *_type_fn = ast_create_node(_arena, NODE_TYPE_FN, _tok, NodeTypeFn *);
  _type_fn->arg_types  = arg_types;
  _type_fn->argc_types = argc_types;
  _type_fn->ret_type   = ret_type;
  return (Node *)_type_fn;
}

_NODE_CTOR(type_struct, Node *members, int membersc, Scope *scope, bool raw)
{
  NodeTypeStruct *_type_struct = ast_create_node(_arena, NODE_TYPE_STRUCT, _tok, NodeTypeStruct *);
  _type_struct->members        = members;
  _type_struct->membersc       = membersc;
  _type_struct->scope          = scope;
  _type_struct->raw            = raw;
  return (Node *)_type_struct;
}

_NODE_CTOR(type_ptr, Node *type)
{
  NodeTypePtr *_type_ptr = ast_create_node(_arena, NODE_TYPE_PTR, _tok, NodeTypePtr *);
  _type_ptr->type        = type;
  return (Node *)_type_ptr;
}

_NODE_CTOR(lit_fn, Node *type, Node *block, Scope *scope)
{
  NodeLitFn *_lit_fn = ast_create_node(_arena, NODE_LIT_FN, _tok, NodeLitFn *);
  _lit_fn->type      = type;
  _lit_fn->block     = block;
  _lit_fn->scope     = scope;
  return (Node *)_lit_fn;
}

_NODE_CTOR(type_enum, Node *type, Node *variants, Scope *scope)
{
  NodeTypeEnum *_type_enum = ast_create_node(_arena, NODE_TYPE_ENUM, _tok, NodeTypeEnum *);
  _type_enum->type         = type;
  _type_enum->scope        = scope;
  _type_enum->variants     = variants;
  return (Node *)_type_enum;
}

_NODE_CTOR(lit, Node *type, TokenValue value)
{
  NodeLit *_lit = ast_create_node(_arena, NODE_LIT, _tok, NodeLit *);
  _lit->type    = type;
  _lit->value   = value;
  return (Node *)_lit;
}

_NODE_CTOR(expr_binop, Node *lhs, Node *rhs, Node *type, BinopKind kind)
{
  NodeExprBinop *_expr_binop = ast_create_node(_arena, NODE_EXPR_BINOP, _tok, NodeExprBinop *);
  _expr_binop->lhs           = lhs;
  _expr_binop->rhs           = rhs;
  _expr_binop->type          = type;
  _expr_binop->kind          = kind;
  return (Node *)_expr_binop;
}

_NODE_CTOR(expr_call, Node *ref, Node *args, int argsc, Node *type, bool run)
{
  NodeExprCall *_expr_call = ast_create_node(_arena, NODE_EXPR_CALL, _tok, NodeExprCall *);
  _expr_call->ref          = ref;
  _expr_call->args         = args;
  _expr_call->argsc        = argsc;
  _expr_call->type         = type;
  _expr_call->run          = run;
  return (Node *)_expr_call;
}

_NODE_CTOR(expr_member, MemberKind kind, Node *ident, Node *next, Node *type, bool ptr_ref, int i)
{
  NodeExprMember *_expr_member = ast_create_node(_arena, NODE_EXPR_MEMBER, _tok, NodeExprMember *);
  _expr_member->kind           = kind;
  _expr_member->ident          = ident;
  _expr_member->next           = next;
  _expr_member->type           = type;
  _expr_member->ptr_ref        = ptr_ref;
  _expr_member->i              = i;
  return (Node *)_expr_member;
}

_NODE_CTOR(expr_elem, Node *next, Node *type, Node *index)
{
  NodeExprElem *_expr_elem = ast_create_node(_arena, NODE_EXPR_ELEM, _tok, NodeExprElem *);
  _expr_elem->next         = next;
  _expr_elem->type         = type;
  _expr_elem->index        = index;
  return (Node *)_expr_elem;
}

_NODE_CTOR(expr_sizeof, Node *in, Node *type)
{
  NodeExprSizeof *_expr_sizeof = ast_create_node(_arena, NODE_EXPR_SIZEOF, _tok, NodeExprSizeof *);
  _expr_sizeof->in             = in;
  _expr_sizeof->type           = type;
  return (Node *)_expr_sizeof;
}

_NODE_CTOR(expr_typeof, Node *in, Node *type)
{
  NodeExprTypeof *_expr_typeof = ast_create_node(_arena, NODE_EXPR_TYPEOF, _tok, NodeExprTypeof *);
  _expr_typeof->in             = in;
  _expr_typeof->type           = type;
  return (Node *)_expr_typeof;
}

_NODE_CTOR(expr_cast, Node *type, Node *next)
{
  NodeExprCast *_expr_cast = ast_create_node(_arena, NODE_EXPR_CAST, _tok, NodeExprCast *);
  _expr_cast->type         = type;
  _expr_cast->next         = next;
  return (Node *)_expr_cast;
}

_NODE_CTOR(expr_unary, UnopKind kind, Node *next, Node *type)
{
  NodeExprUnary *_expr_unary = ast_create_node(_arena, NODE_EXPR_UNARY, _tok, NodeExprUnary *);
  _expr_unary->next          = next;
  _expr_unary->type          = type;
  _expr_unary->kind          = kind;
  return (Node *)_expr_unary;
}

_NODE_CTOR(expr_null, Node *type)
{
  NodeExprNull *_expr_null = ast_create_node(_arena, NODE_EXPR_NULL, _tok, NodeExprNull *);
  _expr_null->type         = type;
  return (Node *)_expr_null;
}

_NODE_CTOR(lit_cmp, Node *type, Node *fields, int fieldc, Scope *scope)
{
  NodeLitCmp *_lit_cmp = ast_create_node(_arena, NODE_LIT_CMP, _tok, NodeLitCmp *);
  _lit_cmp->type       = type;
  _lit_cmp->fields     = fields;
  _lit_cmp->fieldc     = fieldc;
  _lit_cmp->scope      = scope;
  return (Node *)_lit_cmp;
}

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

void
visitor_init(Visitor *visitor)
{
  /* default value for all visitor callbacks */
  memset(visitor->visitors, 0, sizeof(VisitorFunc) * NODE_COUNT);
  visitor->all_visitor = NULL;
}

void
visitor_add(Visitor *visitor, VisitorFunc fn, NodeCode code)
{
  visitor->visitors[code] = fn;
}

void
visitor_add_visit_all(Visitor *visitor, VisitorFunc fn)
{
  visitor->all_visitor = fn;
}

void
visitor_visit(Visitor *visitor, Node *node, void *cnt)
{
  if (!node) return;
  if (visitor->all_visitor) visitor->all_visitor(visitor, node, cnt);
  if (visitor->visitors[ast_node_code(node)])
    visitor->visitors[ast_node_code(node)](visitor, node, cnt);
  else
    visitor_walk(visitor, node, cnt);
}

void
visitor_walk(Visitor *visitor, Node *node, void *cnt)
{
#define visit(node) visitor_visit(visitor, node, cnt)
  if (!node) return;
  Node *tmp = NULL;

  if (!node) return;
  switch (ast_node_code(node)) {

  case NODE_UBLOCK: {
    node_foreach(ast_peek_ublock(node)->nodes, tmp) visit(tmp);
    break;
  }

  case NODE_BLOCK: {
    node_foreach(ast_peek_block(node)->nodes, tmp) visit(tmp);
    break;
  }

  case NODE_DECL: {
    NodeDecl *_decl = ast_peek_decl(node);
    visit(_decl->name);
    visit(_decl->type);
    visit(_decl->value);
    break;
  }

  case NODE_MEMBER: {
    break;
  }

  case NODE_ARG: {
    break;
  }

  case NODE_VARIANT: {
    NodeVariant *_var = ast_peek_variant(node);
    visit(_var->value);
    break;
  }

  case NODE_TYPE_FUND: {
    break;
  }

  case NODE_TYPE_FN: {
    break;
  }

  case NODE_TYPE_STRUCT: {
    NodeTypeStruct *_struct = ast_peek_type_struct(node);
    node_foreach(_struct->members, tmp) visit(tmp);
    break;
  }

  case NODE_TYPE_ENUM: {
    NodeTypeEnum *_enum = ast_peek_type_enum(node);
    node_foreach(_enum->variants, tmp) visit(tmp);
    break;
  }

  case NODE_EXPR_BINOP: {
    NodeExprBinop *_binop = ast_peek_expr_binop(node);
    visit(_binop->lhs);
    visit(_binop->rhs);
    break;
  }

  case NODE_EXPR_CALL: {
    NodeExprCall *_call = ast_peek_expr_call(node);
    node_foreach(_call->args, tmp) visit(tmp);
    break;
  }

  case NODE_EXPR_CAST: {
    visit(ast_peek_expr_cast(node)->next);
    break;
  }

  case NODE_EXPR_UNARY: {
    visit(ast_peek_expr_unary(node)->next);
    break;
  }

  case NODE_EXPR_MEMBER: {
    visit(ast_peek_expr_member(node)->next);
    break;
  }
  case NODE_EXPR_ELEM: {
    visit(ast_peek_expr_elem(node)->index);
    visit(ast_peek_expr_elem(node)->next);
    break;
  }

  case NODE_STMT_RETURN: {
    visit(ast_peek_stmt_return(node)->expr);
    break;
  }

  case NODE_STMT_IF: {
    visit(ast_peek_stmt_if(node)->test);
    visit(ast_peek_stmt_if(node)->true_stmt);
    visit(ast_peek_stmt_if(node)->false_stmt);
    break;
  }

  case NODE_STMT_LOOP: {
    visit(ast_peek_stmt_loop(node)->init);
    visit(ast_peek_stmt_loop(node)->condition);
    visit(ast_peek_stmt_loop(node)->increment);
    visit(ast_peek_stmt_loop(node)->block);
    break;
  }

  case NODE_LIT_CMP: {
    NodeLitCmp *_lit_cmp = ast_peek_lit_cmp(node);
    visit(_lit_cmp->type);
    node_foreach(_lit_cmp->fields, tmp) visit(tmp);
    break;
  }

  case NODE_LIT_FN: {
    visit(ast_peek_lit_fn(node)->block);
    break;
  }

  /* defaults (terminal cases) */
  default:
    break;
  }

#undef visit
}

/*************************************************************************************************
 * other
 *************************************************************************************************/

static void
_type_to_string(char *buf, size_t len, Node *type)
{
#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }
  if (!buf) return;
  if (!type) {
    append_buf(buf, len, "?");
    return;
  }

  switch (ast_node_code(type)) {
  case NODE_IDENT: {
    // identificator can lead to type
    append_buf(buf, len, ast_peek_ident(type)->str);
    break;
  }

  case NODE_DECL: {
    _type_to_string(buf, len, ast_peek_decl(type)->value);
    break;
  }

  case NODE_TYPE_PTR: {
    NodeTypePtr *_ptr = ast_peek_type_ptr(type);
    append_buf(buf, len, "*");
    _type_to_string(buf, len, _ptr->type);
    break;
  }

  case NODE_TYPE_FUND: {
    append_buf(buf, len, ftype_strings[ast_peek_type_fund(type)->code]);
    break;
  }

  case NODE_TYPE_FN: {
    NodeTypeFn *_fn = ast_peek_type_fn(type);
    append_buf(buf, len, "fn (");
    Node *arg = _fn->arg_types;
    while (arg) {
      _type_to_string(buf, len, arg);
      arg = arg->next;
      if (arg) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, ") ");
    _type_to_string(buf, len, _fn->ret_type);
    break;
  }

  case NODE_TYPE_STRUCT: {
    NodeTypeStruct *_struct = ast_peek_type_struct(type);
    append_buf(buf, len, "struct {");

    Node *t = _struct->members;
    while (t) {
      _type_to_string(buf, len, t);
      t = t->next;
      if (t) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, "}");
    break;
  }

  case NODE_TYPE_ENUM: {
    NodeTypeEnum *_enum = ast_peek_type_enum(type);
    append_buf(buf, len, "enum ");
    _type_to_string(buf, len, _enum->type);
    break;
  }

  case NODE_TYPE_ARR: {
    NodeTypeArr *_arr = ast_peek_type_arr(type);
    append_buf(buf, len, "[]");
    _type_to_string(buf, len, _arr->elem_type);
    break;
  }

  case NODE_MEMBER: {
    NodeMember *_mem = ast_peek_member(type);
    _type_to_string(buf, len, _mem->type);
    break;
  }

  case NODE_ARG: {
    NodeArg *_arg = ast_peek_arg(type);
    _type_to_string(buf, len, _arg->type);
    break;
  }

  case NODE_TYPE_TYPE: {
    NodeTypeType *_type = ast_peek_type_type(type);
    if (_type->name) {
      append_buf(buf, len, ast_peek_ident(_type->name)->str);
    } else {
      append_buf(buf, len, "type");
    }
    break;
  }

  case NODE_BAD: {
    append_buf(buf, len, "bad");
    break;
  }

  default:
    bl_abort("%s is not valid type", ast_node_name(type));
  }

#undef append_buf
}

void
ast_type_to_string(char *buf, size_t len, Node *type)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_string(buf, len, type);
}

Node *
ast_get_type(Node *node)
{
next:
  if (!node) return NULL;
  if (ast_node_is_type(node)) return node;

  switch (ast_node_code(node)) {
  case NODE_DECL:
    node = ast_peek_decl(node)->type;
    goto next;
  case NODE_MEMBER:
    node = ast_peek_member(node)->type;
    goto next;
  case NODE_VARIANT:
    node = ast_peek_variant(node)->type;
    goto next;
  case NODE_ARG:
    node = ast_peek_arg(node)->type;
    goto next;
  case NODE_LIT:
    node = ast_peek_lit(node)->type;
    goto next;
  case NODE_LIT_FN:
    return ast_peek_lit_fn(node)->type;
    goto next;
  case NODE_LIT_CMP:
    node = ast_peek_lit_cmp(node)->type;
    goto next;
  case NODE_IDENT:
    node = ast_peek_ident(node)->ref;
    goto next;
  case NODE_EXPR_CALL:
    node = ast_peek_expr_call(node)->type;
    goto next;
  case NODE_EXPR_BINOP:
    node = ast_peek_expr_binop(node)->type;
    goto next;
  case NODE_EXPR_SIZEOF:
    node = ast_peek_expr_sizeof(node)->type;
    goto next;
  case NODE_EXPR_TYPEOF:
    node = ast_peek_expr_typeof(node)->type;
    goto next;
  case NODE_EXPR_CAST:
    node = ast_peek_expr_cast(node)->type;
    goto next;
  case NODE_EXPR_UNARY:
    node = ast_peek_expr_unary(node)->type;
    goto next;
  case NODE_EXPR_NULL:
    node = ast_peek_expr_null(node)->type;
    goto next;
  case NODE_EXPR_MEMBER:
    node = ast_peek_expr_member(node)->type;
    goto next;
  case NODE_EXPR_ELEM:
    node = ast_peek_expr_elem(node)->type;
    goto next;
  default:
    bl_abort("node %s has no type", ast_node_name(node));
  }

  return NULL;
}

int
ast_is_buildin_type(Node *ident)
{
  assert(ident);
  NodeIdent *_ident = ast_peek_ident(ident);

  uint64_t hash;
  array_foreach(ftype_hashes, hash)
  {
    if (_ident->hash == hash) return (int)i;
  }

  return -1;
}

int
ast_is_buildin(Node *ident)
{
  assert(ident);
  NodeIdent *_ident = ast_peek_ident(ident);

  uint64_t hash;
  array_foreach(buildin_hashes, hash)
  {
    if (_ident->hash == hash) return (int)i;
  }

  return -1;
}

bool
ast_type_cmp(Node *first, Node *second)
{
  Node *f = ast_get_type(first);
  Node *s = ast_get_type(second);

  assert(f && s);
  if (f == s) return true;
  TypeKind fkind = ast_type_kind(f);
  TypeKind skind = ast_type_kind(s);

  if (fkind == TYPE_KIND_ENUM) {
    f     = ast_get_type(ast_peek_type_enum(f)->type);
    fkind = ast_type_kind(f);
  }
  if (skind == TYPE_KIND_ENUM) {
    s     = ast_get_type(ast_peek_type_enum(s)->type);
    skind = ast_type_kind(s);
  }

  if (fkind != skind) return false;
  if (ast_node_code(f) != ast_node_code(s)) return false;

  // same nodes
  switch (ast_node_code(f)) {

  case NODE_TYPE_FUND: {
    if (ast_peek_type_fund(f)->code != ast_peek_type_fund(s)->code) return false;
    break;
  }

  case NODE_TYPE_FN: {
    NodeTypeFn *_f = ast_peek_type_fn(f);
    NodeTypeFn *_s = ast_peek_type_fn(s);

    if (_f->argc_types != _s->argc_types) return false;
    if (!ast_type_cmp(_f->ret_type, _s->ret_type)) return false;

    Node *argt1 = _f->arg_types;
    Node *argt2 = _s->arg_types;
    while (argt1 && argt2) {
      if (!ast_type_cmp(argt1, argt2)) return false;

      argt1 = argt1->next;
      argt2 = argt2->next;
    }

    break;
  }

  case NODE_TYPE_STRUCT: {
    NodeTypeStruct *_f = ast_peek_type_struct(f);
    NodeTypeStruct *_s = ast_peek_type_struct(s);

    if (_f->membersc != _s->membersc) return false;

    Node *type1 = _f->members;
    Node *type2 = _s->members;
    while (type1 && type2) {
      if (!ast_type_cmp(type1, type2)) return false;

      type1 = type1->next;
      type2 = type2->next;
    }
    break;
  }

  case NODE_TYPE_ARR: {
    NodeTypeArr *_f = ast_peek_type_arr(f);
    NodeTypeArr *_s = ast_peek_type_arr(s);
    /* TODO: compare lens!!! */
    return ast_type_cmp(_f->elem_type, _s->elem_type);
  }

  case NODE_TYPE_TYPE: {
    return true;
  }

  default:
    bl_abort("missing comparation of %s type", ast_node_name(f));
  }

  return true;
}

TypeKind
ast_type_kind(Node *type)
{
  assert(type);
  switch (ast_node_code(type)) {
  case NODE_TYPE_FUND: {
    NodeTypeFund *_ftype = ast_peek_type_fund(type);

    switch (_ftype->code) {
    case FTYPE_VOID:
      return TYPE_KIND_VOID;
    case FTYPE_S8:
    case FTYPE_S16:
    case FTYPE_S32:
    case FTYPE_S64:
      return TYPE_KIND_SINT;
    case FTYPE_U8:
    case FTYPE_U16:
    case FTYPE_U32:
    case FTYPE_U64:
      return TYPE_KIND_UINT;
    case FTYPE_SIZE:
      return TYPE_KIND_SIZE;
    case FTYPE_F32:
    case FTYPE_F64:
      return TYPE_KIND_REAL;
    case FTYPE_CHAR:
      return TYPE_KIND_CHAR;
    case FTYPE_STRING:
      return TYPE_KIND_STRING;
    case FTYPE_BOOL:
      return TYPE_KIND_BOOL;
    case FTYPE_COUNT:
      break;
    }
    break;
  }

  case NODE_TYPE_FN:
    return TYPE_KIND_FN;

  case NODE_TYPE_STRUCT:
    return TYPE_KIND_STRUCT;

  case NODE_TYPE_ENUM:
    return TYPE_KIND_ENUM;

  case NODE_TYPE_ARR:
    return TYPE_KIND_ARR;

  case NODE_TYPE_PTR:
    return TYPE_KIND_PTR;

  case NODE_TYPE_TYPE:
    return TYPE_KIND_TYPE;

  case NODE_DECL: {
    NodeDecl *_decl = ast_peek_decl(type);
    return ast_type_kind(_decl->value);
  }

  case NODE_IDENT: {
    return ast_type_kind(ast_peek_ident(type)->ref);
  }

  default:
    bl_abort("node %s is not a type", ast_node_name(type));
  }

  return TYPE_KIND_INVALID;
}

bool
ast_can_impl_cast(Node *from_type, Node *to_type)
{
  assert(from_type);
  assert(to_type);

  from_type = ast_get_type(from_type);
  to_type   = ast_get_type(to_type);

  TypeKind fkind = ast_type_kind(from_type);
  TypeKind tkind = ast_type_kind(to_type);

  if (tkind == TYPE_KIND_ANY) return true;
  if (fkind == TYPE_KIND_TYPE || tkind == TYPE_KIND_TYPE) return true;
  if (fkind == TYPE_KIND_STRING && tkind == TYPE_KIND_PTR) return true;
  if (tkind == TYPE_KIND_STRING && fkind == TYPE_KIND_PTR) return true;

  /* implicit casting for int types ??? */
  /*if ((fkind == TYPE_KIND_SINT || fkind == TYPE_KIND_UINT || fkind == TYPE_KIND_SIZE) &&
      (tkind == TYPE_KIND_SINT || tkind == TYPE_KIND_UINT || tkind == TYPE_KIND_SIZE))
      return true;*/

  if (tkind == TYPE_KIND_ENUM) {
    return ast_can_impl_cast(from_type, ast_peek_type_enum(to_type)->type);
  }

  if (fkind == TYPE_KIND_STRUCT || fkind == TYPE_KIND_FN) return false;
  if (fkind == TYPE_KIND_ARR || fkind == TYPE_KIND_ARR) return false;
  if (fkind == TYPE_KIND_PTR && ast_node_is(from_type, NODE_TYPE_FN)) return false;

  return fkind == tkind;
}

Node *
ast_node_dup(Arena *arena, Node *node)
{
  Node *tmp = ast_create_node(arena, -1, NULL, Node *);
#if BL_DEBUG
  int tmp_serial = tmp->_serial;
#endif

  memcpy(tmp, node, sizeof(Node));
  tmp->next = NULL;
#if BL_DEBUG
  tmp->_serial = tmp_serial;
#endif

  return tmp;
}

Node *
ast_unroll_ident(Node *ident)
{
  if (!ident) return NULL;
  for (; ast_node_is(ident, NODE_IDENT); ident = ast_peek_ident(ident)->ref)
    ;
  return ident;
}

Dependency *
ast_add_dep_uq(Node *decl, Node *dep, int type)
{
  assert(dep && "invalid dep");
  BHashTable **deps = &ast_peek_decl(decl)->deps;
  Dependency   tmp  = {.node = dep, .type = type};

  if (!*deps) {
    *deps = bo_htbl_new(sizeof(Dependency), 64);
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  } else if (!bo_htbl_has_key(*deps, (uint64_t)dep)) {
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  }

  return &bo_htbl_at(*deps, (uint64_t)dep, Dependency);
}
