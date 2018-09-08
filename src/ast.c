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

#define CHUNK_SIZE 256
#define MAX_ALIGNMENT 16

#define NODE_SIZE (sizeof(Node) + MAX_ALIGNMENT)

typedef struct Chunk
{
  struct Chunk *next;
  int           count;
} chunk_t;

Node ftypes[] = {
#define ft(name, str)                                                                              \
  {.code             = NODE_TYPE_FUND,                                                             \
   .src              = NULL,                                                                       \
   .next             = NULL,                                                                       \
   .n.type_fund.code = FTYPE_##name,                                                               \
   .n.type_fund.arr  = NULL,                                                                       \
   .n.type_fund.ptr  = 0,                                                                          \
   .state            = CHECKED},

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
#define nt(code, name, data) #name,
    _NODE_TYPE_LIST
#undef nt
};

uint64_t ftype_hashes[FTYPE_COUNT];
uint64_t buildin_hashes[BUILDIN_COUNT];

static inline bool
is_aligned(const void *p, size_t size)
{
  return (uintptr_t)p % size == 0;
}

static void
align_ptr_up(void **p, size_t alignment, ptrdiff_t *adjustment)
{
  if (is_aligned(*p, alignment)) {
    *adjustment = 0;
    return;
  }

  const size_t mask = alignment - 1;
  assert((alignment & mask) == 0 && "wrong alignemet"); // pwr of 2
  const uintptr_t i_unaligned  = (uintptr_t)(*p);
  const uintptr_t misalignment = i_unaligned & mask;
  *adjustment                  = alignment - misalignment;
  *p                           = (void *)(i_unaligned + *adjustment);
}

static void
node_terminate(Node *node)
{
  switch (node->code) {
  case NODE_DECL:
    bo_unref(peek_decl(node)->deps);
    break;
  default:
    break;
  }
}

static inline Node *
get_node_in_chunk(chunk_t *chunk, int i)
{
  void *node = (void *)((char *)chunk + (i * NODE_SIZE));
  /* New node pointer in chunk must be aligned. (ALLOCATED SIZE FOR EVERY NODE MUST BE
   * sizeof(node_t) + MAX_ALIGNMENT) */
  ptrdiff_t adj;
  align_ptr_up(&node, MAX_ALIGNMENT, &adj);
  assert(adj < MAX_ALIGNMENT);
  return node;
}

static inline chunk_t *
alloc_chunk(void)
{
  const size_t size_in_bytes = NODE_SIZE * CHUNK_SIZE;
  chunk_t *    chunk         = bl_malloc(size_in_bytes);
  if (!chunk) bl_abort("bad alloc");
  memset(chunk, 0, size_in_bytes);
  chunk->count = 1;
  return chunk;
}

static inline chunk_t *
free_chunk(chunk_t *chunk)
{
  if (!chunk) return NULL;

  chunk_t *next = chunk->next;
  for (int i = 0; i < chunk->count - 1; ++i) {
    node_terminate(get_node_in_chunk(chunk, i + 1));
  }
  bl_free(chunk);
  return next;
}

#define alloc_node(ast, c, tok, t) (t) _alloc_node((ast), (c), (tok));

static Node *
_alloc_node(Ast *ast, NodeCode c, Token *tok)
{
  if (!ast->current_chunk) {
    ast->current_chunk = alloc_chunk();
    ast->first_chunk   = ast->current_chunk;
  }

  if (ast->current_chunk->count == CHUNK_SIZE) {
    // last chunk node
    chunk_t *chunk           = alloc_chunk();
    ast->current_chunk->next = chunk;
    ast->current_chunk       = chunk;
  }

  Node *node = get_node_in_chunk(ast->current_chunk, ast->current_chunk->count);
  ast->current_chunk->count++;

  node->code = c;
  node->src  = tok ? &tok->src : NULL;

#if BL_DEBUG
  static int serial = 0;
  node->_serial     = serial++;
#endif

  assert(is_aligned(node, MAX_ALIGNMENT) && "unaligned allocation of node");

  return node;
}

/* public */
void
ast_init(Ast *ast)
{
  static bool statics_initialized = false;
  ast->first_chunk                = NULL;
  ast->current_chunk              = NULL;

  /* init ftype hashes */
  if (!statics_initialized) {
    statics_initialized = true;
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
}

void
ast_terminate(Ast *ast)
{
  chunk_t *chunk = ast->first_chunk;
  while (chunk) {
    chunk = free_chunk(chunk);
  }
}

/*************************************************************************************************
 * node constructors
 *************************************************************************************************/

_NODE_NCTOR(bad)
{
  return alloc_node(ast, NODE_BAD, tok, Node *);
}

_NODE_NCTOR(load, const char *filepath)
{
  node_load_t *_load = alloc_node(ast, NODE_LOAD, tok, node_load_t *);
  _load->filepath    = filepath;
  return (Node *)_load;
}

_NODE_NCTOR(link, const char *lib)
{
  node_link_t *_link = alloc_node(ast, NODE_LINK, tok, node_link_t *);
  _link->lib         = lib;
  return (Node *)_link;
}

_NODE_NCTOR(ublock, struct Unit *unit, Scope *scope)
{
  node_ublock_t *_ublock = alloc_node(ast, NODE_UBLOCK, tok, node_ublock_t *);
  _ublock->scope         = scope;
  _ublock->unit          = unit;
  return (Node *)_ublock;
}

_NODE_NCTOR(ident, Node *ref, Node *parent_compound, int ptr, Node *arr)
{
  node_ident_t *_ident    = alloc_node(ast, NODE_IDENT, tok, node_ident_t *);
  _ident->hash            = bo_hash_from_str(tok->value.str);
  _ident->str             = tok->value.str;
  _ident->ref             = ref;
  _ident->ptr             = ptr;
  _ident->arr             = arr;
  _ident->parent_compound = parent_compound;
  return (Node *)_ident;
}

_NODE_NCTOR(stmt_return, Node *expr, Node *fn)
{
  node_stmt_return_t *_ret = alloc_node(ast, NODE_STMT_RETURN, tok, node_stmt_return_t *);
  _ret->expr               = expr;
  _ret->fn_decl            = fn;
  return (Node *)_ret;
}

_NODE_NCTOR(stmt_break)
{
  return alloc_node(ast, NODE_STMT_BREAK, tok, Node *);
}

_NODE_NCTOR(stmt_continue)
{
  return alloc_node(ast, NODE_STMT_CONTINUE, tok, Node *);
}

_NODE_NCTOR(stmt_if, Node *test, Node *true_stmt, Node *false_stmt)
{
  node_stmt_if_t *_if = alloc_node(ast, NODE_STMT_IF, tok, node_stmt_if_t *);
  _if->test           = test;
  _if->true_stmt      = true_stmt;
  _if->false_stmt     = false_stmt;
  return (Node *)_if;
}

_NODE_NCTOR(stmt_loop, Node *test, Node *true_stmt)
{
  node_stmt_loop_t *_loop = alloc_node(ast, NODE_STMT_LOOP, tok, node_stmt_loop_t *);
  _loop->test             = test;
  _loop->true_stmt        = true_stmt;
  return (Node *)_loop;
}

_NODE_NCTOR(block, Node *nodes, Node *parent_compound, Scope *scope)
{
  node_block_t *_block    = alloc_node(ast, NODE_BLOCK, tok, node_block_t *);
  _block->nodes           = nodes;
  _block->parent_compound = parent_compound;
  _block->scope           = scope;
  return (Node *)_block;
}

_NODE_NCTOR(decl, DeclKind kind, Node *name, Node *type, Node *value, bool mutable, int flags,
            int order, bool in_gscope)
{
  node_decl_t *_decl = alloc_node(ast, NODE_DECL, tok, node_decl_t *);
  _decl->kind        = kind;
  _decl->type        = type;
  _decl->name        = name;
  _decl->value       = value;
  _decl->mutable     = mutable;
  _decl->flags       = flags;
  _decl->order       = order;
  _decl->in_gscope   = in_gscope;
  return (Node *)_decl;
}

_NODE_NCTOR(type_fund, FundType code, int ptr, Node *arr)
{
  node_type_fund_t *_type_fund = alloc_node(ast, NODE_TYPE_FUND, tok, node_type_fund_t *);
  _type_fund->code             = code;
  _type_fund->ptr              = ptr;
  _type_fund->arr              = arr;
  return (Node *)_type_fund;
}

_NODE_NCTOR(type_fn, Node *arg_types, int argc_types, Node *ret_type, int ptr)
{
  node_type_fn_t *_type_fn = alloc_node(ast, NODE_TYPE_FN, tok, node_type_fn_t *);
  _type_fn->arg_types      = arg_types;
  _type_fn->argc_types     = argc_types;
  _type_fn->ret_type       = ret_type;
  _type_fn->ptr            = ptr;
  return (Node *)_type_fn;
}

_NODE_NCTOR(type_struct, Node *types, int typesc, Node *base_decl, int ptr)
{
  node_type_struct_t *_type_struct = alloc_node(ast, NODE_TYPE_STRUCT, tok, node_type_struct_t *);
  _type_struct->types              = types;
  _type_struct->typesc             = typesc;
  _type_struct->base_decl          = base_decl;
  _type_struct->ptr                = ptr;
  return (Node *)_type_struct;
}

_NODE_NCTOR(type_enum, Node *type, Node *base_decl, int ptr)
{
  node_type_enum_t *_type_enum = alloc_node(ast, NODE_TYPE_ENUM, tok, node_type_enum_t *);
  _type_enum->base_decl        = base_decl;
  _type_enum->base_type        = type;
  _type_enum->ptr              = ptr;
  return (Node *)_type_enum;
}

_NODE_NCTOR(lit_fn, Node *type, Node *block, Node *parent_compound, Scope *scope)
{
  node_lit_fn_t *_lit_fn   = alloc_node(ast, NODE_LIT_FN, tok, node_lit_fn_t *);
  _lit_fn->type            = type;
  _lit_fn->block           = block;
  _lit_fn->parent_compound = parent_compound;
  _lit_fn->scope           = scope;
  return (Node *)_lit_fn;
}

_NODE_NCTOR(lit_struct, Node *type, Node *parent_compound, Scope *scope)
{
  node_lit_struct_t *_lit_struct = alloc_node(ast, NODE_LIT_STRUCT, tok, node_lit_struct_t *);
  _lit_struct->type              = type;
  _lit_struct->parent_compound   = parent_compound;
  _lit_struct->scope             = scope;
  return (Node *)_lit_struct;
}

_NODE_NCTOR(lit_enum, Node *type, Node *variants, Node *parent_compound, Scope *scope)
{
  node_lit_enum_t *_lit_enum = alloc_node(ast, NODE_LIT_ENUM, tok, node_lit_enum_t *);
  _lit_enum->type            = type;
  _lit_enum->parent_compound = parent_compound;
  _lit_enum->scope           = scope;
  _lit_enum->variants        = variants;
  return (Node *)_lit_enum;
}

_NODE_NCTOR(lit, Node *type, TokenValue value)
{
  node_lit_t *_lit = alloc_node(ast, NODE_LIT, tok, node_lit_t *);
  _lit->type       = type;
  _lit->value      = value;
  return (Node *)_lit;
}

_NODE_NCTOR(expr_binop, Node *lhs, Node *rhs, Node *type, Sym op)
{
  node_expr_binop_t *_expr_binop = alloc_node(ast, NODE_EXPR_BINOP, tok, node_expr_binop_t *);
  _expr_binop->lhs               = lhs;
  _expr_binop->rhs               = rhs;
  _expr_binop->type              = type;
  _expr_binop->op                = op;
  return (Node *)_expr_binop;
}

_NODE_NCTOR(expr_call, Node *ref, Node *args, int argsc, Node *type, bool run)
{
  node_expr_call_t *_expr_call = alloc_node(ast, NODE_EXPR_CALL, tok, node_expr_call_t *);
  _expr_call->ref              = ref;
  _expr_call->args             = args;
  _expr_call->argsc            = argsc;
  _expr_call->type             = type;
  _expr_call->run              = run;
  return (Node *)_expr_call;
}

_NODE_NCTOR(expr_member, MemberKind kind, Node *ident, Node *next, Node *type, bool ptr_ref)
{
  node_expr_member_t *_expr_member = alloc_node(ast, NODE_EXPR_MEMBER, tok, node_expr_member_t *);
  _expr_member->kind               = kind;
  _expr_member->ident              = ident;
  _expr_member->next               = next;
  _expr_member->type               = type;
  _expr_member->ptr_ref            = ptr_ref;
  return (Node *)_expr_member;
}

_NODE_NCTOR(expr_elem, Node *next, Node *type, Node *index)
{
  node_expr_elem_t *_expr_elem = alloc_node(ast, NODE_EXPR_ELEM, tok, node_expr_elem_t *);
  _expr_elem->next             = next;
  _expr_elem->type             = type;
  _expr_elem->index            = index;
  return (Node *)_expr_elem;
}

_NODE_NCTOR(expr_sizeof, Node *in, Node *type)
{
  node_expr_sizeof_t *_expr_sizeof = alloc_node(ast, NODE_EXPR_SIZEOF, tok, node_expr_sizeof_t *);
  _expr_sizeof->in                 = in;
  _expr_sizeof->type               = type;
  return (Node *)_expr_sizeof;
}

_NODE_NCTOR(expr_cast, Node *type, Node *next)
{
  node_expr_cast_t *_expr_cast = alloc_node(ast, NODE_EXPR_CAST, tok, node_expr_cast_t *);
  _expr_cast->type             = type;
  _expr_cast->next             = next;
  return (Node *)_expr_cast;
}

_NODE_NCTOR(expr_unary, Sym op, Node *next, Node *type)
{
  node_expr_unary_t *_expr_unary = alloc_node(ast, NODE_EXPR_UNARY, tok, node_expr_unary_t *);
  _expr_unary->next              = next;
  _expr_unary->type              = type;
  _expr_unary->op                = op;
  return (Node *)_expr_unary;
}

_NODE_NCTOR(expr_null, Node *type)
{
  node_expr_null_t *_expr_null = alloc_node(ast, NODE_EXPR_NULL, tok, node_expr_null_t *);
  _expr_null->type             = type;
  return (Node *)_expr_null;
}

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

void
visitor_init(Visitor *visitor)
{
  /* default value for all visitor callbacks */
  memset(visitor->visitors, 0, sizeof(VisitorFunc) * NODE_COUNT);
}

void
visitor_add(Visitor *visitor, VisitorFunc fn, NodeCode code)
{
  visitor->visitors[code] = fn;
}

void
visitor_visit(Visitor *visitor, Node *node, void *cnt)
{
  if (!node) return;
  if (visitor->visitors[node_code(node)])
    visitor->visitors[node_code(node)](visitor, node, cnt);
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
  switch (node_code(node)) {

  case NODE_UBLOCK: {
    node_foreach(peek_ublock(node)->nodes, tmp) visit(tmp);
    break;
  }

  case NODE_BLOCK: {
    node_foreach(peek_block(node)->nodes, tmp) visit(tmp);
    break;
  }

  case NODE_DECL: {
    node_decl_t *_decl = peek_decl(node);
    visit(_decl->name);
    visit(_decl->type);
    visit(_decl->value);
    break;
  }

  case NODE_TYPE_FUND: {
    node_type_fund_t *_fund = peek_type_fund(node);
    visit(_fund->arr);
    break;
  }

  case NODE_TYPE_FN: {
    node_type_fn_t *_fn = peek_type_fn(node);
    visit(_fn->arr);
    break;
  }

  case NODE_TYPE_STRUCT: {
    node_type_struct_t *_struct = peek_type_struct(node);
    visit(_struct->arr);
    break;
  }

  case NODE_TYPE_ENUM: {
    node_type_enum_t *_enum = peek_type_enum(node);
    visit(_enum->arr);
    break;
  }

  case NODE_EXPR_BINOP: {
    node_expr_binop_t *_binop = peek_expr_binop(node);
    visit(_binop->lhs);
    visit(_binop->rhs);
    break;
  }

  case NODE_EXPR_CALL: {
    node_expr_call_t *_call = peek_expr_call(node);
    node_foreach(_call->args, tmp) visit(tmp);
    break;
  }

  case NODE_EXPR_CAST: {
    visit(peek_expr_cast(node)->next);
    break;
  }

  case NODE_EXPR_UNARY: {
    visit(peek_expr_unary(node)->next);
    break;
  }

  case NODE_EXPR_MEMBER: {
    visit(peek_expr_member(node)->next);
    break;
  }
  case NODE_EXPR_ELEM: {
    visit(peek_expr_elem(node)->index);
    visit(peek_expr_elem(node)->next);
    break;
  }

  case NODE_STMT_RETURN: {
    visit(peek_stmt_return(node)->expr);
    break;
  }

  case NODE_STMT_IF: {
    visit(peek_stmt_if(node)->test);
    visit(peek_stmt_if(node)->true_stmt);
    visit(peek_stmt_if(node)->false_stmt);
    break;
  }

  case NODE_STMT_LOOP: {
    visit(peek_stmt_loop(node)->test);
    visit(peek_stmt_loop(node)->true_stmt);
    break;
  }

  case NODE_LIT_FN: {
    visit(peek_lit_fn(node)->block);
    break;
  }

  case NODE_LIT_ENUM: {
    visit(peek_lit_enum(node)->variants);
    break;
  }

    /* defaults (terminal cases) */
  case NODE_LIT_STRUCT:
  case NODE_IDENT:
  case NODE_LOAD:
  case NODE_LINK:
  case NODE_LIT:
  case NODE_EXPR_NULL:
  case NODE_EXPR_SIZEOF:
  case NODE_BAD:
  case NODE_STMT_BREAK:
  case NODE_STMT_CONTINUE:
  case NODE_COUNT:
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

  switch (node_code(type)) {
  case NODE_IDENT: {
    // identificator can lead to type
    _type_to_string(buf, len, peek_ident(type)->ref);
    break;
  }

  case NODE_DECL: {
    _type_to_string(buf, len, peek_decl(type)->type);
    break;
  }

  case NODE_TYPE_FUND: {
    node_type_fund_t *_type = peek_type_fund(type);
    for (int i = 0; i < _type->ptr; ++i) {
      append_buf(buf, len, "*");
    }
    append_buf(buf, len, ftype_strings[peek_type_fund(type)->code]);
    break;
  }

  case NODE_TYPE_FN: {
    node_type_fn_t *_fn = peek_type_fn(type);
    for (int i = 0; i < _fn->ptr; ++i) {
      append_buf(buf, len, "*");
    }

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
    node_type_struct_t *_struct = peek_type_struct(type);
    for (int i = 0; i < _struct->ptr; ++i) {
      append_buf(buf, len, "*");
    }

    if (_struct->base_decl) {
      Node *name = peek_decl(_struct->base_decl)->name;
      assert(name);

      append_buf(buf, len, peek_ident(name)->str);
      break;
    }

    append_buf(buf, len, "struct {");

    Node *t = _struct->types;
    while (t) {
      _type_to_string(buf, len, t);
      t = t->next;
      if (t) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, "}");
    break;
  }

  case NODE_TYPE_ENUM: {
    node_type_enum_t *_enum = peek_type_enum(type);
    for (int i = 0; i < _enum->ptr; ++i) {
      append_buf(buf, len, "*");
    }
    append_buf(buf, len, "enum ");
    _type_to_string(buf, len, _enum->base_type);
    break;
  }

  default:
    bl_abort("node is not valid type");
  }

  if (ast_type_get_arr(node_is(type, NODE_DECL) ? peek_decl(type)->type : type)) {
    append_buf(buf, len, " []");
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

Scope *
ast_get_scope(Node *node)
{
  assert(node);
  switch (node_code(node)) {

  case NODE_UBLOCK:
    return peek_ublock(node)->scope;
  case NODE_BLOCK:
    return peek_block(node)->scope;
  case NODE_LIT_FN:
    return peek_lit_fn(node)->scope;
  case NODE_LIT_STRUCT:
    return peek_lit_struct(node)->scope;
  case NODE_LIT_ENUM:
    return peek_lit_enum(node)->scope;

  default:
    bl_abort("node %s has no scope", node_name(node));
  }
}

Node *
ast_get_type(Node *node)
{
  if (!node) return NULL;
  switch (node_code(node)) {
  case NODE_DECL:
    return ast_get_type(peek_decl(node)->type);
  case NODE_LIT:
    return ast_get_type(peek_lit(node)->type);
  case NODE_LIT_FN:
    return peek_lit_fn(node)->type;
  case NODE_LIT_STRUCT:
    return peek_lit_struct(node)->type;
  case NODE_LIT_ENUM:
    return peek_lit_enum(node)->type;
  case NODE_IDENT:
    return ast_get_type(peek_ident(node)->ref);
  case NODE_EXPR_CALL:
    return ast_get_type(peek_expr_call(node)->type);
  case NODE_EXPR_BINOP:
    return ast_get_type(peek_expr_binop(node)->type);
  case NODE_EXPR_SIZEOF:
    return ast_get_type(peek_expr_sizeof(node)->type);
  case NODE_EXPR_CAST:
    return ast_get_type(peek_expr_cast(node)->type);
  case NODE_EXPR_UNARY:
    return ast_get_type(peek_expr_unary(node)->type);
  case NODE_EXPR_NULL:
    return ast_get_type(peek_expr_null(node)->type);
  case NODE_EXPR_MEMBER:
    return ast_get_type(peek_expr_member(node)->type);
  case NODE_EXPR_ELEM:
    return ast_get_type(peek_expr_elem(node)->type);
  case NODE_TYPE_FUND:
  case NODE_TYPE_STRUCT:
  case NODE_TYPE_FN:
  case NODE_TYPE_ENUM:
    return node;
  default:
    bl_abort("node %s has no type", node_name(node));
  }
}

void
ast_set_type(Node *node, Node *type)
{
  assert(node && type);
  switch (node_code(node)) {
  case NODE_DECL:
    peek_decl(node)->type = type;
    break;
  case NODE_LIT:
    peek_lit(node)->type = type;
    break;
  case NODE_LIT_FN:
    peek_lit_fn(node)->type = type;
    break;
  case NODE_LIT_STRUCT:
    peek_lit_struct(node)->type = type;
    break;
  case NODE_LIT_ENUM:
    peek_lit_enum(node)->type = type;
    break;
  case NODE_EXPR_CALL:
    peek_expr_call(node)->type = type;
    break;
  case NODE_EXPR_BINOP:
    peek_expr_binop(node)->type = type;
    break;
  case NODE_EXPR_SIZEOF:
    peek_expr_sizeof(node)->type = type;
    break;
  case NODE_EXPR_CAST:
    peek_expr_cast(node)->type = type;
    break;
  case NODE_EXPR_UNARY:
    peek_expr_unary(node)->type = type;
    break;
  case NODE_EXPR_NULL:
    peek_expr_null(node)->type = type;
    break;
  case NODE_EXPR_MEMBER:
    peek_expr_member(node)->type = type;
    break;
  case NODE_EXPR_ELEM:
    peek_expr_elem(node)->type = type;
    break;
  default:
    bl_abort("node %s has no type", node_name(node));
  }
}

int
ast_is_buildin_type(Node *ident)
{
  assert(ident);
  node_ident_t *_ident = peek_ident(ident);

  uint64_t hash;
  array_foreach(ftype_hashes, hash)
  {
    if (_ident->hash == hash) return i;
  }

  return -1;
}

int
ast_is_buildin(Node *ident)
{
  assert(ident);
  node_ident_t *_ident = peek_ident(ident);

  uint64_t hash;
  array_foreach(buildin_hashes, hash)
  {
    if (_ident->hash == hash) return i;
  }

  return -1;
}

bool
ast_type_cmp(Node *first, Node *second)
{
  first  = ast_get_type(first);
  second = ast_get_type(second);
  assert(first);
  assert(second);

  if (node_code(first) != node_code(second)) return false;
  if (ast_get_type_kind(first) != ast_get_type_kind(second)) return false;

  // same nodes
  switch (node_code(first)) {

  case NODE_TYPE_FUND: {
    if (peek_type_fund(first)->code != peek_type_fund(second)->code) return false;
    break;
  }

  case NODE_TYPE_ENUM: {
    node_type_enum_t *_first  = peek_type_enum(first);
    node_type_enum_t *_second = peek_type_enum(second);
    if (peek_type_fund(_first->base_type)->code != peek_type_fund(_second->base_type)->code)
      return false;
    break;
  }

  case NODE_TYPE_FN: {
    node_type_fn_t *_first  = peek_type_fn(first);
    node_type_fn_t *_second = peek_type_fn(second);

    if (_first->argc_types != _second->argc_types) return false;
    if (!ast_type_cmp(_first->ret_type, _second->ret_type)) return false;

    Node *argt1 = _first->arg_types;
    Node *argt2 = _second->arg_types;
    while (argt1 && argt2) {
      if (!ast_type_cmp(argt1, argt2)) return false;

      argt1 = argt1->next;
      argt2 = argt2->next;
    }

    break;
  }

  case NODE_TYPE_STRUCT: {
    node_type_struct_t *_first  = peek_type_struct(first);
    node_type_struct_t *_second = peek_type_struct(second);

    if (_first->typesc != _second->typesc) return false;

    Node *type1 = _first->types;
    Node *type2 = _second->types;
    while (type1 && type2) {
      if (!ast_type_cmp(type1, type2)) return false;

      type1 = type1->next;
      type2 = type2->next;
    }
    break;
  }

  default:
    bl_abort("missing comparation of %s type", node_name(first));
  }

  return true;
}

TypeKind
ast_get_type_kind(Node *type)
{
  assert(type);
  switch (node_code(type)) {
  case NODE_TYPE_FUND: {
    node_type_fund_t *_ftype = peek_type_fund(type);

    if (_ftype->ptr) return TYPE_KIND_PTR;

    switch (_ftype->code) {
    case FTYPE_TYPE:
      return TYPE_KIND_TYPE;
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

  case NODE_TYPE_FN: {
    node_type_fn_t *_fn_type = peek_type_fn(type);
    if (_fn_type->ptr) return TYPE_KIND_PTR;
    return TYPE_KIND_FN;
  }

  case NODE_TYPE_STRUCT: {
    node_type_struct_t *_struct_type = peek_type_struct(type);
    if (_struct_type->ptr) return TYPE_KIND_PTR;
    return TYPE_KIND_STRUCT;
  }

  case NODE_TYPE_ENUM:
    return TYPE_KIND_ENUM;

  default:
    bl_abort("node %s is not a type", node_name(type));
  }

  return TYPE_KIND_UNKNOWN;
}

Node *
ast_get_parent_compound(Node *node)
{
  assert(node);
  switch (node_code(node)) {
  case NODE_IDENT:
    return peek_ident(node)->parent_compound;
  case NODE_UBLOCK:
    return NULL;
  case NODE_BLOCK:
    return peek_block(node)->parent_compound;
  case NODE_LIT_FN:
    return peek_lit_fn(node)->parent_compound;
  case NODE_LIT_STRUCT:
    return peek_lit_struct(node)->parent_compound;
  case NODE_LIT_ENUM:
    return peek_lit_enum(node)->parent_compound;
  default:
    bl_abort("node %s has no parent compound", node_name(node));
  }
}

bool
ast_can_impl_cast(Node *from_type, Node *to_type)
{
  assert(from_type);
  assert(to_type);

  from_type = ast_get_type(from_type);
  to_type   = ast_get_type(to_type);

  TypeKind fkind = ast_get_type_kind(from_type);
  TypeKind tkind = ast_get_type_kind(to_type);

  if (fkind == TYPE_KIND_STRING && tkind == TYPE_KIND_PTR) return true;
  if (tkind == TYPE_KIND_STRING && fkind == TYPE_KIND_PTR) return true;

  if ((fkind == TYPE_KIND_SINT || fkind == TYPE_KIND_UINT || fkind == TYPE_KIND_SIZE) &&
      (tkind == TYPE_KIND_SINT || tkind == TYPE_KIND_UINT || tkind == TYPE_KIND_SIZE))
    return true;

  if (tkind == TYPE_KIND_ENUM) {
    return ast_can_impl_cast(from_type, peek_type_enum(to_type)->base_type);
  }

  if (fkind != tkind) return false;
  if (fkind == TYPE_KIND_STRUCT || fkind == TYPE_KIND_FN) return false;
  if (fkind == TYPE_KIND_PTR && node_is(from_type, NODE_TYPE_FN)) return false;

  return true;
}

Node *
ast_node_dup(Ast *ast, Node *node)
{
  Node *tmp = alloc_node(ast, -1, NULL, Node *);
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

int
ast_type_get_ptr(Node *type)
{
  switch (node_code(type)) {
  case NODE_TYPE_FUND:
    return peek_type_fund(type)->ptr;
  case NODE_TYPE_FN:
    return peek_type_fn(type)->ptr;
  case NODE_TYPE_STRUCT:
    return peek_type_struct(type)->ptr;
  case NODE_TYPE_ENUM:
    return peek_type_enum(type)->ptr;
  default:
    bl_abort("invalid type %s", node_name(type));
  }
}

void
ast_type_set_ptr(Node *type, int ptr)
{
  switch (node_code(type)) {
  case NODE_TYPE_FUND:
    peek_type_fund(type)->ptr = ptr;
    break;
  case NODE_TYPE_FN:
    peek_type_fn(type)->ptr = ptr;
    break;
  case NODE_TYPE_STRUCT:
    peek_type_struct(type)->ptr = ptr;
    break;
  case NODE_TYPE_ENUM:
    peek_type_enum(type)->ptr = ptr;
    break;
  default:
    bl_abort("invalid type %s", node_name(type));
  }
}

Node *
ast_type_get_arr(Node *type)
{
  switch (node_code(type)) {
  case NODE_IDENT:
    return peek_ident(type)->arr;
  case NODE_TYPE_FUND:
    return peek_type_fund(type)->arr;
  case NODE_TYPE_FN:
    return peek_type_fn(type)->arr;
  case NODE_TYPE_STRUCT:
    return peek_type_struct(type)->arr;
  case NODE_TYPE_ENUM:
    return peek_type_enum(type)->arr;
  default:
    bl_abort("invalid type %s", node_name(type));
  }
}

void
ast_type_set_arr(Node *type, Node *arr)
{
  switch (node_code(type)) {
  case NODE_IDENT:
    peek_ident(type)->arr = arr;
    break;
  case NODE_TYPE_FUND:
    peek_type_fund(type)->arr = arr;
    break;
  case NODE_TYPE_FN:
    peek_type_fn(type)->arr = arr;
    break;
  case NODE_TYPE_STRUCT:
    peek_type_struct(type)->arr = arr;
    break;
  case NODE_TYPE_ENUM:
    peek_type_enum(type)->arr = arr;
    break;
  default:
    bl_abort("invalid type %s", node_name(type));
  }
}

bool
ast_is_type(Node *node)
{
  switch (node_code(node)) {
  case NODE_TYPE_FUND:
  case NODE_TYPE_FN:
  case NODE_TYPE_STRUCT:
  case NODE_TYPE_ENUM:
    return true;
  default:
    return false;
  }
}

Node *
ast_unroll_ident(Node *ident)
{
  assert(ident);
  if (node_is(ident, NODE_IDENT)) {
    node_ident_t *_ident = peek_ident(ident);
    assert(_ident->ref);
    return ast_unroll_ident(_ident->ref);
  }

  return ident;
}

Dependency *
ast_add_dep_uq(Node *decl, Node *dep, int type)
{
  assert(dep && "invalid dep");
  BHashTable **deps = &peek_decl(decl)->deps;
  Dependency   tmp  = {.node = dep, .type = type};

  if (!*deps) {
    *deps = bo_htbl_new(sizeof(Dependency), 64);
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  } else if (!bo_htbl_has_key(*deps, (uint64_t)dep)) {
    bo_htbl_insert(*deps, (uint64_t)dep, tmp);
  }

  return &bo_htbl_at(*deps, (uint64_t)dep, Dependency);
}
