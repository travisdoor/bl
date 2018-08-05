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

#include "ast_impl.h"

#define CHUNK_SIZE 256

typedef struct chunk
{
  struct chunk *next;
  int           count;
} chunk_t;

bl_node_t bl_ftypes[] = {
#define ft(name, str)                                                                              \
  (bl_node_t){.code             = BL_NODE_TYPE_FUND,                                               \
              .src              = NULL,                                                            \
              .next             = NULL,                                                            \
              .n.type_fund.code = BL_FTYPE_##name,                                                 \
              .n.type_fund.ptr  = 0},

    _BL_FTYPE_LIST
#undef ft
};

const char *bl_ftype_strings[] = {
#define ft(code, name) #name,
    _BL_FTYPE_LIST
#undef ft
};

const char *bl_buildin_strings[] = {
#define bt(code, name) #name,
    _BL_BUILDINS_LIST
#undef bt
};

const char *bl_node_type_strings[] = {
#define nt(code, name, data) #name,
    _BL_NODE_TYPE_LIST
#undef nt
};

uint64_t bl_ftype_hashes[BL_FTYPE_COUNT];
uint64_t bl_buildin_hashes[BL_BUILDIN_COUNT];

/*static void
node_terminate(bl_node_t *node)
{
  switch (node->code) {
  case BL_NODE_LIT_FN:
    bo_unref(bl_peek_lit_fn(node)->deps);
    break;
  default:
    break;
  }
}*/

static inline bl_node_t *
get_node_in_chunk(chunk_t *chunk, int i)
{
  return (bl_node_t *)((char *)chunk + (i * sizeof(bl_node_t)));
}

static inline chunk_t *
alloc_chunk(void)
{
  const size_t size_in_bytes = sizeof(bl_node_t) * CHUNK_SIZE;
  chunk_t *    chunk         = bl_malloc(size_in_bytes);
  memset(chunk, 0, size_in_bytes);
  chunk->count = 1;
  return chunk;
}

static inline chunk_t *
free_chunk(chunk_t *chunk)
{
  if (!chunk) return NULL;

  chunk_t *next = chunk->next;
  /*for (int i = 0; i < chunk->count - 1; ++i) {
    node_terminate(get_node_in_chunk(chunk, i + 1));
    }*/
  bl_free(chunk);
  return next;
}

#define alloc_node(ast, c, tok, t) (t) _alloc_node((ast), (c), (tok));

static bl_node_t *
_alloc_node(bl_ast_t *ast, bl_node_code_e c, bl_token_t *tok)
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

  bl_node_t *node = get_node_in_chunk(ast->current_chunk, ast->current_chunk->count);
  ast->current_chunk->count++;

  node->code = c;
  node->src  = tok ? &tok->src : NULL;

#if BL_DEBUG
  static int serial = 0;
  node->_serial     = serial++;
#endif

  return node;
}

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  static bool statics_initialized = false;
  ast->first_chunk                = NULL;
  ast->current_chunk              = NULL;

  /* init ftype hashes */
  if (!statics_initialized) {
    statics_initialized = true;
    const char *it;
    bl_array_foreach(bl_ftype_strings, it)
    {
      bl_ftype_hashes[i] = bo_hash_from_str(it);
    }

    bl_array_foreach(bl_buildin_strings, it)
    {
      bl_buildin_hashes[i] = bo_hash_from_str(it);
    }
  }
}

void
bl_ast_terminate(bl_ast_t *ast)
{
  chunk_t *chunk = ast->first_chunk;
  while (chunk) {
    chunk = free_chunk(chunk);
  }
}

/*************************************************************************************************
 * node constructors
 *************************************************************************************************/

_BL_AST_NCTOR(bad)
{
  return alloc_node(ast, BL_NODE_BAD, tok, bl_node_t *);
}

_BL_AST_NCTOR(load, const char *filepath)
{
  bl_node_load_t *_load = alloc_node(ast, BL_NODE_LOAD, tok, bl_node_load_t *);
  _load->filepath       = filepath;
  return (bl_node_t *)_load;
}

_BL_AST_NCTOR(link, const char *lib)
{
  bl_node_link_t *_link = alloc_node(ast, BL_NODE_LINK, tok, bl_node_link_t *);
  _link->lib            = lib;
  return (bl_node_t *)_link;
}

_BL_AST_NCTOR(decl_ublock, struct bl_unit *unit, bl_scope_t *scope)
{
  bl_node_decl_ublock_t *_ublock =
      alloc_node(ast, BL_NODE_DECL_UBLOCK, tok, bl_node_decl_ublock_t *);
  _ublock->scope = scope;
  _ublock->unit  = unit;
  return (bl_node_t *)_ublock;
}

_BL_AST_NCTOR(ident, bl_node_t *ref, bl_node_t *parent_compound, int ptr)
{
  bl_node_ident_t *_ident = alloc_node(ast, BL_NODE_IDENT, tok, bl_node_ident_t *);
  _ident->hash            = bo_hash_from_str(tok->value.str);
  _ident->str             = tok->value.str;
  _ident->ref             = ref;
  _ident->ptr             = ptr;
  _ident->parent_compound = parent_compound;
  return (bl_node_t *)_ident;
}

_BL_AST_NCTOR(stmt_return, bl_node_t *expr, bl_node_t *fn)
{
  bl_node_stmt_return_t *_ret = alloc_node(ast, BL_NODE_STMT_RETURN, tok, bl_node_stmt_return_t *);
  _ret->expr                  = expr;
  _ret->fn                    = fn;
  return (bl_node_t *)_ret;
}

_BL_AST_NCTOR(stmt_break)
{
  return alloc_node(ast, BL_NODE_STMT_BREAK, tok, bl_node_t *);
}

_BL_AST_NCTOR(stmt_continue)
{
  return alloc_node(ast, BL_NODE_STMT_CONTINUE, tok, bl_node_t *);
}

_BL_AST_NCTOR(stmt_if, bl_node_t *test, bl_node_t *true_stmt, bl_node_t *false_stmt)
{
  bl_node_stmt_if_t *_if = alloc_node(ast, BL_NODE_STMT_IF, tok, bl_node_stmt_if_t *);
  _if->test              = test;
  _if->true_stmt         = true_stmt;
  _if->false_stmt        = false_stmt;
  return (bl_node_t *)_if;
}

_BL_AST_NCTOR(stmt_loop, bl_node_t *test, bl_node_t *true_stmt)
{
  bl_node_stmt_loop_t *_loop = alloc_node(ast, BL_NODE_STMT_LOOP, tok, bl_node_stmt_loop_t *);
  _loop->test                = test;
  _loop->true_stmt           = true_stmt;
  return (bl_node_t *)_loop;
}

_BL_AST_NCTOR(decl_block, bl_node_t *nodes, bl_node_t *parent_compound, bl_scope_t *scope)
{
  bl_node_decl_block_t *_block = alloc_node(ast, BL_NODE_DECL_BLOCK, tok, bl_node_decl_block_t *);
  _block->nodes                = nodes;
  _block->parent_compound      = parent_compound;
  _block->scope                = scope;
  return (bl_node_t *)_block;
}

_BL_AST_NCTOR(decl_value, bl_decl_kind_e kind, bl_node_t *name, bl_node_t *type, bl_node_t *value,
              bool mutable, int flags, int order, bool in_gscope)
{
  bl_node_decl_value_t *_decl = alloc_node(ast, BL_NODE_DECL_VALUE, tok, bl_node_decl_value_t *);
  _decl->kind                 = kind;
  _decl->type                 = type;
  _decl->name                 = name;
  _decl->value                = value;
  _decl->mutable              = mutable;
  _decl->flags                = flags;
  _decl->order                = order;
  _decl->in_gscope            = in_gscope;
  return (bl_node_t *)_decl;
}

_BL_AST_NCTOR(type_fund, bl_ftype_e code, int ptr)
{
  bl_node_type_fund_t *_type_fund = alloc_node(ast, BL_NODE_TYPE_FUND, tok, bl_node_type_fund_t *);
  _type_fund->code                = code;
  _type_fund->ptr                 = ptr;
  return (bl_node_t *)_type_fund;
}

_BL_AST_NCTOR(type_fn, bl_node_t *arg_types, int argc_types, bl_node_t *ret_type, int ptr)
{
  bl_node_type_fn_t *_type_fn = alloc_node(ast, BL_NODE_TYPE_FN, tok, bl_node_type_fn_t *);
  _type_fn->arg_types         = arg_types;
  _type_fn->argc_types        = argc_types;
  _type_fn->ret_type          = ret_type;
  _type_fn->ptr               = ptr;
  return (bl_node_t *)_type_fn;
}

_BL_AST_NCTOR(type_struct, bl_node_t *types, int typesc, bl_node_t *base_decl, int ptr)
{
  bl_node_type_struct_t *_type_struct =
      alloc_node(ast, BL_NODE_TYPE_STRUCT, tok, bl_node_type_struct_t *);
  _type_struct->types     = types;
  _type_struct->typesc    = typesc;
  _type_struct->base_decl = base_decl;
  _type_struct->ptr       = ptr;
  return (bl_node_t *)_type_struct;
}

_BL_AST_NCTOR(type_enum, bl_node_t *type, bl_node_t *base_decl, int ptr)
{
  bl_node_type_enum_t *_type_enum = alloc_node(ast, BL_NODE_TYPE_ENUM, tok, bl_node_type_enum_t *);
  _type_enum->base_decl           = base_decl;
  _type_enum->base_type           = type;
  _type_enum->ptr                 = ptr;
  return (bl_node_t *)_type_enum;
}

_BL_AST_NCTOR(lit_fn, bl_node_t *type, bl_node_t *block, bl_node_t *parent_compound,
              bl_scope_t *scope)
{
  bl_node_lit_fn_t *_lit_fn = alloc_node(ast, BL_NODE_LIT_FN, tok, bl_node_lit_fn_t *);
  _lit_fn->type             = type;
  _lit_fn->block            = block;
  _lit_fn->parent_compound  = parent_compound;
  _lit_fn->scope            = scope;
  return (bl_node_t *)_lit_fn;
}

_BL_AST_NCTOR(lit_struct, bl_node_t *type, bl_node_t *parent_compound, bl_scope_t *scope)
{
  bl_node_lit_struct_t *_lit_struct =
      alloc_node(ast, BL_NODE_LIT_STRUCT, tok, bl_node_lit_struct_t *);
  _lit_struct->type            = type;
  _lit_struct->parent_compound = parent_compound;
  _lit_struct->scope           = scope;
  return (bl_node_t *)_lit_struct;
}

_BL_AST_NCTOR(lit_enum, bl_node_t *type, bl_node_t *variants, bl_node_t *parent_compound,
              bl_scope_t *scope)
{
  bl_node_lit_enum_t *_lit_enum = alloc_node(ast, BL_NODE_LIT_ENUM, tok, bl_node_lit_enum_t *);
  _lit_enum->type               = type;
  _lit_enum->parent_compound    = parent_compound;
  _lit_enum->scope              = scope;
  _lit_enum->variants           = variants;
  return (bl_node_t *)_lit_enum;
}

_BL_AST_NCTOR(lit, bl_node_t *type, bl_token_value_u value)
{
  bl_node_lit_t *_lit = alloc_node(ast, BL_NODE_LIT, tok, bl_node_lit_t *);
  _lit->type          = type;
  _lit->value         = value;
  return (bl_node_t *)_lit;
}

_BL_AST_NCTOR(expr_binop, bl_node_t *lhs, bl_node_t *rhs, bl_node_t *type, bl_sym_e op)
{
  bl_node_expr_binop_t *_expr_binop =
      alloc_node(ast, BL_NODE_EXPR_BINOP, tok, bl_node_expr_binop_t *);
  _expr_binop->lhs  = lhs;
  _expr_binop->rhs  = rhs;
  _expr_binop->type = type;
  _expr_binop->op   = op;
  return (bl_node_t *)_expr_binop;
}

_BL_AST_NCTOR(expr_call, bl_node_t *ident, bl_node_t *args, int argsc, bl_node_t *type)
{
  bl_node_expr_call_t *_expr_call = alloc_node(ast, BL_NODE_EXPR_CALL, tok, bl_node_expr_call_t *);
  _expr_call->ident               = ident;
  _expr_call->args                = args;
  _expr_call->argsc               = argsc;
  _expr_call->type                = type;
  return (bl_node_t *)_expr_call;
}

_BL_AST_NCTOR(expr_member, bl_member_kind_e kind, bl_node_t *ident, bl_node_t *next,
              bl_node_t *type, bool ptr_ref)
{
  bl_node_expr_member_t *_expr_member =
      alloc_node(ast, BL_NODE_EXPR_MEMBER, tok, bl_node_expr_member_t *);
  _expr_member->kind    = kind;
  _expr_member->ident   = ident;
  _expr_member->next    = next;
  _expr_member->type    = type;
  _expr_member->ptr_ref = ptr_ref;
  return (bl_node_t *)_expr_member;
}

_BL_AST_NCTOR(expr_sizeof, bl_node_t *in, bl_node_t *type)
{
  bl_node_expr_sizeof_t *_expr_sizeof =
      alloc_node(ast, BL_NODE_EXPR_SIZEOF, tok, bl_node_expr_sizeof_t *);
  _expr_sizeof->in   = in;
  _expr_sizeof->type = type;
  return (bl_node_t *)_expr_sizeof;
}

_BL_AST_NCTOR(expr_cast, bl_node_t *type, bl_node_t *next)
{
  bl_node_expr_cast_t *_expr_cast = alloc_node(ast, BL_NODE_EXPR_CAST, tok, bl_node_expr_cast_t *);
  _expr_cast->type                = type;
  _expr_cast->next                = next;
  return (bl_node_t *)_expr_cast;
}

_BL_AST_NCTOR(expr_unary, bl_sym_e op, bl_node_t *next, bl_node_t *type)
{
  bl_node_expr_unary_t *_expr_unary =
      alloc_node(ast, BL_NODE_EXPR_UNARY, tok, bl_node_expr_unary_t *);
  _expr_unary->next = next;
  _expr_unary->type = type;
  _expr_unary->op   = op;
  return (bl_node_t *)_expr_unary;
}

_BL_AST_NCTOR(expr_null, bl_node_t *type)
{
  bl_node_expr_null_t *_expr_null = alloc_node(ast, BL_NODE_EXPR_NULL, tok, bl_node_expr_null_t *);
  _expr_null->type                = type;
  return (bl_node_t *)_expr_null;
}

/*************************************************************************************************
 * other
 *************************************************************************************************/

void
bl_ast_visit_every_node(bl_ast_t *ast, bl_visit_f visit, void *cnt)
{
  assert(visit);
  chunk_t *chunk = ast->first_chunk;
  while (chunk) {
    for (int i = 0; i < chunk->count - 1; ++i) {
      visit(cnt, get_node_in_chunk(chunk, i + 1));
    }
    chunk = chunk->next;
  }
}

static void
_type_to_string(char *buf, size_t len, bl_node_t *type)
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

  switch (bl_node_code(type)) {
  case BL_NODE_IDENT: {
    // identificator can lead to type
    _type_to_string(buf, len, bl_peek_ident(type)->ref);
    break;
  }

  case BL_NODE_DECL_VALUE: {
    _type_to_string(buf, len, bl_peek_decl_value(type)->type);
    break;
  }

  case BL_NODE_TYPE_FUND: {
    bl_node_type_fund_t *_type = bl_peek_type_fund(type);
    for (int i = 0; i < _type->ptr; ++i) {
      append_buf(buf, len, "*");
    }
    append_buf(buf, len, bl_ftype_strings[bl_peek_type_fund(type)->code]);
    break;
  }

  case BL_NODE_TYPE_FN: {
    bl_node_type_fn_t *_fn = bl_peek_type_fn(type);
    for (int i = 0; i < _fn->ptr; ++i) {
      append_buf(buf, len, "*");
    }

    append_buf(buf, len, "fn (");
    bl_node_t *arg = _fn->arg_types;
    while (arg) {
      _type_to_string(buf, len, arg);
      arg = arg->next;
      if (arg) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, ") ");
    _type_to_string(buf, len, _fn->ret_type);
    break;
  }

  case BL_NODE_TYPE_STRUCT: {
    bl_node_type_struct_t *_struct = bl_peek_type_struct(type);
    for (int i = 0; i < _struct->ptr; ++i) {
      append_buf(buf, len, "*");
    }

    if (_struct->base_decl) {
      bl_node_t *name = bl_peek_decl_value(_struct->base_decl)->name;
      assert(name);

      append_buf(buf, len, bl_peek_ident(name)->str);
      break;
    }

    append_buf(buf, len, "struct {");

    bl_node_t *t = _struct->types;
    while (t) {
      _type_to_string(buf, len, t);
      t = t->next;
      if (t) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, "}");
    break;
  }

  case BL_NODE_TYPE_ENUM: {
    bl_node_type_enum_t *_enum = bl_peek_type_enum(type);
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

#undef append_buf
}

void
bl_ast_type_to_string(char *buf, size_t len, bl_node_t *type)
{
  if (!buf || !len) return;
  buf[0] = '\0';
  _type_to_string(buf, len, type);
}

bl_scope_t *
bl_ast_get_scope(bl_node_t *node)
{
  assert(node);
  switch (bl_node_code(node)) {

  case BL_NODE_DECL_UBLOCK:
    return bl_peek_decl_ublock(node)->scope;
  case BL_NODE_DECL_BLOCK:
    return bl_peek_decl_block(node)->scope;
  case BL_NODE_LIT_FN:
    return bl_peek_lit_fn(node)->scope;
  case BL_NODE_LIT_STRUCT:
    return bl_peek_lit_struct(node)->scope;
  case BL_NODE_LIT_ENUM:
    return bl_peek_lit_enum(node)->scope;

  default:
    bl_abort("node %s has no scope", bl_node_name(node));
  }
}

bl_node_t *
bl_ast_get_type(bl_node_t *node)
{
  if (!node) return NULL;
  switch (bl_node_code(node)) {
  case BL_NODE_DECL_VALUE:
    return bl_ast_get_type(bl_peek_decl_value(node)->type);
  case BL_NODE_LIT:
    return bl_ast_get_type(bl_peek_lit(node)->type);
  case BL_NODE_LIT_FN:
    return bl_peek_lit_fn(node)->type;
  case BL_NODE_LIT_STRUCT:
    return bl_peek_lit_struct(node)->type;
  case BL_NODE_LIT_ENUM:
    return bl_peek_lit_enum(node)->type;
  case BL_NODE_IDENT:
    return bl_ast_get_type(bl_peek_ident(node)->ref);
  case BL_NODE_EXPR_CALL:
    return bl_ast_get_type(bl_peek_expr_call(node)->type);
  case BL_NODE_EXPR_BINOP:
    return bl_ast_get_type(bl_peek_expr_binop(node)->type);
  case BL_NODE_EXPR_SIZEOF:
    return bl_ast_get_type(bl_peek_expr_sizeof(node)->type);
  case BL_NODE_EXPR_CAST:
    return bl_ast_get_type(bl_peek_expr_cast(node)->type);
  case BL_NODE_EXPR_UNARY:
    return bl_ast_get_type(bl_peek_expr_unary(node)->type);
  case BL_NODE_EXPR_NULL:
    return bl_ast_get_type(bl_peek_expr_null(node)->type);
  case BL_NODE_EXPR_MEMBER:
    return bl_ast_get_type(bl_peek_expr_member(node)->type);
  case BL_NODE_TYPE_FUND:
  case BL_NODE_TYPE_STRUCT:
  case BL_NODE_TYPE_FN:
  case BL_NODE_TYPE_ENUM:
    return node;
  default:
    bl_abort("node %s has no type", bl_node_name(node));
  }
}

void
bl_ast_set_type(bl_node_t *node, bl_node_t *type)
{
  assert(node && type);
  switch (bl_node_code(node)) {
  case BL_NODE_DECL_VALUE:
    bl_peek_decl_value(node)->type = type;
    break;
  case BL_NODE_LIT:
    bl_peek_lit(node)->type = type;
    break;
  case BL_NODE_LIT_FN:
    bl_peek_lit_fn(node)->type = type;
    break;
  case BL_NODE_LIT_STRUCT:
    bl_peek_lit_struct(node)->type = type;
    break;
  case BL_NODE_LIT_ENUM:
    bl_peek_lit_enum(node)->type = type;
    break;
  case BL_NODE_EXPR_CALL:
    bl_peek_expr_call(node)->type = type;
    break;
  case BL_NODE_EXPR_BINOP:
    bl_peek_expr_binop(node)->type = type;
    break;
  case BL_NODE_EXPR_SIZEOF:
    bl_peek_expr_sizeof(node)->type = type;
    break;
  case BL_NODE_EXPR_CAST:
    bl_peek_expr_cast(node)->type = type;
    break;
  case BL_NODE_EXPR_UNARY:
    bl_peek_expr_unary(node)->type = type;
    break;
  case BL_NODE_EXPR_NULL:
    bl_peek_expr_null(node)->type = type;
    break;
  case BL_NODE_EXPR_MEMBER:
    bl_peek_expr_member(node)->type = type;
    break;
  default:
    bl_abort("node %s has no type", bl_node_name(node));
  }
}

int
bl_ast_is_buildin_type(bl_node_t *ident)
{
  assert(ident);
  bl_node_ident_t *_ident = bl_peek_ident(ident);

  uint64_t hash;
  bl_array_foreach(bl_ftype_hashes, hash)
  {
    if (_ident->hash == hash) return i;
  }

  return -1;
}

int
bl_ast_is_buildin(bl_node_t *ident)
{
  assert(ident);
  bl_node_ident_t *_ident = bl_peek_ident(ident);

  uint64_t hash;
  bl_array_foreach(bl_buildin_hashes, hash)
  {
    if (_ident->hash == hash) return i;
  }

  return -1;
}

bool
bl_ast_type_cmp(bl_node_t *first, bl_node_t *second)
{
  first  = bl_ast_get_type(first);
  second = bl_ast_get_type(second);
  assert(first);
  assert(second);

  if (bl_node_code(first) != bl_node_code(second)) return false;
  if (bl_ast_get_type_kind(first) != bl_ast_get_type_kind(second)) return false;

  // same nodes
  switch (bl_node_code(first)) {

  case BL_NODE_TYPE_FUND: {
    if (bl_peek_type_fund(first)->code != bl_peek_type_fund(second)->code) return false;
    break;
  }

  case BL_NODE_TYPE_ENUM: {
    bl_node_type_enum_t *_first  = bl_peek_type_enum(first);
    bl_node_type_enum_t *_second = bl_peek_type_enum(second);
    if (bl_peek_type_fund(_first->base_type)->code != bl_peek_type_fund(_second->base_type)->code)
      return false;
    break;
  }

  case BL_NODE_TYPE_FN: {
    bl_node_type_fn_t *_first  = bl_peek_type_fn(first);
    bl_node_type_fn_t *_second = bl_peek_type_fn(second);

    if (_first->argc_types != _second->argc_types) return false;
    if (!bl_ast_type_cmp(_first->ret_type, _second->ret_type)) return false;

    bl_node_t *argt1 = _first->arg_types;
    bl_node_t *argt2 = _second->arg_types;
    while (argt1 && argt2) {
      if (!bl_ast_type_cmp(argt1, argt2)) return false;

      argt1 = argt1->next;
      argt2 = argt2->next;
    }

    break;
  }

  case BL_NODE_TYPE_STRUCT: {
    bl_node_type_struct_t *_first  = bl_peek_type_struct(first);
    bl_node_type_struct_t *_second = bl_peek_type_struct(second);

    if (_first->typesc != _second->typesc) return false;

    bl_node_t *type1 = _first->types;
    bl_node_t *type2 = _second->types;
    while (type1 && type2) {
      if (!bl_ast_type_cmp(type1, type2)) return false;

      type1 = type1->next;
      type2 = type2->next;
    }
    break;
  }

  default:
    bl_abort("missing comparation of %s type", bl_node_name(first));
  }

  return true;
}

bl_type_kind_e
bl_ast_get_type_kind(bl_node_t *type)
{
  assert(type);
  switch (bl_node_code(type)) {
  case BL_NODE_TYPE_FUND: {
    bl_node_type_fund_t *_ftype = bl_peek_type_fund(type);

    if (_ftype->ptr) return BL_KIND_PTR;

    switch (_ftype->code) {
    case BL_FTYPE_TYPE:
      return BL_KIND_TYPE;
    case BL_FTYPE_VOID:
      return BL_KIND_VOID;
    case BL_FTYPE_S8:
    case BL_FTYPE_S16:
    case BL_FTYPE_S32:
    case BL_FTYPE_S64:
      return BL_KIND_SINT;
    case BL_FTYPE_U8:
    case BL_FTYPE_U16:
    case BL_FTYPE_U32:
    case BL_FTYPE_U64:
      return BL_KIND_UINT;
    case BL_FTYPE_SIZE:
      return BL_KIND_SIZE;
    case BL_FTYPE_F32:
    case BL_FTYPE_F64:
      return BL_KIND_REAL;
    case BL_FTYPE_CHAR:
      return BL_KIND_CHAR;
    case BL_FTYPE_STRING:
      return BL_KIND_STRING;
    case BL_FTYPE_BOOL:
      return BL_KIND_BOOL;
    case BL_FTYPE_COUNT:
      break;
    }
    break;
  }

  case BL_NODE_TYPE_FN: {
    bl_node_type_fn_t *_fn_type = bl_peek_type_fn(type);
    if (_fn_type->ptr) return BL_KIND_PTR;
    return BL_KIND_FN;
  }

  case BL_NODE_TYPE_STRUCT: {
    bl_node_type_struct_t *_struct_type = bl_peek_type_struct(type);
    if (_struct_type->ptr) return BL_KIND_PTR;
    return BL_KIND_STRUCT;
  }

  case BL_NODE_TYPE_ENUM:
    return BL_KIND_ENUM;

  default:
    bl_abort("node %s is not a type", bl_node_name(type));
  }

  return BL_KIND_UNKNOWN;
}

bl_node_t *
bl_ast_get_parent_compound(bl_node_t *node)
{
  assert(node);
  switch (bl_node_code(node)) {
  case BL_NODE_IDENT:
    return bl_peek_ident(node)->parent_compound;
  case BL_NODE_DECL_UBLOCK:
    return NULL;
  case BL_NODE_DECL_BLOCK:
    return bl_peek_decl_block(node)->parent_compound;
  case BL_NODE_LIT_FN:
    return bl_peek_lit_fn(node)->parent_compound;
  case BL_NODE_LIT_STRUCT:
    return bl_peek_lit_struct(node)->parent_compound;
  case BL_NODE_LIT_ENUM:
    return bl_peek_lit_enum(node)->parent_compound;
  default:
    bl_abort("node %s has no parent compound", bl_node_name(node));
  }
}

bool
bl_ast_can_impl_cast(bl_node_t *from_type, bl_node_t *to_type)
{
  assert(from_type);
  assert(to_type);

  from_type = bl_ast_get_type(from_type);
  to_type   = bl_ast_get_type(to_type);

  bl_type_kind_e fkind = bl_ast_get_type_kind(from_type);
  bl_type_kind_e tkind = bl_ast_get_type_kind(to_type);

  if (fkind == BL_KIND_STRING && tkind == BL_KIND_PTR) return true;
  if (tkind == BL_KIND_STRING && fkind == BL_KIND_PTR) return true;

  if (tkind == BL_KIND_ENUM) {
    return bl_ast_can_impl_cast(from_type, bl_peek_type_enum(to_type)->base_type);
  }

  if (fkind != tkind) return false;
  if (fkind == BL_KIND_STRUCT || fkind == BL_KIND_FN) return false;

  return true;
}

bl_node_t *
bl_ast_node_dup(bl_ast_t *ast, bl_node_t *node)
{
  bl_node_t *tmp = alloc_node(ast, -1, NULL, bl_node_t *);
#if BL_DEBUG
  int tmp_serial = tmp->_serial;
#endif

  memcpy(tmp, node, sizeof(bl_node_t));
  tmp->next = NULL;
#if BL_DEBUG
  tmp->_serial = tmp_serial;
#endif

  return tmp;
}

int
bl_ast_type_get_ptr(bl_node_t *type)
{
  switch (bl_node_code(type)) {
  case BL_NODE_TYPE_FUND:
    return bl_peek_type_fund(type)->ptr;
  case BL_NODE_TYPE_FN:
    return bl_peek_type_fn(type)->ptr;
  case BL_NODE_TYPE_STRUCT:
    return bl_peek_type_struct(type)->ptr;
  default:
    bl_abort("invalid type");
  }
}

void
bl_ast_type_set_ptr(bl_node_t *type, int ptr)
{
  switch (bl_node_code(type)) {
  case BL_NODE_TYPE_FUND:
    bl_peek_type_fund(type)->ptr = ptr;
    break;
  case BL_NODE_TYPE_FN:
    bl_peek_type_fn(type)->ptr = ptr;
    break;
  case BL_NODE_TYPE_STRUCT:
    bl_peek_type_struct(type)->ptr = ptr;
    break;
  default:
    bl_abort("invalid type");
  }
}

bl_node_t *
bl_ast_unroll_ident(bl_node_t *ident)
{
  assert(ident);
  if (bl_node_is(ident, BL_NODE_IDENT)) {
    bl_node_ident_t *_ident = bl_peek_ident(ident);
    assert(_ident->ref);
    return bl_ast_unroll_ident(_ident->ref);
  }

  return ident;
}

bl_node_t *
bl_ast_get_ident(bl_node_t *node)
{
  assert(node);
  switch (bl_node_code(node)) {
  case BL_NODE_IDENT:
    return node;
  case BL_NODE_EXPR_MEMBER:
    return bl_peek_expr_member(node)->ident;
  default:
    bl_abort("node %s has no ident", bl_node_name(node));
  };
}
