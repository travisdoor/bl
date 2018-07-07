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

const char *bl_ftype_strings[] = {
#define ft(tok, str) str,
    _BL_FTYPE_LIST
#undef ft
};

bl_node_t bl_ftypes[] = {
#define ft(name, str)                                                                              \
  (bl_node_t){.code             = BL_NODE_TYPE_FUND,                                               \
              .src              = NULL,                                                            \
              .next             = NULL,                                                            \
              .prev             = NULL,                                                            \
              .n.type_fund.code = BL_FTYPE_##name},

    _BL_FTYPE_LIST
#undef ft
};

const char *bl_node_type_strings[] = {
#define nt(code, name, data) #name,
    _BL_NODE_TYPE_LIST
#undef nt
};

uint64_t bl_ftype_hashes[BL_FTYPE_COUNT];

static void
node_terminate(bl_node_t *node)
{
  switch (node->code) {
  default:
    break;
  }
}

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

  for (int i = 0; i < chunk->count - 1; ++i) {
    node_terminate(get_node_in_chunk(chunk, i + 1));
  }
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

  return node;
}

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  ast->first_chunk   = NULL;
  ast->current_chunk = NULL;

  /* init ftype hashes */
  const char *it;
  bl_array_foreach(bl_ftype_strings, it)
  {
    bl_ftype_hashes[i] = bo_hash_from_str(it);
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

_BL_AST_NCTOR(ublock)
{
  return alloc_node(ast, BL_NODE_UBLOCK, tok, bl_node_t *);
}

_BL_AST_NCTOR(ident, bl_node_t *ref)
{
  bl_node_ident_t *_ident = alloc_node(ast, BL_NODE_IDENT, tok, bl_node_ident_t *);
  _ident->hash            = bo_hash_from_str(tok->value.str);
  _ident->str             = tok->value.str;
  _ident->ref             = ref;
  return (bl_node_t *)_ident;
}

_BL_AST_NCTOR(stmt_bad)
{
  return alloc_node(ast, BL_NODE_STMT_BAD, tok, bl_node_t *);
}

_BL_AST_NCTOR(stmt_return, bl_node_t *expr, bl_node_t *fn)
{
  bl_node_stmt_return_t *_ret = alloc_node(ast, BL_NODE_STMT_RETURN, tok, bl_node_stmt_return_t *);
  _ret->expr                  = expr;
  _ret->fn                    = fn;
  return (bl_node_t *)_ret;
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

_BL_AST_NCTOR(block, bl_node_t *nodes)
{
  bl_node_decl_block_t *_block = alloc_node(ast, BL_NODE_DECL_BLOCK, tok, bl_node_decl_block_t *);
  _block->nodes                = nodes;
  return (bl_node_t *)_block;
}

_BL_AST_NCTOR(decl, bl_node_t *name, bl_node_t *type, bl_node_t *value, bool mutable)
{
  bl_node_decl_value_t *_decl = alloc_node(ast, BL_NODE_DECL_VALUE, tok, bl_node_decl_value_t *);
  _decl->type                 = type;
  _decl->name                 = name;
  _decl->value                = value;
  _decl->mutable              = mutable;
  return (bl_node_t *)_decl;
}

_BL_AST_NCTOR(decl_bad)
{
  return alloc_node(ast, BL_NODE_DECL_BAD, tok, bl_node_t *);
}

_BL_AST_NCTOR(type_bad)
{
  return alloc_node(ast, BL_NODE_TYPE_BAD, tok, bl_node_t *);
}

_BL_AST_NCTOR(type_fn, bl_node_t *arg_types, bl_node_t *ret_type)
{
  bl_node_type_fn_t *_type_fn = alloc_node(ast, BL_NODE_TYPE_FN, tok, bl_node_type_fn_t *);
  _type_fn->arg_types         = arg_types;
  _type_fn->ret_type          = ret_type;
  return (bl_node_t *)_type_fn;
}

_BL_AST_NCTOR(type_struct, bl_node_t *types)
{
  bl_node_type_struct_t *_type_struct =
      alloc_node(ast, BL_NODE_TYPE_STRUCT, tok, bl_node_type_struct_t *);
  _type_struct->types = types;
  return (bl_node_t *)_type_struct;
}

_BL_AST_NCTOR(lit_fn, bl_node_t *type, bl_node_t *block)
{
  bl_node_lit_fn_t *_lit_fn = alloc_node(ast, BL_NODE_LIT_FN, tok, bl_node_lit_fn_t *);
  _lit_fn->type             = type;
  _lit_fn->block            = block;
  return (bl_node_t *)_lit_fn;
}

_BL_AST_NCTOR(expr_bad)
{
  return alloc_node(ast, BL_NODE_EXPR_BAD, tok, bl_node_t *);
}

_BL_AST_NCTOR(lit, bl_node_t *type)
{
  bl_node_lit_t *_lit = alloc_node(ast, BL_NODE_LIT, tok, bl_node_lit_t *);
  _lit->type          = type;
  _lit->token         = tok;
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

/*************************************************************************************************
 * other
 *************************************************************************************************/

void
bl_ast_insert(bl_node_t **dest, bl_node_t *src)
{
  if (!dest || !src) return;

  if (*dest) {
    if ((*dest)->prev) (*dest)->prev->next = src;
    src->prev     = (*dest)->prev;
    src->next     = (*dest);
    (*dest)->prev = src;
  }
  *dest = src;
}

static void
_type_to_string(char *buf, size_t len, bl_node_t *type)
{
  if (!buf || !type) return;

#define append_buf(buf, len, str)                                                                  \
  {                                                                                                \
    const size_t filled = strlen(buf);                                                             \
    snprintf((buf) + filled, (len)-filled, "%s", str);                                             \
  }

  switch (bl_node_code(type)) {
  case BL_NODE_IDENT: {
    // identificator can lead to type
    _type_to_string(buf, len, bl_peek_ident(type)->ref);
    break;
  }

  case BL_NODE_DECL_VALUE: {
    // identificator can lead to type
    _type_to_string(buf, len, bl_peek_decl_value(type)->type);
    break;
  }

  case BL_NODE_TYPE_FUND: {
    append_buf(buf, len, bl_ftype_strings[bl_peek_type_fund(type)->code]);
    break;
  }

  case BL_NODE_TYPE_FN: {
    append_buf(buf, len, "fn (");
    bl_node_type_fn_t *_fn = bl_peek_type_fn(type);
    bl_node_t *        arg = _fn->arg_types;
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
    append_buf(buf, len, "struct {");
    bl_node_type_struct_t *_struct = bl_peek_type_struct(type);
    bl_node_t *            t       = _struct->types;
    while (t) {
      _type_to_string(buf, len, t);
      t = t->next;
      if (t) append_buf(buf, len, ", ");
    }
    append_buf(buf, len, "}");
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

/**************************************************************************************************/
