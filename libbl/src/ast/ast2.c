//*****************************************************************************
// bl
//
// File:   ast2.c
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
//*****************************************************************************

#include "ast/ast2_impl.h"
#include "common_impl.h"

#define CACHE_PREALLOC_ELEM 256

typedef struct
{
  bl_node_t *next;
} next_t;

static bl_node_t *
alloc_node(bl_ast_t *ast)
{
  if (ast->chunk_used >= CACHE_PREALLOC_ELEM) {
    void *new_chunk = bl_calloc(CACHE_PREALLOC_ELEM + 1, sizeof(bl_node_t));
    ((next_t *)&ast->chunk_current[CACHE_PREALLOC_ELEM])->next = new_chunk;
    ast->chunk_current                                         = new_chunk;
    ast->chunk_used                                            = 0;
  }

  return &(ast->chunk_current[ast->chunk_used++]);
}

static void
node_terminate(bl_node_t *node)
{
  switch (node->node_variant) {
  case BL_NODE_DECL:
    switch (bl_peek_decl(node)->decl_variant) {
    case BL_DECL_MODULE:
      bo_unref(bl_peek_decl_module(node)->nodes);
      break;
    case BL_DECL_FUNC:
      bo_unref(bl_peek_decl_func(node)->args);
      break;
    case BL_DECL_BLOCK:
      bo_unref(bl_peek_decl_block(node)->nodes);
      break;
    default:
      break;
    }
  case BL_NODE_EXPR:
    switch (bl_peek_expr(node)->expr_variant) {
    case BL_EXPR_CALL:
      bo_unref(bl_peek_expr_call(node)->args);
      break;
    default:
      break;
    }

  case BL_NODE_STMT:
  case BL_NODE_TYPE:
    break;
  default:
    bl_abort("invalid node");
  }
}

/* public */
void
bl_ast_init(bl_ast_t *ast)
{
  /* one extra element for jump to next chunk */
  ast->chunk_current = bl_calloc(CACHE_PREALLOC_ELEM + 1, sizeof(bl_node_t));
  ast->chunk_used    = 0;

  ((next_t *)&ast->chunk_current[CACHE_PREALLOC_ELEM])->next = NULL;
  ast->cache_begin                                           = ast->chunk_current;
}

void
bl_ast_terminate(bl_ast_t *ast)
{
  bl_node_t *node;
  bl_node_t *chunk = ast->cache_begin;
  int        i     = 0;

  while (chunk != NULL) {
    if (i != 0 && i % CACHE_PREALLOC_ELEM == 0) {
      chunk = ((next_t *)&chunk[i])->next;
      i     = 0;
      continue;
    }

    node = &chunk[i];
    node_terminate(node);
    i++;
  }

  bl_free(ast->cache_begin);
}

bl_node_t *
bl_ast_add_type_fund(bl_ast_t *ast, bl_token_t *tok, bl_fund_type_e t)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->node_variant               = BL_NODE_TYPE;
  bl_peek_type(type)->type_variant = BL_TYPE_FUND;
  bl_peek_type_fund(type)->type    = t;

  return type;
}

bl_node_t *
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref)
{
  bl_node_t *type = alloc_node(ast);
  if (tok)
    type->src = &tok->src;

  type->node_variant               = BL_NODE_TYPE;
  bl_peek_type(type)->type_variant = BL_TYPE_REF;
  bl_id_init(&bl_peek_type_ref(type)->id, name);
  bl_peek_type_ref(type)->ref = ref;

  return type;
}

bl_node_t *
bl_ast_add_expr_const_char(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, char c)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->node_variant                = BL_NODE_EXPR;
  bl_peek_expr(expr_const)->expr_variant  = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.c = c;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_bool(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, bool b)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->node_variant                = BL_NODE_EXPR;
  bl_peek_expr(expr_const)->expr_variant  = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.b = b;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_signed(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, long long s)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->node_variant                = BL_NODE_EXPR;
  bl_peek_expr(expr_const)->expr_variant  = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.s = s;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_unsigned(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type,
                               unsigned long long u)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->node_variant                = BL_NODE_EXPR;
  bl_peek_expr(expr_const)->expr_variant  = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.u = u;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_double(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, double f)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->node_variant                = BL_NODE_EXPR;
  bl_peek_expr(expr_const)->expr_variant  = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type    = type;
  bl_peek_expr_const(expr_const)->value.f = f;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_const_str(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, const char *str)
{
  bl_node_t *expr_const = alloc_node(ast);
  if (tok)
    expr_const->src = &tok->src;

  expr_const->node_variant                  = BL_NODE_EXPR;
  bl_peek_expr(expr_const)->expr_variant    = BL_EXPR_CONST;
  bl_peek_expr_const(expr_const)->type      = type;
  bl_peek_expr_const(expr_const)->value.str = str;

  return expr_const;
}

bl_node_t *
bl_ast_add_expr_binop(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *lhs, bl_node_t *rhs)
{
  bl_node_t *binop = alloc_node(ast);
  if (tok)
    binop->src = &tok->src;

  binop->node_variant               = BL_NODE_EXPR;
  bl_peek_expr(binop)->expr_variant = BL_EXPR_BINOP;
  bl_peek_expr_binop(binop)->op     = op;
  bl_peek_expr_binop(binop)->lhs    = lhs;
  bl_peek_expr_binop(binop)->rhs    = rhs;

  return binop;
}

bl_node_t *
bl_ast_add_expr_var_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref)
{
  bl_node_t *var_ref = alloc_node(ast);
  if (tok)
    var_ref->src = &tok->src;

  var_ref->node_variant               = BL_NODE_EXPR;
  bl_peek_expr(var_ref)->expr_variant = BL_EXPR_VAR_REF;
  bl_id_init(&bl_peek_expr_var_ref(var_ref)->id, name);
  bl_peek_expr_var_ref(var_ref)->ref = ref;

  return var_ref;
}

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref)
{
  bl_node_t *call = alloc_node(ast);
  if (tok)
    call->src = &tok->src;

  call->node_variant               = BL_NODE_EXPR;
  bl_peek_expr(call)->expr_variant = BL_EXPR_CALL;
  bl_id_init(&bl_peek_expr_call(call)->id, name);
  bl_peek_expr_call(call)->ref = ref;

  return call;
}

bl_node_t *
bl_ast_add_expr_path(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *next)
{
  bl_node_t *path = alloc_node(ast);
  if (tok)
    path->src = &tok->src;

  path->node_variant               = BL_NODE_EXPR;
  bl_peek_expr(path)->expr_variant = BL_EXPR_PATH;
  bl_id_init(&bl_peek_expr_path(path)->id, name);
  bl_peek_expr_path(path)->next = next;

  return path;
}

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *module = alloc_node(ast);
  if (tok)
    module->src = &tok->src;

  module->node_variant               = BL_NODE_DECL;
  bl_peek_decl(module)->decl_variant = BL_DECL_MODULE;
  if (name != NULL)
    bl_id_init(&bl_peek_decl_module(module)->id, name);

  return module;
}

bl_node_t *
bl_ast_add_decl_var(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr)
{
  bl_node_t *var = alloc_node(ast);
  if (tok)
    var->src = &tok->src;

  var->node_variant                = BL_NODE_DECL;
  bl_peek_decl(var)->decl_variant  = BL_DECL_VAR;
  bl_peek_decl_var(var)->init_expr = init_expr;
  bl_peek_decl_var(var)->type      = type;
  bl_id_init(&bl_peek_decl_var(var)->id, name);

  return var;
}

bl_node_t *
bl_ast_add_decl_arg(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type)
{
  bl_node_t *arg = alloc_node(ast);
  if (tok)
    arg->src = &tok->src;

  arg->node_variant               = BL_NODE_DECL;
  bl_peek_decl(arg)->decl_variant = BL_DECL_ARG;
  bl_peek_decl_arg(arg)->type     = type;
  bl_id_init(&bl_peek_decl_arg(arg)->id, name);

  return arg;
}

bl_node_t *
bl_ast_add_decl_func(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *block,
                     bl_node_t *ret_type)
{
  bl_node_t *func = alloc_node(ast);
  if (tok)
    func->src = &tok->src;

  func->node_variant                = BL_NODE_DECL;
  bl_peek_decl(func)->decl_variant  = BL_DECL_FUNC;
  bl_peek_decl_func(func)->block    = block;
  bl_peek_decl_func(func)->ret_type = ret_type;
  bl_id_init(&bl_peek_decl_func(func)->id, name);

  return func;
}

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *strct = alloc_node(ast);
  if (tok)
    strct->src = &tok->src;

  strct->node_variant               = BL_NODE_DECL;
  bl_peek_decl(strct)->decl_variant = BL_DECL_STRUCT;
  bl_id_init(&bl_peek_decl_struct(strct)->id, name);

  return strct;
}

bl_node_t *
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name)
{
  bl_node_t *enm = alloc_node(ast);
  if (tok)
    enm->src = &tok->src;

  enm->node_variant               = BL_NODE_DECL;
  bl_peek_decl(enm)->decl_variant = BL_DECL_ENUM;
  bl_id_init(&bl_peek_decl_enum(enm)->id, name);

  return enm;
}

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok)
{
  bl_node_t *block = alloc_node(ast);
  if (tok)
    block->src = &tok->src;

  block->node_variant               = BL_NODE_DECL;
  bl_peek_decl(block)->decl_variant = BL_DECL_BLOCK;

  return block;
}

bl_node_t *
bl_ast_module_push_node(bl_node_t *module, bl_node_t *node)
{
  bl_assert(bl_peek_decl(module)->decl_variant == BL_DECL_MODULE, "invalid module");
  if (node == NULL)
    return NULL;

  if (bl_peek_decl_module(module)->nodes == NULL) {
    bl_peek_decl_module(module)->nodes = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_decl_module(module)->nodes, node);
  return node;
}

size_t
bl_ast_module_node_count(bl_node_t *module)
{
  bl_assert(bl_peek_decl(module)->decl_variant == BL_DECL_MODULE, "invalid module");
  if (bl_peek_decl_module(module)->nodes == NULL)
    return 0;
  return bo_array_size(bl_peek_decl_module(module)->nodes);
}

bl_node_t *
bl_ast_module_get_node(bl_node_t *module, const size_t i)
{
  bl_assert(bl_peek_decl(module)->decl_variant == BL_DECL_MODULE, "invalid module");
  if (bl_peek_decl_module(module)->nodes == NULL)
    return NULL;
  return bo_array_at(bl_peek_decl_module(module)->nodes, i, bl_node_t *);
}

bl_node_t *
bl_ast_func_push_arg(bl_node_t *func, bl_node_t *arg)
{
  bl_assert(bl_peek_decl(func)->decl_variant == BL_DECL_FUNC, "invalid module");
  if (arg == NULL)
    return NULL;

  if (bl_peek_decl_func(func)->args == NULL) {
    bl_peek_decl_func(func)->args = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_decl_func(func)->args, arg);
  return arg;
}

size_t
bl_ast_func_arg_count(bl_node_t *func)
{
  bl_assert(bl_peek_decl(func)->decl_variant == BL_DECL_FUNC, "invalid func");
  if (bl_peek_decl_func(func)->args == NULL)
    return 0;
  return bo_array_size(bl_peek_decl_func(func)->args);
}

bl_node_t *
bl_ast_func_get_arg(bl_node_t *func, const size_t i)
{
  bl_assert(bl_peek_decl(func)->decl_variant == BL_DECL_FUNC, "invalid func");
  if (bl_peek_decl_func(func)->args == NULL)
    return NULL;
  return bo_array_at(bl_peek_decl_func(func)->args, i, bl_node_t *);
}

bl_node_t *
bl_ast_block_push_node(bl_node_t *block, bl_node_t *node)
{
  bl_assert(bl_peek_decl(block)->decl_variant == BL_DECL_BLOCK, "invalid block");
  if (node == NULL)
    return NULL;

  if (bl_peek_decl_block(block)->nodes == NULL) {
    bl_peek_decl_block(block)->nodes = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_decl_block(block)->nodes, node);
  return node;
}

size_t
bl_ast_block_node_count(bl_node_t *block)
{
  bl_assert(bl_peek_decl(block)->decl_variant == BL_DECL_BLOCK, "invalid block");
  if (bl_peek_decl_block(block)->nodes == NULL)
    return 0;
  return bo_array_size(bl_peek_decl_block(block)->nodes);
}

bl_node_t *
bl_ast_block_get_node(bl_node_t *block, const size_t i)
{
  bl_assert(bl_peek_decl(block)->decl_variant == BL_DECL_BLOCK, "invalid block");
  if (bl_peek_decl_block(block)->nodes == NULL)
    return NULL;
  return bo_array_at(bl_peek_decl_block(block)->nodes, i, bl_node_t *);
}

bl_node_t *
bl_ast_call_push_arg(bl_node_t *call, bl_node_t *arg)
{
  bl_assert(bl_peek_expr(call)->expr_variant == BL_EXPR_CALL, "invalid call");
  if (arg == NULL)
    return NULL;

  if (bl_peek_expr_call(call)->args == NULL) {
    bl_peek_expr_call(call)->args = bo_array_new(sizeof(bl_node_t *));
  }

  bo_array_push_back(bl_peek_expr_call(call)->args, arg);
  return arg;
}

size_t
bl_ast_call_arg_count(bl_node_t *call)
{
  bl_assert(bl_peek_expr(call)->expr_variant == BL_EXPR_CALL, "invalid call");
  if (bl_peek_expr_call(call)->args == NULL)
    return 0;

  return bo_array_size(bl_peek_expr_call(call)->args);
}

bl_node_t *
bl_ast_call_get_arg(bl_node_t *call, const size_t i)
{
  bl_assert(bl_peek_expr(call)->expr_variant == BL_EXPR_CALL, "invalid call");
  if (bl_peek_expr_call(call)->args == NULL)
    return NULL;
  return bo_array_at(bl_peek_expr_call(call)->args, i, bl_node_t *);
}
