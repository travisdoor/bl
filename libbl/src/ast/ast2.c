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

// static const char *node_strings[] = {
//#define nt(tok, str) str,
//  BL_NTYPE_LIST
//#undef nt
//};

#define CACHE_PREALLOC_ELEM 256

typedef struct
{
  bl_node_t *next;
} next_t;

static void
node_init(bl_node_t *node, bl_node_e type, bl_token_t *tok)
{
  /*
   * node allocated by calloc in ast tree has all values set to zero by default
   * when allocation method has been changed, use memset here for setting whole
   * memory block occupied by node to 0
   */

  node->t = type;
  if (tok != NULL) {
    node->src = &tok->src;
  }

  switch (node->t) {
  case BL_NODE_MODULE:
    bl_peek_module(node).nodes = bo_array_new(sizeof(bl_node_t *));
    break;
  case BL_NODE_BLOCK:
    bl_peek_block(node).nodes = bo_array_new(sizeof(bl_node_t *));
    break;
  case BL_NODE_FUNC:
    bl_peek_func(node).args = bo_array_new(sizeof(bl_node_t *));
    break;
  case BL_NODE_STRUCT:
  case BL_NODE_ENUM:
  case BL_NODE_VAR:
  case BL_NODE_TYPE:
  case BL_NODE_EXPR:
  case BL_NODE_ARG:
    break;
  default:
    bl_abort("invalid node type");
  }
}

static void
node_terminate(bl_node_t *node)
{
  switch (node->t) {
  case BL_NODE_MODULE:
    bo_unref(bl_peek_module(node).nodes);
    break;
  case BL_NODE_BLOCK:
    bo_unref(bl_peek_block(node).nodes);
    break;
  case BL_NODE_FUNC:
    bo_unref(bl_peek_func(node).args);
    break;
  case BL_NODE_STRUCT:
  case BL_NODE_ENUM:
  case BL_NODE_VAR:
  case BL_NODE_TYPE:
  case BL_NODE_EXPR:
  case BL_NODE_ARG:
    break;
  default:
    bl_abort("invalid node type");
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
bl_ast_new_node(bl_ast_t *ast, bl_node_e type, bl_token_t *tok)
{
  if (ast->chunk_used >= CACHE_PREALLOC_ELEM) {
    void *new_chunk = bl_calloc(CACHE_PREALLOC_ELEM + 1, sizeof(bl_node_t));
    ((next_t *)&ast->chunk_current[CACHE_PREALLOC_ELEM])->next = new_chunk;
    ast->chunk_current                                         = new_chunk;
    ast->chunk_used                                            = 0;
  }

  bl_node_t *node = &(ast->chunk_current[ast->chunk_used]);
  node_init(node, type, tok);
  ast->chunk_used++;
  return node;
}

bl_node_t *
bl_ast_module_push_node(bl_module_t *module, bl_node_t *node)
{
  if (node == NULL)
    return NULL;

  bo_array_push_back(module->nodes, node);
  return node;
}

size_t
bl_ast_module_node_count(bl_module_t *module)
{
  return bo_array_size(module->nodes);
}

bl_node_t *
bl_ast_module_get_node(bl_module_t *module, const size_t i)
{
  return bo_array_at(module->nodes, i, bl_node_t *);
}

bl_node_t *
bl_ast_func_push_arg(bl_func_t *func, bl_node_t *arg)
{
  if (arg == NULL)
    return NULL;

  bo_array_push_back(func->args, arg);
  return arg;
}

size_t
bl_ast_func_arg_count(bl_func_t *func)
{
  return bo_array_size(func->args);
}

bl_node_t *
bl_ast_func_get_arg(bl_func_t *func, const size_t i)
{
  return bo_array_at(func->args, i, bl_node_t *);
}

bl_node_t *
bl_ast_block_push_node(bl_block_t *block, bl_node_t *node)
{
  if (node == NULL)
    return NULL;

  bo_array_push_back(block->nodes, node);
  return node;
}

size_t
bl_ast_block_node_count(bl_block_t *block)
{
  return bo_array_size(block->nodes);
}

bl_node_t *
bl_ast_block_get_node(bl_block_t *block, const size_t i)
{
  return bo_array_at(block->nodes, i, bl_node_t *);
}
