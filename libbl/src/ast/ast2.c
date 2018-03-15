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

static const char *node_strings[] = {
#define nt(tok, str) str,
  BL_NTYPE_LIST
#undef nt
};

static void
delete_node(bl_node_t *node);

void
bl_ast2_init(bl_ast2_t *ast)
{
  ast->nodes = bo_array_new(sizeof(void *));
  ast->root = NULL;
}

void
bl_ast2_terminate(bl_ast2_t *ast)
{
  const size_t c = bo_array_size(ast->nodes);
  bl_node_t *node;

  for (size_t i = 0; i < c; i++) {
    node = bo_array_at(ast->nodes, i, bl_node_t *);
    delete_node(node);
  }

  bo_unref(ast->nodes);
  ast->root = NULL;
}

bl_node_t *
_bl_ast2_new_node(bl_ast2_t *ast,
                  bl_node_e type)
{
  bl_node_t *new_node;

  switch (type) {
    case BL_NODE_ITEM:
      new_node = bl_calloc(sizeof(bl_item_t), 1);
      break;
    case BL_NODE_MODULE:
      new_node = bl_calloc(sizeof(bl_module_t), 1);
      break;
    case BL_NODE_BLOCK:
      new_node = bl_calloc(sizeof(bl_block_t), 1);
      break;
    case BL_NODE_FUNC_DECL:
      new_node = bl_calloc(sizeof(bl_block_t), 1);
      break;
    default: bl_abort("unknown node type");
  }

  new_node->t = type;
  bo_array_push_back(ast->nodes, new_node);
  return new_node;
}

void
delete_node(bl_node_t *node)
{
  switch (node->t) {
    case BL_NODE_ITEM:
      break;
    case BL_NODE_MODULE:
      bo_unref(((bl_module_t *) node)->items);
      break;
    case BL_NODE_BLOCK:
      bo_unref(((bl_block_t *) node)->stmts);
      break;
    case BL_NODE_FUNC_DECL:
      bo_unref(((bl_func_decl_t *) node)->params);
      break;
    default: bl_abort("unknown node type");
  }
}

const char *
bl_node_to_str(bl_node_t *node)
{
  return node_strings[node->t];
}

bl_item_t *
bl_ast_module_push_item(bl_module_t *module,
                        bl_item_t *item)
{
  if (module->items == NULL) {
    module->items = bo_array_new(sizeof(void *));
  }

  if (item == NULL)
    return NULL;

  bo_array_push_back(module->items, item);
  return item;
}

size_t
bl_ast_module_item_count(bl_module_t *module)
{
  if (module->items == NULL)
    return 0;
  return bo_array_size(module->items);
}

bl_item_t *
bl_ast_module_get_item(bl_module_t *module,
                       size_t i)
{
  if (module->items == NULL)
    return 0;

  return bo_array_at(module->items, i, bl_item_t *);
}

