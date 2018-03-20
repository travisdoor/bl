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

#include <bobject/containers/hash.h>
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
bl_src_init(bl_src_t *src, bl_token_t *tok)
{
  src->file = tok->file;
  src->line = tok->line;
  src->col  = tok->col;
}

void
bl_ast2_init(bl_ast2_t *ast)
{
  ast->nodes = bo_array_new(sizeof(void *));
  ast->root  = NULL;
}

void
bl_ast2_terminate(bl_ast2_t *ast)
{
  const size_t c = bo_array_size(ast->nodes);
  bl_node_t *  node;

  for (size_t i = 0; i < c; i++) {
    node = bo_array_at(ast->nodes, i, bl_node_t *);
    delete_node(node);
  }

  bo_unref(ast->nodes);
  ast->root = NULL;
}

bl_node_t *
_bl_ast2_new_node(bl_ast2_t *ast, bl_node_e type, bl_token_t *tok)
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
    new_node = bl_calloc(sizeof(bl_func_decl_t), 1);
    break;
  case BL_NODE_ARG:
    new_node = bl_calloc(sizeof(bl_arg_t), 1);
    break;
  case BL_NODE_STRUCT_DECL:
    new_node = bl_calloc(sizeof(bl_struct_decl_t), 1);
    break;
  case BL_NODE_ENUM_DECL:
    new_node = bl_calloc(sizeof(bl_enum_decl_t), 1);
    break;
  case BL_NODE_TYPE:
    new_node = bl_calloc(sizeof(bl_type_t), 1);
    break;
  case BL_NODE_STMT:
    new_node = bl_calloc(sizeof(bl_stmt_t), 1);
    break;
  case BL_NODE_EXPR:
    new_node = bl_calloc(sizeof(bl_expr_t), 1);
    break;
  case BL_NODE_DECL:
    new_node = bl_calloc(sizeof(bl_decl_t), 1);
    break;
  case BL_NODE_CONST_EXPR:
    new_node = bl_calloc(sizeof(bl_const_expr_t), 1);
    break;
  case BL_NODE_BINOP:
    new_node = bl_calloc(sizeof(bl_binop_t), 1);
  case BL_NODE_CALL:
    new_node = bl_calloc(sizeof(bl_call_t), 1);
    break;
  case BL_NODE_VAR_REF:
    new_node = bl_calloc(sizeof(bl_var_ref_t), 1);
    break;
  default:
    bl_abort("unknown node type");
  }

  new_node->t = type;
  if (tok != NULL)
    bl_src_init(&new_node->src, tok);
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
    bo_unref(((bl_module_t *)node)->items);
    break;
  case BL_NODE_BLOCK:
    bo_unref(((bl_block_t *)node)->stmts);
    break;
  case BL_NODE_FUNC_DECL:
    bo_unref(((bl_func_decl_t *)node)->params);
    break;
  case BL_NODE_CALL:
    bo_unref(((bl_call_t *)node)->args);
    break;
  case BL_NODE_ARG:
  case BL_NODE_STRUCT_DECL:
  case BL_NODE_ENUM_DECL:
  case BL_NODE_TYPE:
  case BL_NODE_STMT:
  case BL_NODE_EXPR:
  case BL_NODE_DECL:
  case BL_NODE_CONST_EXPR:
  case BL_NODE_BINOP:
  case BL_NODE_VAR_REF:
    break;
  default:
    bl_abort("unknown node type");
  }
}

const char *
bl_ast2_node_to_str(bl_node_t *node)
{
  return node_strings[node->t];
}

bl_item_t *
bl_ast_module_push_item(bl_module_t *module, bl_item_t *item)
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
bl_ast_module_get_item(bl_module_t *module, size_t i)
{
  if (module->items == NULL)
    return 0;

  return bo_array_at(module->items, i, bl_item_t *);
}

bl_arg_t *
bl_ast_func_push_arg(bl_func_decl_t *func, bl_arg_t *arg)
{
  if (func->params == NULL) {
    func->params = bo_array_new(sizeof(void *));
  }

  if (arg == NULL)
    return NULL;

  bo_array_push_back(func->params, arg);
  return arg;
}

size_t
bl_ast_func_arg_count(bl_func_decl_t *func)
{
  if (func->params == NULL)
    return 0;
  return bo_array_size(func->params);
}

bl_arg_t *
bl_ast_func_get_arg(bl_func_decl_t *func, size_t i)
{
  if (func->params == NULL)
    return 0;

  return bo_array_at(func->params, i, bl_arg_t *);
}

bl_stmt_t *
bl_ast_block_push_stmt(bl_block_t *block, bl_stmt_t *stmt)
{
  if (block->stmts == NULL) {
    block->stmts = bo_array_new(sizeof(void *));
  }

  if (stmt == NULL)
    return NULL;

  bo_array_push_back(block->stmts, stmt);
  return stmt;
}

size_t
bl_ast_block_stmt_count(bl_block_t *block)
{
  if (block->stmts == NULL)
    return 0;
  return bo_array_size(block->stmts);
}

bl_stmt_t *
bl_ast_block_get_stmt(bl_block_t *block, size_t i)
{
  if (block->stmts == NULL)
    return 0;

  return bo_array_at(block->stmts, i, bl_stmt_t *);
}

bl_expr_t *
bl_ast_call_push_arg(bl_call_t *call, bl_expr_t *expr)
{
  if (call->args == NULL) {
    call->args = bo_array_new(sizeof(void *));
  }

  if (call == NULL)
    return NULL;

  bo_array_push_back(call->args, expr);
  return expr;
}

size_t
bl_ast_call_arg_count(bl_call_t *call)
{
  if (call->args == NULL)
    return 0;
  return bo_array_size(call->args);
}

bl_expr_t *
bl_ast_call_get_arg(bl_call_t *call, size_t i)
{
  if (call->args == NULL)
    return 0;

  return bo_array_at(call->args, i, bl_expr_t *);
}
