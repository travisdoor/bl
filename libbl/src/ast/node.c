//*****************************************************************************
// bl
//
// File:   node.c
// Author: Martin Dorazil
// Date:   3/1/18
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

#include <string.h>
#include "ast/node_impl.h"
#include "bl/bldebug.h"
#include "blmemory_impl.h"

static const char *node_strings[] = {
#define nt(tok, str) str,
  BL_NTYPE_LIST
#undef nt
};

/* public */
bl_node_t *
bl_node_new(bl_node_type_e type,
            const char *generated_from,
            int line,
            int col)
{
  bl_node_t *node = bl_calloc(1, sizeof(bl_node_t));

  node->type = type;
  node->generated_from = generated_from;
  node->line = line;
  node->col = col;

  switch (type) {
    case BL_NODE_GLOBAL_STMT:
      node->value.glob_stmt.nodes = bo_array_new(sizeof(bl_node_t *));
      break;
    case BL_NODE_CMP_STMT:
      node->value.cmp_stmt.nodes = bo_array_new(sizeof(bl_node_t *));
      break;
    case BL_NODE_IF_STMT:
      break;
    case BL_NODE_BREAK_STMT:
      break;
    case BL_NODE_RETURN_STMT:
      break;
    case BL_NODE_LOOP_STMT:
      break;
    case BL_NODE_CONTINUE_STMT:
      break;
    case BL_NODE_FUNC_DECL:
      node->value.func_decl.params = bo_array_new(sizeof(bl_node_t *));
      break;
    case BL_NODE_VAR_DECL:
      break;
    case BL_NODE_PARAM_VAR_DECL:
      break;
    case BL_NODE_CALL_EXPR:
      node->value.call_expr.args = bo_array_new(sizeof(bl_node_t *));
      break;
    case BL_NODE_DECL_REF_EXPR:
      break;
    case BL_NODE_CONST_EXPR:
      break;
    case BL_NODE_BINOP:
      break;
    default: bl_abort("invalid node type");
  }
  return node;
}

void
bl_node_delete(bl_node_t *node)
{
  switch (node->type) {
    case BL_NODE_GLOBAL_STMT:
      bo_unref(node->value.glob_stmt.nodes);
      break;
    case BL_NODE_CMP_STMT:
      bo_unref(node->value.cmp_stmt.nodes);
      break;
    case BL_NODE_IF_STMT:
      break;
    case BL_NODE_BREAK_STMT:
      break;
    case BL_NODE_RETURN_STMT:
      break;
    case BL_NODE_LOOP_STMT:
      break;
    case BL_NODE_CONTINUE_STMT:
      break;
    case BL_NODE_FUNC_DECL:
      bo_unref(node->value.func_decl.params);
      break;
    case BL_NODE_VAR_DECL:
      break;
    case BL_NODE_PARAM_VAR_DECL:
      break;
    case BL_NODE_CALL_EXPR:
      bo_unref(node->value.func_decl.params);
      break;
    case BL_NODE_DECL_REF_EXPR:
      break;
    case BL_NODE_CONST_EXPR:
      break;
    case BL_NODE_BINOP:
      break;
    default: bl_abort("invalid node type");
  }

  bl_free(node);
}

const char *
bl_node_to_str(bl_node_t *node)
{
  return node_strings[node->type];
}

bl_node_t *
bl_node_glob_stmt_add_child(bl_node_t *node,
                            bl_node_t *child)
{
  bl_assert(node->type == BL_NODE_GLOBAL_STMT, "invalid node");

  if (child == NULL)
    return NULL;

  bo_array_push_back(node->value.glob_stmt.nodes, child);
  return child;
}

int
bl_node_glob_stmt_get_children_count(bl_node_t *node)
{
  bl_assert(node->type == BL_NODE_GLOBAL_STMT, "invalid node");
  return (int) bo_array_size(node->value.glob_stmt.nodes);
}

bl_node_t *
bl_node_glob_stmt_get_child(bl_node_t *node,
                            int i)
{
  bl_assert(node->type == BL_NODE_GLOBAL_STMT, "invalid node");
  if (bo_array_size(node->value.glob_stmt.nodes) == 0)
    return NULL;

  return bo_array_at(node->value.glob_stmt.nodes, i, bl_node_t *);
}

bl_node_t *
bl_node_cmp_stmt_add_child(bl_node_t *node,
                           bl_node_t *child)
{
  bl_assert(node->type == BL_NODE_CMP_STMT, "invalid node");

  if (child == NULL)
    return NULL;

  bo_array_push_back(node->value.cmp_stmt.nodes, child);
  return child;
}

int
bl_node_cmp_stmt_get_children_count(bl_node_t *node)
{
  bl_assert(node->type == BL_NODE_CMP_STMT, "invalid node");
  return (int) bo_array_size(node->value.cmp_stmt.nodes);
}

bl_node_t *
bl_node_cmp_stmt_get_child(bl_node_t *node,
                           int i)
{
  bl_assert(node->type == BL_NODE_CMP_STMT, "invalid node");

  if (bo_array_size(node->value.cmp_stmt.nodes) == 0)
    return NULL;

  return bo_array_at(node->value.cmp_stmt.nodes, i, bl_node_t *);
}

bl_node_t *
bl_node_func_decl_stmt_add_param(bl_node_t *node,
                                 bl_node_t *param)
{
  bl_assert(node->type == BL_NODE_FUNC_DECL, "invalid node");

  if (param == NULL)
    return NULL;

  bo_array_push_back(node->value.func_decl.params, param);
  return param;
}

int
bl_node_func_decl_get_param_count(bl_node_t *node)
{
  bl_assert(node->type == BL_NODE_FUNC_DECL, "invalid node");
  return (int) bo_array_size(node->value.func_decl.params);
}

bl_node_t *
bl_node_func_decl_get_param(bl_node_t *node,
                            int i)
{
  bl_assert(node->type == BL_NODE_FUNC_DECL, "invalid node");
  if (bo_array_size(node->value.func_decl.params) == 0)
    return NULL;

  return bo_array_at(node->value.func_decl.params, i, bl_node_t *);
}

bl_node_t *
bl_node_call_expr_add_arg(bl_node_t *node,
                          bl_node_t *arg)
{
  bl_assert(node->type == BL_NODE_CALL_EXPR, "invalid node");

  if (arg == NULL)
    return NULL;

  bo_array_push_back(node->value.call_expr.args, arg);
  return arg;
}

int
bl_node_call_expr_get_arg_count(bl_node_t *node)
{
  bl_assert(node->type == BL_NODE_CALL_EXPR, "invalid node");
  return (int) bo_array_size(node->value.call_expr.args);
}

bl_node_t *
bl_node_call_expr_get_arg(bl_node_t *node,
                          int i)
{
  bl_assert(node->type == BL_NODE_CALL_EXPR, "invalid node");
  if (bo_array_size(node->value.call_expr.args) == 0)
    return NULL;

  return bo_array_at(node->value.call_expr.args, i, bl_node_t *);
}
