//*****************************************************************************
// bl
//
// File:   ast2_impl.h
// Author: Martin Dorazil
// Date:   3/14/18
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

#ifndef BL_NODE2_IMPL_H
#define BL_NODE2_IMPL_H

#include <bobject/containers/array.h>
#include "id_impl.h"
#include "token_impl.h"

// clang-format off
#define BL_NTYPE_LIST                                                                              \
  nt(ITEM, "item") \
  nt(MODULE, "module") \
  nt(BLOCK, "block") \
  nt(FUNC_DECL, "func_decl") \
  nt(STRUCT_DECL, "struct_decl") \
  nt(ENUM_DECL, "enum_decl") \
  nt(ARG, "arg") \
// clang-format on

typedef struct
{
  int line;
  int col;
  const char *file;
} bl_src_t;

typedef enum {
#define nt(tok, str) BL_NODE_##tok,
  BL_NTYPE_LIST
#undef nt
} bl_node_e;

typedef struct
{
  bl_node_e t;
} bl_node_t;

typedef struct
{
  bl_node_t base_;
  BArray *items;
} bl_module_t;

typedef struct
{
  bl_node_t base_;
  BArray *params;
  // TODO: return type
} bl_func_decl_t;

typedef struct
{
  bl_node_t base_;
} bl_struct_decl_t;

typedef struct
{
  bl_node_t base_;
} bl_enum_decl_t;

typedef struct
{
  bl_node_t base_;
  bl_id_t id;
  // TODO: type 
} bl_arg_t;

typedef struct
{
  bl_node_t base_;
  BArray *stmts;
} bl_block_t;

typedef struct
{
  bl_node_t base_;
  enum
  {
    BL_ITEM_MODULE,
    BL_ITEM_FUNC,
    BL_ITEM_STRUCT,
    BL_ITEM_ENUM
  } t;

  bl_src_t src;
  bl_id_t id;
  union
  {
    bl_module_t *module;
    bl_struct_decl_t *struct_decl;
    bl_enum_decl_t *enum_decl;

    struct
    {
      bl_func_decl_t *func_decl;
      bl_block_t *block;
    } func;
  } node;
} bl_item_t;

typedef struct
{
  BArray *nodes;
  bl_module_t *root;
} bl_ast2_t;

void
bl_src_init(bl_src_t *src, bl_token_t *tok);

void
bl_ast2_init(bl_ast2_t *ast);

void
bl_ast2_terminate(bl_ast2_t *ast);

bl_node_t *
_bl_ast2_new_node(bl_ast2_t *ast, bl_node_e type);

const char *
bl_node_to_str(bl_node_t *node);

#define bl_ast2_new_node(ast, nt, t) (t *)_bl_ast2_new_node((ast), (nt));

bl_item_t *
bl_ast_module_push_item(bl_module_t *module, bl_item_t *item);

size_t
bl_ast_module_item_count(bl_module_t *module);

bl_item_t *
bl_ast_module_get_item(bl_module_t *module, size_t i);


bl_arg_t *
bl_ast_func_push_arg(bl_func_decl_t *func, bl_arg_t *arg);

size_t
bl_ast_func_arg_count(bl_func_decl_t *func);

bl_arg_t *
bl_ast_func_get_arg(bl_func_decl_t *func, size_t i);

#endif // BL_NODE2_IMPL_H
