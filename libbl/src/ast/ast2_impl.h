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
  nt(FUNC, "func") \
  nt(STRUCT, "struct") \
  nt(ENUM, "enum") \
  nt(BLOCK, "block") \
  nt(TYPE, "type") \
  nt(MODULE, "module")

// clang-format on

typedef enum {
#define nt(tok, str) BL_NODE_##tok,
  BL_NTYPE_LIST
#undef nt
} bl_node_e;

typedef struct bl_ast bl_ast_t;
typedef struct bl_src bl_src_t;

struct bl_src
{
  int         line;
  int         col;
  const char *file;
};

#define BL_MODULE(node) (node)->n.module
#define BL_FUNC(node) (node)->n.func
#define BL_STRUCT(node) (node)->n.strct
#define BL_ENUM(node) (node)->n.enm
#define BL_BLOCK(node) (node)->n.block
#define BL_TYPE(node) (node)->n.type

typedef struct bl_node   bl_node_t;
typedef struct bl_module bl_module_t;
typedef struct bl_func   bl_func_t;
typedef struct bl_struct bl_struct_t;
typedef struct bl_enum   bl_enum_t;
typedef struct bl_block  bl_block_t;
typedef struct bl_type   bl_type_t;

typedef enum bl_fund_type bl_fund_type_e;

/*
 * AST main context data
 */
struct bl_ast
{
  bl_node_t *root;
  bl_node_t *cache_begin;
  bl_node_t *chunk_current;
  size_t     chunk_used;
};

/*
 * type node
 */ 
enum bl_fund_type
{
  BL_FTYPE_VOID   = 0x7c9faa57,
  BL_FTYPE_I8     = 0x597806,
  BL_FTYPE_I32    = 0xb887853,
  BL_FTYPE_I64    = 0xb8878b8,
  BL_FTYPE_U8     = 0x597992,
  BL_FTYPE_U32    = 0xb88ab5f,
  BL_FTYPE_U64    = 0xb88abc4,
  BL_FTYPE_F32    = 0xb886b90,
  BL_FTYPE_F64    = 0xb886bf5,
  BL_FTYPE_CHAR   = 0x7c952063,
  BL_FTYPE_STRING = 0x1c93affc,
  BL_FTYPE_BOOL   = 0x7c94b391,
};

struct bl_type
{
  bl_id_t id;

  enum
  {
    BL_TYPE_FUND,
    BL_TYPE_REF
  } t;

  union
  {
    bl_fund_type_e fund;
    bl_node_t *    ref;
  } type;
};

/*
 * module node
 */ 
struct bl_module
{
  bl_id_t id;
  BArray *nodes;
};

/*
 * function declaration node
 */ 
struct bl_func
{
  bl_id_t    id;
  BArray *   params;
  bl_node_t *block;
  // TODO: return type
};

/*
 * structure declaration node
 */ 
struct bl_struct
{
  bl_id_t id;
};

/*
 * enum declaration node
 */ 
struct bl_enum
{
  bl_id_t id;
};

/*
 * block declaration node
 */ 
struct bl_block
{
  BArray *nodes;
};

/*
 * base node union type
 * this node is actually allocated
 */ 
struct bl_node
{
  bl_src_t  src;
  bl_node_e t;

  union
  {
    bl_module_t module;
    bl_func_t   func;
    bl_struct_t strct;
    bl_enum_t   enm;
    bl_block_t  block;
    bl_type_t   type;
  } n;
};

void
bl_ast_init(bl_ast_t *ast);

void
bl_ast_terminate(bl_ast_t *ast);

bl_node_t *
bl_ast_new_node(bl_ast_t *ast, bl_node_e type, bl_token_t *tok);

bl_node_t *
bl_ast_module_push_node(bl_module_t *module, bl_node_t *node);

size_t
bl_ast_module_node_count(bl_module_t *module);

bl_node_t *
bl_ast_module_get_node(bl_module_t *module, const size_t i);

#endif // BL_NODE2_IMPL_H
