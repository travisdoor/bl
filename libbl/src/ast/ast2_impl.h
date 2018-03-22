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
  nt(ARG, "arg") \
  nt(VAR, "var") \
  nt(EXPR, "expr") \
  nt(MODULE, "module")

// clang-format on

typedef enum {
#define nt(tok, str) BL_NODE_##tok,
  BL_NTYPE_LIST
#undef nt
} bl_node_e;

typedef struct bl_ast bl_ast_t;

#define bl_peek_module(node) (node)->n.module
#define bl_peek_func(node) (node)->n.func
#define bl_peek_struct(node) (node)->n.strct
#define bl_peek_enum(node) (node)->n.enm
#define bl_peek_block(node) (node)->n.block
#define bl_peek_type(node) (node)->n.type
#define bl_peek_arg(node) (node)->n.arg
#define bl_peek_var(node) (node)->n.var
#define bl_peek_expr(node) (node)->n.expr

#define bl_peek_const_expr(node) (node)->n.expr.expr.const_expr
#define bl_peek_binop(node) (node)->n.expr.expr.binop

typedef struct bl_node   bl_node_t;
typedef struct bl_module bl_module_t;
typedef struct bl_func   bl_func_t;
typedef struct bl_struct bl_struct_t;
typedef struct bl_enum   bl_enum_t;
typedef struct bl_block  bl_block_t;
typedef struct bl_type   bl_type_t;
typedef struct bl_arg    bl_arg_t;
typedef struct bl_var    bl_var_t;
typedef struct bl_expr   bl_expr_t;

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
 * expr node
 */
struct bl_expr
{
  enum
  {
    BL_EXPR_CONST,
    BL_EXPR_BINOP
  } t;

  union
  {
    struct
    {
      bl_node_t *type;

      union
      {
        char               c;
        bool               b;
        long long          s;
        unsigned long long u;
        double             f;
        const char *       str;
      } value;
    } const_expr;

    struct
    {
      bl_sym_e   op;
      bl_node_t *lhs;
      bl_node_t *rhs;
    } binop;

  } expr;
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
 * var declaration
 */
struct bl_var
{
  bl_id_t    id;
  bl_node_t *type;
};

/*
 * func argument
 */
struct bl_arg
{
  bl_id_t    id;
  bl_node_t *type;
};

/*
 * function declaration node
 */
struct bl_func
{
  bl_id_t    id;
  BArray *   args;
  bl_node_t *block;
  bl_node_t *ret_type;
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
  bl_src_t *src;
  bl_node_e t;

  union
  {
    bl_module_t module;
    bl_func_t   func;
    bl_struct_t strct;
    bl_enum_t   enm;
    bl_block_t  block;
    bl_type_t   type;
    bl_arg_t    arg;
    bl_var_t    var;
    bl_expr_t   expr;
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

bl_node_t *
bl_ast_func_push_arg(bl_func_t *func, bl_node_t *arg);

size_t
bl_ast_func_arg_count(bl_func_t *func);

bl_node_t *
bl_ast_func_get_arg(bl_func_t *func, const size_t i);

bl_node_t *
bl_ast_block_push_node(bl_block_t *block, bl_node_t *node);

size_t
bl_ast_block_node_count(bl_block_t *block);

bl_node_t *
bl_ast_block_get_node(bl_block_t *block, const size_t i);

#endif // BL_NODE2_IMPL_H
