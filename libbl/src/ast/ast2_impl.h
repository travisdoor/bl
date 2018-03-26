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
#define BL_FUND_TYPE_LIST                                                                              \
 ft(VOID, "void") \
 ft(I8, "i8") \
 ft(I32, "i32") \
 ft(I64, "i64") \
 ft(U8, "u8") \
 ft(U32, "u32") \
 ft(U64, "u64") \
 ft(F32, "f32") \
 ft(F64, "f64") \
 ft(CHAR, "char") \
 ft(STRING, "string") \
 ft(BOOL, "bool")

// clang-format on

typedef enum {
#define ft(tok, str) BL_FTYPE_##tok,
  BL_FUND_TYPE_LIST
#undef ft
} bl_fund_type_e;

static const char *bl_fund_type_strings[] = {
#define ft(tok, str) str,
    BL_FUND_TYPE_LIST
#undef ft
};

typedef struct bl_ast bl_ast_t;

#define bl_peek_src(node) (node)->src

#define bl_node_is_stmt(node) ((node)->node_variant == BL_NODE_STMT)
#define bl_node_is_decl(node) ((node)->node_variant == BL_NODE_DECL)
#define bl_node_is_type(node) ((node)->node_variant == BL_NODE_TYPE)
#define bl_node_is_expr(node) ((node)->node_variant == BL_NODE_EXPR)

#define bl_peek_type(node) (&(node)->n.type)
#define bl_peek_stmt(node) (&(node)->n.stmt)
#define bl_peek_expr(node) (&(node)->n.expr)
#define bl_peek_decl(node) (&(node)->n.decl)

#define bl_peek_type_fund(node) (&(node)->n.type.type.fund)
#define bl_peek_type_ref(node) (&(node)->n.type.type.ref)

#define bl_peek_expr_const(node) (&(node)->n.expr.expr.const_expr)
#define bl_peek_expr_binop(node) (&(node)->n.expr.expr.binop)
#define bl_peek_expr_var_ref(node) (&(node)->n.expr.expr.var_ref)
#define bl_peek_expr_call(node) (&(node)->n.expr.expr.call)
#define bl_peek_expr_path(node) (&(node)->n.expr.expr.path)

#define bl_peek_decl_module(node) (&(node)->n.decl.decl.module)
#define bl_peek_decl_var(node) (&(node)->n.decl.decl.var)
#define bl_peek_decl_arg(node) (&(node)->n.decl.decl.arg)
#define bl_peek_decl_func(node) (&(node)->n.decl.decl.func)
#define bl_peek_decl_struct(node) (&(node)->n.decl.decl.strct)
#define bl_peek_decl_enum(node) (&(node)->n.decl.decl.enm)
#define bl_peek_decl_block(node) (&(node)->n.decl.decl.block)

#define bl_peek_stmt_if(node) (&(node)->n.stmt.stmt.if_stmt)

typedef struct bl_node bl_node_t;

typedef struct bl_decl bl_decl_t;
typedef struct bl_stmt bl_stmt_t;
typedef struct bl_type bl_type_t;
typedef struct bl_expr bl_expr_t;

typedef enum bl_node_variant bl_node_variant_e;
typedef enum bl_decl_variant bl_decl_variant_e;
typedef enum bl_stmt_variant bl_stmt_variant_e;
typedef enum bl_expr_variant bl_expr_variant_e;
typedef enum bl_type_variant bl_type_variant_e;

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

enum bl_type_variant
{
  BL_TYPE_FUND,
  BL_TYPE_REF
};

struct bl_type
{
  bl_type_variant_e type_variant;

  union
  {
    struct
    {
      bl_fund_type_e type;
    } fund;

    struct
    {
      bl_id_t    id;
      bl_node_t *ref;
    } ref;
  } type;
};

/*
 * expr node
 */
enum bl_expr_variant
{
  BL_EXPR_CONST,
  BL_EXPR_BINOP,
  BL_EXPR_VAR_REF,
  BL_EXPR_CALL,
  BL_EXPR_PATH
};

struct bl_expr
{
  bl_expr_variant_e expr_variant;

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

    struct
    {
      bl_id_t    id;
      bl_node_t *ref;
    } var_ref;

    struct
    {
      bl_id_t    id;
      bl_node_t *ref;
      BArray *   args;
    } call;

    struct
    {
      bl_id_t    id;
      bl_node_t *ref;
      bl_node_t *next;
    } path;

  } expr;
};

enum bl_decl_variant
{
  BL_DECL_MODULE,
  BL_DECL_VAR,
  BL_DECL_ARG,
  BL_DECL_FUNC,
  BL_DECL_STRUCT,
  BL_DECL_ENUM,
  BL_DECL_BLOCK
};

struct bl_decl
{
  bl_decl_variant_e decl_variant;

  union
  {
    /*
     * module node
     */
    struct
    {
      bl_id_t id;
      BArray *nodes;
    } module;

    /*
     * var declaration
     */
    struct
    {
      bl_id_t    id;
      bl_node_t *type;
      bl_node_t *init_expr;
    } var;

    /*
     * func argument
     */
    struct
    {
      bl_id_t    id;
      bl_node_t *type;
    } arg;

    /*
     * function declaration node
     */
    struct
    {
      bl_id_t    id;
      BArray *   args;
      bl_node_t *block;
      bl_node_t *ret_type;
    } func;

    /*
     * structure declaration node
     */
    struct
    {
      bl_id_t id;
    } strct;

    /*
     * enum declaration node
     */
    struct
    {
      bl_id_t id;
    } enm;

    /*
     * block declaration node
     */
    struct
    {
      BArray *nodes;
    } block;
  } decl;
};

enum bl_stmt_variant
{
  BL_STMT_IF
};

struct bl_stmt
{
  bl_stmt_variant_e stmt_variant;
  union
  {
    /*
     * if statement
     */
    struct
    {
      bl_node_t *test;
      bl_node_t *true_stmt;
      bl_node_t *false_stmt;
    } if_stmt;
  } stmt;
};

/*
 * base node union type
 * this node is actually allocated
 */

enum bl_node_variant
{
  BL_NODE_DECL,
  BL_NODE_STMT,
  BL_NODE_TYPE,
  BL_NODE_EXPR
};

struct bl_node
{
  bl_src_t *        src;
  bl_node_variant_e node_variant;

  union
  {
    bl_type_t type;
    bl_stmt_t stmt;
    bl_decl_t decl;
    bl_expr_t expr;
  } n;
};

void
bl_ast_init(bl_ast_t *ast);

void
bl_ast_terminate(bl_ast_t *ast);

/*
 * constructors
 */
bl_node_t *
bl_ast_add_type_fund(bl_ast_t *ast, bl_token_t *tok, bl_fund_type_e t);

bl_node_t *
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref);

bl_node_t *
bl_ast_add_expr_const_char(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, char c);

bl_node_t *
bl_ast_add_expr_const_bool(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, bool b);

bl_node_t *
bl_ast_add_expr_const_signed(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, long long s);

bl_node_t *
bl_ast_add_expr_const_unsigned(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type,
                               unsigned long long u);

bl_node_t *
bl_ast_add_expr_const_double(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, double f);

bl_node_t *
bl_ast_add_expr_const_str(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, const char *str);

bl_node_t *
bl_ast_add_expr_binop(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *lhs, bl_node_t *rhs);

bl_node_t *
bl_ast_add_expr_var_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref);

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref);

bl_node_t *
bl_ast_add_expr_path(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref,
                     bl_node_t *next);

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_token_t *tok, const char *name);

bl_node_t *
bl_ast_add_decl_var(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr);

bl_node_t *
bl_ast_add_decl_arg(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type);

bl_node_t *
bl_ast_add_decl_func(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *block,
                     bl_node_t *ret_type);

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name);

bl_node_t *
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name);

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok);

bl_node_t *
bl_ast_add_stmt_if(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                   bl_node_t *false_stmt);

/*
 * helpers
 */
bl_node_t *
bl_ast_module_push_node(bl_node_t *module, bl_node_t *node);

size_t
bl_ast_module_node_count(bl_node_t *module);

bl_node_t *
bl_ast_module_get_node(bl_node_t *module, const size_t i);

bl_node_t *
bl_ast_func_push_arg(bl_node_t *func, bl_node_t *arg);

size_t
bl_ast_func_arg_count(bl_node_t *func);

bl_node_t *
bl_ast_func_get_arg(bl_node_t *func, const size_t i);

bl_node_t *
bl_ast_block_push_node(bl_node_t *block, bl_node_t *node);

size_t
bl_ast_block_node_count(bl_node_t *block);

bl_node_t *
bl_ast_block_get_node(bl_node_t *block, const size_t i);

bl_node_t *
bl_ast_call_push_arg(bl_node_t *call, bl_node_t *node);

size_t
bl_ast_call_arg_count(bl_node_t *call);

bl_node_t *
bl_ast_call_get_arg(bl_node_t *call, const size_t i);

#endif // BL_NODE2_IMPL_H
