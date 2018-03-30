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
#include <bobject/containers/htbl.h>
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
#define bl_node_is(node, c) ((node)->code == (c))
#define bl_node_code(node) (node)->code

#define bl_peek_type_fund(node) (&(node)->n.type_fund)
#define bl_peek_type_ref(node) (&(node)->n.type_ref)

#define bl_peek_expr_const(node) (&(node)->n.expr_const)
#define bl_peek_expr_binop(node) (&(node)->n.expr_binop)
#define bl_peek_expr_var_ref(node) (&(node)->n.expr_var_ref)
#define bl_peek_expr_call(node) (&(node)->n.expr_call)
#define bl_peek_expr_path(node) (&(node)->n.expr_path)

#define bl_peek_decl_module(node) (&(node)->n.decl_module)
#define bl_peek_decl_var(node) (&(node)->n.decl_var)
#define bl_peek_decl_arg(node) (&(node)->n.decl_arg)
#define bl_peek_decl_func(node) (&(node)->n.decl_func)
#define bl_peek_decl_struct(node) (&(node)->n.decl_struct)
#define bl_peek_decl_enum(node) (&(node)->n.decl_enum)
#define bl_peek_decl_block(node) (&(node)->n.decl_block)

#define bl_peek_stmt_if(node) (&(node)->n.stmt_if)
#define bl_peek_stmt_loop(node) (&(node)->n.stmt_loop)
#define bl_peek_stmt_while(node) (&(node)->n.stmt_while)
#define bl_peek_stmt_return(node) (&(node)->n.stmt_return)

typedef struct bl_node          bl_node_t;
typedef struct bl_stmt_if       bl_stmt_if_t;
typedef struct bl_stmt_loop     bl_stmt_loop_t;
typedef struct bl_stmt_while    bl_stmt_while_t;
typedef struct bl_stmt_break    bl_stmt_break_t;
typedef struct bl_stmt_continue bl_stmt_continue_t;
typedef struct bl_stmt_return   bl_stmt_return_t;

typedef struct bl_decl_module bl_decl_module_t;
typedef struct bl_decl_var    bl_decl_var_t;
typedef struct bl_decl_arg    bl_decl_arg_t;
typedef struct bl_decl_func   bl_decl_func_t;
typedef struct bl_decl_struct bl_decl_struct_t;
typedef struct bl_decl_enum   bl_decl_enum_t;
typedef struct bl_decl_block  bl_decl_block_t;

typedef struct bl_expr_const   bl_expr_const_t;
typedef struct bl_expr_binop   bl_expr_binop_t;
typedef struct bl_expr_var_ref bl_expr_var_ref_t;
typedef struct bl_expr_call    bl_expr_call_t;
typedef struct bl_expr_path    bl_expr_path_t;

typedef struct bl_type_ref  bl_type_ref_t;
typedef struct bl_type_fund bl_type_fund_t;

typedef enum bl_node_code bl_node_code_e;
typedef enum bl_modif     bl_modif_e;

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

enum bl_node_code
{
  BL_STMT_IF,
  BL_STMT_LOOP,
  BL_STMT_WHILE,
  BL_STMT_BREAK,
  BL_STMT_CONTINUE,
  BL_STMT_RETURN,

  BL_DECL_MODULE,
  BL_DECL_VAR,
  BL_DECL_ARG,
  BL_DECL_FUNC,
  BL_DECL_STRUCT,
  BL_DECL_ENUM,
  BL_DECL_BLOCK,

  BL_EXPR_CONST,
  BL_EXPR_BINOP,
  BL_EXPR_VAR_REF,
  BL_EXPR_CALL,
  BL_EXPR_PATH,

  BL_TYPE_FUND,
  BL_TYPE_REF
};

enum bl_modif
{
  BL_MODIF_NONE   = 0,
  BL_MODIF_PUBLIC = 1,
  BL_MODIF_EXTERN = 2
};

struct bl_stmt_if
{
  bl_node_t *test;
  bl_node_t *true_stmt;
  bl_node_t *false_stmt;
};

struct bl_stmt_loop
{
  bl_node_t *true_stmt;
};

struct bl_stmt_while
{
  bl_node_t *test;
  bl_node_t *true_stmt;
};

struct bl_stmt_break
{};

struct bl_stmt_continue
{};

struct bl_stmt_return
{
  bl_node_t *expr;
};

struct bl_decl_module
{
  bl_id_t          id;
  bl_node_t *      parent;
  BArray *         nodes;
  struct bl_scope *scope;
};

struct bl_decl_var
{
  bl_id_t    id;
  bl_node_t *type;
  bl_node_t *init_expr;
};

struct bl_decl_arg
{
  bl_id_t    id;
  bl_node_t *type;
};

struct bl_decl_func
{
  bl_id_t    id;
  int        modif;
  BArray *   args;
  bl_node_t *block;
  bl_node_t *ret_type;
};

struct bl_decl_struct
{
  bl_id_t id;
  int     modif;
};

struct bl_decl_enum
{
  bl_id_t id;
  int     modif;
};

struct bl_decl_block
{
  BArray *nodes;
};

struct bl_expr_const
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
};

struct bl_expr_binop
{
  bl_sym_e   op;
  bl_node_t *lhs;
  bl_node_t *rhs;
};

struct bl_expr_var_ref
{
  bl_id_t    id;
  bl_node_t *ref;
};

struct bl_expr_call
{
  bl_id_t    id;
  bl_node_t *ref;
  BArray *   path;
  BArray *   args;
};

struct bl_expr_path
{
  bl_id_t    id;
  bl_node_t *ref;
  bl_node_t *next;
};

struct bl_type_fund
{
  bl_fund_type_e type;
};

struct bl_type_ref
{
  bl_id_t    id;
  bl_node_t *ref;
};

struct bl_node
{
  bl_src_t *     src;
  bl_node_code_e code;

  union
  {
    bl_stmt_if_t       stmt_if;
    bl_stmt_loop_t     stmt_loop;
    bl_stmt_while_t    stmt_while;
    bl_stmt_break_t    stmt_break;
    bl_stmt_continue_t stmt_continue;
    bl_stmt_return_t   stmt_return;

    bl_decl_module_t decl_module;
    bl_decl_var_t    decl_var;
    bl_decl_arg_t    decl_arg;
    bl_decl_func_t   decl_func;
    bl_decl_struct_t decl_struct;
    bl_decl_enum_t   decl_enum;
    bl_decl_block_t  decl_block;

    bl_expr_const_t   expr_const;
    bl_expr_binop_t   expr_binop;
    bl_expr_var_ref_t expr_var_ref;
    bl_expr_call_t    expr_call;
    bl_expr_path_t    expr_path;

    bl_type_ref_t  type_ref;
    bl_type_fund_t type_fund;
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
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref, BArray *path);

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
                     bl_node_t *ret_type, int modif);

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif);

bl_node_t *
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif);

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok);

bl_node_t *
bl_ast_add_stmt_if(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                   bl_node_t *false_stmt);

bl_node_t *
bl_ast_add_stmt_loop(bl_ast_t *ast, bl_token_t *tok, bl_node_t *true_stmt);

bl_node_t *
bl_ast_add_stmt_while(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt);

bl_node_t *
bl_ast_add_stmt_break(bl_ast_t *ast, bl_token_t *tok);

bl_node_t *
bl_ast_add_stmt_continue(bl_ast_t *ast, bl_token_t *tok);

bl_node_t *
bl_ast_add_stmt_return(bl_ast_t *ast, bl_token_t *tok, bl_node_t *expr);

/*
 * helpers
 */

/* module */
/**************************************************************************************************/
bl_node_t *
bl_ast_module_push_node(bl_node_t *module, bl_node_t *node);

size_t
bl_ast_module_node_count(bl_node_t *module);

bl_node_t *
bl_ast_module_get_node(bl_node_t *module, size_t i);
/**************************************************************************************************/

/* function */
/**************************************************************************************************/

bl_node_t *
bl_ast_func_push_arg(bl_node_t *func, bl_node_t *arg);

size_t
bl_ast_func_arg_count(bl_node_t *func);

bl_node_t *
bl_ast_func_get_arg(bl_node_t *func, const size_t i);
/**************************************************************************************************/

/* block */
/**************************************************************************************************/
bl_node_t *
bl_ast_block_push_node(bl_node_t *block, bl_node_t *node);

size_t
bl_ast_block_node_count(bl_node_t *block);

bl_node_t *
bl_ast_block_get_node(bl_node_t *block, const size_t i);

/* call */
/**************************************************************************************************/
bl_node_t *
bl_ast_call_push_arg(bl_node_t *call, bl_node_t *node);

size_t
bl_ast_call_arg_count(bl_node_t *call);

bl_node_t *
bl_ast_call_get_arg(bl_node_t *call, const size_t i);
/**************************************************************************************************/

/* other */
/**************************************************************************************************/
bl_id_t *
bl_ast_try_get_id(bl_node_t *node);
/**************************************************************************************************/

#endif // BL_NODE2_IMPL_H
