//*****************************************************************************
// bl
//
// File:   node_impl.h
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

#ifndef BL_NODE_IMPL_H
#define BL_NODE_IMPL_H

#include <bobject/containers/array.h>
#include "token_impl.h"
#include "identifier_impl.h"
#include "type_impl.h"

#define BL_NTYPE_LIST\
  nt(GLOBAL_STMT, "global_stmt") \
  nt(CMP_STMT, "compound_stmt") \
  nt(IF_STMT, "if_statement") \
  nt(BREAK_STMT, "break_stmt") \
  nt(RETURN_STMT, "return_stmt") \
  nt(LOOP_STMT, "loop_stmt") \
  nt(CONTINUE_STMT, "continue_stmt") \
  nt(ENUM_DECL, "enum_decl") \
  nt(FUNC_DECL, "func_decl") \
  nt(VAR_DECL, "var_decl") \
  nt(PARAM_VAR_DECL, "param_var_decl") \
  nt(CALL_EXPR, "call_expr") \
  nt(DECL_REF_EXPR, "decl_ref_expr") \
  nt(CONST_EXPR, "const_expr") \
  nt(BINOP, "binary_operation") \

typedef enum
{
#define nt(tok, str) BL_NODE_##tok,
  BL_NTYPE_LIST
#undef nt
} bl_node_type_e;

typedef enum _bl_node_conts_type_e
{
  BL_CONST_INT,
  BL_CONST_LONG,
  BL_CONST_ULONG,
  BL_CONST_BOOL,
  BL_CONST_STRING,
  BL_CONST_CHAR,
  BL_CONST_DOUBLE,
  BL_CONST_FLOAT
} bl_node_conts_type_e;

/*
 * Statements
 */
typedef struct bl_node_cmp_stmt
{
  BArray *nodes;
} bl_node_cmp_stmt_t;

typedef struct bl_node_glob_stmt
{
  BArray *nodes;
} bl_node_glob_stmt_t;

typedef struct bl_node_break_stmt
{
} bl_node_break_stmt_t;

typedef struct bl_node_continue_stmt
{
} bl_node_continue_stmt_t;

typedef struct bl_node_call_stmt
{
  bl_ident_t ident;
  struct bl_node *callee;
  BArray *args;
} bl_node_call_stmt_t;

typedef struct bl_node_loop_stmt
{
  struct bl_node *cmp_stmt;
} bl_node_loop_stmt_t;

typedef struct bl_node_return_stmt
{
  struct bl_node *expr;
} bl_node_return_stmt_t;

typedef struct bl_node_if_stmt
{
  struct bl_node *expr;
  struct bl_node *then_stmt;
  struct bl_node *else_stmt;
  struct bl_node *else_if_stmt;
} bl_node_if_stmt_t;

/*
 * Declarations
 */
/* TODO: don't use base */
typedef struct bl_node_decl
{
  bl_sym_e modificator;
  bl_ident_t ident;
  bl_type_t type;
} bl_node_decl_t;

typedef struct bl_node_func_decl
{
  bl_node_decl_t base;
  BArray *params;
  struct bl_node *cmp_stmt;
} bl_node_func_decl_t;

typedef struct bl_node_var_decl
{
  bl_node_decl_t base;
  struct bl_node *expr;
} bl_node_var_decl_t;

typedef struct bl_node_param_var_decl
{
  bl_node_decl_t base;
} bl_node_param_var_decl_t;

typedef struct bl_node_enum_decl
{
  bl_node_decl_t base;
  BArray *constants;
} bl_node_enum_decl_t;

/*
 * Expressions
 */
typedef struct bl_node_const_expr
{
  bl_node_conts_type_e type;
  union
  {
    const char *as_string;
    char as_char;
    unsigned long as_ulong;
    double as_double;
    float as_float;
    bool as_bool;
  } value;
} bl_node_const_expr_t;

typedef struct bl_node_decl_ref_expr
{
  bl_ident_t ident;
} bl_node_decl_ref_expr_t;

/*
 * Other
 */
typedef struct bl_node_binop
{
  bl_sym_e operator;

  struct bl_node *lhs;
  struct bl_node *rhs;
} bl_node_binop_t;

/*
 * Node base
 */
typedef struct bl_node
{
  bl_node_type_e type;

  const char *generated_from;
  int line;
  int col;

  union
  {
    bl_node_cmp_stmt_t cmp_stmt;
    bl_node_glob_stmt_t glob_stmt;
    bl_node_break_stmt_t break_stmt;
    bl_node_continue_stmt_t continue_stmt;
    bl_node_loop_stmt_t loop_stmt;
    bl_node_return_stmt_t return_stmt;
    bl_node_if_stmt_t if_stmt;

    bl_node_decl_t decl;
    bl_node_func_decl_t func_decl;
    bl_node_var_decl_t var_decl;
    bl_node_param_var_decl_t param_var_decl;
    bl_node_enum_decl_t enum_decl;

    bl_node_const_expr_t const_expr;
    bl_node_decl_ref_expr_t decl_ref_expr;
    bl_node_call_stmt_t call_expr;

    bl_node_binop_t binop;
  } value;
} bl_node_t;

void
bl_node_init(bl_node_t *node,
             bl_node_type_e type,
             const char *generated_from,
             int line,
             int col);

void
bl_node_terminate(bl_node_t *node);

/* helper functions */
const char *
bl_node_to_str(bl_node_t *node);

/* TODO: direct access ??? */
bl_node_t *
bl_node_glob_stmt_add_child(bl_node_t *node,
                            bl_node_t *child);

int
bl_node_glob_stmt_get_children_count(bl_node_t *node);

bl_node_t *
bl_node_glob_stmt_get_child(bl_node_t *node,
                            int i);

bl_node_t *
bl_node_cmp_stmt_add_child(bl_node_t *node,
                           bl_node_t *child);

int
bl_node_cmp_stmt_get_children_count(bl_node_t *node);

bl_node_t *
bl_node_cmp_stmt_get_child(bl_node_t *node,
                           int i);

bl_node_t *
bl_node_func_decl_stmt_add_param(bl_node_t *node,
                                 bl_node_t *param);

int
bl_node_func_decl_get_param_count(bl_node_t *node);

bl_node_t *
bl_node_func_decl_get_param(bl_node_t *node,
                            int i);

bl_node_t *
bl_node_call_expr_add_arg(bl_node_t *node,
                          bl_node_t *arg);

int
bl_node_call_expr_get_arg_count(bl_node_t *node);

bl_node_t *
bl_node_call_expr_get_arg(bl_node_t *node,
                          int i);

bl_node_t *
bl_node_enum_decl_add_const(bl_node_t *node,
                            bl_node_t *c);

int
bl_node_enum_decl_get_const_count(bl_node_t *node);

bl_node_t *
bl_node_enum_decl_get_conts(bl_node_t *node,
                            int i);

#endif //BL_NODE_IMPL_H
