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
  nt(STMT, "stmt") \
  nt(DECL, "decl") \
  nt(EXPR, "expr") \
  nt(PATH, "path") \
  nt(BINOP, "binop") \
  nt(CALL, "call") \
  nt(IF, "if") \
  nt(VAR_REF, "var_ref") \
  nt(CONST_EXPR, "constant") \
  nt(TYPE, "type")

// clang-format on

typedef struct bl_ast2 bl_ast2_t;

typedef struct bl_src         bl_src_t;
typedef struct bl_path        bl_path_t;
typedef struct bl_node        bl_node_t;
typedef struct bl_type        bl_type_t;
typedef struct bl_module      bl_module_t;
typedef struct bl_func_decl   bl_func_decl_t;
typedef struct bl_struct_decl bl_struct_decl_t;
typedef struct bl_enum_decl   bl_enum_decl_t;
typedef struct bl_arg         bl_arg_t;
typedef struct bl_block       bl_block_t;
typedef struct bl_item        bl_item_t;
typedef struct bl_stmt        bl_stmt_t;
typedef struct bl_decl        bl_decl_t;
typedef struct bl_expr        bl_expr_t;
typedef struct bl_const_expr  bl_const_expr_t;
typedef struct bl_binop       bl_binop_t;
typedef struct bl_call        bl_call_t;
typedef struct bl_var_ref     bl_var_ref_t;
typedef struct bl_if          bl_if_t;

typedef enum bl_fund_type bl_fund_type_e;

struct bl_src
{
  int         line;
  int         col;
  const char *file;
};

typedef enum {
#define nt(tok, str) BL_NODE_##tok,
  BL_NTYPE_LIST
#undef nt
} bl_node_e;

struct bl_node
{
  bl_node_e t;
  bl_src_t  src;
};

struct bl_path
{
  bl_node_t    base_;
  bl_id_t      id;
  bl_module_t *ref;

  enum
  {
    BL_PATH_PATH,
    BL_PATH_CALL,
    BL_PATH_VAR_REF
  } t;

  union
  {
    bl_path_t *   path;
    bl_call_t *   call;
    bl_var_ref_t *var_ref;
  } next;
};

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
  bl_node_t base_;
  bl_id_t   id;

  enum
  {
    BL_TYPE_FUND,
    BL_TYPE_STRUCT,
    BL_TYPE_ENUM,
    BL_TYPE_UNKNOWN
  } t;

  union
  {
    bl_fund_type_e    fund;
    bl_struct_decl_t *strct;
    bl_enum_decl_t *  enm;
  } type;
};

struct bl_module
{
  bl_node_t base_;
  BArray *  items;
};

struct bl_func_decl
{
  bl_node_t  base_;
  BArray *   params;
  bl_type_t *ret;
};

struct bl_struct_decl
{
  bl_node_t base_;
};

struct bl_enum_decl
{
  bl_node_t base_;
};

struct bl_arg
{
  bl_node_t  base_;
  bl_id_t    id;
  bl_type_t *type;
};

struct bl_block
{
  bl_node_t base_;
  BArray *  stmts;
};

struct bl_item
{
  bl_node_t base_;
  enum
  {
    BL_ITEM_MODULE,
    BL_ITEM_FUNC,
    BL_ITEM_STRUCT,
    BL_ITEM_ENUM,
    BL_ITEM_EXTERN,
  } t;

  bl_id_t id;

  union
  {
    bl_module_t *     module;
    bl_struct_decl_t *struct_decl;
    bl_enum_decl_t *  enum_decl;

    struct
    {
      bl_func_decl_t *func_decl;
      bl_block_t *    block;
    } func;

    struct
    {
      bl_func_decl_t *func_decl;
    } extern_func;
  } node;
};

struct bl_decl
{
  bl_node_t  base_;
  bl_id_t    id;
  bl_type_t *type;
  bl_expr_t *init_expr;
};

struct bl_const_expr
{
  bl_node_t  base_;
  bl_type_t *type;

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

struct bl_binop
{
  bl_node_t  base_;
  bl_sym_e   op;
  bl_expr_t *lhs;
  bl_expr_t *rhs;
};

struct bl_call
{
  bl_node_t       base_;
  bl_id_t         id;
  bl_func_decl_t *callee;
  BArray *        args;
};

struct bl_var_ref
{
  bl_node_t  base_;
  bl_id_t    id;
  bl_decl_t *ref;
};

struct bl_expr
{
  bl_node_t base_;

  enum
  {
    BL_EXPR_CONST,
    BL_EXPR_BINOP,
    BL_EXPR_NESTED,
    BL_EXPR_CALL,
    BL_EXPR_VAR_REF,
    BL_EXPR_PATH
  } t;

  union
  {
    bl_const_expr_t *cnst;
    bl_binop_t *     binop;
    bl_expr_t *      nested;
    bl_call_t *      call;
    bl_var_ref_t *   var_ref;
    bl_path_t *      path;
  } expr;
};

struct bl_stmt
{
  bl_node_t base_;

  enum
  {
    BL_STMT_DECL,
    BL_STMT_EXPR,
    BL_STMT_BLOCK,
    BL_STMT_IF
  } t;

  union
  {
    bl_decl_t * decl;
    bl_expr_t * expr;
    bl_block_t *block;
    bl_if_t *   if_stmt;
  } stmt;
};

struct bl_if
{
  bl_node_t base_;
};

struct bl_ast2
{
  BArray *     nodes;
  bl_module_t *root;
};

void
bl_src_init(bl_src_t *src, bl_token_t *tok);

void
bl_ast2_init(bl_ast2_t *ast);

void
bl_ast2_terminate(bl_ast2_t *ast);

bl_node_t *
_bl_ast2_new_node(bl_ast2_t *ast, bl_node_e type, bl_token_t *tok);

const char *
bl_ast2_node_to_str(bl_node_t *node);

#define bl_ast2_new_node(ast, nt, tok, t) (t *)_bl_ast2_new_node((ast), (nt), (tok));

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

bl_stmt_t *
bl_ast_block_push_stmt(bl_block_t *block, bl_stmt_t *stmt);

size_t
bl_ast_block_stmt_count(bl_block_t *block);

bl_stmt_t *
bl_ast_block_get_stmt(bl_block_t *block, size_t i);

bl_expr_t *
bl_ast_call_push_arg(bl_call_t *call, bl_expr_t *expr);

size_t
bl_ast_call_arg_count(bl_call_t *call);

bl_expr_t *
bl_ast_call_get_arg(bl_call_t *call, size_t i);

#endif // BL_NODE2_IMPL_H
