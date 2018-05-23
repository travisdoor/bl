//************************************************************************************************
// bl
//
// File:   ast_impl.h
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
//************************************************************************************************

#ifndef BL_AST2_IMPL_H
#define BL_AST2_IMPL_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include "id_impl.h"
#include "token_impl.h"
#include "scope_impl.h"
#include "common_impl.h"

// clang-format off
#define BL_FUND_TYPE_LIST                                                                              \
    ft(VOID,   "void") \
    ft(I8,     "i8") \
    ft(I16,    "i16") \
    ft(I32,    "i32") \
    ft(I64,    "i64") \
    ft(U8,     "u8") \
    ft(U16,    "u16") \
    ft(U32,    "u32") \
    ft(U64,    "u64") \
    ft(SIZE,   "size_t") \
    ft(F32,    "f32") \
    ft(F64,    "f64") \
    ft(CHAR,   "char") \
    ft(PTR,    "ptr") \
    ft(STRING, "string") \
    ft(BOOL,   "bool")

#define BL_NODE_TYPE_LIST \
    nt(STMT_IF,            stmt_if) \
    nt(STMT_LOOP,          stmt_loop) \
    nt(STMT_BREAK,         stmt_break) \
    nt(STMT_CONTINUE,      stmt_continue) \
    nt(STMT_RETURN,        stmt_return) \
    nt(STMT_USING,         stmt_using) \
    nt(DECL_MODULE,        decl_module) \
    nt(DECL_VAR,           decl_var) \
    nt(DECL_CONST,         decl_const) \
    nt(DECL_FUNC,          decl_func) \
    nt(DECL_ARG,           decl_arg) \
    nt(DECL_STRUCT,        decl_struct) \
    nt(DECL_STRUCT_MEMBER, decl_struct_member) \
    nt(DECL_ENUM,          decl_enum) \
    nt(DECL_ENUM_VARIANT,  decl_enum_variant) \
    nt(DECL_BLOCK,         decl_block) \
    nt(EXPR_CONST,         expr_const) \
    nt(EXPR_BINOP,         expr_binop) \
    nt(EXPR_UNARY,         expr_unary) \
    nt(EXPR_DECL_REF,      expr_decl_ref) \
    nt(EXPR_MEMBER_REF,    expr_member_ref) \
    nt(EXPR_ARRAY_REF,     expr_array_ref) \
    nt(EXPR_CALL,          expr_call) \
    nt(EXPR_SIZEOF,        expr_sizeof) \
    nt(TYPE_FUND,          type_fund) \
    nt(TYPE_REF,           type_ref) \
    nt(PATH_ELEM,          path_elem) \
    nt(PRE_LOAD,           pre_load)

// clang-format on

typedef struct bl_ast     bl_ast_t;
typedef struct bl_node    bl_node_t;
typedef enum bl_node_code bl_node_code_e;
typedef enum bl_modif     bl_modif_e;

typedef enum
{
#define ft(tok, str) BL_FTYPE_##tok,
  BL_FUND_TYPE_LIST
#undef ft
      BL_FUND_TYPE_COUNT
} bl_fund_type_e;

extern const char *bl_fund_type_strings[];
extern const char *bl_node_type_strings[];

/*************************************************************************************************
 * generation of node typedefs and code enum
 *************************************************************************************************/
#define nt(code, name) typedef struct bl_##name bl_##name##_t;
BL_NODE_TYPE_LIST
#undef nt

enum bl_node_code
{
#define nt(code, name) BL_##code,
  BL_NODE_TYPE_LIST
#undef nt
      BL_NODE_COUNT
};

#define bl_peek_src(n) (n)->src
#define bl_node_is(n, c) ((n)->code == (c))
#define bl_node_is_not(n, c) ((n)->code != (c))
#define bl_node_code(n) (n)->code
#define bl_node_name(n) bl_node_type_strings[(n)->code]

struct bl_ast
{
  bl_node_t *root;
  bl_node_t *entry_func;
  BArray *   nodes;
};

enum bl_modif
{
  BL_MODIF_NONE   = 0,
  BL_MODIF_PUBLIC = 1,
  BL_MODIF_EXTERN = 2,
  BL_MODIF_EXPORT = 4
};

/*************************************************************************************************
 * definition of the node structure bodies
 *************************************************************************************************/

/* if statement */
struct bl_stmt_if
{
  bl_node_t *parent;
  bl_node_t *test;
  bl_node_t *true_stmt;
  bl_node_t *false_stmt;
};

/* loop statement */
struct bl_stmt_loop
{
  bl_node_t *parent;
  bl_node_t *test;
  bl_node_t *true_stmt;
};

/* break inside loops */
struct bl_stmt_break
{
  void *_dummy;
};

/* continue inside loops */
struct bl_stmt_continue
{
  void *_dummy;
};

/* function return statement */
struct bl_stmt_return
{
  bl_node_t *expr;
};

struct bl_stmt_using
{
  BArray *   path;
  bl_node_t *ref;
};

struct bl_expr_sizeof
{
  bl_node_t *type;
};

/* module declaration */
struct bl_decl_module
{
  bl_id_t     id;
  bl_node_t * parent;
  int         modif;
  BArray *    nodes;
  bl_scopes_t scopes;
};

/* variable declaration */
struct bl_decl_var
{
  bl_id_t    id;
  int        modif;
  bl_node_t *type;
  bl_node_t *init_expr;
  int        used;
};

struct bl_decl_const
{
  bl_id_t    id;
  int        modif;
  bl_node_t *type;
  bl_node_t *init_expr;
  int        used;
};

struct bl_decl_arg
{
  bl_id_t    id;
  bl_node_t *type;
};

struct bl_decl_func
{
  bl_id_t     id;
  bl_node_t * parent;
  int         modif;
  int         used;
  BArray *    args;
  bl_node_t * block;
  bl_node_t * ret_type;
  bl_scopes_t scopes;
};

struct bl_decl_struct
{
  bl_id_t     id;
  int         modif;
  int         used;
  BArray *    members;
  bl_scopes_t scopes;
};

struct bl_decl_struct_member
{
  bl_id_t    id;
  int        modif;
  bl_node_t *type;
  int        order;
  /*bl_node_t *init_expr;*/
};

struct bl_decl_enum
{
  bl_id_t     id;
  bl_node_t * parent;
  int         modif;
  int         used;
  bl_node_t * type;
  BArray *    variants;
  bl_scopes_t scopes;
};

struct bl_decl_enum_variant
{
  bl_id_t    id;
  bl_node_t *parent;
  bl_node_t *expr;
};

struct bl_decl_block
{
  bl_node_t * parent;
  BArray *    nodes;
  bl_scopes_t scopes;
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

struct bl_expr_cast
{
  bl_node_t *type;
  bl_node_t *expr;
};

struct bl_expr_binop
{
  bl_sym_e   op;
  bl_node_t *lhs;
  bl_node_t *rhs;
  bl_node_t *type;
};

struct bl_expr_unary
{
  bl_sym_e   op;
  bl_node_t *next;
};

struct bl_expr_decl_ref
{
  BArray *   path;
  bl_node_t *ref;
};

struct bl_expr_member_ref
{
  bl_id_t    id;
  bl_node_t *ref;
  bl_node_t *next;
};

struct bl_expr_array_ref
{
  bl_node_t *index;
  bl_node_t *next;
};

struct bl_expr_call
{
  bl_node_t *ref;
  BArray *   path;
  BArray *   args;
};

struct bl_path_elem
{
  bl_id_t id;
};

struct bl_type_fund
{
  bl_fund_type_e type;
  /* dimensions are used when type is array */
  BArray *dims;
  bool    is_ptr;
};

struct bl_type_ref
{
  BArray *   path;
  bl_node_t *ref;
  /* dimensions are used when type is array */
  BArray *dims;
  bool    is_ptr;
};

struct bl_pre_load
{
  /* use id? */
  const char *filepath;
};

struct bl_node
{
  bl_src_t *     src;
  bl_node_code_e code;

  union
  {
#define nt(code, name) bl_##name##_t name;
    BL_NODE_TYPE_LIST
#undef nt
  } n;
};

void
bl_ast_init(bl_ast_t *ast);

void
bl_ast_terminate(bl_ast_t *ast);

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/
#define nt(code, name)                                                                             \
  static inline bl_##name##_t *bl_peek_##name(bl_node_t *n)                                        \
  {                                                                                                \
    bl_assert(bl_node_is(n, BL_##code), "invalid node type, expected: " #name " not %s",           \
              bl_node_name(n));                                                                    \
    return &(n->n.name);                                                                           \
  }
BL_NODE_TYPE_LIST
#undef nt

/*************************************************************************************************
 * constructors
 *************************************************************************************************/
bl_node_t *
bl_ast_add_type_fund(bl_ast_t *ast, bl_token_t *tok, bl_fund_type_e t, bool is_ptr);

bl_node_t *
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref, BArray *path,
                    bool is_ptr);

bl_node_t *
bl_ast_add_expr_sizeof(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type);

bl_node_t *
bl_ast_add_expr_const(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type);

bl_node_t *
bl_ast_add_pre_load(bl_ast_t *ast, bl_token_t *tok, const char *filepath);

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
bl_ast_add_expr_binop(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *lhs, bl_node_t *rhs,
                      bl_node_t *type);

bl_node_t *
bl_ast_add_expr_unary(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *next);

bl_node_t *
bl_ast_add_expr_decl_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, BArray *path);

bl_node_t *
bl_ast_add_expr_member_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *next);

bl_node_t *
bl_ast_add_expr_array_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *index, bl_node_t *next);

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, BArray *path);

bl_node_t *
bl_ast_add_path_elem(bl_ast_t *ast, bl_token_t *tok, const char *name);

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif,
                       bl_node_t *parent);

bl_node_t *
bl_ast_add_decl_var(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr, int modif);

bl_node_t *
bl_ast_add_decl_const(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                      bl_node_t *init_expr, int modif);

bl_node_t *
bl_ast_add_decl_arg(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type);

bl_node_t *
bl_ast_add_decl_func(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *block,
                     bl_node_t *ret_type, int modif, bl_node_t *parent);

bl_node_t *
bl_ast_add_decl_struct(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif);

bl_node_t *
bl_ast_add_decl_struct_member(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                              int modif);

bl_node_t *
bl_ast_add_decl_enum(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type, int modif,
                     bl_node_t *parent);

bl_node_t *
bl_ast_add_decl_enum_variant(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *expr,
                             bl_node_t *parent);

bl_node_t *
bl_ast_add_decl_block(bl_ast_t *ast, bl_token_t *tok, bl_node_t *parent);

bl_node_t *
bl_ast_add_stmt_if(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                   bl_node_t *false_stmt, bl_node_t *parent);

bl_node_t *
bl_ast_add_stmt_loop(bl_ast_t *ast, bl_token_t *tok, bl_node_t *test, bl_node_t *true_stmt,
                     bl_node_t *parent);

bl_node_t *
bl_ast_add_stmt_break(bl_ast_t *ast, bl_token_t *tok);

bl_node_t *
bl_ast_add_stmt_continue(bl_ast_t *ast, bl_token_t *tok);

bl_node_t *
bl_ast_add_stmt_return(bl_ast_t *ast, bl_token_t *tok, bl_node_t *expr);

bl_node_t *
bl_ast_add_stmt_using(bl_ast_t *ast, bl_token_t *tok, BArray *path);

/*************************************************************************************************
 * module
 *************************************************************************************************/
bl_node_t *
bl_ast_module_push_node(bl_decl_module_t *module, bl_node_t *node);

size_t
bl_ast_module_node_count(bl_decl_module_t *module);

bl_node_t *
bl_ast_module_get_node(bl_decl_module_t *module, size_t i);

/*************************************************************************************************
 * function
 *************************************************************************************************/
bl_node_t *
bl_ast_func_push_arg(bl_decl_func_t *func, bl_node_t *arg);

size_t
bl_ast_func_arg_count(bl_decl_func_t *func);

bl_node_t *
bl_ast_func_get_arg(bl_decl_func_t *func, const size_t i);

/*************************************************************************************************
 * block
 *************************************************************************************************/
bl_node_t *
bl_ast_block_push_node(bl_decl_block_t *block, bl_node_t *node);

size_t
bl_ast_block_node_count(bl_decl_block_t *block);

bl_node_t *
bl_ast_block_get_node(bl_decl_block_t *block, const size_t i);

/*************************************************************************************************
 * call
 *************************************************************************************************/
bl_node_t *
bl_ast_call_push_arg(bl_expr_call_t *call, bl_node_t *node);

size_t
bl_ast_call_arg_count(bl_expr_call_t *call);

bl_node_t *
bl_ast_call_get_arg(bl_expr_call_t *call, const size_t i);

/*************************************************************************************************
 * struct
 *************************************************************************************************/
bl_node_t *
bl_ast_struct_push_member(bl_decl_struct_t *strct, bl_node_t *member);

size_t
bl_ast_struct_member_count(bl_decl_struct_t *strct);

bl_node_t *
bl_ast_struct_get_member(bl_decl_struct_t *strct, const size_t i);

/*************************************************************************************************
 * enum
 *************************************************************************************************/
bl_node_t *
bl_ast_enum_push_variant(bl_decl_enum_t *enm, bl_node_t *variant);

bl_node_t *
bl_ast_enum_get_variant(bl_decl_enum_t *enm, const size_t i);

size_t
bl_ast_enum_get_count(bl_decl_enum_t *enm);

/*************************************************************************************************
 * type fund
 *************************************************************************************************/
bl_node_t *
bl_ast_type_fund_push_dim(bl_type_fund_t *type, bl_node_t *dim);

bl_node_t *
bl_ast_type_fund_get_dim(bl_type_fund_t *type, const size_t i);

size_t
bl_ast_type_fund_get_dim_count(bl_type_fund_t *type);

size_t
bl_ast_type_fund_dim_total_size(bl_type_fund_t *type);

/*************************************************************************************************
 * type ref
 *************************************************************************************************/
bl_node_t *
bl_ast_type_ref_push_dim(bl_type_ref_t *type, bl_node_t *dim);

bl_node_t *
bl_ast_type_ref_get_dim(bl_type_ref_t *type, const size_t i);

size_t
bl_ast_type_ref_get_dim_count(bl_type_ref_t *type);

size_t
bl_ast_type_ref_dim_total_size(bl_type_ref_t *type);

/*************************************************************************************************
 * other
 *************************************************************************************************/
bl_id_t *
bl_ast_try_get_id(bl_node_t *node);

int
bl_ast_try_get_modif(bl_node_t *node);

size_t
bl_ast_node_count(bl_ast_t *ast);

bl_node_t *
bl_ast_get_node(bl_ast_t *ast, size_t i);

bool
bl_type_compatible(bl_node_t *first, bl_node_t *second);

const char *
bl_ast_try_get_type_name(bl_node_t *type);

BArray *
bl_ast_try_get_type_dims(bl_node_t *type);

bl_node_t *
bl_ast_path_get_last(BArray *path);

bl_scopes_t *
bl_ast_try_get_scopes(bl_node_t *node);

bl_node_t *
bl_ast_try_get_parent(bl_node_t *node);
/**************************************************************************************************/

#endif // BL_NODE2_IMPL_H
