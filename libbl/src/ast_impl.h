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

#ifndef BL_AST_IMPL_H
#define BL_AST_IMPL_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/list.h>
#include "id_impl.h"
#include "token_impl.h"
#include "scope_impl.h"
#include "common_impl.h"

// clang-format off
#define BL_FUND_TYPE_LIST                                                                         \
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
    ft(STRING, "string") \
    ft(BOOL,   "bool")

#define BL_SPEC_BUILINS\
    bt(ARR_COUNT = 0, count) \
    bt(MAIN,          main) \

#define BL_NODE_TYPE_LIST \
    nt(STMT_IF,            stmt_if) \
    nt(STMT_LOOP,          stmt_loop) \
    nt(STMT_BREAK,         stmt_break) \
    nt(STMT_CONTINUE,      stmt_continue) \
    nt(STMT_RETURN,        stmt_return) \
    nt(STMT_USING,         stmt_using) \
    nt(DECL_MODULE,        decl_module) \
    nt(DECL_MUT,           decl_mut) \
    nt(DECL_CONST,         decl_const) \
    nt(DECL_FUNC,          decl_func) \
    nt(DECL_ARG,           decl_arg) \
    nt(DECL_STRUCT,        decl_struct) \
    nt(DECL_STRUCT_MEMBER, decl_struct_member) \
    nt(DECL_ENUM,          decl_enum) \
    nt(DECL_ENUM_VARIANT,  decl_enum_variant) \
    nt(DECL_BLOCK,         decl_block) \
    nt(EXPR_LITERAL,       expr_literal) \
    nt(EXPR_BINOP,         expr_binop) \
    nt(EXPR_UNARY,         expr_unary) \
    nt(EXPR_DECL_REF,      expr_decl_ref) \
    nt(EXPR_MEMBER_REF,    expr_member_ref) \
    nt(EXPR_ARRAY_REF,     expr_array_ref) \
    nt(EXPR_CALL,          expr_call) \
    nt(EXPR_SIZEOF,        expr_sizeof) \
    nt(EXPR_NULL,          expr_null) \
    nt(EXPR_CAST,          expr_cast) \
    nt(EXPR_INIT,          expr_init) \
    nt(TYPE_FUND,          type_fund) \
    nt(TYPE_REF,           type_ref) \
    nt(PATH_ELEM,          path_elem) \
    nt(PRE_LOAD,           pre_load) \
    nt(PRE_LINK,           pre_link)

// clang-format on

#define BL_AST_TERMINATE 0

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

typedef enum
{
#define bt(name, str) BL_BUILDIN_##name,
  BL_SPEC_BUILINS
#undef bt
      BL_BUILDIN_COUNT
} bl_buildin_e;

typedef enum
{
  BL_UNKNOWN_KIND = 0,
  BL_SINT_KIND,   /* i8, i16, i32, i64 */
  BL_UINT_KIND,   /* u8, i16, u32, u64 */
  BL_SIZE_KIND,   /* size_t */
  BL_PTR_KIND,    /* pointers */
  BL_STRUCT_KIND, /* structs */
  BL_REAL_KIND,   /* f32, f64 */
  BL_STR_KIND,    /* string */
  BL_CHAR_KIND,   /* char */
  BL_BOOL_KIND,   /* bool */
  BL_VOID_KIND,   /* void */
} bl_type_kind_e;

extern const char *bl_fund_type_strings[];
extern const char *bl_node_type_strings[];
extern const char *bl_buildin_strings[];

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

typedef enum
{
  BL_DEP_LAX = 0b01,
  BL_DEP_STRICT = 0b10,
} bl_dep_e;

typedef struct
{
  bl_node_t *node; /* dependency node */
  bl_dep_e   type; /* is dependency strict (ex.: caused by #run directive) */
} bl_dependency_t;

struct chunk;

struct bl_ast
{
  bl_node_t *root;

  struct chunk *first_chunk;
  struct chunk *current_chunk;
};

enum bl_modif
{
  BL_MODIF_NONE   = 0,
  BL_MODIF_PUBLIC = 1, /* explicit */
  BL_MODIF_EXTERN = 2, /* explicit */
  BL_MODIF_EXPORT = 4, /* implicit */
  BL_MODIF_UTEST  = 8, /* partialy implicit (when #test directive has
                          been used before declaration) */
  BL_MODIF_ENTRY = 16, /* entry function (typically main) */
};

/*************************************************************************************************
 * definition of the node structure bodies
 *************************************************************************************************/

/* if statement */
struct bl_stmt_if
{
  bl_node_t *parent;     /* parent node */
  bl_node_t *test;       /* testing condition expression */
  bl_node_t *true_stmt;  /* statement invoked when condition is true */
  bl_node_t *false_stmt; /* statement invoked when condition is false */
};

/* loop statement */
struct bl_stmt_loop
{
  bl_node_t *parent;    /* parent node */
  bl_node_t *test;      /* testing condition */
  bl_node_t *true_stmt; /* statment invoked in loop */
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
  bl_node_t *expr; /* return expression */
  bl_node_t *func; /* function parent */
};

struct bl_stmt_using
{
  bl_node_t *path; /* path */
  bl_node_t *ref;  /* reference to module or enum */
};

struct bl_expr_sizeof
{
  bl_node_t *des_type; /* desired type */
  bl_node_t *type;     /* result type */
};

struct bl_expr_null
{
  bl_node_t *type; /* null type */
};

struct bl_expr_cast
{
  bl_node_t *type; /* destination type of the cast */
  bl_node_t *next; /* fallowing expression */
};

struct bl_expr_init
{
  bl_node_t *type; /* initialization list type result */
  bl_node_t *exprs;
};

/* module declaration */
struct bl_decl_module
{
  bl_id_t     id;     /* identificator */
  bl_node_t * parent; /* parent node */
  int         modif;  /* modificator*/
  bl_node_t * nodes;
  bl_scopes_t scopes; /* scope cache */
};

/* variable declaration */
struct bl_decl_mut
{
  bl_id_t    id;           /* identificator */
  int        modif;        /* modificator */
  bl_node_t *type;         /* variable type */
  bl_node_t *init_expr;    /* initialization expression if there is one */
  int        used;         /* usage count */
  bool       is_anonymous; /* variable invisible for symbol lookup functions when true */
};

struct bl_decl_const
{
  bl_id_t    id;        /* identificator */
  int        modif;     /* modificator */
  bl_node_t *type;      /* constant type */
  bl_node_t *init_expr; /* initialization expressions (must have one) */
  int        used;      /* usage count */
};

struct bl_decl_arg
{
  bl_id_t    id;   /* identificator */
  bl_node_t *type; /* argument type */
};

struct bl_decl_func
{
  bl_id_t     id;                 /* identificator */
  bl_node_t * parent;             /* parent node */
  int         modif;              /* modificator */
  int         used;               /* count of usage */
  bl_node_t * args;               /* pointer to list of arguments */
  int         argsc;              /* arguments count */
  bl_node_t * block;              /* function block (for extern function is NULL) */
  bl_node_t * ret_type;           /* return type */
  bl_scopes_t scopes;             /* scope cache */
  bool        gen_in_compiletime; /* TODO: remove */
  BList *     deps;  /* linked-list of dependencies (function called from this function) */
  char *      uname; /* function unique name */
};

struct bl_decl_struct
{
  bl_id_t     id;       /* structure id */
  int         modif;    /* modificators */
  int         used;     /* count of usage */
  bl_node_t * members;  /* pointer to list of members */
  int         membersc; /* member count */
  bl_scopes_t scopes;   /* scope cache */
};

struct bl_decl_struct_member
{
  bl_id_t    id;    /* identificator */
  int        modif; /* modificator */
  bl_node_t *type;  /* structure member type */
  int        order; /* order inside struct layout */
};

struct bl_decl_enum
{
  bl_id_t     id;     /* identificator */
  bl_node_t * parent; /* parent node */
  int         modif;  /* modificator */
  int         used;   /* count of usage */
  bl_node_t * type;   /* enum type */
  bl_node_t * variants;
  bl_scopes_t scopes; /* scope cache */
};

struct bl_decl_enum_variant
{
  bl_id_t    id;   /* identificator */
  bl_node_t *expr; /* enum variant initialization expression */
  bl_node_t *parent;
  bl_node_t *type;
};

struct bl_decl_block
{
  bl_node_t * parent; /* parent node */
  bl_node_t * nodes;  /* list of nodes in compount block */
  bl_scopes_t scopes; /* scope cache */
};

struct bl_expr_literal
{
  bl_node_t *type; /* variant of type */

  union
  {
    char               c;
    bool               b;
    long long          s;
    unsigned long long u;
    double             f;
    const char *       str;
  } value; /* value of type */
};

struct bl_expr_binop
{
  bl_sym_e   op;   /* operator */
  bl_node_t *lhs;  /* left-hand side value */
  bl_node_t *rhs;  /* right-hand side value */
  bl_node_t *type; /* result type of operations (type of left operand in most cases) */
};

struct bl_expr_unary
{
  bl_sym_e   op;   /* operator of unary expression */
  bl_node_t *next; /* fallowing node */

  /* some unary expressions can change resulting type of next expression (pointer dereferencing,
   * address of, ...), in such cases we create copy of typeof(next) node*/
  bl_node_t *type;
};

struct bl_expr_decl_ref
{
  bl_node_t *path; /* path */
  bl_node_t *ref;  /* reference to referenced node */
  bl_node_t *type;
};

struct bl_expr_member_ref
{
  bl_id_t    id;         /* member identificator */
  bl_node_t *ref;        /* reference to member */
  bl_node_t *next;       /* fallowing expression */
  bool       is_ptr_ref; /* true when we accesig to members via pointer */
  bl_node_t *type;
};

struct bl_expr_array_ref
{
  bl_node_t *index; /* index expression */
  bl_node_t *next;  /* fallowing expression */
  bl_node_t *type;
};

struct bl_expr_call
{
  bl_node_t *path; /* path */
  bl_node_t *ref;  /* reference to function */
  bl_node_t *args; /* argument list passed into function */
  int        argsc;
  bool       run_in_compile_time;
  bl_node_t *type;
};

struct bl_path_elem
{
  bl_id_t id; /* identificator of element in path array */
};

struct bl_type_fund
{
  bl_fund_type_e type;   /* fundamental type variant */
  bl_node_t *    dim;    /* dimensions are used when type is array */
  int            is_ptr; /* is pointer */
};

struct bl_type_ref
{
  bl_node_t *path;   /* path */
  bl_node_t *ref;    /* reference to type */
  bl_node_t *dim;    /* dimensions are used when type is array */
  int        is_ptr; /* is pointer */
};

struct bl_pre_load
{
  const char *filepath;
};

struct bl_pre_link
{
  const char *lib;
};

struct bl_node
{
  bl_src_t *     src;
  bl_node_code_e code;

  bl_node_t *next;
  bl_node_t *prev;

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
bl_ast_add_type_fund(bl_ast_t *ast, bl_token_t *tok, bl_fund_type_e t, int is_ptr);

bl_node_t *
bl_ast_add_type_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *ref,
                    bl_node_t *path, int is_ptr);

bl_node_t *
bl_ast_add_expr_sizeof(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type);

bl_node_t *
bl_ast_add_expr_null(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type);

bl_node_t *
bl_ast_add_expr_literal(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type);

bl_node_t *
bl_ast_add_pre_load(bl_ast_t *ast, bl_token_t *tok, const char *filepath);

bl_node_t *
bl_ast_add_pre_link(bl_ast_t *ast, bl_token_t *tok, const char *lib);

bl_node_t *
bl_ast_add_expr_literal_char(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, char c);

bl_node_t *
bl_ast_add_expr_literal_bool(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, bool b);

bl_node_t *
bl_ast_add_expr_literal_signed(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, long long s);

bl_node_t *
bl_ast_add_expr_literal_unsigned(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type,
                                 unsigned long long u);

bl_node_t *
bl_ast_add_expr_literal_double(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, double f);

bl_node_t *
bl_ast_add_expr_literal_str(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type, const char *str);

bl_node_t *
bl_ast_add_expr_binop(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *lhs, bl_node_t *rhs,
                      bl_node_t *type);

bl_node_t *
bl_ast_add_expr_unary(bl_ast_t *ast, bl_token_t *tok, bl_sym_e op, bl_node_t *next);

bl_node_t *
bl_ast_add_expr_decl_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, bl_node_t *path);

bl_node_t *
bl_ast_add_expr_member_ref(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *next,
                           bl_node_t *ref, bool is_ptr_ref);

bl_node_t *
bl_ast_add_expr_cast(bl_ast_t *ast, bl_token_t *tok, bl_node_t *to_type, bl_node_t *next);

bl_node_t *
bl_ast_add_expr_init(bl_ast_t *ast, bl_token_t *tok, bl_node_t *type);

bl_node_t *
bl_ast_add_expr_array_ref(bl_ast_t *ast, bl_token_t *tok, bl_node_t *index, bl_node_t *next);

bl_node_t *
bl_ast_add_expr_call(bl_ast_t *ast, bl_token_t *tok, bl_node_t *ref, bl_node_t *path,
                     bool run_in_compile_time);

bl_node_t *
bl_ast_add_path_elem(bl_ast_t *ast, bl_token_t *tok, const char *name);

bl_node_t *
bl_ast_add_decl_module(bl_ast_t *ast, bl_token_t *tok, const char *name, int modif,
                       bl_node_t *parent);

bl_node_t *
bl_ast_add_decl_mut(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                    bl_node_t *init_expr, int modif, bool is_anonymous);

bl_node_t *
bl_ast_add_decl_const(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type,
                      bl_node_t *init_expr, int modif);

bl_node_t *
bl_ast_add_decl_arg(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *type);

bl_node_t *
bl_ast_add_decl_func(bl_ast_t *ast, bl_token_t *tok, const char *name, bl_node_t *block,
                     bl_node_t *ret_type, int modif, bl_node_t *parent, bool gen_in_compiletime);

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
bl_ast_add_stmt_return(bl_ast_t *ast, bl_token_t *tok, bl_node_t *expr, bl_node_t *func);

bl_node_t *
bl_ast_add_stmt_using(bl_ast_t *ast, bl_token_t *tok, bl_node_t *path);

/*************************************************************************************************
 * other
 *************************************************************************************************/
bl_id_t *
bl_ast_get_id(bl_node_t *node);

int
bl_ast_get_modif(bl_node_t *node);

bool
bl_ast_type_compatible(bl_node_t *first, bl_node_t *second);

int
bl_ast_type_is_ptr(bl_node_t *first);

void
bl_ast_type_addrof(bl_node_t *type);

void
bl_ast_type_deref(bl_node_t *type);

void
bl_ast_type_remove_dim(bl_node_t *type);

bl_type_kind_e
bl_ast_type_get_kind(bl_node_t *type);

bl_node_t *
bl_ast_get_type(bl_node_t *node);

void
bl_ast_get_type_name(bl_node_t *type, char *out_name, int max_len);

bl_node_t **
bl_ast_get_type_dim(bl_node_t *type);

bool
bl_ast_type_is_fund(bl_node_t *type, bl_fund_type_e t);

bool
bl_ast_type_is_ref(bl_node_t *type, bl_node_code_e t);

bool
bl_ast_node_is_const(bl_node_t *node);

bl_node_t *
bl_ast_path_get_last(bl_node_t *path);

bl_scopes_t *
bl_ast_get_scopes(bl_node_t *node);

bl_node_t *
bl_ast_get_parent(bl_node_t *node);

bl_node_t *
bl_ast_dup_node(bl_ast_t *ast, bl_node_t *node);

void
bl_ast_dup_and_insert(bl_ast_t *ast, bl_node_t **dest, bl_node_t *src);

void
bl_ast_dup_node_buf(bl_node_t *dest, bl_node_t *node);

bool
bl_ast_can_implcast(bl_node_t *from_type, bl_node_t *to_type);

uint64_t
bl_ast_buildin_hash(bl_buildin_e t);

bool
bl_ast_is_buildin(bl_id_t *id, bl_buildin_e t);

bl_dependency_t *
bl_ast_func_add_dep(bl_decl_func_t *_func, bl_node_t *dep, int type);

/**************************************************************************************************/

#endif // BL_NODE2_IMPL_H
