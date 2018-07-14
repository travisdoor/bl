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
#include <bobject/containers/hash.h>
#include "token_impl.h"
#include "common_impl.h"
#include "scope_impl.h"

// clang-format off
#define _BL_FTYPE_LIST                                                                         \
    ft(TYPE,   type_t) \
    ft(VOID,   void) \
    ft(S8,     s8) \
    ft(S16,    s16) \
    ft(S32,    s32) \
    ft(S64,    s64) \
    ft(U8,     u8) \
    ft(U16,    u16) \
    ft(U32,    u32) \
    ft(U64,    u64) \
    ft(SIZE,   size_t) \
    ft(F32,    f32) \
    ft(F64,    f64) \
    ft(CHAR,   char) \
    ft(STRING, string) \
    ft(BOOL,   bool)

#define _BL_BUILDINS_LIST \
    bt(MAIN,    main) \

#define _BL_NODE_TYPE_LIST \
  nt(BAD, bad, struct { \
    void *_; \
  }) \
  nt(IDENT, ident, struct { \
    const char *str; \
    uint64_t    hash; \
    bl_node_t  *ref; \
    bl_node_t  *parent_compound; \
  }) \
  nt(STMT_RETURN, stmt_return, struct { \
    bl_node_t *expr; \
    bl_node_t *fn; \
  }) \
  nt(STMT_IF, stmt_if, struct { \
    bl_node_t *test; \
    bl_node_t *true_stmt; \
    bl_node_t *false_stmt; \
  }) \
  nt(STMT_LOOP, stmt_loop, struct { \
    bl_node_t *test; \
    bl_node_t *true_stmt; \
  }) \
  nt(STMT_BREAK, stmt_break, struct { \
    void *_; \
  }) \
  nt(STMT_CONTINUE, stmt_continue, struct { \
    void *_; \
  }) \
  nt(DECL_UBLOCK, decl_ublock, struct { \
    bl_node_t      *nodes; \
    bl_scope_t     *scope; \
    struct bl_unit *unit; \
  }) \
  nt(DECL_BLOCK, decl_block, struct { \
    bl_node_t  *nodes; \
    bl_scope_t *scope; \
    bl_node_t  *parent_compound; \
  }) \
  nt(DECL_VALUE, decl_value, struct { \
    bl_node_t    *name; \
    bl_node_t    *type; \
    bl_node_t    *value; \
    bool          mutable; \
    int           flags; \
    int           used; \
  }) \
  nt(TYPE_FUND, type_fund, struct { \
    bl_ftype_e code; \
  }) \
  nt(TYPE_FN, type_fn, struct { \
    bl_node_t *arg_types; \
    bl_node_t *ret_type; \
    int        argc_types; \
  }) \
  nt(TYPE_STRUCT, type_struct, struct { \
    bl_node_t *types; \
    int        typesc; \
  }) \
  nt(LIT_FN, lit_fn, struct { \
    bl_node_t  *type; \
    bl_node_t  *block; \
    bl_scope_t *scope; \
    bl_node_t  *parent_compound; \
  }) \
  nt(LIT, lit, struct { \
    bl_node_t       *type; \
    bl_token_value_u value; \
  }) \
  nt(EXPR_CAST, expr_cast, struct { \
    bl_node_t *type; \
    bl_node_t *next; \
  }) \
  nt(EXPR_BINOP, expr_binop, struct { \
    bl_node_t *lhs; \
    bl_node_t *rhs; \
    bl_node_t *type; \
    bl_sym_e   op; \
  }) \
  nt(EXPR_CALL, expr_call, struct { \
    bl_node_t *ident; \
    bl_node_t *args; \
    int        argsc; \
    bl_node_t *type; \
  }) \
  nt(EXPR_SIZEOF, expr_sizeof, struct { \
    bl_node_t *in; \
    bl_node_t *type; \
  })

// clang-format on

typedef enum
{
  BL_KIND_UNKNOWN = 0,
  BL_KIND_SINT,   /* i8, i16, i32, i64 */
  BL_KIND_UINT,   /* u8, i16, u32, u64 */
  BL_KIND_SIZE,   /* size_t */
  BL_KIND_PTR,    /* pointers */
  BL_KIND_STRUCT, /* structs */
  BL_KIND_FN,     /* function */
  BL_KIND_REAL,   /* f32, f64 */
  BL_KIND_STRING, /* string */
  BL_KIND_CHAR,   /* char */
  BL_KIND_BOOL,   /* bool */
  BL_KIND_VOID,   /* void */
  BL_KIND_TYPE,   /* type_t */
} bl_type_kind_e;

typedef enum
{
  BL_FLAG_EXTERN = 1 << 0,
  BL_FLAG_MAIN   = 1 << 1
} bl_node_flag_e;

typedef struct bl_ast     bl_ast_t;
typedef struct bl_node    bl_node_t;
typedef enum bl_node_code bl_node_code_e;

typedef enum
{
#define ft(tok, str) BL_FTYPE_##tok,
  _BL_FTYPE_LIST
#undef ft
      BL_FTYPE_COUNT
} bl_ftype_e;

typedef enum
{
#define bt(name, str) BL_BUILDIN_##name,
  _BL_BUILDINS_LIST
#undef bt
      BL_BUILDIN_COUNT
} bl_buildin_e;

extern const char *bl_ftype_strings[];
extern const char *bl_node_type_strings[];
extern const char *bl_buildin_strings[];

extern uint64_t bl_ftype_hashes[BL_FTYPE_COUNT];
extern uint64_t bl_buildin_hashes[BL_BUILDIN_COUNT];

#define bl_node_foreach(_root, _it) for ((_it) = (_root); (_it); (_it) = (_it)->next)

/*************************************************************************************************
 * generation of node typedefs and code enum
 *************************************************************************************************/

#define nt(code, name, data) BL_NODE_##code,
enum bl_node_code
{
  _BL_NODE_TYPE_LIST BL_NODE_COUNT
};
#undef nt

// clang-format off
/* generate notes */
#define nt(code, name, data) typedef data bl_node_##name##_t;
_BL_NODE_TYPE_LIST
#undef nt

// clang-format on

/*************************************************************************************************
 * AST
 *************************************************************************************************/
#define bl_peek_src(n) (n)->src
#define bl_node_is(n, c) ((n)->code == (c))
#define bl_node_is_not(n, c) ((n)->code != (c))
#define bl_node_code(n) (n)->code
#define bl_node_name(n) bl_node_type_strings[(n)->code]

struct chunk;

struct bl_ast
{
  bl_node_t *root;

  struct chunk *first_chunk;
  struct chunk *current_chunk;
};

void
bl_ast_init(bl_ast_t *ast);

void
bl_ast_terminate(bl_ast_t *ast);

/*************************************************************************************************
 * definition node
 *************************************************************************************************/
struct bl_node
{
  union
  {
#define nt(code, name, data) bl_node_##name##_t name;
    _BL_NODE_TYPE_LIST
#undef nt
  } n;

  bl_src_t *     src;
  bl_node_code_e code;

  bl_node_t *next;
#if BL_DEBUG
  int _serial;
#endif
};

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/
#define nt(code, name, data)                                                                       \
  static inline bl_node_##name##_t *bl_peek_##name(bl_node_t *n)                                   \
  {                                                                                                \
    assert(bl_node_is(n, BL_NODE_##code));                                                         \
    return &(n->n.name);                                                                           \
  }
_BL_NODE_TYPE_LIST
#undef nt

/*************************************************************************************************
 * generate constructors definitions
 *************************************************************************************************/

#define _BL_AST_NCTOR(name, ...)                                                                   \
  bl_node_t *bl_ast_##name(bl_ast_t *ast, bl_token_t *tok, ##__VA_ARGS__)

_BL_AST_NCTOR(bad);
_BL_AST_NCTOR(decl_ublock, struct bl_unit *unit, bl_scope_t *scope);
_BL_AST_NCTOR(ident, bl_node_t *ref, bl_node_t *parent_compound);
_BL_AST_NCTOR(stmt_return, bl_node_t *expr, bl_node_t *fn);
_BL_AST_NCTOR(stmt_if, bl_node_t *test, bl_node_t *true_stmt, bl_node_t *false_stmt);
_BL_AST_NCTOR(stmt_loop, bl_node_t *test, bl_node_t *true_stmt);
_BL_AST_NCTOR(stmt_break);
_BL_AST_NCTOR(stmt_continue);
_BL_AST_NCTOR(decl_block, bl_node_t *nodes, bl_node_t *parent_compound, bl_scope_t *scope);
_BL_AST_NCTOR(decl_value, bl_node_t *name, bl_node_t *type, bl_node_t *value, bool mutable,
              int flags);
_BL_AST_NCTOR(type_struct, bl_node_t *types, int typesc);
_BL_AST_NCTOR(type_fn, bl_node_t *arg_types, int argc_types, bl_node_t *ret_type);
_BL_AST_NCTOR(lit_fn, bl_node_t *type, bl_node_t *block, bl_node_t *parent_compound,
              bl_scope_t *scope);
_BL_AST_NCTOR(lit, bl_node_t *type, bl_token_value_u value);
_BL_AST_NCTOR(expr_binop, bl_node_t *lhs, bl_node_t *rhs, bl_node_t *type, bl_sym_e op);
_BL_AST_NCTOR(expr_call, bl_node_t *ident, bl_node_t *args, int argsc, bl_node_t *type);
_BL_AST_NCTOR(expr_sizeof, bl_node_t *in, bl_node_t *type);
_BL_AST_NCTOR(expr_cast, bl_node_t *type, bl_node_t *next);

/*************************************************************************************************
 * other
 *************************************************************************************************/
extern bl_node_t bl_ftypes[];

void
bl_ast_type_to_string(char *buf, size_t len, bl_node_t *type);

bl_scope_t *
bl_ast_get_scope(bl_node_t *node);

bl_node_t *
bl_ast_get_parent_compound(bl_node_t *node);

bl_node_t *
bl_ast_get_type(bl_node_t *node);

int
bl_ast_is_buildin_type(bl_node_t *ident);

int
bl_ast_is_buildin(bl_node_t *ident);

bool
bl_ast_type_cmp(bl_node_t *first, bl_node_t *second);

bl_type_kind_e
bl_ast_get_type_kind(bl_node_t *type);

bool
bl_ast_can_impl_cast(bl_node_t *from_type, bl_node_t *to_type);

bl_node_t *
bl_ast_node_dup(bl_ast_t *ast, bl_node_t *node);

void
bl_ast_node_insert(bl_node_t **dest, bl_node_t *node);

/**************************************************************************************************/

#endif // BL_NODE2_IMPL_H
