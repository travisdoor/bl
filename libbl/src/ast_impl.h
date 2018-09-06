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

#ifndef BL_NODE_IMPL_H
#define BL_NODE_IMPL_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/list.h>
#include <bobject/containers/hash.h>
#include "token_impl.h"
#include "common_impl.h"
#include "scope_impl.h"

// clang-format off
#define _FTYPE_LIST                                                                         \
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

#define _BUILDINS_LIST \
    bt(MAIN,      main) \
    bt(ARR_COUNT, count) \

#define _NODE_TYPE_LIST \
  nt(BAD, bad, struct { \
    void *_; \
  }) \
  nt(LOAD, load, struct { \
    const char *filepath; \
  }) \
  nt(LINK, link, struct { \
    const char *lib; \
  }) \
  nt(IDENT, ident, struct { \
    const char *str; \
    uint64_t    hash; \
    node_t  *ref; \
    node_t  *parent_compound; \
    node_t  *arr; \
    int         ptr; \
  }) \
  nt(UBLOCK, ublock, struct { \
    node_t      *nodes; \
    scope_t     *scope; \
    struct bl_unit *unit; \
  }) \
  nt(BLOCK, block, struct { \
    node_t  *nodes; \
    scope_t *scope; \
    node_t  *parent_compound; \
  }) \
  nt(STMT_RETURN, stmt_return, struct { \
    node_t *expr; \
    node_t *fn_decl; \
  }) \
  nt(STMT_IF, stmt_if, struct { \
    node_t *test; \
    node_t *true_stmt; \
    node_t *false_stmt; \
  }) \
  nt(STMT_LOOP, stmt_loop, struct { \
    node_t *test; \
    node_t *true_stmt; \
  }) \
  nt(STMT_BREAK, stmt_break, struct { \
    void *_; \
  }) \
  nt(STMT_CONTINUE, stmt_continue, struct { \
    void *_; \
  }) \
  nt(DECL, decl, struct { \
    decl_kind_e kind; \
    node_t     *name; \
    node_t     *type; \
    node_t     *value; \
    bool        mutable; \
    int         flags; \
    int         used; \
    int         order; \
    bool        in_gscope; \
    BHashTable *deps; \
  }) \
  nt(TYPE_FUND, type_fund, struct { \
    ftype_e code; \
    node_t *arr; \
    int     ptr;				\
  }) \
  nt(TYPE_FN, type_fn, struct { \
    node_t *arg_types; \
    node_t *ret_type; \
    node_t *arr; \
    int     argc_types; \
    int     ptr; \
  }) \
  nt(TYPE_STRUCT, type_struct, struct { \
    node_t *base_decl; /* sometimes we need structure name and scope? */ \
    node_t *types; \
    node_t *arr; \
    int     typesc; \
    int     ptr; \
  }) \
  nt(TYPE_ENUM, type_enum, struct { \
    node_t *base_decl; \
    node_t *base_type; \
    node_t *arr; \
    int     ptr; \
  }) \
  nt(LIT_STRUCT, lit_struct, struct { \
    node_t  *type; \
    scope_t *scope; \
    node_t  *parent_compound; \
  }) \
  nt(LIT_ENUM, lit_enum, struct { \
    node_t  *type; \
    scope_t *scope; \
    node_t  *parent_compound; \
    node_t  *variants; \
  }) \
  nt(LIT_FN, lit_fn, struct { \
    node_t  *type; \
    node_t  *block; \
    scope_t *scope; \
    node_t  *parent_compound; \
  }) \
  nt(LIT, lit, struct { \
    node_t       *type; \
    token_value_u value; \
  }) \
  nt(EXPR_CAST, expr_cast, struct { \
    node_t *type; \
    node_t *next; \
  }) \
  nt(EXPR_BINOP, expr_binop, struct { \
    node_t *lhs; \
    node_t *rhs; \
    node_t *type; \
    sym_e   op; \
  }) \
  nt(EXPR_CALL, expr_call, struct { \
    node_t *ref; \
    node_t *args; \
    int        argsc; \
    node_t *type; \
    bool       run; \
  }) \
  nt(EXPR_MEMBER, expr_member, struct { \
    member_kind_e kind; \
    node_t       *ident; \
    node_t       *next; \
    node_t       *type; \
    bool          ptr_ref; \
  }) \
  nt(EXPR_ELEM, expr_elem, struct { \
    node_t       *next; \
    node_t       *type; \
    node_t       *index; \
  }) \
  nt(EXPR_SIZEOF, expr_sizeof, struct { \
    node_t *in; \
    node_t *type; \
  }) \
  nt(EXPR_UNARY, expr_unary, struct { \
    sym_e   op; \
    node_t *next; \
    node_t *type; \
  }) \
  nt(EXPR_NULL, expr_null, struct { \
    node_t *type;		    \
  })

// clang-format on

// TODO: TYPE prefix???
typedef enum
{
  KIND_UNKNOWN = 0,
  KIND_SINT,   /* i8, i16, i32, i64 */
  KIND_UINT,   /* u8, i16, u32, u64 */
  KIND_SIZE,   /* size_t */
  KIND_PTR,    /* pointers */
  KIND_STRUCT, /* structs */
  KIND_ENUM,   /* enums */
  KIND_FN,     /* function */
  KIND_REAL,   /* f32, f64 */
  KIND_STRING, /* string */
  KIND_CHAR,   /* char */
  KIND_BOOL,   /* bool */
  KIND_VOID,   /* void */
  KIND_TYPE,   /* type_t */
} type_kind_e;

typedef enum
{
  DECL_KIND_UNKNOWN  = -1,
  DECL_KIND_FIELD    = 0, /* foo s32; foo := 0; */
  DECL_KIND_FN       = 1, /* foo : fn () {} */
  DECL_KIND_STRUCT   = 2, /* foo : struct {} */
  DECL_KIND_MEMBER   = 3, /* structure member */
  DECL_KIND_ARG      = 4, /* function argument */
  DECL_KIND_ENUM     = 5, /* foo : enum {} */
  DECL_KIND_VARIANT  = 6, /* enum variant */
  DECL_KIND_CONSTANT = 7, /* foo : 10; foo : bar; */
  DECL_KIND_TYPE     = 8, /* foo : s32; foo : bar; */
} decl_kind_e;

typedef enum
{
  MEM_KIND_UNKNOWN = -1,
  MEM_KIND_STRUCT  = 0, /* structure.bar; structure->bar; */
  MEM_KIND_ENUM    = 1, /* enum.A; */
} member_kind_e;

typedef enum
{
  FLAG_EXTERN = 1 << 0, /* methods marked as extern */
  FLAG_MAIN   = 1 << 1  /* main method */
} node_flag_e;

typedef enum
{
  DEP_LAX    = 1 << 0, /* dependency is't needed for successful IR construction */
  DEP_STRICT = 1 << 1, /* dependency must be linked for sucessful IR construction */
} dep_e;

typedef struct ast     ast_t;
typedef struct node    node_t;
typedef enum node_code node_code_e;

typedef struct
{
  node_t *node; /* dependent node */
  dep_e   type; /* is dependency strict (ex.: caused by #run directive) */
} dependency_t;

typedef enum
{
#define ft(tok, str) BL_FTYPE_##tok,
  _FTYPE_LIST
#undef ft
      BL_FTYPE_COUNT
} ftype_e;

typedef enum
{
#define bt(name, str) BL_BUILDIN_##name,
  _BUILDINS_LIST
#undef bt
      BL_BUILDIN_COUNT
} buildin_e;

extern const char *bl_ftype_strings[];
extern const char *node_type_strings[];
extern const char *bl_buildin_strings[];

extern uint64_t ftype_hashes[BL_FTYPE_COUNT];
extern uint64_t buildin_hashes[BL_BUILDIN_COUNT];

#define node_foreach(_root, _it) for ((_it) = (_root); (_it); (_it) = (_it)->next)
#define node_foreach_ref(_root, _it) for ((_it) = &(_root); *(_it); (_it) = &((*(_it))->next))

/*************************************************************************************************
 * generation of node typedefs and code enum
 *************************************************************************************************/

#define nt(code, name, data) NODE_##code,
enum node_code
{
  _NODE_TYPE_LIST NODE_COUNT
};
#undef nt

// clang-format off
/* generate notes */
#define nt(code, name, data) typedef data node_##name##_t;
_NODE_TYPE_LIST
#undef nt

// clang-format on

/*************************************************************************************************
 * AST
 *************************************************************************************************/
#define peek_src(n) (n)->src
#define node_is(n, c) ((n)->code == (c))
#define node_is_not(n, c) ((n)->code != (c))
#define node_code(n) (n)->code
#define node_name(n) node_type_strings[(n)->code]

struct chunk;

struct ast
{
  node_t *root;

  struct chunk *first_chunk;
  struct chunk *current_chunk;
};

void
ast_init(ast_t *ast);

void
ast_terminate(ast_t *ast);

/*************************************************************************************************
 * definition node
 *************************************************************************************************/
typedef enum
{
  NOT_CHECKED = 0, /* not checked node */
  WAITING,         /* waiting for later check */
  CHECKED          /* checked node */
} check_state_e;

struct node
{
  union
  {
#define nt(code, name, data) node_##name##_t name;
    _NODE_TYPE_LIST
#undef nt
  } n;

  src_t *     src;
  node_code_e code;

  node_t *      next;
  check_state_e state;
#if BL_DEBUG
  int _serial;
#endif
};

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/
#define nt(code, name, data)                                                                       \
  static inline node_##name##_t *peek_##name(node_t *n)                                            \
  {                                                                                                \
    assert(node_is(n, NODE_##code));                                                               \
    return &(n->n.name);                                                                           \
  }
_NODE_TYPE_LIST
#undef nt

/*************************************************************************************************
 * generate constructors definitions
 *************************************************************************************************/

#define _NODE_NCTOR(name, ...) node_t *ast_##name(ast_t *ast, token_t *tok, ##__VA_ARGS__)

_NODE_NCTOR(bad);
_NODE_NCTOR(load, const char *filepath);
_NODE_NCTOR(link, const char *lib);
_NODE_NCTOR(ublock, struct bl_unit *unit, scope_t *scope);
_NODE_NCTOR(block, node_t *nodes, node_t *parent_compound, scope_t *scope);
_NODE_NCTOR(ident, node_t *ref, node_t *parent_compound, int ptr, node_t *arr);
_NODE_NCTOR(stmt_return, node_t *expr, node_t *fn);
_NODE_NCTOR(stmt_if, node_t *test, node_t *true_stmt, node_t *false_stmt);
_NODE_NCTOR(stmt_loop, node_t *test, node_t *true_stmt);
_NODE_NCTOR(stmt_break);
_NODE_NCTOR(stmt_continue);
_NODE_NCTOR(decl, decl_kind_e kind, node_t *name, node_t *type, node_t *value, bool mutable,
            int flags, int order, bool in_gscope);
_NODE_NCTOR(type_fund, ftype_e code, int ptr, node_t *arr);
_NODE_NCTOR(type_fn, node_t *arg_types, int argc_types, node_t *ret_type, int ptr);
_NODE_NCTOR(type_struct, node_t *types, int typesc, node_t *base_decl, int ptr);
_NODE_NCTOR(type_enum, node_t *type, node_t *base_decl, int ptr);
_NODE_NCTOR(lit_fn, node_t *type, node_t *block, node_t *parent_compound, scope_t *scope);
_NODE_NCTOR(lit_struct, node_t *type, node_t *parent_compound, scope_t *scope);
_NODE_NCTOR(lit_enum, node_t *type, node_t *variants, node_t *parent_compound, scope_t *scope);
_NODE_NCTOR(lit, node_t *type, token_value_u value);
_NODE_NCTOR(expr_binop, node_t *lhs, node_t *rhs, node_t *type, sym_e op);
_NODE_NCTOR(expr_call, node_t *ref, node_t *args, int argsc, node_t *type, bool run);
_NODE_NCTOR(expr_member, member_kind_e kind, node_t *ident, node_t *next, node_t *type,
            bool ptr_ref);
_NODE_NCTOR(expr_elem, node_t *next, node_t *type, node_t *index);
_NODE_NCTOR(expr_sizeof, node_t *in, node_t *type);
_NODE_NCTOR(expr_cast, node_t *type, node_t *next);
_NODE_NCTOR(expr_unary, sym_e op, node_t *next, node_t *type);
_NODE_NCTOR(expr_null, node_t *type);

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

typedef struct ast_visitor ast_visitor_t;
typedef void (*ast_visit_f)(ast_visitor_t *visitor, node_t *node, void *cnt);

struct ast_visitor
{
  ast_visit_f visitors[NODE_COUNT];
};

void
ast_visitor_init(ast_visitor_t *visitor);

void
ast_visitor_add(ast_visitor_t *visitor, ast_visit_f fn, node_code_e code);

void
ast_visit(ast_visitor_t *visitor, node_t *node, void *cnt);

void
ast_walk(ast_visitor_t *visitor, node_t *node, void *cnt);

/*************************************************************************************************
 * other
 *************************************************************************************************/

/* static fundamental type nodes */
extern node_t bl_ftypes[];

void
ast_type_to_string(char *buf, size_t len, node_t *type);

scope_t *
ast_get_scope(node_t *node);

node_t *
ast_get_parent_compound(node_t *node);

bool
ast_is_type(node_t *node);

node_t *
ast_get_type(node_t *node);

void
ast_set_type(node_t *node, node_t *type);

int
ast_is_buildin_type(node_t *ident);

int
ast_is_buildin(node_t *ident);

bool
ast_type_cmp(node_t *first, node_t *second);

int
ast_type_get_ptr(node_t *type);

void
ast_type_set_ptr(node_t *type, int ptr);

node_t *
ast_type_get_arr(node_t *type);

void
ast_type_set_arr(node_t *type, node_t *arr);

type_kind_e
ast_get_type_kind(node_t *type);

bool
ast_can_impl_cast(node_t *from_type, node_t *to_type);

node_t *
ast_node_dup(ast_t *ast, node_t *node);

node_t *
ast_unroll_ident(node_t *ident);

dependency_t *
ast_add_dep_uq(node_t *decl, node_t *dep, int type);

/**************************************************************************************************/

#endif // NODE2_IMPL_H
