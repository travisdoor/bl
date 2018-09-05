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
    bl_scope_t     *scope; \
    struct bl_unit *unit; \
  }) \
  nt(BLOCK, block, struct { \
    node_t  *nodes; \
    bl_scope_t *scope; \
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
    bl_decl_kind_e kind; \
    node_t     *name; \
    node_t     *type; \
    node_t     *value; \
    bool           mutable; \
    int            flags; \
    int            used; \
    int            order; \
    bool           in_gscope; \
    BHashTable    *deps; \
  }) \
  nt(TYPE_FUND, type_fund, struct { \
    bl_ftype_e code; \
    node_t *arr; \
    int        ptr; \
  }) \
  nt(TYPE_FN, type_fn, struct { \
    node_t *arg_types; \
    node_t *ret_type; \
    node_t *arr; \
    int        argc_types; \
    int        ptr; \
  }) \
  nt(TYPE_STRUCT, type_struct, struct { \
    node_t *base_decl; /* sometimes we need structure name and scope? */ \
    node_t *types; \
    node_t *arr; \
    int        typesc; \
    int        ptr; \
  }) \
  nt(TYPE_ENUM, type_enum, struct { \
    node_t *base_decl; \
    node_t *base_type; \
    node_t *arr; \
    int        ptr; \
  }) \
  nt(LIT_STRUCT, lit_struct, struct { \
    node_t  *type; \
    bl_scope_t *scope; \
    node_t  *parent_compound; \
  }) \
  nt(LIT_ENUM, lit_enum, struct { \
    node_t  *type; \
    bl_scope_t *scope; \
    node_t  *parent_compound; \
    node_t  *variants; \
  }) \
  nt(LIT_FN, lit_fn, struct { \
    node_t  *type; \
    node_t  *block; \
    bl_scope_t *scope; \
    node_t  *parent_compound; \
  }) \
  nt(LIT, lit, struct { \
    node_t       *type; \
    bl_token_value_u value; \
  }) \
  nt(EXPR_CAST, expr_cast, struct { \
    node_t *type; \
    node_t *next; \
  }) \
  nt(EXPR_BINOP, expr_binop, struct { \
    node_t *lhs; \
    node_t *rhs; \
    node_t *type; \
    bl_sym_e   op; \
  }) \
  nt(EXPR_CALL, expr_call, struct { \
    node_t *ref; \
    node_t *args; \
    int        argsc; \
    node_t *type; \
    bool       run; \
  }) \
  nt(EXPR_MEMBER, expr_member, struct { \
    bl_member_kind_e kind; \
    node_t       *ident; \
    node_t       *next; \
    node_t       *type; \
    bool             ptr_ref; \
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
    bl_sym_e   op; \
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
} bl_type_kind_e;

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
} bl_decl_kind_e;

typedef enum
{
  MEM_KIND_UNKNOWN = -1,
  MEM_KIND_STRUCT  = 0, /* structure.bar; structure->bar; */
  MEM_KIND_ENUM    = 1, /* enum.A; */
} bl_member_kind_e;

typedef enum
{
  FLAG_EXTERN = 1 << 0, /* methods marked as extern */
  FLAG_MAIN   = 1 << 1  /* main method */
} node_flag_e;

typedef enum
{
  DEP_LAX    = 1 << 0, /* dependency is't needed for successful IR construction */
  DEP_STRICT = 1 << 1, /* dependency must be linked for sucessful IR construction */
} bl_dep_e;

typedef struct bl_ast  bl_ast_t;
typedef struct node    node_t;
typedef enum node_code node_code_e;

typedef struct
{
  node_t * node; /* dependent node */
  bl_dep_e type; /* is dependency strict (ex.: caused by #run directive) */
} bl_dependency_t;

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
extern const char *node_type_strings[];
extern const char *bl_buildin_strings[];

extern uint64_t bl_ftype_hashes[BL_FTYPE_COUNT];
extern uint64_t bl_buildin_hashes[BL_BUILDIN_COUNT];

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

struct bl_ast
{
  node_t *root;

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
typedef enum
{
  BL_NOT_CHECKED = 0, /* not checked node */
  BL_WAITING,         /* waiting for later check */
  BL_CHECKED          /* checked node */
} bl_check_state_e;

struct node
{
  union
  {
#define nt(code, name, data) node_##name##_t name;
    _NODE_TYPE_LIST
#undef nt
  } n;

  bl_src_t *  src;
  node_code_e code;

  node_t *         next;
  bl_check_state_e state;
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

#define _BL_AST_NCTOR(name, ...)                                                                   \
  node_t *bl_ast_##name(bl_ast_t *ast, bl_token_t *tok, ##__VA_ARGS__)

_BL_AST_NCTOR(bad);
_BL_AST_NCTOR(load, const char *filepath);
_BL_AST_NCTOR(link, const char *lib);
_BL_AST_NCTOR(ublock, struct bl_unit *unit, bl_scope_t *scope);
_BL_AST_NCTOR(block, node_t *nodes, node_t *parent_compound, bl_scope_t *scope);
_BL_AST_NCTOR(ident, node_t *ref, node_t *parent_compound, int ptr, node_t *arr);
_BL_AST_NCTOR(stmt_return, node_t *expr, node_t *fn);
_BL_AST_NCTOR(stmt_if, node_t *test, node_t *true_stmt, node_t *false_stmt);
_BL_AST_NCTOR(stmt_loop, node_t *test, node_t *true_stmt);
_BL_AST_NCTOR(stmt_break);
_BL_AST_NCTOR(stmt_continue);
_BL_AST_NCTOR(decl, bl_decl_kind_e kind, node_t *name, node_t *type, node_t *value, bool mutable,
              int flags, int order, bool in_gscope);
_BL_AST_NCTOR(type_fund, bl_ftype_e code, int ptr, node_t *arr);
_BL_AST_NCTOR(type_fn, node_t *arg_types, int argc_types, node_t *ret_type, int ptr);
_BL_AST_NCTOR(type_struct, node_t *types, int typesc, node_t *base_decl, int ptr);
_BL_AST_NCTOR(type_enum, node_t *type, node_t *base_decl, int ptr);
_BL_AST_NCTOR(lit_fn, node_t *type, node_t *block, node_t *parent_compound, bl_scope_t *scope);
_BL_AST_NCTOR(lit_struct, node_t *type, node_t *parent_compound, bl_scope_t *scope);
_BL_AST_NCTOR(lit_enum, node_t *type, node_t *variants, node_t *parent_compound, bl_scope_t *scope);
_BL_AST_NCTOR(lit, node_t *type, bl_token_value_u value);
_BL_AST_NCTOR(expr_binop, node_t *lhs, node_t *rhs, node_t *type, bl_sym_e op);
_BL_AST_NCTOR(expr_call, node_t *ref, node_t *args, int argsc, node_t *type, bool run);
_BL_AST_NCTOR(expr_member, bl_member_kind_e kind, node_t *ident, node_t *next, node_t *type,
              bool ptr_ref);
_BL_AST_NCTOR(expr_elem, node_t *next, node_t *type, node_t *index);
_BL_AST_NCTOR(expr_sizeof, node_t *in, node_t *type);
_BL_AST_NCTOR(expr_cast, node_t *type, node_t *next);
_BL_AST_NCTOR(expr_unary, bl_sym_e op, node_t *next, node_t *type);
_BL_AST_NCTOR(expr_null, node_t *type);

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

typedef struct bl_ast_visitor bl_ast_visitor_t;
typedef void (*bl_ast_visit_f)(bl_ast_visitor_t *visitor, node_t *node, void *cnt);

struct bl_ast_visitor
{
  bl_ast_visit_f visitors[NODE_COUNT];
};

void
bl_ast_visitor_init(bl_ast_visitor_t *visitor);

void
bl_ast_visitor_add(bl_ast_visitor_t *visitor, bl_ast_visit_f fn, node_code_e code);

void
bl_ast_visit(bl_ast_visitor_t *visitor, node_t *node, void *cnt);

void
bl_ast_walk(bl_ast_visitor_t *visitor, node_t *node, void *cnt);

/*************************************************************************************************
 * other
 *************************************************************************************************/

/* static fundamental type nodes */
extern node_t bl_ftypes[];

void
bl_ast_type_to_string(char *buf, size_t len, node_t *type);

bl_scope_t *
bl_ast_get_scope(node_t *node);

node_t *
bl_ast_get_parent_compound(node_t *node);

bool
bl_ast_is_type(node_t *node);

node_t *
bl_ast_get_type(node_t *node);

void
bl_ast_set_type(node_t *node, node_t *type);

int
bl_ast_is_buildin_type(node_t *ident);

int
bl_ast_is_buildin(node_t *ident);

bool
bl_ast_type_cmp(node_t *first, node_t *second);

int
bl_ast_type_get_ptr(node_t *type);

void
bl_ast_type_set_ptr(node_t *type, int ptr);

node_t *
bl_ast_type_get_arr(node_t *type);

void
bl_ast_type_set_arr(node_t *type, node_t *arr);

bl_type_kind_e
bl_ast_get_type_kind(node_t *type);

bool
bl_ast_can_impl_cast(node_t *from_type, node_t *to_type);

node_t *
bl_ast_node_dup(bl_ast_t *ast, node_t *node);

node_t *
bl_ast_unroll_ident(node_t *ident);

bl_dependency_t *
bl_ast_add_dep_uq(node_t *decl, node_t *dep, int type);

/**************************************************************************************************/

#endif // NODE2_IMPL_H
