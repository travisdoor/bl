//************************************************************************************************
// bl
//
// File:   ast.h
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

#ifndef BL_AST_H
#define BL_AST_H

#include <bobject/containers/array.h>
#include <bobject/containers/htbl.h>
#include <bobject/containers/list.h>
#include <bobject/containers/hash.h>
#include "token.h"
#include "common.h"
#include "scope.h"

// clang-format off
#define _FTYPE_LIST                                                                         \
    ft(VOID,   void) \
    ft(S8,     s8) \
    ft(S16,    s16) \
    ft(S32,    s32) \
    ft(S64,    s64) \
    ft(U8,     u8) \
    ft(U16,    u16) \
    ft(U32,    u32) \
    ft(U64,    u64) \
    ft(SIZE,   usize) \
    ft(F32,    f32) \
    ft(F64,    f64) \
    ft(CHAR,   char) /* REMOVE and use *u8 */ \
    ft(STRING, string) \
    ft(BOOL,   bool)

#define _BUILDINS_LIST \
    bt(MAIN,      main) \
    bt(ASSERT,    assert) \
    bt(ANY,       any) \
    bt(ARRAY,     __Array) \

#define _NODE_TYPE_LIST \
  nt(BAD, Bad, bad, struct { \
    void *_; \
  }) \
  nt(LOAD, Load, load, struct { \
    const char *filepath; \
  }) \
  nt(LINK, Link, link, struct { \
    const char *lib; \
  }) \
  nt(IDENT, Ident, ident, struct { \
    Scope      *scope;\
    const char *str; \
    uint64_t    hash; \
    Node       *ref; \
  }) \
  nt(UBLOCK, UBlock, ublock, struct { \
    Scope     *scope; \
    Node      *nodes; \
    struct Unit *unit; \
  }) \
  nt(BLOCK, Block, block, struct { \
    Scope *scope; \
    Node  *nodes; \
  }) \
  nt(STMT_RETURN, StmtReturn, stmt_return, struct { \
    Node *expr; \
    Node *fn_decl; \
  }) \
  nt(STMT_IF, StmtIf, stmt_if, struct { \
    Node *test; \
    Node *true_stmt; \
    Node *false_stmt; \
  }) \
  nt(STMT_LOOP, StmtLoop, stmt_loop, struct { \
    Scope *scope; \
    Node  *init; \
    Node  *condition; \
    Node  *increment; \
    Node  *block; \
  }) \
  nt(STMT_BREAK, StmtBreak, stmt_break, struct { \
    void *_; \
  }) \
  nt(STMT_CONTINUE, StmtContinue, stmt_continue, struct { \
    void *_; \
  }) \
  nt(DECL, Decl, decl, struct { \
    DeclKind    kind; \
    Node       *name; \
    Node       *type; \
    Node       *value; \
    bool        mutable; \
    int         flags; \
    int         used; \
    bool        in_gscope; \
    BHashTable *deps; \
  }) \
  nt(MEMBER, Member, member, struct { \
    Node *name; \
    Node *type; \
    int   order; \
  }) \
  nt(ARG, Arg, arg, struct { \
    Node *name; \
    Node *type; \
  }) \
  nt(VARIANT, Variant, variant, struct { \
    Node *name; \
    Node *type; \
    Node *value; \
  }) \
  nt(TYPE_TYPE, TypeType, type_type, struct { \
    Node *name; \
    Node *spec; \
  }) \
  nt(TYPE_FUND, TypeFund, type_fund, struct { \
    FundType code; \
  }) \
  nt(TYPE_VARGS, TypeVArgs, type_vargs, struct { \
    void *_; \
  }) \
  nt(TYPE_ARR, TypeArr, type_arr, struct { \
    Node *elem_type; \
    Node *len; \
  }) \
  nt(TYPE_FN, TypeFn, type_fn, struct { \
    Node *arg_types; \
    Node *ret_type; \
    int   argc_types; \
  }) \
  nt(TYPE_STRUCT, TypeStruct, type_struct, struct { \
    Scope *scope; \
    Node  *members; \
    int    membersc; \
    bool   raw; \
  })					    \
  nt(TYPE_ENUM, TypeEnum, type_enum, struct { \
    Scope *scope; \
    Node  *type; \
    Node  *variants; \
  }) \
  nt(TYPE_PTR, TypePtr, type_ptr, struct { \
    Node *type; \
  }) \
  nt(LIT_FN, LitFn, lit_fn, struct { \
    Scope *scope; \
    Node  *type; \
    Node  *block; \
  }) \
  nt(LIT, Lit, lit, struct { \
    Node       *type; \
    TokenValue value; \
  }) \
  nt(LIT_CMP, LitCmp, lit_cmp, struct { \
    Scope *scope; \
    Node  *type; \
    Node  *fields; \
    int    fieldc; \
  }) \
  nt(EXPR_CAST, ExprCast, expr_cast, struct { \
    Node *type; \
    Node *next; \
  }) \
  nt(EXPR_BINOP, ExprBinop, expr_binop, struct { \
    Node *lhs; \
    Node *rhs; \
    Node *type; \
    Sym   op; \
  }) \
  nt(EXPR_CALL, ExprCall, expr_call, struct { \
    Node *ref; \
    Node *args; \
    int   argsc; \
    Node *type; \
    bool  run; \
  }) \
  nt(EXPR_MEMBER, ExprMember, expr_member, struct { \
    MemberKind kind; \
    Node      *ident; \
    Node      *next; \
    Node      *type; \
    bool       ptr_ref; \
    int        i; \
  }) \
  nt(EXPR_ELEM, ExprElem, expr_elem, struct { \
    Node       *next; \
    Node       *type; \
    Node       *index; \
  }) \
  nt(EXPR_SIZEOF, ExprSizeof, expr_sizeof, struct { \
    Node *in; \
    Node *type; \
  }) \
  nt(EXPR_TYPEOF, ExprTypeof, expr_typeof, struct { \
    Node *in; \
    Node *type; \
  }) \
  nt(EXPR_UNARY, ExprUnary, expr_unary, struct { \
    Sym   op; \
    Node *next; \
    Node *type; \
  }) \
  nt(EXPR_NULL, ExprNull, expr_null, struct { \
    Node *type;		    \
  })

// clang-format on

typedef enum
{
  TYPE_KIND_INVALID = 0,
  TYPE_KIND_SINT    = 1,  /* i8, i16, i32, i64 */
  TYPE_KIND_UINT    = 2,  /* u8, i16, u32, u64 */
  TYPE_KIND_SIZE    = 3,  /* size_t */
  TYPE_KIND_PTR     = 4,  /* pointers */
  TYPE_KIND_STRUCT  = 5,  /* structs */
  TYPE_KIND_ENUM    = 6,  /* enums */
  TYPE_KIND_FN      = 7,  /* function */
  TYPE_KIND_REAL    = 8,  /* f32, f64 */
  TYPE_KIND_STRING  = 9,  /* string */
  TYPE_KIND_CHAR    = 10, /* char */
  TYPE_KIND_BOOL    = 11, /* bool */
  TYPE_KIND_VOID    = 12, /* void */
  TYPE_KIND_TYPE    = 13, /* type_t */
  TYPE_KIND_ANY     = 14, /* any */
  TYPE_KIND_ARR     = 15, /* array */
} TypeKind;

typedef enum
{
  DECL_KIND_INVALID = 0,
  DECL_KIND_FIELD   = 1, /* a := 0; */
  DECL_KIND_TYPE    = 2, /* Type : struct { s32 }; */
  DECL_KIND_FN      = 3, /* main : fn () s32 {}; */
  DECL_KIND_ENUM    = 4, /* Enum : enum s8 {}; */
} DeclKind;

typedef enum
{
  MEM_KIND_INVALID = 0,
  MEM_KIND_STRUCT  = 1, /* structure.bar; structure->bar; */
  MEM_KIND_ENUM    = 2, /* enum.A; */
} MemberKind;

typedef enum
{
  FLAG_EXTERN = 1 << 0, /* methods marked as extern */
  FLAG_MAIN   = 1 << 1, /* main method */
  FLAG_TEST   = 1 << 2, /* test case */
} NodeFlag;

typedef enum
{
  DEP_LAX    = 1 << 0, /* dependency is't needed for successful IR construction */
  DEP_STRICT = 1 << 1, /* dependency must be linked for successful IR construction */
} DepType;

typedef struct Ast    Ast;
typedef struct Node   Node;
typedef enum NodeCode NodeCode;

typedef struct
{
  Node *  node; /* dependent node */
  DepType type; /* is dependency strict (ex.: caused by #run directive) */
} Dependency;

typedef enum
{
#define ft(tok, str) FTYPE_##tok,
  _FTYPE_LIST
#undef ft
      FTYPE_COUNT
} FundType;

typedef enum
{
#define bt(name, str) BUILDIN_##name,
  _BUILDINS_LIST
#undef bt
      BUILDIN_COUNT
} BuildinType;

extern const char *ftype_strings[];
extern const char *node_type_strings[];
extern const char *buildin_strings[];

extern uint64_t ftype_hashes[FTYPE_COUNT];
extern uint64_t buildin_hashes[BUILDIN_COUNT];

#define node_foreach(_root, _it) for ((_it) = (_root); (_it); (_it) = (_it)->next)
#define node_foreach_ref(_root, _it) for ((_it) = &(_root); *(_it); (_it) = &((*(_it))->next))

/*************************************************************************************************
 * generation of node typedefs and code enum
 *************************************************************************************************/

#define nt(code, Name, name, data) NODE_##code,
enum NodeCode
{
  _NODE_TYPE_LIST NODE_COUNT
};
#undef nt

// clang-format off
/* generate notes */
#define nt(code, Name, name, data) typedef data Node##Name;
_NODE_TYPE_LIST
#undef nt

// clang-format on

/*************************************************************************************************
 * AST
 *************************************************************************************************/
struct Chunk;

struct Ast
{
  Node *root;

  struct Chunk *first_chunk;
  struct Chunk *current_chunk;
};

void
ast_init(Ast *ast);

void
ast_terminate(Ast *ast);

/*************************************************************************************************
 * definition node
 *************************************************************************************************/

struct Node
{
  union
  {
#define nt(code, Name, name, data) Node##Name name;
    _NODE_TYPE_LIST
#undef nt
  };

  Src *    src;
  NodeCode code;
  Node *   next;
#if BL_DEBUG
  enum
  {
    NOT_CHECKED = 0, /* not checked node */
    WAITING,         /* waiting for later check */
    CHECKED          /* checked node */
  } _state;
  int _serial;
#endif
};

/* static fundamental type nodes */
extern Node ftypes[];
extern Node type_type;

static inline Src *
ast_peek_src(Node *n)
{
  return n->src;
}

static inline NodeCode
ast_node_code(Node *n)
{
  return n->code;
}

static inline bool
ast_node_is(Node *n, NodeCode c)
{
  return ast_node_code(n) == c;
}

static inline bool
ast_node_is_not(Node *n, NodeCode c)
{
  return ast_node_code(n) != c;
}

static inline const char *
ast_node_name(Node *n)
{
  return node_type_strings[ast_node_code(n)];
}

static inline bool
ast_node_is_type(Node *node)
{
  return ast_node_code(node) >= NODE_TYPE_TYPE && ast_node_code(node) <= NODE_TYPE_PTR;
}

void
ast_type_to_string(char *buf, size_t len, Node *type);

Node *
ast_get_type(Node *node);

int
ast_is_buildin_type(Node *ident);

int
ast_is_buildin(Node *ident);

bool
ast_type_cmp(Node *first, Node *second);

TypeKind
ast_type_kind(Node *type);

bool
ast_can_impl_cast(Node *from_type, Node *to_type);

Node *
ast_node_dup(Ast *Ast, Node *node);

Node *
ast_unroll_ident(Node *ident);

Dependency *
ast_add_dep_uq(Node *decl, Node *dep, int type);

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/
#define nt(code, Name, name, data)                                                                 \
  static inline Node##Name *ast_peek_##name(Node *n)                                               \
  {                                                                                                \
    assert(ast_node_is(n, NODE_##code));                                                           \
    return &(n->name);                                                                             \
  }
_NODE_TYPE_LIST
#undef nt

/*************************************************************************************************
 * generate constructors definitions
 *************************************************************************************************/

#define _NODE_CTOR(name, ...) Node *ast_create_##name(Ast *_ast, Token *_tok, ##__VA_ARGS__)

_NODE_CTOR(bad);
_NODE_CTOR(load, const char *filepath);
_NODE_CTOR(link, const char *lib);
_NODE_CTOR(ublock, struct Unit *unit, Scope *scope);
_NODE_CTOR(block, Node *nodes, Scope *scope);
_NODE_CTOR(ident, const char *str, Node *ref, Scope *scope);
_NODE_CTOR(stmt_return, Node *expr, Node *fn);
_NODE_CTOR(stmt_if, Node *test, Node *true_stmt, Node *false_stmt);
_NODE_CTOR(stmt_loop, Node *init, Node *condition, Node *increment, Node *block, Scope *scope);
_NODE_CTOR(stmt_break);
_NODE_CTOR(stmt_continue);
_NODE_CTOR(lit_cmp, Node *type, Node *fields, int fieldc, Scope *scope);
_NODE_CTOR(decl, DeclKind kind, Node *name, Node *type, Node *value, bool mutable, int flags,
           bool in_gscope);
_NODE_CTOR(member, Node *name, Node *type, int order);
_NODE_CTOR(arg, Node *name, Node *type);
_NODE_CTOR(variant, Node *name, Node *type, Node *value);
_NODE_CTOR(type_type, Node *name, Node *spec);
_NODE_CTOR(type_fund, FundType code);
_NODE_CTOR(type_vargs);
_NODE_CTOR(type_arr, Node *elem_type, Node *len);
_NODE_CTOR(type_fn, Node *arg_types, int argc_types, Node *ret_type);
_NODE_CTOR(type_struct, Node *members, int membersc, Scope *scope, bool raw);
_NODE_CTOR(type_enum, Node *type, Node *variants, Scope *scope);
_NODE_CTOR(type_ptr, Node *type);
_NODE_CTOR(lit_fn, Node *type, Node *block, Scope *scope);
_NODE_CTOR(lit, Node *type, TokenValue value);
_NODE_CTOR(expr_binop, Node *lhs, Node *rhs, Node *type, Sym op);
_NODE_CTOR(expr_call, Node *ref, Node *args, int argsc, Node *type, bool run);
_NODE_CTOR(expr_member, MemberKind kind, Node *ident, Node *next, Node *type, bool ptr_ref, int i);
_NODE_CTOR(expr_elem, Node *next, Node *type, Node *index);
_NODE_CTOR(expr_sizeof, Node *in, Node *type);
_NODE_CTOR(expr_typeof, Node *in, Node *type);
_NODE_CTOR(expr_cast, Node *type, Node *next);
_NODE_CTOR(expr_unary, Sym op, Node *next, Node *type);
_NODE_CTOR(expr_null, Node *type);

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

typedef struct Visitor Visitor;
typedef void (*VisitorFunc)(Visitor *visitor, Node *node, void *cnt);

struct Visitor
{
  VisitorFunc visitors[NODE_COUNT];
  VisitorFunc all_visitor;
};

void
visitor_init(Visitor *visitor);

void
visitor_add(Visitor *visitor, VisitorFunc fn, NodeCode code);

void
visitor_add_visit_all(Visitor *visitor, VisitorFunc fn);

void
visitor_visit(Visitor *visitor, Node *node, void *cnt);

void
visitor_walk(Visitor *visitor, Node *node, void *cnt);

/**************************************************************************************************/

#endif
