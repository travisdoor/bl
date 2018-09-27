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
    const char *str; \
    uint64_t    hash; \
    Node       *ref; \
    Node       *parent_compound; \
    Node       *arr; \
    int         ptr; \
  }) \
  nt(UBLOCK, UBlock, ublock, struct { \
    Node      *nodes; \
    Scope     *scope; \
    struct Unit *unit; \
  }) \
  nt(BLOCK, Block, block, struct { \
    Node  *nodes; \
    Scope *scope; \
    Node  *parent_compound; \
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
    Node *init; \
    Node *condition; \
    Node *increment; \
    Node *block; \
    Scope *scope; \
    Node  *parent_compound; \
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
    int         order; \
    bool        in_gscope; \
    BHashTable *deps; \
  }) \
  nt(TYPE_FUND, TypeFund, type_fund, struct { \
    FundType code; \
    Node *arr; \
    int     ptr;				\
  }) \
  nt(TYPE_FN, TypeFn, type_fn, struct { \
    Node *arg_types; \
    Node *ret_type; \
    Node *arr; \
    int   argc_types; \
    int   ptr; \
  }) \
  nt(TYPE_STRUCT, TypeStruct, type_struct, struct { \
    Node *base_decl; /* sometimes we need structure name and scope? */ \
    Node *types; \
    Node *arr; \
    int   typesc; \
    int   ptr; \
  }) \
  nt(TYPE_ENUM, TypeEnum, type_enum, struct { \
    Node *base_decl; \
    Node *base_type; \
    Node *arr; \
    int   ptr; \
  }) \
  nt(LIT_STRUCT, LitStruct, lit_struct, struct { \
    Node  *type; \
    Scope *scope; \
    Node  *parent_compound; \
  }) \
  nt(LIT_ENUM, LitEnum, lit_enum, struct { \
    Node  *type; \
    Scope *scope; \
    Node  *parent_compound; \
    Node  *variants; \
  }) \
  nt(LIT_FN, LitFn, lit_fn, struct { \
    Node  *type; \
    Node  *block; \
    Scope *scope; \
    Node  *parent_compound; \
  }) \
  nt(LIT, Lit, lit, struct { \
    Node       *type; \
    TokenValue value; \
  }) \
  nt(LIT_CMP, LitCmp, lit_cmp, struct { \
    Node *type; \
    Node *fields; \
    int   fieldc; \
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
  TYPE_KIND_UNKNOWN = 0,
  TYPE_KIND_SINT,   /* i8, i16, i32, i64 */
  TYPE_KIND_UINT,   /* u8, i16, u32, u64 */
  TYPE_KIND_SIZE,   /* size_t */
  TYPE_KIND_PTR,    /* pointers */
  TYPE_KIND_STRUCT, /* structs */
  TYPE_KIND_ENUM,   /* enums */
  TYPE_KIND_FN,     /* function */
  TYPE_KIND_REAL,   /* f32, f64 */
  TYPE_KIND_STRING, /* string */
  TYPE_KIND_CHAR,   /* char */
  TYPE_KIND_BOOL,   /* bool */
  TYPE_KIND_VOID,   /* void */
  TYPE_KIND_TYPE,   /* type_t */
} TypeKind;

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
} DeclKind;

typedef enum
{
  MEM_KIND_UNKNOWN = -1,
  MEM_KIND_STRUCT  = 0, /* structure.bar; structure->bar; */
  MEM_KIND_ENUM    = 1, /* enum.A; */
} MemberKind;

typedef enum
{
  FLAG_EXTERN   = 1 << 0, /* methods marked as extern */
  FLAG_MAIN     = 1 << 1, /* main method */
  FLAG_TEST     = 1 << 2, /* test case */
  FLAG_INTERNAL = 1 << 3, /* internal declarations */
} NodeFlag;

typedef enum
{
  DEP_LAX    = 1 << 0, /* dependency is't needed for successful IR construction */
  DEP_STRICT = 1 << 1, /* dependency must be linked for sucessful IR construction */
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
#define peek_src(n) (n)->src
#define node_is(n, c) ((n)->code == (c))
#define node_is_not(n, c) ((n)->code != (c))
#define node_code(n) (n)->code
#define node_name(n) node_type_strings[(n)->code]

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
typedef enum
{
  NOT_CHECKED = 0, /* not checked node */
  WAITING,         /* waiting for later check */
  CHECKED          /* checked node */
} CheckState;

struct Node
{
  union
  {
#define nt(code, Name, name, data) Node##Name name;
    _NODE_TYPE_LIST
#undef nt
  };

  Src *      src;
  NodeCode   code;
  Node *     next;
  CheckState state;
#if BL_DEBUG
  int _serial;
#endif
};

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/
#define nt(code, Name, name, data)                                                                 \
  static inline Node##Name *peek_##name(Node *n)                                                   \
  {                                                                                                \
    assert(node_is(n, NODE_##code));                                                               \
    return &(n->name);                                                                             \
  }
_NODE_TYPE_LIST
#undef nt

/*************************************************************************************************
 * generate constructors definitions
 *************************************************************************************************/

#define _NODE_CTOR(name, ...) Node *ast_##name(Ast *ast, Token *tok, ##__VA_ARGS__)

_NODE_CTOR(bad);
_NODE_CTOR(load, const char *filepath);
_NODE_CTOR(link, const char *lib);
_NODE_CTOR(ublock, struct Unit *unit, Scope *scope);
_NODE_CTOR(block, Node *nodes, Node *parent_compound, Scope *scope);
_NODE_CTOR(ident, const char *str, Node *ref, Node *parent_compound, int ptr, Node *arr);
_NODE_CTOR(stmt_return, Node *expr, Node *fn);
_NODE_CTOR(stmt_if, Node *test, Node *true_stmt, Node *false_stmt);
_NODE_CTOR(stmt_loop, Node *init, Node *condition, Node *increment, Node *block, Scope *scope,
           Node *parent_compound);
_NODE_CTOR(stmt_break);
_NODE_CTOR(stmt_continue);
_NODE_CTOR(lit_cmp, Node *type, Node *fields, int fieldc);
_NODE_CTOR(decl, DeclKind kind, Node *name, Node *type, Node *value, bool mutable, int flags,
           int order, bool in_gscope);
_NODE_CTOR(type_fund, FundType code, int ptr, Node *arr);
_NODE_CTOR(type_fn, Node *arg_types, int argc_types, Node *ret_type, int ptr);
_NODE_CTOR(type_struct, Node *types, int typesc, Node *base_decl, int ptr);
_NODE_CTOR(type_enum, Node *type, Node *base_decl, int ptr);
_NODE_CTOR(lit_fn, Node *type, Node *block, Node *parent_compound, Scope *scope);
_NODE_CTOR(lit_struct, Node *type, Node *parent_compound, Scope *scope);
_NODE_CTOR(lit_enum, Node *type, Node *variants, Node *parent_compound, Scope *scope);
_NODE_CTOR(lit, Node *type, TokenValue value);
_NODE_CTOR(expr_binop, Node *lhs, Node *rhs, Node *type, Sym op);
_NODE_CTOR(expr_call, Node *ref, Node *args, int argsc, Node *type, bool run);
_NODE_CTOR(expr_member, MemberKind kind, Node *ident, Node *next, Node *type, bool ptr_ref);
_NODE_CTOR(expr_elem, Node *next, Node *type, Node *index);
_NODE_CTOR(expr_sizeof, Node *in, Node *type);
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
};

void
visitor_init(Visitor *visitor);

void
visitor_add(Visitor *visitor, VisitorFunc fn, NodeCode code);

void
visitor_visit(Visitor *visitor, Node *node, void *cnt);

void
visitor_walk(Visitor *visitor, Node *node, void *cnt);

/*************************************************************************************************
 * other
 *************************************************************************************************/

/* static fundamental type nodes */
extern Node ftypes[];

void
ast_type_to_string(char *buf, size_t len, Node *type);

Scope *
ast_get_scope(Node *node);

Node *
ast_get_parent_compound(Node *node);

bool
ast_is_type(Node *node);

Node *
ast_get_type(Node *node);

void
ast_set_type(Node *node, Node *type);

int
ast_is_buildin_type(Node *ident);

int
ast_is_buildin(Node *ident);

bool
ast_type_cmp(Node *first, Node *second);

int
ast_type_get_ptr(Node *type);

void
ast_type_set_ptr(Node *type, int ptr);

Node *
ast_type_get_arr(Node *type);

void
ast_type_set_arr(Node *type, Node *arr);

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

/**************************************************************************************************/

#endif
