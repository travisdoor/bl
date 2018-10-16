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

struct Arena;
typedef struct Node Node;

#define node_foreach(_root, _it) for ((_it) = (_root); (_it); (_it) = (_it)->next)
#define node_foreach_ref(_root, _it) for ((_it) = &(_root); *(_it); (_it) = &((*(_it))->next))

/* OLD BEGIN */
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
// clang-format on
/* OLD END */

typedef enum
{
  NODE_BAD,
  NODE_LOAD,
  NODE_LINK,
  NODE_IDENT,
  NODE_UBLOCK,
  NODE_BLOCK,
  NODE_STMT_RETURN,
  NODE_STMT_IF,
  NODE_STMT_LOOP,
  NODE_STMT_BREAK,
  NODE_STMT_CONTINUE,
  NODE_DECL,
  NODE_MEMBER,
  NODE_ARG,
  NODE_VARIANT,
  NODE_TYPE_TYPE,
  NODE_TYPE_FUND,
  NODE_TYPE_VARGS,
  NODE_TYPE_ARR,
  NODE_TYPE_FN,
  NODE_TYPE_STRUCT,
  NODE_TYPE_ENUM,
  NODE_TYPE_PTR,
  NODE_LIT_FN,
  NODE_LIT_INT,
  NODE_LIT_FLOAT,
  NODE_LIT_CHAR,
  NODE_LIT_STRING,
  NODE_LIT_BOOL,
  NODE_LIT_CMP,
  NODE_EXPR_CAST,
  NODE_EXPR_BINOP,
  NODE_EXPR_CALL,
  NODE_EXPR_MEMBER,
  NODE_EXPR_ELEM,
  NODE_EXPR_SIZEOF,
  NODE_EXPR_TYPEOF,
  NODE_EXPR_UNARY,
  NODE_EXPR_NULL,
  NODE_COUNT
} NodeCode;

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

/* map symbols to binary operation kind */
typedef enum
{
  BINOP_ASSIGN     = SYM_ASSIGN,
  BINOP_ADD_ASSIGN = SYM_PLUS_ASSIGN,
  BINOP_SUB_ASSIGN = SYM_MINUS_ASSIGN,
  BINOP_MUL_ASSIGN = SYM_ASTERISK_ASSIGN,
  BINOP_DIV_ASSIGN = SYM_SLASH_ASSIGN,
  BINOP_MOD_ASSIGN = SYM_PERCENT_ASSIGN,
  BINOP_ADD        = SYM_PLUS,
  BINOP_SUB        = SYM_MINUS,
  BINOP_MUL        = SYM_ASTERISK,
  BINOP_DIV        = SYM_SLASH,
  BINOP_MOD        = SYM_PERCENT,
  BINOP_EQ         = SYM_EQ,
  BINOP_NEQ        = SYM_NEQ,
  BINOP_GREATER    = SYM_GREATER,
  BINOP_LESS       = SYM_LESS,
  BINOP_GREATER_EQ = SYM_GREATER_EQ,
  BINOP_LESS_EQ    = SYM_LESS_EQ,
  BINOP_LOGIC_AND  = SYM_LOGIC_AND,
  BINOP_LOGIC_OR   = SYM_LOGIC_OR
} BinopKind;

typedef enum
{
  UNOP_NEG   = SYM_MINUS,
  UNOP_POS   = SYM_PLUS,
  UNOP_NOT   = SYM_NOT,
  UNOP_ADR   = SYM_AND,
  UNOP_DEREF = SYM_ASTERISK
} UnopKind;

typedef enum
{
  DEP_LAX    = 1 << 0, /* dependency is't needed for successful IR construction */
  DEP_STRICT = 1 << 1, /* dependency must be linked for successful IR construction */
} DepType;

struct NodeBad
{
  void *_;
};

struct NodeLoad
{
  const char *filepath;
};

struct NodeLink
{
  const char *lib;
};

struct NodeIdent
{
  Scope *     scope;
  const char *str;
  uint64_t    hash;
};

struct NodeUBlock
{
  Scope *      scope;
  Node *       nodes;
  struct Unit *unit;
};

struct NodeBlock
{
  Scope *scope;
  Node * nodes;
};

struct NodeStmtReturn
{
  Node *expr;
  Node *fn_decl;
};

struct NodeStmtIf
{
  Node *test;
  Node *true_stmt;
  Node *false_stmt;
};

struct NodeStmtLoop
{
  Scope *scope;
  Node * init;
  Node * condition;
  Node * increment;
  Node * block;
};

struct NodeStmtBreak
{
  void *_;
};

struct NodeStmtContinue
{
  void *_;
};

struct NodeDecl
{
  Node *   type;
  DeclKind kind;
  Node *   name;
  Node *   value;
  bool mutable;
  int         flags;
  int         used;
  bool        in_gscope;
  BHashTable *deps;
};

struct NodeMember
{
  Node *type;
  Node *name;
  int   order;
};

struct NodeArg
{
  Node *type;
  Node *name;
};

struct NodeVariant
{
  Node *type;
  Node *name;
  Node *value;
};

struct NodeTypeType
{
  Node *name;
  Node *spec;
};

struct NodeTypeFund
{
  FundType code;
};

struct NodeTypeVArgs
{
  void *_;
};

struct NodeTypeArr
{
  Node *elem_type;
  Node *len;
};

struct NodeTypeFn
{
  Node *arg_types;
  Node *ret_type;
  int   argc_types;
};

struct NodeTypeStruct
{
  Scope *scope;
  Node * members;
  int    membersc;
  bool   raw;
};

struct NodeTypeEnum
{
  Scope *scope;
  Node * type;
  Node * variants;
};

struct NodeTypePtr
{
  Node *type;
};

struct NodeLitFn
{
  Scope *scope;
  Node * type;
  Node * block;
};

struct NodeLitInt
{
  uint64_t i;
};

struct NodeLitFloat
{
  float f;
};

struct NodeLitChar
{
  uint8_t c;
};

struct NodeLitString
{
  const char *s;
};

struct NodeLitBool
{
  bool b;
};

struct NodeLitCmp
{
  Scope *scope;
  Node * type;
  Node * fields;
  int    fieldc;
};

struct NodeExprCast
{
  Node *type;
  Node *next;
};

struct NodeExprBinop
{
  Node *    type;
  Node *    lhs;
  Node *    rhs;
  BinopKind kind;
};

struct NodeExprCall
{
  Node *ref;
  Node *args;
  int   argsc;
  Node *type;
  bool  run;
};

struct NodeExprMember
{
  Node *     type;
  MemberKind kind;
  Node *     ident;
  Node *     next;
  bool       ptr_ref;
  int        i;
};

struct NodeExprElem
{
  Node *type;
  Node *next;
  Node *index;
};

struct NodeExprSizeof
{
  Node *type;
  Node *in;
};

struct NodeExprTypeof
{
  Node *type;
  Node *in;
};

struct NodeExprUnary
{
  Node *   type;
  UnopKind kind;
  Node *   next;
};

struct NodeExprNull
{
  Node *type;
};

typedef struct NodeBad          NodeBad;
typedef struct NodeLoad         NodeLoad;
typedef struct NodeLink         NodeLink;
typedef struct NodeIdent        NodeIdent;
typedef struct NodeUBlock       NodeUBlock;
typedef struct NodeBlock        NodeBlock;
typedef struct NodeStmtReturn   NodeStmtReturn;
typedef struct NodeStmtIf       NodeStmtIf;
typedef struct NodeStmtLoop     NodeStmtLoop;
typedef struct NodeStmtBreak    NodeStmtBreak;
typedef struct NodeStmtContinue NodeStmtContinue;
typedef struct NodeDecl         NodeDecl;
typedef struct NodeMember       NodeMember;
typedef struct NodeArg          NodeArg;
typedef struct NodeVariant      NodeVariant;
typedef struct NodeTypeType     NodeTypeType;
typedef struct NodeTypeFund     NodeTypeFund;
typedef struct NodeTypeVArgs    NodeTypeVArgs;
typedef struct NodeTypeArr      NodeTypeArr;
typedef struct NodeTypeFn       NodeTypeFn;
typedef struct NodeTypeStruct   NodeTypeStruct;
typedef struct NodeTypeEnum     NodeTypeEnum;
typedef struct NodeTypePtr      NodeTypePtr;
typedef struct NodeLitFn        NodeLitFn;
typedef struct NodeLitInt       NodeLitInt;
typedef struct NodeLitFloat     NodeLitFloat;
typedef struct NodeLitChar      NodeLitChar;
typedef struct NodeLitString    NodeLitString;
typedef struct NodeLitBool      NodeLitBool;
typedef struct NodeLitCmp       NodeLitCmp;
typedef struct NodeExprCast     NodeExprCast;
typedef struct NodeExprBinop    NodeExprBinop;
typedef struct NodeExprCall     NodeExprCall;
typedef struct NodeExprMember   NodeExprMember;
typedef struct NodeExprElem     NodeExprElem;
typedef struct NodeExprSizeof   NodeExprSizeof;
typedef struct NodeExprTypeof   NodeExprTypeof;
typedef struct NodeExprUnary    NodeExprUnary;
typedef struct NodeExprNull     NodeExprNull;

struct Node
{
  union
  {
    NodeBad          bad;
    NodeLoad         load;
    NodeLink         link;
    NodeIdent        ident;
    NodeUBlock       ublock;
    NodeBlock        block;
    NodeStmtReturn   stmt_return;
    NodeStmtIf       stmt_if;
    NodeStmtLoop     stmt_loop;
    NodeStmtBreak    stmt_break;
    NodeStmtContinue stmt_continue;
    NodeDecl         decl;
    NodeMember       member;
    NodeArg          arg;
    NodeVariant      variant;
    NodeTypeType     type_type;
    NodeTypeFund     type_fund;
    NodeTypeVArgs    type_vargs;
    NodeTypeArr      type_arr;
    NodeTypeFn       type_fn;
    NodeTypeStruct   type_struct;
    NodeTypeEnum     type_enum;
    NodeTypePtr      type_ptr;
    NodeLitFn        lit_fn;
    NodeLitInt       lit_int;
    NodeLitFloat     lit_float;
    NodeLitChar      lit_char;
    NodeLitString    lit_string;
    NodeLitBool      lit_bool;
    NodeLitCmp       lit_cmp;
    NodeExprCast     expr_cast;
    NodeExprBinop    expr_binop;
    NodeExprCall     expr_call;
    NodeExprMember   expr_member;
    NodeExprElem     expr_elem;
    NodeExprSizeof   expr_sizeof;
    NodeExprTypeof   expr_typeof;
    NodeExprUnary    expr_unary;
    NodeExprNull     expr_null;
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

typedef struct
{
  Node *  node; /* dependent node */
  DepType type; /* is dependency strict (ex.: caused by #run directive) */
} Dependency;

extern const char *node_names[];

/* OLD */
extern Node        ftypes[];
extern Node        type_type;
extern const char *ftype_strings[];
extern const char *buildin_strings[];
extern uint64_t    ftype_hashes[FTYPE_COUNT];
extern uint64_t    buildin_hashes[BUILDIN_COUNT];
/* OLD END */

void
ast_init(struct Arena *arena);

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
  return node_names[ast_node_code(n)];
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
ast_node_dup(struct Arena *arena, Node *node);

Dependency *
ast_add_dep_uq(Node *decl, Node *dep, int type);

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/

#define _NODE_PEEK(_name, _code, _type)                                                            \
  static inline _type *ast_peek_##_name(Node *n)                                                   \
  {                                                                                                \
    assert(n->code == _code);                                                                      \
    return (_type *)n;                                                                             \
  }

_NODE_PEEK(bad, NODE_BAD, NodeBad)
_NODE_PEEK(load, NODE_LOAD, NodeLoad)
_NODE_PEEK(link, NODE_LINK, NodeLink)
_NODE_PEEK(ident, NODE_IDENT, NodeIdent)
_NODE_PEEK(ublock, NODE_UBLOCK, NodeUBlock)
_NODE_PEEK(block, NODE_BLOCK, NodeBlock)
_NODE_PEEK(stmt_return, NODE_STMT_RETURN, NodeStmtReturn)
_NODE_PEEK(stmt_if, NODE_STMT_IF, NodeStmtIf)
_NODE_PEEK(stmt_loop, NODE_STMT_LOOP, NodeStmtLoop)
_NODE_PEEK(stmt_break, NODE_STMT_BREAK, NodeStmtBreak)
_NODE_PEEK(stmt_continue, NODE_STMT_CONTINUE, NodeStmtContinue)
_NODE_PEEK(decl, NODE_DECL, NodeDecl)
_NODE_PEEK(member, NODE_MEMBER, NodeMember)
_NODE_PEEK(arg, NODE_ARG, NodeArg)
_NODE_PEEK(variant, NODE_VARIANT, NodeVariant)
_NODE_PEEK(type_type, NODE_TYPE_TYPE, NodeTypeType)
_NODE_PEEK(type_fund, NODE_TYPE_FUND, NodeTypeFund)
_NODE_PEEK(type_vargs, NODE_TYPE_VARGS, NodeTypeVArgs)
_NODE_PEEK(type_arr, NODE_TYPE_ARR, NodeTypeArr)
_NODE_PEEK(type_fn, NODE_TYPE_FN, NodeTypeFn)
_NODE_PEEK(type_struct, NODE_TYPE_STRUCT, NodeTypeStruct)
_NODE_PEEK(type_enum, NODE_TYPE_ENUM, NodeTypeEnum)
_NODE_PEEK(type_ptr, NODE_TYPE_PTR, NodeTypePtr)
_NODE_PEEK(lit_fn, NODE_LIT_FN, NodeLitFn)
_NODE_PEEK(lit_int, NODE_LIT_INT, NodeLitInt)
_NODE_PEEK(lit_float, NODE_LIT_FLOAT, NodeLitFloat)
_NODE_PEEK(lit_char, NODE_LIT_CHAR, NodeLitChar)
_NODE_PEEK(lit_string, NODE_LIT_STRING, NodeLitString)
_NODE_PEEK(lit_bool, NODE_LIT_BOOL, NodeLitBool)
_NODE_PEEK(lit_cmp, NODE_LIT_CMP, NodeLitCmp)
_NODE_PEEK(expr_cast, NODE_EXPR_CAST, NodeExprCast)
_NODE_PEEK(expr_binop, NODE_EXPR_BINOP, NodeExprBinop)
_NODE_PEEK(expr_call, NODE_EXPR_CALL, NodeExprCall)
_NODE_PEEK(expr_member, NODE_EXPR_MEMBER, NodeExprMember)
_NODE_PEEK(expr_elem, NODE_EXPR_ELEM, NodeExprElem)
_NODE_PEEK(expr_sizeof, NODE_EXPR_SIZEOF, NodeExprSizeof)
_NODE_PEEK(expr_typeof, NODE_EXPR_TYPEOF, NodeExprTypeof)
_NODE_PEEK(expr_unary, NODE_EXPR_UNARY, NodeExprUnary)
_NODE_PEEK(expr_null, NODE_EXPR_NULL, NodeExprNull)

#undef _NODE_PEEK

/*************************************************************************************************
 * generate constructors definitions
 *************************************************************************************************/

#define _NODE_CTOR(name, ...)                                                                      \
  Node *ast_create_##name(struct Arena *_arena, Token *_tok, ##__VA_ARGS__)

_NODE_CTOR(bad);
_NODE_CTOR(load, const char *filepath);
_NODE_CTOR(link, const char *lib);
_NODE_CTOR(ublock, struct Unit *unit, Scope *scope);
_NODE_CTOR(block, Node *nodes, Scope *scope);
_NODE_CTOR(ident, const char *str, Scope *scope);
_NODE_CTOR(stmt_return, Node *expr, Node *fn);
_NODE_CTOR(stmt_if, Node *test, Node *true_stmt, Node *false_stmt);
_NODE_CTOR(stmt_loop, Node *init, Node *condition, Node *increment, Node *block, Scope *scope);
_NODE_CTOR(stmt_break);
_NODE_CTOR(stmt_continue);
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
_NODE_CTOR(lit_cmp, Node *type, Node *fields, int fieldc, Scope *scope);
_NODE_CTOR(lit_fn, Node *type, Node *block, Scope *scope);
_NODE_CTOR(lit_int, uint64_t i);
_NODE_CTOR(lit_float, float f);
_NODE_CTOR(lit_char, uint8_t c);
_NODE_CTOR(lit_string, const char *s);
_NODE_CTOR(lit_bool, bool b);
_NODE_CTOR(expr_binop, Node *lhs, Node *rhs, Node *type, BinopKind kind);
_NODE_CTOR(expr_call, Node *ref, Node *args, int argsc, Node *type, bool run);
_NODE_CTOR(expr_member, MemberKind kind, Node *ident, Node *next, Node *type, bool ptr_ref, int i);
_NODE_CTOR(expr_elem, Node *next, Node *type, Node *index);
_NODE_CTOR(expr_sizeof, Node *in, Node *type);
_NODE_CTOR(expr_typeof, Node *in, Node *type);
_NODE_CTOR(expr_cast, Node *type, Node *next);
_NODE_CTOR(expr_unary, UnopKind kind, Node *next, Node *type);
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
