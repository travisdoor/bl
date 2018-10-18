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
typedef struct Ast             Ast;
typedef struct AstBad          AstBad;
typedef struct AstLoad         AstLoad;
typedef struct AstLink         AstLink;
typedef struct AstIdent        AstIdent;
typedef struct AstUBlock       AstUBlock;
typedef struct AstBlock        AstBlock;
typedef struct AstStmtReturn   AstStmtReturn;
typedef struct AstStmtIf       AstStmtIf;
typedef struct AstStmtLoop     AstStmtLoop;
typedef struct AstStmtBreak    AstStmtBreak;
typedef struct AstStmtContinue AstStmtContinue;
typedef struct AstDecl         AstDecl;
typedef struct AstMember       AstMember;
typedef struct AstArg          AstArg;
typedef struct AstVariant      AstVariant;
typedef struct AstType         AstType;
typedef struct AstTypeType     AstTypeType;
typedef struct AstTypeInt      AstTypeInt;
typedef struct AstTypeVArgs    AstTypeVArgs;
typedef struct AstTypeArr      AstTypeArr;
typedef struct AstTypeFn       AstTypeFn;
typedef struct AstTypeStruct   AstTypeStruct;
typedef struct AstTypeEnum     AstTypeEnum;
typedef struct AstTypePtr      AstTypePtr;
typedef struct AstLitFn        AstLitFn;
typedef struct AstLitInt       AstLitInt;
typedef struct AstLitFloat     AstLitFloat;
typedef struct AstLitChar      AstLitChar;
typedef struct AstLitString    AstLitString;
typedef struct AstLitBool      AstLitBool;
typedef struct AstLitCmp       AstLitCmp;
typedef struct AstExprCast     AstExprCast;
typedef struct AstExprBinop    AstExprBinop;
typedef struct AstExprCall     AstExprCall;
typedef struct AstExprMember   AstExprMember;
typedef struct AstExprElem     AstExprElem;
typedef struct AstExprSizeof   AstExprSizeof;
typedef struct AstExprTypeof   AstExprTypeof;
typedef struct AstExprUnary    AstExprUnary;
typedef struct AstExprNull     AstExprNull;

#define node_foreach(_root, _it) for ((_it) = (_root); (_it); (_it) = (_it)->next)
#define node_foreach_ref(_root, _it) for ((_it) = &(_root); *(_it); (_it) = &((*(_it))->next))

typedef enum
{
  AST_BAD,
  AST_LOAD,
  AST_LINK,
  AST_IDENT,
  AST_UBLOCK,
  AST_BLOCK,
  AST_STMT_RETURN,
  AST_STMT_IF,
  AST_STMT_LOOP,
  AST_STMT_BREAK,
  AST_STMT_CONTINUE,
  AST_DECL,
  AST_MEMBER,
  AST_ARG,
  AST_VARIANT,
  AST_TYPE_TYPE,
  AST_TYPE_INT,
  AST_TYPE_VARGS,
  AST_TYPE_ARR,
  AST_TYPE_FN,
  AST_TYPE_STRUCT,
  AST_TYPE_ENUM,
  AST_TYPE_PTR,
  AST_LIT_FN,
  AST_LIT_INT,
  AST_LIT_FLOAT,
  AST_LIT_CHAR,
  AST_LIT_STRING,
  AST_LIT_BOOL,
  AST_LIT_CMP,
  AST_EXPR_CAST,
  AST_EXPR_BINOP,
  AST_EXPR_CALL,
  AST_EXPR_MEMBER,
  AST_EXPR_ELEM,
  AST_EXPR_SIZEOF,
  AST_EXPR_TYPEOF,
  AST_EXPR_UNARY,
  AST_EXPR_NULL,
  AST_COUNT
} AstCode;

typedef enum
{
  AST_BUILDIN_TYPE_U8 = 0,
  AST_BUILDIN_TYPE_U16,
  AST_BUILDIN_TYPE_U32,
  AST_BUILDIN_TYPE_U64,
  AST_BUILDIN_TYPE_USIZE,
  AST_BUILDIN_TYPE_S8,
  AST_BUILDIN_TYPE_S16,
  AST_BUILDIN_TYPE_S32,
  AST_BUILDIN_TYPE_S64,
  AST_BUILDIN_TYPE_COUNT,
} AstBuildinType;

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
} AstFlag;

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

struct AstBad
{
  void *_;
};

struct AstLoad
{
  const char *filepath;
};

struct AstLink
{
  const char *lib;
};

struct AstIdent
{
  Scope *     scope;
  AstType *   type;
  const char *str;
  uint64_t    hash;
};

struct AstUBlock
{
  Ast *        nodes;
  struct Unit *unit;
};

struct AstBlock
{
  Ast *nodes;
};

struct AstStmtReturn
{
  Ast *expr;
  Ast *fn_decl;
};

struct AstStmtIf
{
  Ast *test;
  Ast *true_stmt;
  Ast *false_stmt;
};

struct AstStmtLoop
{
  Ast *init;
  Ast *condition;
  Ast *increment;
  Ast *block;
};

struct AstStmtBreak
{
  void *_;
};

struct AstStmtContinue
{
  void *_;
};

struct AstDecl
{
  Ast *     type;
  DeclKind  kind;
  AstIdent *name;
  Ast *     value;
  bool mutable;
  int         flags;
  int         used;
  bool        in_gscope;
  BHashTable *deps;
};

struct AstMember
{
  AstIdent *name;
  Ast *     type;
  int       order;
};

struct AstArg
{
  AstIdent *name;
  Ast *     type;
};

struct AstVariant
{
  AstIdent *name;
  Ast *     type;
  Ast *     value;
};

struct AstType
{
  const char *name;
};

struct AstTypeType
{
  Ast *name;
  Ast *spec;
};

struct AstTypeInt
{
  struct AstType base;
  bool           is_signed;
  int            bitcount;
};

struct AstTypeVArgs
{
  void *_;
};

struct AstTypeArr
{
  Ast *elem_type;
  Ast *len;
};

struct AstTypeFn
{
  Ast *ret_type;
  Ast *arg_types;
  int  argc_types;
};

struct AstTypeStruct
{
  Ast *members;
  int  membersc;
  bool raw;
};

struct AstTypeEnum
{
  Ast *type;
  Ast *variants;
};

struct AstTypePtr
{
  Ast *type;
};

struct AstLitFn
{
  Ast *type;
  Ast *block;
};

struct AstLitInt
{
  AstType *type;
  uint64_t i;
};

struct AstLitFloat
{
  AstType *type;
  float    f;
};

struct AstLitChar
{
  AstType *type;
  uint8_t  c;
};

struct AstLitString
{
  AstType *   type;
  const char *s;
};

struct AstLitBool
{
  AstType *type;
  bool     b;
};

struct AstLitCmp
{
  Ast *type;
  Ast *fields;
  int  fieldc;
};

struct AstExprCast
{
  Ast *type;
  Ast *next;
};

struct AstExprBinop
{
  Ast *     lhs;
  Ast *     rhs;
  BinopKind kind;
};

struct AstExprCall
{
  Ast *ref;
  Ast *args;
  int  argsc;
  bool run;
};

struct AstExprMember
{
  MemberKind kind;
  Ast *      ident;
  Ast *      next;
  bool       ptr_ref;
  int        i;
};

struct AstExprElem
{
  Ast *next;
  Ast *index;
};

struct AstExprSizeof
{
  Ast *in;
};

struct AstExprTypeof
{
  Ast *in;
};

struct AstExprUnary
{
  UnopKind kind;
  Ast *    next;
};

struct AstExprNull
{
  void *_;
};

struct Ast
{
  union
  {
    AstBad          bad;
    AstLoad         load;
    AstLink         link;
    AstIdent        ident;
    AstUBlock       ublock;
    AstBlock        block;
    AstStmtReturn   stmt_return;
    AstStmtIf       stmt_if;
    AstStmtLoop     stmt_loop;
    AstStmtBreak    stmt_break;
    AstStmtContinue stmt_continue;
    AstDecl         decl;
    AstMember       member;
    AstArg          arg;
    AstVariant      variant;
    AstTypeType     type_type;
    AstTypeInt      type_int;
    AstTypeVArgs    type_vargs;
    AstTypeArr      type_arr;
    AstTypeFn       type_fn;
    AstTypeStruct   type_struct;
    AstTypeEnum     type_enum;
    AstTypePtr      type_ptr;
    AstLitFn        lit_fn;
    AstLitInt       lit_int;
    AstLitFloat     lit_float;
    AstLitChar      lit_char;
    AstLitString    lit_string;
    AstLitBool      lit_bool;
    AstLitCmp       lit_cmp;
    AstExprCast     expr_cast;
    AstExprBinop    expr_binop;
    AstExprCall     expr_call;
    AstExprMember   expr_member;
    AstExprElem     expr_elem;
    AstExprSizeof   expr_sizeof;
    AstExprTypeof   expr_typeof;
    AstExprUnary    expr_unary;
    AstExprNull     expr_null;
  };

  Src *   src;
  AstCode code;
  Ast *   next;
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
  Ast *   node; /* dependent node */
  DepType type; /* is dependency strict (ex.: caused by #run directive) */
} Dependency;

void
ast_init(struct Arena *arena);

static inline Src *
ast_peek_src(Ast *n)
{
  return n->src;
}

static inline AstCode
ast_code(Ast *n)
{
  return n->code;
}

static inline bool
ast_is(Ast *n,
       AstCode c)
{
  return ast_code(n) == c;
}

static inline bool
ast_is_not(Ast *n,
           AstCode c)
{
  return ast_code(n) != c;
}

static inline bool
ast_is_type(Ast *node)
{
  return ast_code(node) >= AST_TYPE_TYPE && ast_code(node) <= AST_TYPE_PTR;
}

#define ast_create_node(arena, c, tok, t) (t) _ast_create_node((arena), (c), (tok));

Ast *
_ast_create_node(struct Arena *arena, AstCode c, Token *tok);

AstType *
ast_get_buildin_type(AstBuildinType c);

Ast *
ast_dup(struct Arena *arena,
        Ast *node);

Dependency *
ast_add_dep_uq(Ast *decl, Ast *dep, int type);

const char *
ast_get_name(Ast *n);

AstType *
ast_get_type(Ast *n);

/*************************************************************************************************
 * generation of peek function
 * note: in debug mode function will check validity of node type
 *************************************************************************************************/

#define _AST_PEEK(_name, _code, _type)                                                             \
  static inline _type *ast_peek_##_name(Ast *n)                                                    \
  {                                                                                                \
    assert(n->code == _code);                                                                      \
    return (_type *)n;                                                                             \
  }

_AST_PEEK(bad, AST_BAD, AstBad)
_AST_PEEK(load, AST_LOAD, AstLoad)
_AST_PEEK(link, AST_LINK, AstLink)
_AST_PEEK(ident, AST_IDENT, AstIdent)
_AST_PEEK(ublock, AST_UBLOCK, AstUBlock)
_AST_PEEK(block, AST_BLOCK, AstBlock)
_AST_PEEK(stmt_return, AST_STMT_RETURN, AstStmtReturn)
_AST_PEEK(stmt_if, AST_STMT_IF, AstStmtIf)
_AST_PEEK(stmt_loop, AST_STMT_LOOP, AstStmtLoop)
_AST_PEEK(stmt_break, AST_STMT_BREAK, AstStmtBreak)
_AST_PEEK(stmt_continue, AST_STMT_CONTINUE, AstStmtContinue)
_AST_PEEK(decl, AST_DECL, AstDecl)
_AST_PEEK(member, AST_MEMBER, AstMember)
_AST_PEEK(arg, AST_ARG, AstArg)
_AST_PEEK(variant, AST_VARIANT, AstVariant)
_AST_PEEK(type_type, AST_TYPE_TYPE, AstTypeType)
_AST_PEEK(type_vargs, AST_TYPE_VARGS, AstTypeVArgs)
_AST_PEEK(type_arr, AST_TYPE_ARR, AstTypeArr)
_AST_PEEK(type_fn, AST_TYPE_FN, AstTypeFn)
_AST_PEEK(type_struct, AST_TYPE_STRUCT, AstTypeStruct)
_AST_PEEK(type_enum, AST_TYPE_ENUM, AstTypeEnum)
_AST_PEEK(type_ptr, AST_TYPE_PTR, AstTypePtr)
_AST_PEEK(lit_fn, AST_LIT_FN, AstLitFn)
_AST_PEEK(lit_int, AST_LIT_INT, AstLitInt)
_AST_PEEK(lit_float, AST_LIT_FLOAT, AstLitFloat)
_AST_PEEK(lit_char, AST_LIT_CHAR, AstLitChar)
_AST_PEEK(lit_string, AST_LIT_STRING, AstLitString)
_AST_PEEK(lit_bool, AST_LIT_BOOL, AstLitBool)
_AST_PEEK(lit_cmp, AST_LIT_CMP, AstLitCmp)
_AST_PEEK(expr_cast, AST_EXPR_CAST, AstExprCast)
_AST_PEEK(expr_binop, AST_EXPR_BINOP, AstExprBinop)
_AST_PEEK(expr_call, AST_EXPR_CALL, AstExprCall)
_AST_PEEK(expr_member, AST_EXPR_MEMBER, AstExprMember)
_AST_PEEK(expr_elem, AST_EXPR_ELEM, AstExprElem)
_AST_PEEK(expr_sizeof, AST_EXPR_SIZEOF, AstExprSizeof)
_AST_PEEK(expr_typeof, AST_EXPR_TYPEOF, AstExprTypeof)
_AST_PEEK(expr_unary, AST_EXPR_UNARY, AstExprUnary)
_AST_PEEK(expr_null, AST_EXPR_NULL, AstExprNull)

#undef _AST_PEEK

/*************************************************************************************************
 * AST visiting
 *************************************************************************************************/

typedef struct Visitor Visitor;
typedef void (*VisitorFunc)(Visitor *visitor, Ast *node, void *cnt);

struct Visitor
{
  VisitorFunc visitors[AST_COUNT];
  VisitorFunc all_visitor;
};

void
visitor_init(Visitor *visitor);

void
visitor_add(Visitor *visitor, VisitorFunc fn, AstCode code);

void
visitor_add_visit_all(Visitor *visitor, VisitorFunc fn);

void
visitor_visit(Visitor *visitor, Ast *node, void *cnt);

void
visitor_walk(Visitor *visitor, Ast *node, void *cnt);

/**************************************************************************************************/

#endif
