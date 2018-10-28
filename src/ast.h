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

typedef struct AstExprLitFn     AstExprLitFn;
typedef struct AstExprLitInt    AstExprLitInt;
typedef struct AstExprLitFloat  AstExprLitFloat;
typedef struct AstExprLitChar   AstExprLitChar;
typedef struct AstExprLitString AstExprLitString;
typedef struct AstExprLitBool   AstExprLitBool;
typedef struct AstExprLitCmp    AstExprLitCmp;
typedef struct AstExpr          AstExpr;
typedef struct AstExprRef       AstExprRef;
typedef struct AstExprCast      AstExprCast;
typedef struct AstExprBinop     AstExprBinop;
typedef struct AstExprCall      AstExprCall;
typedef struct AstExprMember    AstExprMember;
typedef struct AstExprElem      AstExprElem;
typedef struct AstExprSizeof    AstExprSizeof;
typedef struct AstExprTypeof    AstExprTypeof;
typedef struct AstExprUnary     AstExprUnary;
typedef struct AstExprNull      AstExprNull;

typedef struct AstType       AstType;
typedef struct AstTypeType   AstTypeType;
typedef struct AstTypeRef    AstTypeRef;
typedef struct AstTypeInt    AstTypeInt;
typedef struct AstTypeVoid   AstTypeVoid;
typedef struct AstTypeVArgs  AstTypeVArgs;
typedef struct AstTypeArr    AstTypeArr;
typedef struct AstTypeFn     AstTypeFn;
typedef struct AstTypeStruct AstTypeStruct;
typedef struct AstTypeEnum   AstTypeEnum;
typedef struct AstTypePtr    AstTypePtr;

typedef struct Dependency Dependency;

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
  AST_TYPE,
  AST_EXPR,
  AST_COUNT
} AstKind;

typedef enum
{
  AST_TYPE_BAD,
  AST_TYPE_TYPE,
  AST_TYPE_VOID,
  AST_TYPE_REF,
  AST_TYPE_INT,
  AST_TYPE_VARGS,
  AST_TYPE_ARR,
  AST_TYPE_FN,
  AST_TYPE_STRUCT,
  AST_TYPE_ENUM,
  AST_TYPE_PTR,
} AstTypeKind;

typedef enum
{
  AST_EXPR_BAD,
  AST_EXPR_TYPE,
  AST_EXPR_REF,
  AST_EXPR_CAST,
  AST_EXPR_BINOP,
  AST_EXPR_CALL,
  AST_EXPR_MEMBER,
  AST_EXPR_ELEM,
  AST_EXPR_SIZEOF,
  AST_EXPR_TYPEOF,
  AST_EXPR_UNARY,
  AST_EXPR_NULL,
  AST_EXPR_LIT_FN,
  AST_EXPR_LIT_INT,
  AST_EXPR_LIT_FLOAT,
  AST_EXPR_LIT_CHAR,
  AST_EXPR_LIT_STRING,
  AST_EXPR_LIT_BOOL,
  AST_EXPR_LIT_CMP,
} AstExprKind;

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
  FLAG_EXTERN   = 1 << 0, /* methods marked as extern */
  FLAG_MAIN     = 1 << 1, /* main method */
  FLAG_TEST     = 1 << 2, /* test case */
  FLAG_COMPILER = 1 << 3, /* compiler internal flag */
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
  AstExpr *expr;
  Ast *    fn_decl;
};

struct AstStmtIf
{
  AstExpr *test;
  Ast *    true_stmt;
  Ast *    false_stmt;
};

struct AstStmtLoop
{
  Ast *    init;
  AstExpr *condition;
  AstExpr *increment;
  Ast *    block;
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
  AstType * type;
  DeclKind  kind;
  AstIdent *name;
  AstExpr * value;
  bool mutable;
  int         flags;
  int         used;
  bool        in_gscope;
  BHashTable *deps;
};

struct AstMember
{
  AstType * type;
  AstIdent *name;
  int       order;
};

struct AstArg
{
  AstType * type;
  AstIdent *name;
};

struct AstTypeType
{
  const char *name;
  AstType *   spec;
};

struct AstVariant
{
  AstType * type;
  AstIdent *name;
  AstExpr * value;
};

struct AstTypeInt
{
  const char *name;
  bool        is_signed;
  int         bitcount;
};

struct AstTypeVoid
{
  const char *name;
};

struct AstTypeVArgs
{
  void *_;
};

struct AstTypeArr
{
  AstType *elem_type;
  AstExpr *len;
};

struct AstTypeFn
{
  AstType *ret_type;
  Ast *    args;
  int      argc;
};

struct AstTypeStruct
{
  Ast *members;
  int  membersc;
  bool raw;
};

struct AstTypeEnum
{
  AstType *type;
  Ast *    variants;
};

struct AstTypePtr
{
  AstType *type;
};

struct AstTypeRef
{
  AstType * type;
  AstIdent *ident;
};

struct AstExprLitFn
{
  Ast *block;
};

struct AstExprLitInt
{
  uint64_t i;
};

struct AstExprLitFloat
{
  AstType *type;
  float    f;
};

struct AstExprLitChar
{
  AstType *type;
  uint8_t  c;
};

struct AstExprLitString
{
  AstType *   type;
  const char *s;
};

struct AstExprLitBool
{
  AstType *type;
  bool     b;
};

struct AstExprLitCmp
{
  AstType *type;
  AstExpr *fields;
  int      fieldc;
};

struct AstExprRef
{
  AstIdent *ident;
};

struct AstExprCast
{
  AstType *type;
  AstExpr *next;
};

struct AstExprBinop
{
  AstExpr * lhs;
  AstExpr * rhs;
  BinopKind kind;
};

struct AstExprCall
{
  AstExpr *ref;
  AstExpr *args;
  int      argsc;
  bool     run;
};

struct AstExprMember
{
  MemberKind kind;
  Ast *      ident;
  AstExpr *  next;
  bool       ptr_ref;
  int        i;
};

struct AstExprElem
{
  AstExpr *next;
  AstExpr *index;
};

struct AstExprSizeof
{
  AstExpr *in;
};

struct AstExprTypeof
{
  AstExpr *in;
};

struct AstExprUnary
{
  UnopKind kind;
  AstExpr *next;
};

struct AstExprNull
{
  void *_;
};

struct AstType
{
  union
  {
    AstTypeType   type;
    AstTypeRef    ref;
    AstTypeInt    integer;
    AstTypeVoid   mvoid;
    AstTypeVArgs  vargs;
    AstTypeArr    arr;
    AstTypeFn     fn;
    AstTypeStruct strct;
    AstTypeEnum   enm;
    AstTypePtr    ptr;
  };

  AstTypeKind kind;
};

struct AstExpr
{
  union
  {
    AstExprLitFn     fn;
    AstExprLitInt    integer;
    AstExprLitFloat  real;
    AstExprLitChar   character;
    AstExprLitString string;
    AstExprLitBool   boolean;
    AstExprLitCmp    cmp;
    AstExprRef       ref;
    AstExprCast      cast;
    AstExprBinop     binop;
    AstExprCall      call;
    AstExprMember    member;
    AstExprElem      elem;
    AstExprSizeof    szof;
    AstExprTypeof    tpof;
    AstExprUnary     unary;
    AstExprNull      null;
  };

  AstExprKind kind;
  AstType *   type;
};

struct Ast
{
  union
  {
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
    AstType         type;
    AstExpr         expr;
  };

  AstKind kind;
  Src *   src;
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

struct Dependency
{
  AstDecl *decl; /* dependent node */
  DepType  type; /* is dependency strict (ex.: caused by #run directive) */
};

void
ast_arena_init(struct Arena *arena);

static inline AstKind
ast_kind(Ast *n)
{
  return n->kind;
}

static inline AstTypeKind
ast_type_kind(AstType *t)
{
  return t->kind;
}

static inline AstExprKind
ast_expr_kind(AstExpr *e)
{
  return e->kind;
}

static inline bool
ast_is(Ast *n, AstKind c)
{
  return ast_kind(n) == c;
}

static inline bool
ast_is_not(Ast *n, AstKind c)
{
  return ast_kind(n) != c;
}

static inline bool
ast_is_type(Ast *t)
{
  return t->kind == AST_TYPE;
}

static inline AstType *
ast_get_type(AstExpr *expr)
{
  assert(ast_is((Ast *)expr, AST_EXPR));
  return expr->type;
}

#define ast_create_node(arena, c, tok, t) (t) _ast_create_node((arena), (c), (tok));
#define ast_create_type(arena, c, tok, t) (t) _ast_create_type((arena), (c), (tok));
#define ast_create_expr(arena, c, tok, t) (t) _ast_create_expr((arena), (c), (tok));

Ast *
_ast_create_node(struct Arena *arena, AstKind c, Token *tok);

AstType *
_ast_create_type(struct Arena *arena, AstTypeKind c, Token *tok);

AstExpr *
_ast_create_expr(struct Arena *arena, AstExprKind c, Token *tok);

Ast *
ast_dup(struct Arena *arena, Ast *node);

Dependency *
ast_add_dep_uq(AstDecl *decl, AstDecl *dep, int type);

const char *
ast_get_name(Ast *n);

void
ast_type_to_str(char *buf, int len, AstType *type);

#endif
