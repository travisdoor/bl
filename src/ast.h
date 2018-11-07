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

typedef struct AstDecl        AstDecl;
typedef struct AstDeclEntity  AstDeclEntity;
typedef struct AstDeclMember  AstDeclMember;
typedef struct AstDeclArg     AstDeclArg;
typedef struct AstDeclVariant AstDeclVariant;

typedef struct AstExprLitFn     AstExprLitFn;
typedef struct AstExprLitInt    AstExprLitInt;
typedef struct AstExprLitFloat  AstExprLitFloat;
typedef struct AstExprLitDouble AstExprLitDouble;
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
typedef struct AstTypeVoid   AstTypeVoid;
typedef struct AstTypeType   AstTypeType;
typedef struct AstTypeRef    AstTypeRef;
typedef struct AstTypeInt    AstTypeInt;
typedef struct AstTypeReal   AstTypeReal;
typedef struct AstTypeBool   AstTypeBool;
typedef struct AstTypeVArgs  AstTypeVArgs;
typedef struct AstTypeArr    AstTypeArr;
typedef struct AstTypeFn     AstTypeFn;
typedef struct AstTypeStruct AstTypeStruct;
typedef struct AstTypeEnum   AstTypeEnum;
typedef struct AstTypePtr    AstTypePtr;

typedef struct Dependency Dependency;

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
  AST_TYPE,
  AST_EXPR,
  AST_COUNT
} AstKind;

typedef enum
{
  AST_TYPE_BAD,
  AST_TYPE_VOID,
  AST_TYPE_TYPE,
  AST_TYPE_REF,
  AST_TYPE_INT,
  AST_TYPE_REAL,
  AST_TYPE_BOOL,
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
  AST_EXPR_LIT_DOUBLE,
  AST_EXPR_LIT_CHAR,
  AST_EXPR_LIT_STRING,
  AST_EXPR_LIT_BOOL,
  AST_EXPR_LIT_CMP,
} AstExprKind;

typedef enum
{
  AST_DECL_BAD,
  AST_DECL_ENTITY,
  AST_DECL_MEMBER,
  AST_DECL_ARG,
  AST_DECL_VARIANT,
} AstDeclKind;

typedef enum
{
  DECL_ENTITY_INVALID = 0,
  DECL_ENTITY_FIELD   = 1, /* a := 0; */
  DECL_ENTITY_TYPE    = 2, /* Type : struct { s32 }; */
  DECL_ENTITY_FN      = 3, /* main : fn () s32 {}; */
  DECL_ENTITY_ENUM    = 4, /* Enum : enum s8 {}; */
} DeclEntityKind;

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
  BINOP_INVALID = 0,
  BINOP_ASSIGN,
  BINOP_ADD_ASSIGN,
  BINOP_SUB_ASSIGN,
  BINOP_MUL_ASSIGN,
  BINOP_DIV_ASSIGN,
  BINOP_MOD_ASSIGN,
  BINOP_ADD,
  BINOP_SUB,
  BINOP_MUL,
  BINOP_DIV,
  BINOP_MOD,
  BINOP_EQ,
  BINOP_NEQ,
  BINOP_GREATER,
  BINOP_LESS,
  BINOP_GREATER_EQ,
  BINOP_LESS_EQ,
  BINOP_LOGIC_AND,
  BINOP_LOGIC_OR,
} BinopKind;

typedef enum
{
  UNOP_INVALID = 0,
  UNOP_NEG,
  UNOP_POS,
  UNOP_NOT,
  UNOP_ADR,
  UNOP_DEREF,
} UnopKind;

typedef enum
{
  DEP_LAX    = 1 << 0, /* dependency is't needed for successful IR construction */
  DEP_STRICT = 1 << 1, /* dependency must be linked for successful IR construction */
} DepType;

typedef enum
{
  ADR_MODE_INVALID,
  ADR_MODE_NO_VALUE,
  ADR_MODE_MUT,
  ADR_MODE_IMMUT,
  ADR_MODE_CONST,
} AdrMode;

/* AST base type */
struct Ast
{
  AstKind kind;
  Src *   src;
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

struct AstDecl
{
  Ast         base;
  AstDeclKind kind;
  AstType *   type;
  AstIdent *  name;
};

struct AstType
{
  Ast         base;
  AstTypeKind kind;
};

struct AstExpr
{
  Ast         base;
  AstExprKind kind;
  AstType *   type;
  AdrMode     adr_mode;
};

struct AstLoad
{
  Ast         base;
  const char *filepath;
};

struct AstLink
{
  Ast         base;
  const char *lib;
};

struct AstIdent
{
  Ast         base;
  Scope *     scope;
  const char *str;
  uint64_t    hash;
};

struct AstUBlock
{
  Ast          base;
  BArray *     nodes;
  struct Unit *unit;
};

struct AstBlock
{
  Ast     base;
  BArray *nodes;
};

struct AstStmtReturn
{
  Ast            base;
  AstExpr *      expr;
  AstDeclEntity *fn_decl;
};

struct AstStmtIf
{
  Ast      base;
  AstExpr *test;
  Ast *    true_stmt;
  Ast *    false_stmt;
};

struct AstStmtLoop
{
  Ast      base;
  AstDecl *init;
  AstExpr *condition;
  AstExpr *increment;
  Ast *    block;
};

struct AstStmtBreak
{
  Ast   base;
  void *_;
};

struct AstStmtContinue
{
  Ast   base;
  void *_;
};

struct AstDeclEntity
{
  AstDecl        base;
  DeclEntityKind kind;
  AstExpr *      value;
  int            flags;
  int            used;
  bool           in_gscope;
  bool mutable;
  BHashTable *deps;
};

struct AstDeclMember
{
  AstDecl base;
  int     order;
};

struct AstDeclArg
{
  AstDecl base;
  void *  _;
};

struct AstDeclVariant
{
  AstDecl  base;
  AstExpr *value;
};

struct AstTypeType
{
  AstType     base;
  const char *name;
  AstType *   spec;
};

struct AstTypeVoid
{
  AstType base;
  void *  _;
};

struct AstTypeInt
{
  AstType     base;
  const char *name;
  bool        is_signed;
  int         bitcount;
};

struct AstTypeReal
{
  AstType     base;
  const char *name;
  int         bitcount;
};

struct AstTypeBool
{
  AstType base;
};

struct AstTypeVArgs
{
  AstType base;
  void *  _;
};

struct AstTypeArr
{
  AstType  base;
  AstType *elem_type;
  AstExpr *len;
};

struct AstTypeFn
{
  AstType  base;
  AstType *ret_type;
  BArray * args;
};

struct AstTypeStruct
{
  AstType base;
  BArray *members;
  bool    raw;
};

struct AstTypeEnum
{
  AstType  base;
  AstType *type;
  BArray * variants;
};

struct AstTypePtr
{
  AstType  base;
  AstType *type;
};

struct AstTypeRef
{
  AstType   base;
  AstType * type;
  AstIdent *ident;
};

struct AstExprLitFn
{
  AstExpr base;
  Ast *   block;
};

struct AstExprLitInt
{
  AstExpr  base;
  uint64_t val;
};

struct AstExprLitFloat
{
  AstExpr base;
  float   val;
};

struct AstExprLitDouble
{
  AstExpr base;
  double  val;
};

struct AstExprLitChar
{
  AstExpr base;
  uint8_t val;
};

struct AstExprLitString
{
  AstExpr     base;
  const char *val;
};

struct AstExprLitBool
{
  AstExpr base;
  bool    val;
};

struct AstExprLitCmp
{
  AstExpr base;
  BArray *fields;
};

struct AstExprRef
{
  AstExpr   base;
  AstIdent *ident;
  AstDecl * ref;
};

struct AstExprCast
{
  AstExpr  base;
  AstType *type;
  AstExpr *next;
};

struct AstExprBinop
{
  AstExpr   base;
  AstExpr * lhs;
  AstExpr * rhs;
  BinopKind kind;
};

struct AstExprCall
{
  AstExpr  base;
  AstExpr *ref;
  BArray * args;
  bool     run;
};

struct AstExprMember
{
  AstExpr    base;
  MemberKind kind;
  AstIdent * ident;
  AstExpr *  next;
  bool       ptr_ref;
  int        i;
};

struct AstExprElem
{
  AstExpr  base;
  AstExpr *next;
  AstExpr *index;
};

struct AstExprSizeof
{
  AstExpr  base;
  AstExpr *in;
};

struct AstExprTypeof
{
  AstExpr  base;
  AstExpr *in;
};

struct AstExprUnary
{
  AstExpr  base;
  UnopKind kind;
  AstExpr *next;
};

struct AstExprNull
{
  AstExpr base;
  void *  _;
};

struct Dependency
{
  AstDeclEntity *decl; /* dependent node */
  DepType        type; /* is dependency strict (ex.: caused by #run directive) */
};

void
ast_arena_init(struct Arena *arena);

static inline bool
ast_binop_is_assign(BinopKind op)
{
  return op >= BINOP_ASSIGN && op <= BINOP_MOD_ASSIGN;
}

#define ast_create_node(arena, c, tok, t) (t) _ast_create_node((arena), (c), (tok));
#define ast_create_decl(arena, c, tok, t) (t) _ast_create_decl((arena), (c), (tok));
#define ast_create_type(arena, c, tok, t) (t) _ast_create_type((arena), (c), (tok));
#define ast_create_expr(arena, c, tok, t) (t) _ast_create_expr((arena), (c), (tok));

Ast *
_ast_create_node(struct Arena *arena, AstKind c, Token *tok);

AstType *
_ast_create_type(struct Arena *arena, AstTypeKind c, Token *tok);

AstExpr *
_ast_create_expr(struct Arena *arena, AstExprKind c, Token *tok);

AstDecl *
_ast_create_decl(struct Arena *arena, AstDeclKind c, Token *tok);

Dependency *
ast_add_dep_uq(AstDeclEntity *decl, AstDeclEntity *dep, int type);

const char *
ast_get_name(const Ast *n);

void
ast_type_to_str(char *buf, int len, AstType *type);

#endif
