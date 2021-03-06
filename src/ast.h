// =================================================================================================
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
// =================================================================================================

#ifndef BL_AST_H
#define BL_AST_H

#include "arena.h"
#include "common.h"

#define AST_IS_BAD(node) ((node) && (node)->kind == AST_BAD)
#define AST_IS_OK(node) ((node) && (node)->kind != AST_BAD)

struct Scope;
struct Token;
struct Location;
typedef struct Ast Ast;

typedef enum {
#define GEN_AST_KINDS
#include "ast.inc"
#undef GEN_AST_KINDS
} AstKind;

enum AstFlag {
    FLAG_EXTERN = 1 << 0, // methods marked as extern
    // 1 << 1, free
    FLAG_COMPILER     = 1 << 2,  // compiler internal
    FLAG_PRIVATE      = 1 << 3,  // declared in private scope
    FLAG_INLINE       = 1 << 4,  // inline function
    FLAG_NO_INLINE    = 1 << 5,  // no inline function
    FLAG_ENTRY        = 1 << 6,  // marking entry point function
    FLAG_BUILD_ENTRY  = 1 << 7,  // marking build entry point function
    FLAG_NO_INIT      = 1 << 8,  // no default initialization
    FLAG_INTRINSIC    = 1 << 9,  // intrinsics declaration
    FLAG_TEST_FN      = 1 << 10, // test function
    FLAG_EXPORT       = 1 << 11, // symbols marked for dll export
    FLAG_THREAD_LOCAL = 1 << 12, // symbols marked as thread local
};

// map symbols to binary operation kind
typedef enum {
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
    BINOP_AND,
    BINOP_OR,
    BINOP_SHR,
    BINOP_SHL,
} BinopKind;

typedef enum {
    UNOP_INVALID = 0,
    UNOP_NEG,
    UNOP_POS,
    UNOP_NOT,
    UNOP_BIT_NOT,
} UnopKind;

struct AstDocs {
    const char *text;
};

struct AstLoad {
    const char *filepath;
};

struct AstImport {
    const char *filepath;
};

struct AstPrivate {
    void *_;
};

struct AstScope {
    Ast *ident;
};

struct AstLink {
    const char *lib;
};

struct AstIdent {
    ID id;

    // Optional other identificator (group);
    Ast *next;
};

struct AstRef {
    Ast *ident;
    Ast *next;
};

struct AstUBlock {
    TArray *     nodes;
    struct Unit *unit;
};

struct AstBlock {
    TSmallArray_AstPtr *nodes;
    bool                has_return;
};

struct AstTestCase {
    const char *desc;
    Ast *       block;
};

struct AstStmtReturn {
    // Optional return values.
    TSmallArray_AstPtr *exprs;
    Ast *               fn_decl;
    Ast *               owner_block;
};

struct AstStmtDefer {
    Ast *expr;
};

struct AstStmtIf {
    Ast *test;
    Ast *true_stmt;
    Ast *false_stmt;
};

struct AstStmtSwitch {
    Ast *               expr;
    TSmallArray_AstPtr *cases;
};

struct AstStmtCase {
    TSmallArray_AstPtr *exprs;
    Ast *               block;
    bool                is_default;
};

struct AstStmtLoop {
    Ast *init;
    Ast *condition;
    Ast *increment;
    Ast *block;
};

struct AstDecl {
    Ast *name;
    Ast *type;
    Ast *tags; // Optional.
};

struct AstDeclEntity {
    struct AstDecl base;
    Ast *          value;
    Ast *          explicit_linkage_name; // Optional.
    u32            flags;
    bool           is_global;
    bool           mut;
};

struct AstDeclMember {
    struct AstDecl base;
};

struct AstDeclArg {
    struct AstDecl base;
    Ast *          value;
};

struct AstDeclVariant {
    struct AstDecl base;
    Ast *          value;
};

struct AstTypeArr {
    Ast *elem_type;
    Ast *len;
};

struct AstTypePoly {
    Ast *ident;
};

struct AstTypeSlice {
    Ast *elem_type;
};

struct AstTypeDynArr {
    Ast *elem_type;
};

struct AstTypeFn {
    Ast *               ret_type;
    TSmallArray_AstPtr *args;
    bool                is_polymorph;
};

struct AstTypeFnGroup {
    TSmallArray_AstPtr *variants;
};

struct AstTypeStruct {
    struct Scope *      scope;
    TSmallArray_AstPtr *members;
    Ast *               base_type;
    bool                is_union;
    bool                is_multiple_return_type;
};

struct AstTypeEnum {
    struct Scope *      scope;
    Ast *               type;
    TSmallArray_AstPtr *variants;
};

struct AstTypePtr {
    Ast *type;
};

struct AstTypeVargs {
    Ast *type;
};

struct AstExprType {
    Ast *type;
};

struct AstExprCompound {
    Ast *               type;
    TSmallArray_AstPtr *values;
    // Allow type infer from function return type.
    bool is_multiple_return_value;
};

struct AstExprLitFn {
    Ast *type;
    Ast *block;
};

struct AstExprLitFnGroup {
    TSmallArray_AstPtr *variants;
};

struct AstExprLitInt {
    u64  val;
    bool overflow;
};

struct AstExprLitFloat {
    f32  val;
    bool overflow;
};

struct AstExprLitDouble {
    f64  val;
    bool overflow;
};

struct AstExprLitChar {
    u8 val;
};

struct AstExprLitString {
    const char *val;
};

struct AstExprLitBool {
    bool val;
};

struct AstExprCast {
    Ast *type;
    Ast *next;
    bool auto_cast;
};

struct AstExprBinop {
    Ast *     lhs;
    Ast *     rhs;
    BinopKind kind;
};

struct AstExprCall {
    Ast *               ref;
    TSmallArray_AstPtr *args;
    bool                run;
};

struct AstExprElem {
    Ast *next;
    Ast *index;
};

struct AstExprSizeof {
    Ast *node;
};

struct AstExprTypeInfo {
    Ast *node;
};

struct AstExprAlignof {
    Ast *node;
};

struct AstExprUnary {
    UnopKind kind;
    Ast *    next;
};

struct AstExprAddrOf {
    Ast *next;
};

struct AstExprDeref {
    Ast *next;
};

struct AstTags {
    TSmallArray_AstPtr *values;
};

struct AstCallLoc {
    void *_;
};

// AST base type
struct Ast {
    AstKind          kind;
    struct Location *location;    // Location in source file.
    struct Scope *   owner_scope; // Scope in which is AST node.
    const char *     docs;        // Optional documentation string.

    union {
#define GEN_AST_DATA
#include "ast.inc"
#undef GEN_AST_DATA
    } data;

#if BL_DEBUG
    u64 _serial;
#endif
};

void ast_arena_init(Arena *arena);
void ast_arena_terminate(Arena *arena);
Ast *ast_create_node(struct Arena *arena, AstKind c, struct Token *tok, struct Scope *parent_scope);
const char *ast_binop_to_str(BinopKind op);
const char *ast_unop_to_str(UnopKind op);
const char *ast_get_name(const Ast *n);

static INLINE bool ast_binop_is_logic(BinopKind op)
{
    return op >= BINOP_EQ && op <= BINOP_LOGIC_OR;
}

#endif
