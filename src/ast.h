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
struct location;
struct ast;

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
    BINOP_AND_ASSIGN,
    BINOP_OR_ASSIGN,
    BINOP_XOR_ASSIGN,
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
    BINOP_XOR,
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
    struct ast *ident;
};

struct AstLink {
    const char *lib;
};

struct AstIdent {
    ID id;

    // Optional other identificator (group);
    struct ast *next;
};

struct AstRef {
    struct ast *ident;
    struct ast *next;
};

struct AstUBlock {
    TArray *     nodes;
    struct unit *unit;
};

struct AstBlock {
    TSmallArray_AstPtr *nodes;
    bool                has_return;
};

struct AstTestCase {
    const char *desc;
    struct ast *block;
};

struct AstStmtReturn {
    // Optional return values.
    TSmallArray_AstPtr *exprs;
    struct ast *        fn_decl;
    struct ast *        owner_block;
};

struct AstStmtDefer {
    struct ast *expr;
};

struct AstStmtIf {
    struct ast *test;
    struct ast *true_stmt;
    struct ast *false_stmt;
};

struct AstStmtSwitch {
    struct ast *        expr;
    TSmallArray_AstPtr *cases;
};

struct AstStmtCase {
    TSmallArray_AstPtr *exprs;
    struct ast *        block;
    bool                is_default;
};

struct AstStmtLoop {
    struct ast *init;
    struct ast *condition;
    struct ast *increment;
    struct ast *block;
};

struct AstDecl {
    struct ast *name;
    struct ast *type;
    struct ast *tags; // Optional.
};

struct AstDeclEntity {
    struct AstDecl base;
    struct ast *   value;
    struct ast *   explicit_linkage_name; // Optional.
    u32            flags;
    bool           is_global;
    bool           mut;
};

struct AstDeclMember {
    struct AstDecl base;
};

struct AstDeclArg {
    struct AstDecl base;
    struct ast *   value;
};

struct AstDeclVariant {
    struct AstDecl base;
    struct ast *   value;
};

struct AstTypeArr {
    struct ast *elem_type;
    struct ast *len;
};

struct AstTypePoly {
    struct ast *ident;
};

struct AstTypeSlice {
    struct ast *elem_type;
};

struct AstTypeDynArr {
    struct ast *elem_type;
};

struct AstTypeFn {
    struct ast *        ret_type;
    TSmallArray_AstPtr *args;
    bool                is_polymorph;
};

struct AstTypeFnGroup {
    TSmallArray_AstPtr *variants;
};

struct AstTypeStruct {
    struct Scope *      scope;
    TSmallArray_AstPtr *members;
    struct ast *        base_type;
    bool                is_union;
    bool                is_multiple_return_type;
};

struct AstTypeEnum {
    struct Scope *      scope;
    struct ast *        type;
    TSmallArray_AstPtr *variants;
};

struct AstTypePtr {
    struct ast *type;
};

struct AstTypeVargs {
    struct ast *type;
};

struct AstExprType {
    struct ast *type;
};

struct AstExprCompound {
    struct ast *        type;
    TSmallArray_AstPtr *values;
    // Allow type infer from function return type.
    bool is_multiple_return_value;
};

struct AstExprLitFn {
    struct ast *type;
    struct ast *block;
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
    struct ast *type;
    struct ast *next;
    bool        auto_cast;
};

struct AstExprBinop {
    struct ast *lhs;
    struct ast *rhs;
    BinopKind   kind;
};

struct AstExprCall {
    struct ast *        ref;
    TSmallArray_AstPtr *args;
    bool                run;
};

struct AstExprElem {
    struct ast *next;
    struct ast *index;
};

struct AstExprSizeof {
    struct ast *node;
};

struct AstExprTypeInfo {
    struct ast *node;
};

struct AstExprAlignof {
    struct ast *node;
};

struct AstExprUnary {
    UnopKind    kind;
    struct ast *next;
};

struct AstExprAddrOf {
    struct ast *next;
};

struct AstExprDeref {
    struct ast *next;
};

struct AstTags {
    TSmallArray_AstPtr *values;
};

struct AstCallLoc {
    void *_;
};

// struct ast base type
struct ast {
    AstKind          kind;
    struct location *location;
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
struct ast *
ast_create_node(struct Arena *arena, AstKind c, struct Token *tok, struct Scope *parent_scope);
const char *ast_binop_to_str(BinopKind op);
const char *ast_unop_to_str(UnopKind op);
const char *ast_get_name(const struct ast *n);

static INLINE bool ast_binop_is_logic(BinopKind op)
{
    return op >= BINOP_EQ && op <= BINOP_LOGIC_OR;
}

#endif
