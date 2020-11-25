//************************************************************************************************
// bl
//
// File:   parser.c
// Author: Martin Dorazil
// Date:   3/15/18
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

#include "common.h"
#include "stages.h"
#include <setjmp.h>

TSMALL_ARRAY_TYPE(AstPtr64, Ast *, 64);
TSMALL_ARRAY_TYPE(ScopePtr64, Scope *, 64);

#define EXPECTED_PRIVATE_SCOPE_COUNT 256

#define PARSE_ERROR(kind, tok, pos, format, ...)                                                   \
    {                                                                                              \
        builder_msg(BUILDER_MSG_ERROR, (kind), &(tok)->location, (pos), (format), ##__VA_ARGS__);  \
    }

#define PARSE_WARNING(tok, pos, format, ...)                                                       \
    {                                                                                              \
        builder_msg(BUILDER_MSG_WARNING, 0, &(tok)->location, (pos), (format), ##__VA_ARGS__);     \
    }

#define PARSE_NOTE(tok, pos, format, ...)                                                          \
    {                                                                                              \
        builder_msg(BUILDER_MSG_NOTE, 0, &(tok)->location, (pos), (format), ##__VA_ARGS__);        \
    }

// swap current compound with _cmp and create temporary variable with previous one

#define SCOPE_PUSH(_cnt, _scope) tsa_push_ScopePtr64(&(_cnt)->_scope_stack, (_scope))
#define SCOPE_POP(_cnt) tsa_pop_ScopePtr64(&(_cnt)->_scope_stack)
#define SCOPE_GET(_cnt) tsa_last_ScopePtr64(&(_cnt)->_scope_stack)
#define SCOPE_SET(_cnt, _scope)                                                                    \
    ((_cnt)->_scope_stack.data[(_cnt)->_scope_stack.size - 1]) = (_scope)

#define DECL_PUSH(_cnt, _decl) tsa_push_AstPtr64(&(_cnt)->_decl_stack, (_decl))
#define DECL_POP(_cnt) tsa_pop_AstPtr64(&(_cnt)->_decl_stack)
#define DECL_GET(_cnt) ((_cnt)->_decl_stack.size ? tsa_last_AstPtr64(&(_cnt)->_decl_stack) : NULL)

#define CONSUME_TILL(tokens, ...)                                                                  \
    {                                                                                              \
        Sym _[] = {__VA_ARGS__};                                                                   \
        tokens_consume_till2((tokens), ARRAY_SIZE(_), &_[0]);                                      \
    }

typedef enum {
    HD_NONE        = 1 << 0,
    HD_LOAD        = 1 << 1,
    HD_LINK        = 1 << 2,
    HD_CALL_LOC    = 1 << 3,
    HD_EXTERN      = 1 << 4,
    HD_COMPILER    = 1 << 5,
    HD_PRIVATE     = 1 << 6,
    HD_INLINE      = 1 << 7,
    HD_NO_INLINE   = 1 << 8,
    HD_FILE        = 1 << 9,
    HD_LINE        = 1 << 10,
    HD_BASE        = 1 << 11,
    HD_ENTRY       = 1 << 12,
    HD_BUILD_ENTRY = 1 << 13,
    HD_TAGS        = 1 << 14,
    HD_NO_INIT     = 1 << 16,
    HD_INTRINSIC   = 1 << 17,
    HD_TEST_FN     = 1 << 18,
    HD_IMPORT      = 1 << 19,
} HashDirective;

typedef struct {
    TSmallArray_AstPtr64   _decl_stack;
    TSmallArray_ScopePtr64 _scope_stack;
    Assembly *             assembly;
    Unit *                 unit;
    Arena *                ast_arena;
    ScopeArenas *          scope_arenas;
    Tokens *               tokens;

    // tmps
    bool     inside_loop;
    Scope *  current_private_scope;
    Ast *    current_docs;
    TString *unit_docs_tmp;
} Context;

// helpers
// fw decls
static BinopKind sym_to_binop_kind(Sym sm);
static UnopKind  sym_to_unop_kind(Sym sm);
static bool      parse_docs(Context *cnt);
static bool      parse_unit_docs(Context *cnt);
static void      parse_ublock_content(Context *cnt, Ast *ublock);
static Ast *     parse_hash_directive(Context *cnt, s32 expected_mask, HashDirective *satisfied);
static Ast *     parse_unrecheable(Context *cnt);
static Ast *     parse_ident(Context *cnt);
static Ast *     parse_ident_group(Context *cnt);
static Ast *     parse_block(Context *cnt, bool create_scope);
static Ast *     parse_decl(Context *cnt);
static Ast *     parse_decl_member(Context *cnt, s32 index);
static Ast *     parse_decl_arg(Context *cnt, bool named);
static Ast *     parse_decl_variant(Context *cnt, Ast *prev);
static Ast *     parse_type(Context *cnt);
static Ast *     parse_type_ref(Context *cnt);
static Ast *     parse_type_arr(Context *cnt);
static Ast *     parse_type_slice(Context *cnt);
static Ast *     parse_type_dynarr(Context *cnt);
static Ast *     parse_type_fn(Context *cnt, bool named_args);
static Ast *     parse_type_fn_group(Context *cnt);
static Ast *     parse_type_fn_return(Context *cnt);
static Ast *     parse_type_struct(Context *cnt);
static Ast *     parse_type_enum(Context *cnt);
static Ast *     parse_type_ptr(Context *cnt);
static Ast *     parse_type_vargs(Context *cnt);
static Ast *     parse_stmt_return(Context *cnt);
static Ast *     parse_stmt_if(Context *cnt);
static Ast *     parse_stmt_loop(Context *cnt);
static Ast *     parse_stmt_break(Context *cnt);
static Ast *     parse_stmt_continue(Context *cnt);
static Ast *     parse_stmt_defer(Context *cnt);
static Ast *     parse_stmt_switch(Context *cnt);
static Ast *     parse_stmt_case(Context *cnt);

// EXPRESSIONS
static Ast *       parse_expr(Context *cnt);
static Ast *       _parse_expr(Context *cnt, s32 p);
static Ast *       parse_expr_atom(Context *cnt);
static Ast *       parse_expr_primary(Context *cnt);
static Ast *       parse_expr_unary(Context *cnt);
static Ast *       parse_expr_binary(Context *cnt, Ast *lhs, Ast *rhs, Token *op);
static Ast *       parse_expr_addrof(Context *cnt);
static Ast *       parse_expr_deref(Context *cnt);
static Ast *       parse_expr_type(Context *cnt);
static Ast *       parse_expr_ref(Context *cnt);
static Ast *       parse_expr_nested(Context *cnt);
static Ast *       parse_expr_null(Context *cnt);
static Ast *       parse_expr_cast(Context *cnt);
static Ast *       parse_expr_cast_auto(Context *cnt);
static Ast *       parse_expr_lit(Context *cnt);
static Ast *       parse_expr_lit_fn(Context *cnt);
static Ast *       parse_expr_lit_fn_group(Context *cnt);
static Ast *       parse_expr_sizeof(Context *cnt);
static Ast *       parse_expr_type_info(Context *cnt);
static Ast *       parse_expr_test_cases(Context *cnt);
static Ast *       parse_expr_alignof(Context *cnt);
static INLINE bool parse_semicolon(Context *cnt);
static INLINE bool parse_semicolon_rq(Context *cnt);
static INLINE bool hash_directive_to_flags(HashDirective hd, u32 *out_flags);
static Ast *       parse_expr_member(Context *cnt, Ast *prev);
static Ast *       parse_expr_call(Context *cnt, Ast *prev);
static Ast *       parse_expr_elem(Context *cnt, Ast *prev);
static Ast *       parse_expr_compound(Context *cnt);

// impl

static INLINE void id_init(ID *id, const char *str)
{
    BL_ASSERT(id);
    id->hash = thash_from_str(str);
    id->str  = str;
}

static INLINE bool rq_semicolon_after_decl_entity(Ast *node)
{
    BL_ASSERT(node);
    switch (node->kind) {
    case AST_EXPR_LIT_FN:
    case AST_EXPR_LIT_FN_GROUP:
    case AST_EXPR_TYPE:
        return false;
    default:
        return true;
    }
}

static INLINE const char *pop_docs(Context *cnt)
{
    const char *text = NULL;
    if (cnt->current_docs) {
        text              = cnt->current_docs->data.docs.text;
        cnt->current_docs = NULL;
    }
    return text;
}

BinopKind sym_to_binop_kind(Sym sm)
{
    switch (sm) {
    case SYM_ASSIGN:
        return BINOP_ASSIGN;
    case SYM_PLUS_ASSIGN:
        return BINOP_ADD_ASSIGN;
    case SYM_MINUS_ASSIGN:
        return BINOP_SUB_ASSIGN;
    case SYM_ASTERISK_ASSIGN:
        return BINOP_MUL_ASSIGN;
    case SYM_SLASH_ASSIGN:
        return BINOP_DIV_ASSIGN;
    case SYM_PERCENT_ASSIGN:
        return BINOP_MOD_ASSIGN;
    case SYM_PLUS:
        return BINOP_ADD;
    case SYM_MINUS:
        return BINOP_SUB;
    case SYM_ASTERISK:
        return BINOP_MUL;
    case SYM_SLASH:
        return BINOP_DIV;
    case SYM_PERCENT:
        return BINOP_MOD;
    case SYM_EQ:
        return BINOP_EQ;
    case SYM_NEQ:
        return BINOP_NEQ;
    case SYM_GREATER:
        return BINOP_GREATER;
    case SYM_LESS:
        return BINOP_LESS;
    case SYM_GREATER_EQ:
        return BINOP_GREATER_EQ;
    case SYM_LESS_EQ:
        return BINOP_LESS_EQ;
    case SYM_LOGIC_AND:
        return BINOP_LOGIC_AND;
    case SYM_LOGIC_OR:
        return BINOP_LOGIC_OR;
    case SYM_AND:
        return BINOP_AND;
    case SYM_OR:
        return BINOP_OR;
    case SYM_SHR:
        return BINOP_SHR;
    case SYM_SHL:
        return BINOP_SHL;
    default:
        BL_ABORT("unknown binop operation!!!");
    }
}

UnopKind sym_to_unop_kind(Sym sm)
{
    switch (sm) {
    case SYM_MINUS:
        return UNOP_NEG;
    case SYM_PLUS:
        return UNOP_POS;
    case SYM_NOT:
        return UNOP_NOT;
    case SYM_BIT_NOT:
        return UNOP_BIT_NOT;
    default:
        BL_ABORT("unknown unop operation!!!");
    }
}

Ast *parse_expr_ref(Context *cnt)
{
    Token *tok   = tokens_peek(cnt->tokens);
    Ast *  ident = parse_ident(cnt);
    if (!ident) return NULL;

    Ast *ref                 = ast_create_node(cnt->ast_arena, AST_EXPR_REF, tok, SCOPE_GET(cnt));
    ref->data.expr_ref.ident = ident;
    return ref;
}

bool parse_docs(Context *cnt)
{
    Token *tok_begin = tokens_peek(cnt->tokens);
    if (token_is_not(tok_begin, SYM_DCOMMENT)) return false;
    TString *str = builder_create_cached_str();
    Token *  tok;
    while ((tok = tokens_consume_if(cnt->tokens, SYM_DCOMMENT))) {
        if (str->len > 0) tstring_append(str, "\n");
        tstring_append(str, tok->value.str);
    }

    Ast *docs            = ast_create_node(cnt->ast_arena, AST_DOCS, tok_begin, SCOPE_GET(cnt));
    docs->data.docs.text = str->data;
    cnt->current_docs    = docs;
    return true;
}

bool parse_unit_docs(Context *cnt)
{
    Token *tok_begin = tokens_peek(cnt->tokens);
    if (token_is_not(tok_begin, SYM_DGCOMMENT)) return false;
    if (!cnt->unit_docs_tmp) cnt->unit_docs_tmp = builder_create_cached_str();
    TString *str = cnt->unit_docs_tmp;
    Token *  tok;
    while ((tok = tokens_consume_if(cnt->tokens, SYM_DGCOMMENT))) {
        if (str->len > 0) tstring_append(str, "\n");
        tstring_append(str, tok->value.str);
    }
    return true;
}

// Try to parse hash directive. List of enabled directives can be set by 'expected_mask',
// 'satisfied' is optional output set to parsed directive id if there is one.
Ast *parse_hash_directive(Context *cnt, s32 expected_mask, HashDirective *satisfied)
{
#define set_satisfied(_hd)                                                                         \
    {                                                                                              \
        if (satisfied) *satisfied = _hd;                                                           \
    }

    set_satisfied(HD_NONE);

    Token *tok_hash = tokens_consume_if(cnt->tokens, SYM_HASH);
    if (!tok_hash) return NULL;

    Token *tok_directive = tokens_consume(cnt->tokens);
    if (tok_directive->sym != SYM_IDENT) goto INVALID;

    const char *directive = tok_directive->value.str;
    BL_ASSERT(directive);

    if (strcmp(directive, "load") == 0) {
        // load <string>
        set_satisfied(HD_LOAD);
        if (IS_NOT_FLAG(expected_mask, HD_LOAD)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Load can be used only in global scope.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Token *tok_path = tokens_consume_if(cnt->tokens, SYM_STRING);
        if (!tok_path) {
            Token *tok_err = tokens_peek(cnt->tokens);
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected path \"some/path\" after 'load' directive.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Ast *load = ast_create_node(cnt->ast_arena, AST_LOAD, tok_directive, SCOPE_GET(cnt));
        load->data.load.filepath = tok_path->value.str;

        if (!builder.options.docs) {
            Unit *unit = unit_new_file(load->data.load.filepath, tok_path);
            if (!assembly_add_unit_unique(cnt->assembly, unit)) {
                unit_delete(unit);
            }
        }

        return load;
    }

    if (strcmp(directive, "import") == 0) {
        set_satisfied(HD_IMPORT);
        if (IS_NOT_FLAG(expected_mask, HD_IMPORT)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Import can be used only in global scope.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Token *tok_path = tokens_consume_if(cnt->tokens, SYM_STRING);
        if (!tok_path) {
            Token *tok_err = tokens_peek(cnt->tokens);
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected path \"some/path\" after 'import' directive.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }
        Ast *import = ast_create_node(cnt->ast_arena, AST_IMPORT, tok_directive, SCOPE_GET(cnt));
        import->data.import.filepath = tok_path->value.str;
        if (!builder.options.docs) {
            assembly_import_module(cnt->assembly, tok_path->value.str, tok_path);
        }
        return import;
    }

    if (strcmp(directive, "link") == 0) {
        // link <string>
        set_satisfied(HD_LINK);
        if (IS_NOT_FLAG(expected_mask, HD_LINK)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Link can be used only in global scope.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Token *tok_path = tokens_consume(cnt->tokens);
        if (!token_is(tok_path, SYM_STRING)) {
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_path,
                        BUILDER_CUR_WORD,
                        "Expected path \"some/path\" after 'link' directive.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Ast *link = ast_create_node(cnt->ast_arena, AST_LINK, tok_directive, SCOPE_GET(cnt));
        link->data.link.lib = tok_path->value.str;

        assembly_add_native_lib(cnt->assembly, tok_path->value.str, tok_path);

        return link;
    }

    if (strcmp(directive, "test") == 0) {
        set_satisfied(HD_TEST_FN);
        if (IS_NOT_FLAG(expected_mask, HD_TEST_FN)) {
            PARSE_ERROR(
                ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unexpected directive.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "file") == 0) {
        set_satisfied(HD_FILE);
        if (IS_NOT_FLAG(expected_mask, HD_FILE)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. File can be used only as an expression.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Ast *file =
            ast_create_node(cnt->ast_arena, AST_EXPR_LIT_STRING, tok_directive, SCOPE_GET(cnt));
        file->data.expr_string.val = tok_directive->location.unit->filepath;
        return file;
    }

    if (strcmp(directive, "base") == 0) {
        set_satisfied(HD_BASE);
        if (IS_NOT_FLAG(expected_mask, HD_BASE)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Base can be used only in context with "
                        "struct literal.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return parse_type(cnt);
    }

    if (strcmp(directive, "tags") == 0) {
        set_satisfied(HD_TAGS);
        if (IS_NOT_FLAG(expected_mask, HD_TAGS)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Tags can be used only for struct members.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        // Tags can contain one or move references separated by comma
        Ast *tag;
        bool rq = false;

        TSmallArray_AstPtr *values = create_sarr(TSmallArray_AstPtr, cnt->assembly);

    VALUE:
        tag = parse_expr_ref(cnt);
        if (tag) {
            tsa_push_AstPtr(values, tag);

            if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
                rq = true;
                goto VALUE;
            }
        } else if (rq) {
            Token *tok_err = tokens_peek(cnt->tokens);
            PARSE_ERROR(ERR_EXPECTED_NAME,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected another tag after comma ','.");

            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        if (!values->size) {
            Token *tok_err = tokens_peek(cnt->tokens);
            PARSE_ERROR(
                ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected tag value after #tags.");

            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Ast *tags = ast_create_node(cnt->ast_arena, AST_TAGS, tok_directive, SCOPE_GET(cnt));

        tags->data.tags.values = values;
        return tags;
    }

    if (strcmp(directive, "line") == 0) {
        set_satisfied(HD_LINE);
        if (IS_NOT_FLAG(expected_mask, HD_LINE)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Line can be used only as an expression.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        Ast *line =
            ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, tok_directive, SCOPE_GET(cnt));
        line->data.expr_integer.val = tok_directive->location.line;
        return line;
    }

    if (strcmp(directive, "entry") == 0) {
        set_satisfied(HD_ENTRY);
        if (IS_NOT_FLAG(expected_mask, HD_ENTRY)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Entry can be used only in context of "
                        "function literal.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "noinit") == 0) {
        set_satisfied(HD_NO_INIT);
        if (IS_NOT_FLAG(expected_mask, HD_NO_INIT)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Noinit can be used only with "
                        "uninitialized variables.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "build_entry") == 0) {
        set_satisfied(HD_BUILD_ENTRY);
        if (IS_NOT_FLAG(expected_mask, HD_BUILD_ENTRY)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Build entry can be used only in context "
                        "of function literal.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "call_location") == 0) {
        set_satisfied(HD_CALL_LOC);
        if (IS_NOT_FLAG(expected_mask, HD_CALL_LOC)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Call location can be used only as "
                        "function argument default value.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return ast_create_node(cnt->ast_arena, AST_CALL_LOC, tok_directive, SCOPE_GET(cnt));
    }

    if (strcmp(directive, "extern") == 0) {
        set_satisfied(HD_EXTERN);
        if (IS_NOT_FLAG(expected_mask, HD_EXTERN)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Extern can be used only for external entities.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }
        // Extern flag extension could be linkage name as string
        Token *tok_ext = tokens_consume_if(cnt->tokens, SYM_STRING);
        if (!tok_ext) return NULL;
        // Parse extension token.
        Ast *ext = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ext, SCOPE_GET(cnt));
        id_init(&ext->data.ident.id, tok_ext->value.str);
        return ext;
    }

    if (strcmp(directive, "intrinsic") == 0) {
        set_satisfied(HD_INTRINSIC);
        if (IS_NOT_FLAG(expected_mask, HD_INTRINSIC)) {
            PARSE_ERROR(
                ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unexpected directive.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }
        // Intrinsic flag extension could be linkage name as string
        Token *tok_ext = tokens_consume_if(cnt->tokens, SYM_STRING);
        if (!tok_ext) return NULL;
        // Parse extension token.
        Ast *ext = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ext, SCOPE_GET(cnt));
        id_init(&ext->data.ident.id, tok_ext->value.str);
        return ext;
    }

    if (strcmp(directive, "compiler") == 0) {
        set_satisfied(HD_COMPILER);
        if (IS_NOT_FLAG(expected_mask, HD_COMPILER)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Compiler can be used only for compiler "
                        "internal entities.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "inline") == 0) {
        set_satisfied(HD_INLINE);
        if (IS_NOT_FLAG(expected_mask, HD_INLINE)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Inline can be used only in context of "
                        "function literal.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "no_inline") == 0) {
        set_satisfied(HD_NO_INLINE);
        if (IS_NOT_FLAG(expected_mask, HD_NO_INLINE)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Inline can be used only in context of "
                        "function literal.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        return NULL;
    }

    if (strcmp(directive, "private") == 0) {
        set_satisfied(HD_PRIVATE);

        if (IS_NOT_FLAG(expected_mask, HD_PRIVATE)) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. Private can be used only in global scope.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        if (cnt->current_private_scope) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. File already contains private scope block.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
        }

        // Here we create private scope for the current unit. (only when source file
        // contains private block).
        //
        // Parent of this scope is a global-scope.
        //
        // This scope has also highest priority during symbol lookup inside the current unit
        // and it is visible only from such unit.
        // Private scope contains only global entity declarations with 'private' flag set
        // in AST node.
        Scope *scope = scope_create(cnt->scope_arenas,
                                    SCOPE_PRIVATE,
                                    cnt->assembly->gscope,
                                    EXPECTED_PRIVATE_SCOPE_COUNT,
                                    &tok_directive->location);

        cnt->current_private_scope = scope;
        scope->llvm_meta           = scope->parent->llvm_meta;

        // Make all other declarations in file nested in private scope
        cnt->unit->private_scope = scope;
        SCOPE_SET(cnt, scope);

        return ast_create_node(cnt->ast_arena, AST_PRIVATE, tok_directive, SCOPE_GET(cnt));
    }

INVALID:
    PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unknown directive.");
    return ast_create_node(cnt->ast_arena, AST_BAD, tok_directive, SCOPE_GET(cnt));
#undef set_satisfied
}

Ast *parse_expr_compound(Context *cnt)
{
    if (!tokens_is_seq(cnt->tokens, 2, SYM_LBLOCK, SYM_COLON)) return NULL;
    // eat {
    Token *tok_begin = tokens_consume(cnt->tokens);
    // eat :
    tokens_consume(cnt->tokens);

    Ast *type = parse_type(cnt);
    if (!type) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected type.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    // eat :
    if (!tokens_consume_if(cnt->tokens, SYM_COLON)) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected colon after type.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *compound = ast_create_node(cnt->ast_arena, AST_EXPR_COMPOUND, tok_begin, SCOPE_GET(cnt));
    compound->data.expr_compound.type = type;

    // parse values
    bool rq = false;
    Ast *tmp;

NEXT:
    tmp = parse_expr(cnt);
    if (tmp) {
        if (!compound->data.expr_compound.values)
            compound->data.expr_compound.values = create_sarr(TSmallArray_AstPtr, cnt->assembly);

        tsa_push_AstPtr(compound->data.expr_compound.values, tmp);

        if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(ERR_EXPECTED_NAME,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected expression after comma ','.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
        }
    }

    Token *tok = tokens_consume(cnt->tokens);
    if (tok->sym != SYM_RBLOCK) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of initialization list '}' or another expression "
                    "separated by comma.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    return compound;
}

Ast *parse_expr_sizeof(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_SIZEOF);
    if (!tok_begin) return NULL;

    Token *tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Expected '(' after sizeof operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *szof = ast_create_node(cnt->ast_arena, AST_EXPR_SIZEOF, tok_begin, SCOPE_GET(cnt));
    szof->data.expr_sizeof.node = parse_expr(cnt);
    if (!szof->data.expr_sizeof.node) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after sizeof operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return szof;
}

Ast *parse_expr_type_info(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_TYPEINFO);
    if (!tok_begin) return NULL;

    Token *tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Expected '(' after typeinfo operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *info = ast_create_node(cnt->ast_arena, AST_EXPR_TYPE_INFO, tok_begin, SCOPE_GET(cnt));
    info->data.expr_type_info.node = parse_expr(cnt);
    if (!info->data.expr_type_info.node) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after typeinfo operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return info;
}

Ast *parse_expr_test_cases(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_TESTCASES);
    if (!tok_begin) return NULL;

    Token *tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Expected '(' after testcases operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *tc = ast_create_node(cnt->ast_arena, AST_EXPR_TEST_CASES, tok_begin, SCOPE_GET(cnt));

    tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after testcases operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return tc;
}

Ast *parse_expr_alignof(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ALIGNOF);
    if (!tok_begin) return NULL;

    Token *tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_WORD, "Expected '(' after cast operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *alof = ast_create_node(cnt->ast_arena, AST_EXPR_ALIGNOF, tok_begin, SCOPE_GET(cnt));
    alof->data.expr_alignof.node = parse_expr(cnt);
    if (!alof->data.expr_alignof.node) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after alignof operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return alof;
}

Ast *parse_expr_cast_auto(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST_AUTO);
    if (!tok_begin) return NULL;

    Ast *cast = ast_create_node(cnt->ast_arena, AST_EXPR_CAST, tok_begin, SCOPE_GET(cnt));
    cast->data.expr_cast.auto_cast = true;

    cast->data.expr_cast.next = _parse_expr(cnt, token_prec(tok_begin).priority);
    if (!cast->data.expr_cast.next) {
        Token *tok = tokens_peek(cnt->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "Expected expression after auto cast.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return cast;
}

Ast *parse_expr_cast(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_CAST);
    if (!tok_begin) return NULL;

    Token *tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_WORD, "Expected '(' after expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *cast = ast_create_node(cnt->ast_arena, AST_EXPR_CAST, tok_begin, SCOPE_GET(cnt));
    cast->data.expr_cast.type = parse_type(cnt);
    if (!cast->data.expr_cast.type) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected type name as cast parameter.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    tok = tokens_consume(cnt->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after cast expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    cast->data.expr_cast.next = _parse_expr(cnt, token_prec(tok_begin).priority);
    if (!cast->data.expr_cast.next) {
        tok = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "Expected expression after cast.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return cast;
}

Ast *parse_decl_member(Context *cnt, s32 UNUSED(index))
{
    Token *    tok_begin = tokens_peek(cnt->tokens);
    Ast *      name      = NULL;
    Ast *      type      = NULL;
    const bool named     = tokens_peek_2nd(cnt->tokens)->sym == SYM_COLON;

    if (named) {
        name = parse_ident(cnt);
        if (!name) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_TYPE,
                        &tok_begin->location,
                        BUILDER_CUR_AFTER,
                        "Expected member name.");
            tokens_consume(cnt->tokens);
        }
        BL_ASSERT(tokens_current_is(cnt->tokens, SYM_COLON));
        tokens_consume(cnt->tokens);
        type = parse_type(cnt);
        if (!type) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_TYPE,
                        name->location,
                        BUILDER_CUR_AFTER,
                        "Expected type.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
        }
    } else {
        type = parse_type(cnt);
        if (!type) return NULL;
    }

    if (!name) {
        BL_ASSERT(index >= 0);
        char index_str[22];
        sprintf(index_str, "_%d", index);
        TString *ident_str = builder_create_cached_str();
        tstring_append(ident_str, index_str);
        name = ast_create_node(cnt->ast_arena, AST_IDENT, tok_begin, SCOPE_GET(cnt));
        id_init(&name->data.ident.id, ident_str->data);
    }

    HashDirective found_hd = HD_NONE;
    Ast *         tags     = parse_hash_directive(cnt, HD_TAGS, &found_hd);
    Ast *         mem = ast_create_node(cnt->ast_arena, AST_DECL_MEMBER, tok_begin, SCOPE_GET(cnt));
    mem->docs         = pop_docs(cnt);
    mem->data.decl.type = type;
    mem->data.decl.name = name;
    mem->data.decl.tags = tags;
    return mem;
}

Ast *parse_decl_arg(Context *cnt, bool named)
{
    Token *tok_begin = tokens_peek(cnt->tokens);
    Ast *  name      = NULL;
    Ast *  type      = NULL;
    Ast *  value     = NULL;
    if (tokens_current_is(cnt->tokens, SYM_RPAREN)) return NULL;
    if (tokens_is_seq(cnt->tokens, 2, SYM_IDENT, SYM_COLON)) {
        // <name> :
        name = parse_ident(cnt);
        tokens_consume(cnt->tokens); // eat :
    } else if (named) {
        Token *tok_err = tokens_peek(cnt->tokens);
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_NAME,
                    &tok_err->location,
                    BUILDER_CUR_AFTER,
                    "Expected argument name followed by colon.");

        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }
    type = parse_type(cnt);
    // Parse optional default value expression.
    if (tokens_current_is(cnt->tokens, SYM_COLON)) {
        Token *tok_err = tokens_consume(cnt->tokens);
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_MUTABILITY,
                    &tok_err->location,
                    BUILDER_CUR_WORD,
                    "Function argument cannot be constant (this maybe shoule be possible "
                    "in future).");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }
    if (tokens_consume_if(cnt->tokens, SYM_ASSIGN)) {
        value = parse_hash_directive(cnt, HD_CALL_LOC, NULL);
        if (!value) value = parse_expr(cnt);
        if (!value) {
            Token *tok_err = tokens_peek(cnt->tokens);
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_NAME,
                        &tok_err->location,
                        BUILDER_CUR_AFTER,
                        "Expected .");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
        }
    } else if (!type) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_TYPE,
                    name->location,
                    BUILDER_CUR_AFTER,
                    "Expected argument type.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tokens_peek(cnt->tokens), SCOPE_GET(cnt));
    }
    Ast *arg = ast_create_node(cnt->ast_arena, AST_DECL_ARG, tok_begin, SCOPE_GET(cnt));
    arg->data.decl_arg.value = value;
    arg->data.decl.type      = type;
    arg->data.decl.name      = name;
    return arg;
}

Ast *parse_decl_variant(Context *cnt, Ast *prev)
{
    Token *tok_begin = tokens_peek(cnt->tokens);
    Ast *  name      = parse_ident(cnt);
    if (!name) return NULL;

    Ast *var  = ast_create_node(cnt->ast_arena, AST_DECL_VARIANT, tok_begin, SCOPE_GET(cnt));
    var->docs = pop_docs(cnt);

    // TODO: Validate correcly '::'
    Token *tok_assign = tokens_consume_if(cnt->tokens, SYM_COLON);
    tok_assign        = tokens_consume_if(cnt->tokens, SYM_COLON);
    if (tok_assign) {
        var->data.decl_variant.value = parse_expr(cnt);
        if (!var->data.decl_variant.value) BL_ABORT("Expected enum variant value");
    } else if (prev) {
        BL_ASSERT(prev->kind == AST_DECL_VARIANT);
        Ast *addition =
            ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, tok_begin, SCOPE_GET(cnt));
        addition->data.expr_integer.val = 1;

        Ast *binop = ast_create_node(cnt->ast_arena, AST_EXPR_BINOP, tok_begin, SCOPE_GET(cnt));
        binop->data.expr_binop.kind = BINOP_ADD;
        binop->data.expr_binop.lhs  = prev->data.decl_variant.value;
        binop->data.expr_binop.rhs  = addition;

        var->data.decl_variant.value = binop;
    } else {
        // first variant is allways 0
        var->data.decl_variant.value =
            ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, NULL, SCOPE_GET(cnt));
        var->data.decl_variant.value->data.expr_integer.val = 0;
    }

    BL_ASSERT(var->data.decl_variant.value);
    var->data.decl.name = name;
    return var;
}

bool parse_semicolon(Context *cnt)
{
    return tokens_consume_if(cnt->tokens, SYM_SEMICOLON);
}

bool parse_semicolon_rq(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_SEMICOLON);
    if (!tok) {
        tok = tokens_peek_prev(cnt->tokens);
        PARSE_ERROR(ERR_MISSING_SEMICOLON, tok, BUILDER_CUR_AFTER, "Expected semicolon ';'.");
        CONSUME_TILL(cnt->tokens, SYM_IDENT, SYM_RBLOCK);
        return false;
    }
    return true;
}

bool hash_directive_to_flags(HashDirective hd, u32 *out_flags)
{
#define FLAG_CASE(_c, _f)                                                                          \
    case (_c):                                                                                     \
        (*out_flags) |= (_f);                                                                      \
        return true;

    switch (hd) {
        FLAG_CASE(HD_EXTERN, FLAG_EXTERN);
        FLAG_CASE(HD_INTRINSIC, FLAG_INTRINSIC);
        FLAG_CASE(HD_ENTRY, FLAG_ENTRY);
        FLAG_CASE(HD_BUILD_ENTRY, FLAG_BUILD_ENTRY);
        FLAG_CASE(HD_COMPILER, FLAG_COMPILER);
        FLAG_CASE(HD_INLINE, FLAG_INLINE);
        FLAG_CASE(HD_NO_INLINE, FLAG_NO_INLINE);
        FLAG_CASE(HD_NO_INIT, FLAG_NO_INIT);
        FLAG_CASE(HD_TEST_FN, FLAG_TEST_FN);
    default:
        break;
    }

    return false;
}

Ast *parse_stmt_return(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_RETURN);
    if (!tok_begin) return NULL;
    Ast *ret = ast_create_node(cnt->ast_arena, AST_STMT_RETURN, tok_begin, SCOPE_GET(cnt));
    ret->data.stmt_return.fn_decl = DECL_GET(cnt);
    tok_begin                     = tokens_peek(cnt->tokens);

    Ast *expr;
    bool rq = false;
NEXT:
    expr = parse_expr(cnt);
    if (expr) {
        if (!ret->data.stmt_return.exprs)
            ret->data.stmt_return.exprs = create_sarr(TSmallArray_AstPtr, cnt->assembly);
        tsa_push_AstPtr(ret->data.stmt_return.exprs, expr);
        if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression after comma ','.");
        CONSUME_TILL(cnt->tokens, SYM_SEMICOLON, SYM_RBLOCK, SYM_IDENT);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }
    return ret;
}

Ast *parse_stmt_if(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_IF);
    if (!tok_begin) return NULL;

    Ast *stmt_if = ast_create_node(cnt->ast_arena, AST_STMT_IF, tok_begin, SCOPE_GET(cnt));

    stmt_if->data.stmt_if.test = parse_expr(cnt);
    if (!stmt_if->data.stmt_if.test) {
        Token *tok_err = tokens_consume(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected expression for the if statement.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    if (AST_IS_OK(stmt_if->data.stmt_if.test)) {
        tokens_consume_till(cnt->tokens, SYM_LBLOCK);
    }

    stmt_if->data.stmt_if.true_stmt = parse_block(cnt, true);
    if (!stmt_if->data.stmt_if.true_stmt) {
        Token *tok_err = tokens_consume(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_STMT,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected compound statement for true result of the if expression test.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    stmt_if->data.stmt_if.false_stmt = NULL;
    if (tokens_consume_if(cnt->tokens, SYM_ELSE)) {
        stmt_if->data.stmt_if.false_stmt = parse_stmt_if(cnt);
        if (!stmt_if->data.stmt_if.false_stmt)
            stmt_if->data.stmt_if.false_stmt = parse_block(cnt, true);
        if (!stmt_if->data.stmt_if.false_stmt) {
            Token *tok_err = tokens_consume(cnt->tokens);
            PARSE_ERROR(ERR_EXPECTED_STMT,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected statement for false result of the if expression test.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
        }
    }

    return stmt_if;
}

Ast *parse_stmt_switch(Context *cnt)
{
    Token *tok_switch = tokens_consume_if(cnt->tokens, SYM_SWITCH);
    if (!tok_switch) return NULL;

    Ast *expr = parse_expr(cnt);
    BL_ASSERT(expr && "This should be an error!");

    Token *tok = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
    BL_ASSERT(tok && "This should be an error!");

    TSmallArray_AstPtr *cases        = create_sarr(TSmallArray_AstPtr, cnt->assembly);
    Ast *               stmt_case    = NULL;
    Ast *               default_case = NULL;
NEXT:
    stmt_case = parse_stmt_case(cnt);
    if (AST_IS_OK(stmt_case)) {
        if (stmt_case->data.stmt_case.is_default) {
            if (default_case) {
                builder_msg(BUILDER_MSG_ERROR,
                            ERR_INVALID_SWITCH_CASE,
                            stmt_case->location,
                            BUILDER_CUR_WORD,
                            "Switch statement cannot have more than one default cases.");

                builder_msg(BUILDER_MSG_NOTE,
                            0,
                            default_case->location,
                            BUILDER_CUR_WORD,
                            "Previous found here.");
            } else {
                default_case = stmt_case;
            }
        }

        tsa_push_AstPtr(cases, stmt_case);
        if (tokens_current_is_not(cnt->tokens, SYM_RBLOCK)) goto NEXT;
    }

    tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
    BL_ASSERT(tok && "This should be an error!");

    Ast *stmt_switch = ast_create_node(cnt->ast_arena, AST_STMT_SWITCH, tok_switch, SCOPE_GET(cnt));

    stmt_switch->data.stmt_switch.expr  = expr;
    stmt_switch->data.stmt_switch.cases = cases;
    return stmt_switch;
}

Ast *parse_stmt_case(Context *cnt)
{
    TSmallArray_AstPtr *exprs = NULL;
    Ast *               block = NULL;
    Ast *               expr  = NULL;
    bool                rq    = false;

    if (tokens_current_is(cnt->tokens, SYM_RBLOCK)) return NULL;

    Token *tok_case = tokens_consume_if(cnt->tokens, SYM_DEFAULT);
    if (tok_case) goto SKIP_EXPRS;

    tok_case = tokens_peek(cnt->tokens);
    exprs    = create_sarr(TSmallArray_AstPtr, cnt->assembly);
NEXT:
    expr = parse_expr(cnt);
    if (expr) {
        tsa_push_AstPtr(exprs, expr);

        if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected expression after comma.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

SKIP_EXPRS:
    block = parse_block(cnt, true);
    if (!block && !parse_semicolon_rq(cnt)) {
        Token *tok_err = tokens_peek(cnt->tokens);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    } else {
        parse_semicolon(cnt);
    }

    Ast *stmt_case = ast_create_node(cnt->ast_arena, AST_STMT_CASE, tok_case, SCOPE_GET(cnt));
    stmt_case->data.stmt_case.exprs      = exprs;
    stmt_case->data.stmt_case.is_default = !exprs;
    stmt_case->data.stmt_case.block      = block;

    return stmt_case;
}

static TokensLookaheadState cmp_stmt_loop(Token *curr)
{
    if (token_is(curr, SYM_SEMICOLON)) return TOK_LOOK_HIT;
    // @INCOMPLETE I'm not sure why this was here, check it later.
    // else if (token_is(curr, SYM_RPAREN))
    //    return TOK_LOOK_TERMINAL;
    else if (token_is(curr, SYM_LBLOCK))
        return TOK_LOOK_TERMINAL;

    return TOK_LOOK_CONTINUE;
}

Ast *parse_stmt_loop(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LOOP);
    if (!tok_begin) return NULL;

    // Loop statement is immediately followed by block; this should act like while (true) {} in C.
    const bool while_true = tokens_current_is(cnt->tokens, SYM_LBLOCK);

    Ast *      loop = ast_create_node(cnt->ast_arena, AST_STMT_LOOP, tok_begin, SCOPE_GET(cnt));
    const bool prev_in_loop = cnt->inside_loop;
    cnt->inside_loop        = true;

    Scope *scope =
        scope_create(cnt->scope_arenas, SCOPE_LEXICAL, SCOPE_GET(cnt), 128, &tok_begin->location);

    SCOPE_PUSH(cnt, scope);

    if (!while_true) {
        if (tokens_lookahead(cnt->tokens, cmp_stmt_loop)) {
            // for loop construct loop [init]; [condition]; [increment] {}
            loop->data.stmt_loop.init = parse_decl(cnt);
            if (!parse_semicolon_rq(cnt)) {
                BL_ASSERT(false);
            }

            loop->data.stmt_loop.condition = parse_expr(cnt);
            if (!parse_semicolon_rq(cnt)) {
                BL_ASSERT(false);
            }

            loop->data.stmt_loop.increment = parse_expr(cnt);
        } else {
            // while construct with optional condition
            loop->data.stmt_loop.condition = parse_expr(cnt);
        }
    }

    // block
    loop->data.stmt_loop.block = parse_block(cnt, false);
    if (!loop->data.stmt_loop.block) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "Expected loop body block.");
        cnt->inside_loop = prev_in_loop;
        SCOPE_POP(cnt);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    cnt->inside_loop = prev_in_loop;
    SCOPE_POP(cnt);
    return loop;
}

Ast *parse_stmt_break(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_BREAK);
    if (!tok) return NULL;

    if (!cnt->inside_loop) {
        PARSE_ERROR(
            ERR_BREAK_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD, "Break statement outside a loop.");
    }
    return ast_create_node(cnt->ast_arena, AST_STMT_BREAK, tok, SCOPE_GET(cnt));
}

Ast *parse_stmt_continue(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_CONTINUE);
    if (!tok) return NULL;

    if (!cnt->inside_loop) {
        PARSE_ERROR(
            ERR_CONTINUE_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD, "Continue statement outside a loop.");
    }

    return ast_create_node(cnt->ast_arena, AST_STMT_CONTINUE, tok, SCOPE_GET(cnt));
}

Ast *parse_stmt_defer(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_DEFER);
    if (!tok) return NULL;

    Ast *expr = NULL;
    expr      = parse_expr(cnt);

    if (!expr) {
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after 'defer' statement.");

        Token *tok_err = tokens_peek(cnt->tokens);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }

    Ast *defer = ast_create_node(cnt->ast_arena, AST_STMT_DEFER, tok, SCOPE_GET(cnt));
    defer->data.stmt_defer.expr = expr;

    return defer;
}

Ast *parse_expr(Context *cnt)
{
    return _parse_expr(cnt, 0);
}

Ast *_parse_expr(Context *cnt, s32 p)
{
    Ast *lhs = parse_expr_atom(cnt);
    Ast *tmp = NULL;

    do {
        tmp = parse_expr_call(cnt, lhs);
        if (!tmp) tmp = parse_expr_elem(cnt, lhs);
        if (!tmp) tmp = parse_expr_member(cnt, lhs);
        lhs = tmp ? tmp : lhs;
    } while (tmp);

    while (token_is_binop(tokens_peek(cnt->tokens)) &&
           token_prec(tokens_peek(cnt->tokens)).priority >= p) {
        Token *op = tokens_consume(cnt->tokens);

        const s32 q = token_prec(op).associativity == TOKEN_ASSOC_LEFT ? token_prec(op).priority + 1
                                                                       : token_prec(op).priority;

        Ast *rhs = _parse_expr(cnt, q);
        if (!lhs || !rhs) {
            PARSE_ERROR(ERR_INVALID_EXPR, op, BUILDER_CUR_WORD, "Invalid binary operation.");
        }

        lhs = parse_expr_binary(cnt, lhs, rhs, op);
    }

    return lhs;
}

Ast *parse_expr_primary(Context *cnt)
{
    Ast *expr = NULL;
    if ((expr = parse_expr_nested(cnt))) return expr;
    if ((expr = parse_expr_ref(cnt))) return expr;
    if ((expr = parse_expr_lit(cnt))) return expr;
    if ((expr = parse_expr_lit_fn(cnt))) return expr;
    if ((expr = parse_expr_lit_fn_group(cnt))) return expr;
    if ((expr = parse_expr_type(cnt))) return expr;
    if ((expr = parse_expr_null(cnt))) return expr;
    if ((expr = parse_expr_compound(cnt))) return expr;
    if ((expr = parse_hash_directive(cnt, HD_FILE | HD_LINE, NULL))) return expr;

    return NULL;
}

Ast *parse_expr_unary(Context *cnt)
{
    Token *op = tokens_peek(cnt->tokens);
    if (!token_is_unary(op)) return NULL;

    tokens_consume(cnt->tokens);
    Ast *unary = ast_create_node(cnt->ast_arena, AST_EXPR_UNARY, op, SCOPE_GET(cnt));
    unary->data.expr_unary.next = _parse_expr(cnt, token_prec(op).priority);
    unary->data.expr_unary.kind = sym_to_unop_kind(op->sym);

    if (unary->data.expr_unary.next == NULL) {
        Token *err_tok = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    err_tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after unary operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, op, SCOPE_GET(cnt));
    }

    if (unary->data.expr_unary.next->kind == AST_BAD) return unary->data.expr_unary.next;

    return unary;
}

Ast *parse_expr_atom(Context *cnt)
{
    Ast *expr = NULL;

    if ((expr = parse_expr_primary(cnt))) return expr;
    if ((expr = parse_expr_unary(cnt))) return expr;
    if ((expr = parse_expr_deref(cnt))) return expr;
    if ((expr = parse_expr_addrof(cnt))) return expr;
    if ((expr = parse_expr_cast(cnt))) return expr;
    if ((expr = parse_expr_cast_auto(cnt))) return expr;
    if ((expr = parse_expr_sizeof(cnt))) return expr;
    if ((expr = parse_expr_alignof(cnt))) return expr;
    if ((expr = parse_expr_type_info(cnt))) return expr;
    if ((expr = parse_expr_test_cases(cnt))) return expr;

    return NULL;
}

Ast *parse_expr_binary(Context *cnt, Ast *lhs, Ast *rhs, Token *op)
{
    if (!token_is_binop(op)) return NULL;

    Ast *binop = ast_create_node(cnt->ast_arena, AST_EXPR_BINOP, op, SCOPE_GET(cnt));
    binop->data.expr_binop.kind = sym_to_binop_kind(op->sym);
    binop->data.expr_binop.lhs  = lhs;
    binop->data.expr_binop.rhs  = rhs;

    return binop;
}

Ast *parse_expr_addrof(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_AND);
    if (!tok) return NULL;

    Ast *addrof = ast_create_node(cnt->ast_arena, AST_EXPR_ADDROF, tok, SCOPE_GET(cnt));
    addrof->data.expr_addrof.next = _parse_expr(cnt, token_prec(tok).priority);

    if (addrof->data.expr_addrof.next == NULL) {
        Token *err_tok = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    err_tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after '&' operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    if (addrof->data.expr_addrof.next->kind == AST_BAD) return addrof->data.expr_addrof.next;
    return addrof;
}

Ast *parse_expr_deref(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_CARET);
    if (!tok) return NULL;

    Ast *deref = ast_create_node(cnt->ast_arena, AST_EXPR_DEREF, tok, SCOPE_GET(cnt));
    deref->data.expr_deref.next = _parse_expr(cnt, token_prec(tok).priority);

    if (deref->data.expr_deref.next == NULL) {
        Token *err_tok = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    err_tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after '^' operator.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    if (deref->data.expr_deref.next->kind == AST_BAD) return deref->data.expr_deref.next;
    return deref;
}

Ast *parse_expr_lit(Context *cnt)
{
    Token *tok = tokens_peek(cnt->tokens);
    Ast *  lit = NULL;

    switch (tok->sym) {
    case SYM_NUM:
        lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_INT, tok, SCOPE_GET(cnt));
        lit->data.expr_integer.val      = tok->value.u;
        lit->data.expr_integer.overflow = tok->overflow;
        break;

    case SYM_CHAR:
        lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_CHAR, tok, SCOPE_GET(cnt));
        lit->data.expr_character.val = (u8)tok->value.c;

        break;

    case SYM_TRUE:
        lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, SCOPE_GET(cnt));
        lit->data.expr_boolean.val = true;
        break;

    case SYM_FALSE:
        lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_BOOL, tok, SCOPE_GET(cnt));
        lit->data.expr_boolean.val = false;
        break;

    case SYM_DOUBLE:
        lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_DOUBLE, tok, SCOPE_GET(cnt));
        lit->data.expr_double.val      = tok->value.d;
        lit->data.expr_double.overflow = tok->overflow;
        break;

    case SYM_FLOAT:
        lit = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FLOAT, tok, SCOPE_GET(cnt));
        lit->data.expr_float.val      = (f32)tok->value.d;
        lit->data.expr_float.overflow = tok->overflow;
        break;

    case SYM_STRING: {
        // There is special case for string literals, those can be split into multiple lines and we
        // should handle such situation here, so some pre-scan is needed.
        lit             = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_STRING, tok, SCOPE_GET(cnt));
        const char *str = tok->value.str;
        Token *     tok_next = tokens_peek_2nd(cnt->tokens);
        if (tok_next->sym == SYM_STRING) {
            TString *tmp = builder_create_cached_str();
            while ((tok = tokens_consume_if(cnt->tokens, SYM_STRING))) {
                BL_ASSERT(tok->value.str);
                tstring_append(tmp, tok->value.str);
            }
            str = tmp->data;
        } else {
            tokens_consume(cnt->tokens);
        }
        BL_ASSERT(str);
        lit->data.expr_string.val = str;
        // Directly return, all tokens were consumed.
        return lit;
    }

    default:
        return NULL;
    }

    tokens_consume(cnt->tokens);
    return lit;
}

Ast *parse_expr_lit_fn(Context *cnt)
{
    if (!tokens_is_seq(cnt->tokens, 2, SYM_FN, SYM_LPAREN)) return NULL;
    Token *tok_fn = tokens_peek(cnt->tokens);
    Ast *  fn     = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FN, tok_fn, SCOPE_GET(cnt));

    Scope *   parent_scope = SCOPE_GET(cnt);
    ScopeKind scope_kind =
        (parent_scope->kind == SCOPE_GLOBAL || parent_scope->kind == SCOPE_PRIVATE)
            ? SCOPE_FN
            : SCOPE_FN_LOCAL;
    Scope *scope =
        scope_create(cnt->scope_arenas, scope_kind, SCOPE_GET(cnt), 256, &tok_fn->location);

    SCOPE_PUSH(cnt, scope);

    Ast *type = parse_type_fn(cnt, true);
    BL_ASSERT(type);
    fn->data.expr_fn.type = type;
    // parse flags
    Ast *curr_decl = DECL_GET(cnt);
    if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
        u32 accepted = HD_EXTERN | HD_NO_INLINE | HD_INLINE | HD_COMPILER | HD_ENTRY |
                       HD_BUILD_ENTRY | HD_INTRINSIC | HD_TEST_FN;
        u32 flags = 0;
        while (true) {
            HashDirective found        = HD_NONE;
            Ast *         hd_extension = parse_hash_directive(cnt, accepted, &found);
            if (!hash_directive_to_flags(found, &flags)) break;

            if ((found == HD_EXTERN || found == HD_INTRINSIC) && hd_extension) {
                // Use extern flag extension on function declaration.

                BL_ASSERT(hd_extension->kind == AST_IDENT &&
                          "Expected ident as #extern extension.");
                BL_ASSERT(curr_decl->data.decl_entity.explicit_linkage_name == NULL);
                curr_decl->data.decl_entity.explicit_linkage_name = hd_extension;
            }

            accepted &= ~found;
        }

        curr_decl->data.decl_entity.flags |= flags;
    }

    // parse block (block is optional function body can be external)
    fn->data.expr_fn.block = parse_block(cnt, false);

    SCOPE_POP(cnt);
    return fn;
}

Ast *parse_expr_lit_fn_group(Context *cnt)
{
    if (!tokens_is_seq(cnt->tokens, 2, SYM_FN, SYM_LBLOCK)) return NULL;
    Token *tok_group = tokens_consume(cnt->tokens); // eat fn
    Token *tok_begin = tokens_consume(cnt->tokens); // eat {
    Ast *group = ast_create_node(cnt->ast_arena, AST_EXPR_LIT_FN_GROUP, tok_group, SCOPE_GET(cnt));

    TSmallArray_AstPtr *variants       = create_sarr(TSmallArray_AstPtr, cnt->assembly);
    group->data.expr_fn_group.variants = variants;
    Ast *tmp;
NEXT:
    if ((tmp = parse_expr_ref(cnt))) {
        tsa_push_AstPtr(variants, tmp);
        parse_semicolon_rq(cnt);
        goto NEXT;
    }
    Token *tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
    if (!tok) {
        tok = tokens_peek_prev(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }
    return group;
}

Ast *parse_expr_nested(Context *cnt)
{
    Ast *  expr      = NULL;
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LPAREN);
    if (!tok_begin) return NULL;

    expr = parse_expr(cnt);
    if (expr == NULL) {
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_WORD, "Expected expression.");
    }

    // eat )
    Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RPAREN);
    if (!tok_end) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Unterminated sub-expression, missing ')'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "starting here");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    return expr;
}

Ast *parse_expr_member(Context *cnt, Ast *prev)
{
    if (!prev) return NULL;
    Token *tok = tokens_consume_if(cnt->tokens, SYM_DOT);
    if (!tok) return NULL;

    Ast *ident = parse_ident(cnt);
    if (!ident) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected member name.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    Ast *mem = ast_create_node(cnt->ast_arena, AST_EXPR_MEMBER, tok, SCOPE_GET(cnt));
    mem->data.expr_member.ident = ident;
    mem->data.expr_member.next  = prev;
    mem->data.expr_member.i     = -1;
    return mem;
}

Ast *parse_expr_elem(Context *cnt, Ast *prev)
{
    if (!prev) return NULL;
    Token *tok_elem = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
    if (!tok_elem) return NULL;

    Ast *elem = ast_create_node(cnt->ast_arena, AST_EXPR_ELEM, tok_elem, SCOPE_GET(cnt));
    elem->data.expr_elem.index = parse_expr(cnt);
    elem->data.expr_elem.next  = prev;

    if (!elem->data.expr_elem.index) {
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok_elem, BUILDER_CUR_WORD, "Expected array index expression.");
    }

    Token *tok = tokens_consume(cnt->tokens);
    if (tok->sym != SYM_RBRACKET) {
        PARSE_ERROR(ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Missing bracket ']'.");
    }

    return elem;
}

Ast *parse_ident(Context *cnt)
{
    Token *tok_ident = tokens_consume_if(cnt->tokens, SYM_IDENT);
    if (!tok_ident) return NULL;
    Ast *ident = ast_create_node(cnt->ast_arena, AST_IDENT, tok_ident, SCOPE_GET(cnt));
    id_init(&ident->data.ident.id, tok_ident->value.str);
    return ident;
}

Ast *parse_ident_group(Context *cnt)
{
    Ast *root = NULL;
    Ast *prev = NULL;
    bool rq   = false;
    Ast *ident;
NEXT:
    ident = parse_ident(cnt);
    if (ident) {
        if (prev) prev->data.ident.next = ident;
        if (!root) root = ident;
        prev = ident;
        if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected name after comma ','.");
        CONSUME_TILL(cnt->tokens, SYM_COLON, SYM_SEMICOLON, SYM_IDENT);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_err, SCOPE_GET(cnt));
    }
    return root;
}

Ast *parse_type_ptr(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_ASTERISK);
    if (!tok_begin) return NULL;

    Ast *ptr = ast_create_node(cnt->ast_arena, AST_TYPE_PTR, tok_begin, SCOPE_GET(cnt));
    ptr->data.type_ptr.type = parse_type(cnt);
    BL_ASSERT(ptr->data.type_ptr.type);
    return ptr;
}

Ast *parse_type_vargs(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_VARGS);
    if (!tok_begin) return NULL;

    Ast *ptr = ast_create_node(cnt->ast_arena, AST_TYPE_VARGS, tok_begin, SCOPE_GET(cnt));
    ptr->data.type_ptr.type = parse_type(cnt);
    return ptr;
}

Ast *parse_type_enum(Context *cnt)
{
    Token *tok_enum = tokens_consume_if(cnt->tokens, SYM_ENUM);
    if (!tok_enum) return NULL;

    Ast *enm = ast_create_node(cnt->ast_arena, AST_TYPE_ENUM, tok_enum, SCOPE_GET(cnt));
    enm->data.type_enm.variants = create_sarr(TSmallArray_AstPtr, cnt->assembly);
    enm->data.type_enm.type     = parse_type(cnt);

    // parse flags
    Ast *curr_decl = DECL_GET(cnt);
    if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
        u32 accepted = HD_COMPILER;
        u32 flags    = 0;
        while (true) {
            HashDirective found = HD_NONE;
            parse_hash_directive(cnt, accepted, &found);
            if (!hash_directive_to_flags(found, &flags)) break;
            accepted &= ~found;
        }

        curr_decl->data.decl_entity.flags |= flags;
    }

    Token *tok = tokens_consume(cnt->tokens);
    if (token_is_not(tok, SYM_LBLOCK)) {
        PARSE_ERROR(ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected enum variant list.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    Scope *scope =
        scope_create(cnt->scope_arenas, SCOPE_TYPE_ENUM, SCOPE_GET(cnt), 512, &tok->location);
    enm->data.type_enm.scope = scope;
    SCOPE_PUSH(cnt, scope);

    // parse enum varinats
    bool rq = false;
    Ast *tmp;
    Ast *prev_tmp = NULL;

NEXT:
    if (parse_docs(cnt)) goto NEXT;
    tmp = parse_decl_variant(cnt, prev_tmp);
    if (tmp) {
        prev_tmp = tmp;
        tsa_push_AstPtr(enm->data.type_enm.variants, tmp);

        if (tokens_consume_if(cnt->tokens, SYM_SEMICOLON)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(
                ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected variant after semicolon.");
            SCOPE_POP(cnt);
            return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
        }
    }

    tok = tokens_consume(cnt->tokens);
    if (tok->sym != SYM_RBLOCK) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of variant list '}' or another variant separated by semicolon.");
        SCOPE_POP(cnt);
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    SCOPE_POP(cnt);
    return enm;
}

Ast *parse_type_ref(Context *cnt)
{
    Token *tok   = tokens_peek(cnt->tokens);
    Ast *  ident = parse_ident(cnt);
    if (!ident) return NULL;

    Ast *type_ref = ast_create_node(cnt->ast_arena, AST_TYPE_REF, tok, SCOPE_GET(cnt));
    type_ref->data.type_ref.ident = ident;
    return type_ref;
}

Ast *parse_type_arr(Context *cnt)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBRACKET);
    if (!tok_begin) return NULL;

    Ast *arr = ast_create_node(cnt->ast_arena, AST_TYPE_ARR, tok_begin, SCOPE_GET(cnt));
    arr->data.type_arr.len = parse_expr(cnt);
    if (!arr->data.type_arr.len) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected array size expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
    if (!tok_end) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected closing ']' after array size expression.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);

        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    arr->data.type_arr.elem_type = parse_type(cnt);
    if (!arr->data.type_arr.elem_type) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_INVALID_TYPE, tok_err, BUILDER_CUR_WORD, "Expected array element type.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    return arr;
}

Ast *parse_type_slice(Context *cnt)
{
    if (tokens_peek(cnt->tokens)->sym != SYM_LBRACKET) return NULL;
    if (tokens_peek_2nd(cnt->tokens)->sym != SYM_RBRACKET) return NULL;

    // eat []
    Token *tok_begin = tokens_consume(cnt->tokens);
    tok_begin        = tokens_consume(cnt->tokens);

    Ast *slice = ast_create_node(cnt->ast_arena, AST_TYPE_SLICE, tok_begin, SCOPE_GET(cnt));

    slice->data.type_slice.elem_type = parse_type(cnt);

    if (!slice->data.type_slice.elem_type) {
        PARSE_ERROR(ERR_INVALID_TYPE, tok_begin, BUILDER_CUR_AFTER, "Expected slice element type.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    return slice;
}

Ast *parse_type_dynarr(Context *cnt)
{
    if (tokens_peek(cnt->tokens)->sym != SYM_LBRACKET) return NULL;
    if (tokens_peek_2nd(cnt->tokens)->sym != SYM_DYNARR) return NULL;

    // eat [..
    Token *tok_begin = tokens_consume(cnt->tokens);
    tokens_consume(cnt->tokens);

    Token *tok_end = tokens_consume_if(cnt->tokens, SYM_RBRACKET);
    if (!tok_end) {
        Token *tok_err = tokens_peek(cnt->tokens);
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected closing ']' after dynamic array signature.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);

        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    Ast *slice = ast_create_node(cnt->ast_arena, AST_TYPE_DYNARR, tok_begin, SCOPE_GET(cnt));
    slice->data.type_dynarr.elem_type = parse_type(cnt);

    if (!slice->data.type_dynarr.elem_type) {
        PARSE_ERROR(
            ERR_INVALID_TYPE, tok_end, BUILDER_CUR_AFTER, "Expected dynami array element type.");

        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    return slice;
}

Ast *parse_type(Context *cnt)
{
    Ast *type = NULL;

    type = parse_type_ptr(cnt);
    // keep order
    if (!type) type = parse_type_fn_group(cnt);
    if (!type) type = parse_type_fn(cnt, false);
    // keep order

    if (!type) type = parse_type_struct(cnt);
    if (!type) type = parse_type_enum(cnt);
    if (!type) type = parse_type_vargs(cnt);

    // Keep order!!!
    if (!type) type = parse_type_slice(cnt);
    if (!type) type = parse_type_dynarr(cnt);
    if (!type) type = parse_type_arr(cnt);
    // Keep order!!!

    if (!type) type = parse_type_ref(cnt);

    return type;
}

Ast *parse_type_fn_return(Context *cnt)
{
    if (tokens_current_is(cnt->tokens, SYM_LPAREN)) {
        // multiple return type ( T1, T2 )
        // eat (
        Token *tok_begin = tokens_consume(cnt->tokens);
        Scope *scope     = scope_create(
            cnt->scope_arenas, SCOPE_TYPE_STRUCT, SCOPE_GET(cnt), 16, &tok_begin->location);
        SCOPE_PUSH(cnt, scope);

        Ast *type_struct =
            ast_create_node(cnt->ast_arena, AST_TYPE_STRUCT, tok_begin, SCOPE_GET(cnt));
        type_struct->data.type_strct.scope   = scope;
        type_struct->data.type_strct.members = create_sarr(TSmallArray_AstPtr, cnt->assembly);
        type_struct->data.type_strct.is_multiple_return_type = true;
        Ast *tmp;
        s32  index = 0;
    NEXT:
        tmp = parse_decl_member(cnt, index++);
        if (tmp) {
            if (AST_IS_BAD(tmp)) CONSUME_TILL(cnt->tokens, SYM_COMMA, SYM_RPAREN);
            tsa_push_AstPtr(type_struct->data.type_strct.members, tmp);
            if (tokens_consume_if(cnt->tokens, SYM_COMMA)) goto NEXT;
        }
        Token *tok = tokens_consume_if(cnt->tokens, SYM_RPAREN);
        if (!tok) {
            PARSE_ERROR(ERR_MISSING_BRACKET,
                        tokens_peek(cnt->tokens),
                        BUILDER_CUR_WORD,
                        "Expected end of return list or another return type separated by comma.");
            SCOPE_POP(cnt);
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
        }
        SCOPE_POP(cnt);
        if (!type_struct->data.type_strct.members->size) {
            PARSE_ERROR(ERR_INVALID_TYPE,
                        tok_begin,
                        BUILDER_CUR_WORD,
                        "Expected at least one return type inside parenthesis, if function should "
                        "return 'void' remove parenthesis and leave return type unspecified.");
        }
        return type_struct;
    }
    return parse_type(cnt);
}

Ast *parse_type_fn(Context *cnt, bool named_args)
{
    Token *tok_fn = tokens_consume_if(cnt->tokens, SYM_FN);
    if (!tok_fn) return NULL;

    Token *tok = tokens_consume(cnt->tokens);
    if (tok->sym != SYM_LPAREN) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected function parameter list.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, SCOPE_GET(cnt));
    }
    Ast *fn = ast_create_node(cnt->ast_arena, AST_TYPE_FN, tok_fn, SCOPE_GET(cnt));
    // parse arg types
    bool rq = false;
    Ast *tmp;
NEXT:
    tmp = parse_decl_arg(cnt, named_args);
    if (tmp) {
        if (!fn->data.type_fn.args)
            fn->data.type_fn.args = create_sarr(TSmallArray_AstPtr, cnt->assembly);
        tsa_push_AstPtr(fn->data.type_fn.args, tmp);
        if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(
                ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected type after comma ','.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, SCOPE_GET(cnt));
        }
    }

    tok = tokens_consume(cnt->tokens);
    if (tok->sym != SYM_RPAREN) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of argument type list ')' or another argument separated "
                    "by comma.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_fn, SCOPE_GET(cnt));
    }
    fn->data.type_fn.ret_type = parse_type_fn_return(cnt);
    return fn;
}

Ast *parse_type_fn_group(Context *cnt)
{
    if (!tokens_is_seq(cnt->tokens, 2, SYM_FN, SYM_LBLOCK)) return NULL;
    Token *tok_group = tokens_consume(cnt->tokens); // eat fn
    Token *tok_begin = tokens_consume(cnt->tokens); // eat {
    Ast *  group = ast_create_node(cnt->ast_arena, AST_TYPE_FN_GROUP, tok_group, SCOPE_GET(cnt));

    TSmallArray_AstPtr *variants       = create_sarr(TSmallArray_AstPtr, cnt->assembly);
    group->data.type_fn_group.variants = variants;
    Ast *tmp;
NEXT:
    if (parse_semicolon(cnt)) goto NEXT;
    if ((tmp = parse_type(cnt))) {
        if (tmp->kind != AST_TYPE_FN) {
            // This check is important, when we decide to remove this, validation should
            // be handled in MIR.
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_INVALID_TYPE,
                        tmp->location,
                        BUILDER_CUR_WORD,
                        "Expected function type.");
        }
        tsa_push_AstPtr(variants, tmp);
        goto NEXT;
    }

    Token *tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
    if (!tok) {
        tok = tokens_peek_prev(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }
    return group;
}

Ast *parse_type_struct(Context *cnt)
{
    Token *tok_struct = tokens_consume_if(cnt->tokens, SYM_STRUCT);
    if (!tok_struct) tok_struct = tokens_consume_if(cnt->tokens, SYM_UNION);
    if (!tok_struct) return NULL;

    const bool is_union = tok_struct->sym == SYM_UNION;

    // parse flags
    u32  accepted  = is_union ? 0 : HD_COMPILER | HD_BASE;
    u32  flags     = 0;
    Ast *base_type = NULL;
    while (true) {
        Ast *         hd_extension;
        HashDirective found = HD_NONE;
        hd_extension        = parse_hash_directive(cnt, accepted, &found);
        if (found == HD_BASE) {
            BL_ASSERT(hd_extension);
            base_type = hd_extension;
        } else if (!hash_directive_to_flags(found, &flags)) {
            break;
        }
        accepted &= ~found;
    }

    Ast *curr_decl = DECL_GET(cnt);
    if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
        curr_decl->data.decl_entity.flags |= flags;
    }

    Token *tok = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
    if (!tok) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok_struct, BUILDER_CUR_AFTER, "Expected struct member list.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, SCOPE_GET(cnt));
    }

    Scope *scope =
        scope_create(cnt->scope_arenas, SCOPE_TYPE_STRUCT, SCOPE_GET(cnt), 128, &tok->location);
    SCOPE_PUSH(cnt, scope);

    Ast *type_struct = ast_create_node(cnt->ast_arena, AST_TYPE_STRUCT, tok_struct, SCOPE_GET(cnt));
    type_struct->data.type_strct.scope     = scope;
    type_struct->data.type_strct.members   = create_sarr(TSmallArray_AstPtr, cnt->assembly);
    type_struct->data.type_strct.base_type = base_type;
    type_struct->data.type_strct.is_union  = is_union;

    // parse members
    Ast *tmp;
    s32  index = 0;
NEXT:
    if (parse_docs(cnt)) goto NEXT;
    tmp = parse_decl_member(cnt, index++);
    if (tmp) {
        if (AST_IS_BAD(tmp)) CONSUME_TILL(cnt->tokens, SYM_SEMICOLON, SYM_RBLOCK);
        tsa_push_AstPtr(type_struct->data.type_strct.members, tmp);
        if (tokens_consume_if(cnt->tokens, SYM_SEMICOLON)) goto NEXT;
    }

    tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
    if (!tok) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tokens_peek(cnt->tokens),
                    BUILDER_CUR_WORD,
                    "Expected end of member list '}' or another memeber separated by semicolon.");
        tokens_consume_till(cnt->tokens, SYM_SEMICOLON);
        SCOPE_POP(cnt);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_struct, SCOPE_GET(cnt));
    }

    SCOPE_POP(cnt);
    return type_struct;
}

static TokensLookaheadState cmp_decl(Token *curr)
{
    switch (curr->sym) {
    case SYM_COLON:
        return TOK_LOOK_HIT;
    case SYM_COMMA:
    case SYM_IDENT:
        return TOK_LOOK_CONTINUE;
    default:
        return TOK_LOOK_TERMINAL;
    }
}

Ast *parse_decl(Context *cnt)
{
    // is value declaration?
    if (!tokens_lookahead(cnt->tokens, cmp_decl)) return NULL;
    Token *tok_begin = tokens_peek(cnt->tokens);
    Ast *  ident     = parse_ident_group(cnt);
    if (!ident) return NULL;
    // eat :
    tokens_consume(cnt->tokens);

    Ast *decl  = ast_create_node(cnt->ast_arena, AST_DECL_ENTITY, tok_begin, SCOPE_GET(cnt));
    decl->docs = pop_docs(cnt);
    decl->data.decl.name       = ident;
    decl->data.decl_entity.mut = true;

    DECL_PUSH(cnt, decl);

    decl->data.decl.type = parse_type(cnt);
    Token *tok_assign    = tokens_consume_if(cnt->tokens, SYM_ASSIGN);
    if (!tok_assign) tok_assign = tokens_consume_if(cnt->tokens, SYM_COLON);

    // Parse hash directives.
    s32 hd_accepted = HD_NONE;

    if (tok_assign) {
        decl->data.decl_entity.mut = token_is(tok_assign, SYM_ASSIGN);

        // parse declaration expression
        decl->data.decl_entity.value = parse_expr(cnt);

        if (IS_NOT_FLAG(decl->data.decl_entity.flags, FLAG_EXTERN)) {
            if (!decl->data.decl_entity.value) {
                PARSE_ERROR(ERR_EXPECTED_INITIALIZATION,
                            tok_assign,
                            BUILDER_CUR_AFTER,
                            "Expected binding of declaration to some value.");
                DECL_POP(cnt);
                return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
            }
        }
    } else {
        hd_accepted |= HD_NO_INIT;
    }

    Ast *init_value = decl->data.decl_entity.value;

    if (!init_value || rq_semicolon_after_decl_entity(init_value)) {
        u32 flags = 0;
        while (true) {
            HashDirective found = HD_NONE;
            parse_hash_directive(cnt, hd_accepted, &found);
            if (!hash_directive_to_flags(found, &flags)) break;
            hd_accepted &= ~found;
        }

        if (IS_FLAG(flags, FLAG_NO_INIT) && scope_is_global(SCOPE_GET(cnt))) {
            PARSE_ERROR(ERR_EXPECTED_INITIALIZATION,
                        tok_begin,
                        BUILDER_CUR_AFTER,
                        "Invalid 'noinit' directive for global variable '%s'. All globals must "
                        "be initialized either by explicit value or implicit default value.",
                        ident->data.ident.id.str);
        }

        decl->data.decl_entity.flags |= flags;
    }

    DECL_POP(cnt);
    return decl;
}

Ast *parse_expr_call(Context *cnt, Ast *prev)
{
    if (!prev) return NULL;

    Token *tok = tokens_consume_if(cnt->tokens, SYM_LPAREN);
    if (!tok) return NULL;

    Ast *call                = ast_create_node(cnt->ast_arena, AST_EXPR_CALL, tok, SCOPE_GET(cnt));
    call->data.expr_call.ref = prev;
    call->data.expr_call.run = false;

    // parse args
    bool rq = false;
    Ast *tmp;

arg:
    tmp = parse_expr(cnt);
    if (tmp) {
        if (!call->data.expr_call.args)
            call->data.expr_call.args = create_sarr(TSmallArray_AstPtr, cnt->assembly);
        tsa_push_AstPtr(call->data.expr_call.args, tmp);

        if (tokens_consume_if(cnt->tokens, SYM_COMMA)) {
            rq = true;
            goto arg;
        }
    } else if (rq) {
        Token *tok_err = tokens_peek(cnt->tokens);
        if (tokens_peek_2nd(cnt->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(ERR_EXPECTED_NAME,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected function argument after comma ','.");
            return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
        }
    }

    tok = tokens_consume(cnt->tokens);
    if (tok->sym != SYM_RPAREN) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of parameter list ')' or another parameter separated by comma.");
        return ast_create_node(cnt->ast_arena, AST_BAD, tok, SCOPE_GET(cnt));
    }

    return call;
}

Ast *parse_expr_null(Context *cnt)
{
    Token *tok_null = tokens_consume_if(cnt->tokens, SYM_NULL);
    if (!tok_null) return NULL;
    return ast_create_node(cnt->ast_arena, AST_EXPR_NULL, tok_null, SCOPE_GET(cnt));
}

Ast *parse_unrecheable(Context *cnt)
{
    Token *tok = tokens_consume_if(cnt->tokens, SYM_UNREACHABLE);
    if (!tok) return NULL;

    return ast_create_node(cnt->ast_arena, AST_UNREACHABLE, tok, SCOPE_GET(cnt));
}

Ast *parse_expr_type(Context *cnt)
{
    Token *tok  = tokens_peek(cnt->tokens);
    Ast *  type = NULL;

    type = parse_type_struct(cnt);

    // keep order
    if (!type) type = parse_type_slice(cnt);
    if (!type) type = parse_type_dynarr(cnt);
    if (!type) type = parse_type_arr(cnt);
    // keep order

    if (!type) type = parse_type_enum(cnt);
    if (!type) type = parse_type_ptr(cnt);

    if (type) {
        Ast *expr = ast_create_node(cnt->ast_arena, AST_EXPR_TYPE, tok, SCOPE_GET(cnt));
        expr->data.expr_type.type = type;
        return expr;
    }

    return NULL;
}

Ast *parse_block(Context *cnt, bool create_scope)
{
    Token *tok_begin = tokens_consume_if(cnt->tokens, SYM_LBLOCK);
    if (!tok_begin) return NULL;

    if (create_scope) {
        Scope *scope = scope_create(
            cnt->scope_arenas, SCOPE_LEXICAL, SCOPE_GET(cnt), 1024, &tok_begin->location);

        SCOPE_PUSH(cnt, scope);
    }

    Ast *block = ast_create_node(cnt->ast_arena, AST_BLOCK, tok_begin, SCOPE_GET(cnt));

    Token *tok;
    Ast *  tmp;
    block->data.block.nodes = tarray_new(sizeof(Ast *));

NEXT:
    if (tokens_current_is(cnt->tokens, SYM_SEMICOLON)) {
        tok = tokens_consume(cnt->tokens);
        PARSE_WARNING(tok, BUILDER_CUR_WORD, "extra semicolon can be removed ';'");
        goto NEXT;
    }

    parse_hash_directive(cnt, 0, NULL);

    if ((tmp = (Ast *)parse_decl(cnt))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(cnt);
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_stmt_return(cnt))) {
        if (!AST_IS_BAD(tmp)) parse_semicolon_rq(cnt);
        tarray_push(block->data.block.nodes, tmp);
        block->data.block.has_return      = true;
        tmp->data.stmt_return.owner_block = block;
        goto NEXT;
    }

    if ((tmp = parse_stmt_if(cnt))) {
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_stmt_switch(cnt))) {
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_stmt_loop(cnt))) {
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_stmt_break(cnt))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(cnt);
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_stmt_continue(cnt))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(cnt);
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_stmt_defer(cnt))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(cnt);
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_expr(cnt))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(cnt);
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_block(cnt, true))) {
        tarray_push(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = parse_unrecheable(cnt))) {
        tarray_push(block->data.block.nodes, tmp);
        parse_semicolon_rq(cnt);
        goto NEXT;
    }

    tok = tokens_consume_if(cnt->tokens, SYM_RBLOCK);
    if (!tok) {
        tok = tokens_peek_prev(cnt->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
        if (create_scope) SCOPE_POP(cnt);
        return ast_create_node(cnt->ast_arena, AST_BAD, tok_begin, SCOPE_GET(cnt));
    }

    // store location of block end
    block->location_end = &tok->location;

    if (create_scope) SCOPE_POP(cnt);
    return block;
}

void parse_ublock_content(Context *cnt, Ast *ublock)
{
    BL_ASSERT(ublock->kind == AST_UBLOCK);
    ublock->data.ublock.nodes = tarray_new(sizeof(Ast *));

    Ast *tmp;
NEXT:
    if (parse_semicolon(cnt)) goto NEXT;
    if (parse_docs(cnt)) goto NEXT;
    if (parse_unit_docs(cnt)) goto NEXT;

    if ((tmp = parse_decl(cnt))) {
        if (AST_IS_OK(tmp)) {
            Ast *decl = tmp->data.decl_entity.value;
            if (decl && rq_semicolon_after_decl_entity(decl)) parse_semicolon_rq(cnt);
            // setup global scope flag for declaration
            tmp->data.decl_entity.in_gscope = true;
            if (cnt->current_private_scope) tmp->data.decl_entity.flags |= FLAG_PRIVATE;
        }

        tarray_push(ublock->data.ublock.nodes, tmp);
        goto NEXT;
    }

    // load, import, link, test, private - enabled in global scope
    const int enabled_hd = HD_LOAD | HD_LINK | HD_PRIVATE | HD_IMPORT;
    if ((tmp = parse_hash_directive(cnt, enabled_hd, NULL))) {
        tarray_push(ublock->data.ublock.nodes, tmp);
        goto NEXT;
    }

    Token *tok = tokens_peek(cnt->tokens);
    if (!token_is(tok, SYM_EOF)) {
        PARSE_ERROR(ERR_UNEXPECTED_SYMBOL,
                    tok,
                    BUILDER_CUR_WORD,
                    "Unexpected symbol in module body '%s'.",
                    sym_strings[tok->sym]);
    }
}

void parser_run(Assembly *assembly, Unit *unit)
{
    BL_ASSERT(assembly->gscope && "Missing global scope for assembly.");

    TracyCZone(_tctx, true);
    Context cnt = {
        .assembly     = assembly,
        .unit         = unit,
        .ast_arena    = &assembly->arenas.ast,
        .scope_arenas = &assembly->arenas.scope,
        .tokens       = &unit->tokens,
        .inside_loop  = false,
    };

    tsa_init(&cnt._decl_stack);
    tsa_init(&cnt._scope_stack);

    SCOPE_PUSH(&cnt, assembly->gscope);

    Ast *root              = ast_create_node(cnt.ast_arena, AST_UBLOCK, NULL, SCOPE_GET(&cnt));
    root->data.ublock.unit = unit;
    unit->ast              = root;

#if 0
	if (assembly->options.build_mode == BUILD_MODE_DEBUG) {
		unit->llvm_file_meta =
		    llvm_di_create_file(assembly->llvm.di_builder, unit->filename, unit->dirpath);
	}
#endif

    parse_ublock_content(&cnt, unit->ast);
    if (cnt.unit_docs_tmp) unit->ast->docs = cnt.unit_docs_tmp->data;

    tsa_terminate(&cnt._decl_stack);
    tsa_terminate(&cnt._scope_stack);
    TracyCZoneEnd(_tctx);
}
