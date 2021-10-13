// =================================================================================================
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
// =================================================================================================

#include "builder.h"
#include "tokens_inline_utils.h"
#include <setjmp.h>

#define EXPECTED_PRIVATE_SCOPE_COUNT 256
#define EXPECTED_NAMED_SCOPE_COUNT 4094

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

#define scope_push(ctx, scope) arrput((ctx)->scope_stack, (scope))
#define scope_pop(ctx) arrpop((ctx)->scope_stack)
#define scope_get(ctx) arrlast((ctx)->scope_stack)
#define scope_set(ctx, scope) (arrlast((ctx)->scope_stack) = (scope))

#define decl_push(ctx, decl) arrput((ctx)->decl_stack, decl)
#define decl_pop(ctx) arrpop((ctx)->decl_stack)
#define decl_get(ctx) (arrlenu((ctx)->decl_stack) ? arrlast((ctx)->decl_stack) : NULL)

#define CONSUME_TILL(tokens, ...)                                                                  \
    {                                                                                              \
        enum sym _[] = {__VA_ARGS__};                                                              \
        tokens_consume_till2((tokens), TARRAY_SIZE(_), &_[0]);                                     \
    }

enum hash_directive_flags {
#define HD_GEN(kind, name, flag) kind = flag,
#include "parser.inc"
#undef HD_GEN
};

struct context {
    struct {
        u32                       key;
        enum hash_directive_flags value;
    } * hash_directive_table;

    struct assembly     *assembly;
    struct unit         *unit;
    struct arena        *ast_arena;
    struct scope_arenas *scope_arenas;
    struct tokens       *tokens;

    // tmps
    struct scope **scope_stack;
    struct ast   **decl_stack;
    struct ast   **fn_type_stack;
    bool           is_inside_loop;
    struct scope  *current_private_scope;
    struct scope  *current_named_scope;
    struct ast    *current_docs;
    TString       *unit_docs_tmp;
};

// helpers
// fw decls
static enum binop_kind sym_to_binop_kind(enum sym sm);
static enum unop_kind  sym_to_unop_kind(enum sym sm);
static bool            parse_docs(struct context *ctx);
static bool            parse_unit_docs(struct context *ctx);
static void            parse_ublock_content(struct context *ctx, struct ast *ublock);
static struct ast *
parse_hash_directive(struct context *ctx, s32 expected_mask, enum hash_directive_flags *satisfied);
static struct ast *parse_unrecheable(struct context *ctx);
static struct ast *parse_debugbreak(struct context *ctx);
static struct ast *parse_ident(struct context *ctx);
static struct ast *parse_ident_group(struct context *ctx);
static struct ast *parse_block(struct context *ctx, bool create_scope);
static struct ast *parse_decl(struct context *ctx);
static struct ast *parse_decl_member(struct context *ctx, s32 index);
static struct ast *parse_decl_arg(struct context *ctx, bool named);
static struct ast *parse_decl_variant(struct context *ctx, struct ast *prev);
static struct ast *parse_type(struct context *ctx);
static struct ast *parse_ref(struct context *ctx);
static struct ast *parse_ref_nested(struct context *ctx, struct ast *prev);
static struct ast *parse_type_polymorph(struct context *ctx);
static struct ast *parse_type_arr(struct context *ctx);
static struct ast *parse_type_slice(struct context *ctx);
static struct ast *parse_type_dynarr(struct context *ctx);
static struct ast *parse_type_fn(struct context *ctx, bool named_args);
static struct ast *parse_type_fn_group(struct context *ctx);
static struct ast *parse_type_fn_return(struct context *ctx);
static struct ast *parse_type_struct(struct context *ctx);
static struct ast *parse_type_enum(struct context *ctx);
static struct ast *parse_type_ptr(struct context *ctx);
static struct ast *parse_type_vargs(struct context *ctx);
static struct ast *parse_stmt_return(struct context *ctx);
static struct ast *parse_stmt_if(struct context *ctx, bool is_static);
static struct ast *parse_stmt_loop(struct context *ctx);
static struct ast *parse_stmt_break(struct context *ctx);
static struct ast *parse_stmt_continue(struct context *ctx);
static struct ast *parse_stmt_defer(struct context *ctx);
static struct ast *parse_stmt_switch(struct context *ctx);
static struct ast *parse_stmt_case(struct context *ctx);

// EXPRESSIONS
static struct ast *parse_expr(struct context *ctx);
static struct ast *_parse_expr(struct context *ctx, s32 p);
static struct ast *parse_expr_atom(struct context *ctx);
static struct ast *parse_expr_primary(struct context *ctx);
static struct ast *parse_expr_unary(struct context *ctx);
static struct ast *
parse_expr_binary(struct context *ctx, struct ast *lhs, struct ast *rhs, struct token *op);
static struct ast *parse_expr_addrof(struct context *ctx);
static struct ast *parse_expr_deref(struct context *ctx);
static struct ast *parse_expr_type(struct context *ctx);
static struct ast *parse_expr_ref(struct context *ctx);
static struct ast *parse_expr_nested(struct context *ctx);
static struct ast *parse_expr_null(struct context *ctx);
static struct ast *parse_expr_cast(struct context *ctx);
static struct ast *parse_expr_cast_auto(struct context *ctx);
static struct ast *parse_expr_lit(struct context *ctx);
static struct ast *parse_expr_lit_fn(struct context *ctx);
static struct ast *parse_expr_lit_fn_group(struct context *ctx);
static struct ast *parse_expr_sizeof(struct context *ctx);
static struct ast *parse_expr_type_info(struct context *ctx);
static struct ast *parse_expr_test_cases(struct context *ctx);
static struct ast *parse_expr_alignof(struct context *ctx);
static INLINE bool parse_semicolon(struct context *ctx);
static INLINE bool parse_semicolon_rq(struct context *ctx);
static INLINE bool hash_directive_to_flags(enum hash_directive_flags hd, u32 *out_flags);
static struct ast *parse_expr_call(struct context *ctx, struct ast *prev, const bool is_comptime);
static struct ast *parse_expr_elem(struct context *ctx, struct ast *prev);
static struct ast *parse_expr_compound(struct context *ctx);

// impl

static INLINE bool rq_semicolon_after_decl_entity(struct ast *node)
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

static INLINE const char *pop_docs(struct context *ctx)
{
    const char *text = NULL;
    if (ctx->current_docs) {
        text              = ctx->current_docs->data.docs.text;
        ctx->current_docs = NULL;
    }
    return text;
}

enum binop_kind sym_to_binop_kind(enum sym sm)
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
    case SYM_AND_ASSIGN:
        return BINOP_AND_ASSIGN;
    case SYM_OR_ASSIGN:
        return BINOP_OR_ASSIGN;
    case SYM_XOR_ASSIGN:
        return BINOP_XOR_ASSIGN;
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
    case SYM_XOR:
        return BINOP_XOR;
    case SYM_SHR:
        return BINOP_SHR;
    case SYM_SHL:
        return BINOP_SHL;
    default:
        BL_ABORT("unknown binop operation!!!");
    }
}

enum unop_kind sym_to_unop_kind(enum sym sm)
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

struct ast *parse_expr_ref(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_peek(ctx->tokens);
    if (tok->sym != SYM_IDENT) RETURN_ZONE(NULL);
    struct ast *ident = parse_ident(ctx);
    BL_ASSERT(ident);
    struct ast *ref     = ast_create_node(ctx->ast_arena, AST_REF, tok, scope_get(ctx));
    ref->data.ref.ident = ident;
    RETURN_ZONE(ref);
}

bool parse_docs(struct context *ctx)
{
    struct token *tok_begin = tokens_peek(ctx->tokens);
    if (token_is_not(tok_begin, SYM_DCOMMENT)) return false;
    TString      *str = builder_create_cached_str();
    struct token *tok;
    while ((tok = tokens_consume_if(ctx->tokens, SYM_DCOMMENT))) {
        if (str->len > 0) tstring_append(str, "\n");
        tstring_append(str, tok->value.str);
    }

    struct ast *docs     = ast_create_node(ctx->ast_arena, AST_DOCS, tok_begin, scope_get(ctx));
    docs->data.docs.text = str->data;
    ctx->current_docs    = docs;
    return true;
}

bool parse_unit_docs(struct context *ctx)
{
    struct token *tok_begin = tokens_peek(ctx->tokens);
    if (token_is_not(tok_begin, SYM_DGCOMMENT)) return false;
    if (!ctx->unit_docs_tmp) ctx->unit_docs_tmp = builder_create_cached_str();
    TString      *str = ctx->unit_docs_tmp;
    struct token *tok;
    while ((tok = tokens_consume_if(ctx->tokens, SYM_DGCOMMENT))) {
        if (str->len > 0) tstring_append(str, "\n");
        tstring_append(str, tok->value.str);
    }
    return true;
}

// Try to parse hash directive. List of enabled directives can be set by 'expected_mask',
// 'satisfied' is optional output set to parsed directive id if there is one.
struct ast *
parse_hash_directive(struct context *ctx, s32 expected_mask, enum hash_directive_flags *satisfied)
{
    ZONE();
#define set_satisfied(_hd)                                                                         \
    {                                                                                              \
        if (satisfied) *satisfied = _hd;                                                           \
    }                                                                                              \
    (void)0

    set_satisfied(HD_NONE);
    struct token *tok_hash = tokens_consume_if(ctx->tokens, SYM_HASH);
    if (!tok_hash) RETURN_ZONE(NULL);
    // Special case for static if
    {
        struct ast *if_stmt = parse_stmt_if(ctx, true);
        if (if_stmt) {
            set_satisfied(HD_STATIC_IF);
            if (IS_NOT_FLAG(expected_mask, HD_STATIC_IF)) {
                builder_msg(BUILDER_MSG_ERROR,
                            0,
                            if_stmt->location,
                            BUILDER_CUR_WORD,
                            "Static if is not allowed in this context.");
                RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_hash, scope_get(ctx)));
            }
            RETURN_ZONE(if_stmt);
        }
    }

    struct token *tok_directive = tokens_consume(ctx->tokens);
    if (tok_directive->sym != SYM_IDENT) goto INVALID;

    const char  *directive = tok_directive->value.str;
    const hash_t hash      = strhash(directive);
    const s64    index     = hmgeti(ctx->hash_directive_table, hash);
    if (index == -1) goto INVALID;
    const enum hash_directive_flags hd_flag = ctx->hash_directive_table[index].value;
    BL_ASSERT(directive);

    if (IS_NOT_FLAG(expected_mask, hd_flag)) {
        PARSE_ERROR(
            ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unexpected directive.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
    }
    set_satisfied(hd_flag);

    switch (hd_flag) {
    case HD_NONE:
    case HD_STATIC_IF:
        BL_ABORT("Invalid directive!");
    case HD_TEST_FN:
    case HD_BUILD_ENTRY:
    case HD_NO_INIT:
    case HD_FLAGS:
    case HD_INLINE:
    case HD_NO_INLINE:
    case HD_THREAD_LOCAL:
    case HD_ENTRY:
    case HD_EXPORT:
    case HD_COMPILER: {
        // only flags
        RETURN_ZONE(NULL);
    }

    case HD_COMPTIME: {
        BL_TRACY_MESSAGE("HD_FLAG", "#comptime");
        struct ast *ref = parse_expr_ref(ctx);
        if (!ref) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_INVALID_EXPR, tok_err, BUILDER_CUR_WORD, "Expected call.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        struct ast *call = parse_expr_call(ctx, ref, true);
        if (!call) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_INVALID_EXPR, tok_err, BUILDER_CUR_WORD, "Expected call.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        RETURN_ZONE(call);
    }

    case HD_ERROR: {
        BL_TRACY_MESSAGE("HD_FLAG", "#error");
        struct token *tok_msg = tokens_consume_if(ctx->tokens, SYM_STRING);
        if (!tok_msg) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected message after 'error' directive.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        struct ast *msg = ast_create_node(ctx->ast_arena, AST_MSG, tok_directive, scope_get(ctx));
        msg->data.msg.text = tok_msg->value.str;
        msg->data.msg.kind = AST_MSG_ERROR;
        RETURN_ZONE(msg);
    }

    case HD_WARNING: {
        BL_TRACY_MESSAGE("HD_FLAG", "#warning");
        struct token *tok_msg = tokens_consume_if(ctx->tokens, SYM_STRING);
        if (!tok_msg) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected message after 'warning' directive.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        struct ast *msg = ast_create_node(ctx->ast_arena, AST_MSG, tok_directive, scope_get(ctx));
        msg->data.msg.text = tok_msg->value.str;
        msg->data.msg.kind = AST_MSG_WARNING;
        RETURN_ZONE(msg);
    }

    case HD_LOAD: {
        BL_TRACY_MESSAGE("HD_FLAG", "#load");
        struct token *tok_path = tokens_consume_if(ctx->tokens, SYM_STRING);
        if (!tok_path) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected path \"some/path\" after 'load' directive.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }

        struct ast *load = ast_create_node(ctx->ast_arena, AST_LOAD, tok_directive, scope_get(ctx));
        load->data.load.filepath = tok_path->value.str;
        if (ctx->assembly->target->kind != ASSEMBLY_DOCS) {
            assembly_add_unit_safe(ctx->assembly, load->data.load.filepath, tok_path);
        }
        RETURN_ZONE(load);
    }

    case HD_IMPORT: {
        struct token *tok_path = tokens_consume_if(ctx->tokens, SYM_STRING);
        if (!tok_path) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected path \"some/path\" after 'import' directive.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        struct ast *import =
            ast_create_node(ctx->ast_arena, AST_IMPORT, tok_directive, scope_get(ctx));
        import->data.import.filepath = tok_path->value.str;
        BL_TRACY_MESSAGE("HD_FLAG", "#import (%s)", tok_path->value.str);
        if (ctx->assembly->target->kind != ASSEMBLY_DOCS) {
            assembly_import_module(ctx->assembly, tok_path->value.str, tok_path);
        }
        RETURN_ZONE(import);
    }

    case HD_ASSERT: {
        BL_TRACY_MESSAGE("HD_FLAG", "#assert");
        BL_ASSERT(tok_directive->sym == SYM_IDENT);
        struct ast *ident =
            ast_create_node(ctx->ast_arena, AST_IDENT, tok_directive, scope_get(ctx));
        ident->data.ident.id = *BID(STATIC_ASSERT_FN);
        struct ast *ref = ast_create_node(ctx->ast_arena, AST_REF, tok_directive, scope_get(ctx));
        ref->data.ref.ident = ident;
        struct ast *call    = parse_expr_call(ctx, ref, true);
        if (!call) {
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Static assert is supposed to be a function call '#assert(false)'.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        RETURN_ZONE(call);
    }

    case HD_LINK: {
        BL_TRACY_MESSAGE("HD_FLAG", "#link");
        struct token *tok_path = tokens_consume(ctx->tokens);
        if (!token_is(tok_path, SYM_STRING)) {
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_path,
                        BUILDER_CUR_WORD,
                        "Expected path \"some/path\" after 'link' directive.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }

        struct ast *link = ast_create_node(ctx->ast_arena, AST_LINK, tok_directive, scope_get(ctx));
        link->data.link.lib = tok_path->value.str;

        assembly_add_native_lib(ctx->assembly, tok_path->value.str, tok_path);

        PARSE_WARNING(tok_directive,
                      BUILDER_CUR_WORD,
                      "Link directive is deprecated and will be removed in next release. Please "
                      "use build system to link dependencies or module import.");
        RETURN_ZONE(link);
    }

    case HD_FILE: {
        BL_TRACY_MESSAGE("HD_FLAG", "#file");
        struct ast *file =
            ast_create_node(ctx->ast_arena, AST_EXPR_LIT_STRING, tok_directive, scope_get(ctx));
        file->data.expr_string.val = tok_directive->location.unit->filepath;
        RETURN_ZONE(file);
    }

    case HD_BASE: {
        BL_TRACY_MESSAGE("HD_FLAG", "#line");
        RETURN_ZONE(parse_type(ctx));
    }

    case HD_TAGS: {
        BL_TRACY_MESSAGE("HD_FLAG", "#tags");
        // Tags can contain one or move references separated by comma
        struct ast *tag;
        bool        rq = false;

        TSmallArray_AstPtr *values = create_sarr(TSmallArray_AstPtr, ctx->assembly);

    VALUE:
        tag = parse_expr_ref(ctx);
        if (tag) {
            tsa_push_AstPtr(values, tag);

            if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
                rq = true;
                goto VALUE;
            }
        } else if (rq) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(ERR_EXPECTED_NAME,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected another tag after comma ','.");

            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }

        if (!values->size) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            PARSE_ERROR(
                ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected tag value after #tags.");

            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }

        struct ast *tags = ast_create_node(ctx->ast_arena, AST_TAGS, tok_directive, scope_get(ctx));

        tags->data.tags.values = values;
        RETURN_ZONE(tags);
    }

    case HD_LINE: {
        BL_TRACY_MESSAGE("HD_FLAG", "#line");
        struct ast *line =
            ast_create_node(ctx->ast_arena, AST_EXPR_LIT_INT, tok_directive, scope_get(ctx));
        line->data.expr_integer.val = tok_directive->location.line;
        RETURN_ZONE(line);
    }

    case HD_CALL_LOC: {
        BL_TRACY_MESSAGE("HD_FLAG", "#call_location");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_CALL_LOC, tok_directive, scope_get(ctx)));
    }

    case HD_EXTERN: {
        BL_TRACY_MESSAGE("HD_FLAG", "#extern");
        // Extern flag extension could be linkage name as string
        struct token *tok_ext = tokens_consume_if(ctx->tokens, SYM_STRING);
        if (!tok_ext) RETURN_ZONE(NULL);
        // Parse extension token.
        struct ast *ext = ast_create_node(ctx->ast_arena, AST_IDENT, tok_ext, scope_get(ctx));
        id_init(&ext->data.ident.id, tok_ext->value.str);
        RETURN_ZONE(ext);
    }

    case HD_INTRINSIC: {
        BL_TRACY_MESSAGE("HD_FLAG", "#intrinsic");
        // Intrinsic flag extension could be linkage name as string
        struct token *tok_ext = tokens_consume_if(ctx->tokens, SYM_STRING);
        if (!tok_ext) RETURN_ZONE(NULL);
        // Parse extension token.
        struct ast *ext = ast_create_node(ctx->ast_arena, AST_IDENT, tok_ext, scope_get(ctx));
        id_init(&ext->data.ident.id, tok_ext->value.str);
        RETURN_ZONE(ext);
    }

    case HD_PRIVATE: {
        BL_TRACY_MESSAGE("HD_FLAG", "#private");
        if (ctx->current_private_scope) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. File already contains private scope block.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }

        // Here we create private scope for the current unit. (only when source file
        // contains private block).
        //
        // Parent of this scope is a global-scope.
        //
        // This scope has also highest priority during symbol lookup inside the current unit
        // and it is visible only from such unit.
        // Private scope contains only global entity declarations with 'private' flag set
        // in ast node.
        struct scope *scope = scope_create(ctx->scope_arenas,
                                           SCOPE_PRIVATE,
                                           scope_get(ctx),
                                           EXPECTED_PRIVATE_SCOPE_COUNT,
                                           &tok_directive->location);

        ctx->current_private_scope = scope;
        scope->llvm_meta           = scope->parent->llvm_meta;
        ctx->unit->private_scope   = scope;
        scope_set(ctx, scope);

        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_PRIVATE, tok_directive, scope_get(ctx)));
    }
    case HD_SCOPE: {
        BL_TRACY_MESSAGE("HD_FLAG", "#scope");
        struct ast *ident = parse_ident(ctx);
        if (!ident) {
            PARSE_ERROR(ERR_INVALID_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_AFTER,
                        "Expected scope name after #scope directive.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        struct ast *scope =
            ast_create_node(ctx->ast_arena, AST_SCOPE, tok_directive, scope_get(ctx));
        scope->data.scope.ident = ident;
        struct id *id           = &ident->data.ident.id;

        // Perform lookup of named scope here, in case named scope already exist in global scope
        // we can reuse it!.
        if (scope_get(ctx)->kind == SCOPE_GLOBAL) scope_lock(scope_get(ctx));
        struct scope_entry *scope_entry =
            scope_lookup(scope_get(ctx), SCOPE_DEFAULT_LAYER, id, false, false, NULL);
        if (scope_entry) {
            BL_ASSERT(scope_entry->kind == SCOPE_ENTRY_NAMED_SCOPE &&
                      "Found scope entry is expected to be named scope!");
            BL_ASSERT(scope_entry->data.scope && scope_entry->data.scope->kind == SCOPE_NAMED);
        } else {
            scope_entry = scope_create_entry(
                &ctx->assembly->arenas.scope, SCOPE_ENTRY_NAMED_SCOPE, id, scope, false);
            scope_insert(scope_get(ctx), SCOPE_DEFAULT_LAYER, scope_entry);
            struct scope *named_scope = scope_create(ctx->scope_arenas,
                                                     SCOPE_NAMED,
                                                     scope_get(ctx),
                                                     EXPECTED_NAMED_SCOPE_COUNT,
                                                     &tok_directive->location);
            named_scope->name         = id->str;
            scope_entry->data.scope   = named_scope;
        }
        if (scope_get(ctx)->kind == SCOPE_GLOBAL) scope_unlock(scope_get(ctx));
        if (ctx->current_named_scope) {
            PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE,
                        tok_directive,
                        BUILDER_CUR_WORD,
                        "Unexpected directive. File already contains named scope block.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
        }
        BL_ASSERT(scope_entry->data.scope);
        ctx->current_named_scope = scope_entry->data.scope;
        scope_set(ctx, ctx->current_named_scope);
        RETURN_ZONE(scope);
    }
    }
INVALID:
    PARSE_ERROR(ERR_UNEXPECTED_DIRECTIVE, tok_directive, BUILDER_CUR_WORD, "Unknown directive.");
    RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_directive, scope_get(ctx)));
#undef set_satisfied
}

struct ast *parse_expr_compound(struct context *ctx)
{
    ZONE();
    if (!tokens_is_seq(ctx->tokens, 2, SYM_LBLOCK, SYM_COLON)) RETURN_ZONE(NULL);
    // eat {
    struct token *tok_begin = tokens_consume(ctx->tokens);
    // eat :
    tokens_consume(ctx->tokens);

    struct ast *type = parse_type(ctx);
    if (!type) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected type.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    // eat :
    if (!tokens_consume_if(ctx->tokens, SYM_COLON)) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected colon after type.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *compound =
        ast_create_node(ctx->ast_arena, AST_EXPR_COMPOUND, tok_begin, scope_get(ctx));
    compound->data.expr_compound.type = type;

    // parse values
    bool        rq = false;
    struct ast *tmp;

NEXT:
    tmp = parse_expr(ctx);
    if (tmp) {
        if (!compound->data.expr_compound.values)
            compound->data.expr_compound.values = create_sarr(TSmallArray_AstPtr, ctx->assembly);

        tsa_push_AstPtr(compound->data.expr_compound.values, tmp);

        if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        if (tokens_peek_2nd(ctx->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(ERR_EXPECTED_NAME,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected expression after comma ','.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
        }
    }

    struct token *tok = tokens_consume(ctx->tokens);
    if (tok->sym != SYM_RBLOCK) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of initialization list '}' or another expression "
                    "separated by comma.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }
    RETURN_ZONE(compound);
}

struct ast *parse_expr_sizeof(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_SIZEOF);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct token *tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Expected '(' after sizeof operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *szof = ast_create_node(ctx->ast_arena, AST_EXPR_SIZEOF, tok_begin, scope_get(ctx));
    szof->data.expr_sizeof.node = parse_expr(ctx);
    if (!szof->data.expr_sizeof.node) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after sizeof operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(szof);
}

struct ast *parse_expr_type_info(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_TYPEINFO);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct token *tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Expected '(' after typeinfo operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *info =
        ast_create_node(ctx->ast_arena, AST_EXPR_TYPE_INFO, tok_begin, scope_get(ctx));
    info->data.expr_type_info.node = parse_expr(ctx);
    if (!info->data.expr_type_info.node) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after typeinfo operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(info);
}

struct ast *parse_expr_test_cases(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_TESTCASES);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct token *tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Expected '(' after testcases operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *tc =
        ast_create_node(ctx->ast_arena, AST_EXPR_TEST_CASES, tok_begin, scope_get(ctx));

    tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after testcases operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(tc);
}

struct ast *parse_expr_alignof(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_ALIGNOF);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct token *tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_WORD, "Expected '(' after cast operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *alof = ast_create_node(ctx->ast_arena, AST_EXPR_ALIGNOF, tok_begin, scope_get(ctx));
    alof->data.expr_alignof.node = parse_expr(ctx);
    if (!alof->data.expr_alignof.node) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after alignof operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(alof);
}

struct ast *parse_expr_cast_auto(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_CAST_AUTO);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct ast *cast = ast_create_node(ctx->ast_arena, AST_EXPR_CAST, tok_begin, scope_get(ctx));
    cast->data.expr_cast.auto_cast = true;

    cast->data.expr_cast.next = _parse_expr(ctx, token_prec(tok_begin).priority);
    if (!cast->data.expr_cast.next) {
        struct token *tok = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "Expected expression after auto cast.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(cast);
}

struct ast *parse_expr_cast(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_CAST);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct token *tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_LPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok_begin, BUILDER_CUR_WORD, "Expected '(' after expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *cast = ast_create_node(ctx->ast_arena, AST_EXPR_CAST, tok_begin, scope_get(ctx));
    cast->data.expr_cast.type = parse_type(ctx);
    if (!cast->data.expr_cast.type) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_TYPE, tok_err, BUILDER_CUR_WORD, "Expected type name as cast parameter.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    tok = tokens_consume(ctx->tokens);
    if (!token_is(tok, SYM_RPAREN)) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected ')' after cast expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    cast->data.expr_cast.next = _parse_expr(ctx, token_prec(tok_begin).priority);
    if (!cast->data.expr_cast.next) {
        tok = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok, BUILDER_CUR_WORD, "Expected expression after cast.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(cast);
}

struct ast *parse_decl_member(struct context *ctx, s32 UNUSED(index))
{
    ZONE();
    struct token *tok_begin = tokens_peek(ctx->tokens);
    struct ast   *name      = NULL;
    struct ast   *type      = NULL;
    const bool    named     = tokens_peek_2nd(ctx->tokens)->sym == SYM_COLON;

    if (named) {
        name = parse_ident(ctx);
        if (!name) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_TYPE,
                        &tok_begin->location,
                        BUILDER_CUR_AFTER,
                        "Expected member name.");
            tokens_consume(ctx->tokens);
        }
        BL_ASSERT(tokens_current_is(ctx->tokens, SYM_COLON));
        tokens_consume(ctx->tokens);
        type = parse_type(ctx);
        if (!type) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_TYPE,
                        name->location,
                        BUILDER_CUR_AFTER,
                        "Expected type.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
        }
    } else {
        type = parse_type(ctx);
        if (!type) RETURN_ZONE(NULL);
    }

    if (!name) {
        BL_ASSERT(index >= 0);
        char index_str[22];
        sprintf(index_str, "_%d", index);
        TString *ident_str = builder_create_cached_str();
        tstring_append(ident_str, index_str);
        name = ast_create_node(ctx->ast_arena, AST_IDENT, tok_begin, scope_get(ctx));
        id_init(&name->data.ident.id, ident_str->data);
    }

    enum hash_directive_flags found_hd = HD_NONE;
    struct ast               *tags     = parse_hash_directive(ctx, HD_TAGS, &found_hd);
    struct ast *mem = ast_create_node(ctx->ast_arena, AST_DECL_MEMBER, tok_begin, scope_get(ctx));
    mem->docs       = pop_docs(ctx);
    mem->data.decl.type = type;
    mem->data.decl.name = name;
    mem->data.decl.tags = tags;
    RETURN_ZONE(mem);
}

struct ast *parse_decl_arg(struct context *ctx, bool named)
{
    ZONE();
    struct token *tok_begin = tokens_peek(ctx->tokens);
    struct ast   *name      = NULL;
    struct ast   *type      = NULL;
    struct ast   *value     = NULL;
    if (tokens_current_is(ctx->tokens, SYM_RPAREN)) RETURN_ZONE(NULL);
    if (tokens_is_seq(ctx->tokens, 2, SYM_IDENT, SYM_COLON)) {
        // <name> :
        name = parse_ident(ctx);
        tokens_consume(ctx->tokens); // eat :
    } else if (named) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_NAME,
                    &tok_err->location,
                    BUILDER_CUR_AFTER,
                    "Expected argument name followed by colon.");

        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }
    type = parse_type(ctx);
    // Parse optional default value expression.
    if (tokens_current_is(ctx->tokens, SYM_COLON)) {
        struct token *tok_err = tokens_consume(ctx->tokens);
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_INVALID_MUTABILITY,
                    &tok_err->location,
                    BUILDER_CUR_WORD,
                    "Function argument cannot be constant (this maybe shoule be possible "
                    "in future).");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }
    if (tokens_consume_if(ctx->tokens, SYM_ASSIGN)) {
        value = parse_hash_directive(ctx, HD_CALL_LOC, NULL);
        if (!value) value = parse_expr(ctx);
        if (!value) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_NAME,
                        &tok_err->location,
                        BUILDER_CUR_AFTER,
                        "Expected .");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
        }
    } else if (!type) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_EXPECTED_TYPE,
                    name->location,
                    BUILDER_CUR_AFTER,
                    "Expected argument type.");
        RETURN_ZONE(
            ast_create_node(ctx->ast_arena, AST_BAD, tokens_peek(ctx->tokens), scope_get(ctx)));
    }
    struct ast *arg = ast_create_node(ctx->ast_arena, AST_DECL_ARG, tok_begin, scope_get(ctx));
    arg->data.decl_arg.value = value;
    arg->data.decl.type      = type;
    arg->data.decl.name      = name;
    RETURN_ZONE(arg);
}

struct ast *parse_decl_variant(struct context *ctx, struct ast *prev)
{
    ZONE();
    struct token *tok_begin = tokens_peek(ctx->tokens);
    struct ast   *name      = parse_ident(ctx);
    if (!name) RETURN_ZONE(NULL);
    struct ast *variant =
        ast_create_node(ctx->ast_arena, AST_DECL_VARIANT, tok_begin, scope_get(ctx));
    variant->docs = pop_docs(ctx);

    struct token *tok_assign = tokens_consume_if(ctx->tokens, SYM_ASSIGN);
    if (tok_assign) {
        struct ast *expr = parse_expr(ctx);
        if (!expr) {
            struct token *tok_err = tokens_peek(ctx->tokens);
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_EXPECTED_NAME,
                        &tok_err->location,
                        BUILDER_CUR_AFTER,
                        "Expected enumerator variant value.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
        }
        variant->data.decl_variant.value = expr;
    }
    variant->data.decl.name = name;
    RETURN_ZONE(variant);
}

bool parse_semicolon(struct context *ctx)
{
    return tokens_consume_if(ctx->tokens, SYM_SEMICOLON);
}

bool parse_semicolon_rq(struct context *ctx)
{
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_SEMICOLON);
    if (!tok) {
        tok = tokens_peek_prev(ctx->tokens);
        PARSE_ERROR(ERR_MISSING_SEMICOLON, tok, BUILDER_CUR_AFTER, "Expected semicolon ';'.");
        CONSUME_TILL(ctx->tokens, SYM_IDENT, SYM_RBLOCK);
        return false;
    }
    return true;
}

bool hash_directive_to_flags(enum hash_directive_flags hd, u32 *out_flags)
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
        FLAG_CASE(HD_EXPORT, FLAG_EXPORT);
        FLAG_CASE(HD_THREAD_LOCAL, FLAG_THREAD_LOCAL);
        FLAG_CASE(HD_FLAGS, FLAG_FLAGS);
    default:
        break;
    }

    return false;
}

struct ast *parse_stmt_return(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_RETURN);
    if (!tok_begin) RETURN_ZONE(NULL);
    struct ast *ret = ast_create_node(ctx->ast_arena, AST_STMT_RETURN, tok_begin, scope_get(ctx));
    ret->data.stmt_return.fn_decl = decl_get(ctx);
    tok_begin                     = tokens_peek(ctx->tokens);

    struct ast *expr;
    bool        rq = false;
NEXT:
    expr = parse_expr(ctx);
    if (expr) {
        if (!ret->data.stmt_return.exprs)
            ret->data.stmt_return.exprs = create_sarr(TSmallArray_AstPtr, ctx->assembly);
        tsa_push_AstPtr(ret->data.stmt_return.exprs, expr);
        if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected expression after comma ','.");
        CONSUME_TILL(ctx->tokens, SYM_SEMICOLON, SYM_RBLOCK, SYM_IDENT);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }
    RETURN_ZONE(ret);
}

struct ast *parse_stmt_if(struct context *ctx, bool is_static)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_IF);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct ast *stmt_if = ast_create_node(ctx->ast_arena, AST_STMT_IF, tok_begin, scope_get(ctx));
    stmt_if->data.stmt_if.is_static = is_static;
    stmt_if->data.stmt_if.test      = parse_expr(ctx);
    if (!stmt_if->data.stmt_if.test) {
        struct token *tok_err = tokens_consume(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected expression for the if statement.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    if (AST_IS_OK(stmt_if->data.stmt_if.test)) {
        tokens_consume_till(ctx->tokens, SYM_LBLOCK);
    }

    stmt_if->data.stmt_if.true_stmt = parse_block(ctx, true);
    if (!stmt_if->data.stmt_if.true_stmt) {
        struct token *tok_err = tokens_consume(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_STMT,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected compound statement for true result of the if expression test.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    stmt_if->data.stmt_if.false_stmt = NULL;
    if (tokens_consume_if(ctx->tokens, SYM_ELSE)) {
        stmt_if->data.stmt_if.false_stmt = parse_stmt_if(ctx, is_static);
        if (!stmt_if->data.stmt_if.false_stmt)
            stmt_if->data.stmt_if.false_stmt = parse_block(ctx, true);
        if (!stmt_if->data.stmt_if.false_stmt) {
            struct token *tok_err = tokens_consume(ctx->tokens);
            PARSE_ERROR(ERR_EXPECTED_STMT,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected statement for false result of the if expression test.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
        }
    }

    RETURN_ZONE(stmt_if);
}

struct ast *parse_stmt_switch(struct context *ctx)
{
    ZONE();
    struct token *tok_switch = tokens_consume_if(ctx->tokens, SYM_SWITCH);
    if (!tok_switch) RETURN_ZONE(NULL);

    struct ast *expr = parse_expr(ctx);
    if (!expr) {
        struct token *tok_err = tokens_consume(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected expression for the switch statement.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    struct token *tok = tokens_consume_if(ctx->tokens, SYM_LBLOCK);
    if (!tok) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "Expected switch body block.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    TSmallArray_AstPtr *cases        = create_sarr(TSmallArray_AstPtr, ctx->assembly);
    struct ast         *stmt_case    = NULL;
    struct ast         *default_case = NULL;
NEXT:
    stmt_case = parse_stmt_case(ctx);
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
        if (tokens_current_is_not(ctx->tokens, SYM_RBLOCK)) goto NEXT;
    }

    tok = tokens_consume_if(ctx->tokens, SYM_RBLOCK);
    if (!tok) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "Expected end of switch body block.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    struct ast *stmt_switch =
        ast_create_node(ctx->ast_arena, AST_STMT_SWITCH, tok_switch, scope_get(ctx));

    stmt_switch->data.stmt_switch.expr  = expr;
    stmt_switch->data.stmt_switch.cases = cases;
    RETURN_ZONE(stmt_switch);
}

struct ast *parse_stmt_case(struct context *ctx)
{
    ZONE();
    TSmallArray_AstPtr *exprs = NULL;
    struct ast         *block = NULL;
    struct ast         *expr  = NULL;
    bool                rq    = false;

    if (tokens_current_is(ctx->tokens, SYM_RBLOCK)) RETURN_ZONE(NULL);

    struct token *tok_case = tokens_consume_if(ctx->tokens, SYM_DEFAULT);
    if (tok_case) goto SKIP_EXPRS;

    tok_case = tokens_peek(ctx->tokens);
    exprs    = create_sarr(TSmallArray_AstPtr, ctx->assembly);
NEXT:
    expr = parse_expr(ctx);
    if (expr) {
        tsa_push_AstPtr(exprs, expr);

        if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected expression after comma.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

SKIP_EXPRS:
    block = parse_block(ctx, true);
    if (!block && !parse_semicolon_rq(ctx)) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    } else {
        parse_semicolon(ctx);
    }

    struct ast *stmt_case =
        ast_create_node(ctx->ast_arena, AST_STMT_CASE, tok_case, scope_get(ctx));
    stmt_case->data.stmt_case.exprs      = exprs;
    stmt_case->data.stmt_case.is_default = !exprs;
    stmt_case->data.stmt_case.block      = block;

    RETURN_ZONE(stmt_case);
}

static enum tokens_lookahead_state cmp_stmt_loop(struct token *curr)
{
    if (token_is(curr, SYM_SEMICOLON))
        return TOK_LOOK_HIT;
    else if (token_is(curr, SYM_LBLOCK))
        return TOK_LOOK_TERMINAL;

    return TOK_LOOK_CONTINUE;
}

struct ast *parse_stmt_loop(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_LOOP);
    if (!tok_begin) RETURN_ZONE(NULL);

    // Loop statement is immediately followed by block; this should act like while (true) {} in C.
    const bool while_true = tokens_current_is(ctx->tokens, SYM_LBLOCK);

    struct ast *loop = ast_create_node(ctx->ast_arena, AST_STMT_LOOP, tok_begin, scope_get(ctx));
    const bool  prev_in_loop = ctx->is_inside_loop;
    ctx->is_inside_loop      = true;

    struct scope *scope =
        scope_create(ctx->scope_arenas, SCOPE_LEXICAL, scope_get(ctx), 128, &tok_begin->location);

    scope_push(ctx, scope);

    if (!while_true) {
        if (tokens_lookahead(ctx->tokens, cmp_stmt_loop)) {
            // for loop construct loop [init]; [condition]; [increment] {}
            loop->data.stmt_loop.init = parse_decl(ctx);
            if (!parse_semicolon_rq(ctx)) {
                BL_ASSERT(false);
            }

            loop->data.stmt_loop.condition = parse_expr(ctx);
            if (!parse_semicolon_rq(ctx)) {
                BL_ASSERT(false);
            }

            loop->data.stmt_loop.increment = parse_expr(ctx);
        } else {
            // while construct with optional condition
            loop->data.stmt_loop.condition = parse_expr(ctx);
        }
    }

    // block
    loop->data.stmt_loop.block = parse_block(ctx, false);
    if (!loop->data.stmt_loop.block) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY, tok_err, BUILDER_CUR_WORD, "Expected loop body block.");
        ctx->is_inside_loop = prev_in_loop;
        scope_pop(ctx);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    ctx->is_inside_loop = prev_in_loop;
    scope_pop(ctx);
    RETURN_ZONE(loop);
}

struct ast *parse_stmt_break(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_BREAK);
    if (!tok) RETURN_ZONE(NULL);

    if (!ctx->is_inside_loop) {
        PARSE_ERROR(
            ERR_BREAK_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD, "Break statement outside a loop.");
    }
    RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_STMT_BREAK, tok, scope_get(ctx)));
}

struct ast *parse_stmt_continue(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_CONTINUE);
    if (!tok) RETURN_ZONE(NULL);

    if (!ctx->is_inside_loop) {
        PARSE_ERROR(
            ERR_CONTINUE_OUTSIDE_LOOP, tok, BUILDER_CUR_WORD, "Continue statement outside a loop.");
    }

    RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_STMT_CONTINUE, tok, scope_get(ctx)));
}

struct ast *parse_stmt_defer(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_DEFER);
    if (!tok) RETURN_ZONE(NULL);

    struct ast *expr = NULL;
    expr             = parse_expr(ctx);

    if (!expr) {
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after 'defer' statement.");

        struct token *tok_err = tokens_peek(ctx->tokens);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }

    struct ast *defer = ast_create_node(ctx->ast_arena, AST_STMT_DEFER, tok, scope_get(ctx));
    defer->data.stmt_defer.expr = expr;

    RETURN_ZONE(defer);
}

struct ast *parse_expr(struct context *ctx)
{
    return _parse_expr(ctx, 0);
}

struct ast *_parse_expr(struct context *ctx, s32 p)
{
    ZONE();
    struct ast *lhs = parse_expr_atom(ctx);
    struct ast *tmp = NULL;
    do {
        tmp = parse_expr_call(ctx, lhs, false);
        if (!tmp) tmp = parse_expr_elem(ctx, lhs);
        if (!tmp) tmp = parse_ref_nested(ctx, lhs);
        lhs = tmp ? tmp : lhs;
    } while (tmp);

    while (token_is_binop(tokens_peek(ctx->tokens)) &&
           token_prec(tokens_peek(ctx->tokens)).priority >= p) {
        struct token *op = tokens_consume(ctx->tokens);
        const s32 q = token_prec(op).associativity == TOKEN_ASSOC_LEFT ? token_prec(op).priority + 1
                                                                       : token_prec(op).priority;
        struct ast *rhs = _parse_expr(ctx, q);
        if (!lhs || !rhs) {
            PARSE_ERROR(ERR_INVALID_EXPR, op, BUILDER_CUR_WORD, "Invalid binary operation.");
        }
        lhs = parse_expr_binary(ctx, lhs, rhs, op);
    }
    RETURN_ZONE(lhs);
}

struct ast *parse_expr_primary(struct context *ctx)
{
    struct ast *expr = NULL;
    switch (tokens_peek_sym(ctx->tokens)) {
    case SYM_LPAREN:
        expr = parse_expr_nested(ctx);
        break;
    case SYM_IDENT:
        expr = parse_expr_ref(ctx);
        break;
    case SYM_NULL:
        expr = parse_expr_null(ctx);
        break;
    case SYM_HASH:
        expr = parse_hash_directive(ctx, HD_FILE | HD_LINE | HD_COMPTIME, NULL);
        break;
    case SYM_FN:
        if ((expr = parse_expr_lit_fn(ctx))) break;
        expr = parse_expr_lit_fn_group(ctx);
        break;
    default:
        break;
    }
    if (expr) return expr;
    if ((expr = parse_expr_lit(ctx))) return expr;
    if ((expr = parse_expr_type(ctx))) return expr;
    if ((expr = parse_expr_compound(ctx))) return expr;
    return NULL;
}

struct ast *parse_expr_unary(struct context *ctx)
{
    ZONE();
    struct token *op = tokens_peek(ctx->tokens);
    if (!token_is_unary(op)) RETURN_ZONE(NULL);

    tokens_consume(ctx->tokens);
    struct ast *unary = ast_create_node(ctx->ast_arena, AST_EXPR_UNARY, op, scope_get(ctx));
    unary->data.expr_unary.next = _parse_expr(ctx, token_prec(op).priority);
    unary->data.expr_unary.kind = sym_to_unop_kind(op->sym);

    if (unary->data.expr_unary.next == NULL) {
        struct token *err_tok = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    err_tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after unary operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, op, scope_get(ctx)));
    }

    if (unary->data.expr_unary.next->kind == AST_BAD) RETURN_ZONE(unary->data.expr_unary.next);

    RETURN_ZONE(unary);
}

struct ast *parse_expr_atom(struct context *ctx)
{
    struct ast *expr = NULL;
    switch (tokens_peek_sym(ctx->tokens)) {
    case SYM_AT:
        expr = parse_expr_deref(ctx);
        break;
    case SYM_AND:
        expr = parse_expr_addrof(ctx);
        break;
    case SYM_CAST:
        expr = parse_expr_cast(ctx);
        break;
    case SYM_CAST_AUTO:
        expr = parse_expr_cast_auto(ctx);
        break;
    case SYM_SIZEOF:
        expr = parse_expr_sizeof(ctx);
        break;
    case SYM_ALIGNOF:
        expr = parse_expr_alignof(ctx);
        break;
    case SYM_TYPEINFO:
        expr = parse_expr_type_info(ctx);
        break;
    case SYM_TESTCASES:
        expr = parse_expr_test_cases(ctx);
        break;
    default:
        break;
    }
    if (expr) return expr;
    if ((expr = parse_expr_primary(ctx))) return expr;
    if ((expr = parse_expr_unary(ctx))) return expr;
    return NULL;
}

struct ast *
parse_expr_binary(struct context *ctx, struct ast *lhs, struct ast *rhs, struct token *op)
{
    ZONE();
    if (!token_is_binop(op)) RETURN_ZONE(NULL);

    struct ast *binop = ast_create_node(ctx->ast_arena, AST_EXPR_BINOP, op, scope_get(ctx));
    binop->data.expr_binop.kind = sym_to_binop_kind(op->sym);
    binop->data.expr_binop.lhs  = lhs;
    binop->data.expr_binop.rhs  = rhs;
    RETURN_ZONE(binop);
}

struct ast *parse_expr_addrof(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_AND);
    if (!tok) RETURN_ZONE(NULL);

    struct ast *addrof = ast_create_node(ctx->ast_arena, AST_EXPR_ADDROF, tok, scope_get(ctx));
    addrof->data.expr_addrof.next = _parse_expr(ctx, token_prec(tok).priority);

    if (addrof->data.expr_addrof.next == NULL) {
        struct token *err_tok = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    err_tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after '&' operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    if (addrof->data.expr_addrof.next->kind == AST_BAD) RETURN_ZONE(addrof->data.expr_addrof.next);
    RETURN_ZONE(addrof);
}

struct ast *parse_expr_deref(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_AT);
    if (!tok) RETURN_ZONE(NULL);

    struct ast *deref = ast_create_node(ctx->ast_arena, AST_EXPR_DEREF, tok, scope_get(ctx));
    deref->data.expr_deref.next = _parse_expr(ctx, token_prec(tok).priority);

    if (deref->data.expr_deref.next == NULL) {
        struct token *err_tok = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_EXPR,
                    err_tok,
                    BUILDER_CUR_WORD,
                    "Expected expression after '@' pointer dereference operator.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    if (deref->data.expr_deref.next->kind == AST_BAD) RETURN_ZONE(deref->data.expr_deref.next);
    RETURN_ZONE(deref);
}

struct ast *parse_expr_lit(struct context *ctx)
{
    ZONE();
    struct token *tok = tokens_peek(ctx->tokens);
    struct ast   *lit = NULL;

    switch (tok->sym) {
    case SYM_NUM:
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_INT, tok, scope_get(ctx));
        lit->data.expr_integer.val      = tok->value.u;
        lit->data.expr_integer.overflow = tok->overflow;
        break;

    case SYM_CHAR:
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_CHAR, tok, scope_get(ctx));
        lit->data.expr_character.val = (u8)tok->value.c;

        break;

    case SYM_TRUE:
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_BOOL, tok, scope_get(ctx));
        lit->data.expr_boolean.val = true;
        break;

    case SYM_FALSE:
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_BOOL, tok, scope_get(ctx));
        lit->data.expr_boolean.val = false;
        break;

    case SYM_DOUBLE:
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_DOUBLE, tok, scope_get(ctx));
        lit->data.expr_double.val      = tok->value.d;
        lit->data.expr_double.overflow = tok->overflow;
        break;

    case SYM_FLOAT:
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_FLOAT, tok, scope_get(ctx));
        lit->data.expr_float.val      = (f32)tok->value.d;
        lit->data.expr_float.overflow = tok->overflow;
        break;

    case SYM_STRING: {
        // There is special case for string literals, those can be split into multiple lines and we
        // should handle such situation here, so some pre-scan is needed.
        lit = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_STRING, tok, scope_get(ctx));
        const char   *str      = tok->value.str;
        struct token *tok_next = tokens_peek_2nd(ctx->tokens);
        if (tok_next->sym == SYM_STRING) {
            TString *tmp = builder_create_cached_str();
            while ((tok = tokens_consume_if(ctx->tokens, SYM_STRING))) {
                BL_ASSERT(tok->value.str);
                tstring_append(tmp, tok->value.str);
            }
            str = tmp->data;
        } else {
            tokens_consume(ctx->tokens);
        }
        BL_ASSERT(str);
        lit->data.expr_string.val = str;
        // Directly return, all tokens were consumed.
        RETURN_ZONE(lit);
    }

    default:
        RETURN_ZONE(NULL);
    }

    tokens_consume(ctx->tokens);
    RETURN_ZONE(lit);
}

struct ast *parse_expr_lit_fn(struct context *ctx)
{
    ZONE();
    if (!tokens_is_seq(ctx->tokens, 2, SYM_FN, SYM_LPAREN)) RETURN_ZONE(NULL);
    struct token *tok_fn = tokens_peek(ctx->tokens);
    struct ast   *fn     = ast_create_node(ctx->ast_arena, AST_EXPR_LIT_FN, tok_fn, scope_get(ctx));

    struct scope   *parent_scope = scope_get(ctx);
    enum scope_kind scope_kind =
        (parent_scope->kind == SCOPE_GLOBAL || parent_scope->kind == SCOPE_PRIVATE)
            ? SCOPE_FN
            : SCOPE_FN_LOCAL;
    struct scope *scope =
        scope_create(ctx->scope_arenas, scope_kind, scope_get(ctx), 256, &tok_fn->location);

    scope_push(ctx, scope);

    struct ast *type = parse_type_fn(ctx, true);
    BL_ASSERT(type);
    fn->data.expr_fn.type = type;
    // parse flags
    struct ast *curr_decl = decl_get(ctx);
    if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
        u32 accepted = HD_EXTERN | HD_NO_INLINE | HD_INLINE | HD_COMPILER | HD_ENTRY |
                       HD_BUILD_ENTRY | HD_INTRINSIC | HD_TEST_FN | HD_EXPORT;
        u32 flags = 0;
        while (true) {
            enum hash_directive_flags found        = HD_NONE;
            struct ast               *hd_extension = parse_hash_directive(ctx, accepted, &found);
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
    fn->data.expr_fn.block = parse_block(ctx, false);

    scope_pop(ctx);
    RETURN_ZONE(fn);
}

struct ast *parse_expr_lit_fn_group(struct context *ctx)
{
    ZONE();
    if (!tokens_is_seq(ctx->tokens, 2, SYM_FN, SYM_LBLOCK)) RETURN_ZONE(NULL);
    struct token *tok_group = tokens_consume(ctx->tokens); // eat fn
    struct token *tok_begin = tokens_consume(ctx->tokens); // eat {
    struct ast   *group =
        ast_create_node(ctx->ast_arena, AST_EXPR_LIT_FN_GROUP, tok_group, scope_get(ctx));

    TSmallArray_AstPtr *variants       = create_sarr(TSmallArray_AstPtr, ctx->assembly);
    group->data.expr_fn_group.variants = variants;
    struct ast *tmp;
NEXT:
    if ((tmp = parse_expr(ctx))) {
        tsa_push_AstPtr(variants, tmp);
        parse_semicolon_rq(ctx);
        goto NEXT;
    }
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_RBLOCK);
    if (!tok) {
        tok = tokens_peek_prev(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }
    RETURN_ZONE(group);
}

struct ast *parse_expr_nested(struct context *ctx)
{
    ZONE();
    struct ast   *expr      = NULL;
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_LPAREN);
    if (!tok_begin) RETURN_ZONE(NULL);

    expr = parse_expr(ctx);
    if (expr == NULL) {
        PARSE_ERROR(ERR_EXPECTED_EXPR, tok_begin, BUILDER_CUR_WORD, "Expected expression.");
    }

    // eat )
    struct token *tok_end = tokens_consume_if(ctx->tokens, SYM_RPAREN);
    if (!tok_end) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Unterminated sub-expression, missing ')'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "starting here");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    RETURN_ZONE(expr);
}

struct ast *parse_expr_elem(struct context *ctx, struct ast *prev)
{
    ZONE();
    if (!prev) RETURN_ZONE(NULL);
    struct token *tok_elem = tokens_consume_if(ctx->tokens, SYM_LBRACKET);
    if (!tok_elem) RETURN_ZONE(NULL);

    struct ast *elem = ast_create_node(ctx->ast_arena, AST_EXPR_ELEM, tok_elem, scope_get(ctx));
    elem->data.expr_elem.index = parse_expr(ctx);
    elem->data.expr_elem.next  = prev;

    if (!elem->data.expr_elem.index) {
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok_elem, BUILDER_CUR_WORD, "Expected array index expression.");
    }

    struct token *tok = tokens_consume(ctx->tokens);
    if (tok->sym != SYM_RBRACKET) {
        PARSE_ERROR(ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Missing bracket ']'.");
    }

    RETURN_ZONE(elem);
}

INLINE struct ast *parse_ident(struct context *ctx)
{
    ZONE();
    struct token *tok_ident = tokens_consume_if(ctx->tokens, SYM_IDENT);
    if (!tok_ident) RETURN_ZONE(NULL);
    struct ast *ident = ast_create_node(ctx->ast_arena, AST_IDENT, tok_ident, scope_get(ctx));
    id_init(&ident->data.ident.id, tok_ident->value.str);
    RETURN_ZONE(ident);
}

struct ast *parse_ident_group(struct context *ctx)
{
    ZONE();
    struct ast *root = NULL;
    struct ast *prev = NULL;
    bool        rq   = false;
    struct ast *ident;
NEXT:
    ident = parse_ident(ctx);
    if (ident) {
        if (prev) prev->data.ident.next = ident;
        if (!root) root = ident;
        prev = ident;
        if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected name after comma ','.");
        CONSUME_TILL(ctx->tokens, SYM_COLON, SYM_SEMICOLON, SYM_IDENT);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }
    RETURN_ZONE(root);
}

struct ast *parse_type_ptr(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_ASTERISK);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct ast *ptr      = ast_create_node(ctx->ast_arena, AST_TYPE_PTR, tok_begin, scope_get(ctx));
    struct ast *sub_type = parse_type(ctx);
    if (!sub_type) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_TYPE,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected type after '*' pointer type declaration.");
        CONSUME_TILL(ctx->tokens, SYM_COLON, SYM_SEMICOLON, SYM_IDENT);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_err, scope_get(ctx)));
    }
    ptr->data.type_ptr.type = sub_type;
    RETURN_ZONE(ptr);
}

struct ast *parse_type_vargs(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_VARGS);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct ast *ptr = ast_create_node(ctx->ast_arena, AST_TYPE_VARGS, tok_begin, scope_get(ctx));
    ptr->data.type_ptr.type = parse_type(ctx);
    RETURN_ZONE(ptr);
}

struct ast *parse_type_enum(struct context *ctx)
{
    ZONE();
    struct token *tok_enum = tokens_consume_if(ctx->tokens, SYM_ENUM);
    if (!tok_enum) RETURN_ZONE(NULL);

    struct ast *enm = ast_create_node(ctx->ast_arena, AST_TYPE_ENUM, tok_enum, scope_get(ctx));
    enm->data.type_enm.variants = create_sarr(TSmallArray_AstPtr, ctx->assembly);
    enm->data.type_enm.type     = parse_type(ctx);

    // parse flags
    {
        u32 accepted = HD_COMPILER | HD_FLAGS;
        u32 flags    = 0;
        while (true) {
            enum hash_directive_flags found = HD_NONE;
            parse_hash_directive(ctx, accepted, &found);
            if (!hash_directive_to_flags(found, &flags)) break;
            accepted &= ~found;
        }
        struct ast *curr_decl = decl_get(ctx);
        if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
            curr_decl->data.decl_entity.flags |= flags;
        }
        enm->data.type_enm.is_flags = IS_FLAG(flags, FLAG_FLAGS);
    }

    struct token *tok = tokens_consume(ctx->tokens);
    if (token_is_not(tok, SYM_LBLOCK)) {
        PARSE_ERROR(ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected enum variant list.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    struct scope *scope =
        scope_create(ctx->scope_arenas, SCOPE_TYPE_ENUM, scope_get(ctx), 512, &tok->location);
    enm->data.type_enm.scope = scope;
    scope_push(ctx, scope);

    // parse enum varinats
    bool        rq = false;
    struct ast *tmp;
    struct ast *prev_tmp = NULL;

NEXT:
    if (parse_docs(ctx)) goto NEXT;
    tmp = parse_decl_variant(ctx, prev_tmp);
    if (tmp) {
        prev_tmp = tmp;
        tsa_push_AstPtr(enm->data.type_enm.variants, tmp);

        if (tokens_consume_if(ctx->tokens, SYM_SEMICOLON)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        if (tokens_peek_2nd(ctx->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(
                ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected variant after semicolon.");
            scope_pop(ctx);
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
        }
    }

    tok = tokens_consume(ctx->tokens);
    if (tok->sym != SYM_RBLOCK) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of variant list '}' or another variant separated by semicolon.");
        scope_pop(ctx);
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    scope_pop(ctx);
    RETURN_ZONE(enm);
}

struct ast *parse_ref(struct context *ctx)
{
    ZONE();
    struct token *tok   = tokens_peek(ctx->tokens);
    struct ast   *ident = parse_ident(ctx);
    if (!ident) RETURN_ZONE(NULL);
    struct ast *lhs     = ast_create_node(ctx->ast_arena, AST_REF, tok, scope_get(ctx));
    lhs->data.ref.ident = ident;
    struct ast *tmp     = NULL;
    do {
        tmp = parse_ref_nested(ctx, lhs);
        lhs = tmp ? tmp : lhs;
    } while (tmp);
    RETURN_ZONE(lhs);
}

struct ast *parse_ref_nested(struct context *ctx, struct ast *prev)
{
    ZONE();
    if (!prev) RETURN_ZONE(NULL);
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_DOT);
    if (!tok) RETURN_ZONE(NULL);

    struct ast *ident = parse_ident(ctx);
    if (!ident) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected name.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    struct ast *ref     = ast_create_node(ctx->ast_arena, AST_REF, tok, scope_get(ctx));
    ref->data.ref.ident = ident;
    ref->data.ref.next  = prev;
    RETURN_ZONE(ref);
}

static INLINE void set_polymorph(struct context *ctx)
{
    for (usize i = arrlenu(ctx->fn_type_stack); i-- > 0;) {
        struct ast *fn_type = ctx->fn_type_stack[i];
        BL_ASSERT(fn_type && fn_type->kind == AST_TYPE_FN);
        if (fn_type->data.type_fn.is_polymorph) return;
        fn_type->data.type_fn.is_polymorph = true;
    }
}

struct ast *parse_type_polymorph(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_QUESTION);
    if (!tok_begin) RETURN_ZONE(NULL);

    if (!arrlenu(ctx->fn_type_stack)) {
        PARSE_ERROR(ERR_INVALID_ARG_TYPE,
                    tok_begin,
                    BUILDER_CUR_WORD,
                    "Polymorph type can be specified only in function argument list.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }
    set_polymorph(ctx);
    struct ast *ident = parse_ident(ctx);
    if (!ident) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected name of polymorph type.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }
    struct ast *poly = ast_create_node(ctx->ast_arena, AST_TYPE_POLY, tok_begin, scope_get(ctx));
    poly->data.type_poly.ident = ident;
    RETURN_ZONE(poly);
}

struct ast *parse_type_arr(struct context *ctx)
{
    ZONE();
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_LBRACKET);
    if (!tok_begin) RETURN_ZONE(NULL);

    struct ast *arr = ast_create_node(ctx->ast_arena, AST_TYPE_ARR, tok_begin, scope_get(ctx));
    arr->data.type_arr.len = parse_expr(ctx);
    if (!arr->data.type_arr.len) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(
            ERR_EXPECTED_EXPR, tok_err, BUILDER_CUR_WORD, "Expected array size expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct token *tok_end = tokens_consume_if(ctx->tokens, SYM_RBRACKET);
    if (!tok_end) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected closing ']' after array size expression.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);

        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    arr->data.type_arr.elem_type = parse_type(ctx);
    if (!arr->data.type_arr.elem_type) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_INVALID_TYPE, tok_err, BUILDER_CUR_WORD, "Expected array element type.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    RETURN_ZONE(arr);
}

struct ast *parse_type_slice(struct context *ctx)
{
    ZONE();
    if (tokens_peek(ctx->tokens)->sym != SYM_LBRACKET) RETURN_ZONE(NULL);
    if (tokens_peek_2nd(ctx->tokens)->sym != SYM_RBRACKET) RETURN_ZONE(NULL);

    // eat []
    struct token *tok_begin = tokens_consume(ctx->tokens);
    tok_begin               = tokens_consume(ctx->tokens);

    struct ast *slice = ast_create_node(ctx->ast_arena, AST_TYPE_SLICE, tok_begin, scope_get(ctx));

    slice->data.type_slice.elem_type = parse_type(ctx);

    if (!slice->data.type_slice.elem_type) {
        PARSE_ERROR(ERR_INVALID_TYPE, tok_begin, BUILDER_CUR_AFTER, "Expected slice element type.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    RETURN_ZONE(slice);
}

struct ast *parse_type_dynarr(struct context *ctx)
{
    ZONE();
    if (tokens_peek(ctx->tokens)->sym != SYM_LBRACKET) RETURN_ZONE(NULL);
    if (tokens_peek_2nd(ctx->tokens)->sym != SYM_DYNARR) RETURN_ZONE(NULL);

    // eat [..
    struct token *tok_begin = tokens_consume(ctx->tokens);
    tokens_consume(ctx->tokens);

    struct token *tok_end = tokens_consume_if(ctx->tokens, SYM_RBRACKET);
    if (!tok_end) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok_err,
                    BUILDER_CUR_WORD,
                    "Expected closing ']' after dynamic array signature.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);

        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    struct ast *slice = ast_create_node(ctx->ast_arena, AST_TYPE_DYNARR, tok_begin, scope_get(ctx));
    slice->data.type_dynarr.elem_type = parse_type(ctx);

    if (!slice->data.type_dynarr.elem_type) {
        PARSE_ERROR(
            ERR_INVALID_TYPE, tok_end, BUILDER_CUR_AFTER, "Expected dynami array element type.");

        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }

    RETURN_ZONE(slice);
}

struct ast *parse_type(struct context *ctx)
{
    struct ast *type = NULL;

    type = parse_type_ptr(ctx);
    // keep order
    if (!type) type = parse_type_fn_group(ctx);
    if (!type) type = parse_type_fn(ctx, false);
    // keep order

    if (!type) type = parse_type_polymorph(ctx);
    if (!type) type = parse_type_struct(ctx);
    if (!type) type = parse_type_enum(ctx);
    if (!type) type = parse_type_vargs(ctx);

    // Keep order!!!
    if (!type) type = parse_type_slice(ctx);
    if (!type) type = parse_type_dynarr(ctx);
    if (!type) type = parse_type_arr(ctx);
    // Keep order!!!

    if (!type) type = parse_ref(ctx);

    return type;
}

struct ast *parse_type_fn_return(struct context *ctx)
{
    ZONE();
    if (tokens_current_is(ctx->tokens, SYM_LPAREN)) {
        // multiple return type ( T1, T2 )
        // eat (
        struct token *tok_begin = tokens_consume(ctx->tokens);
        struct scope *scope     = scope_create(
            ctx->scope_arenas, SCOPE_TYPE_STRUCT, scope_get(ctx), 16, &tok_begin->location);
        scope_push(ctx, scope);

        struct ast *type_struct =
            ast_create_node(ctx->ast_arena, AST_TYPE_STRUCT, tok_begin, scope_get(ctx));
        type_struct->data.type_strct.scope   = scope;
        type_struct->data.type_strct.members = create_sarr(TSmallArray_AstPtr, ctx->assembly);
        type_struct->data.type_strct.is_multiple_return_type = true;
        struct ast *tmp;
        s32         index = 0;
    NEXT:
        tmp = parse_decl_member(ctx, index++);
        if (tmp) {
            if (AST_IS_BAD(tmp)) CONSUME_TILL(ctx->tokens, SYM_COMMA, SYM_RPAREN);
            tsa_push_AstPtr(type_struct->data.type_strct.members, tmp);
            if (tokens_consume_if(ctx->tokens, SYM_COMMA)) goto NEXT;
        }
        struct token *tok = tokens_consume_if(ctx->tokens, SYM_RPAREN);
        if (!tok) {
            PARSE_ERROR(ERR_MISSING_BRACKET,
                        tokens_peek(ctx->tokens),
                        BUILDER_CUR_WORD,
                        "Expected end of return list or another return type separated by comma.");
            scope_pop(ctx);
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
        }
        scope_pop(ctx);
        if (!type_struct->data.type_strct.members->size) {
            PARSE_ERROR(ERR_INVALID_TYPE,
                        tok_begin,
                        BUILDER_CUR_WORD,
                        "Expected at least one return type inside parenthesis, if function should "
                        "return 'void' remove parenthesis and leave return type unspecified.");
        }
        RETURN_ZONE(type_struct);
    }
    RETURN_ZONE(parse_type(ctx));
}

struct ast *parse_type_fn(struct context *ctx, bool named_args)
{
    ZONE();
    struct token *tok_fn = tokens_consume_if(ctx->tokens, SYM_FN);
    if (!tok_fn) RETURN_ZONE(NULL);

    struct token *tok = tokens_consume(ctx->tokens);
    if (tok->sym != SYM_LPAREN) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok, BUILDER_CUR_WORD, "Expected function parameter list.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_fn, scope_get(ctx)));
    }
    struct ast *fn = ast_create_node(ctx->ast_arena, AST_TYPE_FN, tok_fn, scope_get(ctx));
    // parse arg types
    bool        rq = false;
    struct ast *tmp;
    arrput(ctx->fn_type_stack, fn);
NEXT:
    tmp = parse_decl_arg(ctx, named_args);
    if (tmp) {
        if (!fn->data.type_fn.args)
            fn->data.type_fn.args = create_sarr(TSmallArray_AstPtr, ctx->assembly);
        tsa_push_AstPtr(fn->data.type_fn.args, tmp);
        if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
            rq = true;
            goto NEXT;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        if (tokens_peek_2nd(ctx->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(
                ERR_EXPECTED_NAME, tok_err, BUILDER_CUR_WORD, "Expected type after comma ','.");
            arrpop(ctx->fn_type_stack);
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_fn, scope_get(ctx)));
        }
    }

    tok = tokens_consume(ctx->tokens);
    if (tok->sym != SYM_RPAREN) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of argument type list ')' or another argument separated "
                    "by comma.");
        arrpop(ctx->fn_type_stack);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_fn, scope_get(ctx)));
    }
    fn->data.type_fn.ret_type = parse_type_fn_return((ctx));
    arrpop(ctx->fn_type_stack);
    RETURN_ZONE(fn);
}

struct ast *parse_type_fn_group(struct context *ctx)
{
    ZONE();
    if (!tokens_is_seq(ctx->tokens, 2, SYM_FN, SYM_LBLOCK)) RETURN_ZONE(NULL);
    struct token *tok_group = tokens_consume(ctx->tokens); // eat fn
    struct token *tok_begin = tokens_consume(ctx->tokens); // eat {
    struct ast   *group =
        ast_create_node(ctx->ast_arena, AST_TYPE_FN_GROUP, tok_group, scope_get(ctx));

    TSmallArray_AstPtr *variants       = create_sarr(TSmallArray_AstPtr, ctx->assembly);
    group->data.type_fn_group.variants = variants;
    struct ast *tmp;
NEXT:
    if (parse_semicolon(ctx)) goto NEXT;
    if ((tmp = parse_type(ctx))) {
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

    struct token *tok = tokens_consume_if(ctx->tokens, SYM_RBLOCK);
    if (!tok) {
        tok = tokens_peek_prev(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
    }
    RETURN_ZONE(group);
}

struct ast *parse_type_struct(struct context *ctx)
{
    ZONE();
    struct token *tok_struct = tokens_consume_if(ctx->tokens, SYM_STRUCT);
    if (!tok_struct) tok_struct = tokens_consume_if(ctx->tokens, SYM_UNION);
    if (!tok_struct) RETURN_ZONE(NULL);

    const bool is_union = tok_struct->sym == SYM_UNION;

    // parse flags
    u32         accepted  = is_union ? 0 : HD_COMPILER | HD_BASE;
    u32         flags     = 0;
    struct ast *base_type = NULL;
    while (true) {
        struct ast               *hd_extension;
        enum hash_directive_flags found = HD_NONE;
        hd_extension                    = parse_hash_directive(ctx, accepted, &found);
        if (found == HD_BASE) {
            BL_ASSERT(hd_extension);
            base_type = hd_extension;
        } else if (!hash_directive_to_flags(found, &flags)) {
            break;
        }
        accepted &= ~found;
    }

    struct ast *curr_decl = decl_get(ctx);
    if (curr_decl && curr_decl->kind == AST_DECL_ENTITY) {
        curr_decl->data.decl_entity.flags |= flags;
    }

    struct token *tok = tokens_consume_if(ctx->tokens, SYM_LBLOCK);
    if (!tok) {
        PARSE_ERROR(
            ERR_MISSING_BRACKET, tok_struct, BUILDER_CUR_AFTER, "Expected struct member list.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_struct, scope_get(ctx)));
    }

    struct scope *scope =
        scope_create(ctx->scope_arenas, SCOPE_TYPE_STRUCT, scope_get(ctx), 128, &tok->location);
    scope_push(ctx, scope);

    struct ast *type_struct =
        ast_create_node(ctx->ast_arena, AST_TYPE_STRUCT, tok_struct, scope_get(ctx));
    type_struct->data.type_strct.scope     = scope;
    type_struct->data.type_strct.members   = create_sarr(TSmallArray_AstPtr, ctx->assembly);
    type_struct->data.type_strct.base_type = base_type;
    type_struct->data.type_strct.is_union  = is_union;

    // parse members
    struct ast *tmp;
    s32         index = 0;
NEXT:
    if (parse_docs(ctx)) goto NEXT;
    tmp = parse_decl_member(ctx, index++);
    if (tmp) {
        if (AST_IS_BAD(tmp)) CONSUME_TILL(ctx->tokens, SYM_SEMICOLON, SYM_RBLOCK);
        tsa_push_AstPtr(type_struct->data.type_strct.members, tmp);
        if (tokens_consume_if(ctx->tokens, SYM_SEMICOLON)) goto NEXT;
    }

    tok = tokens_consume_if(ctx->tokens, SYM_RBLOCK);
    if (!tok) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tokens_peek(ctx->tokens),
                    BUILDER_CUR_WORD,
                    "Expected end of member list '}' or another memeber separated by semicolon.");
        tokens_consume_till(ctx->tokens, SYM_SEMICOLON);
        scope_pop(ctx);
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_struct, scope_get(ctx)));
    }

    scope_pop(ctx);
    RETURN_ZONE(type_struct);
}

static enum tokens_lookahead_state cmp_decl(struct token *curr)
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

struct ast *parse_decl(struct context *ctx)
{
    ZONE();
    // is value declaration?
    if (!tokens_lookahead(ctx->tokens, cmp_decl)) RETURN_ZONE(NULL);
    struct token *tok_begin = tokens_peek(ctx->tokens);
    struct ast   *ident     = parse_ident_group(ctx);
    if (!ident) RETURN_ZONE(NULL);
    // eat :
    tokens_consume(ctx->tokens);

    struct ast *decl = ast_create_node(ctx->ast_arena, AST_DECL_ENTITY, tok_begin, scope_get(ctx));
    decl->docs       = pop_docs(ctx);
    decl->data.decl.name       = ident;
    decl->data.decl_entity.mut = true;

    decl_push(ctx, decl);

    decl->data.decl.type     = parse_type(ctx);
    struct token *tok_assign = tokens_consume_if(ctx->tokens, SYM_ASSIGN);
    if (!tok_assign) tok_assign = tokens_consume_if(ctx->tokens, SYM_COLON);

    // Parse hash directives.
    s32 hd_accepted = HD_COMPILER | HD_THREAD_LOCAL;

    if (tok_assign) {
        decl->data.decl_entity.mut = token_is(tok_assign, SYM_ASSIGN);

        // parse declaration expression
        decl->data.decl_entity.value = parse_expr(ctx);

        if (IS_NOT_FLAG(decl->data.decl_entity.flags, FLAG_EXTERN)) {
            if (!decl->data.decl_entity.value) {
                PARSE_ERROR(ERR_EXPECTED_INITIALIZATION,
                            tok_assign,
                            BUILDER_CUR_AFTER,
                            "Expected binding of declaration to some value.");
                decl_pop(ctx);
                RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx)));
            }
        }
    } else {
        hd_accepted |= HD_NO_INIT;
    }

    struct ast *init_value = decl->data.decl_entity.value;

    if (!init_value || rq_semicolon_after_decl_entity(init_value)) {
        u32 flags = 0;
        while (true) {
            enum hash_directive_flags found = HD_NONE;
            parse_hash_directive(ctx, hd_accepted, &found);
            if (!hash_directive_to_flags(found, &flags)) break;
            hd_accepted &= ~found;
        }

        if (IS_FLAG(flags, FLAG_NO_INIT) && !scope_is_local(scope_get(ctx))) {
            PARSE_ERROR(ERR_EXPECTED_INITIALIZATION,
                        tok_begin,
                        BUILDER_CUR_AFTER,
                        "Invalid 'noinit' directive for global variable '%s'. All globals must "
                        "be initialized either by explicit value or implicit default value.",
                        ident->data.ident.id.str);
        }

        decl->data.decl_entity.flags |= flags;
    }

    decl_pop(ctx);
    RETURN_ZONE(decl);
}

struct ast *parse_expr_call(struct context *ctx, struct ast *prev, const bool is_comptime)
{
    ZONE();
    if (!prev) RETURN_ZONE(NULL);

    struct token *location_token = tokens_peek_prev(ctx->tokens);
    struct token *tok            = tokens_consume_if(ctx->tokens, SYM_LPAREN);
    if (!tok) RETURN_ZONE(NULL);
    if (location_token && location_token->sym != SYM_IDENT) location_token = tok;
    struct ast *call =
        ast_create_node(ctx->ast_arena, AST_EXPR_CALL, location_token, scope_get(ctx));
    call->data.expr_call.ref                  = prev;
    call->data.expr_call.call_in_compile_time = is_comptime;
    // parse args
    bool        rq = false;
    struct ast *tmp;
arg:
    tmp = parse_expr(ctx);
    if (tmp) {
        if (!call->data.expr_call.args)
            call->data.expr_call.args = create_sarr(TSmallArray_AstPtr, ctx->assembly);
        tsa_push_AstPtr(call->data.expr_call.args, tmp);

        if (tokens_consume_if(ctx->tokens, SYM_COMMA)) {
            rq = true;
            goto arg;
        }
    } else if (rq) {
        struct token *tok_err = tokens_peek(ctx->tokens);
        if (tokens_peek_2nd(ctx->tokens)->sym == SYM_RBLOCK) {
            PARSE_ERROR(ERR_EXPECTED_NAME,
                        tok_err,
                        BUILDER_CUR_WORD,
                        "Expected function argument after comma ','.");
            RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
        }
    }

    tok = tokens_consume(ctx->tokens);
    if (tok->sym != SYM_RPAREN) {
        PARSE_ERROR(ERR_MISSING_BRACKET,
                    tok,
                    BUILDER_CUR_WORD,
                    "Expected end of parameter list ')' or another parameter separated by comma.");
        RETURN_ZONE(ast_create_node(ctx->ast_arena, AST_BAD, tok, scope_get(ctx)));
    }

    RETURN_ZONE(call);
}

struct ast *parse_expr_null(struct context *ctx)
{
    struct token *tok_null = tokens_consume_if(ctx->tokens, SYM_NULL);
    if (!tok_null) return NULL;
    return ast_create_node(ctx->ast_arena, AST_EXPR_NULL, tok_null, scope_get(ctx));
}

struct ast *parse_unrecheable(struct context *ctx)
{
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_UNREACHABLE);
    if (!tok) return NULL;

    return ast_create_node(ctx->ast_arena, AST_UNREACHABLE, tok, scope_get(ctx));
}

struct ast *parse_debugbreak(struct context *ctx)
{
    struct token *tok = tokens_consume_if(ctx->tokens, SYM_DEBUGBREAK);
    if (!tok) return NULL;
    return ast_create_node(ctx->ast_arena, AST_DEBUGBREAK, tok, scope_get(ctx));
}

struct ast *parse_expr_type(struct context *ctx)
{
    struct token *tok  = tokens_peek(ctx->tokens);
    struct ast   *type = NULL;

    type = parse_type_struct(ctx);

    // keep order
    if (!type) type = parse_type_slice(ctx);
    if (!type) type = parse_type_dynarr(ctx);
    if (!type) type = parse_type_arr(ctx);
    // keep order

    if (!type) type = parse_type_enum(ctx);
    if (!type) type = parse_type_ptr(ctx);

    if (type) {
        struct ast *expr = ast_create_node(ctx->ast_arena, AST_EXPR_TYPE, tok, scope_get(ctx));
        expr->data.expr_type.type = type;
        return expr;
    }

    return NULL;
}

struct ast *parse_block(struct context *ctx, bool create_scope)
{
    struct token *tok_begin = tokens_consume_if(ctx->tokens, SYM_LBLOCK);
    if (!tok_begin) return NULL;

    if (create_scope) {
        struct scope *scope = scope_create(
            ctx->scope_arenas, SCOPE_LEXICAL, scope_get(ctx), 1024, &tok_begin->location);

        scope_push(ctx, scope);
    }
    struct ast   *block = ast_create_node(ctx->ast_arena, AST_BLOCK, tok_begin, scope_get(ctx));
    struct token *tok;
    struct ast   *tmp       = NULL;
    block->data.block.nodes = create_sarr(TSmallArray_AstPtr, ctx->assembly);

NEXT:
    switch (tokens_peek_sym(ctx->tokens)) {
    case SYM_SEMICOLON:
        tok = tokens_consume(ctx->tokens);
        PARSE_WARNING(tok, BUILDER_CUR_WORD, "Extra semicolon can be removed ';'.");
        goto NEXT;
    case SYM_HASH: {
        enum hash_directive_flags satisfied;
        tmp = parse_hash_directive(
            ctx, HD_STATIC_IF | HD_ASSERT | HD_ERROR | HD_WARNING | HD_COMPTIME, &satisfied);
        // This is little bit ugly but probably faster than parsing expressions first.
        if (AST_IS_OK(tmp) && satisfied == HD_COMPTIME) parse_semicolon_rq(ctx);
        break;
    }
    case SYM_RETURN:
        tmp = parse_stmt_return(ctx);
        if (!AST_IS_BAD(tmp)) parse_semicolon_rq(ctx);
        block->data.block.has_return      = true;
        tmp->data.stmt_return.owner_block = block;
        break;
    case SYM_IF:
        tmp = parse_stmt_if(ctx, false);
        break;
    case SYM_SWITCH:
        tmp = parse_stmt_switch(ctx);
        break;
    case SYM_LOOP:
        tmp = parse_stmt_loop(ctx);
        break;
    case SYM_BREAK:
        tmp = parse_stmt_break(ctx);
        if (AST_IS_OK(tmp)) parse_semicolon_rq(ctx);

        break;
    case SYM_CONTINUE:
        tmp = parse_stmt_continue(ctx);
        if (AST_IS_OK(tmp)) parse_semicolon_rq(ctx);
        break;
    case SYM_DEFER:
        tmp = parse_stmt_defer(ctx);
        if (AST_IS_OK(tmp)) parse_semicolon_rq(ctx);
        break;
    case SYM_LBLOCK:
        tmp = parse_block(ctx, true);
        break;
    case SYM_UNREACHABLE:
        tmp = parse_unrecheable(ctx);
        parse_semicolon_rq(ctx);
        break;
    case SYM_DEBUGBREAK:
        tmp = parse_debugbreak(ctx);
        parse_semicolon_rq(ctx);
        break;
    default:
        tmp = NULL;
        break;
    }

    // Others
    if (tmp) {
        tsa_push_AstPtr(block->data.block.nodes, tmp);
        goto NEXT;
    }

    if ((tmp = (struct ast *)parse_decl(ctx))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(ctx);
        tsa_push_AstPtr(block->data.block.nodes, tmp);
        goto NEXT;
    }
    if ((tmp = parse_expr(ctx))) {
        if (AST_IS_OK(tmp)) parse_semicolon_rq(ctx);
        tsa_push_AstPtr(block->data.block.nodes, tmp);
        goto NEXT;
    }

    tok = tokens_consume_if(ctx->tokens, SYM_RBLOCK);
    if (!tok) {
        tok = tokens_peek_prev(ctx->tokens);
        PARSE_ERROR(ERR_EXPECTED_BODY_END, tok, BUILDER_CUR_AFTER, "Expected end of block '}'.");
        PARSE_NOTE(tok_begin, BUILDER_CUR_WORD, "Block starting here.");
        if (create_scope) scope_pop(ctx);
        return ast_create_node(ctx->ast_arena, AST_BAD, tok_begin, scope_get(ctx));
    }

    if (create_scope) scope_pop(ctx);
    return block;
}

void parse_ublock_content(struct context *ctx, struct ast *ublock)
{
    BL_ASSERT(ublock->kind == AST_UBLOCK);
    ublock->data.ublock.nodes = tarray_new(sizeof(struct ast *));
    tarray_reserve(ublock->data.ublock.nodes, 64);
    struct ast *tmp;
NEXT:
    if (parse_semicolon(ctx)) goto NEXT;
    if (parse_docs(ctx)) goto NEXT;
    if (parse_unit_docs(ctx)) goto NEXT;

    if ((tmp = parse_decl(ctx))) {
        if (AST_IS_OK(tmp)) {
            struct ast *decl = tmp->data.decl_entity.value;
            if (decl && rq_semicolon_after_decl_entity(decl)) parse_semicolon_rq(ctx);
            // setup global scope flag for declaration
            tmp->data.decl_entity.is_global = true;
            if (ctx->current_private_scope) tmp->data.decl_entity.flags |= FLAG_PRIVATE;
        }

        tarray_push(ublock->data.ublock.nodes, tmp);
        goto NEXT;
    }

    // load, import, link, test, private - enabled in global scope
    const int enabled_hd = HD_LOAD | HD_LINK | HD_PRIVATE | HD_IMPORT | HD_SCOPE;
    if ((tmp = parse_hash_directive(ctx, enabled_hd, NULL))) {
        tarray_push(ublock->data.ublock.nodes, tmp);
        goto NEXT;
    }

    struct token *tok = tokens_peek(ctx->tokens);
    if (!token_is(tok, SYM_EOF)) {
        PARSE_ERROR(ERR_UNEXPECTED_SYMBOL,
                    tok,
                    BUILDER_CUR_WORD,
                    "Unexpected symbol in module body '%s'.",
                    sym_strings[tok->sym]);
    }
}

void init_hash_directives(struct context *ctx)
{
    static const char *hash_directive_names[] = {
#define HD_GEN(kind, name, flag) name,
#include "parser.inc"
#undef HD_GEN
    };

    static u32 hash_directive_flags[TARRAY_SIZE(hash_directive_names)] = {
#define HD_GEN(kind, name, flag) flag,
#include "parser.inc"
#undef HD_GEN
    };

    for (usize i = 0; i < TARRAY_SIZE(hash_directive_names); ++i) {
        const hash_t hash = strhash(hash_directive_names[i]);
        hmput(ctx->hash_directive_table, hash, hash_directive_flags[i]);
    }
}

void parser_run(struct assembly *assembly, struct unit *unit)
{
    BL_ASSERT(assembly);
    BL_ASSERT(assembly->gscope && "Missing global scope for assembly.");

    ZONE();
    struct context ctx = {
        .assembly     = assembly,
        .unit         = unit,
        .ast_arena    = &assembly->arenas.ast,
        .scope_arenas = &assembly->arenas.scope,
        .tokens       = &unit->tokens,
    };

    init_hash_directives(&ctx);
    scope_push(&ctx, assembly->gscope);

    struct ast *root       = ast_create_node(ctx.ast_arena, AST_UBLOCK, NULL, scope_get(&ctx));
    root->data.ublock.unit = unit;
    unit->ast              = root;

    parse_ublock_content(&ctx, unit->ast);
    if (ctx.unit_docs_tmp) unit->ast->docs = ctx.unit_docs_tmp->data;

    hmfree(ctx.hash_directive_table);
    arrfree(ctx.decl_stack);
    arrfree(ctx.scope_stack);
    arrfree(ctx.fn_type_stack);
    RETURN_ZONE();
}
