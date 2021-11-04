// =================================================================================================
// bl
//
// File:   docs.c
// Author: Martin Dorazil
// Date:   11/15/20
//
// Copyright 2020 Martin Dorazil
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
#include "stb_ds.h"

#define OUT_DIR "out"

#define DECORATE_HEADER(stream, str, c)                                                            \
    {                                                                                              \
        char *it = (char *)str;                                                                    \
        while (*it++) {                                                                            \
            fprintf(stream, "%s", c);                                                              \
        }                                                                                          \
    }

#define H1(stream, str)                                                                            \
    {                                                                                              \
        fprintf(stream, "%s\n", str);                                                              \
        DECORATE_HEADER(stream, str, "=");                                                         \
        fprintf(stream, "\n");                                                                     \
    }

#define H2(stream, str)                                                                            \
    {                                                                                              \
        fprintf(stream, "%s\n", str);                                                              \
        DECORATE_HEADER(stream, str, "-");                                                         \
        fprintf(stream, "\n");                                                                     \
    }

#define H3(stream, str)                                                                            \
    {                                                                                              \
        fprintf(stream, "%s\n", str);                                                              \
        DECORATE_HEADER(stream, str, "^");                                                         \
        fprintf(stream, "\n");                                                                     \
    }

#define REF(stream, name)                                                                          \
    if (name[0] == '_') {                                                                          \
        fprintf(stream, ".. _%s:\n\n", &name[1]);                                                  \
    } else {                                                                                       \
        fprintf(stream, ".. _%s:\n\n", name);                                                      \
    }

#define CODE_BLOCK_BEGIN(stream, lang) fprintf(stream, ".. code-block:: %s\n", lang);
#define CODE_BLOCK_NEW_LINE(stream) fprintf(stream, "\n    ");

#define DEFAULT_TOC(stream, filename)                                                              \
    fprintf(f, "\n\n.. toctree::\n    :glob:\n    :titlesonly:\n\n    %s/*", filename);

#define PUSH_IS_INLINE(ctx)                                                                        \
    const bool _prev_is_inline = ctx->is_inline;                                                   \
    ctx->is_inline             = true;

#define POP_IS_INLINE(ctx) ctx->is_inline = _prev_is_inline;

#define PUSH_IS_MULTI_RETURN(ctx)                                                                  \
    const bool _prev_is_mr = ctx->is_multi_return;                                                 \
    ctx->is_multi_return   = true;

#define POP_IS_MULTI_RETURN(ctx) ctx->is_multi_return = _prev_is_mr;

struct context {
    struct unit *unit;
    FILE        *stream;
    bool         is_inline;
    bool         is_multi_return;
    char        *path_unit_dir;
    char        *section_variants;
    char        *section_members;
};

static void append_section(struct context *ctx, const char *name, const char *content);

static void doc(struct context *ctx, struct ast *node);
static void doc_unit(struct context *ctx, struct unit *unit);
static void doc_ublock(struct context *ctx, struct ast *block);
static void doc_lit_int(struct context *ctx, struct ast *lit);
static void doc_decl_entity(struct context *ctx, struct ast *decl);
static void doc_decl_arg(struct context *ctx, struct ast *decl);
static void doc_decl_variant(struct context *ctx, struct ast *decl);
static void doc_decl_member(struct context *ctx, struct ast *decl);
static void doc_type_ptr(struct context *ctx, struct ast *type);
static void doc_type_fn(struct context *ctx, struct ast *type);
static void doc_type_enum(struct context *ctx, struct ast *type);
static void doc_type_struct(struct context *ctx, struct ast *type);
static void doc_type_slice(struct context *ctx, struct ast *type);
static void doc_type_dynarray(struct context *ctx, struct ast *type);
static void doc_type_vargs(struct context *ctx, struct ast *type);
static void doc_type_poly(struct context *ctx, struct ast *type);
static void doc_expr_lit_fn_group(struct context *ctx, struct ast *lit);

void append_section(struct context *ctx, const char *name, const char *content)
{
    H2(ctx->stream, name);
    fprintf(ctx->stream, "%s", content);
}

void doc_ublock(struct context *ctx, struct ast *block)
{
    for (usize i = 0; i < arrlenu(block->data.ublock.nodes); ++i) {
        doc(ctx, block->data.ublock.nodes[i]);
    }
}

void doc_lit_int(struct context *ctx, struct ast *lit)
{
    u64 value = lit->data.expr_integer.val;
    fprintf(ctx->stream, "%llu", value);
}

void doc_decl_entity(struct context *ctx, struct ast *decl)
{
    struct ast *ident = decl->data.decl.name;
    struct ast *type  = decl->data.decl.type;
    struct ast *value = decl->data.decl_entity.value;
    if (!ident) return;
    const char *text       = decl->docs;
    const char *name       = ident->data.ident.id.str;
    const bool  is_mutable = decl->data.decl_entity.mut;

    if (text && strstr(text, "@INCOMPLETE")) {
        builder_msg(BUILDER_MSG_WARNING,
                    0,
                    ident->location,
                    BUILDER_CUR_WORD,
                    "Found incomplete documentation!");
    }

    // if (!text) return;
    if (!decl->owner_scope) return;
    if (decl->owner_scope->kind != SCOPE_GLOBAL && decl->owner_scope->kind != SCOPE_NAMED) return;

    char *full_name = gettmpstr();
    if (decl->owner_scope->name) {
        strprint(full_name, "%s.%s", decl->owner_scope->name, ident->data.ident.id.str);
    } else {
        strprint(full_name, "%s", ident->data.ident.id.str);
    }

    char *export_file = gettmpstr();
    strprint(export_file, "%s/%s.rst", ctx->path_unit_dir, full_name);
    FILE *f = fopen(export_file, "w");
    if (f == NULL) {
        builder_error("Cannot open file '%s'", export_file);
        puttmpstr(full_name);
        puttmpstr(export_file);
        return;
    }
    puttmpstr(export_file);
    ctx->stream = f;

    REF(ctx->stream, full_name);
    H1(ctx->stream, full_name);
    CODE_BLOCK_BEGIN(ctx->stream, "bl");
    CODE_BLOCK_NEW_LINE(ctx->stream);
    fprintf(ctx->stream, "%s :", name);
    if (type) {
        fprintf(ctx->stream, " ");
        doc(ctx, type);
        fprintf(ctx->stream, " ");
    }
    fprintf(ctx->stream, "%s", is_mutable ? "= " : ": ");
    doc(ctx, value);

    if (decl->data.decl_entity.flags & FLAG_EXTERN) fprintf(ctx->stream, " #extern");
    if (decl->data.decl_entity.flags & FLAG_INLINE) fprintf(ctx->stream, " #inline");
    fprintf(ctx->stream, "\n\n");
    if (text) fprintf(ctx->stream, "%s\n\n", text);

    if (strlenu(ctx->section_variants) > 0) {
        append_section(ctx, "Variants", ctx->section_variants);
        strclr(ctx->section_variants);
    }

    if (strlenu(ctx->section_members) > 0) {
        append_section(ctx, "Members", ctx->section_members);
        strclr(ctx->section_members);
    }

    fprintf(ctx->stream, "\n\n*Declared in: %s*\n", ctx->unit->filename);

    ctx->stream = NULL;
    fclose(f);
    puttmpstr(full_name);
}

void doc_decl_arg(struct context *ctx, struct ast *decl)
{
    struct ast *ident = decl->data.decl.name;
    struct ast *type  = decl->data.decl.type;
    if (ident) {
        const char *name = ident->data.ident.id.str;
        fprintf(ctx->stream, "%s: ", name);
    }
    doc(ctx, type);
}

void doc_decl_variant(struct context *ctx, struct ast *decl)
{
    struct ast *ident = decl->data.decl.name;
    struct ast *value = decl->data.decl_variant.value;
    if (ident) {
        const char *name = ident->data.ident.id.str;
        fprintf(ctx->stream, "%s", name);
        if (decl->docs) {
            strappend(ctx->section_variants, "**%s** - %s\n\n", name, decl->docs);
        }
    }
    if (value && value->kind == AST_EXPR_LIT_INT) {
        fprintf(ctx->stream, " :: ");
        doc(ctx, value);
    }
}

void doc_decl_member(struct context *ctx, struct ast *decl)
{
    struct ast *ident = decl->data.decl.name;
    struct ast *type  = decl->data.decl.type;
    if (ident) {
        const char *name = ident->data.ident.id.str;
        fprintf(ctx->stream, "%s: ", name);

        if (decl->docs) {
            strappend(ctx->section_members, "**%s** - %s\n\n", name, decl->docs);
        }
    }
    doc(ctx, type);
}

void doc_type_fn(struct context *ctx, struct ast *type)
{
    struct ast *ret_type = type->data.type_fn.ret_type;
    fprintf(ctx->stream, "fn (");
    PUSH_IS_INLINE(ctx);

    ast_nodes_t *args = type->data.type_fn.args;
    for (usize i = 0; i < sarrlenu(args); ++i) {
        struct ast *arg = sarrpeek(args, i);
        doc(ctx, arg);
        if (i + 1 < sarrlenu(args)) fprintf(ctx->stream, ", ");
    }
    fprintf(ctx->stream, ") ");
    PUSH_IS_MULTI_RETURN(ctx);
    if (ret_type) doc(ctx, ret_type);
    POP_IS_MULTI_RETURN(ctx);
    POP_IS_INLINE(ctx);
}

void doc_type_enum(struct context *ctx, struct ast *type)
{
    fprintf(ctx->stream, "enum ");
    if (type->data.type_enm.type) {
        doc(ctx, type->data.type_enm.type);
        fprintf(ctx->stream, " ");
    }
    fprintf(ctx->stream, "{");
    ast_nodes_t *variants = type->data.type_enm.variants;
    for (usize i = 0; i < sarrlenu(variants); ++i) {
        struct ast *variant = sarrpeek(variants, i);
        CODE_BLOCK_NEW_LINE(ctx->stream);
        fprintf(ctx->stream, "    ");
        doc(ctx, variant);
        fprintf(ctx->stream, ";");
    }
    CODE_BLOCK_NEW_LINE(ctx->stream);
    fprintf(ctx->stream, "}");
}

void doc_type_struct(struct context *ctx, struct ast UNUSED(*type))
{
    if (!ctx->is_multi_return)
        fprintf(ctx->stream, "struct {");
    else
        fprintf(ctx->stream, "(");

    ast_nodes_t *members = type->data.type_strct.members;
    for (usize i = 0; i < sarrlenu(members); ++i) {
        struct ast *member = sarrpeek(members, i);
        if (ctx->is_multi_return) {
            doc(ctx, member);
            if (i + 1 < sarrlenu(members)) fprintf(ctx->stream, ", ");
        } else {
            CODE_BLOCK_NEW_LINE(ctx->stream);
            fprintf(ctx->stream, "    ");
            doc(ctx, member);
            fprintf(ctx->stream, ";");
        }
    }
    if (ctx->is_multi_return) {
        fprintf(ctx->stream, ")");
    } else {
        CODE_BLOCK_NEW_LINE(ctx->stream);
        fprintf(ctx->stream, "}");
    }
}

void doc_type_slice(struct context *ctx, struct ast *type)
{
    struct ast *elem_type = type->data.type_slice.elem_type;
    fprintf(ctx->stream, "[]");
    doc(ctx, elem_type);
}

void doc_type_dynarray(struct context *ctx, struct ast *type)
{
    struct ast *elem_type = type->data.type_dynarr.elem_type;
    fprintf(ctx->stream, "[..]");
    doc(ctx, elem_type);
}

void doc_ref(struct context *ctx, struct ast *ref)
{
    struct ast *ident           = ref->data.ref.ident;
    struct ast *ident_namespace = ref->data.ref.next;
    if (ident_namespace) {
        doc(ctx, ident_namespace);
        // const char *name = ident_namespace->data.ident.id.str;
        fprintf(ctx->stream, ".");
    }
    const char *name = ident->data.ident.id.str;
    fprintf(ctx->stream, "%s", name);
}

void doc_type_ptr(struct context *ctx, struct ast *type)
{
    struct ast *next_type = type->data.type_ptr.type;
    fprintf(ctx->stream, "*");
    doc(ctx, next_type);
}

void doc_type_vargs(struct context *ctx, struct ast *type)
{
    struct ast *next_type = type->data.type_vargs.type;
    fprintf(ctx->stream, "...");
    doc(ctx, next_type);
}

void doc_type_poly(struct context *ctx, struct ast *type)
{
    struct ast *ident = type->data.type_poly.ident;
    fprintf(ctx->stream, "?%s", ident->data.ident.id.str);
}

void doc_expr_lit_fn_group(struct context *ctx, struct ast *lit)
{
    ast_nodes_t *variants = lit->data.expr_fn_group.variants;
    fprintf(ctx->stream, "fn { ");
    for (usize i = 0; i < sarrlenu(variants); ++i) {
        struct ast *variant = sarrpeek(variants, i);
        doc(ctx, variant);
        if (i < sarrlenu(variants)) fprintf(ctx->stream, "; ");
    }
    fprintf(ctx->stream, "}");
}

void doc(struct context *ctx, struct ast *node)
{
    if (!node) return;
    switch (node->kind) {
    case AST_UBLOCK:
        doc_ublock(ctx, node);
        break;
    case AST_EXPR_LIT_INT:
        doc_lit_int(ctx, node);
        break;
    case AST_DECL_ENTITY:
        doc_decl_entity(ctx, node);
        break;
    case AST_DECL_ARG:
        doc_decl_arg(ctx, node);
        break;
    case AST_DECL_VARIANT:
        doc_decl_variant(ctx, node);
        break;
    case AST_DECL_MEMBER:
        doc_decl_member(ctx, node);
        break;
    case AST_EXPR_LIT_FN:
        doc(ctx, node->data.expr_fn.type);
        break;
    case AST_EXPR_LIT_FN_GROUP:
        doc_expr_lit_fn_group(ctx, node);
        break;
    case AST_EXPR_TYPE:
        doc(ctx, node->data.expr_type.type);
        break;
    case AST_TYPE_ENUM:
        doc_type_enum(ctx, node);
        break;
    case AST_TYPE_STRUCT:
        doc_type_struct(ctx, node);
        break;
    case AST_TYPE_PTR:
        doc_type_ptr(ctx, node);
        break;
    case AST_REF:
        doc_ref(ctx, node);
        break;
    case AST_TYPE_FN:
        doc_type_fn(ctx, node);
        break;
    case AST_TYPE_SLICE:
        doc_type_slice(ctx, node);
        break;
    case AST_TYPE_DYNARR:
        doc_type_dynarray(ctx, node);
        break;
    case AST_TYPE_VARGS:
        doc_type_vargs(ctx, node);
        break;
    case AST_TYPE_POLY:
        doc_type_poly(ctx, node);
        break;
    case AST_LOAD:
    case AST_PRIVATE:
    case AST_LINK:
    case AST_IMPORT:
    case AST_SCOPE:
        break;
    default:
        bwarn("Missing doc generation for AST node '%s'.", ast_get_name(node));
        break;
    }
}

void doc_unit(struct context *ctx, struct unit *unit)
{
    if (!unit->filename) return;
    char *unit_name = gettmpstr();
    strprint(unit_name, "%.*s", (s32)strlen(unit->filename) - 3, unit->filename); // -3 ('.bl')
    ctx->unit = unit;
    // prepare unit output directory
    {
        strclr(ctx->path_unit_dir);
        strprint(ctx->path_unit_dir, "%s/%s", OUT_DIR, unit_name);
        const char *dirpath = ctx->path_unit_dir;
        if (!dir_exists(dirpath)) create_dir(dirpath);
    }

    doc(ctx, unit->ast);

    // write unit global docs
    char *export_file = gettmpstr();
    strprint(export_file, "%s/%s.rst", OUT_DIR, unit_name);
    FILE *f = fopen(export_file, "w");
    if (f == NULL) {
        builder_error("Cannot open file '%s'", export_file);
        puttmpstr(export_file);
        return;
    }
    puttmpstr(export_file);
    if (unit->ast->docs) {
        fprintf(f, "%s", unit->ast->docs);
    } else {
        H1(f, unit->filename);
    }
    DEFAULT_TOC(f, unit_name);
    fclose(f);
    puttmpstr(unit_name);
}

void docs_run(struct assembly *assembly)
{
    zone();
    struct context ctx;
    memset(&ctx, 0, sizeof(struct context));
    strinit(ctx.path_unit_dir, 128);
    strinit(ctx.section_variants, 128);
    strinit(ctx.section_members, 128);

    // prepare output directory
    if (!dir_exists(OUT_DIR)) create_dir(OUT_DIR);

    for (usize i = 0; i < arrlenu(assembly->units); ++i) {
        struct unit *unit = assembly->units[i];
        doc_unit(&ctx, unit);
    }

    // cleanup
    strfree(ctx.path_unit_dir);
    strfree(ctx.section_variants);
    strfree(ctx.section_members);

    builder_note("Documentation written into '%s' directory.", OUT_DIR);
    return_zone();
}
