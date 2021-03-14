//************************************************************************************************
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
//************************************************************************************************

#include "builder.h"
#include "common.h"

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

#define PUSH_IS_INLINE(cnt)                                                                        \
    const bool _prev_is_inline = cnt->is_inline;                                                   \
    cnt->is_inline             = true;

#define POP_IS_INLINE(cnt) cnt->is_inline = _prev_is_inline;

#define PUSH_IS_MULTI_RETURN(cnt)                                                                  \
    const bool _prev_is_mr = cnt->is_multi_return;                                                 \
    cnt->is_multi_return   = true;

#define POP_IS_MULTI_RETURN(cnt) cnt->is_multi_return = _prev_is_mr;

typedef struct {
    Unit *  unit;
    FILE *  stream;
    TString path_unit_dir;
    TString path_tmp;
    s32     pad;
    bool    is_inline;
    bool    is_multi_return;

    TString section_variants;
    TString section_members;
} Context;

static void append_section(Context *cnt, const char *name, const char *content);

static void doc(Context *cnt, Ast *node);
static void doc_unit(Context *cnt, Unit *unit);
static void doc_ublock(Context *cnt, Ast *block);
static void doc_lit_int(Context *cnt, Ast *lit);
static void doc_decl_entity(Context *cnt, Ast *decl);
static void doc_decl_arg(Context *cnt, Ast *decl);
static void doc_decl_variant(Context *cnt, Ast *decl);
static void doc_decl_member(Context *cnt, Ast *decl);
static void doc_type_ptr(Context *cnt, Ast *type);
static void doc_type_fn(Context *cnt, Ast *type);
static void doc_type_enum(Context *cnt, Ast *type);
static void doc_type_struct(Context *cnt, Ast *type);
static void doc_type_slice(Context *cnt, Ast *type);
static void doc_type_vargs(Context *cnt, Ast *type);

void append_section(Context *cnt, const char *name, const char *content)
{
    H2(cnt->stream, name);
    fprintf(cnt->stream, "%s", content);
}

void doc_ublock(Context *cnt, Ast *block)
{
    Ast *tmp;
    TARRAY_FOREACH(Ast *, block->data.ublock.nodes, tmp) doc(cnt, tmp);
}

void doc_lit_int(Context *cnt, Ast *lit)
{
    u64 value = lit->data.expr_integer.val;
    fprintf(cnt->stream, "%llu", value);
}

void doc_decl_entity(Context *cnt, Ast *decl)
{
    Ast *ident = decl->data.decl.name;
    Ast *type  = decl->data.decl.type;
    Ast *value = decl->data.decl_entity.value;
    if (!ident) return;
    const char *text       = decl->docs;
    const char *name       = ident->data.ident.id.str;
    const bool  is_mutable = decl->data.decl_entity.mut;
    // if (!text) return;
    if (!decl->owner_scope) return;
    if (decl->owner_scope->kind != SCOPE_GLOBAL) return;

    tstring_clear(&cnt->path_tmp);
    tstring_setf(&cnt->path_tmp, "%s/%s.rst", cnt->path_unit_dir.data, name);
    char *export_file = cnt->path_tmp.data;
    FILE *f           = fopen(export_file, "w");
    if (f == NULL) {
        builder_error("Cannot open file '%s'", export_file);
        return;
    }
    cnt->stream = f;

    REF(cnt->stream, name);
    H1(cnt->stream, name);
    CODE_BLOCK_BEGIN(cnt->stream, "bl");
    CODE_BLOCK_NEW_LINE(cnt->stream);
    fprintf(cnt->stream, "%s :", name);
    if (type) {
        fprintf(cnt->stream, " ");
        doc(cnt, type);
        fprintf(cnt->stream, " ");
    }
    fprintf(cnt->stream, "%s", is_mutable ? "= " : ": ");
    doc(cnt, value);

    if (decl->data.decl_entity.flags & FLAG_EXTERN) fprintf(cnt->stream, " #extern");
    if (decl->data.decl_entity.flags & FLAG_INLINE) fprintf(cnt->stream, " #inline");
    fprintf(cnt->stream, "\n\n");
    if (text) fprintf(cnt->stream, "%s\n\n", text);

    if (cnt->section_variants.len > 0) {
        append_section(cnt, "Variants", cnt->section_variants.data);
        tstring_clear(&cnt->section_variants);
    }

    if (cnt->section_members.len > 0) {
        append_section(cnt, "Members", cnt->section_members.data);
        tstring_clear(&cnt->section_members);
    }

    fprintf(cnt->stream, "\n\n*Declared in: %s*\n", cnt->unit->filename);

    cnt->stream = NULL;
    fclose(f);
}

void doc_decl_arg(Context *cnt, Ast *decl)
{
    Ast *ident = decl->data.decl.name;
    Ast *type  = decl->data.decl.type;
    if (ident) {
        const char *name = ident->data.ident.id.str;
        fprintf(cnt->stream, "%s: ", name);
    }
    doc(cnt, type);
}

void doc_decl_variant(Context *cnt, Ast *decl)
{
    Ast *ident = decl->data.decl.name;
    Ast *value = decl->data.decl_variant.value;
    if (ident) {
        const char *name = ident->data.ident.id.str;
        fprintf(cnt->stream, "%s", name);

        if (decl->docs) {
            TString *section = &cnt->section_variants;
            tstring_append(section, "**");
            tstring_append(section, name);
            tstring_append(section, "** - ");
            tstring_append(section, decl->docs);
            tstring_append(section, "\n\n");
        }
    }
    if (value && value->kind == AST_EXPR_LIT_INT) {
        fprintf(cnt->stream, " :: ");
        doc(cnt, value);
    }
}

void doc_decl_member(Context *cnt, Ast *decl)
{
    Ast *ident = decl->data.decl.name;
    Ast *type  = decl->data.decl.type;
    if (ident) {
        const char *name = ident->data.ident.id.str;
        fprintf(cnt->stream, "%s: ", name);

        if (decl->docs) {
            TString *section = &cnt->section_members;
            tstring_append(section, "**");
            tstring_append(section, name);
            tstring_append(section, "** - ");
            tstring_append(section, decl->docs);
            tstring_append(section, "\n\n");
        }
    }
    doc(cnt, type);
}

void doc_type_fn(Context *cnt, Ast *type)
{
    Ast *ret_type = type->data.type_fn.ret_type;
    fprintf(cnt->stream, "fn (");
    PUSH_IS_INLINE(cnt);
    if (type->data.type_fn.args) {
        Ast *arg;
        TSA_FOREACH(type->data.type_fn.args, arg)
        {
            doc(cnt, arg);
            if (i + 1 < type->data.type_fn.args->size) fprintf(cnt->stream, ", ");
        }
    }
    fprintf(cnt->stream, ") ");
    PUSH_IS_MULTI_RETURN(cnt);
    if (ret_type) doc(cnt, ret_type);
    POP_IS_MULTI_RETURN(cnt);
    POP_IS_INLINE(cnt);
}

void doc_type_enum(Context *cnt, Ast *type)
{
    fprintf(cnt->stream, "enum ");
    if (type->data.type_enm.type) {
        doc(cnt, type->data.type_enm.type);
        fprintf(cnt->stream, " ");
    }
    fprintf(cnt->stream, "{");
    if (type->data.type_enm.variants) {
        Ast *variant;
        TSA_FOREACH(type->data.type_enm.variants, variant)
        {
            CODE_BLOCK_NEW_LINE(cnt->stream);
            fprintf(cnt->stream, "    ");
            doc(cnt, variant);
            fprintf(cnt->stream, ";");
        }
    }
    CODE_BLOCK_NEW_LINE(cnt->stream);
    fprintf(cnt->stream, "}");
}

void doc_type_struct(Context *cnt, Ast UNUSED(*type))
{
    if (!cnt->is_multi_return)
        fprintf(cnt->stream, "struct {");
    else
        fprintf(cnt->stream, "(");

    if (type->data.type_strct.members) {
        Ast *member;
        TSA_FOREACH(type->data.type_strct.members, member)
        {
            if (cnt->is_multi_return) {
                doc(cnt, member);
                if (i + 1 < type->data.type_strct.members->size) fprintf(cnt->stream, ", ");
            } else {
                CODE_BLOCK_NEW_LINE(cnt->stream);
                fprintf(cnt->stream, "    ");
                doc(cnt, member);
                fprintf(cnt->stream, ";");
            }
        }
    }
    if (cnt->is_multi_return) {
        fprintf(cnt->stream, ")");
    } else {
        CODE_BLOCK_NEW_LINE(cnt->stream);
        fprintf(cnt->stream, "}");
    }
}

void doc_type_slice(Context *cnt, Ast *type)
{
    Ast *elem_type = type->data.type_slice.elem_type;
    fprintf(cnt->stream, "[]");
    doc(cnt, elem_type);
}

void doc_ref(Context *cnt, Ast *ref)
{
    Ast *ident = ref->data.ref.ident;
    if (!ident) return;
    const char *name = ident->data.ident.id.str;
    fprintf(cnt->stream, "%s", name);
}

void doc_type_ptr(Context *cnt, Ast *type)
{
    Ast *next_type = type->data.type_ptr.type;
    fprintf(cnt->stream, "*");
    doc(cnt, next_type);
}

void doc_type_vargs(Context *cnt, Ast *type)
{
    Ast *next_type = type->data.type_vargs.type;
    fprintf(cnt->stream, "...");
    doc(cnt, next_type);
}

void doc(Context *cnt, Ast *node)
{
    if (!node) return;
    switch (node->kind) {
    case AST_UBLOCK:
        doc_ublock(cnt, node);
        break;
    case AST_EXPR_LIT_INT:
        doc_lit_int(cnt, node);
        break;
    case AST_DECL_ENTITY:
        doc_decl_entity(cnt, node);
        break;
    case AST_DECL_ARG:
        doc_decl_arg(cnt, node);
        break;
    case AST_DECL_VARIANT:
        doc_decl_variant(cnt, node);
        break;
    case AST_DECL_MEMBER:
        doc_decl_member(cnt, node);
        break;
    case AST_EXPR_LIT_FN:
        doc(cnt, node->data.expr_fn.type);
        break;
    case AST_EXPR_TYPE:
        doc(cnt, node->data.expr_type.type);
        break;
    case AST_TYPE_ENUM:
        doc_type_enum(cnt, node);
        break;
    case AST_TYPE_STRUCT:
        doc_type_struct(cnt, node);
        break;
    case AST_TYPE_PTR:
        doc_type_ptr(cnt, node);
        break;
    case AST_REF:
        doc_ref(cnt, node);
        break;
    case AST_TYPE_FN:
        doc_type_fn(cnt, node);
        break;
    case AST_TYPE_SLICE:
        doc_type_slice(cnt, node);
        break;
    case AST_TYPE_VARGS:
        doc_type_vargs(cnt, node);
        break;
    case AST_LOAD:
    case AST_PRIVATE:
    case AST_LINK:
    case AST_IMPORT:
        break;
    default:
        builder_warning("Missing doc generation for AST node '%s'.", ast_get_name(node));
        break;
    }
}

void doc_unit(Context *cnt, Unit *unit)
{
    if (!unit->filename) return;
    TString unit_name;
    tstring_init(&unit_name);
    tstring_append_n(&unit_name, unit->filename, strlen(unit->filename) - 3); // -3 ('.bl')
    cnt->unit = unit;
    // prepare unit output directory
    {
        tstring_clear(&cnt->path_unit_dir);
        tstring_setf(&cnt->path_unit_dir, "%s/%s", OUT_DIR, unit_name.data);
        const char *dirpath = cnt->path_unit_dir.data;
        if (!dir_exists(dirpath)) create_dir(dirpath);
    }

    doc(cnt, unit->ast);

    // write unit global docs
    tstring_clear(&cnt->path_tmp);
    tstring_setf(&cnt->path_tmp, "%s/%s.rst", OUT_DIR, unit_name.data);
    char *export_file = cnt->path_tmp.data;
    FILE *f           = fopen(export_file, "w");
    if (f == NULL) {
        builder_error("Cannot open file '%s'", export_file);
        return;
    }
    if (unit->ast->docs) {
        fprintf(f, "%s", unit->ast->docs);
    } else {
        H1(f, unit->filename);
    }
    DEFAULT_TOC(f, unit_name.data);
    fclose(f);
    tstring_terminate(&unit_name);
}

void docs_run(Assembly *assembly)
{
    Context cnt;
    memset(&cnt, 0, sizeof(Context));
    TracyCZone(_tctx, true);
    tstring_init(&cnt.path_tmp);
    tstring_init(&cnt.path_unit_dir);
    tstring_init(&cnt.section_variants);
    tstring_init(&cnt.section_members);

    // prepare output directory
    if (!dir_exists(OUT_DIR)) create_dir(OUT_DIR);

    Unit *unit;
    TARRAY_FOREACH(Unit *, &assembly->units, unit) doc_unit(&cnt, unit);

    // cleanup
    tstring_terminate(&cnt.path_tmp);
    tstring_terminate(&cnt.path_unit_dir);
    tstring_terminate(&cnt.section_variants);
    tstring_terminate(&cnt.section_members);

    builder_note("Documentation written into '%s' directory.", OUT_DIR);
    TracyCZoneEnd(_tctx);
}
