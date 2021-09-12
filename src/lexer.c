// =================================================================================================
// blc
//
// File:   lexer_new.c
// Author: Martin Dorazil
// Date:   19/02/2018
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
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILItTY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
// =================================================================================================

#include "builder.h"
#include "common.h"
#include "tokens_inline_utils.h"
#include <float.h>
#include <setjmp.h>
#include <string.h>

#define IS_IDENT(c)                                                                                \
    (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || ((c) >= '0' && (c) <= '9') ||     \
     (c) == '_')

#define SCAN_ERROR(code, format, ...)                                                              \
    {                                                                                              \
        builder_msg(BUILDER_MSG_ERROR, (code), NULL, BUILDER_CUR_NONE, (format), ##__VA_ARGS__);   \
        longjmp((ctx)->jmp_error, code);                                                           \
    }

struct context {
    struct assembly *assembly;
    struct unit *    unit;
    struct tokens *  tokens;
    jmp_buf          jmp_error;
    char *           c;
    s32              line;
    s32              col;
};

static void       scan(struct context *ctx);
static bool       scan_comment(struct context *ctx, struct token *tok, const char *term);
static bool       scan_docs(struct context *ctx, struct token *tok);
static bool       scan_ident(struct context *ctx, struct token *tok);
static bool       scan_string(struct context *ctx, struct token *tok);
static bool       scan_char(struct context *ctx, struct token *tok);
static bool       scan_number(struct context *ctx, struct token *tok);
static INLINE int c_to_number(char c, s32 base);
static char       scan_specch(char c);

bool scan_comment(struct context *ctx, struct token *tok, const char *term)
{
    if (tok->sym == SYM_SHEBANG && ctx->line != 1) {
        // Unterminated comment
        SCAN_ERROR(ERR_INVALID_TOKEN,
                   "%s %d:%d Shebang is allowed only on the first line of the file.",
                   ctx->unit->name,
                   ctx->line,
                   ctx->col);
    }
    const usize len = strlen(term);
    while (true) {
        if (*ctx->c == '\n') {
            ctx->line++;
            ctx->col = 1;
        } else if (*ctx->c == '\0' && strcmp(term, "\n") != 0) {
            // Unterminated comment
            SCAN_ERROR(ERR_UNTERMINATED_COMMENT,
                       "%s %d:%d unterminated comment block.",
                       ctx->unit->name,
                       ctx->line,
                       ctx->col);
        }
        if (*ctx->c == SYM_EOF) return true;
        if (strncmp(ctx->c, term, len) == 0) break;
        ctx->c++;
    }
    ctx->c += len;
    return true;
}

bool scan_docs(struct context *ctx, struct token *tok)
{
    BL_ASSERT(token_is(tok, SYM_DCOMMENT) || token_is(tok, SYM_DGCOMMENT));
    tok->location.line = ctx->line;
    tok->location.col  = ctx->col;

    char *begin      = ctx->c;
    s32   len_parsed = 0;
    s32   len_str    = 0;
    while (true) {
        if (*ctx->c == SYM_EOF) break;
        if (*ctx->c == '\r' || *ctx->c == '\n') break;
        if (!len_parsed && *ctx->c == ' ') {
            ++begin;
        } else {
            len_str++;
        }
        len_parsed++;
        ctx->c++;
    }

    TString *cstr = builder_create_cached_str();
    tstring_append_n(cstr, begin, len_str);
    tok->value.str    = cstr->data;
    tok->location.len = len_parsed + 3; // + 3 = '///'
    ctx->col += len_parsed;
    return true;
}

bool scan_ident(struct context *ctx, struct token *tok)
{
    tok->location.line = ctx->line;
    tok->location.col  = ctx->col;
    tok->sym           = SYM_IDENT;

    char *begin = ctx->c;

    s32 len = 0;
    while (true) {
        if (!IS_IDENT(*ctx->c)) {
            break;
        }

        len++;
        ctx->c++;
    }

    if (len == 0) return false;
    TString *cstr = builder_create_cached_str();
    tstring_append_n(cstr, begin, len);
    tok->value.str    = cstr->data;
    tok->location.len = len;
    ctx->col += len;
    return true;
}

INLINE char scan_specch(char c)
{
    switch (c) {
    case 'n':
        return '\n';
    case 'r':
        return '\r';
    case 't':
        return '\t';
    case '0':
        return '\0';
    case '\"':
        return '\"';
    case '\'':
        return '\'';
    case '\\':
        return '\\';
    default:
        return c;
    }
}

bool scan_string(struct context *ctx, struct token *tok)
{
    if (*ctx->c != '"') return false;
    tok->location.line = ctx->line;
    tok->location.col  = ctx->col;
    tok->sym           = SYM_STRING;
    TString *cstr      = builder_create_cached_str();
    char     c;
    s32      len = 0;

    // eat "
    ctx->c++;
    while (true) {
        switch (*ctx->c) {
        case '"': {
            ctx->c++;
            goto DONE;
        }
        case '\0': {
            SCAN_ERROR(ERR_UNTERMINATED_STRING,
                       "%s %d:%d unterminated string.",
                       ctx->unit->name,
                       ctx->line,
                       ctx->col);
        }
        case '\\':
            // special character

            // @INCOMPLETE: this can fail!!!
            // @INCOMPLETE: this can fail!!!
            // @INCOMPLETE: this can fail!!!
            c = scan_specch(*(ctx->c + 1));
            ctx->c += 2;
            len += 2;
            break;
        default:
            c = *ctx->c;
            len++;
            ctx->c++;
        }
        tstring_append_n(cstr, &c, 1);
    }
DONE:
    tok->value.str    = cstr->data;
    tok->location.len = len;
    tok->location.col += 1;
    ctx->col += len + 2;
    return true;
}

bool scan_char(struct context *ctx, struct token *tok)
{
    if (*ctx->c != '\'') return false;
    tok->location.line = ctx->line;
    tok->location.col  = ctx->col;
    tok->location.len  = 0;
    tok->sym           = SYM_CHAR;

    // eat '
    ctx->c++;

    switch (*ctx->c) {
    case '\'': {
        SCAN_ERROR(
            ERR_EMPTY, "%s %d:%d expected character in ''.", ctx->unit->name, ctx->line, ctx->col);
    }
    case '\0': {
        SCAN_ERROR(ERR_UNTERMINATED_STRING,
                   "%s %d:%d unterminated character.",
                   ctx->unit->name,
                   ctx->line,
                   ctx->col);
    }
    case '\\':
        // special character
        tok->value.c = scan_specch(*(ctx->c + 1));
        ctx->c += 2;
        tok->location.len = 2;
        break;
    default:
        tok->value.c      = *ctx->c;
        tok->location.len = 1;
        ctx->c++;
    }

    // eat '
    if (*ctx->c != '\'') {
        SCAN_ERROR(ERR_UNTERMINATED_STRING,
                   "%s %d:%d unterminated character expected '.",
                   ctx->unit->name,
                   ctx->line,
                   ctx->col);
    }
    ctx->c++;

    return true;
}

INLINE int c_to_number(char c, s32 base)
{
#ifndef _MSC_VER
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#endif
    switch (base) {
    case 16:
        if (c >= 'a' && c <= 'f') {
            return c - 'a' + 10;
        }

        if (c >= 'A' && c <= 'F') {
            return c - 'A' + 10;
        }
    case 10:
        if (c >= '2' && c <= '9') {
            return c - '0';
        }
    case 2:
        if (c == '0' || c == '1') {
            return c - '0';
        }
        break;
    default:
        BL_ABORT("invalid number base");
    }

    return -1;
#ifndef _MSC_VER
#pragma GCC diagnostic pop
#endif
}

bool scan_number(struct context *ctx, struct token *tok)
{
    tok->location.line = ctx->line;
    tok->location.col  = ctx->col;
    tok->value.str     = ctx->c;
    tok->overflow      = false;

    u64 n = 0, prev_n = 0;
    s32 len  = 0;
    s32 base = 10;
    s32 buf  = 0;

    if (strncmp(ctx->c, "0x", 2) == 0) {
        base = 16;
        ctx->c += 2;
        len += 2;
    } else if (strncmp(ctx->c, "0b", 2) == 0) {
        base = 2;
        ctx->c += 2;
        len += 2;
    }

    while (true) {
        if (*(ctx->c) == '.') {

            if (base != 10) {
                SCAN_ERROR(ERR_INVALID_TOKEN,
                           "%s %d:%d invalid suffix.",
                           ctx->unit->name,
                           ctx->line,
                           ctx->col + len);
            }

            len++;
            ctx->c++;
            goto SCAN_DOUBLE;
        }

        buf = c_to_number(*(ctx->c), base);
        if (buf == -1) {
            break;
        }

        prev_n = n;
        n      = n * base + buf;

        len++;
        ctx->c++;
        if (n < prev_n) tok->overflow = true;
    }

    if (len == 0) return false;

    tok->location.len = len;
    ctx->col += len;
    tok->sym     = SYM_NUM;
    tok->value.u = n;
    return true;

SCAN_DOUBLE : {
    u64 e = 1;

    while (true) {
        buf = c_to_number(*(ctx->c), 10);
        if (buf == -1) {
            break;
        }

        prev_n = n;
        n      = n * 10 + buf;
        e *= 10;
        len++;
        ctx->c++;

        if (n < prev_n) tok->overflow = true;
    }

    // valid d. or .d -> minimal 2 characters
    if (len < 2) return false;

    if (*(ctx->c) == 'f') {
        len++;
        ctx->c++;
        tok->sym = SYM_FLOAT;
    } else {
        tok->sym = SYM_DOUBLE;
    }

    tok->value.d = n / (f64)e;
    if (tok->value.d > FLT_MAX) tok->overflow = true;

    tok->location.len = len;
    ctx->col += len;

    return true;
}
}

void scan(struct context *ctx)
{
    struct token tok = {0};
SCAN:
    tok.location.line = ctx->line;
    tok.location.col  = ctx->col;

    // Ignored characters
    switch (*ctx->c) {
    case '\0':
        tok.sym = SYM_EOF;
        tokens_push(ctx->tokens, &tok);
        return;
    case '\r':
        ctx->c++;
        goto SCAN;
    case '\n':
        ctx->line++;
        ctx->col = 1;
        ctx->c++;
        goto SCAN;
    case '\t':
        // TODO: can be set by user
        ctx->col += 4;
        ctx->c++;
        goto SCAN;
    case ' ':
        ctx->col++;
        ctx->c++;
        goto SCAN;
    default:
        break;
    }

    // Scan symbols described directly as strings.
    usize      len                   = 0;
    const bool is_current_char_ident = IS_IDENT(*ctx->c);
    const s32  start                 = is_current_char_ident ? SYM_IF : SYM_UNREACHABLE + 1;
    const s32  end                   = is_current_char_ident ? SYM_UNREACHABLE + 1 : SYM_NONE;
    for (s32 i = start; i < end; ++i) {
        len = sym_lens[i];
        BL_ASSERT(len > 0);
        if (ctx->c[0] == sym_strings[i][0] && strncmp(ctx->c, sym_strings[i], len) == 0) {
            ctx->c += len;
            tok.sym          = (enum sym)i;
            tok.location.len = (s32)len;

            // Two joined symbols will be parsed as identifier.
            if (i >= SYM_IF && i <= SYM_UNREACHABLE && IS_IDENT(*ctx->c)) {
                // roll back
                ctx->c -= len;
                break;
            }

#ifndef _MSC_VER
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#endif
            switch (tok.sym) {
            case SYM_DCOMMENT:
            case SYM_DGCOMMENT:
                if (ctx->assembly && ctx->assembly->target->kind == ASSEMBLY_DOCS) {
                    scan_docs(ctx, &tok);
                    goto PUSH_TOKEN;
                }
            case SYM_SHEBANG:
            case SYM_LCOMMENT:
                scan_comment(ctx, &tok, "\n");
                goto SCAN;
            case SYM_LBCOMMENT:
                scan_comment(ctx, &tok, sym_strings[SYM_RBCOMMENT]);
                goto SCAN;
            case SYM_RBCOMMENT: {
                SCAN_ERROR(ERR_INVALID_TOKEN,
                           "%s %d:%d unexpected token.",
                           ctx->unit->name,
                           ctx->line,
                           ctx->col);
            }
            default:
                ctx->col += (s32)len;
                goto PUSH_TOKEN;
            }
#ifndef _MSC_VER
#pragma GCC diagnostic pop
#endif
        }
    }

    // Scan special tokens.
    if (scan_number(ctx, &tok)) goto PUSH_TOKEN;
    if (scan_ident(ctx, &tok)) goto PUSH_TOKEN;
    if (scan_string(ctx, &tok)) goto PUSH_TOKEN;
    if (scan_char(ctx, &tok)) goto PUSH_TOKEN;

    // When symbol is unknown report error
    SCAN_ERROR(ERR_INVALID_TOKEN,
               "%s %d:%d Unexpected token '%c' (%d)",
               ctx->unit->name,
               ctx->line,
               ctx->col,
               *ctx->c,
               *ctx->c);
PUSH_TOKEN:
    tok.location.unit = ctx->unit;
    tokens_push(ctx->tokens, &tok);

    goto SCAN;
}

void lexer_run(struct assembly *assembly, struct unit *unit)
{
    struct context ctx = {
        .assembly = assembly,
        .tokens   = &unit->tokens,
        .unit     = unit,
        .c        = unit->src,
        .line     = 1,
        .col      = 1,
    };

    ZONE();
    s32 error = 0;
    if ((error = setjmp(ctx.jmp_error))) return;

    scan(&ctx);

    builder.total_lines += ctx.line;
    RETURN_END_ZONE();
}
