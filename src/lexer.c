//************************************************************************************************
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
//************************************************************************************************

#include "common.h"
#include "stages.h"
#include <float.h>
#include <setjmp.h>
#include <string.h>

#define IS_IDENT(c)                                                                                \
    (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || ((c) >= '0' && (c) <= '9') ||     \
     (c) == '_')

#define SCAN_ERROR(code, format, ...)                                                              \
    {                                                                                              \
        builder_msg(BUILDER_MSG_ERROR, (code), NULL, BUILDER_CUR_NONE, (format), ##__VA_ARGS__);   \
        longjmp((cnt)->jmp_error, code);                                                           \
    }

typedef struct context {
    Unit *  unit;
    Tokens *tokens;
    jmp_buf jmp_error;
    char *  c;
    s32     line;
    s32     col;
} Context;

static void       scan(Context *cnt);
static bool       scan_comment(Context *cnt, Token *tok, const char *term);
static bool       scan_docs(Context *cnt, Token *tok);
static bool       scan_ident(Context *cnt, Token *tok);
static bool       scan_string(Context *cnt, Token *tok);
static bool       scan_char(Context *cnt, Token *tok);
static bool       scan_number(Context *cnt, Token *tok);
static INLINE int c_to_number(char c, s32 base);
static char       scan_specch(char c);

bool scan_comment(Context *cnt, Token *tok, const char *term)
{
    if (tok->sym == SYM_SHEBANG && cnt->line != 1) {
        // Unterminated comment
        SCAN_ERROR(ERR_INVALID_TOKEN,
                   "%s %d:%d Shebang is allowed only on the first line of the file.",
                   cnt->unit->name,
                   cnt->line,
                   cnt->col);
    }
    const usize len = strlen(term);
    while (true) {
        if (*cnt->c == '\n') {
            cnt->line++;
            cnt->col = 1;
        } else if (*cnt->c == '\0' && strcmp(term, "\n") != 0) {
            // Unterminated comment
            SCAN_ERROR(ERR_UNTERMINATED_COMMENT,
                       "%s %d:%d unterminated comment block.",
                       cnt->unit->name,
                       cnt->line,
                       cnt->col);
        }
        if (*cnt->c == SYM_EOF) return true;
        if (strncmp(cnt->c, term, len) == 0) break;
        cnt->c++;
    }
    cnt->c += len;
    return true;
}

bool scan_docs(Context *cnt, Token *tok)
{
    BL_ASSERT(token_is(tok, SYM_DCOMMENT) || token_is(tok, SYM_DGCOMMENT));
    tok->location.line = cnt->line;
    tok->location.col  = cnt->col;

    char *begin      = cnt->c;
    s32   len_parsed = 0;
    s32   len_str    = 0;
    while (true) {
        if (*cnt->c == SYM_EOF) break;
        if (*cnt->c == '\r' || *cnt->c == '\n') break;
        if (!len_parsed && *cnt->c == ' ') {
            ++begin;
        } else {
            len_str++;
        }
        len_parsed++;
        cnt->c++;
    }

    TString *cstr = builder_create_cached_str();
    tstring_append_n(cstr, begin, len_str);
    tok->value.str    = cstr->data;
    tok->location.len = len_parsed + 3; // + 3 = '///'
    cnt->col += len_parsed;
    return true;
}

bool scan_ident(Context *cnt, Token *tok)
{
    tok->location.line = cnt->line;
    tok->location.col  = cnt->col;
    tok->sym           = SYM_IDENT;

    char *begin = cnt->c;

    s32 len = 0;
    while (true) {
        if (!IS_IDENT(*cnt->c)) {
            break;
        }

        len++;
        cnt->c++;
    }

    if (len == 0) return false;
    TString *cstr = builder_create_cached_str();
    tstring_append_n(cstr, begin, len);
    tok->value.str    = cstr->data;
    tok->location.len = len;
    cnt->col += len;
    return true;
}

char scan_specch(char c)
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

bool scan_string(Context *cnt, Token *tok)
{
    if (*cnt->c != '"') return false;
    tok->location.line = cnt->line;
    tok->location.col  = cnt->col;
    tok->sym           = SYM_STRING;
    TString *cstr      = builder_create_cached_str();
    char     c;
    s32      len = 0;

    // eat "
    cnt->c++;
    while (true) {
        switch (*cnt->c) {
        case '"': {
            cnt->c++;
            goto DONE;
        }
        case '\0': {
            SCAN_ERROR(ERR_UNTERMINATED_STRING,
                       "%s %d:%d unterminated string.",
                       cnt->unit->name,
                       cnt->line,
                       cnt->col);
        }
        case '\\':
            // special character

            // @INCOMPLETE: this can fail!!!
            // @INCOMPLETE: this can fail!!!
            // @INCOMPLETE: this can fail!!!
            c = scan_specch(*(cnt->c + 1));
            cnt->c += 2;
            len += 2;
            break;
        default:
            c = *cnt->c;
            len++;
            cnt->c++;
        }
        tstring_append_n(cstr, &c, 1);
    }
DONE:
    tok->value.str    = cstr->data;
    tok->location.len = len;
    tok->location.col += 1;
    cnt->col += len + 2;
    return true;
}

bool scan_char(Context *cnt, Token *tok)
{
    if (*cnt->c != '\'') return false;
    tok->location.line = cnt->line;
    tok->location.col  = cnt->col;
    tok->location.len  = 0;
    tok->sym           = SYM_CHAR;

    // eat '
    cnt->c++;

    switch (*cnt->c) {
    case '\'': {
        SCAN_ERROR(
            ERR_EMPTY, "%s %d:%d expected character in ''.", cnt->unit->name, cnt->line, cnt->col);
    }
    case '\0': {
        SCAN_ERROR(ERR_UNTERMINATED_STRING,
                   "%s %d:%d unterminated character.",
                   cnt->unit->name,
                   cnt->line,
                   cnt->col);
    }
    case '\\':
        // special character
        tok->value.c = scan_specch(*(cnt->c + 1));
        cnt->c += 2;
        tok->location.len = 2;
        break;
    default:
        tok->value.c      = *cnt->c;
        tok->location.len = 1;
        cnt->c++;
    }

    // eat '
    if (*cnt->c != '\'') {
        SCAN_ERROR(ERR_UNTERMINATED_STRING,
                   "%s %d:%d unterminated character expected '.",
                   cnt->unit->name,
                   cnt->line,
                   cnt->col);
    }
    cnt->c++;

    return true;
}

int c_to_number(char c, s32 base)
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

bool scan_number(Context *cnt, Token *tok)
{
    tok->location.line = cnt->line;
    tok->location.col  = cnt->col;
    tok->value.str     = cnt->c;
    tok->overflow      = false;

    u64 n = 0, prev_n = 0;
    s32 len  = 0;
    s32 base = 10;
    s32 buf  = 0;

    if (strncmp(cnt->c, "0x", 2) == 0) {
        base = 16;
        cnt->c += 2;
        len += 2;
    } else if (strncmp(cnt->c, "0b", 2) == 0) {
        base = 2;
        cnt->c += 2;
        len += 2;
    }

    while (true) {
        if (*(cnt->c) == '.') {

            if (base != 10) {
                SCAN_ERROR(ERR_INVALID_TOKEN,
                           "%s %d:%d invalid suffix.",
                           cnt->unit->name,
                           cnt->line,
                           cnt->col + len);
            }

            len++;
            cnt->c++;
            goto SCAN_DOUBLE;
        }

        buf = c_to_number(*(cnt->c), base);
        if (buf == -1) {
            break;
        }

        prev_n = n;
        n      = n * base + buf;

        len++;
        cnt->c++;
        if (n < prev_n) tok->overflow = true;
    }

    if (len == 0) return false;

    tok->location.len = len;
    cnt->col += len;
    tok->sym     = SYM_NUM;
    tok->value.u = n;
    return true;

SCAN_DOUBLE : {
    u64 e = 1;

    while (true) {
        buf = c_to_number(*(cnt->c), 10);
        if (buf == -1) {
            break;
        }

        prev_n = n;
        n      = n * 10 + buf;
        e *= 10;
        len++;
        cnt->c++;

        if (n < prev_n) tok->overflow = true;
    }

    // valid d. or .d -> minimal 2 characters
    if (len < 2) return false;

    if (*(cnt->c) == 'f') {
        len++;
        cnt->c++;
        tok->sym = SYM_FLOAT;
    } else {
        tok->sym = SYM_DOUBLE;
    }

    tok->value.d = n / (f64)e;
    if (tok->value.d > FLT_MAX) tok->overflow = true;

    tok->location.len = len;
    cnt->col += len;

    return true;
}
}

void scan(Context *cnt)
{
    Token tok = {0};
SCAN:
    tok.location.line = cnt->line;
    tok.location.col  = cnt->col;

    // Ignored characters
    switch (*cnt->c) {
    case '\0':
        tok.sym = SYM_EOF;
        tokens_push(cnt->tokens, &tok);
        return;
    case '\r':
        cnt->c++;
        goto SCAN;
    case '\n':
        cnt->line++;
        cnt->col = 1;
        cnt->c++;
        goto SCAN;
    case '\t':
        // TODO: can be set by user
        cnt->col += 4;
        cnt->c++;
        goto SCAN;
    case ' ':
        cnt->col++;
        cnt->c++;
        goto SCAN;
    default:
        break;
    }

    // Scan symbols described directly as strings.
    usize len = 0;
    for (s32 i = SYM_IF; i < SYM_NONE; ++i) {
        len = strlen(sym_strings[i]);
        if (strncmp(cnt->c, sym_strings[i], len) == 0) {
            cnt->c += len;
            tok.sym          = (Sym)i;
            tok.location.len = (s32)len;

            // Two joined symbols will be parsed as identifier.
            if (i >= SYM_IF && i <= SYM_UNREACHABLE && IS_IDENT(*cnt->c)) {
                // roll back
                cnt->c -= len;
                break;
            }

            switch (tok.sym) {
            case SYM_DCOMMENT:
            case SYM_DGCOMMENT:
                scan_docs(cnt, &tok);
                goto PUSH_TOKEN;
            case SYM_SHEBANG:
            case SYM_LCOMMENT:
                scan_comment(cnt, &tok, "\n");
                goto SCAN;
            case SYM_LBCOMMENT:
                scan_comment(cnt, &tok, sym_strings[SYM_RBCOMMENT]);
                goto SCAN;
            case SYM_RBCOMMENT: {
                SCAN_ERROR(ERR_INVALID_TOKEN,
                           "%s %d:%d unexpected token.",
                           cnt->unit->name,
                           cnt->line,
                           cnt->col);
            }
            default:
                cnt->col += (s32)len;
                goto PUSH_TOKEN;
            }
        }
    }

    // Scan special tokens.
    if (scan_number(cnt, &tok)) goto PUSH_TOKEN;
    if (scan_ident(cnt, &tok)) goto PUSH_TOKEN;
    if (scan_string(cnt, &tok)) goto PUSH_TOKEN;
    if (scan_char(cnt, &tok)) goto PUSH_TOKEN;

    // When symbol is unknown report error
    SCAN_ERROR(ERR_INVALID_TOKEN,
               "%s %d:%d Unexpected token '%c' (%d)",
               cnt->unit->name,
               cnt->line,
               cnt->col,
               *cnt->c,
               *cnt->c);
PUSH_TOKEN:
    tok.location.unit = cnt->unit;
    tokens_push(cnt->tokens, &tok);

    goto SCAN;
}

void lexer_run(Unit *unit)
{
    Context cnt = {
        .tokens = &unit->tokens,
        .unit   = unit,
        .c      = unit->src,
        .line   = 1,
        .col    = 1,
    };

    TracyCZone(_tctx, true);
    s32 error = 0;
    if ((error = setjmp(cnt.jmp_error))) return;

    scan(&cnt);

    builder.total_lines += cnt.line;
    TracyCZoneEnd(_tctx);
}
