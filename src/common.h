// =================================================================================================
// bl
//
// File:   common.h
// Author: Martin Dorazil
// Date:   03/03/2018
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

#ifndef BL_COMMON_H
#define BL_COMMON_H

// clang-format off
#include "blmemory.h"
// clang-format on
#include "TracyC.h"
#include "basic_types.h"
#include "bldebug.h"
#include "config.h"
#include "error.h"
#include "math.h"
#include <limits.h>
#include <stddef.h>
#include <string.h>
#include <time.h>

struct assembly;
struct scope;
struct target;
struct config;

#if BL_PLATFORM_WIN
#include <shlwapi.h>
#define PATH_MAX MAX_PATH
#endif

// =================================================================================================
// Clang and gcc
// =================================================================================================
#if BL_COMPILER_CLANG || BL_COMPILER_GNUC
#define INLINE inline
#ifndef FORCEINLINE
#define FORCEINLINE inline
#endif
#define _SHUT_UP_BEGIN
#define _SHUT_UP_END
#define UNUSED(x) __attribute__((unused)) x

// =================================================================================================
// MSVC
// =================================================================================================
#elif BL_COMPILER_MSVC
#define INLINE __inline
#define FORCEINLINE __forceinline
#define _SHUT_UP_BEGIN __pragma(warning(push, 0))
#define _SHUT_UP_END __pragma(warning(pop))
#define UNUSED(x) __pragma(warning(suppress : 4100)) x

// =================================================================================================
// Other
// =================================================================================================
#else
#error "Unsuported compiler!"
#endif

#define alignment_of(T) _Alignof(T)

#define isflag(_v, _flag) ((bool)(((_v) & (_flag)) == (_flag)))
#define isnotflag(_v, _flag) ((bool)(((_v) & (_flag)) != (_flag)))
#define setflag(_v, _flag) ((_v) |= (_flag))
#define clrflag(_v, _flag) ((_v) &= ~(_flag))

enum { BL_RED, BL_BLUE, BL_YELLOW, BL_GREEN, BL_CYAN, BL_NO_COLOR = -1 };

#define runtime_measure_begin(name) f64 __##name = get_tick_ms()
#define runtime_measure_end(name) ((get_tick_ms() - __##name) / 1000.)

#define LIB_NAME_MAX 256

// Return size of of static array.
#define static_arrlenu(A) (sizeof(A) / sizeof((A)[0]))

// =================================================================================================
// STB utils
// =================================================================================================

#define queue_t(T)                                                                                 \
    struct {                                                                                       \
        T  *q[2];                                                                                  \
        s64 i;                                                                                     \
        s32 qi;                                                                                    \
    }

#define _qcurrent(Q) ((Q)->q[(Q)->qi])
#define _qother(Q) ((Q)->q[(Q)->qi ^ 1])
#define qmaybeswap(Q)                                                                              \
    ((Q)->i >= arrlen(_qcurrent(Q))                                                                \
         ? (arrsetlen(_qcurrent(Q), 0), (Q)->qi ^= 1, (Q)->i = 0, arrlen(_qcurrent(Q)) > 0)        \
         : (true))
#define qfree(Q) (arrfree((Q)->q[0]), arrfree((Q)->q[1]))
#define qpush_back(Q, V) arrput(_qother(Q), (V))
#define qpop_front(Q) (_qcurrent(Q)[(Q)->i++])
#define qsetcap(Q, c) (arrsetcap(_qother(Q), c))

#define strprint(A, fmt, ...)                                                                      \
    {                                                                                              \
        const s32 l = snprintf(NULL, 0, fmt, ##__VA_ARGS__) + 1;                                   \
        arrsetlen(A, l);                                                                           \
        snprintf(A, l, fmt, ##__VA_ARGS__);                                                        \
    }                                                                                              \
    (void)0

#define strinit(A, C) (strsetcap(A, C), (A)[0] = '\0')
#define strfree(A) (arrfree(A))
#define strlenu(A) (arrlenu(A) ? arrlenu(A) - 1 : 0)
#define strclr(A) (arrsetlen(A, 1), (A)[0] = '\0')
#define strsetcap(A, C) (arrsetcap(A, (C) + 1))
#define strappend(A, fmt, ...)                                                                     \
    {                                                                                              \
        const usize orig_len = strlenu(A);                                                         \
        const s32   text_len = snprintf(NULL, 0, fmt, ##__VA_ARGS__) + 1;                          \
        arrsetlen(A, orig_len + text_len);                                                         \
        snprintf((A) + orig_len, text_len, fmt, ##__VA_ARGS__);                                    \
    }                                                                                              \
    (void)0

// =================================================================================================
// Small Array
// =================================================================================================
#define sarr_t(T, C)                                                                               \
    struct {                                                                                       \
        u32 len, cap;                                                                              \
        union {                                                                                    \
            T *_data;                                                                              \
            T  _buf[C];                                                                            \
        };                                                                                         \
    }

typedef sarr_t(u8, 1) sarr_any_t;

#define SARR_ZERO                                                                                  \
    {                                                                                              \
        .len = 0, .cap = 0                                                                         \
    }

#define sarradd(A) sarraddn(A, 1)
#define sarraddn(A, N)                                                                             \
    (sarradd_impl((A), sizeof((A)->_buf[0]), sizeof((A)->_buf) / sizeof((A)->_buf[0]), N),         \
     &sarrpeek(A, sarrlenu(A) - N))
#define sarrput(A, V) (*sarradd(A) = (V))
#define sarrpop(A) (sarrlenu(A) > 0 ? sarrpeek(A, --(A)->len) : (abort(), (A)->_data[0]))
#define sarrlenu(A) ((usize)((A) ? (A)->len : 0))
#define sarrlen(A) ((s64)((A) ? (A)->len : 0))
#define sarrdata(A) ((A)->cap ? ((A)->_data) : ((A)->_buf))
#define sarrpeek(A, I) (sarrdata(A)[I])
#define sarrclear(A) ((A)->len = 0)
#define sarrfree(A) ((A)->cap ? bfree((A)->_data) : (void)0, (A)->len = 0, (A)->cap = 0)
#define sarrsetlen(A, L)                                                                           \
    {                                                                                              \
        const s64 d = ((s64)(L)) - sarrlen(A);                                                     \
        if (d) sarraddn(A, d);                                                                     \
    }                                                                                              \
    (void)0

void sarradd_impl(void *ptr, usize elem_size, usize static_elem_count, usize new_elem_count);

typedef sarr_t(struct ast *, 16) ast_nodes_t;
typedef sarr_t(struct mir_arg *, 16) mir_args_t;
typedef sarr_t(struct mir_fn *, 16) mir_fns_t;
typedef sarr_t(struct mir_type *, 16) mir_types_t;
typedef sarr_t(struct mir_member *, 16) mir_members_t;
typedef sarr_t(struct mir_variant *, 16) mir_variants_t;
typedef sarr_t(struct mir_instr *, 16) mir_instrs_t;

// =================================================================================================
// String cache
// =================================================================================================
struct string_cache;

// Allocate string inside the sting cache, passed cache pointer must be initialized to NULL for the
// first time. The malloc is called only in case there is not enough space left for the string
// inside the preallocated block. Internally len+1 is allocated to hold zero terminator. When 'str'
// is NULL no data copy is done. Function returns pointer to new allocated block/copy of the
// original string.
char *scdup(struct string_cache **cache, const char *str, usize len);
void  scfree(struct string_cache **cache);
char *scprint(struct string_cache **cache, const char *fmt, ...);

// =================================================================================================
// Hashing
// =================================================================================================
typedef u32 hash_t;

struct id {
    const char *str;
    hash_t      hash;
};

static FORCEINLINE hash_t strhash(const char *str)
{
    hash_t hash = 5381;
    s32    c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash;
}

static FORCEINLINE hash_t hashcomb(hash_t first, hash_t second)
{
    return first ^ (second + 0x9e3779b9 + (first << 6) + (first >> 2));
}

static FORCEINLINE struct id *id_init(struct id *id, const char *str)
{
    bassert(id);
    id->hash = strhash(str);
    id->str  = str;
    return id;
}

static FORCEINLINE bool is_ignored_id(const struct id *id)
{
    bassert(id);
    return strcmp(id->str, "_") == 0;
}

// =================================================================================================
// Utils
// =================================================================================================
enum search_flags {
    SEARCH_FLAG_ABS         = 0,
    SEARCH_FLAG_WDIR        = 1,
    SEARCH_FLAG_LIB_DIR     = 2,
    SEARCH_FLAG_SYSTEM_PATH = 4,
    SEARCH_FLAG_ALL         = SEARCH_FLAG_WDIR | SEARCH_FLAG_LIB_DIR | SEARCH_FLAG_SYSTEM_PATH,
};

// Search file specified by 'filepath' and sets output filepath (full file location) and output
// directory path (path without file name).
//
// Search order:
//     1) exec_dir (working directory if not NULL)
//     2) LIB_DIR specified in global config file
//     3) system PATH
//
// Function returns true and modify output variables if file was found otherwise returns false.
bool search_source_file(const char *filepath,
                        const u32   flags,
                        const char *wdir,
                        char      **out_filepath,
                        char      **out_dirpath);

// Replace all backslashes in passed path with forward slash, this is used as workaround on Windows
// platform due to inconsistency 'Unix vs Windows' path separators. This function will modify passed
// buffer.
void        win_path_to_unix(char *buf, usize buf_size);
void        unix_path_to_win(char *buf, usize buf_size);
bool        file_exists(const char *filepath);
bool        dir_exists(const char *dirpath);
bool        normalize_path(char **path);
bool        brealpath(const char *file, char *out, s32 out_len);
bool        get_current_working_dir(char *buf, usize buf_size);
bool        get_dir_from_filepath(char *buf, const usize l, const char *filepath);
bool        get_filename_from_filepath(char *buf, const usize l, const char *filepath);
bool        get_current_exec_path(char *buf, usize buf_size);
bool        get_current_exec_dir(char *buf, usize buf_size);
bool        create_dir(const char *dirpath);
bool        create_dir_tree(const char *dirpath);
bool        copy_dir(const char *src, const char *dest);
bool        copy_file(const char *src, const char *dest);
bool        remove_dir(const char *path);
void        date_time(char *buf, s32 len, const char *format);
bool        is_aligned(const void *p, usize alignment);
void        align_ptr_up(void **p, usize alignment, ptrdiff_t *adjustment);
void       *next_aligned(void *p, usize alignment);
void        print_bits(s32 const size, void const *const ptr);
int         count_bits(u64 n);
void        platform_lib_name(const char *name, char *buffer, usize max_len);
f64         get_tick_ms(void);
s32         get_last_error(char *buf, s32 buf_len);
u32         next_pow_2(u32 n);
void        color_print(FILE *stream, s32 color, const char *format, ...);
s32         cpu_thread_count(void);
char       *execute(const char *cmd);
const char *read_config(struct config       *config,
                        const struct target *target,
                        const char          *path,
                        const char          *default_value);

typedef void (*process_tokens_fn_t)(void *ctx, const char *token);
s32   process_tokens(void *ctx, const char *input, const char *delimiter, process_tokens_fn_t fn);
char *strtrim(char *str);

#endif
