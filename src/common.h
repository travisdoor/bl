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

#include "TracyC.h"
#include "bldebug.h"
#include "blmemory.h"
#include "config.h"
#include "error.h"
#include "math.h"
#include <limits.h>
#include <stddef.h>
#include <time.h>
#include <tlib/tlib.h>

struct assembly;
struct scope;

#if BL_PLATFORM_WIN
#include <shlwapi.h>
#define PATH_MAX MAX_PATH
#endif

// =================================================================================================
// Clang and gcc
// =================================================================================================
#if BL_COMPILER_CLANG || BL_COMPILER_GNUC
#define INLINE inline
#define _SHUT_UP_BEGIN
#define _SHUT_UP_END
#define UNUSED(x) __attribute__((unused)) x

// =================================================================================================
// MSVC
// =================================================================================================
#elif BL_COMPILER_MSVC
#define INLINE inline
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

#define IS_FLAG(_v, _flag) ((bool)(((_v) & (_flag)) == (_flag)))
#define IS_NOT_FLAG(_v, _flag) ((bool)(((_v) & (_flag)) != (_flag)))
#define SET_FLAG(_v, _flag) ((_v) |= (_flag))
#define CLR_FLAG(_v, _flag) ((_v) &= ~(_flag))

#define ARRAY_FOREACH(arr, it)                                                                     \
    for (usize _keep = 1, i = 0, _size = static_arrlen((arr)); _keep && i != _size;                  \
         _keep = !_keep, i++)                                                                      \
        for (it = (arr)[i]; _keep; _keep = !_keep)

enum { BL_RED, BL_BLUE, BL_YELLOW, BL_GREEN, BL_CYAN, BL_NO_COLOR = -1 };

#define RUNTIME_MEASURE_BEGIN_S(name) f64 __##name = get_tick_ms()
#define RUNTIME_MEASURE_END_S(name) ((get_tick_ms() - __##name) / 1000.)

#define LIB_NAME_MAX 256

// Return size of of static array.
#define static_arrlen(A) (sizeof(A) / sizeof((A)[0]))

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

// =================================================================================================
// Small Array
// =================================================================================================
#define sarr_t(T, C)                                                                               \
    struct {                                                                                       \
        s32 len;                                                                                   \
        s32 cap;                                                                                   \
        T  *data;                                                                                  \
        T   buf[C];                                                                                \
    }

#define sarradd(A)                                                                                 \
    (sarradd_impl((A), sizeof((A)->buf[0]), sizeof((A)->buf) / sizeof((A)->buf[0])),               \
     &(A)->data[sarrlen(A) - 1])
#define sarrput(A, V) (*sarradd(A) = (V))
#define sarrpop(A) ((A)->data[--(A)->len])
#define sarrlen(A) ((A)->len)
#define sarrfree(A)                                                                                \
    (((A)->cap > 0) ? (bl_free((A)->data), (A)->data = NULL, (A)->len = 0, (A)->cap = 0) : (void)0)

void sarrinit(void *ptr);
void sarradd_impl(void *ptr, s32 elem_size, s32 elem_count);

TSMALL_ARRAY_TYPE(AstPtr, struct ast *, 16);
TSMALL_ARRAY_TYPE(TypePtr, struct mir_type *, 16);
TSMALL_ARRAY_TYPE(MemberPtr, struct mir_member *, 16);
TSMALL_ARRAY_TYPE(VariantPtr, struct mir_variant *, 16);
TSMALL_ARRAY_TYPE(ArgPtr, struct mir_arg *, 16);
TSMALL_ARRAY_TYPE(InstrPtr, struct mir_instr *, 16);
TSMALL_ARRAY_TYPE(ConstValuePtr, struct MirConstValue *, 16);
TSMALL_ARRAY_TYPE(Char, char, 128);
TSMALL_ARRAY_TYPE(CharPtr, char *, 8);
TSMALL_ARRAY_TYPE(FnPtr, struct mir_fn *, 8);

// =================================================================================================
// Hashing
// =================================================================================================
typedef u32 hash_t;

struct id {
    const char *str;
    hash_t      hash;
};

static INLINE hash_t strhash(const char *str)
{
    hash_t hash = 5381;
    s32    c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash;
}

static INLINE struct id *id_init(struct id *id, const char *str)
{
    BL_ASSERT(id);
    id->hash = strhash(str);
    id->str  = str;
    return id;
}

static INLINE bool is_ignored_id(const struct id *id)
{
    BL_ASSERT(id);
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
//     2) LIB_DIR specified in global congig file
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
void  win_path_to_unix(char *buf, usize buf_size);
void  unix_path_to_win(char *buf, usize buf_size);
bool  file_exists(const char *filepath);
bool  dir_exists(const char *dirpath);
bool  brealpath(const char *file, char *out, s32 out_len);
bool  get_current_working_dir(char *buf, usize buf_size);
bool  get_dir_from_filepath(char *buf, const usize l, const char *filepath);
bool  get_filename_from_filepath(char *buf, const usize l, const char *filepath);
bool  get_current_exec_path(char *buf, usize buf_size);
bool  get_current_exec_dir(char *buf, usize buf_size);
bool  create_dir(const char *dirpath);
bool  create_dir_tree(const char *dirpath);
bool  copy_dir(const char *src, const char *dest);
bool  copy_file(const char *src, const char *dest);
bool  remove_dir(const char *path);
void  date_time(char *buf, s32 len, const char *format);
bool  is_aligned(const void *p, usize alignment);
void  align_ptr_up(void **p, usize alignment, ptrdiff_t *adjustment);
void  print_bits(s32 const size, void const *const ptr);
int   count_bits(u64 n);
void  platform_lib_name(const char *name, char *buffer, usize max_len);
f64   get_tick_ms(void);
s32   get_last_error(char *buf, s32 buf_len);
void *_create_sarr(struct assembly *ctx, usize arr_size);
u32   next_pow_2(u32 n);
void  color_print(FILE *stream, s32 color, const char *format, ...);
s32   cpu_thread_count(void);
#define create_sarr(T, Asm) ((T *)_create_sarr((Asm), sizeof(T)))

#endif
