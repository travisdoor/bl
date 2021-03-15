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
#include <limits.h>
#include <stddef.h>
#include <tlib/tlib.h>

struct Assembly;

// =================================================================================================
// Clang and gcc
// =================================================================================================
#if BL_COMPILER_CLANG || BL_COMPILER_GNUC
#define BL_DEPRECATED __attribute__((deprecated))
//#define INLINE __attribute__((always_inline))
#define INLINE inline
#define _SHUT_UP_BEGIN
#define _SHUT_UP_END
#define UNUSED(x) __attribute__((unused)) x

// =================================================================================================
// MSVC
// =================================================================================================
#elif BL_COMPILER_MSVC
#pragma warning(disable : 4002)
#pragma warning(disable : 6011)
#pragma warning(disable : 4013)
#pragma warning(disable : 4244)
#pragma warning(disable : 6001)
#pragma warning(disable : 4267)
#pragma warning(disable : 4200)
#pragma warning(disable : 4204)
#pragma warning(disable : 4706)

#define BL_DEPRECATED
#if BL_DEBUG
#define INLINE inline
#else
#define INLINE __forceinline
#endif
#define _SHUT_UP_BEGIN __pragma(warning(push, 0))
#define _SHUT_UP_END __pragma(warning(pop))
#define UNUSED(x) __pragma(warning(suppress : 4100)) x

// =================================================================================================
// Other
// =================================================================================================
#else
#error "Unsuported compiler!"
#endif

#define IS_FLAG(_v, _flag) ((bool)((_v & _flag) == _flag))
#define IS_NOT_FLAG(_v, _flag) ((bool)((_v & _flag) != _flag))

#define ARRAY_FOREACH(arr, it)                                                                     \
    for (usize _keep = 1, i = 0, _size = TARRAY_SIZE((arr)); _keep && i != _size;                  \
         _keep = !_keep, i++)                                                                      \
        for (it = (arr)[i]; _keep; _keep = !_keep)

#define BL_RED 1
#define BL_BLUE 2
#define BL_YELLOW 3
#define BL_GREEN 4
#define BL_NO_COLOR -1

#define LIB_NAME_MAX 256

TSMALL_ARRAY_TYPE(AstPtr, struct Ast *, 16);
TSMALL_ARRAY_TYPE(TypePtr, struct MirType *, 16);
TSMALL_ARRAY_TYPE(MemberPtr, struct MirMember *, 16);
TSMALL_ARRAY_TYPE(VariantPtr, struct MirVariant *, 16);
TSMALL_ARRAY_TYPE(ArgPtr, struct MirArg *, 16);
TSMALL_ARRAY_TYPE(InstrPtr, struct MirInstr *, 16);
TSMALL_ARRAY_TYPE(ConstValuePtr, struct MirConstValue *, 16);
TSMALL_ARRAY_TYPE(Char, char, 128);
TSMALL_ARRAY_TYPE(FnPtr, struct MirFn *, 8);
TSMALL_ARRAY_TYPE(Scope, struct Scope *, 4);

typedef struct ID {
    const char *str;
    u64         hash;
} ID;

typedef enum {
    SEARCH_FLAG_ABS         = 0,
    SEARCH_FLAG_WDIR        = 1,
    SEARCH_FLAG_LIB_DIR     = 2,
    SEARCH_FLAG_SYSTEM_PATH = 4,
    SEARCH_FLAG_ALL         = SEARCH_FLAG_WDIR | SEARCH_FLAG_LIB_DIR | SEARCH_FLAG_SYSTEM_PATH,
} SearchFlags;

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
                        char **     out_filepath,
                        char **     out_dirpath);

// Replace all backslashes in passed path with forward slash, this is used as workaround on Windows
// platform due to inconsistency 'Unix vs Windows' path separators. This function will modify passed
// buffer.
void win_path_to_unix(char *buf, usize buf_size);
void unix_path_to_win(char *buf, usize buf_size);
bool file_exists(const char *filepath);
bool dir_exists(const char *dirpath);
bool brealpath(const char *file, char *out, s32 out_len);
bool get_current_working_dir(char *buf, usize buf_size);
bool get_dir_from_filepath(char *buf, const usize l, const char *filepath);
bool get_filename_from_filepath(char *buf, const usize l, const char *filepath);
bool get_current_exec_path(char *buf, usize buf_size);
bool get_current_exec_dir(char *buf, usize buf_size);
bool create_dir(const char *dirpath);
bool create_dir_tree(const char *dirpath);
bool copy_dir(const char *src, const char *dest);
bool copy_file(const char *src, const char *dest);
bool remove_dir(const char *path);
void date_time(char *buf, s32 len, const char *format);
bool is_aligned(const void *p, usize alignment);
void align_ptr_up(void **p, usize alignment, ptrdiff_t *adjustment);
void print_bits(s32 const size, void const *const ptr);
int  count_bits(u64 n);
void platform_lib_name(const char *name, char *buffer, usize max_len);
f64  get_tick_ms(void);

// Creates TArray inside Assembly arena.
// Note: no free is needed.
TArray *create_arr(struct Assembly *assembly, usize size);

// Creates SmallArray inside Assembly arena.
// Note: no free is needed.
void *_create_sarr(struct Assembly *cnt, usize arr_size);
u32   next_pow_2(u32 n);
void  color_print(FILE *stream, s32 color, const char *format, ...);
#define create_sarr(T, Asm) ((T *)_create_sarr((Asm), sizeof(T)))

#endif
