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
#include <Shlwapi.h>
#define PATH_MAX MAX_PATH
#ifndef strtok_r
#define strtok_r strtok_s
#endif
#endif

#if BL_COMPILER_CLANG || BL_COMPILER_GNUC
// clang-format off
#define _SHUT_UP_BEGIN \
	_Pragma("GCC diagnostic push") \
	_Pragma("GCC diagnostic ignored \"-Wcast-qual\"") \
	_Pragma("GCC diagnostic ignored \"-Wpedantic\"") \
	_Pragma("GCC diagnostic ignored \"-Wsign-conversion\"")
// clang-format on
#define _SHUT_UP_END _Pragma("GCC diagnostic pop")
#define UNUSED(x) __attribute__((unused)) x

#elif BL_COMPILER_MSVC
#define _SHUT_UP_BEGIN __pragma(warning(push, 0))
#define _SHUT_UP_END __pragma(warning(pop))
#define UNUSED(x) __pragma(warning(suppress : 4100)) x

#else
#error "Unsuported compiler!"
#endif

#define alignment_of(T) _Alignof(T)

#define isflag(_v, _flag) ((bool)(((_v) & (_flag)) == (_flag)))
#define isnotflag(_v, _flag) ((bool)(((_v) & (_flag)) != (_flag)))
#define setflag(_v, _flag) ((_v) |= (_flag))
#define clrflag(_v, _flag) ((_v) &= ~(_flag))
#define setflagif(_v, _flag, _toggle) ((_toggle) ? setflag(_v, _flag) : clrflag(_v, _flag))

enum { BL_RED, BL_BLUE, BL_YELLOW, BL_GREEN, BL_CYAN, BL_NO_COLOR = -1 };

#define runtime_measure_begin(name) f64 __##name = get_tick_ms()
#define runtime_measure_end(name) ((get_tick_ms() - __##name) / 1000.)

#define LIB_NAME_MAX 256

// Return size of of static array.
#define static_arrlenu(A) (sizeof(A) / sizeof((A)[0]))
#define is_str_valid_nonempty(S) ((S) && (S)[0] != '\0')

#ifndef MIN
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef CLAMP
#define CLAMP(v, min, max) ((v) < (min) ? (v) = (min) : ((v) > (max) ? (v) = (max) : (v)))
#endif

// =================================================================================================
// String View
// =================================================================================================

#define make_str(p, l)                                                                             \
	(str_t)                                                                                        \
	{                                                                                              \
		.ptr = (p), .len = (s32)(l)                                                                \
	}

#define make_str_from_c(p)                                                                         \
	(str_t)                                                                                        \
	{                                                                                              \
		.ptr = (p), .len = (s32)strlen(p)                                                          \
	}

#define str_empty                                                                                  \
	(str_t)                                                                                        \
	{                                                                                              \
		0                                                                                          \
	}

// Non-owning string representation with cached size. Note that the string might not be zero
// terminated. This way we can avoid calling strlen() every time and ew can reduce amount of string
// copying and allocations (e.g. identificators can point directly to the loaded file data).
typedef struct {
	char *ptr;
	// Might be s64 but it cause warning in range-prints.
	s32 len;
} str_t;

static_assert(sizeof(str_t) == 16, "Invalid size of string view type.");

bool str_match(str_t a, str_t b);

// =================================================================================================
// String Buffer
// =================================================================================================

#define str_init(A, C) (str_setcap(A, C), (A)[0] = '\0')
#define str_free(A) (arrfree(A))
#define str_lenu(A) (arrlenu(A) ? arrlenu(A) - 1 : 0)
#define str_clr(A) (arrsetlen(A, 1), (A)[0] = '\0')
#define str_setcap(A, C) (arrsetcap(A, (C) + 1))
#define str_append(A, fmt, ...)                                                                    \
	{                                                                                              \
		const usize orig_len = str_lenu(A);                                                        \
		const usize text_len = (usize)snprintf(NULL, 0, fmt, ##__VA_ARGS__) + 1;                   \
		arrsetlen(A, orig_len + text_len);                                                         \
		snprintf((A) + orig_len, text_len, fmt, ##__VA_ARGS__);                                    \
	}                                                                                              \
	(void)0

#define strprint(A, fmt, ...)                                                                      \
	{                                                                                              \
		const usize l = (usize)snprintf(NULL, 0, fmt, ##__VA_ARGS__) + 1;                          \
		arrsetlen(A, l);                                                                           \
		snprintf(A, l, fmt, ##__VA_ARGS__);                                                        \
	}                                                                                              \
	(void)0

char *str_toupper(char *str);
s32   levenshtein(const str_t s1, const str_t s2);

// String dynamic array buffer.
//
// Note it's guaranteed to be zero terminated, however use 'str_to_c' macro for safety!
struct str_buf {
	char *ptr;
	s32   len, cap;
};

typedef struct str_buf str_buf_t;

void str_buf_free(str_buf_t *buf);
// Set the capacity (preallocates) space for 'cap' characters. Does nothing in case the 'cap' is
// smaller than zero. The preallocation includes extra space for zero terminator.
void str_buf_setcap(str_buf_t *buf, s32 cap);
void str_buf_append(str_buf_t *buf, str_t s);
void str_buf_append_fmt(str_buf_t *buf, const char *fmt, ...);
void str_buf_clr(str_buf_t *buf); // This is also setting the zero

static inline const char *_str_to_c_checked(char *ptr, s32 len)
{
	if (!ptr) return "";
	bassert(ptr[len] == '\0' && "String is not zero terminated!");
	return ptr;
}

// Converts the input string or str_t or str_buf_t to the C string. Zero termination is checked by
// assert.
// In case the buffer is not allocated, returns pointer to the static empty C string.
#define str_to_c(B) _str_to_c_checked((B).ptr, (B).len)

#define str_buf_view(B)                                                                            \
	(str_t)                                                                                        \
	{                                                                                              \
		.ptr = (B).ptr, .len = (B).len                                                             \
	}

// =================================================================================================
// STB utils
// =================================================================================================

#define array(T) T *
#define hash_table(T) T *

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
#define sarrpeekor(A, I, D) ((I) < sarrlenu(A) ? sarrdata(A)[I] : (D))
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
typedef sarr_t(s64, 16) ints_t;

// =================================================================================================
// String cache
// =================================================================================================
struct string_cache;

// Allocate string inside the sting cache, a passed cache pointer must be initialized to NULL for
// the first time. The malloc is called only in case there is not enough space left for the string
// inside the preallocated block. Internally, len+1 is allocated to hold zero terminators. When
// 'str' is NULL no data copy is done. Function returns pointer to new allocated block/copy of the
// original string.
char *scdup(struct string_cache **cache, const char *str, usize len);
void  scfree(struct string_cache **cache);
char *scprint(struct string_cache **cache, const char *fmt, ...); // @Cleanup
str_t scprint2(struct string_cache **cache, const char *fmt, ...);

// =================================================================================================
// Hashing
// =================================================================================================
typedef u32 hash_t;

struct id {
	str_t  str;
	hash_t hash;
};

// Reference implementation: https://github.com/haipome/fnv/blob/master/fnv.c
static inline hash_t strhash(const char *str)
{
#if 1
	// FNV 32-bit hash
	hash_t hash = 2166136261;
	char   c;
	while ((c = *str++)) {
		hash = hash ^ (u8)c;
		hash = hash * 16777619;
	}
	return hash;
#else
	hash_t hash = 5381;
	char   c;
	while ((c = *str++))
		hash = ((hash << 5) + hash) + (hash_t)c;
	return hash;
#endif
}

static inline hash_t strhash2(const str_t s)
{
	// FNV 32-bit hash
	hash_t hash = 2166136261;
	char   c;
	for (s32 i = 0; i < s.len; ++i) {
		c    = s.ptr[i];
		hash = hash ^ (u8)c;
		hash = hash * 16777619;
	}
	return hash;
}

static inline hash_t hashcomb(hash_t first, hash_t second)
{
	return first ^ (second + 0x9e3779b9 + (first << 6) + (first >> 2));
}

static inline struct id *id_init(struct id *id, str_t str)
{
	bassert(id);
	id->hash = strhash2(str);
	id->str  = str;
	return id;
}

static inline bool is_ignored_id(struct id *id)
{
	bassert(id);
	if (id->str.len != 1) return false;
	return id->str.ptr[0] == '_';
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

static inline bool is_aligned(const void *p, usize alignment)
{
	return (uintptr_t)p % alignment == 0;
}

static inline void align_ptr_up(void **p, usize alignment, ptrdiff_t *adjustment)
{
	ptrdiff_t adj;
	if (is_aligned(*p, alignment)) {
		if (adjustment) *adjustment = 0;
		return;
	}

	const usize mask = alignment - 1;
	bassert((alignment & mask) == 0 && "Wrong alignment!"); // pwr of 2
	const uintptr_t i_unaligned  = (uintptr_t)(*p);
	const uintptr_t misalignment = i_unaligned & mask;

	adj = alignment - misalignment;
	*p  = (void *)(i_unaligned + adj);
	if (adjustment) *adjustment = adj;
}

static inline void *next_aligned(void *p, usize alignment)
{
	align_ptr_up(&p, alignment, NULL);
	return p;
}

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
bool        set_current_working_dir(const char *path);
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
