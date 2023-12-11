// =================================================================================================
// bl
//
// File:   common.c
// Author: Martin Dorazil
// Date:   11.8.18
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

// =================================================================================================
// STB
// =================================================================================================
#include "bldebug.h"
#include "blmemory.h"
#include "conf.h"
#define STB_DS_IMPLEMENTATION
#define STBDS_REALLOC(context, ptr, size) brealloc(ptr, size)
#define STBDS_FREE(context, ptr) bfree(ptr)
#include "stb_ds.h"

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include <stdarg.h>
#include <stdio.h>
#include <time.h>

#ifdef BL_USE_SIMD
#	include <emmintrin.h>
#	include <intrin.h>
#	include <nmmintrin.h>
#endif

#if !BL_PLATFORM_WIN
#	include <sys/stat.h>
#	include <unistd.h>
#endif

#if BL_PLATFORM_MACOS
#	include <ctype.h>
#	include <errno.h>
#	include <mach-o/dyld.h>
#	include <mach/mach_time.h>
#	include <sys/time.h>
#endif

#if BL_PLATFORM_LINUX
#	include <ctype.h>
#	include <errno.h>
#endif

#if BL_PLATFORM_WIN
#	include <shlwapi.h>
#	define popen _popen
#	define pclose _pclose
#endif

u64 main_thread_id = 0;
// =================================================================================================
// PUBLIC
// =================================================================================================

// =================================================================================================
// Small Array
// =================================================================================================

void sarradd_impl(void *ptr, usize elem_size, usize static_elem_count, usize new_elem_count) {
	if (new_elem_count < 1) return;
	sarr_any_t *arr        = (sarr_any_t *)ptr;
	const bool  on_heap    = arr->cap;
	const usize needed_len = arr->len + new_elem_count;
	if (on_heap && needed_len > arr->cap) {
		arr->cap  = (u32)(arr->cap * 2 > needed_len ? arr->cap * 2 : needed_len);
		void *tmp = arr->_data;
		if ((arr->_data = brealloc(arr->_data, arr->cap * elem_size)) == NULL) {
			bfree(tmp);
			abort();
		}
	} else if (!on_heap && needed_len > static_elem_count) {
		arr->cap   = (u32)(static_elem_count * 2 > needed_len ? static_elem_count * 2 : needed_len);
		void *data = bmalloc(arr->cap * elem_size);
		memcpy(data, arr->_buf, elem_size * arr->len);
		arr->_data = data;
	}
	arr->len += (u32)new_elem_count;
}

// =================================================================================================
// String cache
// =================================================================================================
#define SC_BLOCK_BYTES 2048

struct string_cache {
	u32 len;
	u32 cap;

	struct string_cache *prev;
};

static struct string_cache *new_block(usize len, struct string_cache *prev) {
	zone();
	const usize          cap   = len > SC_BLOCK_BYTES ? len : SC_BLOCK_BYTES;
	struct string_cache *cache = bmalloc(sizeof(struct string_cache) + cap);
	cache->cap                 = (u32)cap;
	cache->prev                = prev;
	cache->len                 = 0;
	return_zone(cache);
}

char *scdup(struct string_cache **cache, const char *str, usize len) {
	zone();
	len += 1; // +zero terminator
	if (!*cache) {
		(*cache) = new_block(len, NULL);
	} else if ((*cache)->len + len >= (*cache)->cap) {
		(*cache) = new_block(len, *cache);
	}
	char *mem = ((char *)((*cache) + 1)) + (*cache)->len;
	if (str) {
		memcpy(mem, str, len - 1); // Do not copy zero terminator.
		mem[len - 1] = '\0';       // Set zero terminator.
	}
	(*cache)->len += (u32)len;
	return_zone(mem);
}

str_t _scdup2(struct string_cache **cache, char *str, s32 len) {
	len += 1; // +zero terminator
	if (!*cache) {
		(*cache) = new_block(len, NULL);
	} else if ((*cache)->len + len >= (*cache)->cap) {
		(*cache) = new_block(len, *cache);
	}
	char *mem = ((char *)((*cache) + 1)) + (*cache)->len;
	if (str) {
		memcpy(mem, str, len - 1); // Do not copy zero terminator.
		mem[len - 1] = '\0';       // Set zero terminator.
	}
	(*cache)->len += (u32)len;
	return make_str(mem, len - 1); // -1 zero terminator!
}

void scfree(struct string_cache **cache) {
	struct string_cache *c = (*cache);
	while (c) {
		struct string_cache *prev = c->prev;
		bfree(c);
		c = prev;
	}
	(*cache) = NULL;
}

str_t scprint(struct string_cache **cache, const char *fmt, ...) {
	va_list args, args2;
	va_start(args, fmt);
	va_copy(args2, args);
	const s32 len = bvsnprint(NULL, 0, fmt, args);
	bassert(len > 0);
	char     *buf  = scdup(cache, NULL, len);
	const s32 wlen = bvsnprint(buf, len, fmt, args2);
	bassert(wlen == len);
	(void)wlen;
	va_end(args2);
	va_end(args);
	return make_str(buf, len);
}

// =================================================================================================
// String Buffer
// =================================================================================================

void str_buf_free(str_buf_t *buf) {
	bfree(buf->ptr);
	buf->cap = 0;
	buf->len = 0;
	buf->ptr = NULL;
}

void str_buf_clr(str_buf_t *buf) {
	buf->len = 0;
	if (buf->ptr && buf->cap) buf->ptr[0] = '\0';
}

void str_buf_setcap(str_buf_t *buf, s32 cap) {
	if (cap < 1) return;
	cap += 1; // This is for zero terminator.
	if (buf->cap >= cap) return;

	buf->ptr = brealloc(buf->ptr, MAX(cap, 64));
	buf->cap = cap;
}

void _str_buf_append(str_buf_t *buf, char *ptr, s32 len) {
	if (len == 0) return;
	bassert(ptr);
	if (buf->len + len >= buf->cap) {
		str_buf_setcap(buf, (buf->len + len) * 2);
	}
	memcpy(&buf->ptr[buf->len], ptr, len);
	buf->len += len;
	bassert(buf->len < buf->cap);
	buf->ptr[buf->len] = '\0';
}

void str_buf_append_fmt(str_buf_t *buf, const char *fmt, ...) {
	va_list args, args2;
	va_start(args, fmt);
	va_copy(args2, args);
	const s32 len = bvsnprint(NULL, 0, fmt, args);
	bassert(len > 0);

	str_buf_setcap(buf, buf->len + len);

	const s32 wlen = bvsnprint(&buf->ptr[buf->len], len, fmt, args2);
	bassert(wlen == len);
	(void)wlen;

	buf->len += len;

	bassert(buf->len < buf->cap);
	buf->ptr[buf->len] = '\0';

	va_end(args2);
	va_end(args);
}

s32 bvsnprint(char *buf, s32 buf_len, const char *fmt, va_list args) {
	const char *i         = fmt;
	s32         buf_index = 0;
	char        tmp[64];

	while (*i != '\0') {
		if (buf && buf_index >= buf_len) break;
		if (*i != '{' && *i != '}') {
			if (buf) buf[buf_index] = *i;
			++buf_index;
			++i;
			continue;
		}
		++i;

		if (*i == '{' || *i == '}') {
			if (buf) buf[buf_index] = *i;
			++buf_index;
			++i;
			continue;
		}

		const s32 space_left = buf_len - buf_index;

		str_t f = make_str(i, 3);
		if (str_match(f, cstr("str"))) {
			const str_t s = va_arg(args, str_t);
			if (buf) {
				const s32 len = MIN(space_left, s.len);
				memcpy(&buf[buf_index], s.ptr, len);
				buf_index += len;
			} else {
				buf_index += s.len;
			}

			i += f.len;
			goto PASSED;
		}
		if (str_match(f, cstr("s32"))) {
			const s32 s       = va_arg(args, s32);
			const s32 tmp_len = snprintf(tmp, static_arrlenu(tmp), "%i", s);
			if (buf) {
				const s32 len = MIN(space_left, tmp_len);
				memcpy(&buf[buf_index], tmp, len);
				buf_index += len;
			} else {
				buf_index += tmp_len;
			}

			i += f.len;
			goto PASSED;
		}
		if (str_match(f, cstr("u64"))) {
			const u64 s = va_arg(args, u64);
			if (buf) {
				const s32 tmp_len = snprintf(tmp, static_arrlenu(tmp), "%llu", s);
				const s32 len     = MIN(space_left, tmp_len);
				memcpy(&buf[buf_index], tmp, len);
				buf_index += len;
			} else {
				const s32 tmp_len = snprintf(NULL, 0, "%llu", s);
				buf_index += tmp_len;
			}

			i += f.len;
			goto PASSED;
		}
		if (str_match(f, cstr("s64"))) {
			const s64 s = va_arg(args, s64);
			if (buf) {
				const s32 tmp_len = snprintf(tmp, static_arrlenu(tmp), "%lli", s);
				const s32 len     = MIN(space_left, tmp_len);
				memcpy(&buf[buf_index], tmp, len);
				buf_index += len;
			} else {
				const s32 tmp_len = snprintf(NULL, 0, "%lli", s);
				buf_index += tmp_len;
			}

			i += f.len;
			goto PASSED;
		}
		if (str_match(f, cstr("u32"))) {
			const u32 s = va_arg(args, u32);
			if (buf) {
				const s32 tmp_len = snprintf(tmp, static_arrlenu(tmp), "%u", s);
				const s32 len     = MIN(space_left, tmp_len);
				memcpy(&buf[buf_index], tmp, len);
				buf_index += len;
			} else {
				const s32 tmp_len = snprintf(NULL, 0, "%u", s);
				buf_index += tmp_len;
			}

			i += f.len;
			goto PASSED;
		}

		switch (*i) {
		case 's': {
			char     *s     = va_arg(args, char *);
			const s32 s_len = (s32)strlen(s);
			if (buf) {
				const s32 len = MIN(space_left, s_len);
				memcpy(&buf[buf_index], s, len);
				buf_index += len;
			} else {
				buf_index += s_len;
			}

			i += 1;
			goto PASSED;
		}
		}

		babort("Invalid formating string!");

	PASSED:
		if (*i++ != '}') {
			babort("Expected end of formating sequence!");
		}
	}
	return buf_index;
}

s32 bsnprint(char *buf, s32 buf_len, const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	const s32 c = bvsnprint(buf, buf_len, fmt, args);
	va_end(args);
	return c;
}

str_buf_t _str_buf_dup(char *ptr, s32 len) {
	str_buf_t result = {0};
	_str_buf_append(&result, ptr, len);
	return result;
}

// =================================================================================================
// Utils
// =================================================================================================

str_t str_toupper(str_t str) {
	for (s32 i = 0; i < str.len; ++i) {
		str.ptr[i] = toupper(str.ptr[i]);
	}
	return str;
}

bool str_match(str_t a, str_t b) {
	if (a.len != b.len) return false;

#ifdef BL_USE_SIMD
	__m128i *ita = (__m128i *)a.ptr;
	__m128i *itb = (__m128i *)b.ptr;

	// for (s64 i = 0; i < a.len; i += 16, ++ita, ++itb) {
	const __m128i a16 = _mm_loadu_si128(ita);
	const __m128i b16 = _mm_loadu_si128(itb);

	if (_mm_cmpestrc(a16,
	                 a.len,
	                 b16,
	                 b.len,
	                 _SIDD_SBYTE_OPS | _SIDD_CMP_EQUAL_EACH | _SIDD_NEGATIVE_POLARITY |
	                     _SIDD_LEAST_SIGNIFICANT) != 0) {
		return false;
	}
	//}

	return true;
#else
	for (s64 i = 0; i < a.len; i += 1) {
		if (a.ptr[i] != b.ptr[i]) return false;
	}
	return true;
#endif
}

#define MIN3(a, b, c) ((a) < (b) ? ((a) < (c) ? (a) : (c)) : ((b) < (c) ? (b) : (c)))
#define LEVENSHTEIN_MAX_LENGTH 256

// Compute Levenshtein distance of two strings (the legth of both string is limited to
// LEVENSHTEIN_MAX_LENGTH).
// Copy-paste from
// https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#C
s32 levenshtein(const str_t s1, const str_t s2) {
	u32   x, y, lastdiag, olddiag;
	usize s1len = MIN(s1.len, LEVENSHTEIN_MAX_LENGTH);
	usize s2len = MIN(s2.len, LEVENSHTEIN_MAX_LENGTH);
	u32   column[LEVENSHTEIN_MAX_LENGTH + 1];
	for (y = 1; y <= s1len; ++y)
		column[y] = y;
	for (x = 1; x <= s2len; ++x) {
		column[0] = x;
		for (y = 1, lastdiag = x - 1; y <= s1len; y++) {
			olddiag   = column[y];
			column[y] = MIN3(column[y] + 1,
			                 column[y - 1] + 1,
			                 lastdiag + (s1.ptr[y - 1] == s2.ptr[x - 1] ? 0 : 1));
			lastdiag  = olddiag;
		}
	}
	return column[s1len];
}

str_t trim_leading_characters(str_t str, u8 c) {
	while (str.len > 0 && str.ptr[0] == c) {
		str.ptr++;
		str.len--;
	}
	return str;
}

s32 fuzzy_cmp(str_t str, str_t other) {
	if (other.len == 0 || str.len == 0) return str.len == other.len ? 0 : 1;

	s32       score   = 0;
	const s32 min_len = MIN(str.len, other.len);

	for (s32 i = 0; i < min_len; ++i) {
		u8 str_char   = toupper(str.ptr[i]);
		u8 other_char = toupper(other.ptr[i]);

		score += str_char != other_char ? 1 : 0;
		score -= str_char == other_char ? 1 : 0;
	}

	return score;
}

bool search_source_file(const char *filepath,
                        const u32   flags,
                        const char *wdir,
                        char      **out_filepath,
                        char      **out_dirpath) {
	str_buf_t tmp = get_tmp_str();
	if (!filepath) goto NOT_FOUND;
	char        tmp_result[PATH_MAX] = {0};
	const char *result               = NULL;
	if (brealpath(filepath, tmp_result, static_arrlenu(tmp_result))) {
		result = &tmp_result[0];
		goto FOUND;
	}

	// Lookup in working directory.
	if (wdir && isflag(flags, SEARCH_FLAG_WDIR)) {
		str_buf_append_fmt(&tmp, "{s}" PATH_SEPARATOR "{s}", wdir, filepath);
		if (brealpath(str_to_c(tmp), tmp_result, static_arrlenu(tmp_result))) {
			result = &tmp_result[0];
			goto FOUND;
		}
	}

	// file has not been found in current working directory -> search in LIB_DIR
	if (builder_get_lib_dir() && isflag(flags, SEARCH_FLAG_LIB_DIR)) {
		str_buf_clr(&tmp);
		str_buf_append_fmt(&tmp, "{s}" PATH_SEPARATOR "{s}", builder_get_lib_dir(), filepath);
		if (brealpath(str_to_c(tmp), tmp_result, static_arrlenu(tmp_result))) {
			result = &tmp_result[0];
			goto FOUND;
		}
	}

	// file has not been found in current working directory -> search in PATH
	if (isflag(flags, SEARCH_FLAG_SYSTEM_PATH)) {
		char *env = strdup(getenv(ENV_PATH));
		char *s   = env;
		char *p   = NULL;
		do {
			p = strchr(s, ENVPATH_SEPARATORC);
			if (p != NULL) {
				p[0] = 0;
			}
			str_buf_clr(&tmp);
			str_buf_append_fmt(&tmp, "{s}" PATH_SEPARATOR "{s}", s, filepath);
			if (brealpath(str_to_c(tmp), tmp_result, static_arrlenu(tmp_result)))
				result = &tmp_result[0];
			s = p + 1;
		} while (p && !result);
		free(env);
		if (result) goto FOUND;
	}

NOT_FOUND:
	put_tmp_str(tmp);
	return false;

FOUND:
	// Absolute file path.
	if (out_filepath) *out_filepath = strdup(result);
	if (out_dirpath) {
		// Absolute directory path.
		char dirpath[PATH_MAX] = {0};
		if (get_dir_from_filepath(dirpath, static_arrlenu(dirpath), result)) {
			*out_dirpath = strdup(dirpath);
		}
	}
	put_tmp_str(tmp);
	return true;
}

void win_path_to_unix(char *buf, usize buf_size) {
	if (!buf) return;
	for (usize i = 0; i < buf_size; ++i) {
		const char c = buf[i];
		if (c == 0) break;
		if (c != '\\') continue;
		buf[i] = '/';
	}
}

void unix_path_to_win(char *buf, usize buf_size) {
	if (!buf) return;
	for (usize i = 0; i < buf_size; ++i) {
		const char c = buf[i];
		if (c == 0) break;
		if (c != '/') continue;
		buf[i] = '\\';
	}
}

bool get_current_exec_path(char *buf, usize buf_size) {
#if BL_PLATFORM_WIN
	if (GetModuleFileNameA(NULL, buf, (DWORD)buf_size)) {
		win_path_to_unix(buf, buf_size);
		return true;
	}
	return false;
#elif BL_PLATFORM_LINUX
	return readlink("/proc/self/exe", buf, buf_size) != -1;
#elif BL_PLATFORM_MACOS
	return _NSGetExecutablePath(buf, (u32 *)&buf_size) != -1;
#else
	return false;
#endif
}

bool get_current_exec_dir(char *buf, usize buf_size) {
	char tmp[PATH_MAX] = {0};
	if (!get_current_exec_path(tmp, PATH_MAX)) return false;
	if (!get_dir_from_filepath(buf, buf_size, tmp)) return false;
	return true;
}

bool get_current_working_dir(char *buf, usize buf_size) {
	return brealpath(".", buf, (s32)buf_size);
}

bool set_current_working_dir(const char *path) {
#if BL_PLATFORM_WIN
	return SetCurrentDirectoryA(path);
#else
	return chdir(path) != -1;
#endif
}

bool file_exists(const char *filepath) {
#if BL_PLATFORM_WIN
	return (bool)PathFileExistsA(filepath);
#else
	struct stat tmp;
	return stat(filepath, &tmp) == 0;
#endif
}

bool _file_exists(const char *ptr, s32 len) {
	bassert(ptr && len && ptr[len] == '\0');
#if BL_PLATFORM_WIN
	return (bool)PathFileExistsA(ptr);
#else
	struct stat tmp;
	return stat(ptr, &tmp) == 0;
#endif
}

bool dir_exists(const char *dirpath) {
#if BL_PLATFORM_WIN
	DWORD dwAttrib = GetFileAttributesA(dirpath);
	return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
	struct stat sb;
	return stat(dirpath, &sb) == 0 && S_ISDIR(sb.st_mode);
#endif
}

bool _dir_exists(const char *ptr, s32 len) {
	bassert(ptr && len && ptr[len] == '\0');
#if BL_PLATFORM_WIN
	DWORD dwAttrib = GetFileAttributesA(ptr);
	return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
	struct stat sb;
	return stat(ptr, &sb) == 0 && S_ISDIR(sb.st_mode);
#endif
}

bool brealpath(const char *file, char *out, s32 out_len) {
	bassert(out);
	bassert(out_len);
	if (!file) return false;
#if BL_PLATFORM_WIN
	const DWORD len = GetFullPathNameA(file, out_len, out, NULL);
	if (!len) {
		DWORD ec = GetLastError();
		if (ec == ERROR_FILENAME_EXCED_RANGE)
			builder_error("Cannot get full path of '%s', resulting path is too long. (expected "
			              "maximum %d characters)",
			              file,
			              PATH_MAX);
		return false;
	}
	if (!file_exists(out)) return false;
	win_path_to_unix(out, out_len);
	return true;
#else
	return realpath(file, out);
#endif
}

bool normalize_path(str_buf_t *path) {
	char buf[PATH_MAX] = "";
	if (!brealpath(str_to_c(*path), buf, static_arrlenu(buf))) {
		return false;
	}
	str_buf_clr(path);
	str_buf_append(path, make_str_from_c(buf));
	return true;
}

bool create_dir(const char *dirpath) {
#if BL_PLATFORM_WIN
	return CreateDirectoryA(dirpath, NULL) != 0;
#else
	return mkdir(dirpath, 0700) == 0;
#endif
}

bool create_dir_tree(const char *dirpath) {
	char tmp[PATH_MAX] = {0};
	s32  prev_i        = 0;
	for (s32 i = 0; dirpath[i]; ++i) {
		if (dirpath[i] == PATH_SEPARATORC) {
			if (i - prev_i > 1) {
				if (!dir_exists(tmp)) {
					if (!create_dir(tmp)) return false;
				}
			}
			prev_i = i;
		}
		tmp[i] = dirpath[i];
	}
	if (!dir_exists(tmp)) {
		if (!create_dir(tmp)) return false;
	}
	return true;
}

bool copy_dir(const char *src, const char *dest) {
	str_buf_t tmp = get_tmp_str();
#if BL_PLATFORM_WIN
	char *_src  = strdup(src);
	char *_dest = strdup(dest);
	unix_path_to_win(_src, strlen(_src));
	unix_path_to_win(_dest, strlen(_dest));
	str_buf_append_fmt(&tmp, "xcopy /H /E /Y /I \"{s}\" \"{s}\" 2>nul 1>nul", _src, _dest);
	free(_src);
	free(_dest);
#else
	str_buf_append_fmt(&tmp, "mkdir -p {s} && cp -rf {s}/* {s}", dest, src, dest);
	blog("%s", tmp);
#endif
	const bool result = system(str_to_c(tmp)) == 0;
	put_tmp_str(tmp);
	return result;
}

bool copy_file(const char *src, const char *dest) {
	str_buf_t tmp = get_tmp_str();
#if BL_PLATFORM_WIN
	char *_src  = strdup(src);
	char *_dest = strdup(dest);
	unix_path_to_win(_src, strlen(_src));
	unix_path_to_win(_dest, strlen(_dest));
	str_buf_append_fmt(&tmp, "copy /Y /B \"{s}\" \"{s}\" 2>nul 1>nul", _src, _dest);
	free(_src);
	free(_dest);
#else
	str_buf_append_fmt(&tmp, "cp -f {s} {s}", src, dest);
#endif
	const bool result = system(str_to_c(tmp)) == 0;
	put_tmp_str(tmp);
	return result;
}

bool remove_dir(const char *path) {
	str_buf_t tmp = get_tmp_str();
#if BL_PLATFORM_WIN
	char *_path = strdup(path);
	unix_path_to_win(_path, strlen(_path));
	str_buf_append_fmt(&tmp, "del \"{s}\" /q /s 2>nul 1>nul", _path);
	free(_path);
#else
	str_buf_append_fmt(&tmp, "rm -rf {s}", path);
#endif
	const bool result = system(str_to_c(tmp)) == 0;
	put_tmp_str(tmp);
	return result;
}
void date_time(char *buf, s32 len, const char *format) {
	bassert(buf && len);
	time_t     timer;
	struct tm *tm_info;
	time(&timer);
	tm_info = localtime(&timer);
	strftime(buf, len, format, tm_info);
}

void print_bits(s32 const size, void const *const ptr) {
	unsigned char *b = (unsigned char *)ptr;
	unsigned char  byte;
	s32            i, j;

	for (i = size - 1; i >= 0; i--) {
		for (j = 7; j >= 0; j--) {
			byte = (b[i] >> j) & 1;
			printf("%u", byte);
		}
	}
	puts("");
}

int count_bits(u64 n) {
	int count = 0;
	while (n) {
		count++;
		n = n >> 1;
	}
	return count;
}

bool get_dir_from_filepath(char *buf, const usize l, const char *filepath) {
	if (!filepath) return false;
	char *ptr = strrchr(filepath, PATH_SEPARATORC);
	if (!ptr) return false;
	if (filepath == ptr) {
		strncpy(buf, filepath, l);
		return true;
	}
	const ptrdiff_t len = ptr - filepath;
	if (len >= (s64)l) babort("path too long!!!");
	strncpy(buf, filepath, len);
	buf[len] = '\0';
	return true;
}

bool get_filename_from_filepath(char *buf, const usize l, const char *filepath) {
	if (!filepath) return false;

	char *ptr = strrchr(filepath, PATH_SEPARATORC);
	if (!ptr || filepath == ptr) {
		strncpy(buf, filepath, l);
		return true;
	}

	usize len = strlen(filepath) - (ptr - filepath);
	if (len + 1 > l) babort("path too long!!!");
	strncpy(buf, ptr + 1, len);

	return true;
}

str_buf_t platform_lib_name(const str_t name) {
	str_buf_t tmp = get_tmp_str();
	if (!name.len) return tmp;

#if BL_PLATFORM_MACOS
	str_buf_append_fmt(&tmp, "lib{str}.dylib", name);
#elif BL_PLATFORM_LINUX
	str_buf_append_fmt(&tmp, "lib{str}.so", name);
#elif BL_PLATFORM_WIN
	str_buf_append_fmt(&tmp, "{str}.dll", name);
#else
	babort("Unknown dynamic library format.");
#endif
	return tmp;
}

f64 get_tick_ms(void) {
#if BL_PLATFORM_MACOS
	struct mach_timebase_info convfact;
	if (mach_timebase_info(&convfact) != 0) {
		uint64_t tick = mach_absolute_time();
		return (f64)((tick * convfact.numer) / (convfact.denom * 1000000));
	} else {
		struct timeval tv;
		gettimeofday(&tv, NULL);
		const f64 s = (f64)tv.tv_sec;
		const f64 u = (f64)tv.tv_usec;
		return (s * 1000.0) + (u / 1000.0);
	}
#elif BL_PLATFORM_LINUX
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (f64)ts.tv_sec * 1000. + (f64)ts.tv_nsec / 1000000.;
#elif BL_PLATFORM_WIN
	LARGE_INTEGER f;
	LARGE_INTEGER t;

	if (!QueryPerformanceFrequency(&f)) return 0.;
	if (!QueryPerformanceCounter(&t)) return 0.;

	return ((f64)t.QuadPart / (f64)f.QuadPart) * 1000.;
#else
	babort("Unknown dynamic library format.");
#endif
}

s32 get_last_error(char *buf, s32 buf_len) {
#if BL_PLATFORM_MACOS
	const s32 error_code = errno;
	if (!error_code) return 0;
	const char *msg = strerror(error_code);
	strncpy(buf, msg, buf_len);
	return strlen(msg);
#elif BL_PLATFORM_LINUX
	const s32 error_code = errno;
	if (!error_code) return 0;
	const char *msg = strerror(error_code);
	strncpy(buf, msg, buf_len);
	return strlen(msg);
#elif BL_PLATFORM_WIN
	const s32 error_code = GetLastError();
	if (error_code == 0) return 0;
	const DWORD msg_len = FormatMessageA(
	    FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_MAX_WIDTH_MASK,
	    NULL,
	    error_code,
	    0,
	    buf,
	    buf_len,
	    NULL);
	return msg_len;
#else
	babort("Cannot get last error!");
#endif
}

u32 next_pow_2(u32 n) {
	u32 p = 1;
	if (n && !(n & (n - 1))) return n;

	while (p < n)
		p <<= 1;

	return p;
}

void color_print(FILE *stream, s32 color, const char *format, ...) {
	if (builder.is_initialized && builder.options->no_color) color = BL_NO_COLOR;
	va_list args;
	va_start(args, format);

#if BL_PLATFORM_WIN
	s32 c;
	switch (color) {
	case BL_YELLOW:
		c = (0xE % 0x0F);
		break;
	case BL_RED:
		c = (0xC % 0x0F);
		break;
	case BL_BLUE:
		c = (0x9 % 0x0F);
		break;
	case BL_GREEN:
		c = (0xA % 0x0F);
		break;
	case BL_CYAN:
		c = (0xB % 0x0F);
		break;

	default:
		c = 0;
	}

	if (color != BL_NO_COLOR) {
		HANDLE handle =
		    stream == stderr ? GetStdHandle(STD_ERROR_HANDLE) : GetStdHandle(STD_OUTPUT_HANDLE);
		CONSOLE_SCREEN_BUFFER_INFO console_info;
		GetConsoleScreenBufferInfo(handle, &console_info);
		WORD saved_attributes = console_info.wAttributes;

		SetConsoleTextAttribute(handle, c);
		vfprintf(stream, format, args);
		SetConsoleTextAttribute(handle, saved_attributes);
	} else {
		vfprintf(stream, format, args);
	}
#else
	char *c;
	switch (color) {
	case BL_RED:
		c = "\x1b[31m";
		break;
	case BL_GREEN:
		c = "\x1b[32m";
		break;
	case BL_YELLOW:
		c = "\x1b[33m";
		break;
	case BL_BLUE:
		c = "\x1b[34m";
		break;
	case BL_CYAN:
		c = "\x1b[36m";
		break;
	default:
		c = "\x1b[0m";
	}

	fprintf(stream, "%s", c);
	vfprintf(stream, format, args);
	fprintf(stream, "\x1b[0m");
#endif
	va_end(args);
}

s32 cpu_thread_count(void) {
#if BL_PLATFORM_WIN
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	return sysinfo.dwNumberOfProcessors;
#else
	return 8; // @Incomplete: Detect for platform.
#endif
}

str_buf_t execute(const char *cmd) {
	str_buf_t tmp  = get_tmp_str();
	FILE     *pipe = popen(cmd, "r");
	if (!pipe) {
		builder_error("Command '%s' failed!", cmd);
		return tmp;
	}
	char buffer[128];
	while (fgets(buffer, static_arrlenu(buffer), pipe) != NULL) {
		str_buf_append(&tmp, make_str_from_c(buffer));
	}
	pclose(pipe);
	if (tmp.len > 0 && tmp.ptr[tmp.len - 1] == '\n') {
		tmp.len -= 1;
		tmp.ptr[tmp.len] = '\0';
	}
	return tmp;
}

const char *read_config(struct config       *config,
                        const struct target *target,
                        const char          *path,
                        const char          *default_value) {
	bassert(config && target && path);
	str_buf_t fullpath = get_tmp_str();
	char      triple_str[128];
	target_triple_to_string(&target->triple, triple_str, static_arrlenu(triple_str));
	str_buf_append_fmt(&fullpath, "/{s}/{s}", triple_str, path);
	const char *result = confreads(config, str_to_c(fullpath), default_value);
	put_tmp_str(fullpath);
	return result;
}

s32 process_tokens(void *ctx, const char *input, const char *delimiter, process_tokens_fn_t fn) {
	if (!is_str_valid_nonempty(input)) return 0;
	str_buf_t tmp = get_tmp_str();
	str_buf_append(&tmp, make_str_from_c(input));
	char *token;
	char *it    = tmp.ptr;
	s32   count = 0;
	while ((token = strtok_r(it, delimiter, &it))) {
		token = strtrim(token);
		if (token[0] != '\0') {
			fn(ctx, token);
			++count;
		}
	}
	put_tmp_str(tmp);
	return count;
}

static char *ltrim(char *s) {
	while (isspace(*s))
		s++;
	return s;
}

static char *rtrim(char *s) {
	char *back = s + strlen(s);
	while (isspace(*--back))
		;
	*(back + 1) = '\0';
	return s;
}

char *strtrim(char *str) {
	return rtrim(ltrim(str));
}
