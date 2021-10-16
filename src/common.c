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
#include "blmemory.h"
#define STB_DS_IMPLEMENTATION
#define STBDS_REALLOC(context, ptr, size) bl_realloc(ptr, size)
#define STBDS_FREE(context, ptr) bl_free(ptr)
#include "stb_ds.h"

#include "assembly.h"
#include "builder.h"
#include "common.h"
#include <stdarg.h>
#include <time.h>

#if !BL_PLATFORM_WIN
#include "unistd.h"
#include <sys/stat.h>
#endif

#if BL_PLATFORM_MACOS
#include <errno.h>
#include <mach-o/dyld.h>
#include <mach/mach_time.h>
#endif

#if BL_PLATFORM_LINUX
#include <errno.h>
#endif

#if BL_PLATFORM_WIN
#include <shlwapi.h>
#endif

u64 main_thread_id = 0;
// =================================================================================================
// PUBLIC
// =================================================================================================

// =================================================================================================
// Small Array
// =================================================================================================

struct sarr_any {
    s32 len;
    s32 cap;
    u8 *data;
    u8  buf[1];
};

void sarrinit(void *ptr)
{
    BL_ASSERT(ptr);
    struct sarr_any *arr = (struct sarr_any *)ptr;
    arr->data            = arr->buf;
    arr->len             = 0;
    arr->cap             = 0;
}

void sarradd_impl(void *ptr, s32 elem_size, s32 elem_count)
{
    struct sarr_any *arr     = (struct sarr_any *)ptr;
    const bool       on_heap = arr->cap;
    if (!on_heap && arr->len == elem_count) {
        arr->cap        = elem_count * 2;
        const s32 bytes = elem_size * arr->cap;
        arr->data       = bl_malloc(bytes);
        if (!arr->data) abort();
        memcpy(arr->data, arr->buf, bytes);
    } else if (on_heap && arr->len == arr->cap) {
        arr->cap *= 2;
        void *tmp = arr->data;
        if ((arr->data = bl_realloc(arr->data, arr->cap * elem_size)) == NULL) {
            bl_free(tmp);
            abort();
        }
    }
    arr->len++;
}

// =================================================================================================
// Utils
// =================================================================================================

bool search_source_file(const char *filepath,
                        const u32   flags,
                        const char *wdir,
                        char      **out_filepath,
                        char      **out_dirpath)
{
    TString *tmp = get_tmpstr();
    if (!filepath) goto NOT_FOUND;
    char        tmp_result[PATH_MAX] = {0};
    const char *result               = NULL;
    if (brealpath(filepath, tmp_result, static_arrlen(tmp_result))) {
        result = &tmp_result[0];
        goto FOUND;
    }

    // Lookup in working directory.
    if (wdir && IS_FLAG(flags, SEARCH_FLAG_WDIR)) {
        tstring_setf(tmp, "%s" PATH_SEPARATOR "%s", wdir, filepath);
        if (brealpath(tmp->data, tmp_result, static_arrlen(tmp_result))) {
            result = &tmp_result[0];
            goto FOUND;
        }
    }

    // file has not been found in current working directory -> search in LIB_DIR
    if (builder_get_lib_dir() && IS_FLAG(flags, SEARCH_FLAG_LIB_DIR)) {
        tstring_setf(tmp, "%s" PATH_SEPARATOR "%s", builder_get_lib_dir(), filepath);
        if (brealpath(tmp->data, tmp_result, static_arrlen(tmp_result))) {
            result = &tmp_result[0];
            goto FOUND;
        }
    }

    // file has not been found in current working directory -> search in PATH
    if (IS_FLAG(flags, SEARCH_FLAG_SYSTEM_PATH)) {
        char *env = strdup(getenv(ENV_PATH));
        char *s   = env;
        char *p   = NULL;
        do {
            p = strchr(s, ENVPATH_SEPARATOR);
            if (p != NULL) {
                p[0] = 0;
            }
            tstring_setf(tmp, "%s" PATH_SEPARATOR "%s", s, filepath);
            if (brealpath(tmp->data, tmp_result, static_arrlen(tmp_result))) result = &tmp_result[0];
            s = p + 1;
        } while (p && !result);
        free(env);
        if (result) goto FOUND;
    }

NOT_FOUND:
    put_tmpstr(tmp);
    return false;

FOUND:
    // Absolute file path.
    if (out_filepath) *out_filepath = strdup(result);
    if (out_dirpath) {
        // Absolute directory path.
        char dirpath[PATH_MAX] = {0};
        if (get_dir_from_filepath(dirpath, static_arrlen(dirpath), result)) {
            *out_dirpath = strdup(dirpath);
        }
    }
    put_tmpstr(tmp);
    return true;
}

void win_path_to_unix(char *buf, usize buf_size)
{
    if (!buf) return;
    for (usize i = 0; i < buf_size; ++i) {
        const char c = buf[i];
        if (c == 0) break;
        if (c != '\\') continue;
        buf[i] = '/';
    }
}

void unix_path_to_win(char *buf, usize buf_size)
{
    if (!buf) return;
    for (usize i = 0; i < buf_size; ++i) {
        const char c = buf[i];
        if (c == 0) break;
        if (c != '/') continue;
        buf[i] = '\\';
    }
}

bool get_current_exec_path(char *buf, usize buf_size)
{
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

bool get_current_exec_dir(char *buf, usize buf_size)
{
    char tmp[PATH_MAX] = {0};
    if (!get_current_exec_path(tmp, PATH_MAX)) return false;
    if (!get_dir_from_filepath(buf, buf_size, tmp)) return false;
    return true;
}

bool get_current_working_dir(char *buf, usize buf_size)
{
    return brealpath(".", buf, (s32)buf_size);
}

bool file_exists(const char *filepath)
{
#if BL_PLATFORM_WIN
    return (bool)PathFileExistsA(filepath);
#else
    struct stat tmp;
    return stat(filepath, &tmp) == 0;
#endif
}

bool dir_exists(const char *dirpath)
{
#if BL_PLATFORM_WIN
    DWORD dwAttrib = GetFileAttributesA(dirpath);
    return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
    struct stat sb;
    return stat(dirpath, &sb) == 0 && S_ISDIR(sb.st_mode);
#endif
}

bool brealpath(const char *file, char *out, s32 out_len)
{
    BL_ASSERT(out);
    BL_ASSERT(out_len);
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

bool create_dir(const char *dirpath)
{
#if BL_PLATFORM_WIN
    return CreateDirectoryA(dirpath, NULL) != 0;
#else
    return mkdir(dirpath, 0700) == 0;
#endif
}

bool create_dir_tree(const char *dirpath)
{
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

bool copy_dir(const char *src, const char *dest)
{
    TString *tmp = get_tmpstr();
#if BL_PLATFORM_WIN
    char *_src  = strdup(src);
    char *_dest = strdup(dest);
    unix_path_to_win(_src, strlen(_src));
    unix_path_to_win(_dest, strlen(_dest));
    tstring_setf(tmp, "xcopy /H /E /Y /I \"%s\" \"%s\" 2>nul 1>nul", _src, _dest);
    free(_src);
    free(_dest);
#else
    tstring_setf(tmp, "mkdir -p %s && cp -rf %s/* %s", dest, src, dest);
    BL_LOG("%s", tmp->data);
#endif
    const bool result = system(tmp->data) == 0;
    put_tmpstr(tmp);
    return result;
}

bool copy_file(const char *src, const char *dest)
{
    TString *tmp = get_tmpstr();
#if BL_PLATFORM_WIN
    char *_src  = strdup(src);
    char *_dest = strdup(dest);
    unix_path_to_win(_src, strlen(_src));
    unix_path_to_win(_dest, strlen(_dest));
    tstring_setf(tmp, "copy /Y /B \"%s\" \"%s\" 2>nul 1>nul", _src, _dest);
    free(_src);
    free(_dest);
#else
    tstring_setf(tmp, "cp -f %s %s", src, dest);
#endif
    const bool result = system(tmp->data) == 0;
    put_tmpstr(tmp);
    return result;
}

bool remove_dir(const char *path)
{
    TString *tmp = get_tmpstr();
#if BL_PLATFORM_WIN
    char *_path = strdup(path);
    unix_path_to_win(_path, strlen(_path));
    tstring_setf(tmp, "del \"%s\" /q /s 2>nul 1>nul", _path);
    free(_path);
#else
    tstring_setf(tmp, "rm -rf %s", path);
#endif
    const bool result = system(tmp->data) == 0;
    put_tmpstr(tmp);
    return result;
}
void date_time(char *buf, s32 len, const char *format)
{
    BL_ASSERT(buf && len);
    time_t     timer;
    struct tm *tm_info;
    time(&timer);
    tm_info = localtime(&timer);
    strftime(buf, len, format, tm_info);
}

bool is_aligned(const void *p, usize alignment)
{
    return (uintptr_t)p % alignment == 0;
}

void align_ptr_up(void **p, usize alignment, ptrdiff_t *adjustment)
{
    ptrdiff_t adj;
    if (is_aligned(*p, alignment)) {
        if (adjustment) *adjustment = 0;
        return;
    }

    const usize mask = alignment - 1;
    BL_ASSERT((alignment & mask) == 0 && "wrong alignemet"); // pwr of 2
    const uintptr_t i_unaligned  = (uintptr_t)(*p);
    const uintptr_t misalignment = i_unaligned & mask;

    adj = alignment - misalignment;
    *p  = (void *)(i_unaligned + adj);
    if (adjustment) *adjustment = adj;
}

void print_bits(s32 const size, void const *const ptr)
{
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

int count_bits(u64 n)
{
    int count = 0;
    while (n) {
        count++;
        n = n >> 1;
    }
    return count;
}

bool get_dir_from_filepath(char *buf, const usize l, const char *filepath)
{
    if (!filepath) return false;
    char *ptr = strrchr(filepath, PATH_SEPARATORC);
    if (!ptr) return false;
    if (filepath == ptr) {
        strncpy(buf, filepath, l);
        return true;
    }
    usize len = ptr - filepath;
    if (len + 1 > l) BL_ABORT("path too long!!!");
    strncpy(buf, filepath, len);
    return true;
}

bool get_filename_from_filepath(char *buf, const usize l, const char *filepath)
{
    if (!filepath) return false;

    char *ptr = strrchr(filepath, PATH_SEPARATORC);
    if (!ptr || filepath == ptr) {
        strncpy(buf, filepath, l);
        return true;
    }

    usize len = strlen(filepath) - (ptr - filepath);
    if (len + 1 > l) BL_ABORT("path too long!!!");
    strncpy(buf, ptr + 1, len);

    return true;
}

void platform_lib_name(const char *name, char *buffer, usize max_len)
{
    if (!name) return;

#if BL_PLATFORM_MACOS
    snprintf(buffer, max_len, "lib%s.dylib", name);
#elif BL_PLATFORM_LINUX
    snprintf(buffer, max_len, "lib%s.so", name);
#elif BL_PLATFORM_WIN
    snprintf(buffer, max_len, "%s.dll", name);
#else
    BL_ABORT("Unknown dynamic library format.");
#endif
}

f64 get_tick_ms(void)
{
#if BL_PLATFORM_MACOS
    struct mach_timebase_info convfact;
    mach_timebase_info(&convfact);
    uint64_t tick = mach_absolute_time();
    return (f64)((tick * convfact.numer) / (convfact.denom * 1000000));
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
    BL_ABORT("Unknown dynamic library format.");
#endif
}

s32 get_last_error(char *buf, s32 buf_len)
{
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
    BL_ABORT("Cannot get last error!");
#endif
}

void *_create_sarr(struct assembly *assembly, usize arr_size)
{
    BL_ASSERT(
        arr_size <= assembly->arenas.small_array.elem_size_in_bytes &&
        "SmallArray is too big to be allocated inside arena, make array smaller or arena bigger.");

    TSmallArrayAny *tmp = arena_alloc(&assembly->arenas.small_array);
    tsa_init(tmp);
    return tmp;
}

u32 next_pow_2(u32 n)
{
    u32 p = 1;
    if (n && !(n & (n - 1))) return n;

    while (p < n)
        p <<= 1;

    return p;
}

void color_print(FILE *stream, s32 color, const char *format, ...)
{
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

s32 cpu_thread_count(void)
{
#if BL_PLATFORM_WIN
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
#else
    return 8; // @Incomplete: Detect for platform.
#endif
}
