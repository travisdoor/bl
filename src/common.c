//************************************************************************************************
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
//************************************************************************************************

#include "common.h"
#include "assembly.h"
#include "builder.h"
#include <stdarg.h>
#include <time.h>

#ifndef BL_COMPILER_MSVC
#include "unistd.h"
#include <sys/stat.h>
#endif

#ifdef BL_PLATFORM_MACOS
#include <mach-o/dyld.h>
#include <mach/mach_time.h>
#endif

#ifdef BL_PLATFORM_WIN
#include <windows.h>
#endif

u64 main_thread_id = 0;

bool search_source_file(const char *filepath,
                        const u32   flags,
                        const char *wdir,
                        char **     out_filepath,
                        char **     out_dirpath)
{
    if (!filepath) goto NOT_FOUND;
    char        tmp[PATH_MAX] = {0};
    const char *rpath         = tmp;
    // Lookup in working directory.
    if (wdir && IS_FLAG(flags, SEARCH_FLAG_WDIR)) {
        strncpy(tmp, wdir, TARRAY_SIZE(tmp));
        strncat(tmp, PATH_SEPARATOR, TARRAY_SIZE(tmp) - 1);
        strncat(tmp, filepath, TARRAY_SIZE(tmp) - 1);
        if (file_exists(tmp)) goto FOUND;
    }
    rpath = brealpath(filepath, tmp, PATH_MAX);
    if (rpath) goto FOUND;

    // file has not been found in current working direcotry -> search in LIB_DIR
    if (ENV_LIB_DIR && IS_FLAG(flags, SEARCH_FLAG_LIB_DIR)) {
        char tmp_lib_dir[PATH_MAX];
        strncpy(tmp_lib_dir, ENV_LIB_DIR, TARRAY_SIZE(tmp_lib_dir));
        strncat(tmp_lib_dir, PATH_SEPARATOR, TARRAY_SIZE(tmp_lib_dir) - 1);
        strncat(tmp_lib_dir, filepath, TARRAY_SIZE(tmp_lib_dir) - 1);
        rpath = brealpath(tmp_lib_dir, tmp, PATH_MAX);
        if (rpath) goto FOUND;
    }

    // file has not been found in current working direcotry -> search in PATH
    if (IS_FLAG(flags, SEARCH_FLAG_SYSTEM_PATH)) {
        char  tmp_env[PATH_MAX] = {0};
        char *env               = strdup(getenv(ENV_PATH));
        char *s                 = env;
        char *p                 = NULL;
        do {
            p = strchr(s, ENVPATH_SEPARATOR);
            if (p != NULL) {
                p[0] = 0;
            }
            strncpy(tmp_env, s, TARRAY_SIZE(tmp_env));
            strncat(tmp_env, PATH_SEPARATOR, TARRAY_SIZE(tmp_env) - 1);
            strncat(tmp_env, filepath, TARRAY_SIZE(tmp_env) - 1);
            rpath = brealpath(&tmp_env[0], tmp, PATH_MAX);
            s     = p + 1;
        } while (p != NULL && rpath == NULL);
        free(env);
        if (rpath) goto FOUND;
    }

NOT_FOUND:
    return false;

FOUND:
    // Absolute file path.
    if (out_filepath) *out_filepath = strdup(rpath);
    if (out_dirpath) {
        // Absolute directory path.
        memset(tmp, 0, TARRAY_SIZE(tmp));
        if (get_dir_from_filepath(tmp, PATH_MAX, rpath)) {
            *out_dirpath = strdup(tmp);
        }
    }
    return true;
}

void win_fix_path(char *buf, usize buf_size)
{
    if (!buf) return;
    for (usize i = 0; i < buf_size; ++i) {
        const char c = buf[i];
        if (c == 0) break;
        if (c != '\\') continue;

        buf[i] = '/';
    }
}

bool get_current_exec_path(char *buf, usize buf_size)
{
#if defined(BL_PLATFORM_WIN)
    if (GetModuleFileNameA(NULL, buf, (DWORD)buf_size)) {
        win_fix_path(buf, buf_size);
        return true;
    }

    return false;
#elif defined(BL_PLATFORM_LINUX)
    return readlink("/proc/self/exe", buf, buf_size) != -1;
#elif defined(BL_PLATFORM_MACOS)
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

const char *get_current_working_dir(char *buf, usize buf_size)
{
    return brealpath(".", buf, buf_size);
}

bool file_exists(const char *filepath)
{
#if defined(BL_PLATFORM_WIN)
    return (bool)PathFileExistsA(filepath);
#else
    struct stat tmp;
    return stat(filepath, &tmp) == 0;
#endif
}

bool dir_exists(const char *dirpath)
{
#if defined(BL_PLATFORM_WIN)
    DWORD dwAttrib = GetFileAttributesA(dirpath);
    return (dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
#else
    struct stat sb;
    return stat(dirpath, &sb) == 0 && S_ISDIR(sb.st_mode);
#endif
}

const char *brealpath(const char *file, char *out, s32 out_len)
{
    const char *resolved = NULL;
    BL_ASSERT(out);
    BL_ASSERT(out_len);
    if (!file) return resolved;

#if defined(BL_PLATFORM_WIN)
    if (GetFullPathNameA(file, out_len, out, NULL) && file_exists(out)) {
        win_fix_path(out, out_len);
        return &out[0];
    }

    return NULL;
#else
    return realpath(file, out);
#endif
}

bool create_dir(const char *dirpath)
{
#if defined(BL_PLATFORM_WIN)
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

#ifdef BL_PLATFORM_MACOS
    snprintf(buffer, max_len, "lib%s.dylib", name);
#elif defined(BL_PLATFORM_LINUX)
    snprintf(buffer, max_len, "lib%s.so", name);
#elif defined(BL_PLATFORM_WIN)
    snprintf(buffer, max_len, "%s.dll", name);
#else
    BL_ABORT("Unknown dynamic library format.");
#endif
}

f64 get_tick_ms(void)
{
#ifdef BL_PLATFORM_MACOS
    const f64 t = (f64)mach_absolute_time();
    return t * 0.00001;
#elif defined(BL_PLATFORM_LINUX)
    // @INCOMPLETE
    return 0.;
#elif defined(BL_PLATFORM_WIN)
    LARGE_INTEGER f;
    LARGE_INTEGER t;

    if (!QueryPerformanceFrequency(&f)) return 0.;
    if (!QueryPerformanceCounter(&t)) return 0.;

    return ((f64)t.QuadPart / (f64)f.QuadPart) * 1000.;
#else
    BL_ABORT("Unknown dynamic library format.");
#endif
}

TArray *create_arr(Assembly *assembly, usize size)
{
    TArray **tmp = arena_alloc(&assembly->arenas.array);
    *tmp         = tarray_new(size);
    return *tmp;
}

void *_create_sarr(Assembly *assembly, usize arr_size)
{
    BL_ASSERT(arr_size <= assembly->arenas.small_array.elem_size_in_bytes &&
              "SmallArray is too big to be allocated inside arena, make array smaller "
              "or arena "
              "bigger.");

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
    // HACK: Is this reference really needed????
    // HACK: Is this reference really needed????
    // HACK: Is this reference really needed????
    if (builder.options.no_color) color = BL_NO_COLOR;
    va_list args;
    va_start(args, format);

#ifdef BL_PLATFORM_WIN
    s32 c;
    switch (color) {
    case BL_YELLOW:
        c = (14 % 0x0F);
        break;
    case BL_RED:
        c = FOREGROUND_RED;
        break;
    case BL_BLUE:
        c = FOREGROUND_BLUE;
        break;
    case BL_GREEN:
        c = FOREGROUND_GREEN;
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
    fprintf(stream, "\n");
#else
    char *c;
    switch (color) {
    case BL_YELLOW:
        c = "\x1b[33m";
        break;
    case BL_RED:
        c = "\x1b[31m";
        break;
    case BL_BLUE:
        c = "\x1b[34m";
        break;
    case BL_GREEN:
        c = "\x1b[32m";
        break;

    default:
        c = "\x1b[0m";
    }

    fprintf(stream, "%s", c);
    vfprintf(stream, format, args);
    fprintf(stream, "\x1b[0m\n");
#endif
    va_end(args);
}
