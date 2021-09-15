// =================================================================================================
// blc
//
// File:   assembly.c
// Author: Martin Dorazil
// Date:   09/02/2018
//
// Copyright 2017 Martin Dorazil
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

#include "assembly.h"
#include "blmemory.h"
#include "builder.h"
#include "common.h"
#include "unit.h"
#include <string.h>

#if BL_PLATFORM_WIN
#include "winpthreads.h"
#include <windows.h>
#else
#include <pthread.h>
#endif

#define EXPECTED_GSCOPE_COUNT 4094
#define EXPECTED_ARRAY_COUNT 256

const char *arch_names[] = {
#define GEN_ARCH
#define entry(X) #X,
#include "assembly.inc"
#undef entry
#undef GEN_ARCH
};

const char *os_names[] = {
#define GEN_OS
#define entry(X) #X,
#include "assembly.inc"
#undef entry
#undef GEN_OS
};

const char *vendor_names[] = {
#define GEN_VENDOR
#define entry(X) #X,
#include "assembly.inc"
#undef entry
#undef GEN_VENDOR
};

const char *env_names[] = {
#define GEN_ENV
#define entry(X) #X,
#include "assembly.inc"
#undef entry
#undef GEN_ENV
};

BL_STATIC_ASSERT(ARCH_unknown == 0);

union _SmallArrays {
    TSmallArray_TypePtr       type;
    TSmallArray_MemberPtr     member;
    TSmallArray_VariantPtr    variant;
    TSmallArray_InstrPtr      instr;
    TSmallArray_ConstValuePtr cv;
    TSmallArray_AstPtr        ast;
    TSmallArray_ArgPtr        arg;
    TSmallArray_SwitchCase    switch_case;
    TSmallArray_FnPtr         fn;
};

static void tarray_dtor(TArray **arr)
{
    tarray_delete(*arr);
}

static void small_array_dtor(TSmallArrayAny *arr)
{
    tsa_terminate(arr);
}

typedef struct AssemblySyncImpl {
    pthread_mutex_t units_lock;
    pthread_mutex_t import_module_lock;
} AssemblySyncImpl;

static AssemblySyncImpl *sync_new(void)
{
    AssemblySyncImpl *impl = bl_malloc(sizeof(AssemblySyncImpl));
    pthread_mutex_init(&impl->units_lock, NULL);
    pthread_mutex_init(&impl->import_module_lock, NULL);
    return impl;
}

static void sync_delete(AssemblySyncImpl *impl)
{
    pthread_mutex_destroy(&impl->units_lock);
    pthread_mutex_destroy(&impl->import_module_lock);
    bl_free(impl);
}

static void dl_init(struct assembly *assembly)
{
    DCCallVM *vm = dcNewCallVM(4096);
    dcMode(vm, DC_CALL_C_DEFAULT);
    assembly->dc_vm = vm;
}

static void dl_terminate(struct assembly *assembly)
{
    dcFree(assembly->dc_vm);
}

static void parse_triple(const char *normalized_triple, struct target_triple *out_triple)
{
    BL_ASSERT(out_triple);
    // arch-vendor-os-evironment
    char *arch, *vendor, *os, *env;
    arch = vendor = os = env = "";
    const char *delimiter    = "-";
    char *      tmp          = strdup(normalized_triple);
    char *      token        = strtok(tmp, delimiter);
    s32         state        = 0;
    while (token) {
        switch (state++) {
        case 0:
            arch = token;
            break;
        case 1:
            vendor = token;
            break;
        case 2:
            os = token;
            break;
        case 3:
            env = token;
            break;
        default:
            break;
        }
        token = strtok(NULL, delimiter);
    }

    out_triple->arch = ARCH_unknown;
    for (usize i = 0; i < TARRAY_SIZE(arch_names); ++i) {
        if (strcmp(arch, arch_names[i]) == 0) {
            out_triple->arch = i;
            break;
        }
    }

    out_triple->vendor = VENDOR_unknown;
    for (usize i = 0; i < TARRAY_SIZE(vendor_names); ++i) {
        if (strcmp(vendor, vendor_names[i]) == 0) {
            out_triple->vendor = i;
            break;
        }
    }

    out_triple->os = OS_unknown;
    for (usize i = 0; i < TARRAY_SIZE(os_names); ++i) {
        if (strncmp(os, os_names[i], strlen(os_names[i])) == 0) {
            out_triple->os = i;
            break;
        }
    }

    out_triple->env = ENV_unknown;
    for (usize i = 0; i < TARRAY_SIZE(env_names); ++i) {
        if (strcmp(env, env_names[i]) == 0) {
            out_triple->env = i;
            break;
        }
    }
    free(tmp);
}

static void llvm_init(struct assembly *assembly)
{
    // init LLVM
    char *triple    = target_triple_to_string(&assembly->target->triple);
    char *cpu       = /*LLVMGetHostCPUName()*/ "";
    char *features  = /*LLVMGetHostCPUFeatures()*/ "";
    char *error_msg = NULL;
    builder_log("Target: %s", triple);
    LLVMTargetRef llvm_target = NULL;
    if (LLVMGetTargetFromTriple(triple, &llvm_target, &error_msg)) {
        builder_error("Cannot get target with error: %s!", error_msg);
        LLVMDisposeMessage(error_msg);
        builder_error("Available targets are:");
        LLVMTargetRef target = LLVMGetFirstTarget();
        while (target) {
            builder_error("  %s", LLVMGetTargetName(target));
            target = LLVMGetNextTarget(target);
        }
        BL_ABORT("Cannot get target");
    }
    LLVMContextRef llvm_context = LLVMContextCreate();
    LLVMModuleRef  llvm_module =
        LLVMModuleCreateWithNameInContext(assembly->target->name, llvm_context);
    LLVMRelocMode reloc_mode = LLVMRelocDefault;
    switch (assembly->target->kind) {
    case ASSEMBLY_SHARED_LIB:
        reloc_mode = LLVMRelocPIC;
        break;
    default:
        break;
    }
    LLVMTargetMachineRef llvm_tm = LLVMCreateTargetMachine(llvm_target,
                                                           triple,
                                                           cpu,
                                                           features,
                                                           opt_to_LLVM(assembly->target->opt),
                                                           reloc_mode,
                                                           LLVMCodeModelDefault);

    LLVMTargetDataRef llvm_td = LLVMCreateTargetDataLayout(llvm_tm);
    LLVMSetModuleDataLayout(llvm_module, llvm_td);
    LLVMSetTarget(llvm_module, triple);
    assembly->llvm.ctx    = llvm_context;
    assembly->llvm.module = llvm_module;
    assembly->llvm.TM     = llvm_tm;
    assembly->llvm.TD     = llvm_td;
    assembly->llvm.triple = triple;
}

static void llvm_terminate(struct assembly *assembly)
{
    LLVMDisposeModule(assembly->llvm.module);
    LLVMDisposeTargetMachine(assembly->llvm.TM);
    LLVMDisposeTargetData(assembly->llvm.TD);
    LLVMContextDispose(assembly->llvm.ctx);
    bl_free(assembly->llvm.triple);
}

static void native_lib_terminate(struct native_lib *lib)
{
    if (lib->handle) dlFreeLibrary(lib->handle);
    if (lib->is_internal) return;
    free(lib->filename);
    free(lib->filepath);
    free(lib->dir);
    free(lib->user_name);
}

static void mir_init(struct assembly *assembly)
{
    mir_arenas_init(&assembly->arenas.mir);
    tarray_init(&assembly->MIR.global_instrs, sizeof(struct mir_instr *));
    tarray_init(&assembly->MIR.exported_instrs, sizeof(struct mir_instr *));
    thtbl_init(&assembly->MIR.RTTI_table, sizeof(struct mir_var *), 2048);
}

static INLINE void mir_terminate(struct assembly *assembly)
{
    thtbl_terminate(&assembly->MIR.RTTI_table);
    tarray_terminate(&assembly->MIR.global_instrs);
    tarray_terminate(&assembly->MIR.exported_instrs);
    mir_arenas_terminate(&assembly->arenas.mir);
}

static INLINE void set_default_out_dir(TString *dir)
{
    char path[PATH_MAX] = {0};
    if (!get_current_working_dir(&path[0], PATH_MAX)) {
        builder_error("Cannot get current working directory!");
        return;
    }
    tstring_clear(dir);
    tstring_append(dir, path);
}

// Create directory tree and set out_path.
static bool create_auxiliary_dir_tree_if_not_exist(const char *_path, TString *out_path)
{
    BL_ASSERT(_path);
    BL_ASSERT(out_path);
#if BL_PLATFORM_WIN
    char *path = strdup(_path);
    if (!path) BL_ABORT("Invalid directory copy.");
    win_path_to_unix(path, strlen(path));
#else
    const char *path  = _path;
#endif
    if (!dir_exists(path)) {
        if (!create_dir_tree(path)) {
#if BL_PLATFORM_WIN
            free(path);
#endif
            return false;
        }
    }
    char full_path[PATH_MAX] = {0};
    if (!brealpath(path, full_path, PATH_MAX)) {
        return false;
    }
    tstring_clear(out_path);
    tstring_append(out_path, full_path);
#if BL_PLATFORM_WIN
    free(path);
#endif
    return true;
}

static conf_data_t *load_module_config(const char *modulepath, struct token *import_from)
{
    char tmp_path[PATH_MAX] = {0};
    snprintf(tmp_path, TARRAY_SIZE(tmp_path), "%s/%s", modulepath, MODULE_CONFIG_FILE);
    conf_data_t *config = conf_data_new();
    if (builder_compile_config(tmp_path, config, import_from) != COMPILE_OK) {
        conf_data_delete(config);
        return false;
    }
    return config;
}

static INLINE s32 get_module_version(conf_data_t *config)
{
    BL_ASSERT(config);
    if (conf_data_has_key(config, CONF_MODULE_VERSION)) { // optional version
        return conf_data_get_int(config, CONF_MODULE_VERSION);
    }
    return 0;
}

static bool import_module(struct assembly *assembly,
                          conf_data_t *    config,
                          const char *     modulepath,
                          struct token *   import_from)
{
    BL_ASSERT(config);
    BL_ASSERT(modulepath);
    const s32 version = get_module_version(config);
    builder_log("Import module '%s' version %d.", modulepath, version);
    if (!conf_data_has_key(config, CONF_MODULE_ENTRY)) {
        builder_msg(
            BUILDER_MSG_ERROR,
            ERR_MISSING_PLATFORM,
            TOKEN_OPTIONAL_LOCATION(import_from),
            BUILDER_CUR_WORD,
            "Module doesn't support current target platform, configuration entry ('%s') not "
            "found in module config file.",
            CONF_MODULE_ENTRY);
        return false;
    }

    { // entry file
        const char *entry_file = conf_data_get_str(config, CONF_MODULE_ENTRY);
        BL_ASSERT(entry_file && strlen(entry_file) > 0);
        TString *entry_file_path = get_tmpstr();
        tstring_setf(entry_file_path, "%s/%s", modulepath, entry_file);
        assembly_add_unit(assembly, entry_file_path->data, NULL);
        put_tmpstr(entry_file_path);
    }

    // Optional lib path
    if (conf_data_has_key(config, CONF_MODULE_LIB_PATH)) {
        const char *lib_path = conf_data_get_str(config, CONF_MODULE_LIB_PATH);
        BL_ASSERT(lib_path && strlen(lib_path) > 0);
        TString *path = get_tmpstr();
        tstring_setf(path, "%s/%s", modulepath, lib_path);
        if (!dir_exists(path->data)) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_FILE_NOT_FOUND,
                        TOKEN_OPTIONAL_LOCATION(import_from),
                        BUILDER_CUR_WORD,
                        "Cannot find module imported library path '%s' defined by '%s'.",
                        path->data,
                        CONF_MODULE_LIB_PATH);
            put_tmpstr(path);
            return false;
        }
        assembly_add_lib_path(assembly, path->data);
        put_tmpstr(path);
    }

    // Optional linker options
    if (conf_data_has_key(config, CONF_MODULE_LINKER_OPT)) {
        const char *opt = conf_data_get_str(config, CONF_MODULE_LINKER_OPT);
        BL_ASSERT(opt && strlen(opt) > 0);
        assembly_append_linker_options(assembly, opt);
    }

    // Optional libs
    if (conf_data_has_key(config, CONF_MODULE_LINK)) {
        const char *lib = conf_data_get_str(config, CONF_MODULE_LINK);
        BL_ASSERT(lib && strlen(lib) > 0);
        assembly_add_native_lib(assembly, lib, NULL);
    }

    return true;
}

// =================================================================================================
// PUBLIC
// =================================================================================================
struct target *target_new(const char *name)
{
    BL_ASSERT(name && "struct assembly name not specified!");
    struct target *target = bl_malloc(sizeof(struct target));
    memset(target, 0, sizeof(struct target));
    BL_MAGIC_SET(target);
    tarray_init(&target->files, sizeof(char *));
    tarray_init(&target->default_lib_paths, sizeof(char *));
    tarray_init(&target->default_libs, sizeof(char *));
    tstring_init(&target->default_custom_linker_opt);
    tstring_init(&target->module_dir);
    tstring_init(&target->out_dir);
    target->name = strdup(name);

    set_default_out_dir(&target->out_dir);

    // Setup some defaults.
    target->opt           = ASSEMBLY_OPT_DEBUG;
    target->kind          = ASSEMBLY_EXECUTABLE;
    target->module_policy = IMPORT_POLICY_SYSTEM;
    target->reg_split     = true;
#ifdef BL_DEBUG
    target->verify_llvm = true;
#endif

#if BL_PLATFORM_WIN
    target->di        = ASSEMBLY_DI_CODEVIEW;
    target->copy_deps = true;
#else
    target->di        = ASSEMBLY_DI_DWARF;
    target->copy_deps = false;
#endif
    target->triple = (struct target_triple){
        .arch = ARCH_unknown, .vendor = VENDOR_unknown, .os = OS_unknown, .env = ENV_unknown};
    return target;
}

struct target *target_dup(const char *name, const struct target *other)
{
    BL_MAGIC_ASSERT(other);
    struct target *target = target_new(name);
    memcpy(target, other, sizeof(struct {TARGET_COPYABLE_CONTENT}));
    target_set_output_dir(target, other->out_dir.data);
    target->vm = other->vm;
    BL_MAGIC_SET(target);
    return target;
}

void target_delete(struct target *target)
{
    char *file;
    TARRAY_FOREACH(char *, &target->files, file) free(file);

    char *path;
    TARRAY_FOREACH(char *, &target->default_lib_paths, path) free(path);

    char *lib;
    TARRAY_FOREACH(char *, &target->default_libs, lib) free(lib);

    tarray_terminate(&target->files);
    tarray_terminate(&target->default_lib_paths);
    tarray_terminate(&target->default_libs);
    tstring_terminate(&target->out_dir);
    tstring_terminate(&target->default_custom_linker_opt);
    tstring_terminate(&target->module_dir);
    free(target->name);
    bl_free(target);
}

void target_add_file(struct target *target, const char *filepath)
{
    BL_MAGIC_ASSERT(target);
    BL_ASSERT(filepath && "Invalid filepath!");
    char *dup = strdup(filepath);
    tarray_push(&target->files, dup);
}

void target_set_vm_args(struct target *target, s32 argc, char **argv)
{
    BL_MAGIC_ASSERT(target);
    target->vm.argc = argc;
    target->vm.argv = argv;
}

void target_add_lib_path(struct target *target, const char *path)
{
    BL_MAGIC_ASSERT(target);
    if (!path) return;
    char *tmp = strdup(path);
    if (!tmp) return;
    tarray_push(&target->default_lib_paths, tmp);
}

void target_add_lib(struct target *target, const char *lib)
{
    BL_MAGIC_ASSERT(target);
    if (!lib) return;
    char *tmp = strdup(lib);
    if (!tmp) return;
    tarray_push(&target->default_libs, tmp);
}

void target_set_output_dir(struct target *target, const char *dir)
{
    BL_MAGIC_ASSERT(target);
    if (!dir) builder_error("Cannot create output directory.");
    if (!create_auxiliary_dir_tree_if_not_exist(dir, &target->out_dir)) {
        builder_error("Cannot create output directory '%s'.", dir);
    }
}

void target_append_linker_options(struct target *target, const char *option)
{
    BL_MAGIC_ASSERT(target);
    if (!option) return;
    tstring_append(&target->default_custom_linker_opt, option);
    tstring_append(&target->default_custom_linker_opt, " ");
}

void target_set_module_dir(struct target *target, const char *dir, enum module_import_policy policy)
{
    BL_MAGIC_ASSERT(target);
    if (!dir) {
        builder_error("Cannot create module directory.");
        return;
    }
    if (!create_auxiliary_dir_tree_if_not_exist(dir, &target->module_dir)) {
        builder_error("Cannot create module directory '%s'.", dir);
        return;
    }
    target->module_policy = policy;
}

bool target_is_triple_valid(struct target_triple *triple)
{
    if (triple->arch == ARCH_unknown) return false;
    if (triple->os == OS_unknown) return false;
    // @INCOMPLETE Consider validation of other fields???
    return true;
}

bool target_init_default_triple(struct target_triple *triple)
{
    char *llvm_triple = LLVMGetDefaultTargetTriple();
    parse_triple(llvm_triple, triple);
    if (!target_is_triple_valid(triple)) {
        builder_error("Default target triple '%s' is not supported by the compiler.", llvm_triple);
        LLVMDisposeMessage(llvm_triple);
        return false;
    }
    LLVMDisposeMessage(llvm_triple);
    return true;
}

char *target_triple_to_string(const struct target_triple *triple)
{
    const char *arch   = "";
    const char *vendor = "";
    const char *os     = "";
    const char *env    = "";
    if (triple->arch < TARRAY_SIZE(arch_names)) arch = arch_names[triple->arch];
    if (triple->vendor < TARRAY_SIZE(vendor_names)) vendor = vendor_names[triple->vendor];
    if (triple->os < TARRAY_SIZE(os_names)) os = os_names[triple->os];
    if (triple->env < TARRAY_SIZE(env_names)) env = env_names[triple->env];
    const usize len =
        strlen(arch) + strlen(vendor) + strlen(os) + strlen(env) + 4; // 3x'-' + terminator
    char *str = bl_malloc(len);
    snprintf(str, len, "%s-%s-%s-%s", arch, vendor, os, env);
    return str;
}

struct assembly *assembly_new(const struct target *target)
{
    BL_MAGIC_ASSERT(target);
    struct assembly *assembly = bl_malloc(sizeof(struct assembly));
    memset(assembly, 0, sizeof(struct assembly));
    assembly->target = target;
    assembly->sync   = sync_new();

    llvm_init(assembly);
    tarray_init(&assembly->units, sizeof(struct unit *));
    tstring_init(&assembly->custom_linker_opt);
    tarray_init(&assembly->libs, sizeof(struct native_lib));
    tarray_init(&assembly->lib_paths, sizeof(char *));
    tarray_init(&assembly->testing.cases, sizeof(struct mir_fn *));
    vm_init(&assembly->vm, VM_STACK_SIZE);

    // set defaults
    scope_arenas_init(&assembly->arenas.scope);
    ast_arena_init(&assembly->arenas.ast);
    arena_init(&assembly->arenas.array,
               sizeof(TArray *),
               alignment_of(TArray *),
               EXPECTED_ARRAY_COUNT,
               (arena_elem_dtor_t)tarray_dtor);
    arena_init(&assembly->arenas.small_array,
               sizeof(union _SmallArrays),
               alignment_of(union _SmallArrays),
               EXPECTED_ARRAY_COUNT,
               (arena_elem_dtor_t)small_array_dtor);
    assembly->gscope =
        scope_create(&assembly->arenas.scope, SCOPE_GLOBAL, NULL, EXPECTED_GSCOPE_COUNT, NULL);

    dl_init(assembly);
    mir_init(assembly);

    // Add units from target
    for (usize i = 0; i < target->files.size; ++i) {
        char *file = tarray_at(char *, (TArray *)&target->files, i);
        assembly_add_unit(assembly, file, NULL);
    }

    // Add default units based on assembly kind
    switch (assembly->target->kind) {
    case ASSEMBLY_EXECUTABLE:
        if (assembly->target->no_api) break;
        assembly_add_unit(assembly, BUILTIN_FILE, NULL);
        assembly_add_unit(assembly, OS_PRELOAD_FILE, NULL);
        break;
    case ASSEMBLY_SHARED_LIB:
        if (assembly->target->no_api) break;
        assembly_add_unit(assembly, BUILTIN_FILE, NULL);
        assembly_add_unit(assembly, OS_PRELOAD_FILE, NULL);
        break;
    case ASSEMBLY_BUILD_PIPELINE:
        assembly_add_unit(assembly, BUILTIN_FILE, NULL);
        assembly_add_unit(assembly, OS_PRELOAD_FILE, NULL);
        assembly_add_unit(assembly, BUILD_API_FILE, NULL);
        assembly_add_unit(assembly, BUILD_SCRIPT_FILE, NULL);
        break;
    case ASSEMBLY_DOCS:
        break;
    }

    // Duplicate default library paths
    char *lib_path;
    TARRAY_FOREACH(char *, (TArray *)&target->default_lib_paths, lib_path)
    {
        assembly_add_lib_path(assembly, lib_path);
    }

    // Duplicate default libs
    char *lib;
    TARRAY_FOREACH(char *, (TArray *)&target->default_libs, lib)
    {
        assembly_add_native_lib(assembly, lib, NULL);
    }

    // Append custom linker options
    assembly_append_linker_options(assembly, target->default_custom_linker_opt.data);

    return assembly;
}

void assembly_delete(struct assembly *assembly)
{
    ZONE();
    struct unit *unit;
    TARRAY_FOREACH(struct unit *, &assembly->units, unit)
    {
        unit_delete(unit);
    }

    struct native_lib *lib;
    for (usize i = 0; i < assembly->libs.size; ++i) {
        lib = &tarray_at(struct native_lib, &assembly->libs, i);
        native_lib_terminate(lib);
    }

    char *p;
    TARRAY_FOREACH(char *, &assembly->lib_paths, p) free(p);

    tarray_terminate(&assembly->libs);
    tarray_terminate(&assembly->lib_paths);
    tarray_terminate(&assembly->testing.cases);
    tarray_terminate(&assembly->units);

    tstring_terminate(&assembly->custom_linker_opt);
    vm_terminate(&assembly->vm);
    arena_terminate(&assembly->arenas.small_array);
    arena_terminate(&assembly->arenas.array);
    ast_arena_terminate(&assembly->arenas.ast);
    scope_arenas_terminate(&assembly->arenas.scope);
    llvm_terminate(assembly);
    dl_terminate(assembly);
    mir_terminate(assembly);
    sync_delete(assembly->sync);
    bl_free(assembly);
    RETURN_ZONE();
}

void assembly_add_lib_path(struct assembly *assembly, const char *path)
{
    if (!path) return;
    char *tmp = strdup(path);
    if (!tmp) return;
    tarray_push(&assembly->lib_paths, tmp);
}

void assembly_append_linker_options(struct assembly *assembly, const char *opt)
{
    if (!opt) return;
    tstring_append(&assembly->custom_linker_opt, opt);
    tstring_append(&assembly->custom_linker_opt, " ");
}

static INLINE bool assembly_has_unit(struct assembly *assembly, const u64 hash)
{
    struct unit *unit;
    TARRAY_FOREACH(struct unit *, &assembly->units, unit)
    {
        if (hash == unit->hash) return true;
    }
    return false;
}

struct unit *
assembly_add_unit(struct assembly *assembly, const char *filepath, struct token *load_from)
{
    AssemblySyncImpl *sync = assembly->sync;
    pthread_mutex_lock(&sync->units_lock);
    struct unit *unit = NULL;
    const u64    hash = unit_hash(filepath, load_from);
    if (assembly_has_unit(assembly, hash)) {
        goto DONE;
    }

    unit = unit_new(filepath, load_from);
    tarray_push(&assembly->units, unit);
    builder_async_submit_unit(unit);

DONE:
    pthread_mutex_unlock(&sync->units_lock);
    return unit;
}

void assembly_add_native_lib(struct assembly *assembly,
                             const char *     lib_name,
                             struct token *   link_token)
{
    const u64 hash = thash_from_str(lib_name);
    { // Search for duplicity.
        struct native_lib *lib;
        for (usize i = 0; i < assembly->libs.size; ++i) {
            lib = &tarray_at(struct native_lib, &assembly->libs, i);
            if (lib->hash == hash) return;
        }
    }
    struct native_lib lib = {0};
    lib.hash              = hash;
    lib.user_name         = strdup(lib_name);
    lib.linked_from       = link_token;
    tarray_push(&assembly->libs, lib);
}

static INLINE bool module_exist(const char *module_dir, const char *modulepath)
{
    TString *local_conf_path = get_tmpstr();
    tstring_setf(local_conf_path, "%s/%s/%s", module_dir, modulepath, MODULE_CONFIG_FILE);
    const bool found = search_source_file(local_conf_path->data, SEARCH_FLAG_ABS, NULL, NULL, NULL);
    put_tmpstr(local_conf_path);
    return found;
}

bool assembly_import_module(struct assembly *assembly,
                            const char *     modulepath,
                            struct token *   import_from)
{
    AssemblySyncImpl *sync = assembly->sync;
    pthread_mutex_lock(&sync->import_module_lock);
    bool state = false;
    if (!strlen(modulepath)) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_FILE_NOT_FOUND,
                    TOKEN_OPTIONAL_LOCATION(import_from),
                    BUILDER_CUR_WORD,
                    "Module name is empty.");
        goto DONE;
    }

    TString *            local_path = get_tmpstr();
    conf_data_t *        config     = NULL;
    const struct target *target     = assembly->target;
    const char *         module_dir = target->module_dir.len > 0 ? target->module_dir.data : NULL;
    const enum module_import_policy policy = assembly->target->module_policy;
    const bool local_found = module_dir ? module_exist(module_dir, modulepath) : false;
    switch (policy) {
    case IMPORT_POLICY_SYSTEM: {
        if (local_found) {
            tstring_setf(local_path, "%s/%s", module_dir, modulepath);
            config = load_module_config(local_path->data, import_from);
        } else {
            tstring_setf(local_path, "%s/%s", builder_get_lib_dir(), modulepath);
            config = load_module_config(local_path->data, import_from);
        }
        break;
    }

    case IMPORT_POLICY_BUNDLE_LATEST:
    case IMPORT_POLICY_BUNDLE: {
        BL_ASSERT(module_dir);
        TString *  system_path   = get_tmpstr();
        const bool check_version = policy == IMPORT_POLICY_BUNDLE_LATEST;
        tstring_setf(local_path, "%s/%s", module_dir, modulepath);
        tstring_setf(system_path, "%s/%s", builder_get_lib_dir(), modulepath);
        const bool system_found = module_exist(builder_get_lib_dir(), modulepath);
        // Check if module is present in module directory.
        bool do_copy = !local_found;
        if (check_version && local_found && system_found) {
            s32 system_version = 0;
            s32 local_version  = 0;
            tstring_setf(system_path, "%s/%s", builder_get_lib_dir(), modulepath);
            config = load_module_config(system_path->data, import_from);
            if (config) system_version = get_module_version(config);
            conf_data_t *local_config = load_module_config(local_path->data, import_from);
            if (local_config) local_version = get_module_version(local_config);
            conf_data_delete(local_config);
            do_copy = system_version > local_version;
        }
        if (do_copy) {
            // Delete old one.
            if (local_found) {
                TString *backup_name = get_tmpstr();
                char     date[26];
                date_time(date, TARRAY_SIZE(date), "%d-%m-%Y_%H-%M-%S");
                tstring_setf(backup_name, "%s_%s.bak", local_path->data, date);
                copy_dir(local_path->data, backup_name->data);
                remove_dir(local_path->data);
                builder_warning("Backup module '%s'.", backup_name->data);
                put_tmpstr(backup_name);
            }
            // Copy module from system to module directory.
            builder_warning("%s module '%s' in '%s'.",
                            (check_version && local_found) ? "Update" : "Import",
                            modulepath,
                            module_dir);
            if (!copy_dir(system_path->data, local_path->data)) {
                builder_error("Cannot import module '%s'.", modulepath);
            }
        }
        if (!config) config = load_module_config(local_path->data, import_from);
        put_tmpstr(system_path);
        break;
    }

    default:
        BL_ASSERT("Invalid module import policy!");
    }
    if (config) {
        builder_log("%s", local_path->data);
        state = import_module(assembly, config, local_path->data, import_from);
    }
    put_tmpstr(local_path);
    conf_data_delete(config);
DONE:
    pthread_mutex_unlock(&sync->import_module_lock);
    return state;
}

DCpointer assembly_find_extern(struct assembly *assembly, const char *symbol)
{
    void *             handle = NULL;
    struct native_lib *lib;
    for (usize i = 0; i < assembly->libs.size; ++i) {
        lib    = &tarray_at(struct native_lib, &assembly->libs, i);
        handle = dlFindSymbol(lib->handle, symbol);
        if (handle) break;
    }
    return handle;
}
