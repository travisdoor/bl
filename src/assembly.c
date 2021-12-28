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
#if BL_PLATFORM_WIN
#include "winpthreads.h"
#else
#include <pthread.h>
#endif

#include "builder.h"
#include "stb_ds.h"
#include <string.h>

#define EXPECTED_ARRAY_COUNT 256

const char *arch_names[] = {
#define GEN_ARCH
#define entry(X) #X,
#include "target.def"
#undef entry
#undef GEN_ARCH
};

const char *vendor_names[] = {
#define GEN_VENDOR
#define entry(X) #X,
#include "target.def"
#undef entry
#undef GEN_VENDOR
};

const char *os_names[] = {
#define GEN_OS
#define entry(X) #X,
#include "target.def"
#undef entry
#undef GEN_OS
};

const char *env_names[] = {
#define GEN_ENV
#define entry(X) #X,
#include "target.def"
#undef entry
#undef GEN_ENV
};

static const usize sarr_total_size = sizeof(union {
    ast_nodes_t        _1;
    mir_args_t         _2;
    mir_fns_t          _3;
    mir_types_t        _4;
    mir_members_t      _5;
    mir_variants_t     _6;
    mir_instrs_t       _7;
    mir_switch_cases_t _8;
});

static void sarr_dtor(sarr_any_t *arr)
{
    sarrfree(arr);
}

typedef struct AssemblySyncImpl {
    pthread_mutex_t units_lock;
} AssemblySyncImpl;

static AssemblySyncImpl *sync_new(void)
{
    AssemblySyncImpl *impl = bmalloc(sizeof(AssemblySyncImpl));
    pthread_mutex_init(&impl->units_lock, NULL);
    return impl;
}

static void sync_delete(AssemblySyncImpl *impl)
{
    pthread_mutex_destroy(&impl->units_lock);
    bfree(impl);
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

static void parse_triple(const char *llvm_triple, struct target_triple *out_triple)
{
    bassert(out_triple);
    char *arch, *vendor, *os, *env;
    arch = vendor = os = env = "";
    const char *delimiter    = "-";
    char       *tmp          = strdup(llvm_triple);
    char       *token        = strtok(tmp, delimiter);
    s32         state        = 0;
    // arch-vendor-os-evironment
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
    for (usize i = 0; i < static_arrlenu(arch_names); ++i) {
        if (strcmp(arch, arch_names[i]) == 0) {
            out_triple->arch = i;
            break;
        }
    }

    out_triple->vendor = VENDOR_unknown;
    for (usize i = 0; i < static_arrlenu(vendor_names); ++i) {
        if (strncmp(vendor, vendor_names[i], strlen(vendor_names[i])) == 0) {
            out_triple->vendor = i;
            break;
        }
    }

    out_triple->os = OS_unknown;
    for (usize i = 0; i < static_arrlenu(os_names); ++i) {
        if (strncmp(os, os_names[i], strlen(os_names[i])) == 0) {
            out_triple->os = i;
            break;
        }
    }

    out_triple->env = ENV_unknown;
    for (usize i = 0; i < static_arrlenu(env_names); ++i) {
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
        babort("Cannot get target");
    }
    LLVMContextRef llvm_context = LLVMContextCreate();
    LLVMRelocMode  reloc_mode   = LLVMRelocDefault;
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
    assembly->llvm.ctx        = llvm_context;
    assembly->llvm.TM         = llvm_tm;
    assembly->llvm.TD         = llvm_td;
    assembly->llvm.triple     = triple;
}

static void llvm_terminate(struct assembly *assembly)
{
    for (usize i = 0; i < arrlenu(assembly->llvm.modules); ++i) {
        LLVMDisposeModule(assembly->llvm.modules[i]);
    }
    arrfree(assembly->llvm.modules);
    LLVMDisposeTargetMachine(assembly->llvm.TM);
    LLVMDisposeTargetData(assembly->llvm.TD);
    LLVMContextDispose(assembly->llvm.ctx);
    bfree(assembly->llvm.triple);
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
    arrsetcap(assembly->MIR.global_instrs, 1024);
    arrsetcap(assembly->MIR.exported_instrs, 256);
}

static INLINE void mir_terminate(struct assembly *assembly)
{
    hmfree(assembly->MIR.rtti_table);
    arrfree(assembly->MIR.global_instrs);
    arrfree(assembly->MIR.exported_instrs);
    mir_arenas_terminate(&assembly->arenas.mir);
}

static INLINE void set_default_out_dir(char **dir)
{
    char path[PATH_MAX] = {0};
    if (!get_current_working_dir(&path[0], PATH_MAX)) {
        builder_error("Cannot get current working directory!");
        return;
    }
    strprint(*dir, "%s", path);
}

// Create directory tree and set out_path.
static bool create_auxiliary_dir_tree_if_not_exist(const char *_path, char **out_path)
{
    bassert(_path);
    bassert(out_path);
#if BL_PLATFORM_WIN
    char *path = strdup(_path);
    if (!path) babort("Invalid directory copy.");
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
    strprint(*out_path, "%s", full_path);
#if BL_PLATFORM_WIN
    free(path);
#endif
    return true;
}

static conf_data_t *load_module_config(const char *modulepath, struct token *import_from)
{
    char *path = gettmpstr();
    strprint(path, "%s/%s", modulepath, MODULE_CONFIG_FILE);
    conf_data_t *config = conf_data_new();
    if (builder_compile_config(path, config, import_from) != COMPILE_OK) {
        conf_data_delete(config);
        puttmpstr(path);
        return NULL;
    }
    puttmpstr(path);
    return config;
}

static INLINE s32 get_module_version(conf_data_t *config)
{
    bassert(config);
    if (conf_data_has_key(config, CONF_MODULE_VERSION)) { // optional version
        return conf_data_get_int(config, CONF_MODULE_VERSION);
    }
    return 0;
}

static bool import_module(struct assembly *assembly,
                          conf_data_t     *config,
                          const char      *modulepath,
                          struct token    *import_from)
{
    zone();
    bassert(config);
    bassert(modulepath);
    const s32 version = get_module_version(config);
    builder_log("Import module '%s' version %d.", modulepath, version);
    if (!conf_data_has_key(config, CONF_MODULE_ENTRY)) {
        builder_msg(
            MSG_ERR,
            ERR_MISSING_PLATFORM,
            TOKEN_OPTIONAL_LOCATION(import_from),
            CARET_WORD,
            "Module doesn't support current target platform, configuration entry ('%s') not "
            "found in module config file.",
            CONF_MODULE_ENTRY);
        return_zone(false);
    }

    { // entry file
        const char *entry_file = conf_data_get_str(config, CONF_MODULE_ENTRY);
        bassert(entry_file && strlen(entry_file) > 0);
        char *entry_file_path = gettmpstr();
        strprint(entry_file_path, "%s/%s", modulepath, entry_file);
        assembly_add_unit_safe(assembly, entry_file_path, NULL);
        puttmpstr(entry_file_path);
    }

    // Optional lib path
    if (conf_data_has_key(config, CONF_MODULE_LIB_PATH)) {
        const char *lib_path = conf_data_get_str(config, CONF_MODULE_LIB_PATH);
        bassert(lib_path && strlen(lib_path) > 0);
        char *path = gettmpstr();
        strprint(path, "%s/%s", modulepath, lib_path);
        if (!dir_exists(path)) {
            builder_msg(MSG_ERR,
                        ERR_FILE_NOT_FOUND,
                        TOKEN_OPTIONAL_LOCATION(import_from),
                        CARET_WORD,
                        "Cannot find module imported library path '%s' defined by '%s'.",
                        path,
                        CONF_MODULE_LIB_PATH);
            puttmpstr(path);
            return_zone(false);
        }
        assembly_add_lib_path(assembly, path);
        puttmpstr(path);
    }

    // Optional linker options
    if (conf_data_has_key(config, CONF_MODULE_LINKER_OPT)) {
        const char *opt = conf_data_get_str(config, CONF_MODULE_LINKER_OPT);
        bassert(opt && strlen(opt) > 0);
        assembly_append_linker_options(assembly, opt);
    }

    // Optional libs
    if (conf_data_has_key(config, CONF_MODULE_LINK)) {
        const char *lib = conf_data_get_str(config, CONF_MODULE_LINK);
        bassert(lib && strlen(lib) > 0);
        assembly_add_native_lib(assembly, lib, NULL);
    }
    return_zone(true);
}

// =================================================================================================
// PUBLIC
// =================================================================================================
struct target *target_new(const char *name)
{
    bassert(name && "struct assembly name not specified!");
    struct target *target = bmalloc(sizeof(struct target));
    memset(target, 0, sizeof(struct target));
    bmagic_set(target);
    strinit(target->default_custom_linker_opt, 128);
    strinit(target->module_dir, 128);
    strinit(target->out_dir, 128);
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
    bmagic_assert(other);
    struct target *target = target_new(name);
    memcpy(target, other, sizeof(struct {TARGET_COPYABLE_CONTENT}));
    target_set_output_dir(target, other->out_dir);
    target->vm = other->vm;
    bmagic_set(target);
    return target;
}

void target_delete(struct target *target)
{
    for (usize i = 0; i < arrlenu(target->files); ++i)
        free(target->files[i]);
    for (usize i = 0; i < arrlenu(target->default_lib_paths); ++i)
        free(target->default_lib_paths[i]);
    for (usize i = 0; i < arrlenu(target->default_libs); ++i)
        free(target->default_libs[i]);

    arrfree(target->files);
    arrfree(target->default_lib_paths);
    arrfree(target->default_libs);
    strfree(target->out_dir);
    strfree(target->default_custom_linker_opt);
    strfree(target->module_dir);
    free(target->name);
    bfree(target);
}

void target_add_file(struct target *target, const char *filepath)
{
    bmagic_assert(target);
    bassert(filepath && "Invalid filepath!");
    char *dup = strdup(filepath);
    win_path_to_unix(dup, strlen(dup));
    arrput(target->files, dup);
}

void target_set_vm_args(struct target *target, s32 argc, char **argv)
{
    bmagic_assert(target);
    target->vm.argc = argc;
    target->vm.argv = argv;
}

void target_add_lib_path(struct target *target, const char *path)
{
    bmagic_assert(target);
    if (!path) return;
    char *dup = strdup(path);
    win_path_to_unix(dup, strlen(dup));
    arrput(target->default_lib_paths, dup);
}

void target_add_lib(struct target *target, const char *lib)
{
    bmagic_assert(target);
    if (!lib) return;
    char *dup = strdup(lib);
    win_path_to_unix(dup, strlen(dup));
    arrput(target->default_libs, dup);
}

void target_set_output_dir(struct target *target, const char *dir)
{
    bmagic_assert(target);
    if (!dir) builder_error("Cannot create output directory.");
    if (!create_auxiliary_dir_tree_if_not_exist(dir, &target->out_dir)) {
        builder_error("Cannot create output directory '%s'.", dir);
    }
}

void target_append_linker_options(struct target *target, const char *option)
{
    bmagic_assert(target);
    if (!option) return;
    strappend(target->default_custom_linker_opt, "%s ", option);
}

void target_set_module_dir(struct target *target, const char *dir, enum module_import_policy policy)
{
    bmagic_assert(target);
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
    char  *str      = target_triple_to_string(triple);
    bool   is_valid = false;
    char **list     = builder_get_supported_targets();
    char **it       = list;
    for (; *it; it++) {
        if (strcmp(str, *it) == 0) {
            is_valid = true;
            break;
        }
    }
    free(str);
    bfree(list);
    return is_valid;
}

bool target_init_default_triple(struct target_triple *triple)
{
    char *llvm_triple = LLVMGetDefaultTargetTriple();
    parse_triple(llvm_triple, triple);
    if (!target_is_triple_valid(triple)) {
        builder_error("Target triple '%s' is not supported by the compiler.", llvm_triple);
        LLVMDisposeMessage(llvm_triple);
        return false;
    }
    LLVMDisposeMessage(llvm_triple);
    return true;
}

char *target_triple_to_string(const struct target_triple *triple)
{
    const char *arch, *vendor, *os, *env;
    arch = vendor = os = env = "";
    if (triple->arch < static_arrlenu(arch_names)) arch = arch_names[triple->arch];
    if (triple->vendor < static_arrlenu(vendor_names)) vendor = vendor_names[triple->vendor];
    if (triple->os < static_arrlenu(os_names)) os = os_names[triple->os];
    if (triple->env < static_arrlenu(env_names)) env = env_names[triple->env];
    char *str = NULL;
    usize len = 0;
    if (triple->env == ENV_unknown) {
        len = snprintf(NULL, 0, "%s-%s-%s", arch, vendor, os) + 1;
        str = bmalloc(len);
        snprintf(str, len, "%s-%s-%s", arch, vendor, os);
    } else {
        len = snprintf(NULL, 0, "%s-%s-%s-%s", arch, vendor, os, env) + 1;
        str = bmalloc(len);
        snprintf(str, len, "%s-%s-%s-%s", arch, vendor, os, env);
    }
    bassert(str);
    return str;
}

struct assembly *assembly_new(const struct target *target)
{
    bmagic_assert(target);
    struct assembly *assembly = bmalloc(sizeof(struct assembly));
    memset(assembly, 0, sizeof(struct assembly));
    assembly->target = target;
    assembly->sync   = sync_new();

    llvm_init(assembly);
    arrsetcap(assembly->units, 64);
    strinit(assembly->custom_linker_opt, 128);
    vm_init(&assembly->vm, VM_STACK_SIZE);

    // set defaults
    scope_arenas_init(&assembly->arenas.scope);
    ast_arena_init(&assembly->arenas.ast);
    arena_init(&assembly->arenas.sarr,
               sarr_total_size,
               16, // Is this correct?
               EXPECTED_ARRAY_COUNT,
               (arena_elem_dtor_t)sarr_dtor);
    assembly->gscope = scope_create(&assembly->arenas.scope, SCOPE_GLOBAL, NULL, NULL);

    dl_init(assembly);
    mir_init(assembly);

    // Add units from target
    for (usize i = 0; i < arrlenu(target->files); ++i) {
        assembly_add_unit_safe(assembly, target->files[i], NULL);
    }

    const char *preload_file = builder_read_config(assembly->target, "preload_file", "");

    // Add default units based on assembly kind
    switch (assembly->target->kind) {
    case ASSEMBLY_EXECUTABLE:
        if (assembly->target->no_api) break;
        assembly_add_unit_safe(assembly, BUILTIN_FILE, NULL);
        assembly_add_unit_safe(assembly, preload_file, NULL);
        break;
    case ASSEMBLY_SHARED_LIB:
        if (assembly->target->no_api) break;
        assembly_add_unit_safe(assembly, BUILTIN_FILE, NULL);
        assembly_add_unit_safe(assembly, preload_file, NULL);
        break;
    case ASSEMBLY_BUILD_PIPELINE:
        assembly_add_unit_safe(assembly, BUILTIN_FILE, NULL);
        assembly_add_unit_safe(assembly, preload_file, NULL);
        assembly_add_unit_safe(assembly, BUILD_API_FILE, NULL);
        assembly_add_unit_safe(assembly, BUILD_SCRIPT_FILE, NULL);
        break;
    case ASSEMBLY_DOCS:
        break;
    }

    // Duplicate default library paths
    for (usize i = 0; i < arrlenu(target->default_lib_paths); ++i)
        assembly_add_lib_path(assembly, target->default_lib_paths[i]);

    // Duplicate default libs
    for (usize i = 0; i < arrlenu(target->default_libs); ++i)
        assembly_add_native_lib(assembly, target->default_libs[i], NULL);

    // Append custom linker options
    assembly_append_linker_options(assembly, target->default_custom_linker_opt);

    return assembly;
}

void assembly_delete(struct assembly *assembly)
{
    zone();
    for (usize i = 0; i < arrlenu(assembly->units); ++i)
        unit_delete(assembly->units[i]);
    for (usize i = 0; i < arrlenu(assembly->libs); ++i)
        native_lib_terminate(&assembly->libs[i]);
    for (usize i = 0; i < arrlenu(assembly->lib_paths); ++i)
        free(assembly->lib_paths[i]);

    arrfree(assembly->libs);
    arrfree(assembly->lib_paths);
    arrfree(assembly->testing.cases);
    arrfree(assembly->units);

    strfree(assembly->custom_linker_opt);
    vm_terminate(&assembly->vm);
    arena_terminate(&assembly->arenas.sarr);
    ast_arena_terminate(&assembly->arenas.ast);
    scope_arenas_terminate(&assembly->arenas.scope);
    llvm_terminate(assembly);
    dl_terminate(assembly);
    mir_terminate(assembly);
    sync_delete(assembly->sync);
    scfree(&assembly->string_cache);
    bfree(assembly);
    return_zone();
}

void assembly_add_lib_path(struct assembly *assembly, const char *path)
{
    if (!path) return;
    char *tmp = strdup(path);
    if (!tmp) return;
    arrput(assembly->lib_paths, tmp);
}

void assembly_append_linker_options(struct assembly *assembly, const char *opt)
{
    if (!opt) return;
    if (opt[0] == '\0') return;
    strappend(assembly->custom_linker_opt, "%s ", opt);
}

static INLINE bool assembly_has_unit(struct assembly *assembly, const hash_t hash)
{
    for (usize i = 0; i < arrlenu(assembly->units); ++i) {
        struct unit *unit = assembly->units[i];
        if (hash == unit->hash) {
            return true;
        }
    }
    return false;
}

struct unit *
assembly_add_unit_safe(struct assembly *assembly, const char *filepath, struct token *load_from)
{
    zone();
    if (filepath == NULL || filepath[0] == '\0') return_zone(NULL);
    struct unit      *unit = NULL;
    const hash_t      hash = unit_hash(filepath, load_from);
    AssemblySyncImpl *sync = assembly->sync;
    pthread_mutex_lock(&sync->units_lock);
    if (assembly_has_unit(assembly, hash)) goto DONE;
    unit = unit_new(filepath, load_from);
    arrput(assembly->units, unit);
    builder_async_submit_unit(unit);
DONE:
    pthread_mutex_unlock(&sync->units_lock);
    return_zone(unit);
}

void assembly_add_native_lib(struct assembly *assembly,
                             const char      *lib_name,
                             struct token    *link_token)
{
    const hash_t hash = strhash(lib_name);
    { // Search for duplicity.
        for (usize i = 0; i < arrlenu(assembly->libs); ++i) {
            struct native_lib *lib = &assembly->libs[i];
            if (lib->hash == hash) return;
        }
    }
    struct native_lib lib = {0};
    lib.hash              = hash;
    lib.user_name         = strdup(lib_name);
    lib.linked_from       = link_token;
    arrput(assembly->libs, lib);
}

static INLINE bool module_exist(const char *module_dir, const char *modulepath)
{
    char *local_conf_path = gettmpstr();
    strprint(local_conf_path, "%s/%s/%s", module_dir, modulepath, MODULE_CONFIG_FILE);
    const bool found = search_source_file(local_conf_path, SEARCH_FLAG_ABS, NULL, NULL, NULL);
    puttmpstr(local_conf_path);
    return found;
}

bool assembly_import_module(struct assembly *assembly,
                            const char      *modulepath,
                            struct token    *import_from)
{
    zone();
    bool state = false;
    if (!strlen(modulepath)) {
        builder_msg(MSG_ERR,
                    ERR_FILE_NOT_FOUND,
                    TOKEN_OPTIONAL_LOCATION(import_from),
                    CARET_WORD,
                    "Module name is empty.");
        goto DONE;
    }

    char                *local_path = gettmpstr();
    conf_data_t         *config     = NULL;
    const struct target *target     = assembly->target;
    const char          *module_dir = strlenu(target->module_dir) > 0 ? target->module_dir : NULL;
    const enum module_import_policy policy = assembly->target->module_policy;
    const bool local_found = module_dir ? module_exist(module_dir, modulepath) : false;

    switch (policy) {
    case IMPORT_POLICY_SYSTEM: {
        if (local_found) {
            strprint(local_path, "%s/%s", module_dir, modulepath);
        } else {
            strprint(local_path, "%s/%s", builder_get_lib_dir(), modulepath);
        }
        config = load_module_config(local_path, import_from);
        break;
    }

    case IMPORT_POLICY_BUNDLE_LATEST:
    case IMPORT_POLICY_BUNDLE: {
        bassert(module_dir);
        char      *system_path   = gettmpstr();
        const bool check_version = policy == IMPORT_POLICY_BUNDLE_LATEST;
        strprint(local_path, "%s/%s", module_dir, modulepath);
        strprint(system_path, "%s/%s", builder_get_lib_dir(), modulepath);
        const bool system_found = module_exist(builder_get_lib_dir(), modulepath);
        // Check if module is present in module directory.
        bool do_copy = !local_found;
        if (check_version && local_found && system_found) {
            s32 system_version = 0;
            s32 local_version  = 0;
            strprint(system_path, "%s/%s", builder_get_lib_dir(), modulepath);
            config = load_module_config(system_path, import_from);
            if (config) system_version = get_module_version(config);
            conf_data_t *local_config = load_module_config(local_path, import_from);
            if (local_config) local_version = get_module_version(local_config);
            conf_data_delete(local_config);
            do_copy = system_version > local_version;
        }
        if (do_copy) {
            // Delete old one.
            if (local_found) {
                char *backup_name = gettmpstr();
                char  date[26];
                date_time(date, static_arrlenu(date), "%d-%m-%Y_%H-%M-%S");
                strprint(backup_name, "%s_%s.bak", local_path, date);
                copy_dir(local_path, backup_name);
                remove_dir(local_path);
                builder_info("Backup module '%s'.", backup_name);
                puttmpstr(backup_name);
            }
            // Copy module from system to module directory.
            builder_info("%s module '%s' in '%s'.",
                         (check_version && local_found) ? "Update" : "Import",
                         modulepath,
                         module_dir);
            if (!copy_dir(system_path, local_path)) {
                builder_error("Cannot import module '%s'.", modulepath);
            }
        }
        if (!config) config = load_module_config(local_path, import_from);
        puttmpstr(system_path);
        break;
    }

    default:
        bassert("Invalid module import policy!");
    }
    if (config) {
        builder_log("%s", local_path);
        state = import_module(assembly, config, local_path, import_from);
    }
    puttmpstr(local_path);
    conf_data_delete(config);
DONE:
    return_zone(state);
}

DCpointer assembly_find_extern(struct assembly *assembly, const char *symbol)
{
    void              *handle = NULL;
    struct native_lib *lib;
    for (usize i = 0; i < arrlenu(assembly->libs); ++i) {
        lib    = &assembly->libs[i];
        handle = dlFindSymbol(lib->handle, symbol);
        if (handle) break;
    }
    return handle;
}
