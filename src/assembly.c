//************************************************************************************************
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
//************************************************************************************************

#include "assembly.h"
#include "blmemory.h"
#include "builder.h"
#include "common.h"
#include "unit.h"
#include <string.h>

#ifdef BL_PLATFORM_WIN
#include "winpthreads.h"
#include <windows.h>
#else
#include <pthread.h>
#endif

#define EXPECTED_GSCOPE_COUNT 4096
#define EXPECTED_ARRAY_COUNT 256
#define EXPECTED_UNIT_COUNT 512
#define EXPECTED_LINK_COUNT 32

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
    pthread_spinlock_t units_lock;
    pthread_mutex_t    import_module_lock;
} AssemblySyncImpl;

static AssemblySyncImpl *sync_new(void)
{
    AssemblySyncImpl *impl = bl_malloc(sizeof(AssemblySyncImpl));
    pthread_spin_init(&impl->units_lock, PTHREAD_PROCESS_PRIVATE);
    pthread_mutex_init(&impl->import_module_lock, NULL);
    return impl;
}

static void sync_delete(AssemblySyncImpl *impl)
{
    pthread_spin_destroy(&impl->units_lock);
    pthread_mutex_destroy(&impl->import_module_lock);
    bl_free(impl);
}

static void dl_init(Assembly *assembly)
{
    DCCallVM *vm = dcNewCallVM(4096);
    dcMode(vm, DC_CALL_C_DEFAULT);
    assembly->dc_vm = vm;
}

static void llvm_init(Assembly *assembly)
{
    if (assembly->llvm.module) BL_ABORT("Attempt to override assembly options.");
    // init LLVM
    char *triple    = LLVMGetDefaultTargetTriple();
    char *cpu       = /*LLVMGetHostCPUName()*/ "";
    char *features  = /*LLVMGetHostCPUFeatures()*/ "";
    char *error_msg = NULL;
    builder_log("Target: %s", triple);
    LLVMTargetRef llvm_target = NULL;
    if (LLVMGetTargetFromTriple(triple, &llvm_target, &error_msg)) {
        builder_error("Cannot get target with error: %s!", error_msg);
        LLVMDisposeMessage(error_msg);
        BL_ABORT("Cannot get target");
    }
    LLVMContextRef llvm_context = LLVMContextCreate();
    LLVMModuleRef  llvm_module  = LLVMModuleCreateWithNameInContext(assembly->name, llvm_context);
    LLVMTargetMachineRef llvm_tm =
        LLVMCreateTargetMachine(llvm_target,
                                triple,
                                cpu,
                                features,
                                get_opt_level_for_build_mode(assembly->options.build_mode),
                                LLVMRelocDefault,
                                LLVMCodeModelDefault);

    LLVMTargetDataRef llvm_td = LLVMCreateTargetDataLayout(llvm_tm);
    LLVMSetModuleDataLayout(llvm_module, llvm_td);
    LLVMSetTarget(llvm_module, triple);
    assembly->llvm.cnt    = llvm_context;
    assembly->llvm.module = llvm_module;
    assembly->llvm.TM     = llvm_tm;
    assembly->llvm.TD     = llvm_td;
    assembly->llvm.triple = triple;
}

static void mir_init(Assembly *assembly)
{
    mir_arenas_init(&assembly->arenas.mir);
    tarray_init(&assembly->MIR.global_instrs, sizeof(MirInstr *));
    thtbl_init(&assembly->MIR.RTTI_table, sizeof(MirVar *), 2048);
}

static void native_lib_terminate(NativeLib *lib)
{
    if (lib->handle) dlFreeLibrary(lib->handle);
    if (lib->is_internal) return;
    free(lib->filename);
    free(lib->filepath);
    free(lib->dir);
    free(lib->user_name);
}

static void dl_terminate(Assembly *assembly)
{
    dcFree(assembly->dc_vm);
}

static void llvm_terminate(Assembly *assembly)
{
    LLVMDisposeModule(assembly->llvm.module);
    LLVMDisposeTargetMachine(assembly->llvm.TM);
    LLVMDisposeMessage(assembly->llvm.triple);
    LLVMDisposeTargetData(assembly->llvm.TD);
    LLVMContextDispose(assembly->llvm.cnt);
}

static INLINE void mir_terminate(Assembly *assembly)
{
    thtbl_terminate(&assembly->MIR.RTTI_table);
    tarray_terminate(&assembly->MIR.global_instrs);
    mir_arenas_terminate(&assembly->arenas.mir);
}

static INLINE void set_default_out_dir(Assembly *assembly)
{
    char path[PATH_MAX] = {0};
    get_current_working_dir(&path[0], PATH_MAX);
    tstring_clear(&assembly->options.out_dir);
    tstring_append(&assembly->options.out_dir, path);
}

// Create directory tree and set out_path.
static bool create_auxiliary_dir_tree_if_not_exist(const char *_path, TString *out_path)
{
    BL_ASSERT(_path);
    BL_ASSERT(out_path);
#ifdef BL_PLATFORM_WIN
    char *path = strdup(_path);
    if (!path) BL_ABORT("Invalid directory copy.");
    win_path_to_unix(path, strlen(path));
#else
    const char *path            = _path;
#endif
    if (!dir_exists(path)) {
        if (!create_dir_tree(path)) {
#ifdef BL_PLATFORM_WIN
            free(path);
#endif
            return false;
        }
    }
    char full_path[PATH_MAX] = {0};
    brealpath(path, full_path, PATH_MAX);
    tstring_clear(out_path);
    tstring_append(out_path, full_path);
#ifdef BL_PLATFORM_WIN
    free(path);
#endif
    return true;
}

static ConfData *load_module_config(const char *modulepath, Token *import_from)
{
    char tmp_path[PATH_MAX] = {0};
    snprintf(tmp_path, TARRAY_SIZE(tmp_path), "%s/%s", modulepath, MODULE_CONFIG_FILE);
    ConfData *config = conf_data_new();
    if (builder_compile_config(tmp_path, config, import_from) != COMPILE_OK) {
        conf_data_delete(config);
        return false;
    }
    return config;
}

static INLINE s32 get_module_version(ConfData *config)
{
    BL_ASSERT(config);
    if (conf_data_has_key(config, CONF_MODULE_VERSION)) { // optional version
        return conf_data_get_int(config, CONF_MODULE_VERSION);
    }
    return 0;
}

static bool
import_module(Assembly *assembly, ConfData *config, const char *modulepath, Token *import_from)
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

// public
Assembly *assembly_new(const char *name)
{
    Assembly *assembly = bl_malloc(sizeof(Assembly));
    memset(assembly, 0, sizeof(Assembly));
    assembly->sync = sync_new();
    assembly->name = strdup(name);

    tarray_init(&assembly->units, sizeof(Unit *));
    tstring_init(&assembly->options.custom_linker_opt);
    tstring_init(&assembly->options.out_dir);
    tstring_init(&assembly->options.module_dir);
    tarray_init(&assembly->options.libs, sizeof(NativeLib));
    tarray_init(&assembly->options.lib_paths, sizeof(char *));
    tarray_init(&assembly->testing.cases, sizeof(struct MirFn *));
    vm_init(&assembly->vm, VM_STACK_SIZE);

    // set defaults
    assembly->options.build_mode           = builder.options.build_mode;
    assembly->options.build_di_kind        = builder.options.build_di_kind;
    assembly->options.run_tests            = builder.options.run_tests;
    assembly->options.module_import_policy = IMPORT_POLICY_SYSTEM;
#ifdef BL_PLATFORM_WIN // Use target platform tag.
    assembly->options.copy_deps = true;
#else
    assembly->options.copy_deps = false;
#endif
    set_default_out_dir(assembly);

    scope_arenas_init(&assembly->arenas.scope);
    ast_arena_init(&assembly->arenas.ast);
    arena_init(&assembly->arenas.array,
               sizeof(TArray *),
               EXPECTED_ARRAY_COUNT,
               (ArenaElemDtor)tarray_dtor);
    arena_init(&assembly->arenas.small_array,
               sizeof(union _SmallArrays),
               EXPECTED_ARRAY_COUNT,
               (ArenaElemDtor)small_array_dtor);

    assembly->gscope =
        scope_create(&assembly->arenas.scope, SCOPE_GLOBAL, NULL, EXPECTED_GSCOPE_COUNT, NULL);

    dl_init(assembly);
    mir_init(assembly);

    return assembly;
}

void assembly_delete(Assembly *assembly)
{
    free(assembly->name);
    Unit *unit;
    TARRAY_FOREACH(Unit *, &assembly->units, unit)
    {
        unit_delete(unit);
    }

    NativeLib *lib;
    for (usize i = 0; i < assembly->options.libs.size; ++i) {
        lib = &tarray_at(NativeLib, &assembly->options.libs, i);
        native_lib_terminate(lib);
    }

    char *p;
    TARRAY_FOREACH(char *, &assembly->options.lib_paths, p) free(p);

    tarray_terminate(&assembly->options.libs);
    tarray_terminate(&assembly->options.lib_paths);
    tstring_terminate(&assembly->options.custom_linker_opt);
    tstring_terminate(&assembly->options.out_dir);
    tstring_terminate(&assembly->options.module_dir);
    vm_terminate(&assembly->vm);
    tarray_terminate(&assembly->testing.cases);
    arena_terminate(&assembly->arenas.small_array);
    arena_terminate(&assembly->arenas.array);
    ast_arena_terminate(&assembly->arenas.ast);
    scope_arenas_terminate(&assembly->arenas.scope);
    tarray_terminate(&assembly->units);
    dl_terminate(assembly);
    mir_terminate(assembly);
    llvm_terminate(assembly);
    sync_delete(assembly->sync);
    bl_free(assembly);
}

void assembly_apply_options(Assembly *assembly)
{
    llvm_init(assembly);
}

void assembly_add_lib_path(Assembly *assembly, const char *path)
{
    if (!path) return;
    char *tmp = strdup(path);
    if (!tmp) return;
    tarray_push(&assembly->options.lib_paths, tmp);
}

void assembly_append_linker_options(Assembly *assembly, const char *opt)
{
    if (!opt) return;
    tstring_append(&assembly->options.custom_linker_opt, opt);
    tstring_append(&assembly->options.custom_linker_opt, " ");
}

void assembly_set_output_dir(Assembly *assembly, const char *dir)
{
    if (!dir) builder_error("Cannot create output directory.");
    if (!create_auxiliary_dir_tree_if_not_exist(dir, &assembly->options.out_dir)) {
        builder_error("Cannot create output directory '%s'.", dir);
    }
}

void assembly_set_module_dir(Assembly *assembly, const char *dir, ModuleImportPolicy policy)
{
    if (!dir) builder_error("Cannot create module directory.");
    if (!create_auxiliary_dir_tree_if_not_exist(dir, &assembly->options.module_dir)) {
        builder_error("Cannot create module directory '%s'.", dir);
    }
    assembly->options.module_import_policy = policy;
}

static INLINE bool assembly_has_unit(Assembly *assembly, const u64 hash)
{
    Unit *unit;
    TARRAY_FOREACH(Unit *, &assembly->units, unit)
    {
        if (hash == unit->hash) return true;
    }
    return false;
}

Unit *assembly_add_unit(Assembly *assembly, const char *filepath, Token *load_from)
{
    AssemblySyncImpl *sync = assembly->sync;
    pthread_spin_lock(&sync->units_lock);
    Unit *    unit = NULL;
    const u64 hash = unit_hash(filepath, load_from);
    if (assembly_has_unit(assembly, hash)) {
        goto DONE;
    }

    unit = unit_new(filepath, load_from);
    tarray_push(&assembly->units, unit);
    builder_submit_unit(unit);

DONE:
    pthread_spin_unlock(&sync->units_lock);
    return unit;
}

void assembly_add_native_lib(Assembly *assembly, const char *lib_name, struct Token *link_token)
{
    const u64 hash = thash_from_str(lib_name);

    { // Search for duplicity.
        NativeLib *lib;
        for (usize i = 0; i < assembly->options.libs.size; ++i) {
            lib = &tarray_at(NativeLib, &assembly->options.libs, i);
            if (lib->hash == hash) return;
        }
    }

    NativeLib lib   = {0};
    lib.hash        = hash;
    lib.user_name   = strdup(lib_name);
    lib.linked_from = link_token;

    tarray_push(&assembly->options.libs, lib);
}

static INLINE bool module_exist(const char *module_dir, const char *modulepath)
{
    TString *local_conf_path = get_tmpstr();
    tstring_setf(local_conf_path, "%s/%s/%s", module_dir, modulepath, MODULE_CONFIG_FILE);
    const bool found = search_source_file(local_conf_path->data, SEARCH_FLAG_ABS, NULL, NULL, NULL);
    put_tmpstr(local_conf_path);
    return found;
}

bool assembly_import_module(Assembly *assembly, const char *modulepath, Token *import_from)
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

    TString *   local_path = get_tmpstr();
    ConfData *  config     = NULL;
    const char *module_dir =
        assembly->options.module_dir.len > 0 ? assembly->options.module_dir.data : NULL;
    const ModuleImportPolicy policy = assembly->options.module_import_policy;
    const bool local_found          = module_dir ? module_exist(module_dir, modulepath) : false;
    switch (policy) {
    case IMPORT_POLICY_SYSTEM: {
        if (local_found) {
            tstring_setf(local_path, "%s/%s", module_dir, modulepath);
            config = load_module_config(local_path->data, import_from);
        } else {
            tstring_setf(local_path, "%s/%s", ENV_LIB_DIR, modulepath);
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
        tstring_setf(system_path, "%s/%s", ENV_LIB_DIR, modulepath);
        const bool system_found = module_exist(ENV_LIB_DIR, modulepath);
        // Check if module is present in module directory.
        bool do_copy = !local_found;
        if (check_version && local_found && system_found) {
            s32 system_version = 0;
            s32 local_version  = 0;
            tstring_setf(system_path, "%s/%s", ENV_LIB_DIR, modulepath);
            config = load_module_config(system_path->data, import_from);
            if (config) system_version = get_module_version(config);
            ConfData *local_config = load_module_config(local_path->data, import_from);
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

void assembly_set_vm_args(Assembly *assembly, s32 argc, char **argv)
{
    assembly->vm_run.argc = argc;
    assembly->vm_run.argv = argv;
}

DCpointer assembly_find_extern(Assembly *assembly, const char *symbol)
{
    void *     handle = NULL;
    NativeLib *lib;

    for (usize i = 0; i < assembly->options.libs.size; ++i) {
        lib    = &tarray_at(NativeLib, &assembly->options.libs, i);
        handle = dlFindSymbol(lib->handle, symbol);
        if (handle) break;
    }

    return handle;
}
