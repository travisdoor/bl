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
#include "unit.h"
#include <string.h>

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
    win_fix_path(path, strlen(path));
#else
    const char *path = _path;
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

static bool import_module(Assembly *assembly, const char *modulepath, Token *import_from)
{
    if (!strlen(modulepath)) {
        builder_msg(BUILDER_MSG_ERROR,
                    ERR_FILE_NOT_FOUND,
                    TOKEN_OPTIONAL_LOCATION(import_from),
                    BUILDER_CUR_WORD,
                    "Module name is empty.");
        goto INTERRUPT;
    }
    char tmp_path[PATH_MAX] = {0};
    snprintf(tmp_path, ARRAY_SIZE(tmp_path), "%s/%s", modulepath, MODULE_CONFIG_FILE);
    ConfData config;
    conf_data_init(&config);
    if (builder_compile_config(tmp_path, &config, import_from) != COMPILE_OK) goto INTERRUPT;
    if (!conf_data_has_key(&config, CONF_MODULE_ENTRY)) {
        builder_msg(
            BUILDER_MSG_ERROR,
            ERR_MISSING_PLATFORM,
            TOKEN_OPTIONAL_LOCATION(import_from),
            BUILDER_CUR_WORD,
            "Module doesn't support current target platform, configuration entry ('%s') not "
            "found in module config file '%s'.",
            CONF_MODULE_ENTRY,
            tmp_path);
        goto INTERRUPT;
    }

    if (conf_data_has_key(&config, CONF_MODULE_VERSION)) { // optional version
        const s32 version = conf_data_get_int(&config, CONF_MODULE_VERSION);
        BL_LOG("module version is %d", version);
    }

    { // entry file
        const char *entry_file = conf_data_get_str(&config, CONF_MODULE_ENTRY);
        BL_ASSERT(entry_file && strlen(entry_file) > 0);
        snprintf(tmp_path, ARRAY_SIZE(tmp_path), "%s/%s", modulepath, entry_file);
        Unit *unit = unit_new_file(tmp_path, NULL);
        if (!assembly_add_unit_unique(assembly, unit)) {
            unit_delete(unit);
        }
    }

    // Optional lib path
    if (conf_data_has_key(&config, CONF_MODULE_LIB_PATH)) {
        const char *lib_path = conf_data_get_str(&config, CONF_MODULE_LIB_PATH);
        BL_ASSERT(lib_path && strlen(lib_path) > 0);
        snprintf(tmp_path, ARRAY_SIZE(tmp_path), "%s/%s/%s", ENV_LIB_DIR, modulepath, lib_path);
        if (!dir_exists(tmp_path)) {
            builder_msg(BUILDER_MSG_ERROR,
                        ERR_FILE_NOT_FOUND,
                        TOKEN_OPTIONAL_LOCATION(import_from),
                        BUILDER_CUR_WORD,
                        "Cannot find module imported library path '%s' defined by '%s'.",
                        tmp_path,
                        CONF_MODULE_LIB_PATH,
                        tmp_path);
            goto INTERRUPT;
        }
        assembly_add_lib_path(assembly, tmp_path);
    }

    // Optional libs
    if (conf_data_has_key(&config, CONF_MODULE_LINK)) {
        const char *lib = conf_data_get_str(&config, CONF_MODULE_LINK);
        BL_ASSERT(lib && strlen(lib) > 0);
        assembly_add_native_lib(assembly, lib, NULL);
    }
    conf_data_terminate(&config);
    return true;
INTERRUPT:
    conf_data_terminate(&config);
    return false;
}

// public
Assembly *assembly_new(const char *name)
{
    Assembly *assembly = bl_malloc(sizeof(Assembly));
    memset(assembly, 0, sizeof(Assembly));
    assembly->name = strdup(name);

    tarray_init(&assembly->units, sizeof(Unit *));
    thtbl_init(&assembly->unit_cache, 0, EXPECTED_UNIT_COUNT);
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
    thtbl_terminate(&assembly->unit_cache);
    dl_terminate(assembly);
    mir_terminate(assembly);
    llvm_terminate(assembly);
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
    BL_LOG("set module directory %s", assembly->options.module_dir.data);
}

void assembly_add_unit(Assembly *assembly, Unit *unit)
{
    tarray_push(&assembly->units, unit);
}

bool assembly_add_unit_unique(Assembly *assembly, Unit *unit)
{
    const u64 hash = unit->hash;
    if (thtbl_has_key(&assembly->unit_cache, hash)) return false;
    thtbl_insert_empty(&assembly->unit_cache, hash);
    assembly_add_unit(assembly, unit);
    return true;
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

bool assembly_import_module(Assembly *assembly, const char *modulepath, Token *import_from)
{
    switch (assembly->options.module_import_policy) {
    case IMPORT_POLICY_SYSTEM:
        BL_LOG("Policy SYSTEM.");
        break;
    case IMPORT_POLICY_BUNDLE:
        BL_LOG("Policy BUNDLE.");
        break;
    case IMPORT_POLICY_BUNDLE_LATEST:
        BL_LOG("Policy BUNDLE LATEST.");
        break;
    default:
        BL_ASSERT("Invalid module import policy!");
    }
    return import_module(assembly, modulepath, import_from);
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
