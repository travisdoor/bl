//************************************************************************************************
// blc
//
// File:   assembly.h
// Author: Martin Dorazil
// Date:   02/03/2018
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

#ifndef BL_ASSEMBLY_BL
#define BL_ASSEMBLY_BL

#include "arena.h"
#include "mir.h"
#include "scope.h"
#include "unit.h"
#include <dyncall.h>
#include <dynload.h>

struct MirModule;
struct Builder;

typedef enum BuildMode {
    BUILD_MODE_DEBUG         = 1, // Standard debug mode. Opt: NONE
    BUILD_MODE_RELEASE_FAST  = 2, // Standard release mode. Opt: Aggresive
    BUILD_MODE_RELEASE_SMALL = 3, // Standard release mode. Opt: Default
    BUILD_MODE_BUILD         = 4, // Build pipeline entry mode.
} BuildMode;

typedef enum BuildDIKind {
    BUILD_DI_DWARF    = 1, // Emit DWARF debug information in LLVM IR.
    BUILD_DI_CODEVIEW = 2, // Emit MS CodeView debug info (PDB file).
} BuildDIKind;

// keep in sync with build.bl
typedef enum {
    IMPORT_POLICY_SYSTEM        = 0,
    IMPORT_POLICY_BUNDLE        = 1,
    IMPORT_POLICY_BUNDLE_LATEST = 2,
} ModuleImportPolicy;

typedef struct AssemblyOptions {
    BuildMode          build_mode;
    BuildDIKind        build_di_kind;
    ModuleImportPolicy module_import_policy;
    TString            custom_linker_opt;
    TString            out_dir;    // Build output directory
    TString            module_dir; // Module directory
    TArray             lib_paths;
    TArray             libs;
    bool               run_tests;
    bool               copy_deps;
} AssemblyOptions;

typedef struct Assembly {
    AssemblyOptions options;

    struct {
        ScopeArenas scope;
        MirArenas   mir;
        Arena       ast;
        Arena       array;       // Used for all TArrays
        Arena       small_array; // Used for all SmallArrays
    } arenas;

    struct {
        TArray global_instrs; // All global instructions.

        // Map type ids to RTTI variables.
        THashTable RTTI_table;
    } MIR;

    struct {
        LLVMModuleRef        module; // LLVM Module.
        LLVMContextRef       cnt;    // LLVM Context.
        LLVMTargetDataRef    TD;     // LLVM Target data.
        LLVMTargetMachineRef TM;     // LLVM Machine.
        char *               triple; // LLVM triple.
    } llvm;

    struct {
        TArray  cases;    // Optionally contains list of test case functions.
        MirVar *meta_var; // Optional variable containing runtime test case information.
    } testing;

    struct {
        MirFn * entry;                  // Main function
        MirFn * build_entry;            // Set for build assembly
        MirVar *command_line_arguments; // Command line arguments variable.
        // Provide information whether application run in compile time or not.
        MirVar *is_comptime_run;
        s32     argc; // Count of arguments forwarded in script mode.
        char ** argv; // Values of arguments forwarded in script mode.
    } vm_run;

    // DynCall/Lib data used for external method execution in compile time
    DCCallVM *dc_vm;
    VM        vm;

    TArray     units;      // array of all units in assembly
    THashTable unit_cache; // cache for loading only unique units
    char *     name;       // assembly name
    Scope *    gscope;     // global scope of the assembly

    /* Builtins */
    struct BuiltinTypes {
#define GEN_BUILTIN_TYPES
#include "assembly.inc"
#undef GEN_BUILTIN_TYPES
        bool is_rtti_ready;
        bool is_any_ready;
        bool is_test_cases_ready;
    } builtin_types;

    struct AssemblySyncImpl *sync;
} Assembly;

typedef struct NativeLib {
    u32           hash;
    DLLib *       handle;
    struct Token *linked_from;
    char *        user_name;
    char *        filename;
    char *        filepath;
    char *        dir;
    // Disable appending of this library to the linker options.
    bool is_internal;
} NativeLib;

Assembly *      assembly_new(const char *name);
void            assembly_delete(Assembly *assembly);
AssemblyOptions assembly_get_default_options(void);
void            assembly_add_unit(Assembly *assembly, Unit *unit);
void            assembly_add_lib_path(Assembly *assembly, const char *path);
void            assembly_append_linker_options(Assembly *assembly, const char *opt);
void            assembly_set_vm_args(Assembly *assembly, s32 argc, char **argv);
void assembly_add_native_lib(Assembly *assembly, const char *lib_name, struct Token *link_token);
bool assembly_add_unit_unique(Assembly *assembly, Unit *unit);
bool assembly_import_module(Assembly *    assembly,
                            const char *  modulepath,
                            struct Token *import_from // optional
);
DCpointer assembly_find_extern(Assembly *assembly, const char *symbol);
void      assembly_apply_options(Assembly *assembly);
void      assembly_set_output_dir(Assembly *assembly, const char *dir);
void      assembly_set_module_dir(Assembly *assembly, const char *dir, ModuleImportPolicy policy);

static INLINE bool assembly_has_rtti(Assembly *assembly, u64 type_id)
{
    return thtbl_has_key(&assembly->MIR.RTTI_table, type_id);
}

static INLINE MirVar *assembly_get_rtti(Assembly *assembly, u64 type_id)
{
    return thtbl_at(MirVar *, &assembly->MIR.RTTI_table, type_id);
}

static INLINE void assembly_add_rtti(Assembly *assembly, u64 type_id, MirVar *rtti_var)
{
    thtbl_insert(&assembly->MIR.RTTI_table, type_id, rtti_var);
}

static INLINE const char *build_mode_to_str(BuildMode mode)
{
    switch (mode) {
    case BUILD_MODE_DEBUG:
        return "DEBUG";
    case BUILD_MODE_RELEASE_FAST:
        return "RELEASE-FAST";
    case BUILD_MODE_RELEASE_SMALL:
        return "RELEASE-SMALL";
    case BUILD_MODE_BUILD:
        return "BUILD";
    }

    BL_ABORT("Invalid build mode");
}

static INLINE s32 get_opt_level_for_build_mode(BuildMode mode)
{
    switch (mode) {
    case BUILD_MODE_DEBUG:
        return 0;

    case BUILD_MODE_BUILD:
    case BUILD_MODE_RELEASE_FAST:
        return 3;

    case BUILD_MODE_RELEASE_SMALL:
        return 2;
    }

    BL_ABORT("Invalid build mode");
}

#endif
