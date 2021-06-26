// =================================================================================================
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
// =================================================================================================

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
struct BuilderOptions;

typedef enum AssemblyKind {
    ASSEMBLY_EXECUTABLE     = 1,
    ASSEMBLY_SHARED_LIB     = 2,
    ASSEMBLY_BUILD_PIPELINE = 3,
    ASSEMBLY_DOCS           = 4,
} AssemblyKind;

typedef enum AssemblyOpt {
    ASSEMBLY_OPT_DEBUG         = 1, // Standard debug mode. Opt: NONE
    ASSEMBLY_OPT_RELEASE_FAST  = 2, // Standard release mode. Opt: Aggressive
    ASSEMBLY_OPT_RELEASE_SMALL = 3, // Standard release mode. Opt: Default
} AssemblyOpt;

typedef enum AssemblyDIKind {
    ASSEMBLY_DI_DWARF    = 1, // Emit DWARF debug information in LLVM IR.
    ASSEMBLY_DI_CODEVIEW = 2, // Emit MS CodeView debug info (PDB file).
} AssemblyDIKind;

// keep in sync with build.bl
typedef enum {
    IMPORT_POLICY_SYSTEM        = 0,
    IMPORT_POLICY_BUNDLE        = 1,
    IMPORT_POLICY_BUNDLE_LATEST = 2,
} ModuleImportPolicy;

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

// ABI sync!!! Keep this updated with Target representation in build.bl.
#define TARGET_COPYABLE_CONTENT                                                                    \
    AssemblyKind   kind;                                                                           \
    AssemblyOpt    opt;                                                                            \
    AssemblyDIKind di;                                                                             \
    bool           reg_split;                                                                      \
    bool           verify_llvm;                                                                    \
    bool           run_tests;                                                                      \
    bool           no_api;                                                                         \
    bool           copy_deps;                                                                      \
    bool           run;                                                                            \
    bool           print_tokens;                                                                   \
    bool           print_ast;                                                                      \
    bool           emit_llvm;                                                                      \
    bool           emit_mir;                                                                       \
    bool           no_bin;                                                                         \
    bool           no_llvm;                                                                        \
    bool           no_analyze;                                                                     \
    bool           syntax_only;

typedef struct Target {
    // Copyable content of target can be duplicated from default target, the default target is
    // usually target containing some setup acquired from command line arguments of application.
    TARGET_COPYABLE_CONTENT

    char *             name;
    TArray             files;
    TArray             default_lib_paths;
    TArray             default_libs;
    TString            default_custom_linker_opt;
    TString            out_dir;
    TString            module_dir;
    ModuleImportPolicy module_policy;

    struct {
        s32    argc;
        char **argv;
    } vm;
    BL_MAGIC_ADD
} Target;

typedef struct Assembly {
    const Target *target;

    TString custom_linker_opt;
    TArray  lib_paths;
    TArray  libs;

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
        // Instructions for exported symbols (function protorypes).
        TArray exported_instrs;
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

        // Store status of last execution of this assembly.
        s32 last_execution_status;
    } vm_run;

    // Some compilation time related runtimes, this data are reset for every compilation.
    struct {
        f64 parsing_lexing_s;
        f64 mir_s;
        f64 llvm_s;
        f64 linking_s;
    } stats;

    // DynCall/Lib data used for external method execution in compile time
    DCCallVM *dc_vm;
    VM        vm;

    TArray units;  // array of all units in assembly
    Scope *gscope; // global scope of the assembly

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

Target *target_new(const char *name);
Target *target_dup(const char *name, const Target *other);
void    target_delete(Target *target);
void    target_add_file(Target *target, const char *filepath);
void    target_add_lib_path(Target *target, const char *path);
void    target_add_lib(Target *target, const char *lib);
void    target_append_linker_options(Target *target, const char *option);
void    target_set_vm_args(Target *target, s32 argc, char **argv);
void    target_set_output_dir(Target *target, const char *dirpath);
void    target_set_module_dir(Target *target, const char *dir, ModuleImportPolicy policy);

// Create new assembly instance.
Assembly *assembly_new(const Target *target);

// Delete created assembly instance and release all internally initialized resources.
void assembly_delete(Assembly *assembly);

// Add new unit into the assembly.
Unit *assembly_add_unit(Assembly *assembly, const char *filepath, struct Token *load_from);

// Add library path.
void assembly_add_lib_path(Assembly *assembly, const char *path);

// Append linker options string.
void assembly_append_linker_options(Assembly *assembly, const char *opt);

// Add native library.
void assembly_add_native_lib(Assembly *assembly, const char *lib_name, struct Token *link_token);

// Import module.
bool assembly_import_module(Assembly *assembly, const char *modulepath, struct Token *import_from);

DCpointer assembly_find_extern(Assembly *assembly, const char *symbol);

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

// Convert opt level to string.
static INLINE const char *opt_to_str(AssemblyOpt opt)
{
    switch (opt) {
    case ASSEMBLY_OPT_DEBUG:
        return "DEBUG";
    case ASSEMBLY_OPT_RELEASE_FAST:
        return "RELEASE-FAST";
    case ASSEMBLY_OPT_RELEASE_SMALL:
        return "RELEASE-SMALL";
    }
    BL_ABORT("Invalid build mode");
}

// Convert opt level to LLVM.
static INLINE LLVMCodeGenOptLevel opt_to_LLVM(AssemblyOpt opt)
{
    switch (opt) {
    case ASSEMBLY_OPT_DEBUG:
        return LLVMCodeGenLevelNone;
    case ASSEMBLY_OPT_RELEASE_FAST:
        return LLVMCodeGenLevelAggressive;
    case ASSEMBLY_OPT_RELEASE_SMALL:
        return LLVMCodeGenLevelDefault;
    }
    BL_ABORT("Invalid build mode");
}

#endif
