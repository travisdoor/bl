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
#include "conf.h"
#include "threading.h"
#if !BL_PLATFORM_WIN
#include <errno.h>
#endif

#include "builder.h"
#include "stb_ds.h"
#include <string.h>

#define EXPECTED_ARRAY_COUNT 2048

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

static void sarr_dtor(sarr_any_t *arr)
{
	sarrfree(arr);
}

typedef struct AssemblySyncImpl {
	pthread_spinlock_t units_lock;
	pthread_spinlock_t linker_opt_lock;
	pthread_spinlock_t lib_path_lock;
	pthread_spinlock_t link_lock;
} AssemblySyncImpl;

static AssemblySyncImpl *sync_new(void)
{
	AssemblySyncImpl *impl = bmalloc(sizeof(AssemblySyncImpl));
	pthread_spin_init(&impl->units_lock, 0);
	pthread_spin_init(&impl->linker_opt_lock, 0);
	pthread_spin_init(&impl->lib_path_lock, 0);
	pthread_spin_init(&impl->link_lock, 0);
	return impl;
}

static void sync_delete(AssemblySyncImpl *impl)
{
	pthread_spin_destroy(&impl->units_lock);
	pthread_spin_destroy(&impl->linker_opt_lock);
	pthread_spin_destroy(&impl->lib_path_lock);
	pthread_spin_destroy(&impl->link_lock);
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
	char       *token;
	char       *it    = tmp;
	s32         state = 0;
	// arch-vendor-os-evironment
	while ((token = strtok_r(it, delimiter, &it))) {
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
	const s32 triple_len = target_triple_to_string(&assembly->target->triple, NULL, 0);
	char     *triple     = bmalloc(triple_len);
	target_triple_to_string(&assembly->target->triple, triple, triple_len);

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
	arrsetcap(assembly->MIR.global_instrs, 4096);
	arrsetcap(assembly->MIR.exported_instrs, 256);
}

static inline void mir_terminate(struct assembly *assembly)
{
	hmfree(assembly->MIR.rtti_table);
	arrfree(assembly->MIR.global_instrs);
	arrfree(assembly->MIR.exported_instrs);
	mir_arenas_terminate(&assembly->arenas.mir);
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

static struct config *load_module_config(const char *modulepath, struct token *import_from)
{
	char *path = tstr();
	strprint(path, "%s/%s", modulepath, MODULE_CONFIG_FILE);
	struct config *conf = confload(path);
	put_tstr(path);
	return conf;
}

static inline s32 get_module_version(struct config *config)
{
	bassert(config);
	const char     *verstr = confreads(config, "/version", "0");
	const uintmax_t ver    = strtoumax(verstr, NULL, 10);
	if (ver == UINTMAX_MAX && errno == ERANGE) {
		const char *filepath = confreads(config, "@filepath", NULL);
		builder_warning("Cannot read module version '%s' expected integer value.", filepath);
		return 0;
	}
	return (s32)ver;
}

typedef struct {
	struct assembly *assembly;
	struct token    *import_from;
	const char      *modulepath;
} import_elem_context_t;

static void import_source(import_elem_context_t *ctx, const char *srcfile)
{
	char *path = tstr();
	strprint(path, "%s/%s", ctx->modulepath, srcfile);
	// @Cleanup: should we pass the import_from token here?
	assembly_add_unit_safe(ctx->assembly, path, NULL);
	put_tstr(path);
}

static void import_lib_path(import_elem_context_t *ctx, const char *dirpath)
{
	char *path = tstr();
	strprint(path, "%s/%s", ctx->modulepath, dirpath);
	if (!dir_exists(path)) {
		builder_msg(MSG_ERR,
		            ERR_FILE_NOT_FOUND,
		            TOKEN_OPTIONAL_LOCATION(ctx->import_from),
		            CARET_WORD,
		            "Cannot find module imported library path '%s'.",
		            path);
	} else {
		assembly_add_lib_path_safe(ctx->assembly, path);
	}
	put_tstr(path);
}

static void import_link(import_elem_context_t *ctx, const char *lib)
{
	assembly_add_native_lib_safe(ctx->assembly, lib, NULL, false);
}

static void import_link_runtime_only(import_elem_context_t *ctx, const char *lib)
{
	assembly_add_native_lib_safe(ctx->assembly, lib, NULL, true);
}

static bool import_module(struct assembly *assembly,
                          struct config   *config,
                          const char      *modulepath,
                          struct token    *import_from)
{
	zone();
	import_elem_context_t ctx = {assembly, import_from, modulepath};

	const s32 version = get_module_version(config);
	builder_log("Import module '%s' version %d.", modulepath, version);

	// Global
	assembly_append_linker_options_safe(assembly, confreads(config, "/linker_opt", ""));
	process_tokens(&ctx,
	               confreads(config, "/src", ""),
	               ENVPATH_SEPARATOR,
	               (process_tokens_fn_t)&import_source);
	process_tokens(&ctx,
	               confreads(config, "/linker_lib_path", ""),
	               ENVPATH_SEPARATOR,
	               (process_tokens_fn_t)&import_lib_path);
	process_tokens(
	    &ctx, confreads(config, "/link", ""), ENVPATH_SEPARATOR, (process_tokens_fn_t)&import_link);

	// Platform specific
	assembly_append_linker_options_safe(assembly,
	                                    read_config(config, assembly->target, "linker_opt", ""));
	process_tokens(&ctx,
	               read_config(config, assembly->target, "src", ""),
	               ENVPATH_SEPARATOR,
	               (process_tokens_fn_t)&import_source);
	process_tokens(&ctx,
	               read_config(config, assembly->target, "linker_lib_path", ""),
	               ENVPATH_SEPARATOR,
	               (process_tokens_fn_t)&import_lib_path);
	process_tokens(&ctx,
	               read_config(config, assembly->target, "link", ""),
	               ENVPATH_SEPARATOR,
	               (process_tokens_fn_t)&import_link);
	process_tokens(&ctx,
	               read_config(config, assembly->target, "link-runtime", ""),
	               ENVPATH_SEPARATOR,
	               (process_tokens_fn_t)&import_link_runtime_only);

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

	// Default target uses current working directory which may be changed by user compiler flags
	// later (--work-dir).
	strappend(target->out_dir, ".");

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
	char triple_str[128];
	target_triple_to_string(triple, triple_str, static_arrlenu(triple_str));
	bool   is_valid = false;
	char **list     = builder_get_supported_targets();
	char **it       = list;
	for (; *it; it++) {
		if (strcmp(triple_str, *it) == 0) {
			is_valid = true;
			break;
		}
	}
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

s32 target_triple_to_string(const struct target_triple *triple, char *buf, s32 buf_len)
{
	const char *arch, *vendor, *os, *env;
	arch = vendor = os = env = "";
	s32 len                  = 0;
	if (triple->arch < static_arrlenu(arch_names)) arch = arch_names[triple->arch];
	if (triple->vendor < static_arrlenu(vendor_names)) vendor = vendor_names[triple->vendor];
	if (triple->os < static_arrlenu(os_names)) os = os_names[triple->os];
	if (triple->env < static_arrlenu(env_names)) env = env_names[triple->env];
	if (triple->env == ENV_unknown) {
		len = snprintf(NULL, 0, "%s-%s-%s", arch, vendor, os) + 1;
		if (buf) snprintf(buf, MIN(buf_len, len), "%s-%s-%s", arch, vendor, os);
	} else {
		len = snprintf(NULL, 0, "%s-%s-%s-%s", arch, vendor, os, env) + 1;
		if (buf) snprintf(buf, MIN(buf_len, len), "%s-%s-%s-%s", arch, vendor, os, env);
	}
	return len;
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
	scopes_context_init(&assembly->scopes_context);
	arena_init(&assembly->arenas.sarr,
	           sarr_total_size,
	           16, // Is this correct?
	           EXPECTED_ARRAY_COUNT,
	           (arena_elem_dtor_t)sarr_dtor);
	assembly->gscope = scope_create(&assembly->scopes_context, SCOPE_GLOBAL, NULL, NULL);

	dl_init(assembly);
	mir_init(assembly);

	// Add units from target
	for (usize i = 0; i < arrlenu(target->files); ++i) {
		assembly_add_unit_safe(assembly, target->files[i], NULL);
	}

	const char *preload_file = read_config(builder.config, assembly->target, "preload_file", "");

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
		assembly_add_lib_path_safe(assembly, target->default_lib_paths[i]);

	// Duplicate default libs
	for (usize i = 0; i < arrlenu(target->default_libs); ++i)
		assembly_add_native_lib_safe(assembly, target->default_libs[i], NULL, false);

	// Append custom linker options
	assembly_append_linker_options_safe(assembly, target->default_custom_linker_opt);

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
	scopes_context_terminate(&assembly->scopes_context);
	llvm_terminate(assembly);
	dl_terminate(assembly);
	mir_terminate(assembly);
	sync_delete(assembly->sync);
	scfree(&assembly->string_cache);
	bfree(assembly);
	return_zone();
}

void assembly_add_lib_path_safe(struct assembly *assembly, const char *path)
{
	if (!path) return;
	char *tmp = strdup(path);
	if (!tmp) return;
	AssemblySyncImpl *sync = assembly->sync;
	pthread_spin_lock(&sync->lib_path_lock);
	arrput(assembly->lib_paths, tmp);
	pthread_spin_unlock(&sync->lib_path_lock);
}

void assembly_append_linker_options_safe(struct assembly *assembly, const char *opt)
{
	if (!opt) return;
	if (opt[0] == '\0') return;

	AssemblySyncImpl *sync = assembly->sync;
	pthread_spin_lock(&sync->linker_opt_lock);
	strappend(assembly->custom_linker_opt, "%s ", opt);
	pthread_spin_unlock(&sync->linker_opt_lock);
}

static inline bool assembly_has_unit(struct assembly *assembly, const hash_t hash)
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
	if (!is_str_valid_nonempty(filepath)) return_zone(NULL);
	struct unit      *unit = NULL;
	const hash_t      hash = unit_hash(filepath, load_from);
	AssemblySyncImpl *sync = assembly->sync;
	pthread_spin_lock(&sync->units_lock);
	if (assembly_has_unit(assembly, hash)) goto DONE;
	unit = unit_new(filepath, load_from);
	arrput(assembly->units, unit);
	builder_async_submit_unit(unit);
DONE:
	pthread_spin_unlock(&sync->units_lock);
	return_zone(unit);
}

void assembly_add_native_lib_safe(struct assembly *assembly,
                                  const char      *lib_name,
                                  struct token    *link_token,
                                  bool             runtime_only)
{
	AssemblySyncImpl *sync = assembly->sync;
	pthread_spin_lock(&sync->link_lock);
	const hash_t hash = strhash(lib_name);
	{ // Search for duplicity.
		for (usize i = 0; i < arrlenu(assembly->libs); ++i) {
			struct native_lib *lib = &assembly->libs[i];
			if (lib->hash == hash) goto DONE;
		}
	}
	struct native_lib lib = {0};
	lib.hash              = hash;
	lib.user_name         = strdup(lib_name);
	lib.linked_from       = link_token;
	lib.runtime_only      = runtime_only;
	arrput(assembly->libs, lib);
DONE:
	pthread_spin_unlock(&sync->link_lock);
}

static inline bool module_exist(const char *module_dir, const char *modulepath)
{
	char *path = tstr();
	strprint(path, "%s/%s/%s", module_dir, modulepath, MODULE_CONFIG_FILE);
	const bool found = search_source_file(path, SEARCH_FLAG_ABS, NULL, NULL, NULL);
	put_tstr(path);
	return found;
}

bool assembly_import_module(struct assembly *assembly,
                            const char      *modulepath,
                            struct token    *import_from)
{
	zone();
	bool state = false;
	if (!is_str_valid_nonempty(modulepath)) {
		builder_msg(MSG_ERR,
		            ERR_FILE_NOT_FOUND,
		            TOKEN_OPTIONAL_LOCATION(import_from),
		            CARET_WORD,
		            "Module name is empty.");
		goto DONE;
	}

	char                *local_path = tstr();
	struct config       *config     = NULL;
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
		char      *system_path   = tstr();
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
			struct config *local_config = load_module_config(local_path, import_from);
			if (local_config) local_version = get_module_version(local_config);
			confdelete(local_config);
			do_copy = system_version > local_version;
		}
		if (do_copy) {
			// Delete old one.
			if (local_found) {
				char *backup_name = tstr();
				char  date[26];
				date_time(date, static_arrlenu(date), "%d-%m-%Y_%H-%M-%S");
				strprint(backup_name, "%s_%s.bak", local_path, date);
				copy_dir(local_path, backup_name);
				remove_dir(local_path);
				builder_info("Backup module '%s'.", backup_name);
				put_tstr(backup_name);
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
		put_tstr(system_path);
		break;
	}

	default:
		bassert("Invalid module import policy!");
	}
	if (config) {
		state = import_module(assembly, config, local_path, import_from);
	} else {
		builder_msg(MSG_ERR,
		            ERR_FILE_NOT_FOUND,
		            TOKEN_OPTIONAL_LOCATION(import_from),
		            CARET_WORD,
		            "Module not found.");
	}
	put_tstr(local_path);
	confdelete(config);
DONE:
	return_zone(state);
}

DCpointer assembly_find_extern(struct assembly *assembly, const str_t symbol)
{
	char *tmp = tstr();
	// The str_t might not be zero terminated so we have to deal with it in the C world.
	strappend(tmp, "%.*s", symbol.len32, symbol.ptr);

	void              *handle = NULL;
	struct native_lib *lib;
	for (usize i = 0; i < arrlenu(assembly->libs); ++i) {
		lib    = &assembly->libs[i];
		handle = dlFindSymbol(lib->handle, tmp);
		if (handle) break;
	}
	put_tstr(tmp);
	return handle;
}
