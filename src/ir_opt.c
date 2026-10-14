#include "bldebug.h"
#include "builder.h"
#include "stb_ds.h"

void ir_opt_run(struct assembly *assembly) {
	zone();
	LLVMModuleRef llvm_module = assembly->llvm.module;

	const bool use_asan    = assembly->target->sanitize_address;
	const u32  hot_fns_num = (u32)arrlenu(assembly->llvm.hot_fns);

	if (assembly->target->opt == ASSEMBLY_OPT_DEBUG && !use_asan) {
		if (hot_fns_num) {
			llvm_opt_run_on_functions(llvm_module, assembly->llvm.hot_fns, hot_fns_num, LLVMCodeGenLevelAggressive);
		}

		return_zone();
	}

	str_buf_t tmp = get_tmp_str();
	str_t     opt = opt_to_LLVM_pass_str(assembly->target->opt);

	if (use_asan) str_buf_append_fmt(&tmp, "asan,");
	str_buf_append_fmt(&tmp, "default<{str}>", opt);

	LLVMTargetMachineRef      llvm_tm = assembly->llvm.TM;
	LLVMPassBuilderOptionsRef options = LLVMCreatePassBuilderOptions();

	LLVMErrorRef err = LLVMRunPasses(llvm_module, str_buf_to_c(tmp), llvm_tm, options);

	if (err != LLVMErrorSuccess) {
		char *msg = LLVMGetErrorMessage(err);
		builder_error("LLVM error: %s", msg);
		LLVMDisposeErrorMessage(msg);
	}

	LLVMDisposePassBuilderOptions(options);

	put_tmp_str(tmp);
	return_zone();
}
