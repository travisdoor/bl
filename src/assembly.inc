#ifdef GEN_BUILTIN_TYPES
MirType *t_type;
MirType *t_s8;
MirType *t_s16;
MirType *t_s32;
MirType *t_s64;
MirType *t_u8;
MirType *t_u16;
MirType *t_u32;
MirType *t_u64;
MirType *t_usize;
MirType *t_bool;
MirType *t_f32;
MirType *t_f64;
MirType *t_string;
MirType *t_void;
MirType *t_u8_ptr;
MirType *t_string_ptr;
MirType *t_string_slice;
MirType *t_resolve_type_fn;
MirType *t_test_case_fn; // @INCOMPLETE: remove
MirType *t_Any;
MirType *t_Any_ptr;
MirType *t_TypeKind;
MirType *t_TypeInfo;
MirType *t_TypeInfoType;
MirType *t_TypeInfoVoid;
MirType *t_TypeInfoInt;
MirType *t_TypeInfoReal;
MirType *t_TypeInfoFn;
MirType *t_TypeInfoPtr;
MirType *t_TypeInfoArray;
MirType *t_TypeInfoStruct;
MirType *t_TypeInfoEnum;
MirType *t_TypeInfoNull;
MirType *t_TypeInfoBool;
MirType *t_TypeInfoString;
MirType *t_TypeInfoStructMember;
MirType *t_TypeInfoEnumVariant;
MirType *t_TypeInfoFnArg;
MirType *t_TypeInfo_ptr;
MirType *t_TypeInfo_slice;
MirType *t_TypeInfoStructMembers_slice;
MirType *t_TypeInfoEnumVariants_slice;
MirType *t_TypeInfoFnArgs_slice;
MirType *t_TypeInfoFnGroup;
MirType *t_TypeInfoFn_ptr;
MirType *t_TypeInfoFn_ptr_slice;
MirType *t_TestCase;
MirType *t_TestCases_slice;
MirType *t_CodeLocation;
MirType *t_CodeLocation_ptr;
#endif

#if defined(GEN_ASSEMBLY_OPT_DECLS) || defined(GEN_ASSEMBLY_OPT_DEFAULTS)
#define OPTS_SHARED                                                                                \
    OPT(AssemblyOpt, opt, builder.options.assembly_opt)                                            \
    OPT(AssemblyDIKind, di_kind, builder.options.assembly_di_kind)                                 \
    OPT(bool, run, builder.options.run)                                                            \
    OPT(bool, run_tests, builder.options.run_tests)                                                \
    OPT(bool, copy_deps, false)                                                                    \
    OPT(bool, verify_llvm, builder.options.verify_llvm)                                            \
    OPT(bool, reg_split, builder.options.reg_split)                                                \
    OPT(bool, print_tokens, builder.options.print_tokens)                                          \
    OPT(bool, print_ast, builder.options.print_ast)                                                \
    OPT(bool, emit_llvm, builder.options.emit_llvm)                                                \
    OPT(bool, emit_mir, builder.options.emit_mir)                                                  \
    OPT(bool, no_bin, builder.options.no_bin)                                                      \
    OPT(bool, no_llvm, builder.options.no_llvm)                                                    \
    OPT(bool, no_analyze, builder.options.no_analyze)                                              \
    OPT(bool, syntax_only, builder.options.syntax_only)

#define OPTS OPT(ModuleImportPolicy, module_import_policy, IMPORT_POLICY_SYSTEM)
#endif

#if defined(GEN_ASSEMBLY_OPT_DECLS)
#define OPT(type, name, default_value) type name;
OPTS_SHARED
OPTS
#undef OPT
#undef OPTS
#undef OPTS_SHARED
#elif defined(GEN_ASSEMBLY_OPT_DEFAULTS)
#define OPT(type, name, default_value) assembly->options.name = default_value;
OPTS_SHARED
OPTS
#undef OPT
#undef OPTS
#undef OPTS_SHARED
#endif
