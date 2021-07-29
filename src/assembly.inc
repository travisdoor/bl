#ifdef GEN_BUILTIN_TYPES
MirType *t_type;
MirType *t_scope;
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

#ifdef GEN_ARCH
entry(unknown)
entry(x86_64)
entry(aarch64)
entry(arm64)
#endif

#ifdef GEN_OS
entry(unknown)
entry(windows)
entry(darwin)
entry(linux)
#endif

#ifdef GEN_VENDOR
entry(unknown)
entry(pc)
entry(apple)
#endif

#ifdef GEN_ENV
entry(unknown)
entry(msvc)
entry(gnu)
#endif