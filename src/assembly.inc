#ifdef GEN_BUILTIN_TYPES
struct bl_type *t_type;
struct bl_type *t_scope;
struct bl_type *t_s8;
struct bl_type *t_s16;
struct bl_type *t_s32;
struct bl_type *t_s64;
struct bl_type *t_u8;
struct bl_type *t_u16;
struct bl_type *t_u32;
struct bl_type *t_u64;
struct bl_type *t_usize;
struct bl_type *t_bool;
struct bl_type *t_f32;
struct bl_type *t_f64;
struct bl_type *t_string;
struct bl_type *t_void;
struct bl_type *t_u8_ptr;
struct bl_type *t_string_ptr;
struct bl_type *t_string_slice;
struct bl_type *t_resolve_type_fn;
struct bl_type *t_test_case_fn; // @INCOMPLETE: remove
struct bl_type *t_Any;
struct bl_type *t_Any_ptr;
struct bl_type *t_TypeKind;
struct bl_type *t_TypeInfo;
struct bl_type *t_TypeInfoType;
struct bl_type *t_TypeInfoVoid;
struct bl_type *t_TypeInfoInt;
struct bl_type *t_TypeInfoReal;
struct bl_type *t_TypeInfoFn;
struct bl_type *t_TypeInfoPtr;
struct bl_type *t_TypeInfoArray;
struct bl_type *t_TypeInfoStruct;
struct bl_type *t_TypeInfoEnum;
struct bl_type *t_TypeInfoNull;
struct bl_type *t_TypeInfoBool;
struct bl_type *t_TypeInfoString;
struct bl_type *t_TypeInfoStructMember;
struct bl_type *t_TypeInfoEnumVariant;
struct bl_type *t_TypeInfoFnArg;
struct bl_type *t_TypeInfo_ptr;
struct bl_type *t_TypeInfo_slice;
struct bl_type *t_TypeInfoStructMembers_slice;
struct bl_type *t_TypeInfoEnumVariants_slice;
struct bl_type *t_TypeInfoFnArgs_slice;
struct bl_type *t_TypeInfoFnGroup;
struct bl_type *t_TypeInfoFn_ptr;
struct bl_type *t_TypeInfoFn_ptr_slice;
struct bl_type *t_TestCase;
struct bl_type *t_TestCases_slice;
struct bl_type *t_CodeLocation;
struct bl_type *t_CodeLocation_ptr;
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