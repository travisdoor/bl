#ifdef GEN_INSTR_SIZEOF
union __INSTR_T {\
    struct mir_instr_block           block;\
    struct mir_instr_decl_var        var;\
    struct mir_instr_decl_member     member;\
    struct mir_instr_decl_variant    variant;\
    struct mir_instr_decl_arg        decl_arg;\
    struct mir_instr_const           cnst;\
    struct mir_instr_load            load;\
    struct mir_instr_store           store;\
    struct mir_instr_ret             ret;\
    struct mir_instr_binop           binop;\
    struct mir_instr_fn_proto        fn_proto;\
    struct mir_instr_fn_group        fn_group;\
    struct mir_instr_decl_ref        decl_ref;\
    struct mir_instr_decl_direct_ref decl_direct_ref;\
    struct mir_instr_call            call;\
    struct mir_instr_unreachable     unreachable;\
    struct mir_instr_cond_br         cond_br;\
    struct mir_instr_br              br;\
    struct mir_instr_unop            unop;\
    struct mir_instr_arg             arg;\
    struct mir_instr_elem_ptr        elem_ptr;\
    struct mir_instr_member_ptr      member_ptr;\
    struct mir_instr_addrof          addrof;\
    struct mir_instr_type_array      type_array;\
    struct mir_instr_type_slice      type_slice;\
    struct mir_instr_type_dyn_arr     type_dynarr;\
    struct mir_instr_type_vargs      type_vargs;\
    struct mir_instr_type_ptr        type_ptr;\
    struct mir_instr_type_struct     type_struct;\
    struct mir_instr_type_fn         type_fn;\
    struct mir_instr_type_enum       type_enum;\
    struct mir_instr_type_fn_group   type_fn_group;\
    struct mir_instr_type_poly       type_poly;\
    struct mir_instr_cast            cast;\
    struct mir_instr_sizeof          szof;\
    struct mir_instr_alignof         alof;\
    struct mir_instr_compound        init;\
    struct mir_instr_vargs           vargs;\
    struct mir_instr_type_info       type_info;\
    struct mir_instr_phi             phi;\
    struct mir_instr_to_any          toany;\
    struct mir_instr_switch          sw;\
    struct mir_instr_set_initializer si;\
    struct mir_instr_test_case       tcs; \
    struct mir_instr_call_loc        cl; \
    struct mir_instr_unroll          unroll; \
};

#define SIZEOF_MIR_INSTR sizeof(union __INSTR_T)
#define ALIGNOF_MIR_INSTR alignment_of(union __INSTR_T)
#endif

#ifdef GEN_INSTR_KINDS
    MIR_INSTR_BLOCK,
    MIR_INSTR_DECL_VAR,
    MIR_INSTR_DECL_MEMBER,
    MIR_INSTR_DECL_VARIANT,
    MIR_INSTR_DECL_ARG,
    MIR_INSTR_CONST,
    MIR_INSTR_LOAD,
    MIR_INSTR_STORE,
    MIR_INSTR_BINOP,
    MIR_INSTR_RET,
    MIR_INSTR_FN_PROTO,
    MIR_INSTR_FN_GROUP,
    MIR_INSTR_TYPE_FN,
    MIR_INSTR_TYPE_STRUCT,
    MIR_INSTR_TYPE_PTR,
    MIR_INSTR_TYPE_ARRAY,
    MIR_INSTR_TYPE_SLICE,
    MIR_INSTR_TYPE_DYNARR,
    MIR_INSTR_TYPE_VARGS,
    MIR_INSTR_TYPE_ENUM,
    MIR_INSTR_TYPE_FN_GROUP,
    MIR_INSTR_TYPE_POLY,
    MIR_INSTR_CALL,
    MIR_INSTR_DECL_REF,
    MIR_INSTR_DECL_DIRECT_REF,
    MIR_INSTR_UNREACHABLE,
    MIR_INSTR_COND_BR,
    MIR_INSTR_BR,
    MIR_INSTR_UNOP,
    MIR_INSTR_ARG,
    MIR_INSTR_ELEM_PTR,
    MIR_INSTR_MEMBER_PTR,
    MIR_INSTR_ADDROF,
    MIR_INSTR_CAST,
    MIR_INSTR_SIZEOF,
    MIR_INSTR_ALIGNOF,
    MIR_INSTR_COMPOUND,
    MIR_INSTR_VARGS,
    MIR_INSTR_TYPE_INFO,
    MIR_INSTR_PHI,
    MIR_INSTR_TOANY,
    MIR_INSTR_SWITCH,
    MIR_INSTR_SET_INITIALIZER,
    MIR_INSTR_TEST_CASES,
    MIR_INSTR_CALL_LOC,
    MIR_INSTR_UNROLL,
#endif

#ifdef GEN_BUILTIN_NAMES
    MIR_BUILTIN_ID_TYPE_TYPE,
    MIR_BUILTIN_ID_TYPE_NAMED_SCOPE,
    MIR_BUILTIN_ID_TYPE_S8,
    MIR_BUILTIN_ID_TYPE_S16,
    MIR_BUILTIN_ID_TYPE_S32,
    MIR_BUILTIN_ID_TYPE_S64,
    MIR_BUILTIN_ID_TYPE_U8,
    MIR_BUILTIN_ID_TYPE_U16,
    MIR_BUILTIN_ID_TYPE_U32,
    MIR_BUILTIN_ID_TYPE_U64,
    MIR_BUILTIN_ID_TYPE_USIZE,
    MIR_BUILTIN_ID_TYPE_BOOL,
    MIR_BUILTIN_ID_TYPE_F32,
    MIR_BUILTIN_ID_TYPE_F64,
    MIR_BUILTIN_ID_TYPE_VOID,
    MIR_BUILTIN_ID_TYPE_STRING,
    MIR_BUILTIN_ID_NULL,
    MIR_BUILTIN_ID_MAIN,
    MIR_BUILTIN_ID_ARR_LEN,
    MIR_BUILTIN_ID_ARR_PTR,
    MIR_BUILTIN_ID_ARR_ALLOCATED,
    MIR_BUILTIN_ID_ARR_ALLOCATOR,
    MIR_BUILTIN_ID_STRUCT_BASE,
    MIR_BUILTIN_ID_ANY,
    MIR_BUILTIN_ID_TYPE_KIND,
    MIR_BUILTIN_ID_TYPE_INFO,
    MIR_BUILTIN_ID_TYPE_INFO_TYPE,
    MIR_BUILTIN_ID_TYPE_INFO_VOID,
    MIR_BUILTIN_ID_TYPE_INFO_INT,
    MIR_BUILTIN_ID_TYPE_INFO_REAL,
    MIR_BUILTIN_ID_TYPE_INFO_FN,
    MIR_BUILTIN_ID_TYPE_INFO_PTR,
    MIR_BUILTIN_ID_TYPE_INFO_BOOL,
    MIR_BUILTIN_ID_TYPE_INFO_ARRAY,
    MIR_BUILTIN_ID_TYPE_INFO_STRUCT,
    MIR_BUILTIN_ID_TYPE_INFO_ENUM,
    MIR_BUILTIN_ID_TYPE_INFO_NULL,
    MIR_BUILTIN_ID_TYPE_INFO_STRING,
    MIR_BUILTIN_ID_TYPE_INFO_VARGS,
    MIR_BUILTIN_ID_TYPE_INFO_SLICE,
    MIR_BUILTIN_ID_TYPE_INFO_STRUCT_MEMBER,
    MIR_BUILTIN_ID_TYPE_INFO_ENUM_VARIANT,
    MIR_BUILTIN_ID_TYPE_INFO_FN_ARG,
    MIR_BUILTIN_ID_TYPE_INFO_FN_GROUP,
    MIR_BUILTIN_ID_ABORT_FN,
    MIR_BUILTIN_ID_ASSERT_FN,
    MIR_BUILTIN_ID_IS_DEBUG,
    MIR_BUILTIN_ID_TYPE_TEST_CASES,
    MIR_BUILTIN_ID_TYPE_CALL_LOCATION,
    MIR_BUILTIN_ID_COMMAND_LINE_ARGUMENTS,
    MIR_BUILTIN_ID_IS_COMPTIME_RUN,
    MIR_BUILTIN_ID_ARCH_ENUM,
    MIR_BUILTIN_ID_ARCH,
    MIR_BUILTIN_ID_PLATFORM_ENUM,
    MIR_BUILTIN_ID_PLATFORM,
    MIR_BUILTIN_ID_ENV_ENUM,
    MIR_BUILTIN_ID_ENV,
#endif

#ifdef GEN_BUILTIN_IDS
    {.str = "type",                  .hash = 0},
    {.str = "scope",                 .hash = 0},
    {.str = "s8",                    .hash = 0},
    {.str = "s16",                   .hash = 0},
    {.str = "s32",                   .hash = 0},
    {.str = "s64",                   .hash = 0},
    {.str = "u8",                    .hash = 0},
    {.str = "u16",                   .hash = 0},
    {.str = "u32",                   .hash = 0},
    {.str = "u64",                   .hash = 0},
    {.str = "usize",                 .hash = 0},
    {.str = "bool",                  .hash = 0},
    {.str = "f32",                   .hash = 0},
    {.str = "f64",                   .hash = 0},
    {.str = "void",                  .hash = 0},
    {.str = "string",                .hash = 0},
    {.str = "null_t",                .hash = 0},
    {.str = "main",                  .hash = 0},
    {.str = "len",                   .hash = 0},
    {.str = "ptr",                   .hash = 0},
    {.str = "allocated",             .hash = 0},
    {.str = "allocator",             .hash = 0},
    {.str = "base",                  .hash = 0},
    {.str = "Any",                   .hash = 0},
    {.str = "TypeKind",              .hash = 0},
    {.str = "TypeInfo",              .hash = 0},
    {.str = "TypeInfoType",          .hash = 0},
    {.str = "TypeInfoVoid",          .hash = 0},
    {.str = "TypeInfoInt",           .hash = 0},
    {.str = "TypeInfoReal",          .hash = 0},
    {.str = "TypeInfoFn",            .hash = 0},
    {.str = "TypeInfoPtr",           .hash = 0},
    {.str = "TypeInfoBool",          .hash = 0},
    {.str = "TypeInfoArray",         .hash = 0},
    {.str = "TypeInfoStruct",        .hash = 0},
    {.str = "TypeInfoEnum",          .hash = 0},
    {.str = "TypeInfoNull",          .hash = 0},
    {.str = "TypeInfoString",        .hash = 0},
    {.str = "TypeInfoSlice",         .hash = 0},
    {.str = "TypeInfoVArgs",         .hash = 0},
    {.str = "TypeInfoStructMember",  .hash = 0},
    {.str = "TypeInfoEnumVariant",   .hash = 0},
    {.str = "TypeInfoFnArg",         .hash = 0},
    {.str = "TypeInfoFnGroup",       .hash = 0},
    {.str = "__os_abort",            .hash = 0},
    {.str = "assert",                .hash = 0},
    {.str = "IS_DEBUG",              .hash = 0},
    {.str = "TestCase",              .hash = 0},
    {.str = "CodeLocation",          .hash = 0},
    {.str = "command_line_arguments", .hash = 0},
    {.str = "IS_COMPTIME_RUN",       .hash = 0},
    {.str = "Arch",                  .hash = 0},
    {.str = "ARCH",                  .hash = 0},
    {.str = "Platform",              .hash = 0},
    {.str = "PLATFORM",              .hash = 0},
    {.str = "Env",                   .hash = 0},
    {.str = "ENV",                   .hash = 0},
#endif
