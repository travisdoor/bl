#ifdef GEN_INSTR_SIZEOF
#define SIZEOF_MIR_INSTR sizeof(union {\
	MirInstrBlock          block;\
	MirInstrDeclVar        var;\
	MirInstrDeclMember     member;\
	MirInstrDeclVariant    variant;\
	MirInstrDeclArg        decl_arg;\
	MirInstrConst          cnst;\
	MirInstrLoad           load;\
	MirInstrStore          store;\
	MirInstrRet            ret;\
	MirInstrBinop          binop;\
	MirInstrFnProto        fn_proto;\
	MirInstrDeclRef        decl_ref;\
	MirInstrDeclDirectRef  decl_direct_ref;\
	MirInstrCall           call;\
	MirInstrUnreachable    unreachable;\
	MirInstrCondBr         cond_br;\
	MirInstrBr             br;\
	MirInstrUnop           unop;\
	MirInstrArg            arg;\
	MirInstrElemPtr        elem_ptr;\
	MirInstrMemberPtr      member_ptr;\
	MirInstrAddrOf         addrof;\
	MirInstrTypeArray      type_array;\
	MirInstrTypeSlice      type_slice;\
	MirInstrTypeDynArr     type_dynarr;\
	MirInstrTypeVArgs      type_vargs;\
	MirInstrTypePtr        type_ptr;\
	MirInstrTypeStruct     type_struct;\
	MirInstrTypeFn         type_fn;\
	MirInstrTypeEnum       type_enum;\
	MirInstrCast           cast;\
	MirInstrSizeof         szof;\
	MirInstrAlignof        alof;\
	MirInstrCompound       init;\
	MirInstrVArgs          vargs;\
	MirInstrTypeInfo       type_info;\
	MirInstrPhi            phi;\
	MirInstrToAny          toany;\
	MirInstrSwitch         sw;\
	MirInstrSetInitializer si;\
})
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
	MIR_INSTR_TYPE_FN,
	MIR_INSTR_TYPE_STRUCT,
	MIR_INSTR_TYPE_PTR,
	MIR_INSTR_TYPE_ARRAY,
	MIR_INSTR_TYPE_SLICE,
	MIR_INSTR_TYPE_DYNARR,
	MIR_INSTR_TYPE_VARGS,
	MIR_INSTR_TYPE_ENUM,
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
#endif

#ifdef GEN_BUILTIN_NAMES
	MIR_BUILTIN_ID_TYPE_TYPE,
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
	MIR_BUILTIN_ID_ARR_ELEM_SIZE,
	MIR_BUILTIN_ID_STRUCT_BASE,

	MIR_BUILTIN_ID_ANY,
	MIR_BUILTIN_ID_ARRAY,
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

	MIR_BUILTIN_ID_ABORT_FN,
	MIR_BUILTIN_ID_ARRAY_PUSH_FN,
	MIR_BUILTIN_ID_ARRAY_TERMINATE_FN,
	MIR_BUILTIN_ID_ARRAY_RESERVE_FN,
	MIR_BUILTIN_ID_ARRAY_ERASE_FN,
	MIR_BUILTIN_ID_ARRAY_CLEAR_FN,
	MIR_BUILTIN_ID_ARRAY_INIT_FN,

	MIR_BUILTIN_ID_IS_DEBUG,
#endif

#ifdef GEN_BUILTIN_IDS
    {.str = "type",                  .hash = 0},
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
    {.str = "elem_size",             .hash = 0},
    {.str = "base",                  .hash = 0},
    {.str = "Any",                   .hash = 0},
    {.str = "_Array",                .hash = 0},
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
    {.str = "__os_abort",            .hash = 0},
    {.str = "array_push",            .hash = 0},
    {.str = "array_terminate",       .hash = 0},
    {.str = "array_reserve",         .hash = 0},
    {.str = "array_erase",           .hash = 0},
    {.str = "array_clear",           .hash = 0},
    {.str = "array_init",            .hash = 0},
    {.str = "IS_DEBUG",              .hash = 0},
#endif
