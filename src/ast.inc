#ifdef GEN_AST_KINDS
	AST_BAD,
	AST_LOAD,
	AST_IMPORT,
	AST_PRIVATE,
	AST_LINK,
	AST_IDENT,
	AST_UBLOCK,
	AST_BLOCK,
	AST_UNREACHABLE,
	AST_META_DATA,
	AST_TAGS,
	AST_CALL_LOC,
	_AST_DECL_FIRST,
	AST_DECL_ENTITY,
	AST_DECL_MEMBER,
	AST_DECL_ARG,
	AST_DECL_VARIANT,
	_AST_DECL_LAST,
	AST_STMT_RETURN,
	AST_STMT_IF,
	AST_STMT_LOOP,
	AST_STMT_BREAK,
	AST_STMT_CONTINUE,
	AST_STMT_DEFER,
	AST_STMT_SWITCH,
	AST_STMT_CASE,
	_AST_TYPE_FIRST,
	AST_TYPE_REF,
	AST_TYPE_ARR,
	AST_TYPE_SLICE,
	AST_TYPE_DYNARR,
	AST_TYPE_FN,
	AST_TYPE_STRUCT,
	AST_TYPE_ENUM,
	AST_TYPE_PTR,
	AST_TYPE_VARGS,
	AST_TYPE_FN_GROUP,
	_AST_TYPE_LAST,
	_AST_EXPR_FIRST,
	AST_EXPR_TYPE,
	AST_EXPR_REF,
	AST_EXPR_CAST,
	AST_EXPR_BINOP,
	AST_EXPR_CALL,
	AST_EXPR_MEMBER,
	AST_EXPR_ELEM,
	AST_EXPR_SIZEOF,
	AST_EXPR_ALIGNOF,
	AST_EXPR_TYPEOF,
	AST_EXPR_TYPE_INFO,
	AST_EXPR_TEST_CASES,
	AST_EXPR_UNARY,
	AST_EXPR_NULL,
	AST_EXPR_ADDROF,
	AST_EXPR_DEREF,
	AST_EXPR_COMPOUND,
	AST_EXPR_LIT_FN,
	AST_EXPR_LIT_INT,
	AST_EXPR_LIT_FLOAT,
	AST_EXPR_LIT_DOUBLE,
	AST_EXPR_LIT_CHAR,
	AST_EXPR_LIT_STRING,
	AST_EXPR_LIT_BOOL,
	AST_EXPR_LIT_FN_GROUP,
	_AST_EXPR_LAST,
#endif

#ifdef GEN_AST_DATA
	struct AstPrivate        priv;
	struct AstMetaData       meta_data;
	struct AstLoad           load;
	struct AstImport         import;
	struct AstLink           link;
	struct AstIdent          ident;
	struct AstUBlock         ublock;
	struct AstBlock          block;
	struct AstTags           tags;
	struct AstCallLoc        call_loc;
	struct AstStmtReturn     stmt_return;
	struct AstStmtIf         stmt_if;
	struct AstStmtLoop       stmt_loop;
	struct AstStmtDefer      stmt_defer;
	struct AstStmtSwitch     stmt_switch;
	struct AstStmtCase       stmt_case;
	struct AstDecl           decl;
	struct AstDeclEntity     decl_entity;
	struct AstDeclArg        decl_arg;
	struct AstDeclMember     decl_member;
	struct AstDeclVariant    decl_variant;
	struct AstTypeRef        type_ref;
	struct AstTypeArr        type_arr;
	struct AstTypeSlice      type_slice;
	struct AstTypeDynArr     type_dynarr;
	struct AstTypeFn         type_fn;
	struct AstTypeStruct     type_strct;
	struct AstTypeEnum       type_enm;
	struct AstTypePtr        type_ptr;
	struct AstTypeVargs      type_vargs;
	struct AstTypeFnGroup    type_fn_group;
	struct AstExprType       expr_type;
	struct AstExprLitFn      expr_fn;
	struct AstExprLitInt     expr_integer;
	struct AstExprLitFloat   expr_float;
	struct AstExprLitDouble  expr_double;
	struct AstExprLitChar    expr_character;
	struct AstExprLitString  expr_string;
	struct AstExprLitBool    expr_boolean;
	struct AstExprLitFnGroup expr_fn_group;
	struct AstExprRef        expr_ref;
	struct AstExprCast       expr_cast;
	struct AstExprBinop      expr_binop;
	struct AstExprCall       expr_call;
	struct AstExprMember     expr_member;
	struct AstExprElem       expr_elem;
	struct AstExprSizeof     expr_sizeof;
	struct AstExprTypeInfo   expr_type_info;
	struct AstExprAlignof    expr_alignof;
	struct AstExprUnary      expr_unary;
	struct AstExprAddrOf     expr_addrof;
	struct AstExprDeref      expr_deref;
	struct AstExprCompound   expr_compound;
#endif
