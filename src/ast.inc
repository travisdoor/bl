#ifdef GEN_AST_KINDS
    AST_BAD,
    AST_LOAD,
    AST_IMPORT,
    AST_PRIVATE,
    AST_SCOPE,
    AST_LINK,
    AST_IDENT,
    AST_UBLOCK,
    AST_BLOCK,
    AST_UNREACHABLE,
    AST_TAGS,
    AST_CALL_LOC,
    AST_DOCS,
    AST_REF,
    AST_DECL_ENTITY,
    AST_DECL_MEMBER,
    AST_DECL_ARG,
    AST_DECL_VARIANT,
    AST_STMT_RETURN,
    AST_STMT_IF,
    AST_STMT_LOOP,
    AST_STMT_BREAK,
    AST_STMT_CONTINUE,
    AST_STMT_DEFER,
    AST_STMT_SWITCH,
    AST_STMT_CASE,
    AST_TYPE_ARR,
    AST_TYPE_POLY,
    AST_TYPE_SLICE,
    AST_TYPE_DYNARR,
    AST_TYPE_FN,
    AST_TYPE_STRUCT,
    AST_TYPE_ENUM,
    AST_TYPE_PTR,
    AST_TYPE_VARGS,
    AST_TYPE_FN_GROUP,
    AST_EXPR_TYPE,
    AST_EXPR_CAST,
    AST_EXPR_BINOP,
    AST_EXPR_CALL,
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
#endif

#ifdef GEN_AST_DATA
    struct AstPrivate        priv;
    struct AstScope          scope;
    struct AstLoad           load;
    struct AstImport         import;
    struct AstLink           link;
    struct AstIdent          ident;
    struct AstUBlock         ublock;
    struct AstBlock          block;
    struct AstTags           tags;
    struct AstCallLoc        call_loc;
    struct AstDocs           docs;
    struct AstRef            ref;
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
    struct AstTypeArr        type_arr;
    struct AstTypePoly       type_poly;
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
    struct AstExprCast       expr_cast;
    struct AstExprBinop      expr_binop;
    struct AstExprCall       expr_call;
    struct AstExprElem       expr_elem;
    struct AstExprSizeof     expr_sizeof;
    struct AstExprTypeInfo   expr_type_info;
    struct AstExprAlignof    expr_alignof;
    struct AstExprUnary      expr_unary;
    struct AstExprAddrOf     expr_addrof;
    struct AstExprDeref      expr_deref;
    struct AstExprCompound   expr_compound;
#endif
