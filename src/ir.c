//************************************************************************************************
// bl
//
// File:   ir.c
// Author: Martin Dorazil
// Date:   12/7/18
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
//************************************************************************************************

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Linker.h>
#include <llvm-c/DebugInfo.h>
#include "stages.h"
#include "common.h"
#include "ast.h"

#if BL_DEBUG
#define gname(s) s
#define JIT_OPT_LEVEL 0
#else
#define gname(s) ""
#define JIT_OPT_LEVEL 3
#endif

#define VERBOSE 0
#define VALIDATE 1

typedef struct
{
  Builder *              builder;
  Assembly *             assembly;
  BHashTable *           llvm_values;
  bool                   is_gscope;
  BHashTable *           llvm_strings;
  LLVMModuleRef          llvm_module;
  LLVMBuilderRef         llvm_builder;
  LLVMContextRef         llvm_cnt;
  BHashTable *           llvm_modules;
  LLVMExecutionEngineRef llvm_jit;
  BHashTable *           jit_linked;
  Node *                 main_tmp;
  LLVMBasicBlockRef      break_block;
  LLVMBasicBlockRef      continue_block;
  LLVMBasicBlockRef      fn_init_block;
  LLVMBasicBlockRef      fn_ret_block;
  LLVMBasicBlockRef      fn_entry_block;
  LLVMValueRef           fn_ret_val;
} Context;

typedef enum
{
  RR_U64,
  RR_S64,
  RR_REAL,
  RR_VOID,
} RunResultType;

typedef struct
{
  RunResultType type;
  union
  {
    uint64_t u64;
    int64_t  s64;
    double   real;
  };
} RunResult;

static void
print_llvm_module(LLVMModuleRef module);

static void
generate(Context *cnt);

static void
generate_decl(Context *cnt, Node *decl);

static LLVMModuleRef
link(Context *cnt, Node *entry);

/* resursive version */
static LLVMModuleRef
_link(Context *cnt, Node *entry);

static void
create_jit(Context *cnt);

static void
link_into_jit(Context *cnt, Node *fn);

/* check if declaration has all it's dependencies already generated in LLVM Modules, by
 * 'strict_only' tag we can check onlu strict dependencies caused by '#run' directive */
static bool
is_satisfied(Context *cnt, Node *decl, bool strict_only);

static RunResult
run(Context *cnt, Node *fn);

static LLVMValueRef
ir_node(Context *cnt, Node *node);

static LLVMValueRef
ir_decl(Context *cnt, Node *decl);

static LLVMValueRef
ir_fn_get(Context *cnt, Node *fn);

static LLVMValueRef
ir_global_get(Context *cnt, Node *global);

static LLVMValueRef
ir_decl_fn(Context *cnt, Node *decl);

static LLVMValueRef
ir_decl_mut(Context *cnt, Node *decl);

#if 0
static LLVMValueRef
ir_decl_immut(Context *cnt, Node *decl);
#endif

static LLVMValueRef
ir_block(Context *cnt, Node *block);

static LLVMValueRef
ir_lit(Context *cnt, Node *lit);

static LLVMValueRef
ir_lit_cmp(Context *cnt, Node *lit);

static LLVMValueRef
ir_expr_binop(Context *cnt, Node *binop);

static LLVMValueRef
ir_expr_unary(Context *cnt, Node *unary);

static LLVMValueRef
ir_expr_call(Context *cnt, Node *call);

static LLVMValueRef
ir_expr_call_rt(Context *cnt, Node *call);

static LLVMValueRef
ir_expr_call_ct(Context *cnt, Node *call);

static LLVMValueRef
ir_expr_member(Context *cnt, Node *member);

static LLVMValueRef
ir_expr_elem(Context *cnt, Node *elem);

static LLVMValueRef
ir_expr_null(Context *cnt, Node *nl);

static LLVMValueRef
ir_ident(Context *cnt, Node *ident);

static LLVMValueRef
ir_expr_cast(Context *cnt, Node *cast);

static inline LLVMValueRef
ir_expr_sizeof(Context *cnt, Node *szof);

static LLVMValueRef
ir_stmt_if(Context *cnt, Node *stmt_if);

static LLVMValueRef
ir_stmt_return(Context *cnt, Node *stmt_return);

static LLVMValueRef
ir_stmt_loop(Context *cnt, Node *loop);

static inline LLVMValueRef
ir_stmt_break(Context *cnt, Node *brk);

static inline LLVMValueRef
ir_stmt_continue(Context *cnt, Node *cont);

static LLVMTypeRef
to_llvm_type(Context *cnt, Node *type);

// impl
void
print_llvm_module(LLVMModuleRef module)
{
#if VERBOSE
  {
    char *str = LLVMPrintModuleToString(module);
    bl_log("\n--------------------------------------------------------------------------------"
           "\n%s"
           "\n--------------------------------------------------------------------------------",
           str);
    LLVMDisposeMessage(str);
  }
#endif
}

static void
ir_validate(LLVMModuleRef module)
{
#if VALIDATE
  char *error = NULL;
  if (LLVMVerifyModule(module, LLVMReturnStatusAction, &error)) {
    char *str = LLVMPrintModuleToString(module);
    bl_abort("module not verified with error: %s\n%s", error, str);
  }
  LLVMDisposeMessage(error);
#endif
}

static inline void
llvm_values_insert(Context *cnt, Node *node, void *val)
{
  bo_htbl_insert(cnt->llvm_values, (uint64_t)node, val);
}

static inline void *
llvm_values_get(Context *cnt, Node *node)
{
  if (bo_htbl_has_key(cnt->llvm_values, (uint64_t)node))
    return bo_htbl_at(cnt->llvm_values, (uint64_t)node, void *);
  return NULL;
}

static inline void
llvm_values_reset(Context *cnt)
{
  bo_htbl_clear(cnt->llvm_values);
}

static inline void
llvm_strings_insert(Context *cnt, uint64_t hash, LLVMValueRef val)
{
  bo_htbl_insert(cnt->llvm_strings, hash, val);
}

static inline LLVMValueRef
llvm_strings_get(Context *cnt, uint64_t hash)
{
  if (bo_htbl_has_key(cnt->llvm_strings, hash))
    return bo_htbl_at(cnt->llvm_strings, hash, LLVMValueRef);
  return NULL;
}

static inline void
llvm_strings_reset(Context *cnt)
{
  bo_htbl_clear(cnt->llvm_strings);
}

static inline bool
is_terminated(Context *cnt)
{
  return (!cnt->is_gscope && LLVMGetInsertBlock((cnt)->llvm_builder) &&
          LLVMGetBasicBlockTerminator(LLVMGetInsertBlock((cnt)->llvm_builder)) != NULL);
}

static inline bool
should_load(Node *node, LLVMValueRef llvm_value)
{
  if ((node_is(node, NODE_EXPR_UNARY) && peek_expr_unary(node)->op == SYM_ASTERISK)) return true;

  if (node_is(node, NODE_EXPR_MEMBER) && peek_expr_member(node)->kind == MEM_KIND_STRUCT)
    return true;

  if (node_is(node, NODE_EXPR_ELEM)) return true;
  if (LLVMIsAAllocaInst(llvm_value) || LLVMIsAGlobalVariable(llvm_value)) return true;

  return false;
}

LLVMTypeRef
to_llvm_type(Context *cnt, Node *type)
{
  assert(type);
  LLVMTypeRef result = NULL;
  //int         ptr    = ast_type_get_ptr(type);
  int ptr = 0;

  switch (node_code(type)) {
  case NODE_TYPE_FUND: {
    NodeTypeFund *_type = peek_type_fund(type);
    switch (_type->code) {
    case FTYPE_VOID:
      result = LLVMVoidTypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_CHAR:
    case FTYPE_S8:
    case FTYPE_U8:
      result = LLVMInt8TypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_S16:
    case FTYPE_U16:
      result = LLVMInt16TypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_S32:
    case FTYPE_U32:
      result = LLVMInt32TypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_S64:
    case FTYPE_U64:
      result = LLVMInt64TypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_F32:
      result = LLVMFloatTypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_F64:
      result = LLVMDoubleTypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_STRING:
      result = LLVMPointerType(LLVMInt8TypeInContext(cnt->llvm_cnt), 0);
      break;
    case FTYPE_BOOL:
      result = LLVMInt1TypeInContext(cnt->llvm_cnt);
      break;
    case FTYPE_SIZE:
      /* TODO: use target setup later */
      if (sizeof(size_t) == 4) {
        result = LLVMInt32TypeInContext(cnt->llvm_cnt);
      } else if (sizeof(size_t) == 8) {
        result = LLVMInt64TypeInContext(cnt->llvm_cnt);
      } else {
        bl_abort("unsupported architecture");
      }
      break;
    default:
      bl_abort("unknown fundamenetal type %s", node_name(type));
    }
    break;
  }

  case NODE_TYPE_ARR: {
    /* array */
    NodeTypeArr *_arr_type = peek_type_arr(type);
    assert(_arr_type->elem_type);
    const bool is_ref = (bool)!_arr_type->len;

    LLVMTypeRef llvm_elem_type = to_llvm_type(cnt, _arr_type->elem_type);

    if (is_ref) {
      /* array is only reference -> generate implicit compound type */
      LLVMTypeRef types[2];
      types[0] = LLVMPointerType(llvm_elem_type, 0);
      types[1] = LLVMInt64TypeInContext(cnt->llvm_cnt);
      result   = LLVMStructType(types, 2, false);
    } else {
      unsigned int arr_size = 0;
      if (node_is(_arr_type->len, NODE_EXPR_CALL)) {
        NodeExprCall *_call = peek_expr_call(_arr_type->len);
        assert(_call->run);
        RunResult rr = run(cnt, _call->ref);

        switch (rr.type) {
        case RR_S64:
          arr_size = (unsigned int)rr.s64;
          break;
        case RR_U64:
          arr_size = (unsigned int)rr.u64;
          break;
        default:
          bl_abort("invalid type of array size called function");
        }
      } else if (node_is(_arr_type->len, NODE_LIT)) {
        arr_size = (unsigned int)peek_lit(_arr_type->len)->value.u;
      }

      assert(arr_size);
      result = LLVMArrayType(llvm_elem_type, arr_size);
    }
    break;
  }

  case NODE_TYPE_FN: {
    /* args */
    NodeTypeFn *_fn_type = peek_type_fn(type);

    LLVMTypeRef  llvm_ret       = to_llvm_type(cnt, ast_get_type(_fn_type->ret_type));
    LLVMTypeRef *llvm_arg_types = bl_malloc(sizeof(LLVMTypeRef) * _fn_type->argc_types);
    if (!llvm_arg_types) bl_abort("bad alloc");

    Node *   arg;
    Node *   tmp_type;
    unsigned i = 0;
    node_foreach(_fn_type->arg_types, arg)
    {
      tmp_type            = ast_get_type(arg);
      llvm_arg_types[i++] = to_llvm_type(cnt, tmp_type);
    }

    result = LLVMFunctionType(llvm_ret, llvm_arg_types, i, false);
    bl_free(llvm_arg_types);
    break;
  }

  case NODE_TYPE_STRUCT: {
#if 0
    NodeTypeStruct *_struct_type = peek_type_struct(type);

    LLVMTypeRef *llvm_member_types = bl_malloc(sizeof(LLVMTypeRef) * _struct_type->typesc);
    if (!llvm_member_types) bl_abort("bad alloc");

    Node *   member;
    Node *   tmp_type;
    unsigned i = 0;

    if (_struct_type->base_decl) {
      result = llvm_values_get(cnt, _struct_type->base_decl);
      if (!result) {
        NodeDecl *_base_decl = peek_decl(_struct_type->base_decl);
        assert(_base_decl->value);
        const char *name = peek_ident(_base_decl->name)->str;
        assert(name);

        /* create new one named structure */
        result = LLVMStructCreateNamed(cnt->llvm_cnt, name);
        llvm_values_insert(cnt, _struct_type->base_decl, result);

        node_foreach(_struct_type->types, member)
        {
          tmp_type               = ast_get_type(member);
          llvm_member_types[i++] = to_llvm_type(cnt, tmp_type);
        }
        LLVMStructSetBody(result, llvm_member_types, i, false);
      }
    } else {
      /* anonymous structure type */
      node_foreach(_struct_type->types, member)
      {
        tmp_type               = ast_get_type(member);
        llvm_member_types[i++] = to_llvm_type(cnt, tmp_type);
      }
      result = LLVMStructTypeInContext(cnt->llvm_cnt, llvm_member_types, i, false);
    }
    bl_free(llvm_member_types);
#endif
    break;
  }

  case NODE_TYPE_ENUM: {
    /*NodeTypeEnum *_enum_type = peek_type_enum(type);
    assert(_enum_type->base_type);
    result = to_llvm_type(cnt, _enum_type->base_type);*/
    break;
  }

  default:
    bl_abort("invalid node type %s", node_name(type));
  }

  if (ptr) {
    if (LLVMGetTypeKind(result) == LLVMVoidTypeKind) {
      result = LLVMInt8TypeInContext(cnt->llvm_cnt);
    }
    assert(ptr >= 1);
    for (int i = 0; i < ptr; ++i)
      result = LLVMPointerType(result, 0);
  }

  /*if (arr) {
    unsigned int arr_size = 0;
    if (node_is(arr, NODE_EXPR_CALL)) {
      NodeExprCall *_call = peek_expr_call(arr);
      assert(_call->run);
      RunResult rr = run(cnt, _call->ref);

      switch (rr.type) {
      case RR_S64:
        arr_size = (unsigned int)rr.s64;
        break;
      case RR_U64:
        arr_size = (unsigned int)rr.u64;
        break;
      default:
        bl_abort("invalid type of array size called function");
      }
    } else if (node_is(arr, NODE_LIT)) {
      arr_size = (unsigned int)peek_lit(arr)->value.u;
    }

    assert(arr_size);
    result = LLVMArrayType(result, arr_size);
    }*/

  return result;
}

LLVMValueRef
ir_lit(Context *cnt, Node *lit)
{
  LLVMValueRef result = NULL;
  NodeLit *    _lit   = peek_lit(lit);

#define PEEK_ULL _lit->value.u
#define PEEK_REAL _lit->value.d
#define PEEK_STR _lit->value.str
#define PEEK_CHAR (unsigned long long int)_lit->value.c

  switch (peek_type_fund(_lit->type)->code) {
  case FTYPE_S8:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case FTYPE_S16:
    result = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case FTYPE_S32:
    result = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case FTYPE_S64:
    result = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), PEEK_ULL, true);
    break;
  case FTYPE_U8:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case FTYPE_U16:
    result = LLVMConstInt(LLVMInt16TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case FTYPE_U32:
    result = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case FTYPE_U64:
  case FTYPE_SIZE:
    result = LLVMConstInt(LLVMInt64TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case FTYPE_F32:
    result = LLVMConstReal(LLVMFloatTypeInContext(cnt->llvm_cnt), PEEK_REAL);
    break;
  case FTYPE_F64:
    result = LLVMConstReal(LLVMDoubleTypeInContext(cnt->llvm_cnt), PEEK_REAL);
    break;
  case FTYPE_BOOL:
    result = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), PEEK_ULL, false);
    break;
  case FTYPE_CHAR:
    result = LLVMConstInt(LLVMInt8TypeInContext(cnt->llvm_cnt), PEEK_CHAR, false);
    break;

  case FTYPE_STRING: {
    uint64_t hash = bo_hash_from_str(PEEK_STR);
    result        = llvm_strings_get(cnt, hash);
    if (!result) {
      result = LLVMBuildGlobalStringPtr(cnt->llvm_builder, PEEK_STR, "str");
      llvm_strings_insert(cnt, hash, result);
    }
    break;
  }

  default:
    bl_abort("invalid constant type %s", node_name(lit));
  }

#undef PEEK_ULL
#undef PEEK_REAL
#undef PEEK_STR
#undef PEEK_CHAR

  return result;
}

LLVMValueRef
ir_lit_cmp(Context *cnt, Node *lit)
{
  LLVMValueRef result    = NULL;
  NodeLitCmp * _lit_cmp  = peek_lit_cmp(lit);
  LLVMTypeRef  llvm_type = to_llvm_type(cnt, _lit_cmp->type);

  if (cnt->is_gscope) {
    assert(_lit_cmp->fieldc);
    LLVMValueRef *llvm_values = bl_malloc(sizeof(LLVMValueRef) * _lit_cmp->fieldc);
    if (!llvm_values) bl_abort("bad alloc");

    Node *val;
    int   i = 0;
    node_foreach(_lit_cmp->fields, val)
    {
      llvm_values[i++] = ir_node(cnt, val);
    }

    result = LLVMConstNamedStruct(llvm_type, llvm_values, i);
    bl_free(llvm_values);
  } else {
    /* allocate tmp destination storage */
    LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    result = LLVMBuildAlloca(cnt->llvm_builder, llvm_type, gname("init"));
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);

    Node *field;
    int   i = 0;
    node_foreach(_lit_cmp->fields, field)
    {
      LLVMValueRef llvm_field_dest =
          LLVMBuildStructGEP(cnt->llvm_builder, result, (unsigned int)(i++), gname("elem"));
      LLVMValueRef llvm_field = ir_node(cnt, field);
      if (should_load(field, llvm_field))
        llvm_field = LLVMBuildLoad(cnt->llvm_builder, llvm_field, gname("tmp"));
      LLVMBuildStore(cnt->llvm_builder, llvm_field, llvm_field_dest);
    }
  }
  return result;
}

LLVMValueRef
ir_expr_call(Context *cnt, Node *call)
{
  assert(call);
  /* run in compile time or in runtime */
  if (peek_expr_call(call)->run) return ir_expr_call_ct(cnt, call);
  return ir_expr_call_rt(cnt, call);
}

LLVMValueRef
ir_expr_call_rt(Context *cnt, Node *call)
{
  LLVMValueRef  result        = NULL;
  NodeExprCall *_call         = peek_expr_call(call);
  NodeIdent *   _callee_ident = peek_ident(
      node_is(_call->ref, NODE_IDENT) ? _call->ref : peek_expr_member(_call->ref)->ident);

  LLVMValueRef llvm_fn = NULL;
  if (peek_decl(_callee_ident->ref)->mutable) {
    if (node_is(_call->ref, NODE_EXPR_MEMBER)) {
      llvm_fn = ir_expr_member(cnt, _call->ref);
    } else if (node_is(_call->ref, NODE_EXPR_ELEM)) {
      llvm_fn = ir_expr_elem(cnt, _call->ref);
    } else {
      llvm_fn = llvm_values_get(cnt, _callee_ident->ref);
    }
    llvm_fn = LLVMBuildLoad(cnt->llvm_builder, llvm_fn, gname("tmp"));
  } else {
    llvm_fn = ir_fn_get(cnt, _callee_ident->ref);
  }

  assert(llvm_fn);

  LLVMValueRef *llvm_args = bl_malloc(sizeof(LLVMValueRef) * _call->argsc);
  if (!llvm_args) bl_abort("bad alloc");

  Node *arg;
  int   i = 0;
  node_foreach(_call->args, arg)
  {
    llvm_args[i] = ir_node(cnt, arg);
    assert(llvm_args[i] && "invalid call argument");

    if (should_load(arg, llvm_args[i]))
      llvm_args[i] = LLVMBuildLoad(cnt->llvm_builder, llvm_args[i], gname("tmp"));

    ++i;
  }

  result = LLVMBuildCall(cnt->llvm_builder, llvm_fn, llvm_args, (unsigned int)_call->argsc, "");
  bl_free(llvm_args);
  return result;
}

LLVMValueRef
ir_expr_call_ct(Context *cnt, Node *call)
{
  NodeExprCall *_call = peek_expr_call(call);
  RunResult     rr    = run(cnt, _call->ref);

  LLVMTypeRef llvm_type = to_llvm_type(cnt, ast_unroll_ident(_call->type));

  switch (rr.type) {
  case RR_VOID:
    return NULL;
  case RR_S64:
    return LLVMConstInt(llvm_type, (unsigned long long)rr.s64, true);
  case RR_U64:
    return LLVMConstInt(llvm_type, rr.u64, false);
  case RR_REAL:
    return LLVMConstReal(llvm_type, rr.real);
  default:
    bl_abort("unsupported type of run result");
  }

  return NULL;
}

LLVMValueRef
ir_expr_member(Context *cnt, Node *member)
{
  NodeExprMember *_member = peek_expr_member(member);
  NodeIdent *     _ident  = peek_ident(_member->ident);

  LLVMValueRef result = NULL;

  if (_member->kind == MEM_KIND_STRUCT) {
    result = ir_node(cnt, _member->next);
    assert(result);
    assert(_member->i != -1);
    if (_member->ptr_ref && should_load(member, result))
      result = LLVMBuildLoad(cnt->llvm_builder, result, gname("tmp"));
    result =
        LLVMBuildStructGEP(cnt->llvm_builder, result, (unsigned int)_member->i, gname(_ident->str));
  } else if (_member->kind == MEM_KIND_ENUM) {
    NodeDecl *_decl_member = peek_decl(_ident->ref);
    assert(!_member->ptr_ref);
    assert(_decl_member->value);
    result = ir_node(cnt, _decl_member->value);
  } else {
    bl_abort("unknown member kind");
  }
  assert(result);
  return result;
}

LLVMValueRef
ir_expr_elem(Context *cnt, Node *elem)
{
  NodeExprElem *_elem = peek_expr_elem(elem);
  LLVMValueRef  ptr   = ir_node(cnt, _elem->next);

  assert(_elem->index && "invalid array element index");
  LLVMValueRef index = ir_node(cnt, _elem->index);

  if (should_load(_elem->index, index))
    index = LLVMBuildLoad(cnt->llvm_builder, index, gname("tmp"));

  LLVMValueRef indices[2];
  indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
  indices[1] = index;

  return LLVMBuildGEP(cnt->llvm_builder, ptr, indices, ARRAY_SIZE(indices), "");
}

LLVMValueRef
ir_expr_cast(Context *cnt, Node *cast)
{
  NodeExprCast *_cast          = peek_expr_cast(cast);
  LLVMTypeRef   llvm_dest_type = to_llvm_type(cnt, _cast->type);
  LLVMValueRef  llvm_src       = ir_node(cnt, _cast->next);

  if (should_load(_cast->next, llvm_src)) {
    llvm_src = LLVMBuildLoad(cnt->llvm_builder, llvm_src, gname("tmp"));
  }

  LLVMTargetDataRef  data_layout = LLVMGetModuleDataLayout(cnt->llvm_module);
  unsigned long long src_size    = LLVMSizeOfTypeInBits(data_layout, LLVMTypeOf(llvm_src));
  unsigned long long dest_size   = LLVMSizeOfTypeInBits(data_layout, llvm_dest_type);

  TypeKind src_kind  = ast_type_kind(ast_get_type(_cast->next));
  TypeKind dest_kind = ast_type_kind(_cast->type);

  /* types are same -> no cast needed */
  if (src_kind == dest_kind && src_size == dest_size && src_kind != TYPE_KIND_PTR) {
    // assert(false && "try to build cast on a same types");
    return llvm_src;
  }

  LLVMOpcode op = 0;

  /* u8, u16, u32, u64 */
  if (src_kind == TYPE_KIND_UINT) {
    if (dest_kind == TYPE_KIND_UINT)
      op = src_size < dest_size ? LLVMZExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_SINT)
      op = src_size < dest_size ? LLVMZExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_SIZE)
      op = src_size < dest_size ? LLVMZExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_REAL)
      op = LLVMUIToFP;
    else if (dest_kind == TYPE_KIND_PTR)
      op = LLVMIntToPtr;
    else if (dest_kind == TYPE_KIND_BOOL)
      op = LLVMTrunc; /* should be true or false ??? */
    else if (dest_kind == TYPE_KIND_STRING)
      op = LLVMIntToPtr;
    else if (dest_kind == TYPE_KIND_CHAR)
      op = src_size < dest_size ? LLVMZExt : LLVMTrunc;

    /* usize */
  } else if (src_kind == TYPE_KIND_SIZE) {
    if (dest_kind == TYPE_KIND_UINT)
      op = src_size < dest_size ? LLVMZExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_SINT)
      op = src_size < dest_size ? LLVMZExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_REAL)
      op = LLVMUIToFP;
    else if (dest_kind == TYPE_KIND_PTR)
      op = LLVMIntToPtr;

    /* s8, s16, s32, s64 */
  } else if (src_kind == TYPE_KIND_SINT) {
    if (dest_kind == TYPE_KIND_SINT)
      op = src_size < dest_size ? LLVMSExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_REAL)
      op = LLVMSIToFP;
    else if (dest_kind == TYPE_KIND_PTR)
      op = LLVMIntToPtr;
    else if (dest_kind == TYPE_KIND_UINT)
      op = src_size < dest_size ? LLVMSExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_SIZE)
      op = src_size < dest_size ? LLVMSExt : LLVMTrunc;
    else if (dest_kind == TYPE_KIND_BOOL)
      op = LLVMTrunc; /* should be true or false ??? */
    else if (dest_kind == TYPE_KIND_STRING)
      op = LLVMIntToPtr;
    else if (dest_kind == TYPE_KIND_CHAR)
      op = src_size < dest_size ? LLVMSExt : LLVMTrunc;

    /* f32, f64 */
  } else if (src_kind == TYPE_KIND_REAL) {
    if (dest_kind == TYPE_KIND_REAL)
      op = src_size < dest_size ? LLVMFPExt : LLVMFPTrunc;
    else if (dest_kind == TYPE_KIND_SINT)
      op = LLVMFPToSI;
    else if (dest_kind == TYPE_KIND_UINT)
      op = LLVMFPToUI;
    else if (dest_kind == TYPE_KIND_SIZE)
      op = LLVMFPToUI;
    else if (dest_kind == TYPE_KIND_BOOL)
      op = LLVMFPToUI; /* should be true or false ??? */
    else if (dest_kind == TYPE_KIND_CHAR)
      op = LLVMFPToUI;

    /* pointers */
  } else if (src_kind == TYPE_KIND_PTR) {
    if (dest_kind == TYPE_KIND_SINT)
      op = LLVMPtrToInt;
    else if (dest_kind == TYPE_KIND_UINT)
      op = LLVMPtrToInt;
    else if (dest_kind == TYPE_KIND_SIZE)
      op = LLVMPtrToInt;
    else if (dest_kind == TYPE_KIND_PTR)
      op = LLVMBitCast;
    else if (dest_kind == TYPE_KIND_STRING)
      op = LLVMBitCast;

    /* bool */
  } else if (src_kind == TYPE_KIND_BOOL) {
    if (dest_kind == TYPE_KIND_CHAR)
      op = LLVMZExt;
    else if (dest_kind == TYPE_KIND_SINT)
      op = LLVMZExt;
    else if (dest_kind == TYPE_KIND_UINT)
      op = LLVMZExt;
    else if (dest_kind == TYPE_KIND_SIZE)
      op = LLVMZExt;
    else if (dest_kind == TYPE_KIND_PTR)
      op = LLVMIntToPtr;
    else if (dest_kind == TYPE_KIND_STRING)
      op = LLVMIntToPtr;
    else if (dest_kind == TYPE_KIND_REAL)
      op = LLVMUIToFP;

    /* string */
  } else if (src_kind == TYPE_KIND_STRING) {
    if (dest_kind == TYPE_KIND_UINT)
      op = LLVMPtrToInt;
    else if (dest_kind == TYPE_KIND_SINT)
      op = LLVMPtrToInt;
    else if (dest_kind == TYPE_KIND_SIZE)
      op = LLVMPtrToInt;
    else if (dest_kind == TYPE_KIND_PTR)
      op = LLVMBitCast;
    else if (dest_kind == TYPE_KIND_BOOL)
      op = LLVMPtrToInt; /* should be true or false ??? */
    else if (dest_kind == TYPE_KIND_CHAR)
      op = LLVMPtrToInt;
  }

  if (op == 0) {
    char tmp_first[256];
    char tmp_second[256];
    ast_type_to_string(tmp_first, 256, ast_get_type(_cast->next));
    ast_type_to_string(tmp_second, 256, _cast->type);
    bl_abort("invalid cast from '%s' to '%s'", tmp_first, tmp_second);
  }

  // bl_log("cast %d to %d with size %d to %d", src_kind, dest_kind, src_size, dest_size);
  return LLVMBuildCast(cnt->llvm_builder, op, llvm_src, llvm_dest_type, gname("cast"));
}

LLVMValueRef
ir_ident(Context *cnt, Node *ident)
{
  LLVMValueRef result = NULL;
  NodeIdent *  _ident = peek_ident(ident);
  assert(_ident->ref);

  Node *ref = ast_unroll_ident(_ident->ref);
  assert(node_is(ref, NODE_DECL));
  NodeDecl *_ref = peek_decl(ref);

  /*switch (_ref->kind) {
  case DECL_KIND_FIELD:
    if (_ref->in_gscope)
      result = ir_global_get(cnt, _ident->ref);
    else
      result = llvm_values_get(cnt, _ident->ref);
    break;

  case DECL_KIND_FN:
    result = ir_fn_get(cnt, ast_unroll_ident(ident));
    break;

  case DECL_KIND_STRUCT:
    bl_log("here");
    break;

    case DECL_KIND_ARG:
  case DECL_KIND_MEMBER:
  case DECL_KIND_VARIANT:
    result = llvm_values_get(cnt, _ident->ref);
    break;

  case DECL_KIND_ENUM:
  case DECL_KIND_TYPE:
    bl_abort("unimplemented");
  case DECL_KIND_INVALID:
    bl_abort("unknown declaration kind");
  }*/

  assert(result);
  return result;
}

LLVMValueRef
ir_expr_binop(Context *cnt, Node *binop)
{

  NodeExprBinop *_binop = peek_expr_binop(binop);
  LLVMValueRef   lhs    = ir_node(cnt, _binop->lhs);
  LLVMValueRef   rhs    = ir_node(cnt, _binop->rhs);

  if (should_load(_binop->rhs, rhs)) rhs = LLVMBuildLoad(cnt->llvm_builder, rhs, gname("tmp"));

  LLVMTypeKind lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(lhs));
  bool         float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  /* Assignments */
  switch (_binop->op) {
  case SYM_ASSIGN:
    LLVMBuildStore(cnt->llvm_builder, rhs, lhs);
    return lhs;

  case SYM_PLUS_ASSIGN: {
    LLVMValueRef value = lhs;
    if (should_load(_binop->lhs, lhs)) value = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
    if (float_kind)
      value = LLVMBuildFAdd(cnt->llvm_builder, value, rhs, gname("tmp"));
    else
      value = LLVMBuildAdd(cnt->llvm_builder, value, rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, lhs);
  }

  case SYM_MINUS_ASSIGN: {
    LLVMValueRef value = lhs;
    if (should_load(_binop->lhs, lhs)) value = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
    if (float_kind)
      value = LLVMBuildFSub(cnt->llvm_builder, value, rhs, gname("tmp"));
    else
      value = LLVMBuildSub(cnt->llvm_builder, value, rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, lhs);
  }

  case SYM_MUL_ASSIGN: {
    LLVMValueRef value = lhs;
    if (should_load(_binop->lhs, lhs)) value = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
    if (float_kind)
      value = LLVMBuildFMul(cnt->llvm_builder, value, rhs, gname("tmp"));
    else
      value = LLVMBuildMul(cnt->llvm_builder, value, rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, lhs);
  }

  case SYM_DIV_ASSIGN: {
    LLVMValueRef value = lhs;
    if (should_load(_binop->lhs, lhs)) value = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
    if (float_kind)
      value = LLVMBuildFDiv(cnt->llvm_builder, value, rhs, gname("tmp"));
    else
      value = LLVMBuildSDiv(cnt->llvm_builder, value, rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, lhs);
  }

  case SYM_MOD_ASSIGN: {
    LLVMValueRef value = lhs;
    if (should_load(_binop->lhs, lhs)) value = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
    value = LLVMBuildSRem(cnt->llvm_builder, value, rhs, gname("tmp"));
    return LLVMBuildStore(cnt->llvm_builder, value, lhs);
  }

  default:
    break;
  }

  if (should_load(_binop->lhs, lhs)) lhs = LLVMBuildLoad(cnt->llvm_builder, lhs, gname("tmp"));
  lhs_kind   = LLVMGetTypeKind(LLVMTypeOf(lhs));
  float_kind = lhs_kind == LLVMFloatTypeKind || lhs_kind == LLVMDoubleTypeKind;

  switch (_binop->op) {
  case SYM_PLUS:
    if (float_kind) return LLVMBuildFAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildAdd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case SYM_MINUS:
    if (float_kind) return LLVMBuildFSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildSub(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case SYM_ASTERISK:
    if (float_kind) return LLVMBuildFMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildMul(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case SYM_SLASH:
    if (float_kind) return LLVMBuildFDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));
    return LLVMBuildSDiv(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case SYM_MODULO:
    return LLVMBuildSRem(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case SYM_EQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOEQ, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntEQ, lhs, rhs, gname("tmp"));

  case SYM_NEQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealONE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntNE, lhs, rhs, gname("tmp"));

  case SYM_GREATER:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGT, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGT, lhs, rhs, gname("tmp"));

  case SYM_LESS:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLT, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLT, lhs, rhs, gname("tmp"));

  case SYM_GREATER_EQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOGE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSGE, lhs, rhs, gname("tmp"));

  case SYM_LESS_EQ:
    if (float_kind) return LLVMBuildFCmp(cnt->llvm_builder, LLVMRealOLE, lhs, rhs, gname("tmp"));
    return LLVMBuildICmp(cnt->llvm_builder, LLVMIntSLE, lhs, rhs, gname("tmp"));

  case SYM_LOGIC_AND:
    return LLVMBuildAnd(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  case SYM_LOGIC_OR:
    return LLVMBuildOr(cnt->llvm_builder, lhs, rhs, gname("tmp"));

  default:
    bl_abort("unknown binop");
  }
}

LLVMValueRef
ir_expr_unary(Context *cnt, Node *unary)
{
  NodeExprUnary *_unary = peek_expr_unary(unary);
  assert(_unary->next);
  LLVMValueRef next_val  = ir_node(cnt, _unary->next);
  LLVMTypeRef  next_type = LLVMTypeOf(next_val);

  switch (_unary->op) {
  case SYM_MINUS:
  case SYM_PLUS: {
    if (should_load(_unary->next, next_val)) {
      next_val  = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
      next_type = LLVMTypeOf(next_val);
    }

    /* TODO use BL_KIND */
    LLVMTypeKind next_type_kind = LLVMGetTypeKind(next_type);

    int mult = 1;
    switch (_unary->op) {
    case SYM_MINUS:
      mult = -1;
      break;
    case SYM_PLUS:
      mult = 1;
      break;
    default:
      bl_abort("invalid unary operation %s", sym_strings[_unary->op]);
    }

    if (next_type_kind == LLVMFloatTypeKind || next_type_kind == LLVMDoubleTypeKind) {
      LLVMValueRef cnst = LLVMConstReal(next_type, (double)mult);
      return LLVMBuildFMul(cnt->llvm_builder, cnst, next_val, "");
    }

    LLVMValueRef cnst = LLVMConstInt(next_type, (unsigned long long int)mult, false);
    return LLVMBuildMul(cnt->llvm_builder, cnst, next_val, "");
  }

  case SYM_NOT: {
    if (should_load(_unary->next, next_val)) {
      next_val = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
    }

    next_val = LLVMBuildNot(cnt->llvm_builder, next_val, gname("tmp"));
    return next_val;
  }

  case SYM_AND: {
    /* unary operation is getting address of something "&foo" */
    LLVMValueRef indices[1];
    indices[0] = LLVMConstInt(LLVMInt32TypeInContext(cnt->llvm_cnt), 0, false);
    return LLVMBuildGEP(cnt->llvm_builder, next_val, indices, ARRAY_SIZE(indices), gname("tmp"));
  }

  case SYM_ASTERISK: {
    next_val = LLVMBuildLoad(cnt->llvm_builder, next_val, gname("tmp"));
    return next_val;
  }

  default:
    bl_abort("invalid unary operation %s", sym_strings[_unary->op]);
  }
}

LLVMValueRef
ir_expr_sizeof(Context *cnt, Node *szof)
{
  assert(szof);
  return LLVMSizeOf(to_llvm_type(cnt, peek_expr_sizeof(szof)->in));
}

LLVMValueRef
ir_expr_null(Context *cnt, Node *nl)
{
  NodeExprNull *_null = peek_expr_null(nl);
  assert(_null->type);
  LLVMTypeRef type = to_llvm_type(cnt, _null->type);
  return LLVMConstPointerNull(type);
}

LLVMValueRef
ir_block(Context *cnt, Node *block)
{
  bool prev_is_gscope = cnt->is_gscope;
  cnt->is_gscope      = false;

  NodeBlock *_block = peek_block(block);
  Node *     tmp;

  node_foreach(_block->nodes, tmp)
  {
    ir_node(cnt, tmp);
  }

  cnt->is_gscope = prev_is_gscope;
  return NULL;
}

LLVMValueRef
ir_decl_mut(Context *cnt, Node *decl)
{
  LLVMValueRef result    = NULL;
  NodeDecl *   _decl     = peek_decl(decl);
  LLVMTypeRef  llvm_type = to_llvm_type(cnt, _decl->type);
  assert(llvm_type);

  if (cnt->is_gscope) {
    result = ir_global_get(cnt, decl);
    assert(result);
    LLVMValueRef init = ir_node(cnt, _decl->value);
    assert(init);
    LLVMSetInitializer(result, init);
  } else {
    LLVMBasicBlockRef prev_block = LLVMGetInsertBlock(cnt->llvm_builder);
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    result = LLVMBuildAlloca(cnt->llvm_builder, llvm_type, gname(peek_ident(_decl->name)->str));
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, prev_block);
    llvm_values_insert(cnt, decl, result);

    if (!_decl->value) return result;
    LLVMValueRef init = ir_node(cnt, _decl->value);
    if (should_load(_decl->value, init))
      init = LLVMBuildLoad(cnt->llvm_builder, init, gname("tmp"));

    LLVMBuildStore(cnt->llvm_builder, init, result);
  }
  return result;
}

#if 0
LLVMValueRef
ir_decl_immut(Context *cnt, Node *decl)
{
  NodeDecl *_decl = peek_decl(decl);
  assert(_decl->value);
  LLVMValueRef result = ir_node(cnt, _decl->value);
  llvm_values_insert(cnt, decl, result);
  return result;
}
#endif

LLVMValueRef
ir_fn_get(Context *cnt, Node *fn)
{
  NodeDecl *  _fn     = peek_decl(fn);
  const char *fn_name = peek_ident(_fn->name)->str;

  LLVMValueRef result = LLVMGetNamedFunction(cnt->llvm_module, fn_name);
  if (!result) {

    LLVMTypeRef llvm_type = to_llvm_type(cnt, _fn->type);
    result                = LLVMAddFunction(cnt->llvm_module, fn_name, llvm_type);
  }

  return result;
}

LLVMValueRef
ir_global_get(Context *cnt, Node *global)
{
  NodeDecl *  _global = peek_decl(global);
  const char *g_name  = peek_ident(_global->name)->str;

  LLVMValueRef result = LLVMGetNamedGlobal(cnt->llvm_module, g_name);
  if (!result) {
    LLVMTypeRef llvm_type = to_llvm_type(cnt, _global->type);
    result                = LLVMAddGlobal(cnt->llvm_module, llvm_type, g_name);
  }

  return result;
}

LLVMValueRef
ir_decl_fn(Context *cnt, Node *decl)
{
  /* local functions will be generated in separate module */
  if (!cnt->is_gscope) return NULL;

  NodeDecl *_decl = peek_decl(decl);

  LLVMValueRef result = ir_fn_get(cnt, decl);
  NodeLitFn *  _fn    = peek_lit_fn(_decl->value);

  {
    assert(_decl->value);
    cnt->fn_init_block  = LLVMAppendBasicBlock(result, gname("init"));
    cnt->fn_entry_block = LLVMAppendBasicBlock(result, gname("entry"));
    cnt->fn_ret_block   = LLVMAppendBasicBlock(result, gname("exit"));

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);

    NodeTypeFn *_fn_type = peek_type_fn(_fn->type);
    /*
     * Create named references to function parameters so they
     * can be called by name in function body.
     */

    Node *arg;
    int   i = 0;
    node_foreach(_fn_type->arg_types, arg)
    {
      const char * name  = peek_ident(peek_decl(arg)->name)->str;
      LLVMValueRef p     = LLVMGetParam(result, (unsigned int)i++);
      LLVMValueRef p_tmp = LLVMBuildAlloca(cnt->llvm_builder, LLVMTypeOf(p), gname(name));
      LLVMBuildStore(cnt->llvm_builder, p, p_tmp);

      llvm_values_insert(cnt, arg, p_tmp);
    }

    /*
     * Prepare return value.
     */
    LLVMTypeRef llvm_ret_type = to_llvm_type(cnt, ast_get_type(_fn_type->ret_type));
    if (llvm_ret_type != LLVMVoidTypeInContext(cnt->llvm_cnt)) {
      cnt->fn_ret_val = LLVMBuildAlloca(cnt->llvm_builder, llvm_ret_type, gname("ret"));
    } else {
      cnt->fn_ret_val = NULL;
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_entry_block);
  }

  /* generate function body */
  ir_node(cnt, _fn->block);

  {
    LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_init_block);
    LLVMBuildBr(cnt->llvm_builder, cnt->fn_entry_block);

    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMPositionBuilderAtEnd(cnt->llvm_builder, curr_block);
      LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
    }

    LLVMPositionBuilderAtEnd(cnt->llvm_builder, cnt->fn_ret_block);
    if (cnt->fn_ret_val) {
      cnt->fn_ret_val = LLVMBuildLoad(cnt->llvm_builder, cnt->fn_ret_val, gname("tmp"));
      LLVMBuildRet(cnt->llvm_builder, cnt->fn_ret_val);
    } else {
      LLVMBuildRetVoid(cnt->llvm_builder);
    }
  }

  /* reset tmps */
  cnt->fn_entry_block = NULL;
  cnt->fn_init_block  = NULL;
  cnt->fn_ret_block   = NULL;
  cnt->fn_ret_val     = NULL;
  cnt->break_block    = NULL;
  cnt->continue_block = NULL;

  return result;
}

LLVMValueRef
ir_decl(Context *cnt, Node *decl)
{
  if (is_terminated(cnt)) return NULL;
  NodeDecl *_decl = peek_decl(decl);
  assert(_decl->type);
  assert(_decl->name);

  /*
  switch (_decl->kind) {
  case DECL_KIND_FIELD:
    return ir_decl_mut(cnt, decl);
  case DECL_KIND_FN:
    return ir_decl_fn(cnt, decl);
    /*
  case DECL_KIND_MEMBER:
    return ir_decl_mut(cnt, decl);
  case DECL_KIND_ARG:
    return ir_decl_mut(cnt, decl);
  case DECL_KIND_VARIANT:
    // return ir_decl_immut(cnt, decl);
  case DECL_KIND_STRUCT:
  case DECL_KIND_ENUM:
  case DECL_KIND_TYPE:
    break;
  default:
    bl_abort("unknown declaration kind");
  }
*/

  return NULL;
}

LLVMValueRef
ir_stmt_if(Context *cnt, Node *stmt_if)
{
  NodeStmtIf *      _stmt_if     = peek_stmt_if(stmt_if);
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef      parent       = LLVMGetBasicBlockParent(insert_block);
  assert(LLVMIsAFunction(parent));

  LLVMBasicBlockRef if_then = LLVMAppendBasicBlock(parent, gname("if_then"));
  LLVMBasicBlockRef if_else = LLVMAppendBasicBlock(parent, gname("if_else"));
  LLVMBasicBlockRef if_cont = LLVMAppendBasicBlock(parent, gname("if_cont"));
  LLVMValueRef      expr    = ir_node(cnt, _stmt_if->test);

  if (should_load(_stmt_if->test, expr))
    expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));

  expr =
      LLVMBuildIntCast(cnt->llvm_builder, expr, LLVMInt1TypeInContext(cnt->llvm_cnt), gname("tmp"));

  /*
   * If condition break generation.
   */
  LLVMBuildCondBr(cnt->llvm_builder, expr, if_then, if_else);

  if (_stmt_if->false_stmt == NULL) {
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* then block */
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_then);
  ir_node(cnt, _stmt_if->true_stmt);

  LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
  if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
    LLVMBuildBr(cnt->llvm_builder, if_cont);
  }

  /* else if */
  if (_stmt_if->false_stmt != NULL) {
    /* else */
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_else);
    ir_node(cnt, _stmt_if->false_stmt);

    curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
    if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
      LLVMBuildBr(cnt->llvm_builder, if_cont);
    }
  }

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, if_cont);
  return NULL;
}

LLVMValueRef
ir_stmt_return(Context *cnt, Node *stmt_return)
{
  NodeStmtReturn *_ret = peek_stmt_return(stmt_return);
  if (!_ret->expr) {
    LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
    return NULL;
  }

  LLVMValueRef val = ir_node(cnt, _ret->expr);

  if (should_load(_ret->expr, val)) val = LLVMBuildLoad(cnt->llvm_builder, val, gname("tmp"));

  assert(cnt->fn_ret_val);
  assert(cnt->fn_ret_block);
  LLVMBuildStore(cnt->llvm_builder, val, cnt->fn_ret_val);
  LLVMBuildBr(cnt->llvm_builder, cnt->fn_ret_block);
  return NULL;
}

LLVMValueRef
ir_stmt_loop(Context *cnt, Node *loop)
{
  NodeStmtLoop *    _loop        = peek_stmt_loop(loop);
  LLVMBasicBlockRef insert_block = LLVMGetInsertBlock(cnt->llvm_builder);
  LLVMValueRef      parent       = LLVMGetBasicBlockParent(insert_block);
  assert(LLVMIsAFunction(parent));

  LLVMBasicBlockRef loop_increment =
      _loop->increment ? LLVMAppendBasicBlock(parent, gname("loop_increment")) : NULL;

  LLVMBasicBlockRef loop_decide         = LLVMAppendBasicBlock(parent, gname("loop_decide"));
  LLVMBasicBlockRef loop_block          = LLVMAppendBasicBlock(parent, gname("loop_block"));
  LLVMBasicBlockRef loop_cont           = LLVMAppendBasicBlock(parent, gname("loop_cont"));
  LLVMValueRef      expr                = NULL;
  LLVMBasicBlockRef prev_break_block    = cnt->break_block;
  LLVMBasicBlockRef prev_continue_block = cnt->continue_block;
  cnt->break_block                      = loop_cont;
  cnt->continue_block                   = loop_increment ? loop_increment : loop_decide;

  if (_loop->init) {
    /* generate ir fo init block */
    ir_node(cnt, _loop->init);
  }

  LLVMBuildBr(cnt->llvm_builder, loop_decide);
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_decide);

  if (_loop->condition) {
    expr = ir_node(cnt, _loop->condition);
    if (should_load(_loop->condition, expr))
      expr = LLVMBuildLoad(cnt->llvm_builder, expr, gname("tmp"));
  } else {
    expr = LLVMConstInt(LLVMInt1TypeInContext(cnt->llvm_cnt), true, false);
  }

  LLVMBuildCondBr(cnt->llvm_builder, expr, loop_block, loop_cont);

  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_block);
  ir_node(cnt, _loop->block);

  LLVMBasicBlockRef curr_block = LLVMGetInsertBlock(cnt->llvm_builder);
  if (LLVMGetBasicBlockTerminator(curr_block) == NULL) {
    LLVMBuildBr(cnt->llvm_builder, _loop->increment ? loop_increment : loop_decide);
  }

  if (_loop->increment) {
    LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_increment);
    ir_node(cnt, _loop->increment);
    LLVMBuildBr(cnt->llvm_builder, loop_decide);
  }

  cnt->break_block    = prev_break_block;
  cnt->continue_block = prev_continue_block;
  LLVMPositionBuilderAtEnd(cnt->llvm_builder, loop_cont);
  return NULL;
}

LLVMValueRef
ir_stmt_break(Context *cnt, Node *brk)
{
  LLVMBuildBr(cnt->llvm_builder, cnt->break_block);
  return NULL;
}

LLVMValueRef
ir_stmt_continue(Context *cnt, Node *cont)
{
  LLVMBuildBr(cnt->llvm_builder, cnt->continue_block);
  return NULL;
}

LLVMValueRef
ir_node(Context *cnt, Node *node)
{
  if (is_terminated(cnt)) return NULL;
  if (!node) return NULL;
  switch (node_code(node)) {
  case NODE_BLOCK:
    return ir_block(cnt, node);
    break;

  case NODE_DECL:
    return ir_decl(cnt, node);

  case NODE_EXPR_NULL:
    return ir_expr_null(cnt, node);

  case NODE_EXPR_BINOP:
    return ir_expr_binop(cnt, node);

  case NODE_EXPR_CALL:
    return ir_expr_call(cnt, node);

  case NODE_EXPR_CAST:
    return ir_expr_cast(cnt, node);

  case NODE_EXPR_UNARY:
    return ir_expr_unary(cnt, node);

  case NODE_EXPR_MEMBER:
    return ir_expr_member(cnt, node);

  case NODE_EXPR_ELEM:
    return ir_expr_elem(cnt, node);

  case NODE_STMT_RETURN:
    return ir_stmt_return(cnt, node);

  case NODE_STMT_IF:
    return ir_stmt_if(cnt, node);

  case NODE_STMT_LOOP:
    return ir_stmt_loop(cnt, node);

  case NODE_STMT_BREAK:
    return ir_stmt_break(cnt, node);

  case NODE_STMT_CONTINUE:
    return ir_stmt_continue(cnt, node);

  case NODE_LIT:
    return ir_lit(cnt, node);

  case NODE_EXPR_SIZEOF:
    return ir_expr_sizeof(cnt, node);

  case NODE_IDENT:
    return ir_ident(cnt, node);

  case NODE_LIT_CMP:
    return ir_lit_cmp(cnt, node);

  case NODE_TYPE_FUND:
  case NODE_TYPE_FN:
  case NODE_TYPE_STRUCT:
  case NODE_TYPE_ENUM:
  case NODE_LIT_FN:
  case NODE_LOAD:
  case NODE_LINK:
  case NODE_BAD:
  case NODE_UBLOCK:
  case NODE_COUNT:
    break;
  default:
    bl_abort("cannot generate IR for %s", node_name(node));
  }
  return NULL;
}

void
create_jit(Context *cnt)
{
  LLVMModuleRef module     = LLVMModuleCreateWithNameInContext("compile_time", cnt->llvm_cnt);
  char *        llvm_error = NULL;

  if (LLVMCreateJITCompilerForModule(&cnt->llvm_jit, module, JIT_OPT_LEVEL, &llvm_error) != 0)
    bl_abort("failed to create execution engine for compile-time module with error %s", llvm_error);
}

RunResult
run(Context *cnt, Node *fn)
{
  Node *decl = ast_unroll_ident(fn);
  link_into_jit(cnt, decl);
  LLVMValueRef        llvm_fn;
  LLVMGenericValueRef generic;

  NodeDecl *  _decl     = peek_decl(decl);
  const char *decl_name = peek_ident(_decl->name)->str;
  assert(decl_name);

  if (!LLVMFindFunction(cnt->llvm_jit, decl_name, &llvm_fn)) {
    generic = LLVMRunFunction(cnt->llvm_jit, llvm_fn, 0, NULL);
  } else {
    bl_abort("unknown function %s", decl_name);
  }

  if (!generic) bl_abort("invalid result of compile time executed method");
  RunResult result;

  Node *   ret_type = peek_type_fn(_decl->type)->ret_type;
  TypeKind kind     = ast_type_kind(ast_unroll_ident(ret_type));
  switch (kind) {
  case TYPE_KIND_SINT: {
    result.type = RR_S64;
    result.s64  = (int64_t)LLVMGenericValueToInt(generic, true);
    break;
  }

  case TYPE_KIND_UINT:
  case TYPE_KIND_SIZE: {
    result.type = RR_U64;
    result.u64  = LLVMGenericValueToInt(generic, false);
    break;
  }

  case TYPE_KIND_REAL: {
    LLVMTypeRef llvm_type = to_llvm_type(cnt, ret_type);
    result.type           = RR_REAL;
    result.real           = LLVMGenericValueToFloat(llvm_type, generic);
    break;
  }

  case TYPE_KIND_VOID: {
    result.type = RR_VOID;
    result.u64  = 0;
    break;
  }
  default:
    bl_abort("unsupported return type of compile time executed function %s", decl_name);
  }

  LLVMDisposeGenericValue(generic);
  return result;
}

void
generate(Context *cnt)
{
  BList *   queue = cnt->assembly->ir_queue;
  Node *    decl;
  NodeDecl *_decl;

  while (!bo_list_empty(queue)) {
    decl = bo_list_front(queue, Node *);
    bo_list_pop_front(queue);

    _decl = peek_decl(decl);
    if (_decl->flags & FLAG_EXTERN) continue;

    if (is_satisfied(cnt, decl, true)) {
      generate_decl(cnt, decl);

      if (_decl->flags & FLAG_MAIN) {
        assert(!cnt->main_tmp);
        cnt->main_tmp = decl;
      }
    } else {
      /* declaration is waiting for it's dependencies and need to be processed later */
      bo_list_push_back(queue, decl);
#if VERBOSE
      // bl_log(RED("defered: '%s'"), peek_ident(peek_decl(decl)->name)->str);
#endif
    }
  }
}

void
generate_decl(Context *cnt, Node *decl)
{
  assert(decl);
  NodeDecl *_decl = peek_decl(decl);
#if VERBOSE
  bl_log(GREEN("generate: '%s'"), peek_ident(_decl->name)->str);
#endif
  cnt->llvm_module = LLVMModuleCreateWithNameInContext(peek_ident(_decl->name)->str, cnt->llvm_cnt);
  ir_node(cnt, decl);
  bo_htbl_insert(cnt->llvm_modules, (uint64_t)decl, cnt->llvm_module);
  llvm_values_reset(cnt);
  llvm_strings_reset(cnt);
}

LLVMModuleRef
link(Context *cnt, Node *entry)
{
  if (!entry) return NULL;
  LLVMModuleRef dest_module = _link(cnt, entry);
  print_llvm_module(dest_module);
  return dest_module;
}

LLVMModuleRef
_link(Context *cnt, Node *entry)
{
  NodeDecl *_entry = peek_decl(entry);

  if (!bo_htbl_has_key(cnt->llvm_modules, (uint64_t)entry)) return NULL;

  LLVMModuleRef dest_module = bo_htbl_at(cnt->llvm_modules, (uint64_t)entry, LLVMModuleRef);
  assert(dest_module);
  if (!_entry->deps) return dest_module;

  Dependency    dep;
  bo_iterator_t it;
  bhtbl_foreach(_entry->deps, it)
  {
    dep = bo_htbl_iter_peek_value(_entry->deps, &it, Dependency);

    /* link all lax dependencies */
    if (dep.type & DEP_LAX) {
      /* must be linked */
      LLVMModuleRef src_module = _link(cnt, dep.node);

      if (src_module) {
        if (LLVMLinkModules2(dest_module, src_module)) bl_abort("unable to link modules");

        bo_htbl_erase_key(cnt->llvm_modules, (uint64_t)dep.node);
      }
    }
  }

  return dest_module;
}

static void
link_into_jit(Context *cnt, Node *fn)
{
  assert(fn);
  if (bo_htbl_has_key(cnt->jit_linked, (uint64_t)fn)) return;

  NodeDecl *_fn = peek_decl(fn);
  if (!bo_htbl_has_key(cnt->llvm_modules, (uint64_t)fn))
    bl_abort("Missing llvm module for '%s', this is usualy caused by #run directive.",
             peek_ident(_fn->name)->str);

  LLVMModuleRef module = bo_htbl_at(cnt->llvm_modules, (uint64_t)fn, LLVMModuleRef);
  module               = LLVMCloneModule(module);
  print_llvm_module(module);
  ir_validate(module);
  LLVMAddModule(cnt->llvm_jit, module);
  bo_htbl_insert_empty(cnt->jit_linked, (uint64_t)fn);

  if (!_fn->deps) return;

  bo_iterator_t iter;
  Dependency    dep;
  bhtbl_foreach(_fn->deps, iter)
  {
    dep = bo_htbl_iter_peek_value(_fn->deps, &iter, Dependency);

    if (dep.type & DEP_LAX) {
      link_into_jit(cnt, dep.node);
    }
  }
}

static inline bool
generated(Context *cnt, Node *decl)
{
  return bo_htbl_has_key(cnt->llvm_modules, (uint64_t)decl);
}

bool
is_satisfied(Context *cnt, Node *decl, bool strict_only)
{
#if VERBOSE
  if (!strict_only) bl_log(YELLOW("checking non-strict dependencies"));
#endif
  assert(decl);
  BHashTable *deps = peek_decl(decl)->deps;
  if (!deps) return true;

  bo_iterator_t iter;
  Dependency    dep;
  bhtbl_foreach(deps, iter)
  {
    dep = bo_htbl_iter_peek_value(deps, &iter, Dependency);

    // PERFORMANCE: is there some better solution than check whole tree???
    bool check_tree = (bool)(strict_only ? dep.type & DEP_STRICT : true);
    if (check_tree) {
      if (!generated(cnt, dep.node)) {
        return false;
      } else if (!is_satisfied(cnt, dep.node, false)) {
        return false;
      }
    }
  }

  return true;
}

void
ir_run(Builder *builder, Assembly *assembly)
{
  assert(!bo_list_empty(assembly->ir_queue) && "nothig to generate");
  Context cnt;
  memset(&cnt, 0, sizeof(cnt));
  cnt.builder      = builder;
  cnt.assembly     = assembly;
  cnt.is_gscope    = true;
  cnt.jit_linked   = bo_htbl_new(0, bo_list_size(assembly->ir_queue));
  cnt.llvm_cnt     = LLVMContextCreate();
  cnt.llvm_builder = LLVMCreateBuilderInContext(cnt.llvm_cnt);
  cnt.llvm_modules = bo_htbl_new(sizeof(LLVMModuleRef), bo_list_size(assembly->ir_queue));
  cnt.llvm_values  = bo_htbl_new(sizeof(LLVMValueRef), 256);
  cnt.llvm_strings = bo_htbl_new(sizeof(LLVMValueRef), 128);

  create_jit(&cnt);
  generate(&cnt);

  /* link all test cases */
  if (builder->flags & BUILDER_RUN_TESTS) {
    TestCase     tc;
    const size_t c = bo_array_size(assembly->test_cases);
    for (size_t i = 0; i < c; ++i) {
      tc = bo_array_at(assembly->test_cases, i, TestCase);
      link_into_jit(&cnt, tc.fn);
    }
  }

  /* link runtime */
  if (cnt.main_tmp) {
    assert(cnt.main_tmp);
    assembly->llvm_module = link(&cnt, cnt.main_tmp);
    bo_htbl_erase_key(cnt.llvm_modules, (uint64_t)cnt.main_tmp);
  }

  if (assembly->llvm_module) ir_validate(assembly->llvm_module);

  assembly->llvm_cnt = cnt.llvm_cnt;
  assembly->llvm_jit = cnt.llvm_jit;

  bo_unref(cnt.llvm_modules);
  bo_unref(cnt.llvm_values);
  bo_unref(cnt.jit_linked);
  bo_unref(cnt.llvm_strings);

  LLVMDisposeBuilder(cnt.llvm_builder);
}
