//*****************************************************************************
// bl 
//
// File:   analyzer.c
// Author: Martin Dorazil
// Date:   09/02/2018
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
//*****************************************************************************

#include <setjmp.h>
#include "bl/analyzer.h"
#include "bl/bldebug.h"
#include "bl/bllimits.h"
#include "ast/ast_impl.h"
#include "unit_impl.h"

#define analyze_error(cnt, format, ...) \
  { \
    bl_actor_error((Actor *)(cnt)->unit, ("(analyzer) "format), ##__VA_ARGS__); \
    longjmp((cnt)->jmp_error, 1); \
  }

typedef struct _context_t
{
  Unit    *unit;
  jmp_buf  jmp_error;

  /* tmps */
  NodeFuncDecl *current_func_tmp;
} context_t;

static bool
run(Analyzer *self,
    Unit     *unit);

static void
analyze_func(context_t    *cnt,
             NodeFuncDecl *func);

static void
analyze_var_decl(context_t   *cnt,
                 NodeVarDecl *vdcl);

static void
analyze_stmt(context_t *cnt,
             NodeStmt  *stmt);

static void
analyze_gstmt(context_t      *cnt,
              NodeGlobalStmt *node);

/* Analyzer members */
bo_decl_members_begin(Analyzer, Stage)
bo_end();

/* Analyzer constructor parameters */
bo_decl_params_with_base_begin(Analyzer, Stage)
bo_end();

bo_impl_type(Analyzer, Stage);

/* Analyzer class init */
void
AnalyzerKlass_init(AnalyzerKlass *klass)
{
  bo_vtbl_cl(klass, Stage)->run 
    = (bool (*)(Stage*, Actor *)) run;
}

/* Analyzer constructor */
void
Analyzer_ctor(Analyzer *self, AnalyzerParams *p)
{
  bo_parent_ctor(Stage, p);
}

/* Analyzer destructor */
void
Analyzer_dtor(Analyzer *self)
{
}

/* Analyzer copy constructor */
bo_copy_result
Analyzer_copy(Analyzer *self, Analyzer *other)
{
  return BO_NO_COPY;
}

void
analyze_func(context_t    *cnt,
             NodeFuncDecl *func)
{
  cnt->current_func_tmp = func;
  /*
   * Check return type existence.
   */
  Type *type_tmp = bl_node_func_decl_get_type(func);
  if (bl_type_is_user_defined(type_tmp)) {
    analyze_error(cnt, "%s %d:%d unknown return type '%s' for function '%s'",
                  cnt->unit->filepath,
                  bo_members(func, Node)->line,
                  bo_members(func, Node)->col,
                  bl_type_get_name(type_tmp), bl_node_func_decl_get_ident(func));
  }

  /*
   * Check params.
   */
  const int c = bl_node_func_decl_get_param_count(func);

  /*
   * Reached maximum count of parameters.
   */
  if (c > BL_MAX_FUNC_PARAM_COUNT)
    analyze_error(cnt,
                  "%s %d:%d reached maximum count of function parameters, function has: %d and maximum is: %d",
                  cnt->unit->filepath,
                  bo_members(func, Node)->line,
                  bo_members(func, Node)->col,
                  c,
                  BL_MAX_FUNC_PARAM_COUNT);


  NodeParamVarDecl *param = NULL;
  for (int i = 0; i < c; i++) {
    param = bl_node_func_decl_get_param(func, i);

    type_tmp = bl_node_param_var_decl_get_type(param);
    if (bl_type_is_user_defined(type_tmp)) {
      analyze_error(cnt, "%s %d:%d unknown type '%s' for function parameter",
                    cnt->unit->filepath,
                    bo_members(param, Node)->line,
                    bo_members(param, Node)->col, bl_type_get_name(type_tmp));
    }
  }

  /*
   * Validate scope statement if there is one.
   */
  NodeStmt *stmt = bl_node_func_decl_get_stmt(func);
  if (stmt) {
    analyze_stmt(cnt, stmt);
  }
}

void
analyze_var_decl(context_t *cnt,
                 NodeVarDecl *vdcl)
{
  /*
   * Check type of declaration.
   */
  Type *type_tmp = bl_node_var_decl_get_type(vdcl);
  if (bl_type_is_user_defined(type_tmp)) {
    analyze_error(cnt, "%s %d:%d unknown type '%s'",
                  cnt->unit->filepath,
                  bo_members(vdcl, Node)->line,
                  bo_members(vdcl, Node)->col, bl_type_get_name(type_tmp));
  } else if (bl_type_is(type_tmp, BL_TYPE_VOID)) {
    analyze_error(cnt, "%s %d:%d 'void' is not allowed here",
                  cnt->unit->filepath,
                  bo_members(vdcl, Node)->line,
                  bo_members(vdcl, Node)->col);
  }
}

static void
analyze_stmt(context_t *cnt,
             NodeStmt  *stmt)
{
  bool return_presented = false;
  const int c = bl_node_stmt_child_get_count(stmt);
  Node *node = NULL;
  for (int i = 0; i < c; i++) {
    node = bl_node_stmt_get_child(stmt, i);
    switch (node->type) {
      case BL_NODE_RETURN_STMT:
        return_presented = true;
        if (i != c-1) {
          bl_warning("(analyzer) %s %d:%d unrecheable code after 'return' statement",
                     cnt->unit->filepath,
                     node->line,
                     node->col);
        }
        break;
      case BL_NODE_VAR_DECL:
        analyze_var_decl(cnt, (NodeVarDecl *) node);
        break;
      default:
        break;
    }
  }

  Type *exp_ret = bl_node_func_decl_get_type(cnt->current_func_tmp);
  if (!return_presented && bl_type_is_not(exp_ret, BL_TYPE_VOID)) {
    analyze_error(cnt, "%s %d:%d unterminated function '%s'",
                  cnt->unit->filepath,
                  bo_members(cnt->current_func_tmp, Node)->line,
                  bo_members(cnt->current_func_tmp, Node)->col,
                  bl_node_func_decl_get_ident(cnt->current_func_tmp));
  }
}

void
analyze_gstmt(context_t      *cnt,
              NodeGlobalStmt *node)
{
  Node *child = NULL;
  const int c = bl_node_global_stmt_get_child_count(node);
  for (int i = 0; i < c; i++) {
    child = bl_node_global_stmt_get_child(node, i);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        analyze_func(cnt, (NodeFuncDecl *)child);
        break;
      default:
        break;
    }
  }
}

bool
run(Analyzer *self,
    Unit     *unit)
{
  context_t cnt = {0};
  cnt.unit = unit;

  if (setjmp(cnt.jmp_error))
    return false;

  Node *root = bl_ast_get_root(unit->ast);

  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      analyze_gstmt(&cnt, (NodeGlobalStmt *)root);
      break;
    default:
      break;
  }
  return true;
}

/* public */
Analyzer *
bl_analyzer_new(bl_compile_group_e group)
{
  AnalyzerParams p = {
    .base.group = group
  };
  
  return bo_new(Analyzer, &p);
}

