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
#include "bl/type_table.h"
#include "unit_impl.h"
#include "ast/ast_impl.h"

#define analyze_error(format, ...) \
  { \
    bl_actor_error((Actor *)unit, ("(analyzer) "format), ##__VA_ARGS__); \
    longjmp(jmp_error, 1); \
  } 

static bool
run(Analyzer *self,
    Unit     *unit);

static void
analyze_func(Unit         *unit,
             NodeFuncDecl *node,
             jmp_buf       jmp_error);

static void
analyze_gstmt(Unit           *unit,
              NodeGlobalStmt *node,
              jmp_buf         jmp_error);

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
analyze_func(Unit         *unit,
             NodeFuncDecl *node,
             jmp_buf       jmp_error)
{
  /*
   * Check return type existence.
   */
  if (bl_strtotype(bl_node_func_decl_type(node)) == BL_TYPE_REF) {
    analyze_error("%s %d:%d unknown return type '%s' for function '%s'",
                  unit->filepath,
                  bo_members(node, Node)->line,
                  bo_members(node, Node)->col,
                  bl_node_func_decl_type(node),
                  bl_node_func_decl_ident(node));
  }

  /*
   * Check params.
   */
  const int c = bl_node_func_decl_param_count(node);
  NodeParamVarDecl *param = NULL;
  for (int i = 0; i < c; i++) {
    param = bl_node_func_decl_param(node, i);

    if (bl_strtotype(param->type) == BL_TYPE_REF) {
      analyze_error("%s %d:%d unknown type '%s' for function parameter",
                    unit->filepath,
                    bo_members(param, Node)->line,
                    bo_members(param, Node)->col,
                    param->type);
    }
  }
}

void
analyze_gstmt(Unit           *unit,
              NodeGlobalStmt *node,
              jmp_buf         jmp_error)
{
  Node *child = NULL;
  const int c = bl_node_global_stmt_child_count(node);
  for (int i = 0; i < c; i++) {
    child = bl_node_global_stmt_child(node, i);
    switch (child->type) {
      case BL_NODE_FUNC_DECL:
        analyze_func(unit, (NodeFuncDecl *)child, jmp_error);
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
  jmp_buf jmp_error;
  if (setjmp(jmp_error))
    return false;

  Node *root = bl_ast_get_root(unit->ast);

  switch (root->type) {
    case BL_NODE_GLOBAL_STMT:
      analyze_gstmt(unit, (NodeGlobalStmt *)root, jmp_error);
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

