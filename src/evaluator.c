//*****************************************************************************
// bl
//
// File:   evaluator.c
// Author: Martin Dorazil
// Date:   26.1.18
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

#include <stdio.h>
#include <evaluator.h>
#include <string.h>
#include "src_context.h"
#include "token.h"
#include "snippets.h"
#include "bldebug.h"

#define SCOPE_PAD 2

static const char *header = "/* Biscuit generated source code, do not modify. */\n\n";
static const char *gprefix = "_bl_";

/* decl */
static void
eval_scope(SrcContext *cnt,
           Pnode      *node,
           int         pad);

static void
eval_gscope(SrcContext *cnt,
            Pnode      *node);

static void
eval_attribute(SrcContext *cnt,
               Pnode      *node);

static void
eval_decl(SrcContext *cnt,
          Pnode      *node,
          int         pad);

static void
eval_method(SrcContext *cnt,
            Pnode      *node,
            int         pad);

static void
eval_call(SrcContext *cnt,
          Pnode      *node,
          int         pad);

static BString *
create_src(SrcContext *cnt);

/* impl */
BString *
create_src(SrcContext *cnt)
{
  BString *out = bo_string_new(1024);

  // TODO: depends on dependencies of source code
  char buf[1024];
  sprintf(buf, bl_snipp_inclh, "stdio.h");
  bo_string_append(cnt->includes, &buf[0]);
  
  bo_string_append(out, header);
  bo_string_append_str(out, cnt->includes);
  bo_string_append(out, "\n");
  bo_string_append_str(out, cnt->fdecl);
  bo_string_append(out, "\n");
  bo_string_append_str(out, cnt->impl);
  bo_string_append(out, "\n");

  return out;
}

void
eval_attribute(SrcContext *cnt,
               Pnode      *node)
{
  bl_pt_attribute *a = &node->content.as_attribute;
  bl_pt_method    *m = NULL;

  Pnode *child = bo_array_at(node->nodes, 0, Pnode *);
  // TODO: implement assert
  if (!child || child->type != BL_PT_METHOD)
    abort();

  m = &child->content.as_method;
  bl_src_context_addem(cnt, m->name, a->entry_point);
}

void
eval_decl(SrcContext *cnt,
          Pnode      *node,
          int         pad)
{
  // TODO: validate type
  char buf[1024];
  sprintf(buf, bl_snipp_var_decl,
    pad, "",
    node->content.as_decl.type->len,
    node->content.as_decl.type->content.as_string, 
    "",
    node->content.as_decl.name->len,
    node->content.as_decl.name->content.as_string
  );

  bo_string_append(cnt->impl, &buf[0]);
}

void
eval_call(SrcContext *cnt,
          Pnode      *node,
          int         pad)
{
  // TODO: validate type
  char buf[1024];
  bl_pt_call *c = &node->content.as_call;

  // external reference
  const bl_token_t *name = bl_src_context_getem(cnt, c->name); 
  bl_assert(name, "undeclared function call");

  // declaration
  sprintf(buf, bl_snipp_method_call,
    pad, "",
    "",
    name->len,
    name->content.as_string, 
    c->param1->len,
    c->param1->content.as_string
  );
  bo_string_append(cnt->impl, &buf[0]);
}


void
eval_method(SrcContext *cnt,
            Pnode      *node,
            int         pad)
{
  // TODO: validate type
  char buf[1024];
  bl_pt_method *m = &node->content.as_method;

  // external reference
  if (m->modif && m->modif->sym == BL_SYM_EXTERN) {
    return;
  }

  bl_src_context_addem(cnt, m->name, m->name);

  bool is_main = strncmp(m->name->content.as_string, "main", m->name->len) == 0;
  const char *prefix = is_main ? gprefix : "";
  

  // declaration
  sprintf(buf, bl_snipp_method_decl,
    0, "",
    m->ret->len,
    m->ret->content.as_string, 
    prefix,
    m->name->len,
    m->name->content.as_string
  );
  bo_string_append(cnt->fdecl, &buf[0]);

  // implementation
  sprintf(buf, bl_snipp_method_impl,
    0, "",
    m->ret->len,
    m->ret->content.as_string, 
    prefix,
    m->name->len,
    m->name->content.as_string
  );
  bo_string_append(cnt->impl, &buf[0]);
  eval_scope(cnt, bo_array_at(node->nodes, 0, Pnode *), pad);
  bo_string_append(cnt->impl, "}\n\n");

  // detect main method
  if (strncmp(m->name->content.as_string, "main", m->name->len) == 0) {
    sprintf(buf, bl_snipp_main,
      prefix,
      m->name->len,
      m->name->content.as_string
    );
    bo_string_append(cnt->impl, &buf[0]);
  }
}

void
eval_scope(SrcContext *cnt,
           Pnode      *node,
           int         pad)
{
  Pnode *child = NULL;
  pad+=SCOPE_PAD;
  size_t c = bo_array_size(node->nodes);
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(node->nodes, i, Pnode *);

    switch (child->type) {
      case BL_PT_DECL:
        eval_decl(cnt, child, pad);
        break;
      case BL_PT_METHOD:
        eval_method(cnt, child, pad);
        break;
      case BL_PT_CALL:
        eval_call(cnt, child, pad);
        break;
      default:
        bl_parse_error("unknown node");
    }
  }
}


void
eval_gscope(SrcContext *cnt,
            Pnode      *node)
{
  Pnode *child = NULL;
  size_t c = bo_array_size(node->nodes);
  for (size_t i = 0; i < c; i++) {
    child = bo_array_at(node->nodes, i, Pnode *);

    switch (child->type) {
      case BL_PT_DECL:
        eval_decl(cnt, child, 0);
        break;
      case BL_PT_METHOD:
        eval_method(cnt, child, 0);
        break;
      case BL_PT_ATTRIBUTE:
        eval_attribute(cnt, child);
        break;
      default:
        bl_parse_error("unknown node");
    }
  }
}

/* public */
BString *
bl_evaluator_evaluate(Pnode *node)
{
  // TODO: cache context for other sources???
  SrcContext *cnt = bl_src_context_new();

  if (node->type != BL_PT_GSCOPE)
    bl_parse_error("invalid node, expected global scope");

  eval_gscope(cnt, node);

  BString * src = create_src(cnt);  
  bo_unref(cnt);
  return src;
}
