//************************************************************************************************
// bl
//
// File:   preproc.h
// Author: Martin Dorazil
// Date:   03/03/2018
//
// Copyright 2018 Martin Dorazil
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, preproc, publish, distribute, sublicense, and/or sell
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

#include "common_impl.h"
#include "stages_impl.h"
#include "visitor_impl.h"

#define peek_cnt(visitor) ((context_t *)(visitor)->context)

typedef struct {
  bl_assembly_t *assembly;
} context_t;

static void
preproc_load(bl_visitor_t *visitor, bl_node_t *load);

static void
preproc_link(bl_visitor_t *visitor, bl_node_t *link);

void
preproc_load(bl_visitor_t *visitor, bl_node_t *load)
{
  context_t *    cnt   = peek_cnt(visitor);
  bl_pre_load_t *_load = bl_peek_pre_load(load);
  bl_unit_t *unit = bl_unit_new_file(_load->filepath);
  bool added = bl_assembly_add_unit_unique(cnt->assembly, unit);
  if (added == false) {
    bl_unit_delete(unit);
  }
}

void
preproc_link(bl_visitor_t *visitor, bl_node_t *link)
{
  context_t *    cnt   = peek_cnt(visitor);
  bl_assembly_add_link(cnt->assembly, bl_peek_pre_link(link)->lib);
}

/*************************************************************************************************
 * main entry function
 *************************************************************************************************/
bl_error_e
bl_preproc_run(bl_builder_t *builder, bl_unit_t *unit, bl_assembly_t *assembly)
{
  context_t cnt = {.assembly = assembly};

  bl_visitor_t visitor_preproc;
  bl_visitor_init(&visitor_preproc, &cnt);
  bl_visitor_add(&visitor_preproc, preproc_load, BL_VISIT_LOAD);
  bl_visitor_add(&visitor_preproc, preproc_link, BL_VISIT_LINK);
  bl_visitor_add(&visitor_preproc, BL_SKIP_VISIT, BL_VISIT_FUNC);
  bl_visitor_add(&visitor_preproc, BL_SKIP_VISIT, BL_VISIT_STRUCT);
  bl_visitor_add(&visitor_preproc, BL_SKIP_VISIT, BL_VISIT_CONST);
  bl_visitor_add(&visitor_preproc, BL_SKIP_VISIT, BL_VISIT_ENUM);

  bl_visitor_walk_module(&visitor_preproc, unit->ast.root);

  return BL_NO_ERR;
}
