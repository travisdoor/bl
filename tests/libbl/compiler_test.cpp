//*****************************************************************************
// blc
//
// File:   compiler_test.cpp
// Author: Martin Dorazil
// Date:   14/02/2018
//
// Copyright 2017 Martin Dorazil
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

#include <gtest/gtest.h>
#include "bl/bl.h"

#define SRC_LOC "../tests/src/"
#define FLAGS BL_BUILDER_RUN | BL_BUILDER_LOAD_FROM_FILE

class CompilerTest : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    assembly = bl_assembly_new("test_assembly");
    builder = bl_builder_new();
  }

  void
  TearDown()
  {
    bo_unref(assembly);
    bo_unref(builder);
  }

  bl_assembly_ref assembly;
  bl_builder_ref builder;
};

TEST_F(CompilerTest, simple_definitions)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "simple_decl_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, simple_definitions_with_asignement)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "decl_def_values_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, simple_extern_call)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "simple_ext_call_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, func_def_ordering)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "method_ordering_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, assign_expr)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "simple_assignment_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, simple_call)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "simple_method_call_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, expressions)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "expression_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, compound_sub_statemets)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "sub_statement_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, ifs)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "if_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}

TEST_F(CompilerTest, loop)
{
  bl_unit_ref unit = bl_unit_new_file(SRC_LOC "loop_test.bl");
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly, FLAGS)) {
    ASSERT_TRUE(true);
  }
}
