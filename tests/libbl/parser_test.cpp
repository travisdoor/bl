//*****************************************************************************
// blc
//
// File:   parser_test.cpp
// Author: Martin Dorazil
// Date:   08/02/2018
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

class ParserTest : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    pipeline = bl_pipeline_new();
    assembly = bl_assembly_new("test_assembly");
    builder = bl_builder_new_custom(pipeline);

    auto *lexer = (Stage *)bl_lexer_new(BL_CGROUP_PRE_ANALYZE);
    bl_pipeline_add_stage(pipeline, lexer);

    auto *parser = (Stage *)bl_parser_new(BL_CGROUP_PRE_ANALYZE);
    bl_pipeline_add_stage(pipeline, parser);
  }

  void
  TearDown()
  {
    bo_unref(assembly);
    bo_unref(pipeline);
    bo_unref(builder);
  }

  Pipeline *pipeline;
  Assembly *assembly;
  Builder *builder;
};

TEST_F(ParserTest, ast_func_decl)
{
  const char *src =
    "void func() {}";

  auto *unit = bl_unit_new_str("ast_func_decl", src);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_pipeline_get_failed(pipeline);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(ParserTest, ast_func_decl_param)
{
  const char *src =
    "void func(int i) {}";

  auto *unit = bl_unit_new_str("ast_func_decl_param", src);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_pipeline_get_failed(pipeline);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(ParserTest, ast_func_decl_params)
{
  const char *src =
    "void func(int i, char c, float f) {}";

  auto *unit = bl_unit_new_str("ast_func_decl_params", src);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_pipeline_get_failed(pipeline);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(ParserTest, ast_func_decl_extra_semicolon)
{
  const char *src =
    "void func() {;;;}";

  auto *unit = bl_unit_new_str("ast_func_decl_extra_semicolon", src);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_pipeline_get_failed(pipeline);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(ParserTest, ast_func_decl_ret)
{
  const char *src =
    "int func() {return 0;}";

  auto *unit = bl_unit_new_str("ast_func_decl_ret", src);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_pipeline_get_failed(pipeline);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(ParserTest, ast_func_decl_extern)
{
  const char *src =
    "extern int func(int i);";

  auto *unit = bl_unit_new_str("ast_func_decl_extern", src);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_pipeline_get_failed(pipeline);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

