//*****************************************************************************
// bl
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

const char *src1 =
  "int main() {"
    "int i;"
    "char ch;"
    "string s;"
    "bool bl;"
    "return 0;"
  "}";

const char *src2 =
  "int main() {"
    "int i = 10;"
    "char ch;"
    "string s = \"hello\";"
    "int a = 0;"
    "bool bl = true;"
    "bool bl2 = false;"
    "bool bl3 = 1;"
    "bool bl4 = 0;"
    "return 0;"
    "}";

const char *src3 =
  "extern void puts(string s);"
  "int main() {"
    "puts(\"hello\");"
    "return 0;"
  "}";

const char *src4 =
  "void before() {"
  "}"
  "int main() {"
    "before();"
    "after();"
    "return 0;"
  "}"
  "void after() {"
  "}";

const char *src5 =
  "int main() {"
    "int i = 10;"
    "i = 0;"
    "string s = \"string\";"
    "string s2;"
    "s = \"string\";"
    "s2 = s;"
    "return 0;"
    "}";

const char *src6 =
  "int before(int a) {"
    "return a;"
  "}"
  "int main() {"
    "int a = before(0);"
    "after(a);"
    "return a;"
  "}"
  "int after(int a) {"
    "return a;"
  "}";

class CompilerTest : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    assembly = bl_assembly_new("test_assembly");
    builder = bl_builder_new(BL_BUILDER_RUN);
  }

  void
  TearDown()
  {
    bo_unref(assembly);
    bo_unref(builder);
  }

  Assembly *assembly;
  Builder *builder;
};

TEST_F(CompilerTest, simple_definitions)
{
  Unit *unit = bl_unit_new_str("unit1", src1);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_builder_get_failed(builder);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(CompilerTest, simple_definitions_with_asignement)
{
  Unit *unit = bl_unit_new_str("unit2", src2);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_builder_get_failed(builder);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(CompilerTest, simple_extern_call)
{
  Unit *unit = bl_unit_new_str("unit3", src3);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_builder_get_failed(builder);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(CompilerTest, func_def_ordering)
{
  Unit *unit = bl_unit_new_str("unit4", src4);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_builder_get_failed(builder);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(CompilerTest, assign_expr)
{
  Unit *unit = bl_unit_new_str("unit5", src5);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_builder_get_failed(builder);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

TEST_F(CompilerTest, simple_call)
{
  Unit *unit = bl_unit_new_str("unit6", src6);
  bl_assembly_add_unit(assembly, unit);

  if (!bl_builder_compile(builder, assembly)) {
    auto *failed = bl_builder_get_failed(builder);
    ASSERT_STREQ(bl_actor_get_error(failed), "");
  }
}

