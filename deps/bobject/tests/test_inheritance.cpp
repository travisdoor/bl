#include <gtest/gtest.h>
#include "mock_child.h"

class test_inheritance : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    child = bo_new(MockChild, 0);
  }

  void
  TearDown()
  {
    bo_unref(child);
    mock_child_reset_utest_stats();
  }

  MockChild *child;
};

TEST_F(test_inheritance, initialization)
{
  ASSERT_TRUE(child);
  ASSERT_EQ(mock_child_ctor_called, 1);
  ASSERT_EQ(mock_child_dtor_called, 0);
  ASSERT_EQ(mock_child_init_called, 1);

  ASSERT_EQ(child->base.first, 1);
  ASSERT_EQ(child->base.second, 2);
  ASSERT_EQ(child->base.third, 3);

  ASSERT_TRUE(bo_vtbl(child, Mock));
  ASSERT_TRUE(bo_vtbl(child, Mock)->method_one);
  ASSERT_TRUE(bo_vtbl(child, Mock)->method_two);
}

TEST_F(test_inheritance, helpers)
{
  ASSERT_STREQ(bo_type_name(child), "MockChild");
  ASSERT_TRUE(bo_is_typeof(child, MockChild));
  ASSERT_EQ(bo_sizeof(MockChild), sizeof(MockChild));
}

TEST_F(test_inheritance, method_call)
{
  ASSERT_TRUE(bo_vtbl(child, Mock));
  ASSERT_TRUE(bo_vtbl(child, Mock)->method_one);
  ASSERT_TRUE(bo_vtbl(child, Mock)->method_two);

  bo_vtbl(child, Mock)->method_one((Mock *)child);
  bo_vtbl(child, Mock)->method_two((Mock *)child);

  ASSERT_EQ(mock_child_method_one_called, 1);
  ASSERT_EQ(mock_method_two_called, 1);
}

TEST_F(test_inheritance, copying)
{
  MockChild *dest = (MockChild *)bo_duplicate(child);

  ASSERT_EQ(mock_child_ctor_called, 1);
  ASSERT_EQ(mock_child_dtor_called, 0);
  ASSERT_EQ(mock_child_copy_called, 1);

  ASSERT_EQ(dest->base.first, 1);
  ASSERT_EQ(dest->base.second, 2);
  ASSERT_EQ(dest->base.third, 3);

  bo_unref(dest);

  ASSERT_EQ(mock_child_ctor_called, 1);
  ASSERT_EQ(mock_child_dtor_called, 1);
}

