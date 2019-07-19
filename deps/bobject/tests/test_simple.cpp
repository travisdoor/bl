#include <gtest/gtest.h>
#include "mock.h"

#define TEST_COUNT 1000

class test_simple : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    MockParams p;
    p.first = 1;
    p.second = 2;
    p.third = 3;

    mock = bo_new(Mock, &p);
  }

  void
  TearDown()
  {
    bo_unref(mock);
    mock = nullptr;
    mock_reset_utest_stats();
  }

  Mock *mock;
};

TEST_F(test_simple, initialization)
{
  ASSERT_TRUE(mock);
  ASSERT_EQ(mock_ctor_called, 1);
  ASSERT_EQ(mock_dtor_called, 0);
  ASSERT_EQ(mock_init_called, 1);

  ASSERT_EQ(mock->first, 1);
  ASSERT_EQ(mock->second, 2);
  ASSERT_EQ(mock->third, 3);

  ASSERT_TRUE(bo_vtbl(mock, Mock));
  ASSERT_TRUE(bo_vtbl(mock, Mock)->method_one);
  ASSERT_TRUE(bo_vtbl(mock, Mock)->method_two);
}

TEST_F(test_simple, helpers)
{
  ASSERT_STREQ(bo_type_name(mock), "Mock");
  ASSERT_TRUE(bo_is_typeof(mock, Mock));
  ASSERT_EQ(bo_sizeof(Mock), sizeof(Mock));
}

TEST_F(test_simple, method_call)
{
  ASSERT_TRUE(bo_vtbl(mock, Mock));
  ASSERT_TRUE(bo_vtbl(mock, Mock)->method_one);
  ASSERT_TRUE(bo_vtbl(mock, Mock)->method_two);

  bo_vtbl(mock, Mock)->method_one(mock);
  bo_vtbl(mock, Mock)->method_two(mock);

  ASSERT_EQ(mock_method_one_called, 1);
  ASSERT_EQ(mock_method_two_called, 1);
}

TEST_F(test_simple, copying)
{
  Mock *dest = (Mock *)(bo_duplicate(mock));

  ASSERT_EQ(mock_ctor_called, 1);
  ASSERT_EQ(mock_dtor_called, 0);
  ASSERT_EQ(mock_copy_called, 1);

  ASSERT_EQ(dest->first, 1);
  ASSERT_EQ(dest->second, 2);
  ASSERT_EQ(dest->third, 3);

  bo_unref(dest);

  ASSERT_EQ(mock_ctor_called, 1);
  ASSERT_EQ(mock_dtor_called, 1);
  ASSERT_EQ(mock_copy_called, 1);
}

TEST_F(test_simple, ref_counting)
{
  for (int i = TEST_COUNT; i--;)
  {
    bo_ref(mock);
  }

  ASSERT_EQ(bo_ref_count(mock), TEST_COUNT + 1);

  for (int i = TEST_COUNT + 1; i--;)
  {
    bo_unref(mock);
  }

  ASSERT_EQ(mock_ctor_called, 1);
  ASSERT_EQ(mock_dtor_called, 1);
  ASSERT_EQ(mock_copy_called, 0);
}

