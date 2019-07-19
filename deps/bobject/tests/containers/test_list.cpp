#include <gtest/gtest.h>
#include <bobject/containers/list.h>

#include "../mock.h"

#define TEST_COUNT 1000
#define TEST_DATA 666

class test_list : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    list = bo_list_new_bo(bo_typeof(Mock), true);
    mock_reset_utest_stats();
  }

  void
  TearDown()
  {
    bo_unref(list);
  }

  BList *list;
};

TEST_F(test_list, initialization)
{
  ASSERT_TRUE(bo_list_empty(list));
  ASSERT_EQ(bo_list_begin(list).opaque, bo_list_end(list).opaque);
  ASSERT_EQ(bo_list_data_size(list), sizeof(BObject *));
  ASSERT_EQ(bo_list_size(list), 0);
  ASSERT_TRUE(bo_list_managed(list));
}

TEST_F(test_list, push_back)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_list_push_back(list, obj);
  }

  ASSERT_EQ(bo_list_size(list), TEST_COUNT);
  ASSERT_EQ(mock_ctor_called, TEST_COUNT);
  ASSERT_EQ(mock_dtor_called, 0);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_list, push_front)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_list_push_front(list, obj);
  }

  ASSERT_EQ(bo_list_size(list), TEST_COUNT);
  ASSERT_EQ(mock_ctor_called, TEST_COUNT);
  ASSERT_EQ(mock_dtor_called, 0);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_list, pop_back)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_list_push_back(list, obj);
  }

  mock_reset_utest_stats();
  while (!bo_list_empty(list)) {
    bo_list_pop_back(list);
  }

  ASSERT_EQ(bo_list_size(list), 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_list, pop_front)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_list_push_back(list, obj);
  }

  mock_reset_utest_stats();
  while (!bo_list_empty(list)) {
    bo_list_pop_front(list);
  }

  ASSERT_EQ(bo_list_size(list), 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_list, erase)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_list_push_back(list, obj);
  }

  mock_reset_utest_stats();
  bo_iterator_t iter = bo_list_begin(list);
  while (!bo_list_empty(list)) {
    bo_list_erase(list, &iter);
  }

  ASSERT_EQ(bo_list_size(list), 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
  ASSERT_EQ(mock_copy_called, 0);
}
