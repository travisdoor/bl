#include <gtest/gtest.h>
#include <bobject/containers/array.h>

#include "../mock.h"

#define TEST_COUNT 1000
#define TEST_DATA 666
#define TEST_INSERT_COUNT 10

class test_array : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    array = bo_array_new_bo(bo_typeof(Mock), true);
  }

  void
  TearDown()
  {
    bo_unref(array);
    array = NULL;
    mock_reset_utest_stats();
  }

  BArray *array;
};

TEST_F(test_array, initialization)
{
  ASSERT_EQ(bo_array_type(array), bo_typeof(Mock));
  ASSERT_EQ(bo_array_data_size(array), sizeof(Mock *));
  ASSERT_EQ(bo_array_size(array), 0);
  ASSERT_EQ(bo_array_capacity(array), 0);
  ASSERT_FALSE(bo_array_data(array));
}

TEST_F(test_array, push_back)
{
  bo_array_reserve(array, TEST_COUNT);
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_array_push_back(array, obj);
  }

  ASSERT_EQ(bo_array_size(array), TEST_COUNT);
  ASSERT_EQ(mock_ctor_called, TEST_COUNT);
  ASSERT_EQ(mock_dtor_called, 0);
  ASSERT_EQ(mock_copy_called, 0);

  for (int i = 0; i < TEST_COUNT; ++i) {
    ASSERT_EQ(bo_array_at(array, i, Mock *)->data, TEST_DATA);
  }
}

TEST_F(test_array, pop)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_array_push_back(array, obj);
  }

  mock_reset_utest_stats();
  for (int i = 0; i < TEST_COUNT; ++i) {
    bo_array_pop_back(array);
  }

  ASSERT_EQ(bo_array_size(array), 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_array, reserve)
{
  bo_array_reserve(array, TEST_COUNT);

  ASSERT_EQ(bo_array_size(array), 0);
  ASSERT_EQ(bo_array_capacity(array), TEST_COUNT);
  ASSERT_TRUE(bo_array_data(array));
}

TEST_F(test_array, resize_unmanaged)
{
  BArray *array2 = bo_array_new(sizeof(int));

  bo_array_resize(array2, TEST_COUNT);
  ASSERT_EQ(bo_array_size(array2), TEST_COUNT);

  for (size_t i = bo_array_size(array2); i--;)
  {
    ASSERT_EQ(bo_array_at(array2, i, int), 0);
  }

  bo_array_resize(array2, TEST_COUNT / 2);
  ASSERT_EQ(bo_array_size(array2), TEST_COUNT / 2);

  bo_unref(array2);
}

TEST_F(test_array, resize_managed)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_array_push_back(array, obj);
  }

  mock_reset_utest_stats();

  bo_array_resize(array, 0);
  ASSERT_EQ(bo_array_size(array), 0);

  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
  ASSERT_EQ(mock_copy_called, 0);

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_array_push_back(array, obj);
  }

  mock_reset_utest_stats();
  bo_array_resize(array, TEST_COUNT / 2);
  ASSERT_EQ(bo_array_size(array), TEST_COUNT / 2);

  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT / 2);
  ASSERT_EQ(mock_copy_called, 0);

  bo_array_clear(array);

  for (int i = 0; i < TEST_COUNT / 2; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_array_push_back(array, obj);
  }
  mock_reset_utest_stats();

  bo_array_resize(array, TEST_COUNT);
  ASSERT_EQ(bo_array_size(array), TEST_COUNT);

  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, 0);
  ASSERT_EQ(mock_copy_called, 0);

  bo_array_clear(array);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT / 2);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_array, insert)
{
  BArray *array2 = bo_array_new(sizeof(int));
  int test_data[10];
  for (size_t i = 10; i--;)
  {
    test_data[i] = TEST_DATA;
  }

  bo_array_insert(array2, 0, (void *)&test_data[0], 10);
  ASSERT_EQ(bo_array_size(array2), 10);

  bo_array_insert(array2, 0, (void *)&test_data[0], 10);
  ASSERT_EQ(bo_array_size(array2), 10);

  for (size_t i = bo_array_size(array2); i--;)
  {
    ASSERT_EQ(bo_array_at(array2, i, int), TEST_DATA);
  }

  bo_array_insert(array2, 10, (void *)&test_data[0], 10);
  ASSERT_EQ(bo_array_size(array2), 20);

  for (size_t i = bo_array_size(array2); i--;)
  {
    ASSERT_EQ(bo_array_at(array2, i, int), TEST_DATA);
  }

  bo_unref(array2);
}

TEST_F(test_array, erase)
{
  MockParams params;
  params.data = TEST_DATA;

  for (int i = 0; i < TEST_COUNT; ++i) {
    Mock *obj = bo_new(Mock, &params);
    bo_array_push_back(array, obj);
  }
  mock_reset_utest_stats();

  while (!bo_array_empty(array)) {
    bo_array_erase(array, 0);
  }

  ASSERT_EQ(bo_array_size(array), 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
  ASSERT_EQ(mock_copy_called, 0);
}

TEST_F(test_array, simple_data)
{
  BArray *sarr = bo_array_new(sizeof(int));
  for (int i = 0; i < TEST_COUNT; i++)
  {
    bo_array_push_back(sarr, i);
  }

  ASSERT_EQ(bo_array_size(sarr), TEST_COUNT);

  for (int i = 0; i < TEST_COUNT; i++)
  {
    ASSERT_EQ(bo_array_at(sarr, i, int), i);
  }
  bo_unref(sarr);
}

TEST_F(test_array, iter)
{
  BArray *sarr = bo_array_new(sizeof(int));
  for (int i = 0; i < TEST_COUNT; i++)
  {
    bo_array_push_back(sarr, i);
  }

  ASSERT_EQ(bo_array_size(sarr), TEST_COUNT);

  bo_iterator_t iter = bo_array_begin(sarr);
  bo_iterator_t end = bo_array_end(sarr);

  int i = 0;
  int val = 0;
  while (!bo_iterator_equal(&iter, &end)) {
    val = bo_array_iter_peek(sarr, &iter, int);
    ASSERT_EQ(val, i);
    i++;
    bo_array_iter_next(sarr, &iter);
  }
  bo_unref(sarr);
}
