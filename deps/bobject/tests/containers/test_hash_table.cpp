#include <gtest/gtest.h>
#include <bobject/containers/htbl.h>

#include "../mock.h"

#define TEST_COUNT 1000
#define TEST_DATA 666
#define TEST_INSERT_COUNT 10

class test_hash_table : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    table = bo_htbl_new_bo(bo_typeof(Mock), true, TEST_COUNT);

    MockParams p;
    p.data = TEST_DATA;
    mock_reset_utest_stats();

    for (int i = 0; i < TEST_COUNT; ++i) {
      Mock *obj = bo_new(Mock, &p);
      bo_htbl_insert(table, i, obj);
    }
  }

  void
  TearDown()
  {
    bo_unref(table);
    table = NULL;
    mock_reset_utest_stats();
  }

  BHashTable *table;
};

TEST_F(test_hash_table, initialization)
{
  ASSERT_EQ(bo_htbl_type(table), bo_typeof(Mock));
  ASSERT_EQ(bo_htbl_data_size(table), sizeof(Mock *));
  ASSERT_EQ(bo_htbl_size(table), TEST_COUNT);
}

TEST_F(test_hash_table, iterators)
{
  bo_iterator_t iter = bo_htbl_begin(table);
  bo_iterator_t end = bo_htbl_end(table);
  Mock *buf;
  while (!bo_iterator_equal(&iter, &end)) {
    buf = bo_htbl_iter_peek_value(table, &iter, Mock *);
    ASSERT_EQ(buf->data, TEST_DATA);
    bo_htbl_iter_next(table, &iter);
  }
}

TEST_F(test_hash_table, insert)
{
  ASSERT_EQ(mock_copy_called, 0);
  ASSERT_EQ(mock_ctor_called, TEST_COUNT);
  ASSERT_EQ(mock_dtor_called, 0);
}

TEST_F(test_hash_table, erase)
{
  mock_reset_utest_stats();

  bo_iterator_t iter = bo_htbl_begin(table);
  bo_iterator_t end = bo_htbl_end(table);
  while (!bo_iterator_equal(&iter, &end)) {
    bo_htbl_erase(table, &iter);
  }

  ASSERT_EQ(mock_copy_called, 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
}

TEST_F(test_hash_table, erase_key)
{
  mock_reset_utest_stats();

  for (int i = 0; i < TEST_COUNT; ++i) {
    bo_htbl_erase_key(table, i);
  }

  ASSERT_EQ(mock_copy_called, 0);
  ASSERT_EQ(mock_ctor_called, 0);
  ASSERT_EQ(mock_dtor_called, TEST_COUNT);
}
