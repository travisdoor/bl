#include <gtest/gtest.h>
#include <bobject/containers/string.h>

#define TEST_STRING "this is test string"
#define TEST_COUNT 100

class test_string : public ::testing::Test
{
protected:
  void
  SetUp()
  {
    string = bo_string_new(0);
  }

  void
  TearDown()
  {
    bo_unref(string);
  }

  BString *string;
};

TEST_F(test_string, initialization)
{
  ASSERT_EQ(bo_string_len(string), 0);
  ASSERT_STREQ(bo_string_get(string), "");
}

TEST_F(test_string, get_set)
{
  bo_string_set(string, TEST_STRING);

  ASSERT_EQ(bo_string_len(string), strlen(TEST_STRING));
  ASSERT_STREQ(bo_string_get(string), TEST_STRING);
}

TEST_F(test_string, append)
{
  for (int i = 0; i < TEST_COUNT; i++) {
    bo_string_append(string, TEST_STRING);
  }

  ASSERT_EQ(bo_string_len(string), strlen(TEST_STRING) * TEST_COUNT);
}

TEST_F(test_string, cmp)
{
  BString *other = bo_string_new(strlen(TEST_STRING));
  bo_string_set(string, TEST_STRING);
  bo_string_set(other, TEST_STRING);

  ASSERT_EQ(bo_string_cmp(string, other), 0);
}
