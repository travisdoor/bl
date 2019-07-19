enable_testing()
find_package(GTest REQUIRED)

add_executable(bobject_tests 
  test_all.cpp 
  mock.c 
  mock_child.c 
  test_simple.cpp 
  test_inheritance.cpp
  containers/test_array.cpp
  containers/test_hash_table.cpp
  containers/test_list.cpp
  containers/test_string.cpp
  )

target_link_libraries(bobject_tests GTest::GTest GTest::Main)
target_link_libraries(bobject_tests bobject)

add_test(bobject bobject_tests)
