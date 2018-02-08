enable_testing()
find_package(GTest REQUIRED)

add_executable(bl_tests
        libbl/lexer_test.cpp
        libbl/test_all.c)

target_link_libraries(bl_tests GTest::GTest GTest::Main)
target_link_libraries(bl_tests bl)

add_test(bl bl_tests)
