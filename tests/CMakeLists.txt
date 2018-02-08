enable_testing()
find_package(GTest REQUIRED)

add_executable(bl_tests
        libbl/lexer_test.cpp
        )

target_link_libraries(bl_tests GTest::GTest GTest::Main)

add_test(bl bl_tests)
