enable_testing()
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

find_package(GTest REQUIRED)

add_executable(bl_tests
        libbl/test_all.cpp libbl/compiler_test.cpp)

target_link_libraries(bl_tests GTest::GTest GTest::Main)
target_link_libraries(bl_tests bl)

add_test(bl bl_tests)
