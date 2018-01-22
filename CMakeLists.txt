cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.1.0)

set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(SOURCE_FILES src/main.c src/parser.c src/parser.h)

add_executable(bl ${SOURCE_FILES})

target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Od")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall")
endif()
