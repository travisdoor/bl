cmake_minimum_required(VERSION 3.5)

project(deleteme LANGUAGES C)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_library(deleteme SHARED
  deleteme.c
)

target_compile_definitions(deleteme PRIVATE DELETEME_LIBRARY)
