cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.1.0 LANGUAGES C)

set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(SOURCE_FILES 
  src/pipeline/stage.h 
  src/pipeline/stage.c
  src/pipeline/actor.h 
  src/pipeline/actor.c
  src/pipeline/pipeline.h 
  src/pipeline/pipeline.c
  src/node.c 
  src/main.c
  src/bldebug.h
  src/lexer.c
  src/lexer.h 
  src/module.c
  src/tokens.c
  src/tokens.h 
  src/file_loader.c
  src/file_loader.h 
  src/ast_printer.c
  src/ast_printer.h 
  src/module.h 
  src/domains.h 
  src/token.h
  src/token.c
  src/unit.c
  src/unit.h 
  src/parser.h 
  src/parser.c 
  src/node.h 
  src/node.c 
  src/node_func_decl.h 
  src/node_func_decl.c 
  src/node_global_stmt.h 
  src/node_global_stmt.c 
  )

add_executable(bl ${SOURCE_FILES})

target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

find_package(BObject 1.0 REQUIRED)
target_link_libraries(bl bobject)

if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Od")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces")
endif()
