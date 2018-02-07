set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_VISIBILITY_PRESET hidden)

# setup output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

# deps
find_package(BObject 1.0 REQUIRED)

set(HEADER_FILES 
  src/pipeline/stage_impl.h 
  src/pipeline/actor_impl.h 
  src/pipeline/pipeline_impl.h 
  src/bldebug_impl.h
  src/lexer_impl.h 
  src/tokens_impl.h 
  src/file_loader_impl.h 
  src/ast_printer_impl.h 
  src/module_impl.h 
  src/domains_impl.h 
  src/token_impl.h
  src/unit_impl.h 
  src/parser_impl.h 
  src/ast/ast_impl.h
  src/ast/node_impl.h
  src/ast/node_param_var_decl_impl.h
  src/ast/node_func_decl_impl.h
  src/ast/node_global_stmt_impl.h
  src/ast/node_stmt_impl.h
  src/token_printer_impl.h 
  )

set(SOURCE_FILES 
  src/pipeline/stage.c
  src/pipeline/actor.c
  src/pipeline/pipeline.c
  src/lexer.c
  src/module.c
  src/tokens.c
  src/file_loader.c
  src/ast_printer.c
  src/token.c
  src/unit.c
  src/parser.c
  src/ast/ast.c
  src/ast/node.c
  src/ast/node_param_var_decl.c
  src/ast/node_func_decl.c
  src/ast/node_global_stmt.c
  src/ast/node_stmt.c
  src/token_printer.c)

add_library(bl SHARED ${SOURCE_FILES})

target_link_libraries(bl bobject)

target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

