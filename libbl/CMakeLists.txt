set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_VISIBILITY_PRESET hidden)

# setup output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

# deps
find_package(BObject 1.0 REQUIRED)
find_package(LLVM REQUIRED CONFIG)

llvm_map_components_to_libnames(LLVM_LIBS support core bitwriter)

set(HEADER_FILES
        include/bl/pipeline/actor.h
        include/bl/pipeline/pipeline.h
        include/bl/pipeline/stage.h
        include/bl/ast_printer.h
        include/bl/bl.h
        include/bl/bldebug.h
        include/bl/type_table.h
        include/bl/file_loader.h
        include/bl/lexer.h
        include/bl/parser.h
        include/bl/token_printer.h
        include/bl/unit.h
        include/bl/token.h
        include/bl/tokens.h
        include/bl/ast/node.h
        include/bl/ast/node_expr.h
        include/bl/ast/node_return_stmt.h
        include/bl/ast/node_global_stmt.h
        include/bl/ast/node_func_decl.h
        include/bl/ast/node_param_var_decl.h
        include/bl/ast/ast.h
        include/bl/llvm_backend.h
        include/bl/ast/node_stmt.h include/bl/assembly.h include/bl/compile_group.h)

set(HEADER_FILES_IMPL
        src/ast/node_impl.h
        src/ast/node_func_decl_impl.h
        src/ast/node_global_stmt_impl.h
        src/ast/node_param_var_decl_impl.h
        src/ast/node_stmt_impl.h
        src/ast/node_return_stmt_impl.h
        src/ast/node_expr_impl.h
        src/ast/ast_impl.h
        src/pipeline/actor_impl.h
        src/ast_printer_impl.h
        src/file_loader_impl.h
        src/unit_impl.h src/assembly_impl.h)

set(SOURCE_FILES 
        src/pipeline/stage.c
        src/pipeline/actor.c
        src/pipeline/pipeline.c
        src/lexer.c
        src/tokens.c
        src/file_loader.c
        src/ast_printer.c
        src/token.c
        src/unit.c
        src/parser.c
        src/type_table.c
        src/llvm_backend.c
        src/ast/ast.c
        src/ast/node.c
        src/ast/node_expr.c
        src/ast/node_param_var_decl.c
        src/ast/node_func_decl.c
        src/ast/node_global_stmt.c
        src/ast/node_return_stmt.c
        src/ast/node_stmt.c
        src/token_printer.c src/assembly.c)

add_library(bl SHARED ${SOURCE_FILES} ${HEADER_FILES} ${HEADER_FILES_IMPL})

target_link_libraries(bl bobject)
target_link_libraries(bl ${LLVM_LIBS})

target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

