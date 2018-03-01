set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_VISIBILITY_PRESET hidden)

# setup output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

# deps
find_package(BObject 1.0 REQUIRED)
find_package(LLVM REQUIRED CONFIG)

llvm_map_components_to_libnames(LLVM_LIBS
        support
        core
        bitwriter
        executionengine
        interpreter
        linker
        native)

set(HEADER_FILES
        include/bl/pipeline/actor.h
        include/bl/pipeline/pipeline.h
        include/bl/pipeline/stage.h
        include/bl/ast_printer.h
        include/bl/bl.h
        include/bl/bldebug.h
        include/bl/file_loader.h
        include/bl/parser.h
        include/bl/token_printer.h
        include/bl/unit.h
        include/bl/bllimits.h
        include/bl/token.h
        include/bl/analyzer.h
        include/bl/llvm_backend.h
        include/bl/assembly.h
        include/bl/compile_group.h
        include/bl/llvm_jit_exec.h
        include/bl/builder.h
        include/bl/llvm_bc_writer.h
        include/bl/lexer.h
        include/bl/llvm_linker.h include/bl/blmemory.h)

set(HEADER_FILES_IMPL
        src/ast/ast_impl.h
        src/containers/array_impl.h
        src/llvm_bl_cnt_impl.h
        src/pipeline/actor_impl.h
        src/sym_tbl_impl.h
        src/tokens_impl.h
        src/identifier_impl.h src/ast/node_impl.h src/type_impl.h src/unit_impl.h)

set(SOURCE_FILES
        src/pipeline/stage.c
        src/pipeline/actor.c
        src/pipeline/pipeline.c
        src/tokens.c
        src/file_loader.c
        src/containers/array.c
        src/ast_printer.c
        src/token.c
        src/unit.c
        src/parser.c
        src/analyzer.c
        src/llvm_backend.c
        src/ast/ast.c
        src/token_printer.c
        src/assembly.c
        src/type.c
        src/identifier.c
        src/sym_tbl.c
        src/llvm_jit_exec.c
        src/builder.c
        src/llvm_bc_writer.c
        src/llvm_bl_cnt.c
        src/lexer.c
        src/llvm_linker.c src/ast/node.c)

add_library(bl SHARED ${SOURCE_FILES} ${HEADER_FILES} ${HEADER_FILES_IMPL})

target_link_libraries(bl bobject)
target_link_libraries(bl ${LLVM_LIBS})

target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

