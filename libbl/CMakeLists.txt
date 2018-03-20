set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_VISIBILITY_PRESET hidden)

# setup output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

# deps
find_package(BObject 1.0 REQUIRED)
find_package(LLVM REQUIRED CONFIG)

llvm_map_components_to_libnames(LLVM_LIBS
        all
        )

set(HEADER_FILES
        include/bl/bl.h
        include/bl/unit.h
        include/bl/bllimits.h
        include/bl/assembly.h
        include/bl/builder.h
        include/bl/bldebug.h
        include/bl/error.h include/bl/config.h)

set(HEADER_FILES_IMPL
        src/builder_impl.h
        src/blmemory_impl.h
        src/llvm_bl_cnt_impl.h
        src/assembly_impl.h
        src/token_impl.h
        src/tokens_impl.h
        src/identifier_impl.h
        src/ast/node_impl.h
        src/type_impl.h
        src/unit_impl.h
        src/stages_impl.h
        src/common_impl.h 
        src/ast/id_impl.h
        src/ast/visitor_impl.h
        src/ast/ast2_impl.h)

set(SOURCE_FILES
        src/tokens.c
        src/file_loader.c
        src/unit.c
        src/token.c
        src/token_printer.c
        src/assembly.c
        src/type.c
        src/identifier.c
        src/llvm_jit_exec.c
        src/builder.c
        src/llvm_bc_writer.c
        src/llvm_bl_cnt.c
        src/llvm_native_bin.c
        src/lexer.c
        src/ast_printer.c
        src/llvm_linker.c 
        src/ast/id.c src/parser2.c src/ast/ast2.c src/ast/visitor.c)

add_library(bl SHARED ${SOURCE_FILES} ${HEADER_FILES} ${HEADER_FILES_IMPL})

target_link_libraries(bl bobject)
target_link_libraries(bl ${LLVM_LIBS})

target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

