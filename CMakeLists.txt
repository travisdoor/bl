cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.4.1 LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")
#set(DYNCALL_PATH "C:/Program Files/DynCall" CACHE PATH "Path to Dyncall.")

if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

add_subdirectory(deps/bobject)
add_subdirectory(deps/dyncall-1.0)

# deps
#find_package(BObject 1.0 REQUIRED)
find_package(LLVM REQUIRED CONFIG)

set(HEADER_FILES
        src/bldebug.h
        src/error.h 
        src/config.h
	src/messages.h
        src/builder.h
        src/blmemory.h
        src/assembly.h
        src/token.h
        src/tokens.h
        src/unit.h
        src/stages.h
        src/common.h 
        src/scope.h
        src/mir.h
        src/mir_printer.h
        src/arena.h
        src/ast.h)

set(SOURCE_FILES
        src/main.c
        src/arena.c
        src/tokens.c
        src/file_loader.c
        src/unit.c
        src/ast_printer.c
        src/token.c
        src/token_printer.c
        src/assembly.c
        src/builder.c
        src/lexer.c
        src/bldebug.c
        src/parser.c
        src/linker.c
        src/obj_writer.c
        src/bc_writer.c
	src/mir_writer.c
        src/native_bin.c
        src/scope.c
        src/common.c 
        src/mir.c
        src/ir.c
        src/ir_opt.c
        src/mir_printer.c
        src/ast.c)

add_executable(blc ${SOURCE_FILES} ${HEADER_FILES})

if (MSVC)
    add_definitions(${LLVM_DEFINITIONS})
    llvm_map_components_to_libnames(llvm_libs core all)

    #target_include_directories(blc PRIVATE ${DYNCALL_PATH}/include ${LLVM_INCLUDE_DIRS})
    #target_link_directories(blc PRIVATE ${DYNCALL_PATH}/lib)
    target_link_libraries(blc PUBLIC
        dynload_s
        dyncall_s
	bobject
        shlwapi
    )

    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /wd4996 /MDd")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces -O0 -fsanitize=address -Wdeprecated-declarations")
    target_link_libraries(blc PUBLIC
        dynload_s
        dyncall_s
	bobject
	LLVM
	${CMAKE_DL_LIBS}
    )
endif()

target_include_directories(blc PRIVATE
  src
  deps/dyncall-1.0/dyncall
  deps/dyncall-1.0/dynload
  deps/bobject/include
)

configure_file(${CMAKE_SOURCE_DIR}/src/version.h.in ${CMAKE_SOURCE_DIR}/src/version.h)
