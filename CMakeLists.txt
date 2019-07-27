cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.4.1 LANGUAGES C)

# Must use GNUInstallDirs to install libraries into correct
# locations on all platforms.
include(GNUInstallDirs)

set(CMAKE_C_STANDARD 99)
set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")

if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(BL_CONF_FILE ${CMAKE_INSTALL_SYSCONFDIR}/bl.conf)

add_subdirectory(deps/bobject)
add_subdirectory(deps/dyncall-1.0)

# deps
find_package(LLVM REQUIRED CONFIG)

set(HEADER_FILES
        src/conf_data.h
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
        src/conf_parser.c
        src/conf_data.c
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

add_definitions(${LLVM_DEFINITIONS})

if (MSVC)
    llvm_map_components_to_libnames(LLVM_LIBS core all)

    target_link_libraries(blc PUBLIC
        shlwapi
	${LLVM_LIBS}
	dynload_s
	dyncall_s
	bobject
	${CMAKE_DL_LIBS}
    )

    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /wd4996 /MDd")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd")
else()
    target_link_libraries(blc PUBLIC
        LLVM
	dynload_s
	dyncall_s
	bobject
	${CMAKE_DL_LIBS}
    )

    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces -O0 -fsanitize=address -Wdeprecated-declarations")
endif()

target_include_directories(blc PRIVATE
    src
    deps/dyncall-1.0/dyncall
    deps/dyncall-1.0/dynload
    deps/bobject/include
    ${LLVM_INCLUDE_DIRS}
)

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS blc
        RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR})

install(DIRECTORY lib/bl/api DESTINATION ${CMAKE_INSTALL_LIBDIR}/bl)

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(BL_CONFIGURE_SH "blc_configure.sh")
    install(FILES ${CMAKE_SOURCE_DIR}/install/configure_linux.sh DESTINATION ${CMAKE_INSTALL_BINDIR} RENAME ${BL_CONFIGURE_SH})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(BL_CONFIGURE_SH "blc_configure.sh")
    install(FILES ${CMAKE_SOURCE_DIR}/install/configure_macos.sh DESTINATION ${CMAKE_INSTALL_BINDIR} RENAME ${BL_CONFIGURE_SH})
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(BL_CONFIGURE_SH "blc_configure.bat")
endif()

configure_file(${CMAKE_SOURCE_DIR}/src/config.h.in ${CMAKE_SOURCE_DIR}/src/config.h)
