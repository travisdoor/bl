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

get_filename_component(LINKER_FILENAME ${CMAKE_LINKER} NAME)

if (LINKER_FILENAME STREQUAL link.exe)
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake")
    find_package(WindowsSDK)

    set(BL_WINSDK_LIB_DIR "${WINDOWSSDK_LATEST_DIR}/Lib/${CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION}")
    set(BL_LINKER_EXEC ${CMAKE_LINKER})
    set(BL_LINKER_OPT "\
                       /NOLOGO /INCREMENTAL:NO /MACHINE:x64 \
                       /LIBPATH:'${BL_WINSDK_LIB_DIR}/um/x64' \ 
                       /LIBPATH:'${BL_WINSDK_LIB_DIR}/ucrt/x64'")
elseif (LINKER_FILENAME STREQUAL ld)
  if (APPLE)
        execute_process(COMMAND sw_vers -productVersion OUTPUT_VARIABLE BL_MIN_MACOS_VERSION)
	set(BL_LINKER_EXEC "/Library/Developer/CommandLineTools/usr/bin/ld")
	set(BL_LINKER_OPT
	  "${CMAKE_OSX_SYSROOT}/usr/lib/crt1.o -lc"
	)
    else()
        set(BL_LINKER_EXEC "/usr/bin/ld")
	set(BL_LINKER_OPT 
	    "--hash-style=gnu --no-add-needed --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/x86_64-linux-gnu/crt1.o /usr/lib/x86_64-linux-gnu/crti.o /usr/lib/x86_64-linux-gnu/crtn.o -L/usr/bin -L/usr/lib/x86_64-linux-gnu -lc -lm")
    endif()
endif()

set(BL_LIBDIR ${CMAKE_SOURCE_DIR}/api)
configure_file(${CMAKE_SOURCE_DIR}/install/bl.conf.in ${CMAKE_SOURCE_DIR}/etc/bl.conf)

set(BL_LIBDIR ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/bl/api)
configure_file(${CMAKE_SOURCE_DIR}/install/bl.conf.in ${CMAKE_BINARY_DIR}/bl.conf)

configure_file(${CMAKE_SOURCE_DIR}/src/config.h.in ${CMAKE_SOURCE_DIR}/src/config.h)

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS blc
        RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR})

install(DIRECTORY api/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/bl/api)
install(FILES ${CMAKE_BINARY_DIR}/bl.conf DESTINATION ${CMAKE_INSTALL_SYSCONFDIR})
