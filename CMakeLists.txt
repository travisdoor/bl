cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.9.0)

# Must use GNUInstallDirs to install libraries into correct locations on all
# platforms.
include(GNUInstallDirs)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-DBL_DEBUG")

if(NOT CMAKE_BUILD_TYPE)
  message(
    STATUS
      "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
  set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

option(ASSERT_ENABLE "Turn on assert checks" OFF)
if(ENABLE_ASSERT)
  add_definitions(-DBL_ASSERT_ENABLE=1)
else()
  add_definitions(-DBL_ASSERT_ENABLE=0)
endif()

option(TRACY_ENABLE "Turn on Tracy profiler" OFF)
if(TRACY_ENABLE)
  add_definitions(-DTRACY_ENABLE)
  # add_definitions(-DTRACY_NO_EXIT)
endif()

# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/bin")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/bin")
set(BL_CONF_FILE ${CMAKE_INSTALL_SYSCONFDIR}/bl.conf)

add_subdirectory(deps/dyncall-1.0)
add_subdirectory(deps/tlib-c)

# deps
if(MSVC)
  # DOWNLOAD LLVM
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(LLVM_BIN_DEBUG ${CMAKE_CURRENT_BINARY_DIR}/llvm-debug)
    set(LLVM_BIN_DEBUG_ZIP
        ${CMAKE_CURRENT_BINARY_DIR}/llvm-11.0.1-Win64-debug.zip)

    if(NOT EXISTS ${LLVM_BIN_DEBUG})
      if(NOT EXISTS ${LLVM_BIN_DEBUG_ZIP})
        message(STATUS "Downloading LLVM binaries for Debug build.")
        file(
          DOWNLOAD biscuitlang.org/releases/llvm-11.0.1-Win64-debug.zip
          ${LLVM_BIN_DEBUG_ZIP}
          SHOW_PROGRESS
          EXPECTED_HASH MD5=71adf8bf8b205dfb8db6381ec041bfb9)
      endif()

      execute_process(COMMAND ${CMAKE_COMMAND} -E tar -xf ${LLVM_BIN_DEBUG_ZIP}
                      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()

    set(LLVM_DIR "${LLVM_BIN_DEBUG}/lib/cmake/llvm")
  else()
    set(LLVM_BIN_RELEASE ${CMAKE_CURRENT_BINARY_DIR}/llvm-release)
    set(LLVM_BIN_RELEASE_ZIP
        ${CMAKE_CURRENT_BINARY_DIR}/llvm-11.0.1-Win64-release.zip)

    if(NOT EXISTS ${LLVM_BIN_RELEASE})
      if(NOT EXISTS ${LLVM_BIN_RELEASE_ZIP})
        message(STATUS "Downloading LLVM binaries for Release build.")
        file(
          DOWNLOAD biscuitlang.org/releases/llvm-11.0.1-Win64-release.zip
          ${LLVM_BIN_RELEASE_ZIP}
          SHOW_PROGRESS
          EXPECTED_HASH MD5=764ede9bdd1dff8e9e97ca536b4981e2)
      endif()

      execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xf ${LLVM_BIN_RELEASE_ZIP}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()

    set(LLVM_DIR "${LLVM_BIN_RELEASE}/lib/cmake/llvm")
  endif()
endif()

if(UNIX AND NOT APPLE)
  # There is no LLVM v11 on Ubuntu 18.04 by default.
  find_package(LLVM 10 REQUIRED)
else()
  find_package(LLVM 11 REQUIRED)
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set(HEADER_FILES
    src/conf_data.h
    src/bldebug.h
    src/error.h
    src/config.h
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
    src/llvm_di.h
    src/vm.h
    src/llvm_api.h
    src/ast.h)

set(SOURCE_FILES
    src/main.c
    src/blmemory.c
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
    src/vm.c
    src/ir_opt.c
    src/mir_printer.c
    src/ast.c
    src/build_api.c
    src/intrinsic.c
    src/docs.c
    src/vm_runner.c
    src/link.c
    src/llvm_api.cpp
    src/llvm_di.cpp
    deps/tracy/TracyClient.cpp)

add_definitions(${LLVM_DEFINITIONS})
add_executable(blc ${SOURCE_FILES} ${HEADER_FILES})

llvm_map_components_to_libnames(LLVM_LIBS core support X86 passes)

if(MSVC)
  target_link_libraries(blc PUBLIC shlwapi DbgHelp)

  # C
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /wd4996 /MD /Oi")
  set(CMAKE_C_FLAGS_DEBUG
      "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd /W4 /WX /GS")

  # CXX
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2 /wd4996 /MD /Oi")
  set(CMAKE_CXX_FLAGS_DEBUG
      "${CMAKE_CXX_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd /W4 /WX /GS")
else()
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_package(Threads REQUIRED)
  target_link_libraries(blc PRIVATE Threads::Threads)

  # C
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
  set(CMAKE_C_FLAGS_DEBUG
      "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Wno-missing-braces -Werror -O0"
  )

  # CXX
  set(CMAKE_CXX_FLAGS_RELEASE
      "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG -fno-strict-aliasing -fno-exceptions -fno-rtti"
  )
  set(CMAKE_CXX_FLAGS_DEBUG
      "${CMAKE_CXX_FLAGS_DEBUG} -fno-strict-aliasing -fno-exceptions -fno-rtti -g -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Wno-missing-braces -O0"
  )
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wno-format-overflow -Wno-format-truncation")
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wno-format-overflow -Wno-format-truncation")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wno-format-overflow -Wno-format-truncation")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wno-format-overflow -Wno-format-truncation")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wno-nullability-completeness -Wno-unused-local-typedef")
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wno-nullability-completeness -Wno-unused-local-typedef")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wno-nullability-completeness -Wno-unused-local-typedef")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wno-nullability-completeness -Wno-unused-local-typedef")
endif()

target_link_libraries(
  blc
  PRIVATE dynload_s
          dyncall_s
          dyncallback_s tlib
          ${CMAKE_DL_LIBS}
          #LLVM) #use only LLVM to link dynamically
          ${LLVM_LIBS})

target_include_directories(
  blc
  PRIVATE src
          deps/dyncall-1.0/dyncall
          deps/dyncall-1.0/dynload
          deps/dyncall-1.0/dyncallback
          deps/tlib-c/include
          deps/tracy
          ${LLVM_INCLUDE_DIRS})

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS blc RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(DIRECTORY lib/bl/api DESTINATION ${CMAKE_INSTALL_LIBDIR}/bl)
install(DIRECTORY lib/bl/rt DESTINATION ${CMAKE_INSTALL_LIBDIR}/bl)

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  set(BL_CONFIGURE_SH "blc_configure.sh")
  install(
    FILES ${CMAKE_SOURCE_DIR}/install/configure_linux.sh
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_CONFIGURE_SH})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
  set(BL_CONFIGURE_SH "blc_configure.sh")
  install(
    FILES ${CMAKE_SOURCE_DIR}/install/configure_macos.sh
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_CONFIGURE_SH})
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  set(BL_CONFIGURE_SH "blc_configure.bat")
  install(
    FILES ${CMAKE_SOURCE_DIR}/install/configure_win64.bat
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_CONFIGURE_SH})
endif()

configure_file(${CMAKE_SOURCE_DIR}/src/config.h.in
               ${CMAKE_SOURCE_DIR}/src/config.h)
