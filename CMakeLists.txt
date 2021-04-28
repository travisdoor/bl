cmake_minimum_required(VERSION 3.0)
project(bl VERSION 1.0.0)

# Must use GNUInstallDirs to install libraries into correct locations on all
# platforms.
include(GNUInstallDirs)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(BL_DEPS_REMOTE biscuitlang.org/releases)

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

option(BL_ASSERT_ENABLE "Turn on assert checks" OFF)
if(BL_ASSERT_ENABLE)
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
  set(BL_LLVM_VER "11.1.0")
  if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(LLVM_BIN_DEBUG ${CMAKE_CURRENT_BINARY_DIR}/llvm-debug)
    set(LLVM_BIN_DEBUG_ZIP
        ${CMAKE_CURRENT_BINARY_DIR}/llvm-${BL_LLVM_VER}-win64-debug.zip)

    if(NOT EXISTS ${LLVM_BIN_DEBUG})
      if(NOT EXISTS ${LLVM_BIN_DEBUG_ZIP})
        message(STATUS "Downloading LLVM binaries for Debug build.")
        file(
          DOWNLOAD ${BL_DEPS_REMOTE}/llvm-${BL_LLVM_VER}-win64-debug.zip
          ${LLVM_BIN_DEBUG_ZIP}
          SHOW_PROGRESS
          EXPECTED_HASH MD5=7a4888c51fe7f7140928b43680c363cc)
      endif()

      execute_process(COMMAND ${CMAKE_COMMAND} -E tar -xf ${LLVM_BIN_DEBUG_ZIP}
                      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()

    set(LLVM_DIR "${LLVM_BIN_DEBUG}/lib/cmake/llvm")
  else()
    set(LLVM_BIN_RELEASE ${CMAKE_CURRENT_BINARY_DIR}/llvm-release)
    set(LLVM_BIN_RELEASE_ZIP
        ${CMAKE_CURRENT_BINARY_DIR}/llvm-${BL_LLVM_VER}-win64-release.zip)

    if(NOT EXISTS ${LLVM_BIN_RELEASE})
      if(NOT EXISTS ${LLVM_BIN_RELEASE_ZIP})
        message(STATUS "Downloading LLVM binaries for Release build.")
        file(
          DOWNLOAD ${BL_DEPS_REMOTE}/llvm-${BL_LLVM_VER}-win64-release.zip
          ${LLVM_BIN_RELEASE_ZIP}
          SHOW_PROGRESS
          EXPECTED_HASH MD5=85f65d2fa3278b53262f99d570108c58)
      endif()

      execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xf ${LLVM_BIN_RELEASE_ZIP}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()

    set(LLVM_DIR "${LLVM_BIN_RELEASE}/lib/cmake/llvm")
  endif()
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  set(LLD_BIN ${CMAKE_CURRENT_BINARY_DIR}/lld.exe)
  set(LLD_ZIP lld-11.0.1-win64-release.zip)
  set(LLD_ZIP_MD5 e9606991f195b22e55cfdafa1213031a)
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  set(LLD_BIN ${CMAKE_CURRENT_BINARY_DIR}/lld)
  set(LLD_ZIP lld-13.0.0-linux64-release.zip)
  set(LLD_ZIP_MD5 f19860ad1cb21a385d99d584f0a96638)
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
  set(LLD_BIN ${CMAKE_CURRENT_BINARY_DIR}/lld)
  set(LLD_ZIP lld-11.0.0-macos-release.zip)
  set(LLD_ZIP_MD5 b37f7524cf2d12efa4f27268713b45f3)
endif()

# DOWNLOAD LLD binary
if(NOT EXISTS ${LLD_BIN})
  set(LLD_LOCAL_ZIP ${CMAKE_CURRENT_BINARY_DIR}/${LLD_ZIP})
  set(LLD_REMOTE_ZIP ${BL_DEPS_REMOTE}/${LLD_ZIP})
  if(NOT EXISTS ${LLD_LOCAL_ZIP})
    message(STATUS "Downloading LLD binaries.")
    file(
      DOWNLOAD ${LLD_REMOTE_ZIP} 
      ${LLD_LOCAL_ZIP}
      SHOW_PROGRESS
      EXPECTED_HASH MD5=${LLD_ZIP_MD5})
  endif()
  execute_process(COMMAND ${CMAKE_COMMAND} -E tar -xf ${LLD_LOCAL_ZIP}
                  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endif()

if(UNIX AND NOT APPLE)
  # There is no LLVM v11 on Ubuntu 18.04 by default.
  find_package(LLVM 10 REQUIRED)
else()
  find_package(LLVM 11.0 REQUIRED)
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set(HEADER_FILES
    src/arena.h
    src/assembly.h
    src/ast.h
    src/bldebug.h
    src/blmemory.h
    src/builder.h
    src/common.h
    src/conf_data.h
    src/config.h
    src/error.h
    src/llvm_api.h
    src/llvm_di.h
    src/mir.h
    src/mir_printer.h
    src/scope.h
    src/token.h
    src/tokens.h
    src/unit.h
    src/vm.h
)

set(SOURCE_FILES
    deps/tracy/TracyClient.cpp
    src/arena.c
    src/assembly.c
    src/ast.c
    src/ast_printer.c
    src/bc_writer.c
    src/bldebug.c
    src/blmemory.c
    src/build_api.c
    src/builder.c
    src/common.c
    src/conf_data.c
    src/conf_parser.c
    src/docs.c
    src/file_loader.c
    src/intrinsic.c
    src/ir.c
    src/ir_opt.c
    src/lexer.c
    src/linker.c
    src/lld_ld.c
    src/lld_link.c
    src/llvm_api.cpp
    src/llvm_di.cpp
    src/main.c
    src/mir.c
    src/mir_printer.c
    src/mir_writer.c
    src/native_bin.c
    src/obj_writer.c
    src/parser.c
    src/scope.c
    src/token.c
    src/token_printer.c
    src/tokens.c
    src/unit.c
    src/vm.c
    src/vm_runner.c
)

add_definitions(${LLVM_DEFINITIONS})
add_executable(blc ${SOURCE_FILES} ${HEADER_FILES})

llvm_map_components_to_libnames(LLVM_LIBS core support X86 passes)

if(MSVC)
  target_link_libraries(blc PUBLIC shlwapi DbgHelp)

  # C
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /wd4996 /MD /Oi")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd /W3 /WX /GS")

  # CXX
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2 /wd4996 /MD /Oi")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd /W3 /WX /GS")
else()
  set(THREADS_PREFER_PTHREAD_FLAG ON)
  find_package(Threads REQUIRED)
  target_link_libraries(blc PRIVATE Threads::Threads)

  # C
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Wno-missing-braces -Werror -O0"
  )

  # CXX
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG -fno-strict-aliasing -fno-exceptions -fno-rtti"
  )
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-strict-aliasing -fno-exceptions -fno-rtti -g -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Wno-missing-braces -O0"
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
  # Copy LLD executable into output directory
  set(BL_LLD_EXE "bl-lld")
  if(NOT EXISTS ${LLD_BIN})
    message(FATAL_ERROR "LLD not found expected location is ${LLD_BIN}" )
  endif()
  add_custom_command(TARGET blc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${LLD_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_LLD_EXE}
  )

  # Copy bl-config executable into output directory
  set(BL_CONFIG_EXE "bl-config")
  set(BL_CONFIG "${CMAKE_SOURCE_DIR}/deps/bl-config/bin/linux/${BL_CONFIG_EXE}")
  if(NOT EXISTS ${BL_CONFIG})
    message(FATAL_ERROR "BL configuration tool not found expected location is ${BL_CONFIG}" )
  endif()
  add_custom_command(TARGET blc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${BL_CONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_CONFIG_EXE}
  )

  set(BL_CONFIGURE_SH ${BL_CONFIG_EXE})
  set(BL_LINKER ${BL_LLD_EXE})
  
  install(
    FILES ${BL_CONFIG}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(
    FILES ${LLD_BIN}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_LLD_EXE})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
  # Copy LLD executable into output directory
  set(BL_LLD_EXE "bl-lld")
  if(NOT EXISTS ${LLD_BIN})
    message(FATAL_ERROR "LLD not found expected location is ${LLD_BIN}" )
  endif()
  add_custom_command(TARGET blc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${LLD_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_LLD_EXE}
  )

  # Copy bl-config executable into output directory
  set(BL_CONFIG_EXE "bl-config")
  set(BL_CONFIG "${CMAKE_SOURCE_DIR}/deps/bl-config/bin/macos/${BL_CONFIG_EXE}")
  if(NOT EXISTS ${BL_CONFIG})
    message(FATAL_ERROR "BL configuration tool not found expected location is ${BL_CONFIG}" )
  endif()
  add_custom_command(TARGET blc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${BL_CONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_CONFIG_EXE}
  )

  set(BL_CONFIGURE_SH ${BL_CONFIG_EXE})
  set(BL_LINKER ${BL_LLD_EXE})
  
  install(
    FILES ${BL_CONFIG}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
  install(
    FILES ${LLD_BIN}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_LLD_EXE})
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  # Copy LLD executable into output directory
  set(BL_LLD_EXE "bl-lld.exe")
  if(NOT EXISTS ${LLD_BIN})
    message(FATAL_ERROR "LLD not found expected location is ${LLD_BIN}" )
  endif()
  add_custom_command(TARGET blc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${LLD_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_LLD_EXE}
  )

  # Copy bl-config executable into output directory
  set(BL_CONFIG_EXE "bl-config.exe")
  set(BL_CONFIG "${CMAKE_SOURCE_DIR}/deps/bl-config/bin/win32/bl-config.exe")
  if(NOT EXISTS ${BL_CONFIG})
    message(FATAL_ERROR "BL configuration tool not found expected location is ${BL_CONFIG}" )
  endif()
  add_custom_command(TARGET blc POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${BL_CONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_CONFIG_EXE}
  )

  set(BL_CONFIGURE_SH ${BL_CONFIG_EXE})
  set(BL_LINKER ${BL_LLD_EXE})
  
  install(
    FILES ${BL_CONFIG}
    DESTINATION ${CMAKE_INSTALL_BINDIR})
  install(
    FILES ${LLD_BIN}
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_LLD_EXE})
endif()

configure_file(${CMAKE_SOURCE_DIR}/src/config.h.in
               ${CMAKE_SOURCE_DIR}/src/config.h)

configure_file(${CMAKE_SOURCE_DIR}/deps/bl-config/src/config.bl.in
               ${CMAKE_SOURCE_DIR}/deps/bl-config/src/config.bl)
  
# Compile utils
add_custom_target(Utils
  COMMAND 
    ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/bl-config -s && ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/blc --full-path --release-fast --build
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/utils"
  COMMENT "Compile bl utils."
)

add_dependencies(Utils blc)
