# Package: dyncall
# File: CMakeLists.txt
# Description: DynCall Project cmake files
# License:
#
# Copyright (c) 2010-2020 Daniel Adler <dadler@uni-goettingen.de>
#                    2020 armasm64 support: Bernhard Urban-Forster <beurba@microsoft.com>
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#

cmake_minimum_required (VERSION 2.6)
project(DynCall C)

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "dyncall enables dynamic invocation of machine-level function calls")
set(CPACK_PACKAGE_VENDOR "dyncall project")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
if(APPLE)
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README")
endif(APPLE)
set(CPACK_PACKAGE_NAME "dyncall")
include(CPack)

# add cmake modules shipped with the package to cmake's module path.
# currently we have no use for this, but maybe later..
#set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/buildsys/cmake/Modules" "${CMAKE_MODULE_PATH}")

option(LANG_CXX "Enable and build C++ tests" ON)

if(LANG_CXX)
  enable_language(CXX)
endif()

include(CheckCCompilerFlag)
check_c_compiler_flag("-fPIC -Werror" COMPILER_HAS_FPIC)


# compile_asm(TARGET target ASM_FILES file1 [file2 ...] OUTPUT_OBJECTS [variableName])
# CMake does not support the ARM or ARM64 assemblers on Windows when using the
# MSBuild generator.
# credit to dotnet/runtime:
#    https://github.com/dotnet/runtime/blob/e98fb61d8b4bb6687ccddead861d8b140751647b/eng/native/functions.cmake#L173-L207
function(compile_asm)
  set(options "")
  set(oneValueArgs TARGET OUTPUT_OBJECTS)
  set(multiValueArgs ASM_FILES)
  cmake_parse_arguments(COMPILE_ASM "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGV})

  set (ASSEMBLED_OBJECTS "")

  foreach(ASM_FILE ${COMPILE_ASM_ASM_FILES})
    get_filename_component(name ${ASM_FILE} NAME_WE)
    # Produce object file where CMake would store .obj files for an OBJECT library.
    # ex: artifacts\obj\coreclr\windows.arm64.Debug\src\vm\wks\cee_wks.dir\Debug\AsmHelpers.obj
    set (OBJ_FILE "${CMAKE_CURRENT_BINARY_DIR}/${COMPILE_ASM_TARGET}.dir/${CMAKE_CFG_INTDIR}/${name}.obj")

    # Need to compile asm file using custom command as include directories are not provided to asm compiler
    add_custom_command(OUTPUT ${OBJ_FILE}
                        COMMAND "${CMAKE_ASM_COMPILER}" ${ASM_INCLUDE_DIRECTORIES} -o ${OBJ_FILE} ${ASM_FILE}
                        DEPENDS ${ASM_FILE}
                        COMMENT "Assembling ${ASM_FILE} ---> \"${CMAKE_ASM_COMPILER}\" ${ASM_INCLUDE_DIRECTORIES} -o ${OBJ_FILE} ${ASM_FILE}"+                        COMMENT "Assembling ${ASM_FILE} ---> \"${CMAKE_ASM_COMPILER}\" ${ASM_INCLUDE_DIRECTORIES} -o ${OBJ_FILE} ${ASM_FILE}")

    # mark obj as source that does not require compile
    set_source_files_properties(${OBJ_FILE} PROPERTIES EXTERNAL_OBJECT TRUE)

    # Add the generated OBJ in the dependency list so that it gets consumed during linkage
    list(APPEND ASSEMBLED_OBJECTS ${OBJ_FILE})
  endforeach()

  set(${COMPILE_ASM_OUTPUT_OBJECTS} ${ASSEMBLED_OBJECTS} PARENT_SCOPE)
endfunction()


if(MSVC)
  if("${CMAKE_GENERATOR_PLATFORM}" MATCHES "ARM64")
    # Explicitly specify the assembler to be used for Arm64 compile
    file(TO_CMAKE_PATH "$ENV{VCToolsInstallDir}\\bin\\HostX64\\arm64\\armasm64.exe" CMAKE_ASM_COMPILER)
    # file(TO_CMAKE_PATH "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise\\VC\\Tools\\MSVC\\14.28.29333\\bin\\HostX64\\arm64\\armasm64.exe" CMAKE_ASM_COMPILER)

    set(CMAKE_ASM_MASM_COMPILER ${CMAKE_ASM_COMPILER})
    message("CMAKE_ASM_MASM_COMPILER explicitly set to: ${CMAKE_ASM_MASM_COMPILER}")
    # Enable generic assembly compilation to avoid CMake generate VS proj files that explicitly
    # use ml[64].exe as the assembler.
    enable_language(ASM)
    set(CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreaded         "")
    set(CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDLL      "")
    set(CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDebug    "")
    set(CMAKE_ASM_COMPILE_OPTIONS_MSVC_RUNTIME_LIBRARY_MultiThreadedDebugDLL "")
    set(CMAKE_ASM_COMPILE_OBJECT "<CMAKE_ASM_COMPILER> <INCLUDES> <FLAGS> -o <OBJECT> <SOURCE>")
  else()
    enable_language(ASM_MASM)

    #if(CMAKE_SIZEOF_VOID_P MATCHES 8)
    #  set(CMAKE_ASM_COMPILER "ml64")
    #else()
    #  set(CMAKE_ASM_COMPILER "ml")
    #endif()
    #set(CMAKE_ASM_COMPILER_ARG1 "/c")
    #set(CMAKE_ASM_MASM_SOURCE_FILE_EXTENSIONS asm)
    #set(CMAKE_ASM_MASM_COMPILE_OBJECT "<CMAKE_ASM_MASM_COMPILER> <FLAGS> /c /Fo <OBJECT> <SOURCE>")
  endif()
elseif(CMAKE_COMPILER_IS_GNUCC OR "${CMAKE_C_COMPILER_ID}" STREQUAL "Intel")
  if(COMPILER_HAS_FPIC)
    # when used in shared libraries, -fPIC is required by several architectures
    # and platforms (observed on AMD64, Solaris/Sparc).
    # we enable it per default here.
    add_definitions("-fPIC")
  endif()
  # enable gcc as front-end to assembler for .S files
  set(CMAKE_ASM_COMPILER "${CMAKE_C_COMPILER}")
  set(CMAKE_ASM_COMPILER_ARG1 "${CPPFLAGS} -c")
  enable_language(ASM)
elseif(CMAKE_COMPILER_IS_CLANG)
elseif("${CMAKE_C_COMPILER_ID}" STREQUAL "SunPro")
  set(CMAKE_ASM_COMPILER "${CMAKE_C_COMPILER}")
  set(CMAKE_ASM_COMPILER_ARG1 "${CPPFLAGS} -c")
  enable_language(ASM)
else()
  if (CMAKE_C_COMPILER_ID STREQUAL "Clang")
  if(COMPILER_HAS_FPIC)
    add_definitions("-fPIC")
  endif()
  # enable gcc as front-end to assembler for .S files
  set(CMAKE_ASM_COMPILER "${CMAKE_C_COMPILER}")
  set(CMAKE_ASM_COMPILER_ARG1 "${CPPFLAGS} -c")
  enable_language(ASM)
  endif()
endif()

add_subdirectory(dynload)
add_subdirectory(dyncall)
add_subdirectory(dyncallback)
add_subdirectory(doc/manual EXCLUDE_FROM_ALL)
add_subdirectory(test EXCLUDE_FROM_ALL)

# install cmake Find scripts (disabled per default)

option(INSTALL_CMAKE_MODULES "install cmake modules to locate dyncall" "NO")

if(INSTALL_CMAKE_MODULES)
file(GLOB INSTALL_CMAKE_MODULES_FILES buildsys/cmake/Modules/Find*.cmake)
install(FILES ${INSTALL_CMAKE_MODULES_FILES} DESTINATION ${CMAKE_ROOT}/Modules)
endif(INSTALL_CMAKE_MODULES)

