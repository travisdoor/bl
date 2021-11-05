cmake_minimum_required(VERSION 3.3)
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
set(BL_DEPS_DIR ${CMAKE_SOURCE_DIR}/deps)

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-DBL_DEBUG")

if(NOT CMAKE_BUILD_TYPE)
    #message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    #set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

option(BL_ASSERT_ENABLE "Turn on assert checks" OFF)
if(BL_ASSERT_ENABLE)
    add_definitions(-DBL_ASSERT_ENABLE=1)
else()
    add_definitions(-DBL_ASSERT_ENABLE=0)
endif()

option(TRACY_ENABLE "Turn on Tracy profiler." OFF)
set(TRACY_VER "0.7.8" CACHE STRING "Set Tracy profiler version.")

if(TRACY_ENABLE)
    add_definitions(-DTRACY_ENABLE)
    message(STATUS "Tracy profiler enabled.")
endif()

# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
set(PDB_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/bin")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/bin")
set(BL_CONF_FILE ${CMAKE_INSTALL_SYSCONFDIR}/bl.conf)

add_subdirectory(${BL_DEPS_DIR}/dyncall-1.2)

if(UNIX AND NOT APPLE)
    # There is no LLVM v11 on Ubuntu 18.04 by default.
    set(BL_LLVM_VER "10")
else()
    set(BL_LLVM_VER "11.1.0")
endif()

if(MSVC AND NOT LLVM_DIR)
    # DOWNLOAD LLVM
    set(BL_LLVM_DIR ${BL_DEPS_DIR}/llvm-${BL_LLVM_VER}-win64)
    set(BL_LLVM_DIR_ZIP ${BL_DEPS_DIR}/llvm-${BL_LLVM_VER}-win64.zip)
    set(BL_LLVM_REMOTE_ZIP ${BL_DEPS_REMOTE}/llvm-${BL_LLVM_VER}-win64.zip)

    if(NOT EXISTS ${BL_LLVM_DIR})
        if(NOT EXISTS ${BL_LLVM_DIR_ZIP})
            message(STATUS "Downloading LLVM binaries from ${BL_LLVM_REMOTE_ZIP}.")
            file(
                DOWNLOAD ${BL_LLVM_REMOTE_ZIP}
                ${BL_LLVM_DIR_ZIP}
                SHOW_PROGRESS
                EXPECTED_HASH MD5=15a5aee3e0296656de7a917667a8bb94)
        endif()
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar -xf ${BL_LLVM_DIR_ZIP}
            WORKING_DIRECTORY ${BL_DEPS_DIR}
        )
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E rm -f ${BL_LLVM_DIR_ZIP}
            WORKING_DIRECTORY ${BL_DEPS_DIR}
        )
    endif()
    if(CMAKE_CONFIGURATION_TYPES STREQUAL "Debug")
        set(LLVM_DIR "${BL_LLVM_DIR}/debug/lib/cmake/llvm")
    else()
        set(LLVM_DIR "${BL_LLVM_DIR}/release/lib/cmake/llvm")
    endif()
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(LLD_BIN ${BL_DEPS_DIR}/lld.exe)
    set(LLD_ZIP lld-11.0.1-win64-release.zip)
    set(LLD_ZIP_MD5 e9606991f195b22e55cfdafa1213031a)
    set(BL_LLD_EXE "bl-lld.exe")
    set(BL_CONFIG_EXE "bl-config.exe")
    set(BL_CONFIG "${BL_DEPS_DIR}/bl-config/bin/win32/bl-config.exe")
    set(BL_LINKER ${BL_LLD_EXE})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(LLD_BIN ${BL_DEPS_DIR}/lld)
    set(LLD_ZIP lld-13.0.0-linux64-release.zip)
    set(LLD_ZIP_MD5 f19860ad1cb21a385d99d584f0a96638)
    set(BL_LLD_EXE "bl-lld")
    set(BL_CONFIG_EXE "bl-config")
    set(BL_CONFIG "${BL_DEPS_DIR}/bl-config/bin/linux/${BL_CONFIG_EXE}")
    set(BL_LINKER ${BL_LLD_EXE})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(LLD_BIN ${BL_DEPS_DIR}/lld)
    set(LLD_ZIP lld-11.0.0-macos-release.zip)
    set(LLD_ZIP_MD5 b37f7524cf2d12efa4f27268713b45f3)
    set(BL_LLD_EXE "bl-lld")
    set(BL_CONFIG_EXE "bl-config")
    set(BL_CONFIG "${BL_DEPS_DIR}/bl-config/bin/macos/${BL_CONFIG_EXE}")
    set(BL_LINKER ${BL_LLD_EXE})
endif()

set(BL_CONFIGURE_SH ${BL_CONFIG_EXE})

# DOWNLOAD LLD binary
if(NOT EXISTS ${LLD_BIN})
    set(LLD_LOCAL_ZIP ${BL_DEPS_DIR}/${LLD_ZIP})
    set(LLD_REMOTE_ZIP ${BL_DEPS_REMOTE}/${LLD_ZIP})
    if(NOT EXISTS ${LLD_LOCAL_ZIP})
        message(STATUS "Downloading LLD binaries.")
        file(
        DOWNLOAD ${LLD_REMOTE_ZIP} 
        ${LLD_LOCAL_ZIP}
        SHOW_PROGRESS
        EXPECTED_HASH MD5=${LLD_ZIP_MD5})
    endif()
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E tar -xf ${LLD_LOCAL_ZIP}
        WORKING_DIRECTORY ${BL_DEPS_DIR}
    )
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E rm -f ${LLD_LOCAL_ZIP}
        WORKING_DIRECTORY ${BL_DEPS_DIR}
    )
endif()

find_package(LLVM ${BL_LLVM_VER} REQUIRED)

file(GLOB HEADER_FILES src/*.h)
file(GLOB SOURCE_FILES src/*.c)

set(SOURCE_FILES
    ${SOURCE_FILES}
    ${BL_DEPS_DIR}/tracy-${TRACY_VER}/TracyClient.cpp
)

# LLVM
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
add_definitions(${LLVM_DEFINITIONS})
llvm_map_components_to_libnames(LLVM_LIBS core support X86 AArch64 passes)

add_executable(blc ${SOURCE_FILES} ${HEADER_FILES})

if(MSVC)
    add_definitions(-DNOMINMAX)
    target_link_libraries(blc PUBLIC shlwapi DbgHelp)

    # C
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Ox /Oi /wd4996 /MD")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd /W3 /WX /GS")

    # CXX
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /Ox /Oi /wd4996 /MD")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd /W3 /WX /GS")
else()
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(blc PRIVATE Threads::Threads)

    # C
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Wno-missing-braces -Werror -O0")

    # CXX
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG -fno-exceptions -fno-rtti")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-exceptions -fno-rtti -g -Wall -Wextra -O0")

endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Wno-format-overflow -Wno-format-truncation -Wno-address -Wno-unused-value -Wno-type-limits")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -Wno-format-overflow -Wno-format-truncation -Wno-address -Wno-unused-value -Wno-type-limits")
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
            dyncallback_s
            ${CMAKE_DL_LIBS}
            ${LLVM_LIBS}
)

target_include_directories(
    blc
    PRIVATE src
            ${BL_DEPS_DIR}/dyncall-1.2/dyncall
            ${BL_DEPS_DIR}/dyncall-1.2/dynload
            ${BL_DEPS_DIR}/dyncall-1.2/dyncallback
            ${BL_DEPS_DIR}/tracy-${TRACY_VER}
            ${LLVM_INCLUDE_DIRS}
)

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS blc RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

install(DIRECTORY lib/bl/api DESTINATION ${CMAKE_INSTALL_LIBDIR}/bl)
install(DIRECTORY lib/bl/rt DESTINATION ${CMAKE_INSTALL_LIBDIR}/bl)

# Copy LLD executable into output directory
if(NOT EXISTS ${LLD_BIN})
    message(FATAL_ERROR "LLD not found expected location is ${LLD_BIN}" )
endif()
add_custom_command(TARGET blc POST_BUILD
COMMAND ${CMAKE_COMMAND} -E copy ${LLD_BIN} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_LLD_EXE})

# Copy bl-config executable into output directory
if(NOT EXISTS ${BL_CONFIG})
    message(FATAL_ERROR "BL configuration tool not found, expected location is ${BL_CONFIG}" )
endif()
add_custom_command(TARGET blc POST_BUILD
COMMAND ${CMAKE_COMMAND} -E copy ${BL_CONFIG} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_CONFIG_EXE})

install(
    FILES ${BL_CONFIG}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR})
install(
    FILES ${LLD_BIN}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_LLD_EXE}
)

configure_file(${CMAKE_SOURCE_DIR}/src/config.h.in
               ${CMAKE_SOURCE_DIR}/src/config.h)

configure_file(${BL_DEPS_DIR}/bl-config/src/config.bl.in
               ${BL_DEPS_DIR}/bl-config/src/config.bl)
  
# Compile utils
add_custom_target(Utils
    COMMAND 
    ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/bl-config -s && ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/blc --full-path --release-fast --build
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/utils"
    COMMENT "Compile bl utils."
)

add_dependencies(Utils blc)
