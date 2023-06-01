cmake_minimum_required(VERSION 3.3)
project(bl VERSION 0.10.0)

# Must use GNUInstallDirs to install libraries into correct locations on all
# platforms.
include(GNUInstallDirs)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_INCLUDE_HEADERS_IN_COMPILE_COMMANDS ON)
set(HAVE_64_BIT 1)

set(BL_DEPS_REMOTE biscuitlang.org/releases)
set(BL_DEPS_DIR ${CMAKE_SOURCE_DIR}/deps)

set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "" FORCE)

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")
set(CMAKE_CXX_FLAGS_DEBUG "-DBL_DEBUG")

option(BL_ASSERT_ENABLE "Turn on assert checks" OFF)
if(BL_ASSERT_ENABLE)
    add_compile_definitions(BL_ASSERT_ENABLE=1)
else()
    add_compile_definitions(
        $<$<CONFIG:Debug>:BL_ASSERT_ENABLE=1>
        $<$<CONFIG:Release>:BL_ASSERT_ENABLE=0>)
endif()

option(BL_DIRTY_ENABLE "Turn on dirty mode, compiler executable leaves freeing of allocated memory on the operating system." ON)
if(BL_DIRTY_ENABLE)
    add_definitions(-DBL_DIRTY_ENABLE)
    message(STATUS "Dirty memory mode enabled to gain some speed.")
endif()

option(TRACY_ENABLE "Turn on Tracy profiler." OFF)
set(TRACY_VER "0.7.8" CACHE STRING "Set Tracy profiler version.")

if(TRACY_ENABLE)
    add_definitions(-DTRACY_ENABLE)
    add_definitions(-DTRACY_CALLSTACK=8)
    message(STATUS "Tracy profiler enabled.")
endif()

# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
set(PDB_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/bin")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/bin")
set(BL_CONFIG_FILE ${CMAKE_INSTALL_SYSCONFDIR}/bl.yaml)
set(BL_API_DIR "../lib/bl/api") # relative to the executable

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(BL_LLVM_VER "16.0.5")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(BL_LLVM_VER "16.0.4")
elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(BL_LLVM_VER "16.0.0")
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
                EXPECTED_HASH MD5=0ca622e68fdfa6298a4dea038ab45146)
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
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(LLVM_DIR "${BL_LLVM_DIR}/debug/lib/cmake/llvm")
    else()
        set(LLVM_DIR "${BL_LLVM_DIR}/release/lib/cmake/llvm")
    endif()
endif()

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(LLD_BIN ${BL_DEPS_DIR}/lld.exe)
    set(LLD_ZIP lld-13.0.1-win64-release.zip)
    set(LLD_ZIP_MD5 c8dc9a2a72ce71990bfa255570114e0a)
    set(BL_VSWHERE_EXE "vswhere.exe")
    set(BL_VSWHERE "${BL_DEPS_DIR}/vswhere-2.8.4/${BL_VSWHERE_EXE}")
    set(BL_LLD_EXE "bl-lld.exe")
    set(BL_LINKER ${BL_LLD_EXE})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
    set(LLD_BIN ${BL_DEPS_DIR}/lld)
    set(LLD_ZIP lld-13.0.0-linux64-release.zip)
    set(LLD_ZIP_MD5 f19860ad1cb21a385d99d584f0a96638)
    set(BL_LLD_EXE "bl-lld")
    set(BL_LINKER ${BL_LLD_EXE})
endif()

if(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
    set(LLD_BIN ${BL_DEPS_DIR}/lld)
    set(LLD_ZIP lld-11.0.0-macos-release.zip)
    set(LLD_ZIP_MD5 b37f7524cf2d12efa4f27268713b45f3)
    set(BL_LLD_EXE "bl-lld")
    set(BL_LINKER ${BL_LLD_EXE})
endif()

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

add_subdirectory(${BL_DEPS_DIR}/dyncall-1.2)
add_subdirectory(${BL_DEPS_DIR}/libyaml-0.2.5)

if(MSVC)
    add_definitions(-DNOMINMAX)
    target_link_libraries(blc PUBLIC shlwapi DbgHelp)
    set(MSVC_FLAGS /Z7 /Gr /GF /EHsc)
    set(MSVC_RELEASE_FLAGS ${MSVC_FLAGS} /DEBUG /O2 /Oi /Ot /GL)
    set(MSVC_DEBUG_FLAGS  ${MSVC_FLAGS} /Od /MDd)
    target_compile_options(blc PRIVATE
        $<$<CONFIG:Release>: ${MSVC_RELEASE_FLAGS}>
        $<$<CONFIG:Debug>:   ${MSVC_DEBUG_FLAGS}>)
    target_compile_options(dynload_s PRIVATE
        $<$<CONFIG:Release>: ${MSVC_RELEASE_FLAGS}>
        $<$<CONFIG:Debug>:   ${MSVC_DEBUG_FLAGS}>)
    target_compile_options(yaml PRIVATE
        $<$<CONFIG:Release>: ${MSVC_RELEASE_FLAGS} /wd4996 /wd4267 /wd4244>
        $<$<CONFIG:Debug>:   ${MSVC_DEBUG_FLAGS} /wd4996 /wd4267 /wd4244>)
else()
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(blc PRIVATE Threads::Threads)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0")
endif()

target_link_libraries(
    blc
    PRIVATE dynload_s
            dyncall_s
            dyncallback_s
            yaml
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
            ${BL_DEPS_DIR}/libyaml-0.2.5/include
            ${LLVM_INCLUDE_DIRS}
)

set_target_properties(dyncall_s dyncallback_s dynload_s PROPERTIES FOLDER dyncall)
set_target_properties(acc_gen intrinsics_gen omp_gen PROPERTIES FOLDER LLVM)
set_target_properties(yaml PROPERTIES FOLDER yaml)

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

if(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    add_custom_command(TARGET blc POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${BL_VSWHERE} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${BL_VSWHERE_EXE})
endif()

install(
    FILES ${LLD_BIN}
    PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME ${BL_LLD_EXE}
)

configure_file(${CMAKE_SOURCE_DIR}/src/config.h.in ${CMAKE_SOURCE_DIR}/src/config.h)

