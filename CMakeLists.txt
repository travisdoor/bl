cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.2.0 LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
SET(LLVM_PATH "" CACHE STRING "Path to LLVM.")

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")

if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

find_package(BObject 1.0 REQUIRED)


if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /wd4996 /MT")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MTd")

    # Link LLVM from custom path
    include_directories(${LLVM_PATH}/include)
    link_directories(${LLVM_PATH}/lib)
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Werror -Wall -Wno-missing-braces -O0")
endif()

add_subdirectory(libbl)
add_subdirectory(blc)
# add_subdirectory(tests)

