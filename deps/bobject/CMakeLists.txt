cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(bobject VERSION 2.1.0 LANGUAGES C)

set(HAVE_64_BIT 1)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Must use GNUInstallDirs to install libraries into correct
# locations on all platforms.
include(GNUInstallDirs)

set(DEFAULT_BUILD_TYPE "Release")
if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

set(SOURCE_FILES 
  src/bobject.c
  src/bmemory.c
  src/bdebug.c
  src/containers/array.c
  src/containers/htbl.c
  src/containers/list.c
  src/containers/string.c
  )

if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Od")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces")
endif()

add_library(bobject SHARED ${SOURCE_FILES})
target_compile_definitions(bobject PRIVATE -DBO_COMPILING_DLL)

target_include_directories(bobject PUBLIC
        PRIVATE src include)

install(TARGETS bobject
        RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR})
