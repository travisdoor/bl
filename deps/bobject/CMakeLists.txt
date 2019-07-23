cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(bobject VERSION 2.1.0 LANGUAGES C)

set(HAVE_64_BIT 1)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

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
