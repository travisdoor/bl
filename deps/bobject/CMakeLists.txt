cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(bobject VERSION 2.1.0 LANGUAGES C CXX)

set(HAVE_64_BIT 1)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)

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

#set(HEADER_FILES include/bobject/bobject.h include/bobject/batomic.h)

if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Od")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces")
endif()

# Unit testing
set(ENABLE_TESTS FALSE CACHE PATH "Enable unit testing (GTest required).")
if (ENABLE_TESTS OR CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_subdirectory(tests)
endif()
#add_subdirectory(examples)

add_library(bobject SHARED ${SOURCE_FILES})
target_compile_definitions(bobject PRIVATE -DBO_COMPILING_DLL)

# Define headers for this library. PUBLIC headers are used for
# compiling the library, and will be added to consumers' build
# paths.
target_include_directories(bobject PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
        PRIVATE src)

configure_file(src/config.h.in ${CMAKE_SOURCE_DIR}/include/bobject/config.h)

# 'make install' to the correct locations (provided by GNUInstallDirs).
install(TARGETS bobject
        EXPORT BObjectConfig
        ARCHIVE  DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY  DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR})  # This is for Windows

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
        ${CMAKE_CURRENT_BINARY_DIR}/BObjectConfigVersion.cmake
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY AnyNewerVersion)

install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# This makes the project importable from the install directory
# Put config file in per-project dir (name MUST match), can also
# just go into 'cmake'.
install(EXPORT BObjectConfig DESTINATION share/bobject/cmake)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/BObjectConfigVersion.cmake DESTINATION share/bobject/cmake)

# This makes the project importable from the build directory
export(TARGETS bobject FILE BObjectConfig.cmake)

