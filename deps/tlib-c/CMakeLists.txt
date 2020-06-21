cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(tlib-c VERSION 1.0.0 LANGUAGES C)

set(HAVE_64_BIT 1)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_VISIBILITY_PRESET hidden)
set(CMAKE_VISIBILITY_INLINES_HIDDEN ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    set(ENABLE_TESTS TRUE)
    set(ENABLE_INSTALLATION TRUE)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
else()
    set(ENABLE_TESTING FALSE)
    set(ENABLE_INSTALLATION FALSE)
endif()

set(DEFAULT_BUILD_TYPE "Release")
if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

set(SOURCE_FILES 
	src/string.c
	src/array.c
	src/hash_table.c
	src/list.c
  )

set(HEADER_FILES
	include/tlib/tlib.h
	include/tlib/common.h
	include/tlib/config.h
	include/tlib/string.h
	include/tlib/array.h
	include/tlib/hash_table.h
	include/tlib/small_array.h
	include/tlib/list.h
	include/tlib/hash.h
  )

if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /Od")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces")
endif()

add_library(tlib STATIC ${SOURCE_FILES} ${HEADER_FILES})

if (${ENABLE_TESTS})
    enable_testing()
    add_subdirectory(tests)
endif()

# Define headers for this library. PUBLIC headers are used for
# compiling the library, and will be added to consumers' build
# paths.
target_include_directories(tlib PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
        PRIVATE src)

if (${ENABLE_INSTALLATION})
    configure_file(src/config.h.in ${CMAKE_SOURCE_DIR}/include/tlib/config.h)
    # Must use GNUInstallDirs to install libraries into correct
    # locations on all platforms.
    include(GNUInstallDirs)

    # 'make install' to the correct locations (provided by GNUInstallDirs).
    install(TARGETS tlib
	    EXPORT TLibConfig
	    ARCHIVE  DESTINATION ${CMAKE_INSTALL_LIBDIR}
	    LIBRARY  DESTINATION ${CMAKE_INSTALL_LIBDIR}
	    RUNTIME  DESTINATION ${CMAKE_INSTALL_BINDIR})  # This is for Windows

    include(CMakePackageConfigHelpers)
    write_basic_package_version_file(
	    ${CMAKE_CURRENT_BINARY_DIR}/TLibConfigVersion.cmake
	    VERSION ${PROJECT_VERSION}
	    COMPATIBILITY AnyNewerVersion)

    install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

    # This makes the project importable from the install directory
    # Put config file in per-project dir (name MUST match), can also
    # just go into 'cmake'.
    install(EXPORT TLibConfig DESTINATION share/tlib/cmake)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/TLibConfigVersion.cmake DESTINATION share/tlib/cmake)

    # This makes the project importable from the build directory
    export(TARGETS tlib FILE TLibConfig.cmake)
endif()
