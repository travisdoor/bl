# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(HEADER_FILES 
  )

set(SOURCE_FILES 
  src/main.c
  )

add_executable(blc ${SOURCE_FILES})

target_include_directories(blc PRIVATE src)
target_link_libraries(blc PUBLIC bl)

