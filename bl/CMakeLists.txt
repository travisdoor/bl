# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

set(HEADER_FILES 
  )

set(SOURCE_FILES 
  src/main.c
  )

add_executable(bl-bin ${SOURCE_FILES})
set_target_properties(bl-bin PROPERTIES OUTPUT_NAME bl)

target_include_directories(bl-bin PRIVATE src)
target_link_libraries(bl-bin bl)

