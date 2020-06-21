file(GLOB files "test_*.c")

foreach(file ${files})
	string(REGEX REPLACE "(^.*/|\\.[^.]*$)" "" file_without_ext ${file})
	add_executable(${file_without_ext} ${file})
	target_link_libraries(${file_without_ext} tlib)
	add_test(
	  NAME ${file_without_ext}
	  COMMAND ${file_without_ext})
endforeach()
