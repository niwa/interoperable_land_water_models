add_library(bmit STATIC bmit.cpp target.cpp table.cpp)
target_include_directories(bmit PRIVATE ${YAML_CPP_INCLUDE})
target_include_directories(bmit PRIVATE ${SQLITE_INCLUDE})
target_link_libraries(bmit ${YAML_CPP_LIB})
target_link_libraries(bmit ${SQLITE_LIB})
target_link_libraries(bmit os)

add_executable(bmit_tests bmit_tests.cpp table_tests.cpp)
target_include_directories(bmit_tests PRIVATE ${IT_CATCH_DIR})
target_include_directories(bmit_tests PRIVATE ${IT_TEST_DIR})
target_include_directories(bmit_tests PRIVATE ${SQLITE_INCLUDE})
target_link_libraries(bmit_tests tests_main bmit)
target_link_libraries(bmit_tests ${SQLITE_LIB})

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/os)

configure_file(
	${SQLITE_LIB}
	${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/sqlite3.dll
	COPYONLY
)
