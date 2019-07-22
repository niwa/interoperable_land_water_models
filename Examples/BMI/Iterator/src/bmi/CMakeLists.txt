add_library(bmi_iterator SHARED bmi_iterator.cpp)
target_include_directories(bmi_iterator PRIVATE ${IT_LIB_DIR})
target_link_libraries(bmi_iterator bmit)

add_executable(bmi_iterator_tests bmi_iterator_tests.cpp)
target_include_directories(bmi_iterator_tests PRIVATE ${IT_CATCH_DIR})
target_include_directories(bmi_iterator_tests PRIVATE ${IT_TEST_DIR})
target_include_directories(bmi_iterator_tests PRIVATE ${SQLITE_INCLUDE})
target_link_libraries(bmi_iterator_tests tests_main bmi_iterator)
target_link_libraries(bmi_iterator_tests ${SQLITE_IMPORT_LIB})

add_executable(bmi_it_runner runner.cpp)
target_link_libraries(bmi_it_runner bmi_iterator)
