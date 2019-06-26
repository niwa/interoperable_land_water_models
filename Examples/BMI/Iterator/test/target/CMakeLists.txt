add_library(bmi_concat SHARED bmi_concat.cpp)

add_executable(bmi_concat_tests bmi_concat_tests.cpp)
target_include_directories(bmi_concat_tests PRIVATE ${IT_CATCH_DIR})
target_link_libraries(bmi_concat_tests tests_main bmi_concat)
