add_library(bmi_summer SHARED bmi_summer.cpp)

add_executable(bmi_summer_tests bmi_summer_tests.cpp)
target_include_directories(bmi_summer_tests PRIVATE ${IT_CATCH_DIR})
target_link_libraries(bmi_summer_tests tests_main bmi_summer)
