add_library(bmi_lookup SHARED bmi_lookup.cpp)
target_include_directories(bmi_lookup PRIVATE ${LU_LIB_DIR})
target_link_libraries(bmi_lookup lookup)

add_executable(bmi_lookup_tests bmi_lookup_tests.cpp)
target_include_directories(bmi_lookup_tests PRIVATE ${LU_CATCH_DIR})
target_link_libraries(bmi_lookup_tests tests_main bmi_lookup)

add_custom_command(TARGET bmi_lookup_tests POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${LU_DATA_DIR}/typologies.yml
        ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/typologies.yml
)
