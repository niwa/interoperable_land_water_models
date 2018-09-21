add_library(typo_bmi_lib SHARED bmi_typo.cpp)
set_target_properties(typo_bmi_lib PROPERTIES OUTPUT_NAME typo_bmi)
target_include_directories(typo_bmi_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# Linking to the typo library
target_include_directories(typo_bmi_lib PRIVATE ${TYPO_LIB_DIR})
target_link_libraries(typo_bmi_lib PRIVATE typo_lib)
