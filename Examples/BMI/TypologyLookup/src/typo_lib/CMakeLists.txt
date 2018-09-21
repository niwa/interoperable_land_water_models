add_library(typo_lib SHARED typo.cpp)
set_target_properties(typo_lib PROPERTIES OUTPUT_NAME typo)
target_include_directories(typo_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
