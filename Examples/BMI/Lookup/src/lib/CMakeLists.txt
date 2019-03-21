add_library(lookup STATIC lookup.cpp)
target_include_directories(lookup PRIVATE ${YAML_CPP_INCLUDE})
target_link_libraries(lookup ${YAML_CPP_LIB})

add_executable(lookup_tests lookup_tests.cpp)
target_include_directories(lookup_tests PRIVATE ${LU_CATCH_DIR})
target_link_libraries(lookup_tests tests_main lookup)
