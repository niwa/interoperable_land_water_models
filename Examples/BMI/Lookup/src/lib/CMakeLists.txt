add_library(lookup STATIC lookup.cpp)
target_include_directories(lookup PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/yaml-cpp/include)
target_link_libraries(lookup ${CMAKE_CURRENT_SOURCE_DIR}/yaml-cpp/libyaml-cpp.a)

add_executable(lookup_tests lookup_tests.cpp)
target_include_directories(lookup_tests PRIVATE ${LU_CATCH_DIR})
target_link_libraries(lookup_tests tests_main lookup)
