add_library(ArrayBuilder SHARED dllmain.cpp ArrayEcho.cpp EchoYamlParser.cpp)
target_include_directories(ArrayBuilder PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_link_libraries(ArrayBuilder ${CMAKE_CURRENT_SOURCE_DIR}/../lib/yaml.lib)
