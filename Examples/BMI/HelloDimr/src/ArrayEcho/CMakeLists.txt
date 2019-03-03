add_library(ArrayEcho SHARED dllmain.cpp ArrayEcho.cpp EchoYamlParser.cpp)
target_include_directories(ArrayEcho PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_link_libraries(ArrayEcho ${CMAKE_CURRENT_SOURCE_DIR}/../../lib/yaml.lib)
