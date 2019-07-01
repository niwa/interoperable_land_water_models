cmake_minimum_required(VERSION 3.0.0)
project(lookup)

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_BUILD_TYPE Release)

if(MSVC)
    # MSVC defines build types at runtime
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/$<CONFIG>)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/$<CONFIG>)
else()
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_BUILD_TYPE})
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_BUILD_TYPE})
endif()

# Local settings
#
# Should define at least the following variables
#
# YAML_CPP_INCLUDE : path to yaml-cpp include dir
# YAML_CPP_STATIC_LIB : path to yaml-cpp static library
#
# E.g. local.cmake contents:
#
# set(YAML_CPP_ROOT c:/dev/yaml-cpp)
# set(YAML_CPP_INCLUDE ${YAML_CPP_ROOT}/include)
# set(YAML_CPP_STATIC_LIB ${YAML_CPP_ROOT}/build/libyaml-cpp.a)
include(local.cmake)

# Defining some directory paths
set(LU_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(LU_DATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/data)
set(LU_CATCH_DIR ${LU_SRC_DIR}/catch)
set(LU_LIB_DIR ${LU_SRC_DIR}/lib)
set(LU_BMI_DIR ${LU_SRC_DIR}/bmi)

# Copy mingw runtime libraries to output folder
IF (WIN32 AND NOT MSVC)
    configure_file(
        ${MINGW_BIN_DIR}/libstdc++-6.dll
        ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libstdc++-6.dll
        COPYONLY
    )
    configure_file(
        ${MINGW_BIN_DIR}/libgcc_s_seh-1.dll
        ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libgcc_s_seh-1.dll
        COPYONLY
    )
ENDIF (WIN32 AND NOT MSVC)

# Targets
add_subdirectory(${LU_CATCH_DIR})
add_subdirectory(${LU_LIB_DIR})
add_subdirectory(${LU_BMI_DIR})


