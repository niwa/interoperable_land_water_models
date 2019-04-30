cmake_minimum_required(VERSION 3.0.0)
project(bmit)

set(CMAKE_CXX_STANDARD 17)

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Local settings
#
# Should define at least the following variables
#
# SQLITE_INCLUDE   : path to sqlite include dir
# SQLITE_LIB       : path to sqlite static library
# YAML_CPP_INCLUDE : path to yaml-cpp include dir
# YAML_CPP_LIB     : path to yaml-cpp static library
#
# E.g. local.cmake contents:
#
# set(SQLITE_ROOT c:/dev/sqlite)
# set(SQLITE_INCLUDE ${SQLITE_ROOT})
# set(SQLITE_LIB ${SQLITE_ROOT}/sqlite3.dll)
# set(YAML_CPP_ROOT c:/dev/yaml-cpp)
# set(YAML_CPP_INCLUDE ${YAML_CPP_ROOT}/include)
# set(YAML_CPP_LIB ${YAML_CPP_ROOT}/build/libyaml-cpp.a)
include(local.cmake)

# Defining some directory paths
set(IT_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(IT_CATCH_DIR ${IT_SRC_DIR}/catch)
set(IT_LIB_DIR ${IT_SRC_DIR}/lib)
set(IT_BMI_DIR ${IT_SRC_DIR}/bmi)
set(IT_TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR}/test)
set(IT_TARGET_DIR ${IT_TEST_DIR}/target)

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
add_subdirectory(${IT_CATCH_DIR})
add_subdirectory(${IT_TARGET_DIR})
add_subdirectory(${IT_LIB_DIR})
add_subdirectory(${IT_BMI_DIR})
