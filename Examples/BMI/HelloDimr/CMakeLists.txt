cmake_minimum_required(VERSION 3.0.0)
project(HelloDimr)

set(CMAKE_CXX_STANDARD 17)

SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Defining some directory paths
set(AB_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ArrayBuilder)
set(AE_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ArrayEcho)
set(ARRAY_LIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/lib)

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
add_subdirectory(${AB_SRC_DIR})
add_subdirectory(${AE_SRC_DIR})


