cmake_minimum_required(VERSION 3.0.0)
project(typo)

# compile flags
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")


# Defining some directory paths
set(TYPO_APP_DIR ${CMAKE_CURRENT_SOURCE_DIR}/typo_app)
set(TYPO_LIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/typo_lib)
set(TYPO_BMI_APP_DIR ${CMAKE_CURRENT_SOURCE_DIR}/typo_bmi_app)
set(TYPO_BMI_LIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/typo_bmi_lib)

# Targets
add_subdirectory(${TYPO_LIB_DIR})
add_subdirectory(${TYPO_APP_DIR})
add_subdirectory(${TYPO_BMI_LIB_DIR})
add_subdirectory(${TYPO_BMI_APP_DIR})
