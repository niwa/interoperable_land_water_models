# The app executable
add_executable(typo_app main.cpp)
set_target_properties(typo_app PROPERTIES OUTPUT_NAME typo)
target_include_directories(typo_app PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# Linking to the typo library
target_include_directories(typo_app PRIVATE ${TYPO_LIB_DIR})
target_link_libraries(typo_app PRIVATE typo_lib)

# Copy runtime libraries to output folder
IF (WIN32 AND NOT MSVC)
    add_custom_command(TARGET typo_app POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${MINGW_BIN_DIR}/libstdc++-6.dll
            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libstdc++-6.dll
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${MINGW_BIN_DIR}/libgcc_s_seh-1.dll
            ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libgcc_s_seh-1.dll
    )
ENDIF (WIN32 AND NOT MSVC)
