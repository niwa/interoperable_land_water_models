#ifndef BMIT_TARGET_SETTINGS_H
#define BMIT_TARGET_SETTINGS_H

// Defines paths to test target, relative to test executables

#if _MSC_VER && !__INTEL_COMPILER // MSVC
#define TARGET_LIB ".\\bmi_concat.dll"
#define TARGET_CFG ".\\bmi_concat.yaml"
#elif WIN32 // Other compilers on Windows
#define TARGET_LIB ".\\libbmi_concat.dll"
#define TARGET_CFG ".\\bmi_concat.yaml"
#else // Linux
#define TARGET_LIB "./libbmi_concat.so"
#define TARGET_CFG "./bmi_concat.yaml"
#endif

#endif // BMIT_TARGET_SETTINGS_H
