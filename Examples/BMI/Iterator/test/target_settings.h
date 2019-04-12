#ifndef BMIT_TARGET_SETTINGS_H
#define BMIT_TARGET_SETTINGS_H

// Defines paths to test target, relative to test executables

#ifdef WIN32
#define TARGET_LIB ".\\libbmi_summer.dll"
#define TARGET_CFG ".\\bmi_summer.yaml"
#else
#define TARGET_LIB "./libbmi_summer.so"
#define TARGET_CFG "./bmi_summer.yaml"
#endif // WIN32

#endif // BMIT_TARGET_SETTINGS_H
