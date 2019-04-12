#ifndef BMIT_OS_H
#define BMIT_OS_H

#include  <string>

#ifdef _WIN32
#include "Windows.h"
#define BMIT_LIB HMODULE
#define BMIT_SYM FARPROC
#else
// TODO
#endif // _WIN32

BMIT_LIB load_library(const std::string &path);

BMIT_SYM load_symbol(BMIT_LIB lib_handle, const std::string &name);

bool free_library(BMIT_LIB lib_handle);


#endif // BMIT_OS_H
