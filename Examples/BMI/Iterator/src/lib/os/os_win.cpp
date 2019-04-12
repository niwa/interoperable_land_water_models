#include "os.h"

BMIT_LIB load_library(const std::string &path) {
	return LoadLibraryA((LPCSTR) path.c_str());
}

BMIT_SYM load_symbol(BMIT_LIB lib_handle, const std::string &name) {
	return GetProcAddress(lib_handle, (LPCSTR) name.c_str());
}

bool free_library(BMIT_LIB lib_handle) {
	if (lib_handle == nullptr) return true;
	return FreeLibrary(lib_handle);
}
