#include <string>
#include <cstring>

#include "bmi.h"
#include "bmit.h"

bmit::Iterator* IT = nullptr;

/* Store callback */
Logger logger = NULL;

/* control functions. These return an error code. */
BMI_API int initialize(const char* config_file) {
	/* Avoid initializing over existing instance */
	if (IT != nullptr) {
		// ToDo: log this (trying to initialize initalized iterator instance)
		return -1;
	}

	// Instantiating the iterator will throw if config is not valid
	try {
		IT = bmit::Iterator::Create(config_file);
	}
	catch (std::exception&) {
		// ToDo: log exception
		IT = nullptr;
    	return -1;
    }

	// OK
	return 0;
}


BMI_API int update(double dt) {
	try {
        IT->run();
    }
    catch (std::exception&) {
        // ToDo: log exception
        return -1;
    }
    return 0;
}


BMI_API int finalize() {
	bmit::Iterator::Dispose(IT);
	IT = nullptr;
	return 0;
}


/* time control functions */
BMI_API void get_start_time(double* t) {}


BMI_API void get_end_time(double* t) {}


BMI_API void get_current_time(double* t) {}


BMI_API void get_time_step(double* dt) {}


/* variable info */
BMI_API void get_var_shape(const char *name, int shape[MAXDIMS]) {
    std::vector<int> dims = IT->get_var_shape(name);
    if (dims.size() > MAXDIMS) {
        // TODO: log this
    }
    int index = 0;
    for (const auto dim : dims) {
        shape[index] = dim;
        index++;
    }
}


BMI_API void get_var_rank(const char* name, int* rank) {
    *rank = IT->get_var_rank(name);
}


BMI_API void get_var_type(const char* name, char* type) {
    auto s = IT->get_var_type(name);
    strncpy_s(type, MAXSTRINGLEN, s.c_str(), s.size() + 1);
}


BMI_API void get_var_count(int* count) {
    *count = IT->get_var_count();
}


BMI_API void get_var_name(int index, char* name) {
    auto str = IT->get_var_name(index);
    strncpy_s(name, MAXSTRINGLEN, str.c_str(), str.size() + 1);
}


/* data access */
BMI_API void get_var(const char* name, void** ptr) {
    IT->get_var(name, ptr);
}


BMI_API void set_var(const char* name, void* ptr) {
    IT->set_var(name, ptr);
}

/* set logger by setting a pointer to the log function */
void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Muskingum BMI library attached to logger.";
	logger = callback;
	logger(level, msg.c_str());
}