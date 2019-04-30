#include <string>
#include <cstring>

#include "bmi_iterator.h"
#include "bmit.h"

bmit::Iterator* IT = nullptr;


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
	catch (std::exception& e) {
		// ToDo: log exception
		IT = nullptr;
    	return -1;
    }

	// OK
	return 0;
}


BMI_API int update() {
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
BMI_API void get_input_var_name_count(int* count) {
    *count = IT->count_inputs();
}


BMI_API void get_output_var_name_count(int* count) {
    *count = IT->count_outputs();
}


BMI_API void get_input_var_names(char** return_names) {
    auto names = IT->get_input_names();
    for (auto it = names.begin(); it != names.end(); ++it) {
        auto index = it - names.begin();
        auto name = names[index];
        strncpy_s(return_names[index], MAXSTRINGLEN, name.c_str(), name.size() + 1);
    }
}


BMI_API void get_output_var_names(char** return_names) {
    auto names = IT->get_output_names();
    for (auto it = names.begin(); it != names.end(); ++it) {
        auto index = std::distance(names.begin(), it);
        auto name = names[index];
        strncpy_s(return_names[index], MAXSTRINGLEN, name.c_str(), name.size() + 1);
    }
}


BMI_API void get_var_type(const char* name, char* type) {
    auto s = IT->get_var_type(name);
    strncpy_s(type, MAXSTRINGLEN, s.c_str(), s.size() + 1);
}


BMI_API void get_var_units(const char* name, char* units) {
    auto s = IT->get_var_units(name);
    strncpy_s(units, MAXSTRINGLEN, s.c_str(), s.size() + 1);
}


BMI_API void get_var_itemsize(const char* name, int* itemsize) {
    *itemsize = IT->get_var_itemsize(name);
}


BMI_API void get_var_rank(const char* name, int* rank) {
    *rank = IT->get_var_rank(name);
}


BMI_API void get_var_size(const char* name, int* size) {
    auto sizes = IT->get_var_size(name);
    auto index = 0;
    for (const auto val : sizes) {
    	*(size + index) = val;
    	index++;
    }
}


BMI_API void get_var_nbytes(const char* name, int* nbytes) {
	*nbytes = IT->get_var_nbytes(name);
}


/* data access */
BMI_API void get_value(const char* name, char* buffer) {
    IT->get_value(name, (void*) buffer);
}


BMI_API void set_value(const char* name, char* buffer) {
    IT->set_value(name, (void*) buffer);
}
