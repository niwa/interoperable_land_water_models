#include "target.h"
#include "os/os.h"
#include <iostream>

// TODO: retrieve from loaded shared lib
#define MAXSTRINGLEN 1024

typedef int bmi_initialize(const char*);
typedef int bmi_update();
typedef int bmi_finalize();
typedef void bmi_get_start_time(double*);
typedef void bmi_get_end_time(double*);
typedef void bmi_get_current_time(double*);
typedef void bmi_get_time_step(double*);
typedef void bmi_get_input_var_name_count(int*);
typedef void bmi_get_output_var_name_count(int*);
typedef void bmi_get_input_var_names(char**);
typedef void bmi_get_output_var_names(char**);
typedef void bmi_get_var_units(const char*, char*);
typedef void bmi_get_var_type(const char*, char*);
typedef void bmi_get_var_itemsize(const char*, int*);
typedef void bmi_get_var_rank(const char*, int*);
typedef void bmi_get_var_size(const char*, int*);
typedef void bmi_get_var_nbytes(const char*, int*);
typedef void bmi_get_value(const char*, char*);
typedef void bmi_set_value(const char*, char*);


bmit::Target::Target(const std::string &lib_path) {
	auto lib = load_library(lib_path);
	if (lib == NULL) throw "Failed to load target library";
	m_lib = (void*) lib;

	/* Load symbols, store as void* */
	m_initialize = (void*) load_symbol(lib, "initialize");
	m_update = (void*) load_symbol(lib, "update");
	m_finalize = (void*) load_symbol(lib, "finalize");
	m_get_start_time = (void*) load_symbol(lib, "get_start_time");
	m_get_end_time = (void*) load_symbol(lib, "get_end_time");
	m_get_current_time = (void*) load_symbol(lib, "get_current_time");
	m_get_time_step = (void*) load_symbol(lib, "get_time_step");
	m_get_input_var_name_count = (void*) load_symbol(lib, "get_input_var_name_count");
	m_get_output_var_name_count = (void*) load_symbol(lib, "get_output_var_name_count");
	m_get_input_var_names = (void*) load_symbol(lib, "get_input_var_names");
	m_get_output_var_names = (void*) load_symbol(lib, "get_output_var_names");
	m_get_var_units = (void*) load_symbol(lib, "get_var_units");
	m_get_var_type = (void*) load_symbol(lib, "get_var_type");
	m_get_var_itemsize = (void*) load_symbol(lib, "get_var_itemsize");
	m_get_var_rank = (void*) load_symbol(lib, "get_var_rank");
	m_get_var_size = (void*) load_symbol(lib, "get_var_size");
	m_get_var_nbytes = (void*) load_symbol(lib, "get_var_nbytes");
	m_get_value = (void*) load_symbol(lib, "get_value");
	m_set_value = (void*) load_symbol(lib, "set_value");
}


bmit::Target::~Target() {
	if (m_lib != nullptr) {
		auto lib = (BMIT_LIB) m_lib;
		free_library(lib);
		m_lib = nullptr;
	}
}


int bmit::Target::initialize(const std::string &cfg_path) {
	if (m_initialize == nullptr) throw "initialize symbol not loaded";
	auto f = (bmi_initialize*) m_initialize;
	return f(cfg_path.c_str());
}


int bmit::Target::update() {
	if (m_update == nullptr) throw "update symbol not loaded";
	auto f = (bmi_update*) m_update;
	return f();
}


int bmit::Target::finalize() {
	if (m_finalize == nullptr) throw "finalize symbol not loaded";
	auto f = (bmi_finalize*) m_finalize;
	return f();
}


void bmit::Target::get_start_time(double* t) {
	// get_start_time(double* t);
}


void bmit::Target::get_end_time(double* t) {
	// get_end_time(double* t);
}


void bmit::Target::get_current_time(double* t) {
	// get_current_time(double* t);
}


void bmit::Target::get_time_step(double* dt) {
	// get_time_step(double* dt);
}


int bmit::Target::get_input_var_name_count() {
	if (m_get_input_var_name_count == nullptr) {
		throw "get_input_var_name_count symbol not loaded";
	}
	auto f = (bmi_get_input_var_name_count*) m_get_input_var_name_count;

	int count;
	f(&count);
	return count;
}


int bmit::Target::get_output_var_name_count() {
	if (m_get_output_var_name_count == nullptr) {
		throw "get_output_var_name_count symbol not loaded";
	}
	auto f = (bmi_get_output_var_name_count*) m_get_output_var_name_count;

	int count;
	f(&count);
	return count;
}


std::vector<std::string>
bmit::Target::get_input_var_names() {
	if (m_get_input_var_names == nullptr) {
		throw "get_input_var_names symbol not loaded";
	}
	auto f = (bmi_get_input_var_names*) m_get_input_var_names;

	/* Allocate memory to retrieve names */
	int nb_inputs = get_input_var_name_count();
	char* names[nb_inputs];
    for (int i = 0; i < nb_inputs; i++) {
        names[i] = (char*) malloc(MAXSTRINGLEN * sizeof(char));
    }

    /* Library call*/
    f(names);

    /* Collect names and free allocated memory */
	auto vec = std::vector<std::string> {};
	for (int i = 0; i < nb_inputs; i++) {
		vec.push_back(std::string {names[i]});
		free (names[i]);
	}

	return vec;
}

std::vector<std::string>
bmit::Target::get_output_var_names() {
	if (m_get_output_var_names == nullptr) {
		throw "get_output_var_names symbol not loaded";
	}
	auto f = (bmi_get_output_var_names*) m_get_output_var_names;

	/* Allocate memory to retrieve names */
	int nb_outputs = get_output_var_name_count();
	char* names[nb_outputs];
    for (int i = 0; i < nb_outputs; i++) {
        names[i] = (char*) malloc(MAXSTRINGLEN * sizeof(char));
    }

    /* Library call*/
    f(names);

    /* Collect names and free allocated memory */
	auto vec = std::vector<std::string> {};
	for (int i = 0; i < nb_outputs; i++) {
		vec.push_back(std::string {names[i]});
		free (names[i]);
	}

	return vec;
}


void bmit::Target::get_var_units(const char* name, char* units) {
	// get_var_units(const char* name, char* units);
}


void bmit::Target::get_var_type(const char* name, char* type) {
	// get_var_type(const char* name, char* type);
}


void bmit::Target::get_var_itemsize(const char* name, int* itemsize) {
	// get_var_itemsize(const char* name, int* itemsize);
}


void bmit::Target::get_var_rank(const char* name, int* rank) {
	// get_var_rank(const char* name, int* rank);
}


void bmit::Target::get_var_size(const char* name, int* size) {
	// get_var_size(const char* name, int* size);
}


void bmit::Target::get_var_nbytes(const char* name, int* nbytes) {
	// get_var_nbytes(const char* name, int* nbytes);
}


void bmit::Target::get_value(const std::string& name, void* valptr) {
	if (m_get_value == nullptr) {
		throw "get_value symbol not loaded";
	}
	auto f = (bmi_get_value*) m_get_value;

	f(name.c_str(), (char*) valptr);
}


void bmit::Target::set_value(const std::string& name, void* valptr) {
	if (m_set_value == nullptr) {
		throw "set_value symbol not loaded";
	}
	auto f = (bmi_set_value*) m_set_value;

	f(name.c_str(), (char*) valptr);
}
