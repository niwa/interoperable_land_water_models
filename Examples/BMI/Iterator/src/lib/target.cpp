#include "target.h"
#include "os/os.h"
#include <iostream>

#define MAXSTRINGLEN 1024
#define MAXDIMS 6

typedef int bmi_initialize(const char*);
typedef int bmi_update(double dt);
typedef int bmi_finalize();
typedef void bmi_get_start_time(double*);
typedef void bmi_get_end_time(double*);
typedef void bmi_get_current_time(double*);
typedef void bmi_get_time_step(double*);
typedef void bmi_get_var_shape(const char*, int[MAXDIMS]);
typedef void bmi_get_var_rank(const char*, int*);
typedef void bmi_get_var_type(const char*, char*);
typedef void bmi_get_var_count(int*);
typedef void bmi_get_var_name(int, char*);
typedef void bmi_get_var(const char*, void**);
typedef void bmi_set_var(const char*, const void*);


bmit::Target::Target(const std::string &lib_path) {
	auto lib = load_library(lib_path);
	if (lib == NULL) throw std::runtime_error("Failed to load target library");
	m_lib = (void*) lib;

	/* Load symbols, store as void* */
	m_initialize = (void*) load_symbol(lib, "initialize");
	m_update = (void*) load_symbol(lib, "update");
	m_finalize = (void*) load_symbol(lib, "finalize");
	m_get_start_time = (void*) load_symbol(lib, "get_start_time");
	m_get_end_time = (void*) load_symbol(lib, "get_end_time");
	m_get_current_time = (void*) load_symbol(lib, "get_current_time");
	m_get_time_step = (void*) load_symbol(lib, "get_time_step");
	m_get_var_shape = (void*) load_symbol(lib, "get_var_shape");
    m_get_var_rank = (void*) load_symbol(lib, "get_var_rank");
    m_get_var_type = (void*) load_symbol(lib, "get_var_type");
    m_get_var_count = (void*) load_symbol(lib, "get_var_count");
    m_get_var_name = (void*) load_symbol(lib, "get_var_name");
    m_get_var = (void*) load_symbol(lib, "get_var");
	m_set_var = (void*) load_symbol(lib, "set_var");
}


bmit::Target::~Target() {
	if (m_lib != nullptr) {
		auto lib = (BMIT_LIB) m_lib;
		free_library(lib);
		m_lib = nullptr;
	}
}


int bmit::Target::initialize(const std::string &cfg_path) {
	if (m_initialize == nullptr) throw std::runtime_error("initialize symbol not loaded");
	auto f = (bmi_initialize*) m_initialize;
	return f(cfg_path.c_str());
}


int bmit::Target::update(double dt) {
	if (m_update == nullptr) throw std::runtime_error("update symbol not loaded");
	auto f = (bmi_update*) m_update;
	return f(dt);
}


int bmit::Target::finalize() {
	if (m_finalize == nullptr) throw std::runtime_error("finalize symbol not loaded");
	auto f = (bmi_finalize*) m_finalize;
	return f();
}


void bmit::Target::get_start_time(double* t) {
	// get_start_time(double* t);
	if (m_get_start_time == nullptr) {
		throw std::runtime_error("get_start_time symbol not loaded");
	}
	throw std::runtime_error("target get_start_time not implemented");
}


void bmit::Target::get_end_time(double* t) {
	// get_end_time(double* t);
	if (m_get_end_time == nullptr) {
		throw std::runtime_error("get_end_time symbol not loaded");
	}
	throw std::runtime_error("target get_end_time not implemented");
}


void bmit::Target::get_current_time(double* t) {
	// get_current_time(double* t);
	if (m_get_current_time == nullptr) {
		throw std::runtime_error("get_current_time symbol not loaded");
	}
	throw std::runtime_error("target get_current_time not implemented");
}


void bmit::Target::get_time_step(double* dt) {
	// get_time_step(double* dt);
	if (m_get_time_step == nullptr) {
		throw std::runtime_error("get_time_step symbol not loaded");
	}
	throw std::runtime_error("target get_time_step not implemented");
}


std::vector<int> bmit::Target::get_var_shape(const std::string& name) {
	if (m_get_var_shape == nullptr) {
		throw std::runtime_error("get_var_shape symbol not loaded");
	}
	auto f = (bmi_get_var_shape*) m_get_var_shape;

	// Get shape array from dll target
	int buff[MAXDIMS];
	f(name.c_str(), buff);

	// Copy shape value to vector
	// Stop at first 0 (assuming nobody uses a shape like [5,0,4,0,0,0])
	auto shape = std::vector<int> {};
	for (int i = 0; i < MAXSTRINGLEN; i++) {
		if (buff[i] == 0) break;
		shape.push_back(buff[i]);
	}

	return shape;
}


int bmit::Target::get_var_rank(const std::string& name) {
	if (m_get_var_rank == nullptr) {
		throw std::runtime_error("get_var_rank symbol not loaded");
	}
	auto f = (bmi_get_var_rank*) m_get_var_rank;

	int rank;
	f(name.c_str(), &rank);

	return rank;
}


std::string bmit::Target::get_var_type(const std::string& name) {
	if (m_get_var_type == nullptr) {
		throw std::runtime_error("get_var_type symbol not loaded");
	}
	auto f = (bmi_get_var_type*) m_get_var_type;

	char type[MAXSTRINGLEN];
	f(name.c_str(), type);

	return std::string(type);
}


int bmit::Target::get_var_count() {
	if (m_get_var_count == nullptr) {
		throw std::runtime_error("get_var_count symbol not loaded");
	}
	auto f = (bmi_get_var_count*) m_get_var_count;

	int count;
	f(&count);
	return count;
}


std::string bmit::Target::get_var_name(int index) {
	if (m_get_var_name == nullptr) {
		throw std::runtime_error("get_var_name symbol not loaded");
	}
	auto f = (bmi_get_var_name*) m_get_var_name;

	/* Allocate memory to retrieve name */
	char name[MAXSTRINGLEN];

    /* Library call*/
    f(index, name);

    /* Retrun as string */
    return std::string {name};
}


void bmit::Target::get_var(const std::string& name, void** ptr) {
	if (m_get_var == nullptr) {
		throw std::runtime_error("get_var symbol not loaded");
	}
	auto f = (bmi_get_var*) m_get_var;

	f(name.c_str(), (void**) ptr);
}


void bmit::Target::set_var(const std::string& name, const void* ptr) {
	if (m_set_var == nullptr) {
		throw std::runtime_error("set_var symbol not loaded");
	}
	auto f = (bmi_set_var*) m_set_var;

	f(name.c_str(), (const void*) ptr);
}
