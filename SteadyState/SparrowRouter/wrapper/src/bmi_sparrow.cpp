/* bmi_sparrow.cpp
*
*  Sparrow is a steady-state contaminant routing model written in Python. It is 
*  wrapped in a C++ DLL to make it run from Delta Shell or DIMR using the Open Earth
*  version of BMI. 
*
*  The functions that are declared in "bmi.h" are defined here for Sparrow.
*  These functions are pass-throughs to the Python model:
*      initialize()
*      update()
*      finalize()
*  The remaining BMI functions are defined here in the C++ wrapper.
*
*  The Python model is accessed through the Pybind11 library. If no Python interpreter 
*  is active when the initialize() function is called, pybind11::initialize_interpreter()
*  is called. Sparrow's Python module is freed for garbage collection in the finalize()
*  function, but the Python interpreter is not shut down. In a DIMR configuration, 
*  bmi_sparrow should be followed by another program that shuts down the Python interpreter.
*
*/
#include <cstdio>
#include <string>
#include <sstream>
#include <iostream>
#include "bmi.h"
#include <pybind11/pybind11.h>
#include <pybind11/embed.h>

namespace py = pybind11;

double current = 0;
double timestep = 1;

/* Store callback */
Logger logger = NULL;

/* Logger function */
void _log(Level level, std::string msg);

// static object to represent the Sparrow Router Python module
py::module srModule;

/*
PYBIND11_MODULE(SparrowRouter, bmi_log) {
	bmi_log.doc() = "Logging methods using OE BMI";
	bmi_log.def("log", &_log, "Invokes the BMI logger");
}
*/

int initialize(const char *config_file)
{
	std::ostringstream msg;
	msg << "initializing with " << config_file;
	_log(LEVEL_INFO, msg.str());

	try
	{
		if (!Py_IsInitialized()) {
			py::initialize_interpreter();
		}
		// py::print("In initialize");

		// Interpreter is initialized; we can import the Sparrow module
		srModule = py::module::import("SparrowRouter");
		// call the initialize function of the Sparrow module
		srModule.attr("initialize")(config_file);
	}
	// all-purpose catch block for Python errors
	catch (py::error_already_set const &pythonErr)
	{
		std::cout << pythonErr.what();
	}

	return 0;
}

int update(double dt)
{
	std::ostringstream msg;
	msg << "updating from " << current << " with dt: " << (dt != -1 ? dt : timestep);
	_log(LEVEL_DEBUG, msg.str());
	current += (dt != -1 ? dt : timestep);

	try
	{
		// py::print("In update");
		srModule.attr("update")(dt);
	}
	catch (py::error_already_set const &pythonErr)
	{
		std::cout << pythonErr.what();
	}

	return 0;
}

int finalize()
{
	try {
		// py::print("In finalize");
		srModule.attr("finalize")();
	}
	catch (py::error_already_set const &pythonErr)
	{
		std::cout << pythonErr.what();
	}
	// srModule is global, but is instantiated in initialize().
	// Its Python reference count is decremented here so that the interpreter
	// can garbage-collect it before the interpreter is shut down.
	srModule.dec_ref();
	return 0;
}


void get_start_time(double *t)
{
	*t = 0;
}

void get_end_time(double *t)
{
	*t = 1;
}

void get_current_time(double *t)
{
	*t = current;
}

void get_time_step(double *dt)
{
	*dt = timestep;
}

/* variable info */
void get_var_shape(const char *name, int shape[MAXDIMS]) {
	_log(LEVEL_DEBUG, "Function get_var_shape not implemented in Sparrow wrapper.");
	return;
}

void get_var_rank(const char *name, int *rank) {
	_log(LEVEL_DEBUG, "Function get_var_rank not implemented in Sparrow wrapper.");
	return;
}

void get_var_type(const char *name, char *type) {
	_log(LEVEL_DEBUG, "Function get_var_type not implemented in Sparrow wrapper.");
	return;
}

void get_var_count(int *count) {
	_log(LEVEL_DEBUG, "Function get_var_count not implemented in Sparrow wrapper.");
	return;
}

void get_var_name(int index, char *name) {
	_log(LEVEL_DEBUG, "Function get_var_name not implemented in Sparrow wrapper.");
	return;
}

/* get a pointer pointer - a reference to a multidimensional array */
void get_var(const char *name, void **ptr) {
	_log(LEVEL_DEBUG, "Function get_var not implemented in Sparrow wrapper.");
	return;
}

/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char *name, const void *ptr) {
	_log(LEVEL_DEBUG, "Function set_var not implemented in Sparrow wrapper.");
	return;
}

/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
void set_var_slice(const char *name, const int *start, const int *count, const void *ptr) {
	_log(LEVEL_DEBUG, "Function set_var_slice not implemented in Sparrow wrapper.");
	return;
}


void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to Sparrow model";
	logger = callback;
	logger(level, msg.c_str());
}

void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}
