

/* bmi_sqliteAdapter.cpp
*
*  sqliteAdapter is a utility that merges columns across tables in SQLite databases.
*  It is written in Python and wrapped in a C++ DLL to make it run from Delta Shell 
*  or DIMR using the Open Earth version of BMI.
*
*  BMI functions are declared in "bmi.h" and defined here.
*  These functions are pass-throughs to the Python program:
*      initialize()
*      update()
*  The remaining BMI functions are defined here in the C++ wrapper.
*
*  Python model is accessed through the Pybind11 library. If no Python interpreter
*  is active when the initialize() function is called, the DLL will call the
*  pybind11::initialize_interpreter() to launch one.
*
*  sqliteAdapter's Python module is freed for garbage collection in the finalize()
*  function, but the Python interpreter is not shut down. 
*
*  In a DIMR configuration, bmi_sqliteAdapter should be followed by another program
*  that shuts down the Python interpreter.
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

double current_time = 0;
double timestep = 1;

/* Global declarations */
// here's a static object to represent the sqliteAdapter Python module
py::module sqlModule;

/* We expect a python interpreter to be running before with initialize
sqliteAdapter. If it's not -- like if we're running a test in bmi-runner --
we need to close the interpreter when we run finalize(). */
bool close_python_on_finalize = false;

/* bmi.h defines Logger as a pointer to a logging function. The logger variable
declared here is assigned in the set_logger function below.*/
Logger logger = NULL;

/* Logger function; declared here, defined below. */
void _log(Level level, std::string msg);

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

		// Interpreter is initialized; we can import the sqliteAdapter module
		sqlModule = py::module::import("SqliteAdapter");
		// call the initialize function of the sqliteAdapter module
		sqlModule.attr("initialize")(config_file);
		py::object retVal = sqlModule.attr("closeInterpreterOnExit");
		close_python_on_finalize = retVal.cast<bool>();
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
	msg << "Executing SQLite Adapter.";
	_log(LEVEL_DEBUG, msg.str());
	current_time += 1;

	/* Since the sqliteAdapter is a run-once utility, updating by a time
	*  increment isn't really appropriate. No matte what is presented as
	*  the value of dt, the sqliteAdapter.update() Python function is
	*  always called with an argument of -1.
	*/
	try
	{
		// py::print("In update");
		sqlModule.attr("update")(-1);
	}
	catch (py::error_already_set const &pythonErr)
	{
		std::cout << pythonErr.what();
	}

	return 0;
}

int finalize()
{
	/* The sqliteAdapter Python module doesn't have a finalize method.
	*
	*  The sqlModule is declared with global scope, but it is instantiated in 
	*  the initialize() function. Its Python reference count is decremented 
	*  here so that the interpreter can garbage-collect it before the 
	*  interpreter is shut down.
	*/
	sqlModule.dec_ref();
	if (close_python_on_finalize)
		py::finalize_interpreter();
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
	*t = current_time;
}

void get_time_step(double *dt)
{
	*dt = timestep;
}

/* variable info */
void get_var_shape(const char *name, int shape[MAXDIMS]) {
	_log(LEVEL_DEBUG, "Function get_var_shape not implemented in sqliteAdapter wrapper.");
	return;
}

void get_var_rank(const char *name, int *rank) {
	_log(LEVEL_DEBUG, "Function get_var_rank not implemented in sqliteAdapter wrapper.");
	return;
}

void get_var_type(const char *name, char *type) {
	_log(LEVEL_DEBUG, "Function get_var_type not implemented in sqliteAdapter wrapper.");
	return;
}

void get_var_count(int *count) {
	_log(LEVEL_DEBUG, "Function get_var_count not implemented in sqliteAdapter wrapper.");
	return;
}

void get_var_name(int index, char *name) {
	_log(LEVEL_DEBUG, "Function get_var_name not implemented in sqliteAdapter wrapper.");
	return;
}

/* get a pointer pointer - a reference to a multidimensional array */
void get_var(const char *name, void **ptr) {
	_log(LEVEL_DEBUG, "Function get_var not implemented in sqliteAdapter wrapper.");
	return;
}

/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char *name, const void *ptr) {
	_log(LEVEL_DEBUG, "Function set_var not implemented in sqliteAdapter wrapper.");
	return;
}

/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
void set_var_slice(const char *name, const int *start, const int *count, const void *ptr) {
	_log(LEVEL_DEBUG, "Function set_var_slice not implemented in sqliteAdapter wrapper.");
	return;
}

void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to sqliteAdapter";
	logger = callback;
	logger(level, msg.c_str());
}

void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}
