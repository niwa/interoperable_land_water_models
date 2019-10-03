// bmi_PySwitch.cpp :
// Starts or shuts down the Python interpreter 
//
#include <string>
#include <iostream>
#include "bmi.h"
#include <pybind11/pybind11.h>
#include <pybind11/embed.h>

namespace py = pybind11;
typedef enum {
	START_MODE,
	SHUTDOWN_MODE,
	TEST_MODE,
	UNSET_MODE = -1
} PySwitchMode;

PySwitchMode switchMode = UNSET_MODE;
double currentTime = 0.;

/* Store callback */
Logger logger = NULL;

/* Logger function */
void _log(Level level, std::string msg);

int initialize(const char *modeArg)
{
	std::string modeString = "";
	for (int i = 0; i < strlen(modeArg); i++) {
		modeString.append(1, toupper(modeArg[i]));
	}
	std::string msg = "initializing in ";
	msg.append(modeString);
	msg.append(" mode.");
	_log(LEVEL_INFO, msg.c_str());

	if (!modeString.compare("START")) {
		switchMode = START_MODE; 
	}
	else if (!modeString.compare("SHUTDOWN")){
		switchMode = SHUTDOWN_MODE;
	}
	else if (!modeString.compare("TEST")) {
		switchMode = TEST_MODE;
	}

	try
	{
		if (!Py_IsInitialized() 
			&& (switchMode == START_MODE || switchMode == TEST_MODE)) {
			py::initialize_interpreter();
			// py::print("Interpreter initialized");
		}
	}
	// all-purpose catch block for Python errors
	catch (py::error_already_set const &pythonErr)
	{
		std::cout << pythonErr.what();
	}

	return 0;
}

int update(double dt) {
	if (dt == -1) get_time_step(&dt);
	currentTime += dt;
	if (Py_IsInitialized() && switchMode == TEST_MODE) {
		py::print("UPDATE: The Python interpreter is running.");
	} 
	return 0;
}

int finalize() {
	/* Returns:
	     -1 (true) if the Python interpreter isn't running at call time
	     0 (false) if call to this function shuts down the Python interpreter
	     1 (true) if the Python interpreter is left running
	*/
	if (Py_IsInitialized()) {
		switch (switchMode){
			// Note the fall-through behavior of the switch
			case TEST_MODE:
				py::print("FINALIZE: Shutting down the Python interpreter.");
			case SHUTDOWN_MODE:
				py::finalize_interpreter();
				return 0;
			case START_MODE:
			case UNSET_MODE:
			default:
				return 1;
		}
	}
	return -1;
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
	*t = currentTime;
}

void get_time_step(double *dt)
{
	*dt = 1;
}

/* variable info */
void get_var_shape(const char *name, int shape[MAXDIMS]) {
	_log(LEVEL_DEBUG, "Function get_var_shape not implemented in Python switch.");
	return;
}

void get_var_rank(const char *name, int *rank) {
	_log(LEVEL_DEBUG, "Function get_var_rank not implemented in Python switch.");
	return;
}

void get_var_type(const char *name, char *type) {
	_log(LEVEL_DEBUG, "Function get_var_type not implemented in Python switch.");
	return;
}

void get_var_count(int *count) {
	_log(LEVEL_DEBUG, "Function get_var_count not implemented in Python switch.");
	return;
}

void get_var_name(int index, char *name) {
	_log(LEVEL_DEBUG, "Function get_var_name not implemented in Python switch.");
	return;
}

/* get a pointer pointer - a reference to a multidimensional array */
void get_var(const char *name, void **ptr) {
	_log(LEVEL_DEBUG, "Function get_var not implemented in Python switch.");
	return;
}

/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char *name, const void *ptr) {
	_log(LEVEL_DEBUG, "Function set_var not implemented in Python switch.");
	return;
}

/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
void set_var_slice(const char *name, const int *start, const int *count, const void *ptr) {
	_log(LEVEL_DEBUG, "Function set_var_slice not implemented in Python switch.");
	return;
}


void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging function attached to Python switch.";
	logger = callback;
	logger(level, msg.c_str());
}

void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}
