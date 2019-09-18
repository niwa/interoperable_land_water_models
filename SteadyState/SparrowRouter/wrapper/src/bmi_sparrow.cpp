// bmi_sparrow.cpp : Defines the exported functions for the DLL application.
//
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

py::scoped_interpreter guard{}; // start the interpreter and keep it alive
py::module srModule = py::module::import("SparrowRouter");
// py::object srObj = srModule.attr("SparrowRouter")();

PYBIND11_MODULE(SparrowRouter, bmi_log) {
	bmi_log.doc() = "Logging methods using OE BMI";
	bmi_log.def("log", &_log, "Invokes the BMI logger");
}

int initialize(const char *config_file)
{
	std::ostringstream msg;
	msg << "initializing with " << config_file;
	_log(LEVEL_INFO, msg.str());

	try
	{
		// py::print("In initialize");
		srModule.attr("initialize")(config_file);
	}
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

BMI_API int finalize()
{
	try {
		// py::print("In finalize");
		srModule.attr("finalize")();
	}
	catch (py::error_already_set const &pythonErr)
	{
		std::cout << pythonErr.what();
	}
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

void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to cxx model";
	logger = callback;
	logger(level, msg.c_str());
}

void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}


