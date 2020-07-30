// SampleCppLibrary.cpp : Defines the exported functions for the DLL application.
//

#include <cstdio>
#include <string>
#include <sstream>
#include "../include/bmi.h"
#include <iostream>
#include <pybind11/embed.h>

namespace py = pybind11;

double current = 0;
double timestep = 1;
// define some arrays for exchange
double arr1[3] = { 3, 2, 1 };
int arr2[2][3] =
{
  { 3, 2, 1},
  { 6, 4, 2}
};
bool arr3[2][2][3] =
{
  {
	{ true, false, false},
	{ false, true, false}
  },
  {
	{ false, false, false},
	{ false, true, false}
  }
};

/* Store callback */
Logger logger = NULL;

/* Logger function */
void _log(Level level, std::string msg);

py::scoped_interpreter guard{}; // start the interpreter and keep it alive
py::module bmiheatmodule = py::module::import("bmi-python.heat.bmi_heat");
py::object bmiheatobj = bmiheatmodule.attr("BmiHeat")();


extern "C" {
	BMI_API int initialize(const char *config_file)
	{
		std::ostringstream msg;
		msg << "initializing with " << config_file;
		_log(LEVEL_INFO, msg.str());

		try
		{
			/*
			py::print("Hello, World!"); // use the Python API

			py::exec(R"(
				kwargs = dict(name="World", number=42)
				message = "Hello, {name}! The answer is {number}".format(**kwargs)
				print(message)
			)");
			*/

			//py::module sys = py::module::import("sys");
			//py::print(sys.attr("path"));
			//py::print(sys.attr("version"));

			//py::object result = hello.attr("add")(1, 2);
			//int n = result.cast<int>();
			//py::print(n);

			bmiheatobj.attr("initialize")(config_file);

			py::print(bmiheatobj.attr("_model"));
			py::print(bmiheatobj.attr("_values"));
			py::print(bmiheatobj.attr("_var_units"));
			py::print(bmiheatobj.attr("_grids"));
			py::print(bmiheatobj.attr("_grid_type"));
		}
		catch (py::error_already_set const &pythonErr)
		{
			std::cout << pythonErr.what();
		}

		return 0;
	}

	BMI_API int update(double dt)
	{
		std::ostringstream msg;
		msg << "updating from " << current << " with dt: " << (dt != -1 ? dt : timestep);
		_log(LEVEL_DEBUG, msg.str());
		current += (dt != -1 ? dt : timestep);

		try
		{
			bmiheatobj.attr("update")();
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
			py::print(bmiheatobj.attr("get_value")("plate_surface__temperature_out"));
		}
		catch (py::error_already_set const &pythonErr)
		{
			std::cout << pythonErr.what();
		}
		return 0;
	}

	BMI_API void get_start_time(double *t)
	{
		*t = 0;
	}

	BMI_API void get_end_time(double *t)
	{
		*t = 5;
	}

	BMI_API void get_current_time(double *t)
	{
		*t = current;
	}

	BMI_API void get_time_step(double *dt)
	{
		*dt = timestep;
	}

	BMI_API void get_var(const char *name, void **ptr)
	{
		/* The value referenced to by ptr is the memory address of arr1 */
		*ptr = &arr1;
	}

	BMI_API void set_logger(Logger callback)
	{
		Level level = LEVEL_INFO;
		std::string msg = "Logging attached to cxx model";
		logger = callback;
		logger(level, msg.c_str());
	}
}

void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}

// placeholder function, all dll's need a main.. in windows only
#if defined _WIN32
void main()
{
}
#endif
