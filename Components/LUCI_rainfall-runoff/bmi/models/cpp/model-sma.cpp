#include <cstdio>
#include <string>
#include <sstream>
#include "bmi.h"
#include <iostream>
#include <pybind11/pybind11.h>
#include <pybind11/embed.h>

namespace py = pybind11;

double current = 0;
double timestep = 1;

/* Store callback */
Logger logger = NULL;

/* Logger function */
void _log(Level level, std::string msg);

/* declared here but instantiated in initalize function */
py::module rrModule;
py::object rrObj;

extern "C" {
	BMI_API int initialize(const char *config_file)
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

			rrModule = py::module::import("SMA.rainfall_runoff");
			rrObj = rrModule.attr("RainfallRunoff")();
			rrObj.attr("initialize")(config_file);
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
		if (dt == -1) msg << " (default)";
		_log(LEVEL_INFO, msg.str());
		current += (dt != -1 ? dt : timestep);

		// limit dt so we don't update past model end time
		double stop;
		get_end_time(&stop);
		if (current + dt > stop) {
			dt = stop - current;
			msg.clear();
			msg << "update to end time: " << stop;
			_log(LEVEL_INFO, msg.str());
		}

		try
		{
			py::print("In update");
			py::print(current);
			rrObj.attr("update")(current);
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
			py::object prevRchsTsd = rrObj.attr("prev_reaches_tsd");
			py::print("\noverland_flow");
			py::print(prevRchsTsd.attr("overland_flow"));
			py::print("\ngw_flow");
			py::print(prevRchsTsd.attr("gw_flow"));
		}
		catch (py::error_already_set const &pythonErr)
		{
			std::cout << pythonErr.what();
		}
		rrObj.dec_ref();
		rrModule.dec_ref();
		return 0;
	}

	BMI_API void get_start_time(double *t)
	{
		*t = 0;
	}

	BMI_API void get_end_time(double *t)
	{
		// *t = 79583;
		*t = 2;
	}

	BMI_API void get_current_time(double *t)
	{
		*t = current;
	}

	BMI_API void get_time_step(double *dt)
	{
		*dt = timestep;
	}
	
	/* variable info */
	void get_var_shape(const char *name, int shape[MAXDIMS])
	{
		return;
	}

	void get_var_rank(const char *name, int *rank)
	{
		return;
	}

	void get_var_type(const char *name, char *type)
	{
		return;
	}

	void get_var_count(int *count)
	{
		return;
	}

	void get_var_name(int index, char *name)
	{
		return;
	}

	/* get a pointer pointer - a reference to a multidimensional array */
	void get_var(const char *name, void **ptr)
	{
		return;
	}

	/* Set the variable from contiguous memory referenced to by ptr */
	void set_var(const char *name, const void *ptr)
	{
		return;
	}

	/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
	void set_var_slice(const char *name, const int *start, const int *count, const void *ptr)
	{
		return;
	}


	void set_logger(Logger callback)
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

// Placeholder function as all 'dll's need a main. Windows only.
#if defined _WIN32
void main()
{
}
#endif
