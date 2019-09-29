/**
 * GroundwaterBMI - Groundwater Basic Model Interface
 *
 * This module depends on Python 3.6 (ideally) with a minimum of
 * the following packages:
 *   - pyyaml
 *   - flopy
 *   - netCDF4
 *
 * This work is part of a National Science Challenge
 * Our Land and Water - Interoperable Modelling
 *
 * Copyright 2019 Mike Toews, GNS Science <m.toews@gns.cri.nz>
 **/

#ifdef _DEBUG
#undef _DEBUG
#include <Python.h>
#define _DEBUG
#else
#include <Python.h>
#endif

#include <iostream>
#include <exception>
#include <fstream>
#include <sstream>
#include <string>

/* OpenEarth BMI - https://github.com/openearth/bmi */
#include "bmi.h"

/* Logger function */
void _log(Level level, std::string msg);

/* Misc globals */
Logger logger = NULL;
bool is_initialized = false;
bool externally_initialized = false;
PyObject* pGM = NULL;


extern "C" {

	BMI_API int initialize(const char *config_file)
	{
		int ret;
		std::ostringstream msg;
		msg << "initializing with config file: " << config_file;
		_log(LEVEL_DEBUG, msg.str());

		// Use an embedded Python interpreter to do most of the work
		externally_initialized = (bool)Py_IsInitialized();
		if (!externally_initialized) {
			// Start a new Python interpreter
			Py_Initialize();
		}

		PyRun_SimpleString("import os, sys");

		// Send config_file path to Python via formatted string (easy!)
		std::string set_config_file = "config_file = r'''";
		set_config_file.append(config_file);
		set_config_file.append("'''");
		PyRun_SimpleString(set_config_file.c_str());

		// PyYAML package can read config_file from here
		ret = PyRun_SimpleString("import yaml");
		if (ret != 0) {
			_log(LEVEL_FATAL, "PyYAML package is required; pip install pyyaml");
			return -1;
		}
		ret = PyRun_SimpleString("fp = open(config_file, 'r')");
		if (ret != 0) {
			std::ostringstream msg;
			msg << "config file not found: " << config_file;
			_log(LEVEL_FATAL, msg.str());
			return -2;
		}
		ret = PyRun_SimpleString("config = yaml.safe_load(fp)");
		if (ret != 0) {
			std::ostringstream msg;
			msg << "config file cannot be loaded as YAML: " << config_file;
			_log(LEVEL_FATAL, msg.str());
			return -3;
		}
		PyRun_SimpleString("fp.close()");

		// config file must have a 'run_dir' string
		ret = PyRun_SimpleString("run_dir = config['run_dir']");
		if (ret != 0) {
			_log(LEVEL_FATAL, "config file does not have 'run_dir' entry");
			return -4;
		}

		// ensure 'run_dir' is a directory
		ret = PyRun_SimpleString("assert os.path.isdir(run_dir)");
		if (ret != 0) {
			_log(LEVEL_FATAL, "'run_dir' entry is not a directory");
			return -5;
		}
		//PyRun_SimpleString("os.chdir(run_dir)");
		PyRun_SimpleString("sys.path.insert(0, run_dir)");

		// do equivalent of: from run import GroundwaterModel
		PyObject* pName = PyUnicode_FromString("run");
		PyObject* pModule = PyImport_Import(pName);
		if (pName == NULL) {
			_log(LEVEL_FATAL, "cannot import run module");
			PyErr_Print();
			return -6;
		}
		else
			Py_DECREF(pName);
		PyObject* pClass = PyObject_GetAttrString(pModule, (char*)"GroundwaterModel");
		if (pClass == NULL) {
			_log(LEVEL_FATAL, "cannot import GroundwaterModel class");
			PyErr_Print();
			return -7;
		}

		// do equivalent of: gm = GroundwaterModel.initialize(config_file)
		pGM = PyObject_CallMethod(pClass, (char*)"initialize", "(s)", config_file);
		Py_DECREF(pClass);
		if (pGM == NULL) {
			_log(LEVEL_FATAL, "cannot initialize GroundwaterModel");
			PyErr_Print();
			return -8;
		}
		is_initialized = true;
		return 0;
	}

	BMI_API int update(double dt)
	{
		if (!is_initialized)
			return -1;
		int ret = 0;
		_log(LEVEL_DEBUG, "updating ... ");
		PyRun_SimpleString("os.chdir(run_dir)");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"update", "(d)", dt);
		if (PyLong_Check(pRes)) {
			ret = (int)PyLong_AsLong(pRes);;
			if (ret != 0)
				_log(LEVEL_ERROR, "'gm.update(dt)' failed");
		}
		else
			_log(LEVEL_WARNING, "unexpected return type from 'gm.update(dt)'");
		Py_DECREF(pRes);
		return ret;
	}

	BMI_API int finalize()
	{
		if (!is_initialized)
			return -1;
		int ret = 0;
		_log(LEVEL_DEBUG, "finalize ... ");
		PyRun_SimpleString("os.chdir(run_dir)");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"finalize", "()", NULL);
		if (PyLong_Check(pRes)) {
			ret = (int)PyLong_AsLong(pRes);;
			if (ret != 0)
				_log(LEVEL_ERROR, "'gm.finalize()' failed");
		}
		else
			_log(LEVEL_WARNING, "unexpected return type from 'gm.finalize()'");
		Py_DECREF(pRes);
		Py_DECREF(pGM);
		if (!externally_initialized) {
			Py_Finalize();
		}
		return 0;
	}

	BMI_API void get_start_time(double *t)
	{
		if (!is_initialized) {
			*t = -1.0;
			return;
		}
		_log(LEVEL_DEBUG, "get_start_time ... ");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_start_time", "()", NULL);
		if (PyFloat_Check(pRes))
			*t = PyFloat_AsDouble(pRes);
		else {
			*t = 0.0;
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_start_time()'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_end_time(double *t)
	{
		if (!is_initialized) {
			*t = -1.0;
			return;
		}
		_log(LEVEL_DEBUG, "get_end_time ... ");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_end_time", "()", NULL);
		if (PyFloat_Check(pRes))
			*t = PyFloat_AsDouble(pRes);
		else {
			*t = 0.0;
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_end_time()'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_current_time(double *t)
	{
		if (!is_initialized) {
			*t = -1.0;
			return;
		}
		_log(LEVEL_DEBUG, "get_current_time ... ");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_current_time", "()", NULL);
		if (PyFloat_Check(pRes))
			*t = PyFloat_AsDouble(pRes);
		else {
			*t = 0.0;
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_current_time()'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_time_step(double *dt)
	{
		if (!is_initialized) {
			*dt = -1.0;
			return;
		}
		_log(LEVEL_DEBUG, "get_time_step ... ");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_time_step", "()", NULL);
		if (PyFloat_Check(pRes))
			*dt = PyFloat_AsDouble(pRes);
		else {
			*dt = 0.0;
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_time_step()'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_var_count(int *count)
	{
		if (!is_initialized) {
			*count = -1;
			return;
		}
		_log(LEVEL_DEBUG, "get_var_count ... ");
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_var_count", "()", NULL);
		if (PyLong_Check(pRes))
			*count = (int)PyLong_AsLong(pRes);
		else {
			*count = 0;
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_var_count()'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_var_name(int index, char *name)
	{
		if (!is_initialized) {
			name[0] = '\0';
			return;
		}
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_var_name", "(i)", index);
		if (PyUnicode_Check(pRes)) {
			Py_ssize_t size;
			const char* utf8 = PyUnicode_AsUTF8AndSize(pRes, &size);
			if (utf8 != NULL) {
				for (int i = 0; i < size; i++)
					name[i] = utf8[i];
				name[size] = '\0';
			}
			else {
				name[0] = '\0';
				_log(LEVEL_WARNING, "unexpected item from 'gm.get_var_name(index)'");
			}
		}
		else {
			name[0] = '\0';
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_var_name(index)'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_var_rank(const char *name, int *rank)
	{
		if (!is_initialized) {
			*rank = -1;
			return;
		}
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_var_rank", "(s)", name);
		if (PyLong_Check(pRes))
			*rank = (int)PyLong_AsLong(pRes);
		else {
			*rank = 0;
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_var_rank(name)'");
		}
		Py_DECREF(pRes);
	}

	BMI_API void get_var_shape(const char *name, int shape[MAXDIMS]) {
		if (!is_initialized) {
			for (int i = 0; i < MAXDIMS; i++)
				shape[i] = -1;
			return;
		}
		for (int i = 0; i < MAXDIMS; i++)
			shape[i] = 0;
		PyObject* pTuple = PyObject_CallMethod(pGM, (char*)"get_var_shape", "(s)", name);
		if (PyTuple_Check(pTuple)) {
			Py_ssize_t size = PyTuple_Size(pTuple);
			for (int i = 0; i < size; i++) {
				PyObject* pItem = PyTuple_GetItem(pTuple, i);
				if (PyLong_Check(pItem))
					shape[i] = (int)PyLong_AsLong(pItem);
				else
					_log(LEVEL_WARNING, "unexpected return item from 'gm.get_var_shape(name)'");
				// do not call Py_DECREF(pItem)
			}
		}
		else {
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_var_shape(name)'");
		}
		Py_DECREF(pTuple);
	}

	BMI_API void get_var_type(const char *name, char *type)
	{
		if (!is_initialized) {
			type[0] = '\0';
			return;
		}
		PyObject* pRes = PyObject_CallMethod(pGM, (char*)"get_var_type", "(s)", name);
		if (PyUnicode_Check(pRes)) {
			Py_ssize_t size;
			const char* utf8 = PyUnicode_AsUTF8AndSize(pRes, &size);
			if (utf8 != NULL) {
				for (int i = 0; i < size; i++)
					type[i] = utf8[i];
				type[size] = '\0';
			}
			else {
				type[0] = '\0';
				_log(LEVEL_WARNING, "unexpected item from 'gm.get_var_type(name)'");
			}
		}
		else {
			type[0] = '\0';
			_log(LEVEL_WARNING, "unexpected return type from 'gm.get_var_type(name)'");
		}
		Py_DECREF(pRes);

	}

	BMI_API void set_logger(Logger callback)
	{
		logger = callback;
		logger(LEVEL_DEBUG, "Logging attached to GroundwaterBMI");
	}
}

void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}

// placeholder function, all dll's need a main.. in windows only
#if defined _WIN32
int main()
{
}
#endif
