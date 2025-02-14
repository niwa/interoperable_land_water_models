// ArrayBuilder.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "BuilderYamlUtils.h"

static char transferFileName[MAXSTRINGLEN];
static int shape[MAXDIMS];
static double* values;
// How many values set so far?
static double talley = 0.0;
// rows, cols are convenient aliases for 
// shape[0], shape[1] while we're 
// limiting ourselves to 2D arrays
static int rows = 0;
static int cols = 0;

static char message[MAXSTRINGLEN];

static double currentTime = 0.0;
static double startTime = 0.0;
static double endTime = 5.0;
static double timeStep = 1.0;

/* YAML initializer*/
void initialize_from_yaml(const char* yaml_filename) {
	BuilderYamlUtils yml = BuilderYamlUtils();

	yml.initializeBuilderFromYaml(yaml_filename);
	rows = yml.shape[0];
	cols = yml.shape[1];
	shape[0] = rows;
	shape[1] = cols;
	strncpy_s(message, yml.message.c_str(), MAXSTRINGLEN);
	strncpy_s(transferFileName, yml.tranferFileName.c_str(), MAXSTRINGLEN);
}

/* reverses the order of elements in a null=terminated char array */
void reverse_string(char* str1) {
	char tmp;
	size_t l = strlen(str1);
	size_t m = l / 2;
	for (int i = 0; i < m; i++) {
		tmp = str1[i];
		str1[i] = str1[l - i - 1];
		str1[l - i - 1] = tmp;
	}
}

/***************
* The remainder of this file implements
* declarations given in "bmi.h"
***************/

/* Store callback */
Logger logger = NULL;

/* Logger function */
void _log(Level level, std::string msg);

/* control functions. These return an error code. */
int initialize(const char* config_file) {
	int i;
	std::cout << "\n\tBuilder: Initializing from "
		<< config_file << std::endl;
	for (i = 0; i < MAXSTRINGLEN; i++) {
		message[i] = '\0';
		transferFileName[i] = '\0';
	}
	for (i = 0; i < MAXDIMS; i++)shape[i] = 0;

	initialize_from_yaml(config_file);
	if (shape[0] > 0) rows = shape[0];
	else rows = 3;
	if (shape[1] > 0) cols = shape[1];
	else cols = 2;
	
	values = new double[rows * cols];
	
	std::cout << "\tBuilder: Message = " << message << "\n\tBuilder: Transfer file name = "
		<< transferFileName << '\n' << std::endl;

	return 0;
}

int update(double dt) {
	if (dt <= 0.) dt = timeStep;
	std::cout << "\n\tBuilder: Time = " << currentTime << std::endl;

	double now = currentTime;
	double doneTime = currentTime + dt;
	int nvals = rows * cols;
	while (now < (doneTime)) {
		if (now > currentTime)
			std::cout << "\n\tBuilder: Time = " << now << std::endl;
		now += timeStep;
		reverse_string(message);
		for (int i = 0; i < nvals; i++) values[i] = ++talley;
		std::cout << "\tBuilder: Message = " << message
			<< "\n\tBuilder: Talley = " << talley
			<< "\n\tBuilder: Values [min = "<< values[0] 
			<< "; max = " << values[nvals - 1] << ']' << std::endl;
	}

	BuilderYamlUtils yml;
	yml.message.assign(message);
	yml.shape[0] = rows; yml.shape[1] = cols;
	for (int i = 2; i < MAXDIMS; i++) yml.shape[i] = 0;
	yml.array = values;
	yml.writeStuffToYamlFile(transferFileName);
	currentTime = now;
	std::cout << "\tBuilder: Time = " << currentTime << '\n' << std::endl;
	return 0;
}

int finalize() {
	std::cout << "\n\tBuilder: Finalizing..." << '\n' << std::endl;
	delete[] values;
	return 0;
}

/* time control functions */
void get_start_time(double *t) { *t = startTime; }

void get_end_time(double *t) { *t = endTime; }

void get_current_time(double *t) { *t = currentTime; }

void get_time_step(double *dt) { *dt = timeStep; }

/* variable info */
void get_var_shape(const char *name, int shape[MAXDIMS]) {
	for (int i = 0; i < MAXDIMS; i++) {
		shape[i] = 0;
	}
	if (!strcmp(name, "count"))
		shape[0] = 1;
	else if (!strcmp(name, "message"))
		shape[0] = 1; // (int)strlen(message) + 1;
	else
		shape[0] = rows; shape[1] = cols;
}

void get_var_rank(const char *name, int *rank) {
	if (!strcmp(name, "count")) {
		*rank = 1;
	}
	else if (!strcmp(name, "message")) {
		*rank = 1;
	}
	else {
		*rank = 2;
	}
}

void get_var_type(const char *name, char *type) {
	if (!strcmp(name, "message")) {
		strncpy_s(type, MAXSTRINGLEN, "str", 4);
	}
	else {
		// all other variables are doubles
		strcpy_s(type, 7, "double");
	}
}

void get_var_count(int *talley) {*talley = 3;}

void get_var_name(int index, char *name) {
	if (index == 0)strcat_s(name, 6, "array");
	if (index == 1)strcat_s(name, 7, "talley");
	if (index == 2)strcat_s(name, 9, "message");
	if (index == 3)strcat_s(name, 17, "transferFileName");
}

/* get a pointer pointer - a reference to a multidimensional array */
void get_var(const char *name, void **ptr) {
	if (!strcmp(name, "message")) {
		*ptr = message;
	}
	else if (!strcmp(name, "transferFileName")) {
		*ptr = transferFileName;
	}
	else if (!strcmp(name, "talley")) {
		*ptr = &talley;
	}
	else {
		*ptr = values;
	}
}

/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char *name, const void *ptr) {
	if (!strncmp(name, "message", 8)) {
		strcpy_s(message, MAXSTRINGLEN, (char*)ptr);
	}
	if (!strncmp(name, "talley", 5)) {
		double* local = (double*)ptr;
		talley = *local;
	}
	if (!strncmp(name, "array", 5)) {
		double* newVals = (double*)ptr;
		for (int i = 0; i < rows*cols; i++)
			values[i] = newVals[i];
	}
	if (!strncmp(name, "transferFileName", 16)) {
		strcpy_s(transferFileName, MAXSTRINGLEN, (char*)ptr);
	}
}

void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to cxx model";
	logger = callback;
	logger(level, msg.c_str());
}

//void _log(Level level, std::string msg) {
//	if (logger != NULL) {
//		logger(level, msg.c_str());
//	}
//}
