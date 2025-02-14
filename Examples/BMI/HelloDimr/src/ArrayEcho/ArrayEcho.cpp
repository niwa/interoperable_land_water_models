// ArrayEcho.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "EchoYamlParser.h"

static char transferFileName[MAXSTRINGLEN];
static int shape[MAXDIMS];
static double* values;
// How many values parsed so far?
static double nValues = 0.0;
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

static bool pauseOnUpdate = true;

/* YAML initializer*/
void initialize_from_yaml(const char* yaml_filename) {
	EchoYamlParser yml = EchoYamlParser();

	//yml.initializeBuilderFromYaml(yaml_filename);
	yml.parseArrayConfiguration(yaml_filename);
	rows = yml.shape[0];
	cols = yml.shape[1];
	shape[0] = rows;
	shape[1] = cols;
	strncpy_s(message, yml.message.c_str(), MAXSTRINGLEN);
	strncpy_s(transferFileName, yml.tranferFileName.c_str(), MAXSTRINGLEN);
	if (yml.testMode) pauseOnUpdate = false;
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
	std::cout << "\n\tEcho: Initializing from " << config_file << std::endl;
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

	values = new double[rows*cols];

	std::cout << "\tEcho: Transfer file name = " << transferFileName << '\n' << std::endl;

	return 0;
}

int update(double dt) {
	std::cout << "\n\tEcho: Time = " << currentTime << std::endl;

	// Read updated data from transfer file
	EchoYamlParser yp;
	yp.parseArrayData(transferFileName, shape, values);
	// "nValues" may come from DIMR or from the transfer file
	// take the value from the transfer file if it's available
	if (yp.nValues > 0) nValues = yp.nValues;
	strncpy_s(message, yp.message.c_str(), yp.message.length() + 1);

	double now = currentTime;
	while (now < (currentTime + dt)) {
		if (now > currentTime)
			std::cout << "\tEcho: Time = " << now << '\n';
		now += timeStep;
		std::cout << "\tEcho: Message = " << message
			<< "\n\tEcho: Talley = " << nValues
			<< "\n\tEcho: Values = \n";
		for (int i = 0; i < rows; i++) {
			std::cout << "\t\t";
			for (int j = 0; j < cols; j++) {
				std::cout << values[i*cols+j];
				if (j < cols - 1) std::cout << ", ";
				else std::cout << '\n';
			}
		}
	}
	currentTime = now;

	if (pauseOnUpdate) {
		std::cout << "\tEcho: Time = " << currentTime << '\n'
			<< "Press enter to continue...\n" << std::endl;
		char c;
		c = getc(stdin);
	}

	return 0;
}

int finalize() {
	std::cout << "\n\tEcho: Finalizing..." << '\n' << std::endl;
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
	if (!strcmp(name, "talley"))
		shape[0] = 1;
	else if (!strcmp(name, "message") || !strcmp(name, "transferFileName"))
		shape[0] = 1; // (int)strlen(message) + 1;
	else
		shape[0] = rows; shape[1] = cols;
}

void get_var_rank(const char *name, int *rank) {
	if (!strcmp(name, "talley"))
		*rank = 1;
	else if (!strcmp(name, "message") || !strcmp(name, "transferFileName"))
		*rank = 1;
	else
		*rank = 2; }

void get_var_type(const char *name, char *type) {
	if (!strcmp(name, "message") || !strcmp(name, "transferFileName")) {
		strncpy_s(type, MAXSTRINGLEN, "str", 4);
	}
	else {
		// all other variables are doubles
		strcpy_s(type, 7, "double");
	}
}

void get_var_count(int *count) { *count = 4; }

void get_var_name(int index, char *name) {
	if (index == 0)strcat_s(name, 6, "array");
	if (index == 1)strcat_s(name, 7, "talley");
	if (index == 2)strcat_s(name, 9, "message");
	if (index == 3)strcat_s(name, 17, "transferFileName");
	if (index < 0 || index > 3)strcat_s(name, 1, "\0");
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
		*ptr = &nValues;
	}
	else {
		*ptr = values;
	}
}

/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char *name, const void *ptr) {
	if (!strncmp(name, "talley", 5)) {
		double* local = (double*) ptr;
		nValues = *local;
	}
	else if (!strncmp(name, "message", 8)) {
		size_t i = 0;
		char* local = (char*)ptr;
		i = strlen(local);
		strncpy_s(message, i+1, local, MAXSTRINGLEN);
	}
	else if (!strncmp(name, "transferFileName", 16)) {
		size_t i = 0;
		char* local = (char*)ptr;
		i = strlen(local);
		strncpy_s(transferFileName, i + 1, local, MAXSTRINGLEN);
	}
	if (!strncmp(name, "array", 5)) {
		memcpy(values, ptr, rows*cols);
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
