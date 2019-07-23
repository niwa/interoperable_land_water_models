// Aggregator.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "AggregatorConfiguration.h"
#include "bmi.h"
#include <stdexcept>
#include "sqlite3.h"

static model_timer timer;
static model_tables_info tables_info;
static Logger logger = nullptr;

// Wrap logger to check for nullptr
static void log(Level level, std::string msg) {
	if (logger != nullptr) {
		logger(level, msg.c_str());
	}
}

/* control functions. These return an error code. */
int initialize(const char *config_file) {
	Level log_level = LEVEL_INFO;
	std::string msg = "Initializing from configuration file: ";
	msg.append(config_file);
	log(log_level, msg.c_str());
	try {
		get_table_info_from_yaml(config_file, &tables_info);
	}
	catch (std::runtime_error e) {
		log_level = LEVEL_ERROR;
		msg.append(config_file);
		log(log_level, e.what());
		return -1;
	}
	return 0;
}

int update(double dt) {
	timer.current_time += timer.time_step;
	Level log_level = LEVEL_INFO;
	char step_str[20];
	sprintf_s(step_str, "%lf3.1", timer.current_time);
	std::string msg = "Update step: " + std::to_string(timer.current_time);
	log(log_level, msg.c_str());

	sqlite3 *outputDbConnection;
	int status = sqlite3_open(tables_info.otFileName.c_str(), &outputDbConnection);

	// BLUNT INSTRUMENT!! Clearing output table completely. 
	std::string SQL_statement = "DELETE FROM " + tables_info.otTableName + ";";
	status = sqlite3_exec(outputDbConnection, SQL_statement.c_str(), 0, 0, 0);

	SQL_statement.assign("INSERT INTO " + tables_info.otTableName
		+ " SELECT " + tables_info.wtAggColName + ", SUM("
		+ tables_info.itValueColName + " * " + tables_info.wtWeightColName
		+ ")/SUM(" + tables_info.wtWeightColName + ") AS "
		+ tables_info.otValueColName + " FROM " + tables_info.wtTableName
		+ " INNER JOIN " + tables_info.itTableName + " ON "
		+ tables_info.itTableName + "." + tables_info.itEntityColName + " = "
		+ tables_info.wtTableName + "." + tables_info.wtEntityColName
		+ " GROUP BY " + tables_info.wtAggColName + ";");
	status = sqlite3_exec(outputDbConnection, SQL_statement.c_str(), 0, 0, 0);

	status = sqlite3_close(outputDbConnection);

	return 0;
}

int finalize() {
	Level log_level = LEVEL_INFO;
	std::string msg = "Finalizing aggregator.";
	log(log_level, msg.c_str());
	return 0;
}

/* time control functions */
void get_start_time(double *t) {
	*t = timer.start_time;
}

void get_end_time(double *t) {
	*t = timer.end_time;
}

void get_current_time(double *t) {
	*t = timer.current_time;
}

void get_time_step(double *dt) {
	*dt = timer.time_step;
}


/* variable info */
void get_var_shape(const char *name, int shape[MAXDIMS]) {
	return;
}

void get_var_rank(const char *name, int *rank) {
	return;
}

void get_var_type(const char *name, char *type) {
	return;
}

void get_var_count(int *count) {
	*count = 0;
	return;
}

void get_var_name(int index, char *name) {
	return;
}

/* get a pointer pointer - a reference to a multidimensional array */
void get_var(const char *name, void **ptr) {
	return;
}

/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char *name, const void *ptr) {
	return;
}

/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
void set_var_slice(const char *name, const int *start, const int *count, const void *ptr) {
	return;
}

/* logger to be set from outside so we can log messages */
typedef void (CALLCONV *Logger)(Level level, const char *msg);

/* set logger by setting a pointer to the log function */
void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to BMI Iterator";
	logger = callback;
	logger(level, msg.c_str());
}


