// bmi_aggregator.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "AggregatorConfiguration.h"
#include "bmi.h"
#include <stdexcept>
#include <filesystem>
#include "sqlite3.h"

static model_times m_times;
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
	int status = 0;
	try {
		get_table_info_from_yaml(config_file, &tables_info);
	}
	catch (std::runtime_error e) {
		log_level = LEVEL_ERROR;
		msg.append(config_file);
		log(log_level, e.what());
		return -1;
	}
	if (initialize_sqlite_connections(&tables_info, &msg)) {
		log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	else {
		log(LEVEL_INFO, "Connected to SQLITE files.");
	}
	if (verify_sqlite_tables(&tables_info, &msg)) {
		// something's wrong with the table setup
		log(LEVEL_ERROR, msg.c_str());
		if (clean_up_connections(tables_info)) {
			log(LEVEL_ERROR, tables_info.errMsg);
		}
		else {
			log(LEVEL_INFO, "Closed SQLITE files and connections.");
		}
		return -1;
	}
	else {
		// Only the output database connection is used for updates
		if (tables_info.itDbConnection != tables_info.otDbConnection) {
			status = sqlite3_close(tables_info.itDbConnection);
			if (status == SQLITE_OK) {
				tables_info.itDbConnection = NULL;
			}
		}
		if (tables_info.wtDbConnection != tables_info.otDbConnection) {
			status = sqlite3_close(tables_info.wtDbConnection);
			if (status == SQLITE_OK) {
				tables_info.wtDbConnection = NULL;
			}
		}
		log(LEVEL_INFO, "SQLITE tables verified.");
	}
	return 0;
}

int update(double dt) {
	m_times.current_time += m_times.time_step;
	Level log_level = LEVEL_INFO;
	char step_str[20];
	int status;
	sprintf_s(step_str, "%lf3.1", m_times.current_time);
	std::string msg = "Update step: " + std::to_string(m_times.current_time);
	log(log_level, msg.c_str());

	std::string SQL_statement("");
	char* errMsg;

	// BLUNT INSTRUMENT!! Clearing output table completely. 
	SQL_statement.assign("DELETE FROM " + tables_info.otTableName + ";");
	status = sqlite3_exec(tables_info.otDbConnection, SQL_statement.c_str(), 0, 0, 0);
	if (status == SQLITE_OK) {
		msg = "Cleared old results from table " + tables_info.otTableName + '.';
		log(LEVEL_INFO, msg.c_str());
	}
	SQL_statement.clear();

	std::string itPrefix = "";
	std::string wtPrefix = "";
	if (tables_info.inputsDbAttached) {
		itPrefix = tables_info.itDbName + '.';
	}
	if (tables_info.weightsDbAttached) {
		wtPrefix = tables_info.wtDbName + '.';
	}

	SQL_statement.assign("INSERT INTO " + tables_info.otTableName
		+ " SELECT " + tables_info.wtAggColName + ", SUM("
		+ tables_info.itValueColName + " * " + tables_info.wtWeightColName + ")");
	if (tables_info.aggOperation.compare("AVERAGE") == 0) {
		SQL_statement.append("/SUM(" + tables_info.wtWeightColName + ")");
	}
	if (tables_info.aggScale.compare("1.0") != 0) {
		SQL_statement.append("*" + tables_info.aggScale );
	}
	SQL_statement.append(" AS "
		+ tables_info.otValueColName + " FROM " + wtPrefix + tables_info.wtTableName
		+ " INNER JOIN " + itPrefix + tables_info.itTableName + " ON "
		+ tables_info.itTableName + "." + tables_info.itEntityColName + " = "
		+ tables_info.wtTableName + "." + tables_info.wtEntityColName
		+ " GROUP BY " + tables_info.wtAggColName + ";");

	status = sqlite3_exec(tables_info.otDbConnection, SQL_statement.c_str(), 0, 0, &errMsg);
	if (status == SQLITE_ERROR) {
		log(LEVEL_ERROR, errMsg);
		sqlite3_free((void*)errMsg);
		return -1;
	}

	return 0;
}

int finalize() {
	std::string msg = "Finalizing aggregator: Closing database connections";
	log(LEVEL_INFO, msg.c_str());
	if (clean_up_connections(tables_info)) {
		log(LEVEL_ERROR, tables_info.errMsg);
	}
	else {
		log(LEVEL_INFO, "Closed SQLITE files and connections.");
	}
	msg = "Finished.";
	log(LEVEL_INFO, msg.c_str());
	return 0;
}

/* time control functions */
void get_start_time(double *t) {
	*t = m_times.start_time;
}

void get_end_time(double *t) {
	*t = m_times.end_time;
}

void get_current_time(double *t) {
	*t = m_times.current_time;
}

void get_time_step(double *dt) {
	*dt = m_times.time_step;
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
	std::string msg = "Logging attached to BMI Aggregator";
	logger = callback;
	logger(level, msg.c_str());
}


