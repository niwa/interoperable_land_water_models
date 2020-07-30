#ifndef AGGREGATORCONFIGURATION_H
#define AGGREGATORCONFIGURATION_H

#include <string>
#include "sqlite3.h"
#include <filesystem>

// Prior to C++ v 17
namespace fs = std::experimental::filesystem;
// C++ v 17 and later
// namespace fs = std::filesystem;

struct model_times {
public:
	double current_time = 0.0;
	double time_step = 1.0;
	double start_time = 0.0;
	double end_time = 1.0;
};

struct model_tables_info {
public:
	// table of weights
	std::string wtFormat = std::string("");
	std::string wtFileName = std::string("");
	fs::path wtPath;
	sqlite3 *wtDbConnection = NULL;
	std::string wtDbName = "WeightsDB";
	bool weightsDbAttached = false;
	std::string wtTableName = std::string("");
	std::string wtAggColName = std::string("");
	std::string wtAggColType = std::string("");
	std::string wtEntityColName = std::string("");
	std::string wtEntityColType = std::string("");
	std::string wtWeightColName = std::string("");
	std::string wtWeightColType = std::string("");

	// aggregator operation
	std::string aggOperation = std::string("AVERAGE");
	std::string aggScale = std::string("1.0");

	// table of inputs
	std::string itFormat = std::string("");
	std::string itFileName = std::string("");
	fs::path itPath;
	sqlite3 *itDbConnection = NULL;
	std::string itDbName = "ValuesDB";
	bool inputsDbAttached = false;
	std::string itTableName = std::string("");
	std::string itValueColName = std::string("");
	std::string itValueColType = std::string("");
	std::string itEntityColName = std::string("");
	std::string itEntityColType = std::string("");

	// table of outputs
	std::string otFormat = std::string("");
	std::string otFileName = std::string("");
	fs::path otPath;
	sqlite3 *otDbConnection = NULL;
	std::string otTableName = std::string("");
	std::string otValueColName = std::string("");
	std::string otAggColName = std::string("");

	// space for error messages
	std::string errMsg = std::string("");
};

void get_table_info_from_yaml(const char*, model_tables_info*);
int initialize_sqlite_connections(model_tables_info*, std::string*);
int verify_sqlite_tables(model_tables_info*, std::string*);
int clean_up_connections(model_tables_info info);
#endif