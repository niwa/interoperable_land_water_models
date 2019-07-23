#ifndef AGGREGATORCONFIGURATION_H
#define AGGREGATORCONFIGURATION_H

#include <string>

struct model_timer {
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
	std::string wtTableName = std::string("");
	std::string wtAggColName = std::string("");
	std::string wtEntityColName = std::string("");
	std::string wtWeightColName = std::string("");

	// table of inputs
	std::string itFormat = std::string("");
	std::string itFileName = std::string("");
	std::string itTableName = std::string("");
	std::string itValueColName = std::string("");
	std::string itEntityColName = std::string("");

	// table of outputs
	std::string otFormat = std::string("");
	std::string otFileName = std::string("");
	std::string otTableName = std::string("");
	std::string otValueColName = std::string("");
	std::string otAggColName = std::string("");
};

void get_table_info_from_yaml(const char*, model_tables_info*);

#endif