#pragma once

#include "AggregatorConfiguration.h"
#include <stdexcept>
#include "yaml-cpp/yaml.h"

void get_table_info_from_yaml(const char *config_file, model_tables_info *tables_info) {

	bool hasRuntimeError = false;
	std::string errMsg = "";

	YAML::Node tablesConfig= YAML::LoadFile(config_file);

	if (tablesConfig.Type() != YAML::NodeType::Map) {
		errMsg.append("Invalid YAML contents for config file.");
		throw(std::runtime_error(errMsg));
	}
	YAML::Node weightTableNode = tablesConfig["weights"];
	YAML::Node inputTableNode = tablesConfig["input"];
	YAML::Node outputTableNode = tablesConfig["output"];

	if (not weightTableNode) { 
		hasRuntimeError = true;
		errMsg.append("No Weights table definition.\n");
	}
	if (not inputTableNode) {
		hasRuntimeError = true;
		errMsg.append("No Input table definition.\n");
	}
	if (not outputTableNode) {
		hasRuntimeError = true;
		errMsg.append("No Output table definition.\n");
	}
	if (hasRuntimeError) { throw std::runtime_error(errMsg); }


	// table of weights
	tables_info->wtFormat.assign(weightTableNode["format"].as<std::string>());
	tables_info->wtFileName.assign(weightTableNode["path"].as<std::string>());
	tables_info->wtTableName.assign(weightTableNode["table"].as<std::string>());
	tables_info->wtAggColName.assign(weightTableNode["aggregate_column"].as<std::string>());
	tables_info->wtEntityColName.assign(weightTableNode["entity_column"].as<std::string>());
	tables_info->wtWeightColName.assign(weightTableNode["weight_column"].as<std::string>());

	// table of inputs
	tables_info->itFormat.assign(inputTableNode["format"].as<std::string>());
	tables_info->itFileName.assign(inputTableNode["path"].as<std::string>());
	tables_info->itTableName.assign(inputTableNode["table"].as<std::string>());
	tables_info->itValueColName.assign(inputTableNode["value_column"].as<std::string>());
	tables_info->itEntityColName.assign(inputTableNode["entity_column"].as<std::string>());

	// table of outputs
	tables_info->otFormat.assign(outputTableNode["format"].as<std::string>());
	tables_info->otFileName.assign(outputTableNode["path"].as<std::string>());
	tables_info->otTableName.assign(outputTableNode["table"].as<std::string>());
	tables_info->otValueColName.assign(outputTableNode["value_column"].as<std::string>());
	tables_info->otAggColName.assign(outputTableNode["aggregate_column"].as<std::string>());

	return;
}