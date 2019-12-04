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
	YAML::Node aggTableNode = tablesConfig["aggregate"];

	if (not weightTableNode.IsMap()) { 
		hasRuntimeError = true;
		errMsg.append("No Weights table definition.\n");
	}
	if (not inputTableNode.IsMap()) {
		hasRuntimeError = true;
		errMsg.append("No Input table definition.\n");
	}
	if (not outputTableNode.IsMap()) {
		hasRuntimeError = true;
		errMsg.append("No Output table definition.\n");
	}
	if (hasRuntimeError) { throw std::runtime_error(errMsg); }

	// table of weights
	try {
	tables_info->wtFormat.assign(weightTableNode["format"].as<std::string>());
	tables_info->wtFileName.assign(weightTableNode["path"].as<std::string>());
	tables_info->wtTableName.assign(weightTableNode["table"].as<std::string>());
	tables_info->wtAggColName.assign(weightTableNode["aggregate_column"].as<std::string>());
	tables_info->wtEntityColName.assign(weightTableNode["entity_column"].as<std::string>());
	tables_info->wtWeightColName.assign(weightTableNode["weight_column"].as<std::string>());
	}
	catch (YAML::ParserException) {
		errMsg.append("Error in 'inputs' section of config file");
		throw(std::runtime_error(errMsg));
	}

	// table of inputs
	try {
	tables_info->itFormat.assign(inputTableNode["format"].as<std::string>());
	tables_info->itFileName.assign(inputTableNode["path"].as<std::string>());
	tables_info->itTableName.assign(inputTableNode["table"].as<std::string>());
	tables_info->itValueColName.assign(inputTableNode["value_column"].as<std::string>());
	tables_info->itEntityColName.assign(inputTableNode["entity_column"].as<std::string>());
	}
	catch (YAML::ParserException) {
		errMsg.append("Error in 'weights' section of config file");
		throw(std::runtime_error(errMsg));
	}

	// table of outputs
	try {
	tables_info->otFormat.assign(outputTableNode["format"].as<std::string>());
	tables_info->otFileName.assign(outputTableNode["path"].as<std::string>());
	tables_info->otTableName.assign(outputTableNode["table"].as<std::string>());
	tables_info->otValueColName.assign(outputTableNode["value_column"].as<std::string>());
	tables_info->otAggColName.assign(outputTableNode["aggregate_column"].as<std::string>());
	}
	catch (YAML::ParserException) {
		errMsg.append("Error in 'outputs' section of config file");
		throw(std::runtime_error(errMsg));
	}

	// aggregation settings

	if (aggTableNode.IsMap()) {
		if (aggTableNode["operation"]) {
			// Choices are SUM or AVERAGE (default)
			tables_info->aggOperation.assign(
				aggTableNode["operation"].as<std::string>());
			// capitalize
			for (char &c : tables_info->aggOperation) c = toupper(c);
			if (tables_info->aggOperation.compare("SUM")
				&& tables_info->aggOperation.compare("AVERAGE"))
			{
				errMsg.append(tables_info->aggOperation
				+ " not recognized.");
				throw(std::runtime_error(errMsg));
			}
		}
		if (aggTableNode["scale"]){
			tables_info->aggScale.assign(aggTableNode["scale"].as<std::string>());
			try { std::stod(tables_info->aggScale.c_str(), nullptr); }
			catch (std::invalid_argument) {
				errMsg.append(tables_info->aggScale
					+ " is not convertable to a number.");
				throw(std::runtime_error(errMsg));
			}

		}
	}
	return;
}

int initialize_sqlite_connections(model_tables_info* info, std::string *msg) {

	std::string SQL_statement;
	msg->clear();
	// Check files, tables, fields
	info->itPath = info->itFileName;
	info->wtPath = info->wtFileName;
	info->otPath = info->otFileName;

	int status = sqlite3_open(info->otPath.string().c_str(), &(info->otDbConnection));
	if (status != SQLITE_OK) {
		msg->assign("Can't open SQLITE file: " + info->otPath.string() + " for output.");
		info->otDbConnection = NULL;
		return -1;
	}

	//TODO: Finish ATTACH statements and consequences for input and weight tables.

	if (equivalent(info->itPath, info->otPath)) {
		info->itDbConnection = info->otDbConnection;
	}
	else {
		SQL_statement = "ATTACH DATABASE '" + info->itPath.string() +
			"' AS " + info->itDbName + ";";
		status = sqlite3_exec(info->otDbConnection, SQL_statement.c_str(), 0, 0, 0);
		if (status == SQLITE_OK) {
			info->inputsDbAttached = true;
		}
		status = sqlite3_open(info->itPath.string().c_str(), &(info->itDbConnection));
	}
	if (equivalent(info->wtPath, info->otPath)) {
		info->wtDbConnection = info->otDbConnection;
	}
	else {
		SQL_statement = "ATTACH DATABASE '" + info->wtPath.string() +
			"' AS " + info->wtDbName + ";";
		status = sqlite3_exec(info->otDbConnection, SQL_statement.c_str(), 0, 0, 0);
		if (status == SQLITE_OK) {
			info->weightsDbAttached = true;
		}
		if (equivalent(info->wtPath, info->itPath)) {
			info->wtDbConnection = info->itDbConnection;
		}
		else {
			status = sqlite3_open(info->wtPath.string().c_str(), &(info->wtDbConnection));
		}
	}
	return 0;
}

int verify_sqlite_tables(model_tables_info* info, std::string* msg) {
	// Test that columns for input values, zones, and weights are present
	// in the SQLITE tables.
	// TODO: Refactor into something more compact.
	// This section badly violates DRY programming practice 

	// These variables are reused for all tables and column inquiries
	const char *typeStr, *collationStr;
	int notNullFlag, pKeyFlag, autoIncFlag, status;

	// Weight table tests
	status = sqlite3_table_column_metadata(info->wtDbConnection, NULL,
		info->wtTableName.c_str(), NULL, &typeStr, &collationStr,
		&notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Table " + info->wtTableName + " not found in database " +
			info->wtFileName);
		//log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	status = sqlite3_table_column_metadata(info->wtDbConnection, NULL,
		info->wtTableName.c_str(), info->wtWeightColName.c_str(),
		&typeStr, &collationStr, &notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Column " + info->wtWeightColName + " not found in weights table " +
			info->wtTableName);
		//log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	info->wtWeightColType.assign(typeStr);
	status = sqlite3_table_column_metadata(info->wtDbConnection, NULL,
		info->wtTableName.c_str(), info->wtEntityColName.c_str(),
		&typeStr, &collationStr, &notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Column " + info->wtEntityColName + " not found in weights table " +
			info->wtTableName);
		//log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	info->wtEntityColType.assign(typeStr);
	status = sqlite3_table_column_metadata(info->wtDbConnection, NULL,
		info->wtTableName.c_str(), info->wtAggColName.c_str(),
		&typeStr, &collationStr, &notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Column " + info->wtAggColName + " not found in weights table " +
			info->wtTableName);
		// log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	info->wtAggColType.assign(typeStr);

	// Input table tests
	status = sqlite3_table_column_metadata(info->itDbConnection, NULL,
		info->itTableName.c_str(), NULL, &typeStr, &collationStr,
		&notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Table " + info->itTableName + " not found in database " +
			info->wtFileName);
		// log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	status = sqlite3_table_column_metadata(info->itDbConnection, NULL,
		info->itTableName.c_str(), info->itEntityColName.c_str(),
		&typeStr, &collationStr, &notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Column " + info->itEntityColName + " not found in inputs table " +
			info->itTableName);
		//log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	info->itEntityColType.assign(typeStr);
	status = sqlite3_table_column_metadata(info->itDbConnection, NULL,
		info->itTableName.c_str(), info->itValueColName.c_str(),
		&typeStr, &collationStr, &notNullFlag, &pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		msg->assign("Column " + info->itValueColName + " not found in inputs table " +
			info->itTableName);
		// log(LEVEL_ERROR, msg.c_str());
		return -1;
	}
	info->itValueColType.assign(typeStr);

	// Output table -- if not present, create it
	status = sqlite3_table_column_metadata(info->otDbConnection, NULL,
		info->otTableName.c_str(), NULL, &typeStr, &collationStr, &notNullFlag,
		&pKeyFlag, &autoIncFlag);
	if (status == SQLITE_ERROR) {
		std::string SQL_statement = "CREATE TABLE " + info->otTableName + "('" +
			info->otAggColName + "' " + info->wtAggColType + ", '" +
			info->otValueColName + "' " + info->itValueColType +
			", PRIMARY KEY('" + info->otAggColName + "'));";
		status = sqlite3_exec(info->otDbConnection, SQL_statement.c_str(), 0, 0, 0);
	}
	else {
		// BLUNT INSTRUMENT!! Clearing output table completely. 
		std::string SQL_statement = "DELETE FROM " + info->otTableName + ";";
		status = sqlite3_exec(info->otDbConnection, SQL_statement.c_str(), 0, 0, 0);
	}
	return 0;
}

// The AggregatorConfiguration structure holds all the database connections,
// so this is where they will be shut down
int clean_up_connections(model_tables_info info) {
	int status = 0;
	std::string SQL_statement("");
	char* errMsg;
	if (info.inputsDbAttached) {
		SQL_statement = "DETACH DATABASE " + info.itDbName;
		status = sqlite3_exec(info.otDbConnection, SQL_statement.c_str(), 0, 0, &errMsg);
		if (status == SQLITE_ERROR) {
			info.errMsg.append(errMsg + '\n');
			sqlite3_free((void*)errMsg);
		}
		else {
			info.inputsDbAttached = false;
		}
	}
	if (info.weightsDbAttached) {
		SQL_statement = "DETACH DATABASE " + info.wtDbName;
		status = sqlite3_exec(info.otDbConnection, SQL_statement.c_str(), 0, 0, &errMsg);
		if (status == SQLITE_ERROR) {
			info.errMsg.append(errMsg + '\n');
			sqlite3_free((void*)errMsg);
		}
		else {
			info.weightsDbAttached = false;
		}
	}
	if (info.otDbConnection != NULL) {
		status = sqlite3_close(info.otDbConnection);
		if (status == SQLITE_ERROR) {
			info.errMsg.append("Error closing connection to "
				+ info.otFileName + '\n');
		}
	}
	if (info.itDbConnection != NULL) {
		status = sqlite3_close(info.itDbConnection);
		if (status == SQLITE_ERROR) {
			info.errMsg.append("Error closing connection to "
				+ info.itFileName + '\n');
		}
	}
	if (info.wtDbConnection != NULL) {
		status = sqlite3_close(info.wtDbConnection);
		if (status == SQLITE_ERROR) {
			info.errMsg.append("Error closing connection to "
				+ info.wtFileName + '\n');
		}
	}
	if (info.errMsg.length() > 0) return -1;
	return 0;
}

