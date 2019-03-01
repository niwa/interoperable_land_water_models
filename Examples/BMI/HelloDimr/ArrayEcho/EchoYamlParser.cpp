#include "stdafx.h"
#include "EchoYamlParser.h"


EchoYamlParser::EchoYamlParser()
{
}


EchoYamlParser::~EchoYamlParser()
{
}

void EchoYamlParser::parseArrayConfiguration(const char* configFileName) {
	/* Another YAML parsing method using the libyaml events interface.
	More hodgepodge of C and C++ styles. */
	yaml_parser_t parser;
	std::string errmsg;
	std::string tempString;
	std::stack<std::string> keyStack;
	FILE *fh;

	yaml_event_t event;
	int done = 0;
	int event_count = 0;

	/* Create YAML parser for named input file */
	if (fopen_s(&fh, configFileName, "r")) {
		errmsg = "Could not open file: ";
		errmsg.append(configFileName);
		throw std::runtime_error(errmsg);
	}
	if (!yaml_parser_initialize(&parser))
		throw std::runtime_error("Failed to initialize yaml parser!");
	yaml_parser_set_input_file(&parser, fh);
	if (parser.error != YAML_NO_ERROR) {
		throw std::runtime_error("Errors in yaml file!");
	}

	//yaml_document_t doc;
	//yaml_parser_load(&parser, &doc);

	int inMapping = 0, seekingKey = 0, inSequence = 0, seqIndex = 0;
	// Here's the loop that reads the YAML contents.
	while (!done)
	{
		if (!yaml_parser_parse(&parser, &event)) {
			errmsg = "Error in yaml file at event ";
			errmsg.append(std::to_string(event_count));
			throw std::runtime_error(errmsg);
			break;
		}
		if (event.type == YAML_SCALAR_EVENT) {
			if (seekingKey) {
				keyStack.push((std::string)(char*)event.data.scalar.value);
				seekingKey--;
			}
			else if (inSequence) {
				if (keyStack.top() == "shape") {
					if (seqIndex >= MAXDIMS) {
						errmsg = "Shape array length exceeds BMI MAXDIMS";
						throw std::runtime_error(errmsg);
						break;
					}
					shape[seqIndex++] = atoi((char*)event.data.scalar.value);
				}
			}
			else {
				if (keyStack.top() == "relayFile") {
					tranferFileName = (std::string)(char*)event.data.scalar.value;
				}
				else if (keyStack.top() == "message") {
					message = (std::string)(char*)event.data.scalar.value;
				}
				else if (keyStack.top() == "testMode") {
					testMode = true;
				}
				keyStack.pop();
				seekingKey++;
			}
		}
		else if (event.type == YAML_MAPPING_START_EVENT) {
			inMapping = 1;
			seekingKey++;
		}
		else if (event.type == YAML_MAPPING_END_EVENT) {
			inMapping = 0;
		}
		else if (event.type == YAML_SEQUENCE_START_EVENT) {
			inSequence = 1;
			seqIndex = 0;
		}
		else if (event.type == YAML_SEQUENCE_END_EVENT) {
			inSequence = 0;
			seekingKey++;
		}

		done = (event.type == YAML_STREAM_END_EVENT);

		yaml_event_delete(&event);

		event_count++;
	}
	// Cleanup
	yaml_parser_delete(&parser);
	fclose(fh);
}

void EchoYamlParser::parseArrayData(const char* dataFileName, int modelShape[MAXDIMS], double* modelValues) {

	/* Another YAML parsing method using the libyaml events interface.
	More hodgepodge of C and C++ styles. */

	/******************
	* Variables modelShape and modelValues are so named to distinguish them from 
	* the shape and value arrays that are read from the data file. 
	******************/
	yaml_parser_t parser;
	std::string errmsg;
	std::string tempString;
	std::stack<std::string> keyStack;
	FILE *fh;

	// Prepare for some consistency tests between what's in our configuration
	// and what's in this time step's data file.
	// How big do we expect our data array to be?
	int nVals = 1;
	for (int i = 0; i < MAXDIMS; i++) {
		shape[i] = 0;
		if (modelShape[i] > 1) nVals *= modelShape[i]; // from initialization
	}

	yaml_event_t event;
	int done = 0;
	int event_count = 0;

	/* Create YAML parser for named input file */
	if (fopen_s(&fh, dataFileName, "r")) {
		errmsg = "Could not open file: ";
		errmsg.append(dataFileName);
		throw std::runtime_error(errmsg);
	}
	if (!yaml_parser_initialize(&parser))
		throw std::runtime_error("Failed to initialize yaml parser!");
	yaml_parser_set_input_file(&parser, fh);
	if (parser.error != YAML_NO_ERROR) {
		throw std::runtime_error("Errors in yaml file!");
	}

	//yaml_document_t doc;
	//yaml_parser_load(&parser, &doc);

	int inMapping = 0, seekingKey = 0, inSequence = 0, seqIndex = 0;
	// Here's the loop that reads the YAML contents.
	while (!done)
	{
		if (!yaml_parser_parse(&parser, &event)) {
			errmsg = "Error in yaml file at event ";
			errmsg.append(std::to_string(nValues));
			throw std::runtime_error(errmsg);
			break;
		}
		if (event.type == YAML_SCALAR_EVENT) {
			if (seekingKey) {
				keyStack.push((std::string)(char*)event.data.scalar.value);
				seekingKey--;
			}
			else if (inSequence) {
				if (keyStack.top() == "shape") {
					if (seqIndex >= MAXDIMS) {
						errmsg = "Shape array length exceeds BMI MAXDIMS";
						throw std::runtime_error(errmsg);
						break;
					}
					shape[seqIndex++] = atoi((char*)event.data.scalar.value);
				}
				if (keyStack.top() == "array") {
					if (seqIndex >= nVals) {
						errmsg = "Data array length exceeds data shape";
						throw std::runtime_error(errmsg);
						break;
					}
					modelValues[seqIndex++] = atof((char*)event.data.scalar.value);
				}
			}
			else {
				if (keyStack.top() == "message") {
					message = (std::string)(char*)event.data.scalar.value;
				}
				keyStack.pop();
				seekingKey++;
			}
		}
		else if (event.type == YAML_MAPPING_START_EVENT) {
			inMapping = 1;
			seekingKey++;
		}
		else if (event.type == YAML_MAPPING_END_EVENT) {
			inMapping = 0;
		}
		else if (event.type == YAML_SEQUENCE_START_EVENT) {
			inSequence = 1;
			seqIndex = 0;
		}
		else if (event.type == YAML_SEQUENCE_END_EVENT) {
			if (keyStack.top() == "shape") {
				for (int i = 0; i < MAXDIMS; i++) {
					if (modelShape[i] != shape[i]) {
						throw std::runtime_error("Array shape changed unexpectedly!");
					}
				}
				keyStack.pop();
			}
			else if (keyStack.top() == "array") {
				keyStack.pop();
			}
			inSequence = 0;
			seekingKey++;
		}

		done = (event.type == YAML_STREAM_END_EVENT);

		yaml_event_delete(&event);

		event_count++;
	}
	// Cleanup
	yaml_parser_delete(&parser);
	fclose(fh);
}

