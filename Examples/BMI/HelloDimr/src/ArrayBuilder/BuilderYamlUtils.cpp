#include "stdafx.h"
#include "BuilderYamlUtils.h"

/*
Implemented as a self-teaching exercise for libyaml using events to drive the 
emitter and the parser.

The code style here is gosh-awful mess. This C++ class uses the libyaml
library, which is written in C. I've tried to use camelCase elsewhere in this 
project, so the naming style is inconsistent. I can hope that this will be 
abstracted into just this class, so it shouldn't matter.

TODO: Provide alternatives when a condition isn't met because some perfectly
legal YAML is present but it's not meaningful to this parser.

*/

BuilderYamlUtils::BuilderYamlUtils()
{
}

BuilderYamlUtils::~BuilderYamlUtils()
{
}


void BuilderYamlUtils::initializeBuilderFromYaml(const char * yaml_filename)
{
	/* Trying to provide an example of YAML parsing using YAML events.
	An unwholesome blend of C and C++ styles, perhaps.*/
	yaml_parser_t parser;
	std::string errmsg;
	std::string tempString;
	std::stack<std::string> keyStack;
	FILE *fh;

	yaml_event_t event;
	int done = 0;
	int talley = 0;

	/* Create YAML parser for named input file */
	if (fopen_s(&fh, yaml_filename, "r")) {
		errmsg = "Could not open file: ";
		errmsg.append(yaml_filename);
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
			errmsg.append(std::to_string(talley));
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
					if (seqIndex >= MAXDIMS){
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

		talley++;
	}
	// Cleanup
	yaml_parser_delete(&parser);
	fclose(fh);
}

void BuilderYamlUtils::writeStuffToYamlFile(const char* outFileName) {
	FILE *fh;
	std::string errmsg;
	yaml_emitter_t emitter;
	yaml_event_t event;

	int canonical = 0;
	int unicode = 0;
	char tempStr[16];

	/* Create YAML parser for named input file */
	if (fopen_s(&fh, outFileName, "wb")) {
		errmsg = "Could not open file: ";
		errmsg.append(outFileName);
		throw std::runtime_error(errmsg);
	}

	if (!yaml_emitter_initialize(&emitter)) {
		errmsg = "Could not initalize the emitter object.";
		throw std::runtime_error(errmsg);
	}

	yaml_emitter_set_output_file(&emitter, fh);
	yaml_emitter_set_canonical(&emitter, canonical);
	yaml_emitter_set_unicode(&emitter, unicode);

	int ok;
	char anchor[256];
	char tag[256];
	int implicit = 0; // 0=> tags are explicit

	// YAML file and document setup
	ok = yaml_stream_start_event_initialize(&event,
		YAML_UTF8_ENCODING);
	yaml_emitter_emit(&emitter, &event);
	ok = yaml_document_start_event_initialize(
		&event, NULL, NULL, NULL, implicit);
	yaml_emitter_emit(&emitter, &event);

	// Begin the mapping (key-value pair) output
	ok = yaml_mapping_start_event_initialize(
		&event, NULL, NULL, 1, YAML_ANY_MAPPING_STYLE);
	yaml_emitter_emit(&emitter, &event);
	implicit = 1; // 1=> tags are implicit

	// Transfering the message string
	ok = yaml_scalar_event_initialize(
		&event, NULL, NULL, (yaml_char_t *)"message", -1,
		implicit, implicit, YAML_PLAIN_SCALAR_STYLE); // message key
	yaml_emitter_emit(&emitter, &event);
	ok = yaml_scalar_event_initialize(
		&event, NULL, NULL, (yaml_char_t *)message.c_str(), -1,
		implicit, implicit, YAML_PLAIN_SCALAR_STYLE); // message value
	yaml_emitter_emit(&emitter, &event);

	// Transfering the shape the data array
	int nVals = 1;
	ok = yaml_scalar_event_initialize(
		&event, NULL, NULL, (yaml_char_t *)"shape", -1,
		implicit, implicit, YAML_PLAIN_SCALAR_STYLE); // array key
	yaml_emitter_emit(&emitter, &event);
	ok = yaml_sequence_start_event_initialize(
		&event, NULL, NULL, implicit, YAML_FLOW_SEQUENCE_STYLE
	);
	yaml_emitter_emit(&emitter, &event);
	for (int i = 0; i < MAXDIMS; i++) {
		if (shape[i] > 1) nVals *= shape[i];
		sprintf_s(tempStr, "%d", shape[i]);
		ok = yaml_scalar_event_initialize(
			&event, NULL, NULL, (yaml_char_t *)tempStr, -1,
			implicit, implicit, YAML_PLAIN_SCALAR_STYLE);
		yaml_emitter_emit(&emitter, &event);
	}
	ok = yaml_sequence_end_event_initialize(&event);
	yaml_emitter_emit(&emitter, &event);

	// Transfering the values in the data array itself
	ok = yaml_scalar_event_initialize(
		&event, NULL, NULL, (yaml_char_t *)"array", -1,
		implicit, implicit, YAML_PLAIN_SCALAR_STYLE); // array key
	yaml_emitter_emit(&emitter, &event);
	ok = yaml_sequence_start_event_initialize(
		&event, NULL, NULL, implicit, YAML_FLOW_SEQUENCE_STYLE
	);
	yaml_emitter_emit(&emitter, &event);
	for (int i = 0; i < nVals; i++) {
		sprintf_s(tempStr, "%.1f", array[i]);
		ok = yaml_scalar_event_initialize(
			&event, NULL, NULL, (yaml_char_t *)tempStr, -1,
			implicit, implicit, YAML_PLAIN_SCALAR_STYLE);
		yaml_emitter_emit(&emitter, &event);
	}
	ok = yaml_sequence_end_event_initialize(&event);
	yaml_emitter_emit(&emitter, &event);

	ok = yaml_mapping_end_event_initialize(&event);
	yaml_emitter_emit(&emitter, &event);

	ok = yaml_document_end_event_initialize(
		&event, implicit);
	yaml_emitter_emit(&emitter, &event);
	ok = yaml_stream_end_event_initialize(&event);
	yaml_emitter_emit(&emitter, &event);
	yaml_emitter_delete(&emitter);
	fclose(fh);
}
