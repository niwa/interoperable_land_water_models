#include "stdafx.h"
#include "MuskingumYamlParser.h"

/*
Much of the logic here derives from an example YAML document parser that I 
borrowed from this StackOverflow inquiry
https://stackoverflow.com/questions/36410122/c-libyaml-document-based-parsing

I took the source from http://codepad.org/W7StVSkV which was referenced in 
the SO article.

The logic that steps through YAML sequences and maps is copied with minimal
alteration from that example, and I've noted that in comments.

TODO items include providing alternatives when a condition isn't met because 
some perfectly legal YAML is present but it's not meaningful to this parser.

The code style here is gosh-awful mess. This class uses the libyaml library, 
but this is a C++ class, and I've tried to use camelCase elsewhere in this 
project, so the naming style is inconsistent. I can hope that this wiil be 
abstracted into just this class, so it shouldn't matter.
*/

MuskingumYamlParser::MuskingumYamlParser()
{
}

MuskingumYamlParser::~MuskingumYamlParser()
{
}

int MuskingumYamlParser::readReachesFromYamlDoc(yaml_document_t *document_p, yaml_node_t *node, MuskingumBMI* muskingumModel)
{
	static int x = 0;
	x++;
	int node_n = x;

	yaml_node_t *next_node_p;

	switch (node->type) {
	case YAML_NO_NODE:
		break;
	case YAML_SCALAR_NODE:
		// only expecting scalars as part of mapped values
		break;
	case YAML_SEQUENCE_NODE:
		// recursive process borrowed from SO example cited in comments at top
		yaml_node_item_t *i_node;
		for (i_node = node->data.sequence.items.start; i_node < node->data.sequence.items.top; i_node++) {
			next_node_p = yaml_document_get_node(document_p, *i_node);
			if (!next_node_p || next_node_p->type != YAML_MAPPING_NODE)
			{
				throw std::runtime_error("YAML parsing problem while processing reach settings.");
			}
			else
			{
				muskingumModel->_reachVector.push_back(*(new MuskingumRouter()));
				readReachesFromYamlDoc(document_p, next_node_p, muskingumModel);
			}
			// TODO: Better handling of non-mapping-node cases
		}
		break;

	case YAML_MAPPING_NODE:
		// this is where the parameters for the reaches are actually set
		// recursive process borrowed from SO example cited in comments at top

		yaml_node_pair_t * i_node_p;
		for (i_node_p = node->data.mapping.pairs.start;
			i_node_p < node->data.mapping.pairs.top; i_node_p++) {
			node_n = i_node_p->key;
			next_node_p = yaml_document_get_node(document_p, i_node_p->key);
			if (!next_node_p || next_node_p->type != YAML_SCALAR_NODE) {
				throw std::runtime_error("YAML process fails looking for key while processing reach settings.");
			}
			keyStack.push((const std::string)(char *)next_node_p->data.scalar.value);
			node_n = i_node_p->value;
			next_node_p = yaml_document_get_node(document_p, i_node_p->value);
			if (!next_node_p || next_node_p->type != YAML_SCALAR_NODE) {
				throw std::runtime_error("YAML process fails looking for value while processing reach settings.");
			}
			// processing reaches, we should only see these key types
			// we'll ignore others 
			else if (keyStack.top() == "name") {
				muskingumModel->_reachVector.back().reachName((char*)next_node_p->data.scalar.value);
				keyStack.pop();
				node_n++;
			}
			else if (keyStack.top() == "K") {
				muskingumModel->_reachVector.back().muskK(atof((char*)next_node_p->data.scalar.value));
				keyStack.pop();
				node_n++;
			}
			else if (keyStack.top() == "X") {
				muskingumModel->_reachVector.back().muskX(atof((char*)next_node_p->data.scalar.value));
				keyStack.pop();
				node_n++;
			}
			else if (keyStack.top() == "InputVariable") {
				muskingumModel->_reachVector.back().inputVarName((char*)next_node_p->data.scalar.value);
				keyStack.pop();
				node_n++;
			}
			else if (keyStack.top() == "InputVariable") {
				muskingumModel->_reachVector.back().outputVarName((char*)(next_node_p->data.scalar.value));
				keyStack.pop();
				node_n++;
			}
			else {
				keyStack.pop();
				node_n++;
			}
		}
		break;

	default:
		// fputs("Unknown node type\n", stderr);
		// exit(1);
		break;
	}// close switch on node type
	return x;

}

void MuskingumYamlParser::initializeFromYaml(const char * yaml_filename, MuskingumBMI* muskingumModel) 
{

	yaml_parser_t parser;
	std::string errmsg;
	std::string tempString;
	//std::stack<std::string> keyStack;
	FILE *fh;

	/* Create YAML parser for named input file */
	if (fopen_s(&fh, yaml_filename, "r")) {
		errmsg = "Could not open file: ";
		errmsg.append(yaml_filename);
		throw std::runtime_error(errmsg);
	}
	if (!yaml_parser_initialize(&parser))
		throw std::runtime_error("Failed to initialize yaml parser!");
	yaml_parser_set_input_file(&parser, fh);

	yaml_document_t doc;
	yaml_parser_load(&parser, &doc);

	// starting at the root node, look for YAML_MAPPING_NODES with 
	// keys that we recognize at the top level of our reach setup
	yaml_node_t *search_node = NULL;
	yaml_node_t *next_node_p;
	int node_n = 1;
	search_node = yaml_document_get_node(&doc, node_n);

	while (search_node != NULL)
	{
		switch (search_node->type) {
		case YAML_NO_NODE:
			node_n++;
			break;
		case YAML_SCALAR_NODE:
			tempString.assign((char*)(search_node->data.scalar.value));
			node_n++;
			break;
		case YAML_SEQUENCE_NODE:
			node_n++;
			break;
		case YAML_MAPPING_NODE:
			// this is probably very fragile
			yaml_node_pair_t * i_node_p;
			for (i_node_p = search_node->data.mapping.pairs.start;
				i_node_p < search_node->data.mapping.pairs.top; i_node_p++) {
				node_n = i_node_p->key;
				next_node_p = yaml_document_get_node(&doc, i_node_p->key);
				if (!next_node_p || next_node_p->type != YAML_SCALAR_NODE) {
					throw std::runtime_error("YAML process fails looking for value while processing reach settings.");
				}
				keyStack.push((const std::string)(char *)next_node_p->data.scalar.value);
				node_n = i_node_p->value;
				next_node_p = yaml_document_get_node(&doc, i_node_p->value);
				if (!next_node_p || (next_node_p->type != YAML_SCALAR_NODE && 
					next_node_p->type != YAML_SEQUENCE_NODE)) {
					throw std::runtime_error("YAML process fails looking for value while processing reach settings.");
				}
				// at top level, we should only see the key types in this sequence of if/elses
				// others will be ignored
				if (keyStack.top() == "time step") {
					muskingumModel->timeStep = atof((char*)next_node_p->data.scalar.value);
					keyStack.pop();
					node_n++;
				}
				else if (keyStack.top() == "start time") {
					muskingumModel->startTime = atof((char*)next_node_p->data.scalar.value);
					keyStack.pop();
					node_n++;
				}
				else if (keyStack.top() == "end time") {
					muskingumModel->endTime = atof((char*)next_node_p->data.scalar.value);
					keyStack.pop();
					node_n++;
				}
				else if (keyStack.top() == "reaches") {
					// the following method recurses its way through this list of reaches
					readReachesFromYamlDoc(&doc, next_node_p, muskingumModel);
					keyStack.pop();
				} 
				else {
					keyStack.pop();
					node_n++;
				}
			}
			break;
		} // end node-type switch
		search_node = yaml_document_get_node(&doc, node_n);
	} // end while loop processing nodes

	// Cleanup
	yaml_parser_delete(&parser);
	fclose(fh);
}