#pragma once
#include "yaml.h"
#include "MuskingumRouter.h"
#include "MuskingumBMI.h"
#include <stack>

class MuskingumYamlParser
{
public:
	MuskingumYamlParser();
	~MuskingumYamlParser();

	void initializeFromYaml(const char *, MuskingumBMI *muskingumModel);

private:
	std::stack<std::string> keyStack;
	int readReachesFromYamlDoc(yaml_document_t*, yaml_node_t*, MuskingumBMI*);
};

