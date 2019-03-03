#pragma once
#include <stack>
#include <string>
#include "yaml.h"

class BuilderYamlUtils
{
public:

	std::string tranferFileName;
	std::string message;
	int talley = -1, shape[MAXDIMS];
	double* array;

	__declspec(dllexport) BuilderYamlUtils();
	__declspec(dllexport) ~BuilderYamlUtils();

	__declspec(dllexport) void initializeBuilderFromYaml(const char *);
	__declspec(dllexport) void writeStuffToYamlFile(const char*);
};
