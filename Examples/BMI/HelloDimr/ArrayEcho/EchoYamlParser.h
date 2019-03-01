#pragma once
#include <stack>
#include <string>
#include "yaml.h"

class EchoYamlParser
{
public:

	std::string tranferFileName;
	std::string message;
	int nValues = -1, shape[MAXDIMS];
	double* array;
	bool testMode = false;

	__declspec(dllexport) EchoYamlParser();
	__declspec(dllexport) ~EchoYamlParser();

	__declspec(dllexport) void parseArrayConfiguration(const char*);
	__declspec(dllexport) void parseArrayData(const char*, int modelShape[MAXDIMS], double* modelValues);
};
