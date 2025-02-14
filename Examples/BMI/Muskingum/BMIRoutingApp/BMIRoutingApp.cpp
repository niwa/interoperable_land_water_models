// BMIRoutingApp.cpp : Defines the entry point for the console application.
//
// This is a console app that runs a simple Muskingum Routing model through 
// BMI method calls.

#include "stdafx.h"
#include "MuskingumBMI.h"
#include <vector>
using namespace std;

/*
string GetReachNameFromVariableName(const char* vName) {
	for (size_t i = 0; i < strlen(vName) - 2; i++) {
		if (strncmp(vName + i, "__", 2) == 0) {
			return string(vName, i);
		}
	}
	return "";
}
*/

int main()
{
	// MuskingumBMI muskModel = MuskingumBMI();
	char uEntry_t[50] = "";
	double uEntry_d;
	double CurrentTime = 0.;
	double timeStep = NAN;
	double flow = 0.;
	string workString;
	// vector<size_t> inputs, outputs;

	printf("Enter configuration file name: ");
	scanf_s("%s", uEntry_t, 50);
	try {
		initialize(uEntry_t);
	}
	catch (runtime_error e) {
		printf_s(e.what());
		return -1;
	}

	int count;
	double startTime, endTime;
	get_start_time(&startTime);
	get_end_time(&endTime);
	get_time_step(&timeStep);
	if (timeStep == NAN)
	{
		cout << "Enter the model time step: ";
		cin >> timeStep;
	}
	if (endTime == NAN) {
		cout << "Enter number of time steps for model to run: ";
		cin >> count;
		endTime = (double)count * timeStep;
	}
	else
	{
		cout << "Model will run from start time: " << startTime << "\n to end time: " << endTime << endl;
		count = ceil((endTime - startTime) / timeStep);
		cout << "Equivalent to " << count << " time steps." << endl;
	}

	// find the names of the variable vectors
	int varCount;
	get_var_count(&varCount);
	char** varNames = new char*[varCount];
	char tempStr[MAXVARNAMELEN];
	for (int i = 0; i < varCount; i++) {
		get_var_name(i, tempStr);
		size_t nameLen = strlen(tempStr);
		nameLen++;
		varNames[i] = new char[nameLen];
		strncpy_s(varNames[i], nameLen, tempStr, nameLen);
		workString = varNames[i];
		/*
		if (workString.find("entrance") != string::npos) {
			inputs.push_back(i);
		}
		else if (workString.find("exit") != string::npos){
			outputs.push_back(i);
		}
		*/
	}

	// How many reaches do we have? Should be equivalent to the length 
	// of any of our variable vectors.
	int shape[MAXDIMS];
	get_var_shape(varNames[0], shape);
	size_t numReaches = shape[0];

	// The reaches have names, which are in one of our variables.
	char** rchNames;
	get_var("channel__name", (void**)&rchNames);
	// Same with inflows and outflows
	double *inFlows, *inBuffer, *outBuffer;
	get_var("channel_entrance_water_x-section__volume_flow_rate", (void**)&inFlows);
	get_var("channel_exit_water_x-section__volume_flow_rate", (void**)&outBuffer);
	inBuffer = new double[numReaches];

	// Check that all reaches have been initialized
	string rName;
	for (int i = 0; i < numReaches; i++) {
		rName = rchNames[i];
		inBuffer[i] = inFlows[i];
		if (isnan(inBuffer[i])) {
			cout << "Enter initial flow value for " + rName
				+ " [Q(" + rchNames[i] + ", 0)]: ";
			scanf_s("%lf", &uEntry_d);
			inBuffer[i] = uEntry_d;
		}
		else
		{
			cout << "Initial flow for reach " + rName
				+ " is " << inBuffer[i] << endl;
		}
	}
	set_var("channel_entrance_water_x-section__volume_flow_rate", inBuffer);

	// main loop using the BMI Update method
		for (int i = 0; i < count; i++) {
			try {
				// set the inputs for the reaches
				cout << "\nFor time step " << i + 1 << " enter the incoming flow value for:" << endl;
				// for (int i = 0; i < inputs.size(); i++) {
				for (int i = 0; i < numReaches; i++) {
					rName = rchNames[i];
					get_current_time(&CurrentTime);
					cout << "    Reach " + rName
						+ " [Qin(" + rName + ", " << CurrentTime + timeStep << ")]: ";
					scanf_s("%lf", &uEntry_d);
					inBuffer[i] = uEntry_d;
				}
			set_var("channel_entrance_water_x-section__volume_flow_rate", inBuffer);
			update(timeStep);
			}
			catch (runtime_error e) {
				printf_s(e.what());
				return -1;
			}
			get_var("channel_exit_water_x-section__volume_flow_rate", (void**)&outBuffer);
			// report the outputs for the reaches
			cout << "\n  The resulting out flows are:" << endl;
			// for (int i = 0; i < outputs.size(); i++) {
			for (int i = 0; i < numReaches; i++) {
				string rName = rchNames[i];
				cout << "    Reach " + rName
					+ " [Qout(" + rName + ", " << CurrentTime << ")] = " << outBuffer[i] << endl;
			}
		}
	

	/*// main loop embedded in BMI UpdateUntil method
	try { muskModel.UpdateUntil(endTime); }
	catch (runtime_error e) {
	printf_s(e.what());
	return -1;
	}
	*/
	/*
	for (unsigned int i = 0; i < muskModel.getReachCount(); i++) {
	cout << "Reach \"" << muskModel.getReach(i)->reachName() << "\":"
	<< "\n\tInput from " << inputs[i]
	<< "\n\tOutput from " << outputs[i] << endl;
	}
	*/

	for (int i = 0; i < varCount; i++) {
	delete[] varNames[i];
	}
	delete[] varNames;
	delete[] inBuffer;

	finalize();
	cout << "Press enter to close program" << endl;
	getchar();
	getchar();

	return 0;
}


