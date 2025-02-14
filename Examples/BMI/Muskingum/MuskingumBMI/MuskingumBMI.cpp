// MuskingumBMI.cpp
// Defines the exported functions for the BMI wrapper around the
// Muskingum library.

#include "stdafx.h"
#include "MuskingumBMI.h"
#include "MuskingumYamlParser.h"

/* Store callback */
Logger logger = NULL;

/* Logger function */
void _log(Level level, std::string msg);

/* control functions. These return an error code. */
int initialize(const char* config_file) {
	try
	{
		muskModel = new MuskingumBMI();
		muskModel->Initialize(config_file);
		std::cout << "Muskingum model initialized from " << config_file << "." << std::endl;
	}
	catch (std::exception e)
	{
		return -1;
	}
	return 0;
}

int update(double tStep) {
	double now, stopTime, deltaT;
	get_current_time(&now);
	stopTime = now + tStep;
	if (tStep < 0.) {
		get_time_step(&deltaT);
		stopTime = now + deltaT;
	}
	int status = 0;
	while (now < stopTime) {
		status = muskModel->Update();
		if (status) break;
		get_current_time(&now);
	}
	return status;
}

int finalize(void)
{
	delete muskModel;
	return 0;
}

/* time control functions */
/* Config file expresses times in unreferenced hours
*  DIMR sees time as seconds (since what epoch?). 
*  Converting hours to seconds is a cheap
*  way to cross the gap.*/
// TODO: De - hack-ify this fix
void get_start_time(double* sTime) {
	*sTime = muskModel->GetStartTime() * 3600.0;
}
void get_end_time(double* eTime)
{
	*eTime = muskModel->GetEndTime() * 3600.0;
}
void get_current_time(double* cTime)
{
	*cTime = muskModel->GetCurrentTime() * 3600.;
}
void get_time_step(double* tStep)
{
	*tStep = muskModel->GetTimeStep() * 3600.0;
}

/* variable info */
void get_var_shape(const char * key, int shape[MAXDIMS]) {
	muskModel->GetGridShape(key, shape);
	return;
}
void get_var_rank(const char * key, int* value) {
	*value = 1;
}
void get_var_type(const char * key, char* type) {
	muskModel->GetVarType(key, type);
	return;
}
void get_var_count(int* count) {
	*count = muskModel->GetVarNameCount();
}
void get_var_name(int i, char* vName) {
	strncpy_s(vName, strlen(muskModel->GetVarName(i)) + 1, muskModel->GetVarName(i), MAXVARNAMELEN);
}
/* get a pointer pointer - a reference to a multidimensional array */
void get_var(const char * key, void ** ref)
{
	muskModel->GetValue(key, ref);
}
/* Set the variable from contiguous memory referenced to by ptr */
void set_var(const char * key, const void * value)
{
	muskModel->SetValue(key, value);
}

/* set logger by setting a pointer to the log function */
void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Muskingum BMI library attached to logger.";
	logger = callback;
	logger(level, msg.c_str());
}
void _log(Level level, std::string msg) {
	if (logger != NULL) {
		logger(level, msg.c_str());
	}
}


MuskingumBMI::MuskingumBMI() {}
MuskingumBMI::~MuskingumBMI() {
	delete[] _parameterK;
	delete[] _parameterX;
	delete[] _flowIn;
	delete[] _flowOut;
	for (size_t i = 0; i < _reachVector.size(); i++)
	{
		delete[] _reachName[i];
	}
	delete[] _reachName;
}

/* Perform startup tasks for the model.

Perform all tasks that take place before entering the model's time
loop, including opening files and initializing the model state.Model
inputs are read from a text - based configuration file, specified by
`filename`.

Models should be refactored, if necessary, to use a
configuration file. CSDMS does not impose any constraint on
how configuration files are formatted, although YAML is
recommended. A template of a model's configuration file
with placeholder values is used by the BMI.
*/
int MuskingumBMI::Initialize(const char *config_file) {
	//Manditory BMI method

	MuskingumYamlParser ymp;
	size_t reachCount = 0;
	try
	{
		ymp.initializeFromYaml(config_file, this);
		// ymp.initializeFromYaml("C:\\Temp\\musk3.yaml", this); // hard-code for BMI test
		reachCount = _reachVector.size();
	}
	catch (std::exception e)
	{
		throw e;
	}

	// allocate arrays for inputs and outputs
	_parameterK = new double[reachCount];
	_parameterX = new double[reachCount];
	_flowIn = new double[reachCount];
	_flowOut = new double[reachCount];
	// initialize all values to NaN
	for (int i = 0; i < reachCount; i++) {
		_parameterK[i] = NAN;
		_parameterX[i] = NAN;
		_flowIn[i] = NAN;
		_flowOut[i] = NAN;
	}

	// allocate an array of arrays for reach names
	_reachName = new char*[reachCount];

	// set input and output variable names and initialize the data arrays
	for (size_t i = 0; i < _reachVector.size(); i++) {
		size_t nameArrayLen = _reachVector[i].reachName().length() + 1;
		_reachName[i] = new char[nameArrayLen];
		strncpy_s(_reachName[i], nameArrayLen, _reachVector[i].reachName().c_str(), nameArrayLen);
		_flowIn[i] = _reachVector[i].flow();
		_flowOut[i] = _flowIn[i];
		_parameterK[i] = _reachVector[i].muskK();
		_parameterX[i] = _reachVector[i].muskX();
	}
	currentTime = this->startTime;

	return 0;

}


/*Advance model state by one time step.

Perform all tasks that take place within one pass through the model's
time loop. This typically includes incrementing all of the model's
state variables. If the model's state variables don't change in time,
then they can be computed by the :func:`initialize` method and this
method can return with no action.
*/
int MuskingumBMI::Update(void) {
	currentTime += timeStep;
	std::cout << "Time: " << currentTime << std::endl;
	size_t reachCount = getReachCount();
	for (size_t i = 0; i < reachCount; i++) {
		_flowOut[i] = _reachVector[i].flow(currentTime, _flowIn[i]);
		// std::cout << "flow = " << _flowOut[i] << std::endl;
	}
	return 0;
}

/*Advance model state until the given time.

Parameters
----------
time : float
	A model time value.

	See Also
	--------
	Update

*/
void MuskingumBMI::UpdateUntil(double then) { 
	while (currentTime < then) {
		Update();
	}
}

/*Perform tear-down tasks for the model.

Perform all tasks that take place after exiting the model's
time loop.This typically includes deallocating memory,
closing files, and printing reports.

*/
void MuskingumBMI::Finalize(void) {
	std::cout << "Done. ";
}

// End BMI Model Control Functions block

// BMI Model Information Functions
// int GetInputVarNameCount(void) { return reachList.size(); }        // inline implementation in header
// int GetOutputVarNameCount(void) { return reachList.size(); }       // inline implementation in header

void MuskingumBMI::GetInputVarNames(char * const * const names) {
	for (int i = 0; i < GetInputVarNameCount(); i++) {
		// There are 3 input variables (vectors with one element per reach)
		// These are: Flow from the reach and the two Muskingum Parameters (K and X)
		strncpy_s((names)[0], 21, "channel__Muskingum_K", 21);
		strncpy_s((names)[1], 21, "channel__Muskingum_X", 21);
		strncpy_s((names)[2], 51, "channel_entrance_water_x-section__volume_flow_rate", 51);
	}
}
void MuskingumBMI::GetOutputVarNames(char * const * const names) {
	// There are 3 input variables (vectors with one element per reach)
	// These are: Flow from the reach and the two Muskingum Parameters (K and X)
	strncpy_s((names)[0], 14, "channel__name", 14);
	strncpy_s((names)[1], 47, "channel_exit_water_x-section__volume_flow_rate", 47);
}

const char* MuskingumBMI::GetVarName(int i) {
	if (i < _varCount){
		return _varNames[i];
	}
	return NULL;
}

std::string MuskingumBMI::GetVarUnits(const char * name) {
	int index = getVarNameIndex(name);
	switch (index) {

		case 0:
			// "channel__Muskingum_K"
			return "hours";

		case 1:
		case 4:
			// "channel__Muskingum_X"
			// "channel__name"
			return "-";

		case 2:
		case 3:
			// "channel_entrance_water_x-section__volume_flow_rate",
			// "channel_exit_water_x-section__volume_flow_rate",
			return "m3 s-1";

		case -1:
		default:
				break;
	}
	return "";
}

int MuskingumBMI::GetGridShape(const char *  name, int* shape) {
	// all in and out variables are 1D vectors with one element per reach
	shape[0] = (int)_reachVector.size();
	return 0;
}

const char* MuskingumBMI::getQuantityName(const char* name) {
	// assume CSDMS double underscore separates object name from quantity name;
	// looking for substring after the last double underscore as quantity name
	const char *ptr = name + strlen(name);
	int count = 0;
	while (ptr > name) {
		if (*ptr == '_') count++;
		else count = 0;
		if (count == 2) return ptr + 2;
	}
	return name;
}

void MuskingumBMI::GetValue(std::string name, void** handle) {
	// handle should point to a pointer, which is set to 
	// the memory address of the value
	*handle = NULL;
	int index = getVarNameIndex(name.c_str());
	switch (index){

		case 0:
		// "channel__Muskingum_K"
			*handle = (void*)_parameterK;
			return;

		case 1:
		// "channel__Muskingum_X"
			*handle = (void*)_parameterX;
			return;

		case 2:
		// "channel_entrance_water_x-section__volume_flow_rate",
			*handle = (void*)_flowIn;
			return;

		case 3:
		// "channel_exit_water_x-section__volume_flow_rate",
			*handle = (void*)_flowOut;
			return;

		case 4:
		// "channel__name"
			*handle = (void*)_reachName;
			return;

		case -1:
		default:
			break;
	}
	return;
}

void MuskingumBMI::SetValue(std::string name, const void* buffer) {
	int index = getVarNameIndex(name);
	if (index < -1) {
		std::cerr << "Variable name \"" << name << "\" not found." << std::endl;
		return;
	}
	if (name.compare("channel__name") == 0) {
		std::cerr << "No support for changing reach names." << std::endl;
		return;
	}
	if (name.compare("channel_exit_water_x-section__volume_flow_rate") == 0) {
		std::cerr << "No support for overriding flow results." << std::endl;
		return;
	}
	double* values = (double*) buffer;
	for (int i = 0; i < _reachVector.size(); i++) {
		switch (index) {

		case 0:
			// "channel__Muskingum_K"
			_parameterK[i] = values[i];
			_reachVector[i].muskK(values[i]);
			break;

		case 1:
			// "channel__Muskingum_X"
			_parameterX[i] = values[i];
			_reachVector[i].muskX(values[i]);
			break;

		case 2:
			// "channel_entrance_water_x-section__volume_flow_rate",
			_flowIn[i] = values[i];
			// special case for initializing
			if (currentTime == startTime) {
				if (isnan(_reachVector[i].flow())) {
					_reachVector[i].startFlow(values[i]);
				}
			}
			break;

		case 3:
		case 4:
		case -1:
		default:
			break;
		}
	}
	return;
}

int MuskingumBMI::GetVarGrid(const char *  name) {
	return getVarNameIndex(name);
}

void MuskingumBMI::GetVarType(const char * name, char * type) {
	int index = getVarNameIndex(name);
	switch (index) {
	case 0:
	case 1:
	case 2:
	case 3:
		strncpy_s(type, 7, "double", 8);
		break;
	case 4:
		strncpy_s(type, 6, "char*", 8);
		break;
	case -1:
	default:
		strncpy_s(type, 1, "", 8);
	}
}

int MuskingumBMI::
GetVarItemsize(const char *name)
{
	int index = getVarNameIndex(name);
	switch (index) {
	case 0:
	case 1:
	case 2:
	case 3:
		return sizeof(double);
	case 4:
		return sizeof(char*);
	case -1:
	default:
		return 0;
	}
}

int MuskingumBMI::
GetVarNbytes(const char *name)
{

	int index = getVarNameIndex(name);
	switch (index) {
	case 0:
	case 1:
	case 2:
	case 3:
		return sizeof(double) * (int)_reachVector.size();
	case 4:
		return sizeof(char*) * (int)_reachVector.size();
	case -1:
	default:
		return 0;
	}
}

int MuskingumBMI::getVarNameIndex(const std::string name) {
	for (int i = 0; i < _varCount; i++) {
		if (name.compare(_varNames[i]) == 0) {
			return i;
		}
	}
	return -1;
}
/*
MuskingumRouter* MuskingumBMI::getReach(unsigned int i) {
	if (i < _reachVector.size())
		return &_reachVector[i];
	else return NULL;
}

void MuskingumBMI::setReachInflow(unsigned int i) {
	MuskingumRouter* reach = getReach(i);
	std::string errmsg;
	if (reach == NULL) {
		errmsg = "Reach " + i;
		errmsg += " is out of range.";
		throw std::runtime_error(errmsg);
	}
	if (reach->inputVarName() == "STD_IN") {
		double uEntry_d;
		if (currentTime == 0.0) {
			std::cout << "Enter initial flow  for reach " + reach->reachName()
				+ " [Q(" + reach->reachName() + ", 0)]: ";
			scanf_s("%lf", &uEntry_d);
			reach->reset(uEntry_d);
		}
		else {
			printf("I(%s, %lf) = ", reach->reachName().data(), currentTime);
			scanf_s("%lf", &uEntry_d);
			reach->flow(currentTime, uEntry_d);
		}
	}
	else {
		errmsg = "Input method for " + reach->inputVarName() + "not yet implemented";
		throw std::logic_error(errmsg);
	}
}

void MuskingumBMI::reportReachOutflow(unsigned int i) {
	MuskingumRouter* reach = getReach(i);
	std::string errmsg;
	if (reach == NULL) {
		errmsg = "Reach " + i;
		errmsg += " is out of range.";
		throw std::runtime_error(errmsg);
	}
	if (reach->outputVarName() == "STD_OUT") {
		printf("Q(%s, %lf) = %lf\n", reach->reachName().data(), currentTime, 
			reach->flow());	
	}
	else {
		errmsg = "Input method for " + reach->inputVarName() + "not yet implemented";
		throw std::logic_error(errmsg);
	}
}
*/
