#pragma once

#include "MuskingumRouter.h"
#include "bmi.h"
#include <vector>
#include <string>

#define MAXVARNAMELEN 128

extern "C" {

	class MuskingumBMI {
		friend class MuskingumYamlParser;

	public:
		BMI_API MuskingumBMI();
		BMI_API ~MuskingumBMI();

		// BMI Model Control Functions
		BMI_API int Initialize(const char *config_file);
		BMI_API int Update(void);
		BMI_API void UpdateUntil(double then);
		BMI_API void Finalize(void);

		// BMI Model Information Functions
		BMI_API int GetInputVarNameCount(void) { return 3; }  // Flow, plus K and X parameters
		BMI_API int GetOutputVarNameCount(void) { return 2; } // Flow, reach IDs
		BMI_API void GetInputVarNames(char * const * const names);
		BMI_API void GetOutputVarNames(char * const * const names);
		const char* GetVarName(int i);
		const int GetVarNameCount() { return 5; }

		// BMI Time functions
		BMI_API double GetTimeStep() { return timeStep; }
		BMI_API std::string GetTimeUnits() { return "hours"; }
		BMI_API double GetStartTime() { return startTime; }
		BMI_API double GetCurrentTime() { return currentTime; }
		BMI_API double GetEndTime() { return endTime; }

		// BMI Variable Information Functions
		BMI_API int GetVarGrid(const char *  name);
		BMI_API std::string GetVarUnits(const char * name); // { return "m3 s-1"; } // all inputs and outputs are volume flows
		BMI_API void GetVarType(const char * name, char * type);
		BMI_API int GetVarItemsize(const char *name);
		BMI_API int GetVarNbytes(const char *  name);
		BMI_API std::string GetGridType(int id) { return "uniform_rectilinear"; };
		BMI_API int GetGridRank(const char *  name) { return 1; } // all inputs and outputs are 1D vectors
		BMI_API int GetGridShape(const char *  name, int* shape);

		// BMI Variable Getter and Setter Functions
		BMI_API void GetValue(std::string name, void** buffer);
		BMI_API void SetValue(std::string name, const void* buffer);

		double timeStep = NAN;
		double startTime = 0.0;
		double endTime = NAN;
		double currentTime = 0.0;

		size_t getReachCount() { return _reachVector.size(); }

	private:
		std::vector<MuskingumRouter> _reachVector;
		int _varCount = 5;
		const char* _varNames[5] = {
			"channel__Muskingum_K", 
			"channel__Muskingum_X", 
			"channel_entrance_water_x-section__volume_flow_rate", 
			"channel_exit_water_x-section__volume_flow_rate", 
			"channel__name"};
		double* _parameterK;
		double* _parameterX;
		double* _flowIn;
		double* _flowOut;
		char** _reachName;
		const char* getQuantityName(const char* name);
		int getVarNameIndex(const std::string);
	};

	static MuskingumBMI* muskModel;

}// end extern block
