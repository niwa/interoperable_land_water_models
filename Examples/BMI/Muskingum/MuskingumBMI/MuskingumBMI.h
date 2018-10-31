#pragma once

#ifdef MUSKINGUMBMI_EXPORTS
#define MUSKINGUMBMI_API __declspec(dllexport)
#else
#define MUSKINGUMBMI_API __declspec(dllimport)
#endif

#include "MuskingumRouter.h"
#include <vector>
#include <string>

extern "C" {

	class MuskingumBMI {
		friend class MuskingumYamlParser;

	public:
		MUSKINGUMBMI_API MuskingumBMI();
		MUSKINGUMBMI_API ~MuskingumBMI();

		// BMI Model Control Functions
		MUSKINGUMBMI_API int Initialize(const char *config_file);
		MUSKINGUMBMI_API int Update(void);
		MUSKINGUMBMI_API void UpdateUntil(double then);
		MUSKINGUMBMI_API void Finalize(void);

		// BMI Model Information Functions
		MUSKINGUMBMI_API int GetInputVarNameCount(void) { return _reachVector.size(); }  // one input and one output [time series]
		MUSKINGUMBMI_API int GetOutputVarNameCount(void) { return _reachVector.size(); } // for each reach
		MUSKINGUMBMI_API void GetInputVarNames(char * const * const names);
		MUSKINGUMBMI_API void GetOutputVarNames(char * const * const names);
		const char* GetVarName(int i);

		// BMI Time functions
		MUSKINGUMBMI_API double GetTimeStep() { return timeStep; }
		MUSKINGUMBMI_API std::string GetTimeUnits() { return "hours"; }
		MUSKINGUMBMI_API double GetStartTime() { return startTime; }
		MUSKINGUMBMI_API double GetCurrentTime() { return currentTime; }
		MUSKINGUMBMI_API double GetEndTime() { return endTime; }

		// BMI Variable Information Functions
		MUSKINGUMBMI_API int GetVarGrid(const char *  name);
		MUSKINGUMBMI_API std::string GetVarUnits(const char * name) { return "m3 s-1"; } // all inputs and outputs are volume flows
		MUSKINGUMBMI_API void GetVarType(const char * name, char * type);
		MUSKINGUMBMI_API int GetVarItemsize(const char *name);
		MUSKINGUMBMI_API int GetVarNbytes(const char *  name);
		MUSKINGUMBMI_API std::string GetGridType(int id) { return "uniform_rectilinear"; };
		MUSKINGUMBMI_API int GetGridRank(const char *  name) { return 0; } // all inputs and outputs are scalar

		// BMI Variable Getter and Setter Functions
		MUSKINGUMBMI_API void GetValue(std::string name, void* buffer);
		MUSKINGUMBMI_API void SetValue(std::string name, void* buffer);

		double timeStep = NAN;
		double startTime = 0.0;
		double endTime = NAN;
		double currentTime = 0.0;

		size_t getReachCount() { return _reachVector.size(); }

	private:
		std::vector<MuskingumRouter> _reachVector;
		double* _dataArray;
		const char* getQuantityName(const char* name);
	};

	static MuskingumBMI* muskModel;

	MUSKINGUMBMI_API int initialize(const char* config_file);
	MUSKINGUMBMI_API int update(double);
	MUSKINGUMBMI_API int finalize(void);
	MUSKINGUMBMI_API void get_var_count(int*);
	MUSKINGUMBMI_API void get_start_time(double* );
	MUSKINGUMBMI_API void get_end_time(double* );
	MUSKINGUMBMI_API void get_current_time(double*);
	MUSKINGUMBMI_API void get_time_step(double*);
	MUSKINGUMBMI_API void get_var(const char * key, void ** ref);
	MUSKINGUMBMI_API void set_var(const char * key, const void * value);
	MUSKINGUMBMI_API void get_var_rank(const char * key, int* value);
	MUSKINGUMBMI_API void get_var_shape(const char * key, int** value);
	MUSKINGUMBMI_API void get_var_type(const char * key, char* type);
	MUSKINGUMBMI_API void get_var_name(int i, char*);
}// end extern block
