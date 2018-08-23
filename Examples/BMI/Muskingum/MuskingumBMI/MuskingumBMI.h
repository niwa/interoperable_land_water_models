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
		MUSKINGUMBMI_API void Initialize(const char *config_file);
		MUSKINGUMBMI_API void Update(void);
		MUSKINGUMBMI_API void UpdateUntil(double then);
		MUSKINGUMBMI_API void Finalize(void);

		// BMI Model Information Functions
		MUSKINGUMBMI_API int GetInputVarNameCount(void) { return _reachVector.size(); }  // one input and one output [time series]
		MUSKINGUMBMI_API int GetOutputVarNameCount(void) { return _reachVector.size(); } // for each reach
		MUSKINGUMBMI_API void GetInputVarNames(char * const * const names);
		MUSKINGUMBMI_API void GetOutputVarNames(char * const * const names);

		// BMI Time functions
		MUSKINGUMBMI_API double get_time_step() { return timeStep; }
		MUSKINGUMBMI_API std::string get_time_units() { return "hours"; }
		MUSKINGUMBMI_API double get_start_time() { return startTime; }
		MUSKINGUMBMI_API double get_current_time() { return currentTime; }
		MUSKINGUMBMI_API double get_end_time() { return endTime; }

		double timeStep = NAN;
		double startTime = 0.0;
		double endTime = NAN;
		double currentTime;


		size_t getReachCount() { return _reachVector.size(); }
		MUSKINGUMBMI_API MuskingumRouter* getReach(unsigned int);    // TODO: not part of BMI -- remove from API 
		void setReachInflow(unsigned int);
		void reportReachOutflow(unsigned int);


	private:
		std::vector<MuskingumRouter> _reachVector;
	};
}// end extern block
