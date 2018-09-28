#ifndef BMI_TYPO_H_INCLUDED
#define BMI_TYPO_H_INCLUDED

#include <string>
#include "typo.h"

namespace Typology{
    class BmiLookup
    {
    public:
        BmiLookup() {
            this->input_var_names[0] = "typology";
            this->output_var_names[0] = "nitrogen";
            this->output_var_names[1] = "phosphorus";
            this->output_var_names[2] = "table";
        };

        void Initialize (std::string csv_file);
        void Update() {};
        void UpdateUntil(double) {};
        void UpdateFrac(double) {};
        void Finalize() {};

        void GetComponentName(char *name);
        void GetInputVarNameCount(int * count);
        void GetOutputVarNameCount(int * count);
        void GetInputVarNames(char** names);
        void GetOutputVarNames(char** names);

        void GetVarType(const char * name, char* type);
        void GetVarItemsize(const char * name, int* itemsize);
        void GetVarUnits(const char * name, char* units);
        void GetVarRank(const char * name, int* rank);
        void GetVarSize(const char * name, int* size);
        void GetVarNbytes(const char * name, int* nbytes);

        void GetCurrentTime(double *time) {};
        void GetStartTime(double *start) {};
        void GetEndTime(double *end) {};
        void GetTimeStep(double *dt) {};
        void GetTimeUnits(char *units) {};

        void GetValue(const char *, char *);
        void GetValuePtr(const char *, char **);

        void SetValue(const char *, char *);

        void GetGridType(const char *, char *);
        void GetGridShape(const char *, int *) {};
        void GetGridSpacing(const char *, double *) {};
        void GetGridOrigin(const char *, double *) {};

        void GetGridX(const char *, double *) {};
        void GetGridY(const char *, double *) {};

    private:
        Lookup _lookup;

        static const int input_var_name_count = 1;
        static const int output_var_name_count = 3;

        const char* input_var_names[1];
        const char* output_var_names[3];

        /*
        The wrapped lookup library has no state keeping track
        of what typology is to be looked up, or what values
        have been retrieved. We add those here so we can point
        to them from GetValue and SetValue.
        */
        char typology[2048];
        double nitrogen;
        double phosphorus;
    };
}

#endif
