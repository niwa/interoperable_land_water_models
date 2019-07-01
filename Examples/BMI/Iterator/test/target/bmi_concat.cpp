#include <string>
#include <cstring>
#include <algorithm>
#include <iostream>
#include <sstream>
#include "bmi.h"


// This is simple BMI component that takes two inputs, a double and a string,
// and produces two outputs, an integer and a string.
// The string output is the string input concatenated with the double input.
// The integer output is the length of the string output.
static std::string _inp_str;
static double _inp_val;
static std::string _out_str;
static int _out_len;

#define NB_VARIABLES 4
const std::string _varnames[NB_VARIABLES] = {
    "input_str",
    "input_val",
    "output_str",
    "output_len"
};


/* control functions. These return an error code. */
BMI_API int initialize(const char *config_file) {return 0;}


BMI_API int update(double dt) {
    std::stringstream ss;
    ss << _inp_str << _inp_val;
    _out_str = ss.str();
    _out_len = (int) _out_str.size();
    return 0;
}


BMI_API int finalize() {return 0; }


/* time control functions */
BMI_API void get_start_time(double *t) {}


BMI_API void get_end_time(double *t) {}


BMI_API void get_current_time(double *t) {}


BMI_API void get_time_step(double *dt) {}


/* variable info */
BMI_API void get_var_shape(const char *name, int shape[MAXDIMS]) {
    auto varname = std::string {name};
    if (varname == "input_val" || varname == "output_len") {
        shape[0] = 1;
    }
    else if (varname == "input_str" || varname == "output_str") {
        shape[0] = 1;
    }
}


BMI_API void get_var_rank(const char *name, int *rank) {
    auto varname = std::string {name};
    if (varname == "input_val" || varname == "output_len") {
        *rank = 1;
    }
    else if (varname == "input_str" || varname == "output_str") {
        *rank = 1;
    }
}


BMI_API void get_var_type(const char *name, char *type) {
    auto varname = std::string {name};
    if (varname == "input_str" || varname == "output_str") {
        strncpy_s(type, MAXSTRINGLEN, "str", 4);
    }
    else if (varname == "input_val") {
        strncpy_s(type, MAXSTRINGLEN, "double", 7);
    }
    else if (varname == "output_len") {
        strncpy_s(type, MAXSTRINGLEN, "int", 4);
    }
}


BMI_API void get_var_count(int *count) {
    *count = NB_VARIABLES;
}


BMI_API void get_var_name(int index, char *name) {
    if (index >= NB_VARIABLES) return;
    auto varname = _varnames[index];
    strncpy_s(name, MAXSTRINGLEN, varname.c_str(), varname.size()+1);
}


/* data access */
BMI_API void get_var(const char *name, void **ptr) {
    auto varname = std::string {name};
    if (varname == "input_str") {
        *ptr = (void*) _inp_str.c_str();
    }
    else if (varname == "input_val") {
        *ptr = (void*) &_inp_val;
    }
    else if (varname == "output_str") {
        *ptr = (void*) _out_str.c_str();
    }
    else if (varname == "output_len") {
        *ptr = (void*) &_out_len;
    }
}

BMI_API void set_var(const char *name, const void *ptr) {
    auto varname = std::string {name};
    if (varname == "input_str") {
        _inp_str = std::string {(char*) ptr};
    }
    else if (varname == "input_val") {
        _inp_val = *((double*) ptr);
    }
    else if (varname == "output_str") {
        _out_str = std::string {(char*) ptr};
    }
    else if (varname == "output_len") {
        _out_len = *((int*) ptr);
    }
}
