#ifndef LOOKUP_BMI_H
#define LOOKUP_BMI_H

/*
 * BMI header based on https://github.com/openearth/bmi/blob/master/models/include/bmi.h
 * */
#ifndef TYPO_BMI_H
#define TYPO_BMI_H

#if defined _WIN32
#define BMI_API __declspec(dllexport)
/* Calling convention, stdcall in windows, cdecl in the rest of the world */
#define CALLCONV __stdcall
#else
#define BMI_API
#define CALLCONV
#endif

#define MAXSTRINGLEN 1024
#define MAXDIMS 6

#include <cstddef>

#ifdef __cplusplus
extern "C" {
#endif

/* control functions. These return an error code. */
BMI_API int initialize(const char* config_file);

BMI_API int update();

BMI_API int finalize();


/* time control functions */
//BMI_API void get_start_time(double* t);
//
//BMI_API void get_end_time(double* t);
//
//BMI_API void get_current_time(double* t);
//
//BMI_API void get_time_step(double* dt);


/* variable info */
BMI_API void get_input_var_name_count(int* count);

BMI_API void get_output_var_name_count(int* count);

BMI_API void get_input_var_names(char** names);

BMI_API void get_output_var_names(char** names);

BMI_API void get_var_units(const char* name, char* units);

BMI_API void get_var_type(const char* name, char* type);

BMI_API void get_var_itemsize(const char* name, int* itemsize);

BMI_API void get_var_rank(const char* name, int* rank);

BMI_API void get_var_size(const char* name, int* size);

BMI_API void get_var_nbytes(const char* name, int* nbytes);


/* data access */
BMI_API void get_value(const char* name, char* buffer);

BMI_API void set_value(const char* name, char* buffer);


#ifdef __cplusplus
}
#endif

#endif //TYPO_BMI_H


#endif //LOOKUP_BMI_H
