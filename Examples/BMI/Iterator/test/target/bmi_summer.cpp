#include <string>
#include <cstring>
#include <algorithm>
#include <iostream>
#include "bmi_summer.h"


int input_a;
int input_b;

extern "C" {

/* control functions. These return an error code. */
BMI_API int initialize(const char *config_file) {
    return 0;
}


BMI_API int update() {
    return 0;
}


BMI_API int finalize() {
    return 0;
}

/* variable info */
BMI_API void get_input_var_name_count(int* count) {
    *count = 2;
}


BMI_API void get_output_var_name_count(int* count) {
    *count = 1;
}


BMI_API void get_input_var_names(char** names) {
    strncpy_s(names[0], MAXSTRINGLEN, "input_a", 8);
    strncpy_s(names[1], MAXSTRINGLEN, "input_b", 8);
}


BMI_API void get_output_var_names(char** names) {
    strncpy_s(names[0], MAXSTRINGLEN, "output_c", 8);
}


BMI_API void get_var_units(const char* name, char* units) {
    strncpy_s(units, MAXSTRINGLEN, "-", 2);
}


BMI_API void get_var_type(const char* name, char* type) {
    strncpy_s(type, MAXSTRINGLEN, "int", 4);
}


BMI_API void get_var_itemsize(const char* name, int* itemsize) {
    *itemsize = (int) sizeof(int);
}


BMI_API void get_var_rank(const char* name, int* rank) {
    *rank = 0;
}


BMI_API void get_var_size(const char* name, int* size) {
    *size = 1;
}


BMI_API void get_var_nbytes(const char* name, int* nbytes) {
    int itemsize, size = 0;
    get_var_itemsize(name, &itemsize);
    get_var_size(name, &size);
    *nbytes = itemsize * size;
}


/* data access */
BMI_API void get_value(const char* name, char* buffer) {
    if (std::string {name} == "input_a") {
        memcpy(buffer, &input_a, sizeof(int));
    } else if (std::string {name} == "input_b") {
        memcpy(buffer, &input_b, sizeof(int));
    } else if (std::string {name} == "output_c") {
        int output = input_a + input_b;
        memcpy(buffer, &output, sizeof(int));
    }
}

BMI_API void set_value(const char* name, char* buffer) {
    if (std::string {name} == "input_a") {
        input_a = *((int*) buffer);
    } else if (std::string {name} == "input_b") {
        input_b = *((int*) buffer);
    }
}


} // extern "c"
