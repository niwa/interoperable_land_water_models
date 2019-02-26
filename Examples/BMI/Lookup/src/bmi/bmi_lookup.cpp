#include <string>
#include <cstring>
#include <algorithm>

#include "bmi_lookup.h"
#include "lookup.h"

lup::Lookup* _lookup = nullptr;
std::vector<lup::Input> _inputs;

extern "C" {

/* control functions. These return an error code. */
BMI_API int initialize(const char *config_file) {
    auto filename = std::string (config_file);
    _lookup = lup::Lookup::Create(filename);

    for (const auto &name : _lookup->get_input_names()) {
        auto type = _lookup->get_var_type(name);

        auto input = lup::Input {};
        if (type == "str") {
            input = lup::Input {name, ""};
        } else if (type == "double") {
            input = lup::Input {name, 0.0};
        }
        _inputs.push_back(input);
    }

    return 0;
}


BMI_API int update() {
    return 0;
}


BMI_API int finalize() {
    _inputs.clear();
    lup::Lookup::Dispose(_lookup);
    return 0;
}

/* variable info */
BMI_API void get_input_var_name_count(int* count) {
    *count = _lookup->count_inputs();
}


BMI_API void get_output_var_name_count(int* count) {
    *count = _lookup->count_outputs();
}


BMI_API void get_input_var_names(char** names) {
    auto labels = _lookup->get_input_names();
    for (auto it = labels.begin(); it != labels.end(); ++it) {
        auto index = it - labels.begin();
        auto label = labels[index];
        strncpy_s(names[index], MAXSTRINGLEN, label.c_str(), label.size() + 1);
    }
}


BMI_API void get_output_var_names(char** names) {
    auto labels = _lookup->get_output_names();
    for (auto it = labels.begin(); it != labels.end(); ++it) {
        auto index = std::distance(labels.begin(), it);
        auto label = labels[index];
        strncpy_s(names[index], MAXSTRINGLEN, label.c_str(), label.size() + 1);
    }
}


BMI_API void get_var_units(const char* name, char* units) {
    auto _units = _lookup->get_var_units(name);
    strncpy_s(units, MAXSTRINGLEN, _units.c_str(), _units.size() + 1);
}


BMI_API void get_var_type(const char* name, char* type) {
    auto _type = _lookup->get_var_type(name);
    strncpy_s(type, MAXSTRINGLEN, _type.c_str(), _type.size() + 1);
}


BMI_API void get_var_itemsize(const char* name, int* itemsize) {
    auto type = _lookup->get_var_type(name);
    if (type == "str") *itemsize = (int) sizeof(char);
    if (type == "double") *itemsize = (int) sizeof(double);
}


BMI_API void get_var_rank(const char* name, int* rank) {
    auto type = _lookup->get_var_type(name);
    if (type == "str") *rank = 1;
    if (type == "double") *rank = 0;
}


BMI_API void get_var_size(const char* name, int* size) {
    auto type = _lookup->get_var_type(name);
    if (type == "str") *size = MAXSTRINGLEN;
    if (type == "double") *size = 1;
}


BMI_API void get_var_nbytes(const char* name, int* nbytes) {
    int itemsize, size = 0;
    get_var_itemsize(name, &itemsize);
    get_var_size(name, &size);
    *nbytes = itemsize * size;
}


/* data access */
BMI_API void get_value(const char* name, char* buffer) {
    auto value = _lookup->get_value(_inputs, std::string {name});
    /* All outputs are doubles, so keeping it simple */
    memcpy(buffer, &value, sizeof(double));
}

BMI_API void set_value(const char* name, char* buffer) {
    /* Find target input */
    lup::Input* input = nullptr;
    for (auto &_input : _inputs) {
        if (_input.name == name) {
            input = &_input;
            break;
        }
    }

    /* Error: Target input not found */
    if (input == nullptr) return;

    /* OK, proceed setting input value */
    switch (input->value.index()) {
        case 0: {
            auto value = std::string {buffer};
            input->value = value;
            break;
        }
        case 1: {
            input->value = *((double *) buffer);
            break;
        }
        default:
            break;
    }
}

} // extern "c"