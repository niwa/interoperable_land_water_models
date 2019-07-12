#include <string>
#include <cstring>
#include <algorithm>

#include "bmi.h"
#include "lookup.h"

Logger logger = NULL;

lup::Lookup* _lookup = nullptr;
std::vector<lup::Input> _inputs;
double* _outputs;


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
        } else {
            // TODO: log this (unsupported variable type)
            lup::Lookup::Dispose(_lookup);
            return -1;
        }
        _inputs.push_back(input);
    }

    // Initialize memory to store outputs
    _outputs = (double*) malloc(_lookup->count_outputs() * sizeof(double));

    return 0;
}


BMI_API int update(double dt) {
    try {
        // Retrieve all outputs and copy to local mem
        int index = 0;
        for (auto value : _lookup->get_values(_inputs)) {
            _outputs[index++] = value;
        }
    }
    catch (std::exception e) {
        return -1;
    }

    return 0;
}


BMI_API int finalize() {
    _inputs.clear();
    lup::Lookup::Dispose(_lookup);
    delete _outputs;
    return 0;
}


/* time control functions */
BMI_API void get_start_time(double *t) {}

BMI_API void get_end_time(double *t) {}

BMI_API void get_current_time(double *t) {}

BMI_API void get_time_step(double *dt) {}


/* variable info */
BMI_API void get_var_shape(const char* name,  int shape[MAXDIMS]) {
    // This is a point model, all variables are scalars
    for (int i = 0; i < MAXDIMS; i++) {
        shape[i] = 0;
    }
    shape[0] = 1;
}


BMI_API void get_var_rank(const char* name, int* rank) {
    *rank = 1;
}


BMI_API void get_var_type(const char* name, char* type) {
    auto _type = _lookup->get_var_type(name);
    strncpy_s(type, MAXSTRINGLEN, _type.c_str(), _type.size() + 1);
}


BMI_API void get_var_count(int* count) {
    *count = _lookup->count_inputs() + _lookup->count_outputs();
}


BMI_API void get_var_name(int index, char* name) {
    int n_inputs = _lookup->count_inputs();
    int n_total = n_inputs + _lookup->count_outputs();

    std::string _name;

    if (index < n_inputs) {
        auto names = _lookup->get_input_names();
        _name = names[index];

    }
    else if(index < n_total) {
        auto names = _lookup->get_output_names();
        _name = names[ (size_t) index - n_inputs];
    }
    else {
        return;
    }

    strncpy_s(name, MAXSTRINGLEN, _name.c_str(), _name.size() + 1);
}


/* data access */
BMI_API void get_var(const char *name, void **ptr) {
    int index = _lookup->get_output_index(name);
    *ptr = (void*) &(_outputs[index]);
}


BMI_API void set_var(const char *name, const void *ptr) {
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
            auto value = std::string {(char*) ptr};
            input->value = value;
            break;
        }
        case 1: {
            input->value = *((double *) ptr);
            break;
        }
        default:
            break;
    }
}

BMI_API void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to cxx model";
	logger = callback;
	logger(level, msg.c_str());
}

