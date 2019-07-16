#include <algorithm>
#include <cstring>
#include <sstream>
#include <string>

#include "bmi.h"
#include "lookup.h"

Logger logger = NULL;

lup::Lookup* _lookup = nullptr;
std::vector<lup::Input> _inputs;
int _nb_inputs;
int _nb_outputs;
double* _outputs;


// Wrap logger to check for nullptr
static void log (Level level, std::string msg) {
    if (logger != nullptr) {
        logger(level, msg.c_str());
    }
}

// Some get/set helpers
static void get_var_input(const int index, void **ptr);
static void set_var_input(const int index, const void *ptr);


/* control functions. These return an error code. */
BMI_API int initialize(const char *config_file) {
    log(LEVEL_DEBUG, "Initializing lookup");
    auto filename = std::string (config_file);

    try {
        _lookup = lup::Lookup::Create(filename);
        _nb_inputs = _lookup->count_inputs();
        _nb_outputs = _lookup->count_outputs();
    }
    catch (std::exception& e) {
        auto msg = std::stringstream {};
        msg << "Failed initializing lookup: " << e.what();
        log(LEVEL_FATAL, msg.str());
    }

    // Instantiate inputs
    for (const auto &name : _lookup->get_input_names()) {
        auto type = _lookup->get_var_type(name);

        auto input = lup::Input {};
        if (type == "str") {
            input = lup::Input {name, ""};
        } else if (type == "double") {
            input = lup::Input {name, 0.0};
        } else if (type == "int") {
            input = lup::Input {name, 0};
        } else {
            log(
                LEVEL_FATAL,
                "Unsupported input variable type in lookup configuration"
            );
            lup::Lookup::Dispose(_lookup);
            return -1;
        }
        _inputs.push_back(input);
    }

    // Allocate outputs
    _outputs = (double*) malloc(_lookup->count_outputs() * sizeof(double));

    log(LEVEL_INFO, "Initialized lookup");
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
    catch (std::exception& e) {
        auto msg = std::stringstream {};
        msg << "In lookup update call: " <<  e.what();
        log(LEVEL_ERROR, msg.str());
        return -1;
    }

    return 0;
}


BMI_API int finalize() {
    log(LEVEL_DEBUG, "Finalizing lookup");
    _inputs.clear();
    lup::Lookup::Dispose(_lookup);
    delete _outputs;
    log(LEVEL_INFO, "Finalized lookup");
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
    try {
        int index = _lookup->get_var_index(name);
        if (index < _nb_inputs) {
            // Inputs
            get_var_input(index, ptr);
        } else {
            // Outputs
            index -= _nb_inputs;
            *ptr = (void*) &(_outputs[index]);
        }
    }
    catch (std::exception& e) {
        std::stringstream msg;
        msg << "BMI Lookup get_var error: " << e.what();
        log(LEVEL_ERROR, msg.str());
        // TODO: remove re-throw once logger is set by iterator
        throw;
    }
}


BMI_API void set_var(const char *name, const void *ptr) {
    try {
        int index = _lookup->get_var_index(name);
        if (index < _nb_inputs) {
            // Inputs
            set_var_input(index, ptr);
        } else {
            // Outputs
            index -= _nb_inputs;
            _outputs[index] = *((double *) ptr);
        }
    }
    catch (std::exception& e) {
        std::stringstream msg;
        msg << "BMI Lookup get_var error: " << e.what();
        log(LEVEL_ERROR, msg.str());
        // TODO: remove throw once logger is set by iterator
        throw;
    }
}


/* logging */
BMI_API void set_logger(Logger callback)
{
	Level level = LEVEL_INFO;
	std::string msg = "Logging attached to BMI Lookup";
	logger = callback;
	logger(level, msg.c_str());
}


/* helpers */
static void get_var_input(const int index, void **ptr) {
    auto& input = _inputs[index];
    switch (input.value.index()) {
        case 0: {
            *ptr = (void*) std::get_if<std::string>(&input.value)->c_str();
            break;
        }
        case 1: {
            *ptr = (void*) std::get_if<double>(&input.value);
            break;
        }
        case 2: {
            *ptr = (void*) std::get_if<int>(&input.value);
            break;
        }
        default:
            log(LEVEL_ERROR, "BMI lookup get_var: Unhandled lookup variable type");
            // TODO: remove throw once logger is set by iterator
            throw std::runtime_error("BMI lookup get_var: Unhandled lookup variable type");
    }
}


static void set_var_input(const int index, const void *ptr) {
    auto& input = _inputs[index];

    switch (input.value.index()) {
        case 0: {
            auto value = std::string {(char*) ptr};
            input.value = value;
            break;
        }
        case 1: {
            input.value = *((double *) ptr);
            break;
        }
        case 2: {
            input.value = *((int *) ptr);
            break;
        }
        default:
            log(LEVEL_ERROR, "BMI lookup set_var: Unhandled lookup variable type");
            // TODO: remove throw once logger is set by iterator
            throw std::runtime_error("BMI lookup set_var: Unhandled lookup variable type");
    }
}

