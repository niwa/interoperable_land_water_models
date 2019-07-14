#include <cstring>
#include <iostream>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "bmi.h"

// This should match the name defined in bmit.cpp
#define BMIT_SQLITE_PTR_NAME "sqlite__pointer"

// Tests use a simple target BMI component: bmi_concat
// See test/target/bmi_concat.cpp
//
// It takes two scalar inputs:
//     a string (input_str)
//     a double (input_val)
// and produces two scalar outputs:
//     a string (output_str) - the concatenation of both inputs
//     an integer (output_len) - the length of the output string
//
// The paths to the compiled test target and it's config file are set
// in the header below:
#include "target_settings.h"


static const Level log_level = LEVEL_INFO;

CALLCONV void log(Level level, const char* msg) {
    if (level >= log_level) {
        std::cout << msg << std::endl;
    }
}


SCENARIO("Initializing and finalizing") {
    set_logger((Logger) log);

    GIVEN("A valid config file and input data") {

        std::string const input_str_csv_file = "_temporary_input_str.csv";
        std::string const input_val_csv_file = "_temporary_input_val.csv";
        std::string const output_str_csv_file = "_temporary_output_str.csv";
        std::string const output_len_csv_file = "_temporary_output_len.csv";

        std::ofstream f;

        f.open(input_str_csv_file, std::ios::out);
        f << "A1;A2\n";
        f << "aa-;ab-\n";
        f << "ba-;bb-\n";
        f << "ca-;cb-\n";
        f.close();

        f.open(input_val_csv_file, std::ios::out);
        f << "B1,B2\n";
        f << "1.1,1.2\n";
        f << "2.1,2.2\n";
        f << "3.1,3.2\n";
        f.close();

        std::string const config_file = "_temporary_config.yml";
        f.open(config_file, std::ios::out);
        f << "inputs:\n";
        f << "  input_str:\n";
        f << "    type: str\n";
        f << "    format: csv\n";
        f << "    path: " << input_str_csv_file << "\n";
        f << "    sep: ';'\n";
        f << "  input_val:\n";
        f << "    type: double\n";
        f << "    format: csv\n";
        f << "    path: " << input_val_csv_file << "\n";
        f << "outputs:\n";
        f << "  output_str:\n";
        f << "    type: str\n";
        f << "    format: csv\n";
        f << "    path: " << output_str_csv_file << "\n";
        f << "    sep: ';'\n";
        f << "  output_len:\n";
        f << "    type: int\n";
        f << "    format: csv\n";
        f << "    path: " << output_len_csv_file << "\n";
        f << "    sep: ';'\n";
        f << "target:\n";
        f << "  library: " << TARGET_LIB << "\n";
        f << "  config: " << TARGET_CFG << "\n";
        f.close();


        THEN("A bmi iterator instance can be initialized") {
            CHECK(initialize(config_file.c_str()) == 0);
        }

        AND_THEN("A second call to initialize will fail") {
            CHECK(initialize(config_file.c_str()) != 0);
        }

        AND_THEN("The iterator instance can be finalized") {
            CHECK(finalize() == 0);
        }

        /* Delete temporary file */
        remove(input_str_csv_file.c_str());
        remove(input_val_csv_file.c_str());
        remove(output_str_csv_file.c_str());
        remove(output_len_csv_file.c_str());
        remove(config_file.c_str());
    }
}


SCENARIO("Getting variable info") {
    set_logger((Logger) log);

    GIVEN("An initialized bmi iterator instance and input data") {

        std::string const input_str_csv_file = "_temporary_input_str.csv";
        std::string const input_val_csv_file = "_temporary_input_val.csv";
        std::string const output_str_csv_file = "_temporary_output_str.csv";
        std::string const output_len_csv_file = "_temporary_output_len.csv";

        std::ofstream f;

        f.open(input_str_csv_file, std::ios::out);
        f << "A1;A2\n";
        f << "aa-;ab-\n";
        f << "ba-;bb-\n";
        f << "ca-;cb-\n";
        f.close();

        f.open(input_val_csv_file, std::ios::out);
        f << "B1,B2\n";
        f << "1.1,1.2\n";
        f << "2.1,2.2\n";
        f << "3.1,3.2\n";
        f.close();

        std::string const config_file = "_temporary_config.yml";
        f.open(config_file, std::ios::out);
        f << "inputs:\n";
        f << "  input_str:\n";
        f << "    type: str\n";
        f << "    format: csv\n";
        f << "    path: " << input_str_csv_file << "\n";
        f << "    sep: ';'\n";
        f << "  input_val:\n";
        f << "    type: double\n";
        f << "    format: csv\n";
        f << "    path: " << input_val_csv_file << "\n";
        f << "outputs:\n";
        f << "  output_str:\n";
        f << "    type: str\n";
        f << "    format: csv\n";
        f << "    path: " << output_str_csv_file << "\n";
        f << "    sep: ';'\n";
        f << "  output_len:\n";
        f << "    type: int\n";
        f << "    format: csv\n";
        f << "    path: " << output_len_csv_file << "\n";
        f << "    sep: ';'\n";
        f << "target:\n";
        f << "  library: " << TARGET_LIB << "\n";
        f << "  config: " << TARGET_CFG << "\n";
        f.close();
        CHECK(initialize(config_file.c_str()) == 0);

        THEN("The number of variables can be retrieved") {
            int n = 0;
            get_var_count(&n);
            // 2 target inputs + 2 target outputs + iterator own variables
            CHECK(n == 2 + 2 + 1);
        }

        THEN("Variable names can be retrieved") {
            char name[MAXSTRINGLEN];

            /* Target variables */
            get_var_name(0, name); CHECK(!strcmp(name, "input_str"));
            get_var_name(1, name); CHECK(!strcmp(name, "input_val"));
            get_var_name(2, name); CHECK(!strcmp(name, "output_str"));
            get_var_name(3, name); CHECK(!strcmp(name, "output_len"));

            /* Iterator variables */
            get_var_name(4, name); CHECK(!strcmp(name, BMIT_SQLITE_PTR_NAME));
        }

        THEN("Variable types can be retrieved") {
            char type[MAXSTRINGLEN];

            /* Target variables */
            get_var_type("input_str", type); CHECK(!strcmp(type, "str"));
            get_var_type("input_val", type); CHECK(!strcmp(type, "double"));
            get_var_type("output_str", type); CHECK(!strcmp(type, "str"));
            get_var_type("output_len", type); CHECK(!strcmp(type, "int"));

            /* Iterator variables */
            get_var_type(BMIT_SQLITE_PTR_NAME, type); CHECK(!strcmp(type, "int64"));
        }

        THEN("Variable rank can be retrieved") {
            int rank = -1;

            /* Target variables */
            get_var_rank("input_str", &rank); CHECK(rank == 2);
            get_var_rank("input_val", &rank); CHECK(rank == 2);
            get_var_rank("output_str", &rank); CHECK(rank == 2);
            get_var_rank("output_len", &rank); CHECK(rank == 2);

            /* Iterator variables */
            get_var_rank(BMIT_SQLITE_PTR_NAME, &rank); CHECK(rank == 1);
        }

        THEN("Variable shape can be retrieved") {
            /* Target variables */
            {
                int shape[MAXDIMS] = {0};
                get_var_shape("input_str", shape);
                CHECK(shape[0] == 3);
                CHECK(shape[1] == 2);
                CHECK(shape[2] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("input_val", shape);
                CHECK(shape[0] == 3);
                CHECK(shape[1] == 2);
                CHECK(shape[2] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("output_str", shape);
                CHECK(shape[0] == 3);
                CHECK(shape[1] == 2);
                CHECK(shape[2] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("output_len", shape);
                CHECK(shape[0] == 3);
                CHECK(shape[1] == 2);
                CHECK(shape[2] == 0);
            }

            /* Iterator variables */
            {
                int shape[MAXDIMS] = {0};
                get_var_shape(BMIT_SQLITE_PTR_NAME, shape);
                CHECK(shape[0] == 1);
                CHECK(shape[1] == 0);
            }
        }

        CHECK(finalize() == 0);

        /* Delete temporary file */
        remove(input_str_csv_file.c_str());
        remove(input_val_csv_file.c_str());
        remove(output_str_csv_file.c_str());
        remove(output_len_csv_file.c_str());
        remove(config_file.c_str());
    }
}


// SCENARIO("Running the iterator on SQL tables") {
//     // ToDo
// }
