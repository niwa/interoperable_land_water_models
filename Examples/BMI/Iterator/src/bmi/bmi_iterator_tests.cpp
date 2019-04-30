#include <cstring>
#include <iostream>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "bmi_iterator.h"

// This should match the name defined in bmit.h
#define BMIT_SQLITE_PTR_NAME "sqlite__pointer"

// Tests use a simple target BMI component: bmi_summer
// It takes two scaler inputs (input_a & input_b)
// and yields a sigle scalar output (output_c),
// the sum of its inputs.
// The paths to the compiled test target and it's config file are set
// in the header below:
#include "target_settings.h"


SCENARIO("Initializing and finalizing") {

    GIVEN("A valid config file and input data") {

        std::string const input_a_csv_file = "_temporary_input_a.csv";
        std::string const input_b_csv_file = "_temporary_input_b.csv";
        std::string const output_c_csv_file = "_temporary_output_c.csv";

        std::ofstream csvfile;

        csvfile.open(input_a_csv_file, std::ios::out);
        csvfile << "A1,A2\n";
        csvfile << "10,11\n";
        csvfile << "20,21\n";
        csvfile << "30,31\n";
        csvfile.close();

        csvfile.open(input_b_csv_file, std::ios::out);
        csvfile << "B1,B2\n";
        csvfile << "1000,1000\n";
        csvfile << "2000,2000\n";
        csvfile << "3000,3000\n";
        csvfile.close();

        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: int, format: csv, path: "
                 << input_a_csv_file << "}\n";
        yamlfile << "  input_b: {type: int, format: csv, path: "
                 << input_b_csv_file << "}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_c: {type: int, format: csv, path: "
                 << output_c_csv_file << "}\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();

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
        remove(input_a_csv_file.c_str());
        remove(input_b_csv_file.c_str());
        remove(output_c_csv_file.c_str());
        remove(config_file.c_str());
    }
}


SCENARIO("Getting variable info") {

    GIVEN("An initialized bmi iterator instance and input data") {

        std::string const input_a_csv_file = "_temporary_input_a.csv";
        std::string const input_b_csv_file = "_temporary_input_b.csv";
        std::string const output_c_csv_file = "_temporary_output_c.csv";

        std::ofstream csvfile;

        csvfile.open(input_a_csv_file, std::ios::out);
        csvfile << "A1,A2\n";
        csvfile << "10,11\n";
        csvfile << "20,21\n";
        csvfile << "30,31\n";
        csvfile.close();

        csvfile.open(input_b_csv_file, std::ios::out);
        csvfile << "B1,B2\n";
        csvfile << "1000,1000\n";
        csvfile << "2000,2000\n";
        csvfile << "3000,3000\n";
        csvfile.close();

        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: int, format: csv, path: "
                 << input_a_csv_file << "}\n";
        yamlfile << "  input_b: {type: int, format: csv, path: "
                 << input_b_csv_file << "}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_c: {type: int, format: csv, path: "
                 << output_c_csv_file << "}\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();
        CHECK(initialize(config_file.c_str()) == 0);

        THEN("The number of input variables can be retrieved") {
            int n = 0;
            get_input_var_name_count(&n);
            CHECK(n == 2 + 1);
        }

        THEN("The number of output variables can be retrieved") {
            int n = 0;
            get_output_var_name_count(&n);
            CHECK(n == 1);
        }

        THEN("Input names can be retrieved") {
            int nb_inputs;
            get_input_var_name_count(&nb_inputs);

            char* names[nb_inputs];
            for (int i = 0; i < nb_inputs; i++) {
                names[i] = (char*) malloc(MAXSTRINGLEN * sizeof(char));
            }

            get_input_var_names(names);
            CHECK(!strcmp(names[0], "input_a"));
            CHECK(!strcmp(names[1], "input_b"));
            CHECK(!strcmp(names[2], BMIT_SQLITE_PTR_NAME));

            for (int i = 0; i < nb_inputs; i++) {
                free (names[i]);
            }
        }

        THEN("Output names can be retrieved") {
            int nb_outputs;
            get_output_var_name_count(&nb_outputs);

            char* names[nb_outputs];
            for (int i = 0; i < nb_outputs; i++) {
                names[i] = (char*) malloc(MAXSTRINGLEN * sizeof(char));
            }

            get_output_var_names(names);
            CHECK(!strcmp(names[0], "output_c"));

            for (int i = 0; i < nb_outputs; i++) {
                free (names[i]);
            }
        }

        THEN("Variable types can be retrieved") {
            char type[MAXSTRINGLEN];

            /* Inputs */
            get_var_type("input_a", type); CHECK(!strcmp(type, "int"));
            get_var_type("input_b", type); CHECK(!strcmp(type, "int"));
            get_var_type(BMIT_SQLITE_PTR_NAME, type); CHECK(!strcmp(type, "int64"));

            /* Outputs */
            get_var_type("output_c", type);
            CHECK(!strcmp(type, "int"));
        }

        THEN("Variable units can be retrieved") {
            // The expected units are defined in the test target (bmi_summer)

            char units[MAXSTRINGLEN];

            /* Inputs */
            get_var_units("input_a", units); CHECK(!strcmp(units, "-"));
            get_var_units("input_b", units); CHECK(!strcmp(units, "-"));
            get_var_units(BMIT_SQLITE_PTR_NAME, units); CHECK(!strcmp(units, "-"));

            /* Outputs */
            get_var_units("output_c", units); CHECK(!strcmp(units, "-"));
        }

        THEN("Variable item size can be retrieved") {
            int itemsize = -1;
            get_var_itemsize("input_a", &itemsize); CHECK(itemsize == sizeof(int));
            get_var_itemsize("input_b", &itemsize); CHECK(itemsize == sizeof(int));
            get_var_itemsize("output_c", &itemsize); CHECK(itemsize == sizeof(int));

            get_var_itemsize(BMIT_SQLITE_PTR_NAME, &itemsize);
            CHECK(itemsize == sizeof(int*));
        }

        THEN("Variable rank can be retrieved") {
            int rank = -1;
            get_var_rank("input_a", &rank); CHECK(rank == 2);
            get_var_rank("input_b", &rank); CHECK(rank == 2);
            get_var_rank("output_c", &rank); CHECK(rank == 2);
            get_var_rank(BMIT_SQLITE_PTR_NAME, &rank); CHECK(rank == 0);
        }

        THEN("Variable size can be retrieved") {
            int sizes[2] = {0, 0};
            get_var_size("input_a", sizes); CHECK(sizes[0] == 3);
                                            CHECK(sizes[1] == 2);
            get_var_size("input_b", sizes); CHECK(sizes[0] == 3);
                                            CHECK(sizes[1] == 2);
            get_var_size("output_c", sizes); CHECK(sizes[0] == 3);
                                             CHECK(sizes[1] == 2);

            int size;
            get_var_size(BMIT_SQLITE_PTR_NAME, &size); CHECK(size == 1);
        }

        THEN("Variable number of bytes can be retrieved") {
            int nbytes = -1;
            get_var_nbytes("input_a", &nbytes); CHECK(nbytes == sizeof(int) * 6);
            get_var_nbytes("input_b", &nbytes); CHECK(nbytes == sizeof(int) * 6);
            get_var_nbytes("output_c", &nbytes); CHECK(nbytes == sizeof(int) * 6);
            get_var_nbytes(BMIT_SQLITE_PTR_NAME, &nbytes); CHECK(nbytes == 8);
        }

        CHECK(finalize() == 0);

        /* Delete temporary file */
        remove(input_a_csv_file.c_str());
        remove(input_b_csv_file.c_str());
        remove(output_c_csv_file.c_str());
        remove(config_file.c_str());
    }
}


SCENARIO("Running the iterator on SQL tables") {
    // ToDo
}
