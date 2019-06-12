#include <cstring>
#include <fstream>

#include "catch.hpp"
#include "bmi_lookup.h"


SCENARIO("Initializing and finalizing") {

    GIVEN("A valid config file") {

        std::string const filename = "_lookup_temproary_test_file.yaml";
        std::ofstream yamlfile;
        yamlfile.open(filename, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: str}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_x: {type: double}\n";
        yamlfile.close();

        THEN("A bmi lookup instance can be initialized and finalized") {
            CHECK(initialize(filename.c_str()) == 0);
            CHECK(finalize() == 0);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}


SCENARIO("Variable info") {

    GIVEN("An initialized bmi lookup instance") {

        std::string const filename = "_lookup_temproary_test_file.yaml";
        std::ofstream yamlfile;
        yamlfile.open(filename, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: str}\n";
        yamlfile << "  input_b: {type: double, units: mm}\n";
        yamlfile << "  input_c: {type: str}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  TN: {type: double, units: kg/ha}\n";
        yamlfile << "  TP: {type: double, units: g/m2}\n";
        yamlfile << "mappings:\n";
        yamlfile << "  input_a:\n";
        yamlfile << "    a1: class_a1\n";
        yamlfile << "    a2: class_a2\n";
        yamlfile << "  input_b:\n";
        yamlfile << "    1.1: class_ax\n";
        yamlfile << "    2.6: class_ay\n";
        yamlfile.close();
        CHECK(initialize(filename.c_str()) == 0);

        THEN("The number of input variables can be retrieved") {
            int n = 0;
            get_input_var_name_count(&n);
            CHECK(n == 3);
        }

        THEN("The number of output variables can be retrieved") {
            int n = 0;
            get_output_var_name_count(&n);
            CHECK(n == 2);
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
            CHECK(!strcmp(names[2], "input_c"));

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
            CHECK(!strcmp(names[0], "TN"));
            CHECK(!strcmp(names[1], "TP"));

            for (int i = 0; i < nb_outputs; i++) {
                free (names[i]);
            }
        }

        THEN("Variable types can be retrieved") {
            char type[MAXSTRINGLEN];

            /* Inputs */
            get_var_type("input_a", type);
            CHECK(!strcmp(type, "str"));
            get_var_type("input_b", type);
            CHECK(!strcmp(type, "double"));
            get_var_type("input_c", type);
            CHECK(!strcmp(type, "str"));

            /* Outputs */
            get_var_type("TN", type);
            CHECK(!strcmp(type, "double"));
            get_var_type("TP", type);
            CHECK(!strcmp(type, "double"));
        }

        THEN("Variable units can be retrieved") {
            char units[MAXSTRINGLEN];

            /* Inputs */
            get_var_units("input_a", units); CHECK(!strcmp(units, ""));
            get_var_units("input_b", units); CHECK(!strcmp(units, "mm"));
            get_var_units("input_c", units); CHECK(!strcmp(units, ""));

            /* Outputs */
            get_var_units("TN", units); CHECK(!strcmp(units, "kg/ha"));
            get_var_units("TP", units); CHECK(!strcmp(units, "g/m2"));
        }

        THEN("Variable item size can be retrieved") {
            int itemsize = -1;
            get_var_itemsize("input_a", &itemsize); CHECK(itemsize == sizeof(char));
            get_var_itemsize("input_b", &itemsize); CHECK(itemsize == sizeof(double));
            get_var_itemsize("input_c", &itemsize); CHECK(itemsize == sizeof(char));
            get_var_itemsize("TN", &itemsize); CHECK(itemsize == sizeof(double));
            get_var_itemsize("TP", &itemsize); CHECK(itemsize == sizeof(double));
        }

        THEN("Variable rank can be retrieved") {
            int rank = -1;
            get_var_rank("input_a", &rank); CHECK(rank == 1);
            get_var_rank("input_b", &rank); CHECK(rank == 0);
            get_var_rank("input_c", &rank); CHECK(rank == 1);
            get_var_rank("TN", &rank); CHECK(rank == 0);
            get_var_rank("TP", &rank); CHECK(rank == 0);
        }

        THEN("Variable size can be retrieved") {
            int size = -1;
            get_var_size("input_a", &size); CHECK(size == MAXSTRINGLEN);
            get_var_size("input_b", &size); CHECK(size == 1);
            get_var_size("input_c", &size); CHECK(size == MAXSTRINGLEN);
            get_var_size("TN", &size); CHECK(size == 1);
            get_var_size("TP", &size); CHECK(size == 1);
        }

        THEN("Variable number of bytes can be retrieved") {
            int nbytes = -1;
            get_var_nbytes("input_a", &nbytes); CHECK(nbytes == sizeof(char) * MAXSTRINGLEN);
            get_var_nbytes("input_b", &nbytes); CHECK(nbytes == sizeof(double));
            get_var_nbytes("input_c", &nbytes); CHECK(nbytes == sizeof(char) * MAXSTRINGLEN);
            get_var_nbytes("TN", &nbytes); CHECK(nbytes == sizeof(double));
            get_var_nbytes("TP", &nbytes); CHECK(nbytes == sizeof(double));
        }

        THEN("Finalizing return no errors") {
            CHECK(finalize() == 0);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}


SCENARIO("Data access") {

    GIVEN("An initialized bmi lookup instance") {

        std::string const filename = "_lookup_temproary_test_file.yaml";
        std::ofstream yamlfile;
        yamlfile.open(filename, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: str}\n";
        yamlfile << "  input_b: {type: double, units: mm}\n";
        yamlfile << "  input_c: {type: str}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  TN: {type: double, units: kg/ha}\n";
        yamlfile << "  TP: {type: double, units: g/m2}\n";
        yamlfile << "mappings:\n";
        yamlfile << "  input_a:\n";
        yamlfile << "    a1: class_a1\n";
        yamlfile << "    a2: class_a2\n";
        yamlfile << "  input_b:\n";
        yamlfile << "    1.0: class_b1\n";
        yamlfile << "    2.5: class_b2\n";
        yamlfile << "lookup:\n";
        yamlfile << "  class_a1:\n";
        yamlfile << "    class_b1:\n";
        yamlfile << "      c1: [111.1, 111.2]\n";
        yamlfile << "      c2: [112.1, 112.2]\n";
        yamlfile << "    class_b2:\n";
        yamlfile << "      c1: [121.1, 121.2]\n";
        yamlfile << "      c2: [122.1, 122.2]\n";
        yamlfile << "  class_a2:\n";
        yamlfile << "    class_b1:\n";
        yamlfile << "      c1: [211.1, 211.2]\n";
        yamlfile << "      c2: [212.1, 212.2]\n";
        yamlfile << "    class_b2:\n";
        yamlfile << "      c1: [221.1, 221.2]\n";
        yamlfile << "      c2: [222.1, 222.2]\n";
        yamlfile.close();
        CHECK(initialize(filename.c_str()) == 0);

        WHEN("Setting input values") {
            auto s_value = std::string {};
            auto d_value = double {};

            s_value = "a1"; // maps to class_a1
            set_var("input_a", (void*) s_value.c_str());

            d_value = 2.5; // maps to class_b2
            set_var("input_b", (void*) &d_value);

            s_value = "c1"; // no mapping
            set_var("input_c", (void*) s_value.c_str());

            THEN("Outputs can be retrieved after calling update") {
                CHECK(update() == 0);

                double** value;
                get_var("TN", (void **) value);
                CHECK(**value == 121.1);

                get_var("TP", (void **) value);
                CHECK(**value == 121.2);
            }
        }


        THEN("Finalizing returns no errors") {
            CHECK(finalize() == 0);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}
