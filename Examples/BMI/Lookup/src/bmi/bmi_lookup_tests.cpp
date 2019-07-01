#include <cstring>
#include <fstream>

#include "catch.hpp"
#include "bmi.h"


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

        THEN("The number variables can be retrieved") {
            int n = 0;
            get_var_count(&n);
            CHECK(n == 3 + 2);
        }

        THEN("Variable names can be retrieved") {
            char buffer[MAXSTRINGLEN];
            // Inputs
            get_var_name(0, buffer); CHECK(!strcmp(buffer, "input_a"));
            get_var_name(1, buffer); CHECK(!strcmp(buffer, "input_b"));
            get_var_name(2, buffer); CHECK(!strcmp(buffer, "input_c"));
            // Outputs
            get_var_name(3, buffer); CHECK(!strcmp(buffer, "TN"));
            get_var_name(4, buffer); CHECK(!strcmp(buffer, "TP"));
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

        THEN("Variable rank can be retrieved") {
            int rank = -1;
            get_var_rank("input_a", &rank); CHECK(rank == 1);
            get_var_rank("input_b", &rank); CHECK(rank == 1);
            get_var_rank("input_c", &rank); CHECK(rank == 1);
            get_var_rank("TN", &rank); CHECK(rank == 1);
            get_var_rank("TP", &rank); CHECK(rank == 1);
        }

        THEN("Variable shape can be retrieved") {
            
            // Inputs
            {
                int shape[MAXDIMS] = {0};
                get_var_shape("input_a", shape);
                CHECK(shape[0] == 1);
                CHECK(shape[1] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("input_b", shape);
                CHECK(shape[0] == 1);
                CHECK(shape[1] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("input_c", shape);
                CHECK(shape[0] == 1);
                CHECK(shape[1] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("TN", shape);
                CHECK(shape[0] == 1);
                CHECK(shape[1] == 0);
            }

            {
                int shape[MAXDIMS] = {0};
                get_var_shape("TP", shape);
                CHECK(shape[0] == 1);
                CHECK(shape[1] == 0);
            }
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
                CHECK(update(0) == 0);


                double* ptr;
                get_var("TN", (void **) &ptr);
                CHECK(*ptr == 121.1);

                get_var("TP", (void **) &ptr);
                CHECK(*ptr == 121.2);
            }
        }


        THEN("Finalizing returns no errors") {
            CHECK(finalize() == 0);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}
