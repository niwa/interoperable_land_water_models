#include <iostream>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "lookup.h"


SCENARIO("Querying variable info")
{
    GIVEN("A valid config file") {
        std::string const filename = "_lookup_temproary_test_file.yaml";
        std::ofstream yamlfile;
        yamlfile.open(filename, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_1: {type: str}\n";
        yamlfile << "  input_2: {type: double, units: \"%\"}\n";
        yamlfile << "  input_3: {type: str}\n";
        yamlfile << "\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_a: {type: double, units: kg}\n";
        yamlfile << "  output_b: {type: double, units: s}\n";
        yamlfile << "\n";
        yamlfile.close();

        WHEN("Initialized") {

            auto lookup = lup::Lookup::Create(filename);

            THEN("The number of inputs is defined") {
                CHECK(lookup->count_inputs() == 3);
            }

            THEN("The number of outputs is defined") {
                CHECK(lookup->count_outputs() == 2);
            }

            THEN("Inputs names are defined") {
                auto names = lookup->get_input_names();
                CHECK(names[0] == "input_1");
                CHECK(names[1] == "input_2");
                CHECK(names[2] == "input_3");
            }

            THEN("Outputs names are defined") {
                auto names = lookup->get_output_names();
                CHECK(names[0] == "output_a");
                CHECK(names[1] == "output_b");
            }

            THEN("Variable types are defined") {
                CHECK(lookup->get_var_type("input_1") == "str");
                CHECK(lookup->get_var_type("input_2") == "double");
                CHECK(lookup->get_var_type("input_3") == "str");
                CHECK(lookup->get_var_type("output_a") == "double");
                CHECK(lookup->get_var_type("output_b") == "double");
            }

            THEN("Variable units are defines") {
                CHECK(lookup->get_var_units("input_1").empty());
                CHECK(lookup->get_var_units("input_2") == "%");
                CHECK(lookup->get_var_units("input_3").empty());
                CHECK(lookup->get_var_units("output_a") == "kg");
                CHECK(lookup->get_var_units("output_b") == "s");
            }

            lup::Lookup::Dispose(lookup);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}


SCENARIO("String inputs can be mapped to classes")
{
    GIVEN("A valid config file") {
        std::string const filename = "_lookup_temproary_test_file.yaml";
        std::ofstream yamlfile;
        yamlfile.open(filename, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: str}\n";
        yamlfile << "  input_b: {type: str}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_x: {type: double, unit: m}\n";
        yamlfile << "  output_y: {type: double, unit: m}\n";
        yamlfile << "mappings:\n";
        yamlfile << "  input_a:\n";
        yamlfile << "    a1: class_a11\n";
        yamlfile << "    a2: class_a23\n";
        yamlfile << "    a3: class_a23\n";
        yamlfile << "\n";
        yamlfile << "lookup:\n";
        yamlfile << "  class_a11:\n";
        yamlfile << "    b1: [1.1, 10.1]\n";
        yamlfile << "    b2: [1.2, 10.2]\n";
        yamlfile << "  class_a23:\n";
        yamlfile << "    b1: [2.1, 20.1]\n";
        yamlfile << "    b2: [2.2, 20.2]\n";
        yamlfile << "\n";
        yamlfile.close();

        WHEN("Initialized") {

            auto lookup = lup::Lookup::Create(filename);

            THEN("Values can be retrieved") {
                std::vector<lup::Input> inputs;
                std::vector<double> outputs;

                inputs = {{"input_a", "a1"}, {"input_b", "b1"}};
                outputs = {1.1, 10.1};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);

                inputs = {{"input_a", "a2"}, {"input_b", "b1"}};
                outputs = {2.1, 20.1};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);

                inputs = {{"input_a", "a3"}, {"input_b", "b2"}};
                outputs = {2.2, 20.2};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);
            }

            THEN ("Requesting a non existing output throws an exception") {
                std::vector<lup::Input> inputs;
                inputs = {{"input_a", "a1"}, {"input_b", "b1"}};
                REQUIRE_THROWS_AS(lookup->get_value(inputs, "oooops"),
                        std::invalid_argument);
                REQUIRE_THROWS_WITH(lookup->get_value(inputs, "oooops"),
                        "Requested output does not exists");
            }

            THEN ("Requesting an unknown mapped input value throws an exception") {
                std::vector<lup::Input> inputs;
                inputs = {{"input_a", "oooops"}, {"input_b", "b1"}};
                REQUIRE_THROWS_AS(lookup->get_value(inputs,"output_x"),
                        std::invalid_argument);
                REQUIRE_THROWS_WITH(lookup->get_value(inputs, "output_x"),
                        "String input value not in mapping");
            }

            lup::Lookup::Dispose(lookup);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}


SCENARIO("Double inputs are classified by lower bounds")
{
    GIVEN("A valid config file") {
        std::string const filename = "_lookup_temproary_test_file.yaml";
        std::ofstream yamlfile;
        yamlfile.open(filename, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: double}\n";
        yamlfile << "  input_b: {type: double}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_x: {type: double, unit: m}\n";
        yamlfile << "  output_y: {type: double, unit: m}\n";
        yamlfile << "mappings:\n";
        yamlfile << "  input_a:\n";
        yamlfile << "    1: class_a1\n";
        yamlfile << "    5: class_a2\n";
        yamlfile << "  input_b:\n";
        yamlfile << "    10: class_b1\n";
        yamlfile << "    20: class_b2\n";
        yamlfile << "lookup:\n";
        yamlfile << "  class_a1:\n";
        yamlfile << "    class_b1: [1.1, 10.1]\n";
        yamlfile << "    class_b2: [1.2, 10.2]\n";
        yamlfile << "  class_a2:\n";
        yamlfile << "    class_b1: [2.1, 20.1]\n";
        yamlfile << "    class_b2: [2.2, 20.2]\n";
        yamlfile << "\n";
        yamlfile.close();

        WHEN("Initialized") {

            auto lookup = lup::Lookup::Create(filename);

            THEN("Values can be retrieved") {
                std::vector<lup::Input> inputs;
                std::vector<double> outputs;

                inputs = {{"input_a", 1.0}, {"input_b", 11}};
                outputs = {1.1, 10.1};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);

                inputs = {{"input_a", 1.3}, {"input_b", 20}};
                outputs = {1.2, 10.2};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);

                inputs = {{"input_a", 5}, {"input_b", 10.0}};
                outputs = {2.1, 20.1};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);

                inputs = {{"input_a", 5.1}, {"input_b", 3000}};
                outputs = {2.2, 20.2};
                CHECK(lookup->get_values(inputs) == outputs);
                CHECK(lookup->get_value(inputs, "output_x") == outputs[0]);
                CHECK(lookup->get_value(inputs, "output_y") == outputs[1]);
            }

            THEN ("Requesting a non existing output throws an exception") {
                std::vector<lup::Input> inputs;
                inputs = {{"input_a", 5}, {"input_b", 10.0}};
                REQUIRE_THROWS_AS(lookup->get_value(inputs, "oooops"),
                        std::invalid_argument);
                REQUIRE_THROWS_WITH(lookup->get_value(inputs, "oooops"),
                                  "Requested output does not exists");
            }

            THEN ("Requesting a value outside of mapping bounds throws an exception") {
                std::vector<lup::Input> inputs;
                // 0.0 < 1.0 !
                inputs = {{"input_a", 0.0}, {"input_b", 10.0}};
                REQUIRE_THROWS_AS(lookup->get_value(inputs, "output_x"),
                        std::invalid_argument);
                REQUIRE_THROWS_WITH(lookup->get_value(inputs, "output_x"),
                        "Double input value < lower mappin bound");
            }

            lup::Lookup::Dispose(lookup);
        }

        /* Delete temporary file */
        remove(filename.c_str());
    }
}