#include <cstring>
#include <fstream>
#include <iostream>

#include "catch.hpp"
#include "bmi.h"

static const Level log_level = LEVEL_NONE;

static void log(Level level, const char* msg) {
    if (level >= log_level) {
        std::cout << msg << std::endl;
    }
}


SCENARIO("Initializing and finalizing") {
    set_logger((Logger) log);

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
    set_logger((Logger) log);

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


TEST_CASE("Getting and setting values") {
    set_logger((Logger) log);

    std::string const filename = "_lookup_temproary_test_file.yaml";
    std::ofstream yamlfile;
    yamlfile.open(filename, std::ios::out);
    yamlfile << "inputs:\n";
    yamlfile << "  input_a: {type: str}\n";
    yamlfile << "  input_b: {type: double}\n";
    yamlfile << "  input_c: {type: int}\n";
    yamlfile << "outputs:\n";
    yamlfile << "  output_x: {type: double}\n";
    yamlfile << "mappings:\n";
    yamlfile << "  input_b:\n";
    yamlfile << "    a1: class_a1\n";
    yamlfile << "    a2: class_a2\n";
    yamlfile << "  input_b:\n";
    yamlfile << "    1.0: class_b1\n";
    yamlfile << "    2.0: class_b2\n";
    yamlfile << "  input_c:\n";
    yamlfile << "    1: class_c1\n";
    yamlfile << "    2: class_c2\n";
    // yamlfile << "lookup:\n";
    // yamlfile << "  class_a1:\n";
    // yamlfile << "    class_b1:\n";
    // yamlfile << "      c1: [111.1, 111.2]\n";
    // yamlfile << "      c2: [112.1, 112.2]\n";
    // yamlfile << "    class_b2:\n";
    // yamlfile << "      c1: [121.1, 121.2]\n";
    // yamlfile << "      c2: [122.1, 122.2]\n";
    // yamlfile << "  class_a2:\n";
    // yamlfile << "    class_b1:\n";
    // yamlfile << "      c1: [211.1, 211.2]\n";
    // yamlfile << "      c2: [212.1, 212.2]\n";
    // yamlfile << "    class_b2:\n";
    // yamlfile << "      c1: [221.1, 221.2]\n";
    // yamlfile << "      c2: [222.1, 222.2]\n";
    yamlfile.close();

    REQUIRE(initialize(filename.c_str()) == 0);

    SECTION("Input variables of type str") {
        auto set_value = std::string {"abcdef"};
        set_var("input_a", (void*) set_value.c_str());

        char* ptr;
        get_var("input_a", (void**) &ptr);
        auto get_value = std::string(ptr);

        CHECK(get_value == set_value);

    }

    SECTION("Input variables of type double") {
        auto set_value = double (123.4);
        set_var("input_b", (void*) &set_value);

        double* ptr;
        get_var("input_b", (void**) &ptr);
        auto get_value = double(*ptr);

        CHECK(get_value == set_value);
    }

    SECTION("Input variables of type int") {
        auto set_value = int (256);
        set_var("input_c", (void*) &set_value);

        int* ptr;
        get_var("input_c", (void**) &ptr);
        auto get_value = int(*ptr);

        CHECK(get_value == set_value);
    }

    SECTION("Output variables of type double") {
        auto set_value = double (123.4);
        set_var("output_x", (void*) &set_value);

        double* ptr;
        get_var("output_x", (void**) &ptr);
        auto get_value = double(*ptr);

        CHECK(get_value == set_value);
    }

        // WHEN("Setting input values") {
        //     auto s_value = std::string {};
        //     auto d_value = double {};

        //     s_value = "a1"; // maps to class_a1
        //     set_var("input_a", (void*) s_value.c_str());

        //     d_value = 2.5; // maps to class_b2
        //     set_var("input_b", (void*) &d_value);

        //     s_value = "c1"; // no mapping
        //     set_var("input_c", (void*) s_value.c_str());

        //     THEN("Outputs can be retrieved after calling update") {
        //         CHECK(update(0) == 0);


        //         double* ptr;
        //         get_var("TN", (void **) &ptr);
        //         CHECK(*ptr == 121.1);

        //         get_var("TP", (void **) &ptr);
        //         CHECK(*ptr == 121.2);
        //     }
        // }

        // THEN("Finalizing returns no errors") {
        //     CHECK(finalize() == 0);
        // }

    /* Delete temporary file */
    remove(filename.c_str());
}





TEST_CASE("Running the typology lookup") {
    set_logger((Logger) log);

    std::string const cfg_file = "typologies.yml";
    CHECK(initialize(cfg_file.c_str()) == 0);

    SECTION("Retrieving variable names") {
        char buffer[MAXSTRINGLEN];
        // Inputs
        get_var_name(0, buffer); CHECK(!strcmp(buffer, "model_land-use-type__identification_number"));
        get_var_name(1, buffer); CHECK(!strcmp(buffer, "model_climate-type__identification_number"));
        get_var_name(2, buffer); CHECK(!strcmp(buffer, "model_basin__slope"));
        get_var_name(3, buffer); CHECK(!strcmp(buffer, "model_soil-type__identification_number"));
        get_var_name(4, buffer); CHECK(!strcmp(buffer, "model_basin_irrigation_area__fraction"));
        get_var_name(5, buffer); CHECK(!strcmp(buffer, "atmosphere_water~10-year-average__precipitation_volume_flux"));
        get_var_name(6, buffer); CHECK(!strcmp(buffer, "anion_storage__capacity"));
        // Outputs
        get_var_name(7, buffer); CHECK(!strcmp(buffer, "model_basin_N__loss"));
        get_var_name(8, buffer); CHECK(!strcmp(buffer, "model_basin_P__loss"));
    }

    SECTION("Retrieving variable types") {
        char type[MAXSTRINGLEN];
        // Inputs
        get_var_type("model_land-use-type__identification_number", type);
        CHECK(!strcmp(type, "int"));
        get_var_type("model_climate-type__identification_number", type);
        CHECK(!strcmp(type, "int"));
        get_var_type("model_basin__slope", type);
        CHECK(!strcmp(type, "double"));
        get_var_type("model_soil-type__identification_number", type);
        CHECK(!strcmp(type, "int"));
        get_var_type("model_basin_irrigation_area__fraction", type);
        CHECK(!strcmp(type, "double"));
        get_var_type("atmosphere_water~10-year-average__precipitation_volume_flux", type);
        CHECK(!strcmp(type, "double"));
        get_var_type("anion_storage__capacity", type);
        CHECK(!strcmp(type, "double"));
        // Outputs
        get_var_type("model_basin_N__loss", type);
        CHECK(!strcmp(type, "double"));
        get_var_type("model_basin_P__loss", type);
        CHECK(!strcmp(type, "double"));
    }

    SECTION("Retrieving variable ranks") {
        int rank = -1;
        // Inputs
        get_var_rank("model_land-use-type__identification_number", &rank);
        CHECK(rank == 1);
        get_var_rank("model_climate-type__identification_number", &rank);
        CHECK(rank == 1);
        get_var_rank("model_basin__slope", &rank);
        CHECK(rank == 1);
        get_var_rank("model_soil-type__identification_number", &rank);
        CHECK(rank == 1);
        get_var_rank("model_basin_irrigation_area__fraction", &rank);
        CHECK(rank == 1);
        get_var_rank("atmosphere_water~10-year-average__precipitation_volume_flux", &rank);
        CHECK(rank == 1);
        get_var_rank("anion_storage__capacity", &rank);
        CHECK(rank == 1);
        // Outputs
        get_var_rank("model_basin_N__loss", &rank);
        CHECK(rank == 1);
        get_var_rank("model_basin_P__loss", &rank);
        CHECK(rank == 1);
    }

    SECTION("Retrieving variable shapes") {
        // Inputs
        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_land-use-type__identification_number", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_climate-type__identification_number", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_basin__slope", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_soil-type__identification_number", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_basin_irrigation_area__fraction", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("atmosphere_water~10-year-average__precipitation_volume_flux", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("anion_storage__capacity", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        // Outputs
        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_basin_N__loss", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }

        {
            int shape[MAXDIMS] = {0};
            get_var_shape("model_basin_P__loss", shape);
            CHECK(shape[0] == 1);
            CHECK(shape[1] == 0);
        }
    }

    SECTION("Looking up a known input combination") {
        auto int_buff = int {};
        auto dbl_buff = double {};

        int_buff = 1;
        set_var("model_land-use-type__identification_number", (void*) &int_buff);

        int_buff = 2;
        set_var("model_climate-type__identification_number", (void*) &int_buff);

        dbl_buff = 1;
        set_var("model_basin__slope", (void*) &dbl_buff);

        int_buff = 3;
        set_var("model_soil-type__identification_number", (void*) &int_buff);

        dbl_buff = 0.0;
        set_var("model_basin_irrigation_area__fraction", (void*) &dbl_buff);

        dbl_buff = 1700.0;
        set_var("atmosphere_water~10-year-average__precipitation_volume_flux", (void*) &dbl_buff);

        dbl_buff = 10.0;
        set_var("anion_storage__capacity", (void*) &dbl_buff);

        // Outputs can be retrieved after calling update
        REQUIRE(update(0) == 0);

        double* ptr;
        get_var("model_basin_N__loss", (void **) &ptr);
        CHECK(*ptr == 127.0);

        get_var("model_basin_P__loss", (void **) &ptr);
        CHECK(*ptr == 2.80);
    }

    SECTION("Looking up an unknwon input combination with fallback") {
        auto int_buff = int {};
        auto dbl_buff = double {};

        int_buff = 1;
        set_var("model_land-use-type__identification_number", (void*) &int_buff);

        int_buff = 2;
        set_var("model_climate-type__identification_number", (void*) &int_buff);

        dbl_buff = 1;
        set_var("model_basin__slope", (void*) &dbl_buff);

        int_buff = 3;
        set_var("model_soil-type__identification_number", (void*) &int_buff);

        dbl_buff = 0.0;
        set_var("model_basin_irrigation_area__fraction", (void*) &dbl_buff);

        dbl_buff = 1200.0;
        set_var("atmosphere_water~10-year-average__precipitation_volume_flux", (void*) &dbl_buff);

        dbl_buff = 10.0;
        set_var("anion_storage__capacity", (void*) &dbl_buff);

        // Outputs can be retrieved after calling update
        REQUIRE(update(0) == 0);

        double* ptr;
        get_var("model_basin_N__loss", (void **) &ptr);
        CHECK(*ptr == -999.9);

        get_var("model_basin_P__loss", (void **) &ptr);
        CHECK(*ptr == -999.9);
    }

    REQUIRE(finalize() == 0);
}
