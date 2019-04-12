#include <iostream>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "bmit.h"

// Tests use a simple target BMI component: bmi_summer
// It takes two scaler inputs (input_a & input_b)
// and yields a sigle scalar output (output_c),
// the sum of its inputs.
// The paths to the compiled test target and it's config file are set
// in the header below:
#include "target_settings.h"


SCENARIO("Config validation") {

    GIVEN("An input count mismatch") {
        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yaml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_c: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "input count mismatch");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }

    GIVEN("An output count mismatch") {
        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yaml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "  input_b: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_c: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "  output_d: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "output count mismatch");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }

    GIVEN("An input name mismatch") {
        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yaml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "  ooooops: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_c: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "input name mismatch");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }

    GIVEN("An output name mismatch") {
        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yaml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "  input_b: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  ooooops: {type: int, format: csv, path: dummy.csv}\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "output name mismatch");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }
}


SCENARIO("CSV") {

	GIVEN("A target component and csv data") {
        std::string const input_a_csv_file = "_temporary_input_a.csv";
        std::string const input_b_csv_file = "_temporary_input_b.csv";
        std::string const output_c_csv_file = "_temporary_output_c.csv";

        std::ofstream csvfile;

        csvfile.open(input_a_csv_file, std::ios::out);
        csvfile << "A1;A2\n";
        csvfile << "10;11\n";
        csvfile << "20;21\n";
        csvfile << "30;31\n";
        csvfile.close();

        csvfile.open(input_b_csv_file, std::ios::out);
        csvfile << "B1,B2\n";
        csvfile << "1000,1000\n";
        csvfile << "2000,2000\n";
        csvfile << "3000,3000\n";
        csvfile.close();

        std::ofstream yamlfile;
        std::string const config_file = "_temporary_config.yaml";
        yamlfile.open(config_file, std::ios::out);
        yamlfile << "inputs:\n";
        yamlfile << "  input_a:\n";
        yamlfile << "    type: int\n";
        yamlfile << "    format: csv\n";
        yamlfile << "    path: " << input_a_csv_file << "\n";
        yamlfile << "    sep: ';'\n";
        yamlfile << "  input_b: {type: int, format: csv, path: " << input_b_csv_file << "}\n";
        yamlfile << "outputs:\n";
        yamlfile << "  output_c:\n";
        yamlfile << "    type: int\n";
        yamlfile << "    format: csv\n";
        yamlfile << "    path: " << output_c_csv_file << "\n";
        yamlfile << "    sep: ';'\n";
        yamlfile << "target:\n";
        yamlfile << "  library: " << TARGET_LIB << "\n";
        yamlfile << "  config: " << TARGET_CFG << "\n";
        yamlfile.close();

		WHEN("Instantiated") {

			auto iterator = bmit::Iterator::Create(config_file);

			THEN("Calling run produces expected output") {

                iterator->run();
                std::ifstream outfile(output_c_csv_file);
                std::string header, line1, line2, line3;
                std::getline(outfile, header);
                std::getline(outfile, line1);
                std::getline(outfile, line2);
                std::getline(outfile, line3);
                CHECK(line1 == "1010;1011");
                CHECK(line2 == "2020;2021");
                CHECK(line3 == "3030;3031");
                outfile.close();
			}
            bmit::Iterator::Dispose(iterator);
		}

    	/* Delete temporary file */
        remove(input_a_csv_file.c_str());
        remove(input_b_csv_file.c_str());
        remove(output_c_csv_file.c_str());
        remove(config_file.c_str());
	}
}
