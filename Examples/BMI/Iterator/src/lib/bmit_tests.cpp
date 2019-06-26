#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include "catch.hpp"
#include "sqlite3.h"
#include "bmit.h"

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


SCENARIO("Config validation") {

    GIVEN("A variable count mismatch (missing input)") {
        std::ofstream f;
        std::string const config_file = "_temporary_config.yml";
        f.open(config_file, std::ios::out);
        f << "inputs:\n";
        f << "  input_str: {type: str, format: csv, path: dummy.csv}\n";
        f << "outputs:\n";
        f << "  output_str: {type: str, format: csv, path: dummy.csv}\n";
        f << "  output_len: {type: int, format: csv, path: dummy.csv}\n";
        f << "target:\n";
        f << "  library: " << TARGET_LIB << "\n";
        f << "  config: " << TARGET_CFG << "\n";
        f.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "Variable count mismatch between iterator and target dll");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }

    GIVEN("A variable count mismatch (extra output)") {
        std::ofstream f;
        std::string const config_file = "_temporary_config.yml";
        f.open(config_file, std::ios::out);
        f << "inputs:\n";
        f << "  input_str: {type: str, format: csv, path: dummy.csv}\n";
        f << "  input_val: {type: double, format: csv, path: dummy.csv}\n";
        f << "outputs:\n";
        f << "  output_str: {type: str, format: csv, path: dummy.csv}\n";
        f << "  output_len: {type: int, format: csv, path: dummy.csv}\n";
        f << "  extra_outp: {type: int, format: csv, path: dummy.csv}\n";
        f << "target:\n";
        f << "  library: " << TARGET_LIB << "\n";
        f << "  config: " << TARGET_CFG << "\n";
        f.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "Variable count mismatch between iterator and target dll");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }

    GIVEN("A variable name mismatch") {
        std::ofstream f;
        std::string const config_file = "_temporary_config.yml";
        f.open(config_file, std::ios::out);
        f << "inputs:\n";
        f << "  input_str: {type: str, format: csv, path: dummy.csv}\n";
        f << "  ooooooops: {type: double, format: csv, path: dummy.csv}\n";
        f << "outputs:\n";
        f << "  output_str: {type: str, format: csv, path: dummy.csv}\n";
        f << "  output_len: {type: int, format: csv, path: dummy.csv}\n";
        f << "target:\n";
        f << "  library: " << TARGET_LIB << "\n";
        f << "  config: " << TARGET_CFG << "\n";
        f.close();

        THEN("Instantiation throws") {

            bmit::Iterator* it = nullptr;
            CHECK_THROWS_WITH(
                it = bmit::Iterator::Create(config_file),
                "Variable name mismatch between iterator and target dll");
            bmit::Iterator::Dispose(it);
        }

        /* Delete temporary file */
        remove(config_file.c_str());
    }
}


SCENARIO("Applying target component over CSV tables") {

	GIVEN("A target component and csv data") {
        std::string const input_str_csv_file = "_temporary_input_str.csv";
        std::string const input_val_csv_file = "_temporary_input_val.csv";
        std::string const output_str_csv_file = "_temporary_output_str.csv";
        std::string const output_len_csv_file = "_temporary_output_len.csv";

        std::ofstream csvfile;

        csvfile.open(input_str_csv_file, std::ios::out);
        csvfile << "A1;A2\n";
        csvfile << "aa-;ab-\n";
        csvfile << "ba-;bb-\n";
        csvfile << "ca-;cb-\n";
        csvfile.close();

        csvfile.open(input_val_csv_file, std::ios::out);
        csvfile << "B1,B2\n";
        csvfile << "1.1,1.2\n";
        csvfile << "2.1,2.2\n";
        csvfile << "3.1,3.2\n";
        csvfile.close();

        std::ofstream f;
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

		WHEN("Instantiated") {

			auto iterator = bmit::Iterator::Create(config_file);

			THEN("Calling run produces expected output") {

                iterator->run();

                std::ifstream ifs;
                std::string header, line1, line2, line3;

                // Check string output
                ifs.open(output_str_csv_file);
                std::getline(ifs, header);
                std::getline(ifs, line1);
                std::getline(ifs, line2);
                std::getline(ifs, line3);
                CHECK(line1 == "aa-1.1;ab-1.2");
                CHECK(line2 == "ba-2.1;bb-2.2");
                CHECK(line3 == "ca-3.1;cb-3.2");
                ifs.close();

                // Check integer output
                ifs.open(output_len_csv_file);
                std::getline(ifs, header);
                std::getline(ifs, line1);
                std::getline(ifs, line2);
                std::getline(ifs, line3);
                CHECK(line1 == "6;6");
                CHECK(line2 == "6;6");
                CHECK(line3 == "6;6");
                ifs.close();
			}
            bmit::Iterator::Dispose(iterator);
		}

    	/* Delete temporary files */
        remove(input_str_csv_file.c_str());
        remove(input_val_csv_file.c_str());
        remove(output_str_csv_file.c_str());
        remove(output_len_csv_file.c_str());
        remove(config_file.c_str());
	}
}


SCENARIO("Applying target component over SQL tables") {

    GIVEN("A target component and sql data") {
        std::string const test_db_file = "_temporary_file.db";
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        REQUIRE(sqlite3_open(test_db_file.c_str(), &db) == SQLITE_OK);

        // Input table A
        sql = "\
            CREATE TABLE input_str (\
                col_1 TEXT NOT NULL,\
                col_2 TEXT NOT NULL\
            );";
        REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
        REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_str (col_1, col_2) VALUES\
                ('aa-', 'ab-'),\
                ('ba-', 'bb-'),\
                ('ca-', 'cb-');";
        REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
        REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

        // Input table B
        sql = "\
            CREATE TABLE input_val (\
                col_1 FLOAT NOT NULL,\
                col_2 FLOAT NOT NULL\
            );";
        REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
        REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_val (col_1, col_2) VALUES\
                (1.1, 1.2),\
                (2.1, 2.2),\
                (3.1, 3.2);";
        REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
        REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

        REQUIRE(sqlite3_close(db) == SQLITE_OK);

        // Iterator config
        std::ofstream f;
        std::string const config_file = "_temporary_config.yml";
        f.open(config_file, std::ios::out);
        f << "inputs:\n";
        f << "  input_str: {type: str, format: sqlite, path: " << test_db_file << "}\n";
        f << "  input_val: {type: double, format: sqlite, path: " << test_db_file << "}\n";
        f << "outputs:\n";
        f << "  output_str: {type: str, format: sqlite, path: " << test_db_file << "}\n";
        f << "  output_len: {type: int, format: sqlite, path: " << test_db_file << "}\n";
        f << "target:\n";
        f << "  library: " << TARGET_LIB << "\n";
        f << "  config: " << TARGET_CFG << "\n";
        f.close();

        WHEN("Instantiated") {

            auto iterator = bmit::Iterator::Create(config_file);

            THEN("Calling run produces expected output") {

                iterator->run();

                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // Connect to db
                REQUIRE(sqlite3_open_v2(test_db_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);


                // Check output table for output_str
                sql = "SELECT * FROM output_str;";
                REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto computed_str = std::vector<std::vector<std::string>>
                    (3, std::vector<std::string> (2));
                for (int irow = 0; irow < 3; irow++) {
                    REQUIRE(sqlite3_step(qry) == SQLITE_ROW);
                    for (int icol = 0; icol < 2; icol++) {
                        auto c = sqlite3_column_text(qry, icol);
                        auto s = std::string(reinterpret_cast<const char*>(c));
                        computed_str[irow][icol] = s;
                    }
                }
                REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
                REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

                CHECK(computed_str[0] == std::vector<std::string> {"aa-1.1", "ab-1.2"});
                CHECK(computed_str[1] == std::vector<std::string> {"ba-2.1", "bb-2.2"});
                CHECK(computed_str[2] == std::vector<std::string> {"ca-3.1", "cb-3.2"});

                // Check output table for output_len
                sql = "SELECT * FROM output_len;";
                REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto computed_len = std::vector<std::vector<int>> (3, std::vector<int> (2));
                for (int irow = 0; irow < 3; irow++) {
                    REQUIRE(sqlite3_step(qry) == SQLITE_ROW);
                    for (int icol = 0; icol < 2; icol++) {
                        computed_len[irow][icol] = sqlite3_column_int(qry, icol);
                    }
                }
                REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
                REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

                CHECK(computed_len[0] == std::vector<int> {6, 6});
                CHECK(computed_len[1] == std::vector<int> {6, 6});
                CHECK(computed_len[2] == std::vector<int> {6, 6});
                // Close DB
                REQUIRE(sqlite3_close(db) == SQLITE_OK);
            }
            bmit::Iterator::Dispose(iterator);
        }

        /* Delete temporary files */
        REQUIRE( remove(test_db_file.c_str()) == 0 );
        REQUIRE( remove(config_file.c_str()) == 0);
    }
}
