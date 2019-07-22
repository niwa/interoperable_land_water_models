#include <cstring>
#include <iostream>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "bmi.h"
#include "sqlite3.h"

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


static const Level log_level = LEVEL_WARNING;

void log(Level level, const char* msg) {
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


TEST_CASE("Running the iterator on SQL columns") {
    // This test case runs the bmi_concat opint model
    // with all input and output column in a single table
    // with a custom primary key name (`pk_id` instead of default `id`).

    std::string const test_db_file = "_temporary_file.db";
    std::string sql;
    sqlite3* db = nullptr;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;

    // Create temporary sqlite database
    REQUIRE(sqlite3_open(test_db_file.c_str(), &db) == SQLITE_OK);

    // Prepare input columns with primary id
    sql = "\
        CREATE TABLE test_table (\
            pk_id INTEGER PRIMARY KEY,\
            input_str_col TEXT NOT NULL,\
            input_val_col FLOAT NOT NULL\
        );";
    REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
    REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
    REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

    // Populate pk and input columns
    // PK not ordered on purpose
    sql = "\
        INSERT INTO test_table (pk_id, input_str_col, input_val_col) VALUES\
            (1, 'rowa-', 1.1),\
            (3, 'rowb-', 1.2),\
            (2, 'rowc-', 1.3);";
    REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
    REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
    REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

    REQUIRE(sqlite3_close(db) == SQLITE_OK);

    // Create config file for iterator
    std::ofstream f;
    std::string const config_file = "_temporary_config.yml";
    f.open(config_file, std::ios::out);
    f << "inputs:\n";
    f << "  input_str:\n";
    f << "    type: str\n";
    f << "    format: sqlite\n";
    f << "    path: " << test_db_file << "\n";
    f << "    table: test_table\n";
    f << "    column: input_str_col\n";
    f << "  input_val:\n";
    f << "    type: double\n";
    f << "    format: sqlite\n";
    f << "    path: " << test_db_file << "\n";
    f << "    table: test_table\n";
    f << "    column: input_val_col\n";
    f << "outputs:\n";
    f << "  output_str:\n";
    f << "    type: str\n";
    f << "    format: sqlite\n";
    f << "    path: " << test_db_file << "\n";
    f << "    table: test_table\n";
    f << "    column: output_str_col\n";
    f << "    pk_name: pk_id\n";
    f << "  output_len:\n";
    f << "    type: int\n";
    f << "    format: sqlite\n";
    f << "    path: " << test_db_file << "\n";
    f << "    table: test_table\n";
    f << "    column: output_len_col\n";
    f << "    pk_name: pk_id\n";
    f << "target:\n";
    f << "  library: " << TARGET_LIB << "\n";
    f << "  config: " << TARGET_CFG << "\n";
    f.close();

    REQUIRE(initialize(config_file.c_str()) == 0);

    SECTION("Outputs are written to database after calling update") {
        REQUIRE(update(0) == 0);

        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        // Connect to db
        REQUIRE(sqlite3_open_v2(test_db_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);

        // Check outputs for output_str
        sql = "SELECT output_str_col, pk_id FROM test_table ORDER BY pk_id;";
        REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

        auto computed_str = std::vector<std::string> (3);
        for (int irow = 0; irow < 3; irow++) {
            REQUIRE(sqlite3_step(qry) == SQLITE_ROW);
            auto c = sqlite3_column_text(qry, 0);
            auto s = std::string(reinterpret_cast<const char*>(c));
            computed_str[irow] = s;
        }
        REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
        REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(computed_str[0] == "rowa-1.1");
        CHECK(computed_str[1] == "rowc-1.3");
        CHECK(computed_str[2] == "rowb-1.2");

        // Check outputs for output_len
        sql = "SELECT output_len_col, pk_id FROM test_table ORDER BY pk_id;";
        REQUIRE(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

        auto computed_len = std::vector<int> (3);
        for (int irow = 0; irow < 3; irow++) {
            REQUIRE(sqlite3_step(qry) == SQLITE_ROW);
            computed_len[irow] = sqlite3_column_int(qry, 0);
        }
        REQUIRE(sqlite3_step(qry) == SQLITE_DONE);
        REQUIRE(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(computed_len[0] == 8);
        CHECK(computed_len[1] == 8);
        CHECK(computed_len[2] == 8);
        // Close DB
        REQUIRE(sqlite3_close(db) == SQLITE_OK);

    }

    REQUIRE(finalize() == 0);

    // Delete temporary files
    REQUIRE( remove(test_db_file.c_str()) == 0 );
    REQUIRE( remove(config_file.c_str()) == 0);
}
