#include <iostream>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "sqlite3.h"
#include "table.h"

// Simple temporary file, deleted when out of scope
class TempFile {
public:
    TempFile(const std::string& path) : m_path(path){};
    ~TempFile() {remove(m_path.c_str());};
    std::string& path() {return m_path;};
    const char* c_str() {return m_path.c_str();}
private:
    std::string m_path;
};


SCENARIO("Reading a CSV table") {

    GIVEN("A CSV table with int data") {
        std::ofstream csvfile;
        auto test_file = TempFile("_temporary_file.csv");
        csvfile.open(test_file.path(), std::ios::out);
        csvfile << "Col_1;Col_2;Col_3\n";
        csvfile << "11;12;13\n";
        csvfile << "21;22;23\n";
        csvfile.close();

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto t = bmit::CsvTable<int>(name, path, ';');

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.nb_cols() == 3);
                CHECK(t.nb_rows() == 2);
            }

            THEN("Data can be loaded and accessed")
            {
                t.load();
                const int* ptr;
                auto vals = std::vector<int> (6);
                ptr = (const int*) t.get_cell(0,0); vals[0] = *ptr;
                ptr = (const int*) t.get_cell(0,1); vals[1] = *ptr;
                ptr = (const int*) t.get_cell(0,2); vals[2] = *ptr;
                ptr = (const int*) t.get_cell(1,0); vals[3] = *ptr;
                ptr = (const int*) t.get_cell(1,1); vals[4] = *ptr;
                ptr = (const int*) t.get_cell(1,2); vals[5] = *ptr;
                CHECK(vals == std::vector<int> {11,12,13,21,22,23});
            }
        }
    }

    GIVEN("A CSV table with double data") {
        std::ofstream csvfile;
        auto test_file = TempFile("_temporary_file.csv");
        csvfile.open(test_file.path(), std::ios::out);
        csvfile << "Col_1;Col_2;Col_3\n";
        csvfile << "1.1;1.2;1.3\n";
        csvfile << "2.1;2.2;2.3\n";
        csvfile.close();

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto t = bmit::CsvTable<double>(name, path, ';');

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.nb_cols() == 3);
                CHECK(t.nb_rows() == 2);
            }

            THEN("Data can be loaded and accessed")
            {
                t.load();
                double* ptr;
                auto vals = std::vector<double> (6);
                ptr = (double*) t.get_cell(0,0); vals[0] = *ptr;
                ptr = (double*) t.get_cell(0,1); vals[1] = *ptr;
                ptr = (double*) t.get_cell(0,2); vals[2] = *ptr;
                ptr = (double*) t.get_cell(1,0); vals[3] = *ptr;
                ptr = (double*) t.get_cell(1,1); vals[4] = *ptr;
                ptr = (double*) t.get_cell(1,2); vals[5] = *ptr;
                CHECK(vals == std::vector<double> {1.1,1.2,1.3,2.1,2.2,2.3});
            }
        }
    }

    GIVEN("A CSV table with string data") {
        std::ofstream csvfile;
        auto test_file = TempFile("_temporary_file.csv");
        csvfile.open(test_file.path(), std::ios::out);
        csvfile << "Col_1;Col_2;Col_3\n";
        csvfile << "One;Two;Three\n";
        csvfile << "Four;Five;Six\n";
        csvfile.close();

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto t = bmit::CsvTable<std::string>(name, path, ';');

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.nb_cols() == 3);
                CHECK(t.nb_rows() == 2);
            }

            THEN("Data can be loaded and accessed")
            {
                t.load();
                char* ptr;
                auto vals = std::vector<std::string> (6);
                ptr = (char*) t.get_cell(0,0); vals[0] = std::string {ptr};
                ptr = (char*) t.get_cell(0,1); vals[1] = std::string {ptr};
                ptr = (char*) t.get_cell(0,2); vals[2] = std::string {ptr};
                ptr = (char*) t.get_cell(1,0); vals[3] = std::string {ptr};
                ptr = (char*) t.get_cell(1,1); vals[4] = std::string {ptr};
                ptr = (char*) t.get_cell(1,2); vals[5] = std::string {ptr};
                auto expected = std::vector<std::string> {
                    "One","Two","Three",
                    "Four","Five","Six"};
                CHECK(vals == expected);
            }
        }
    }
}


SCENARIO("Writing a CSV table") {

    GIVEN("A CsvTable and int data") {

        const int rows = 2;
        const int cols = 3;
        auto row1 = std::vector<int> {11, 12, 13};
        auto row2 = std::vector<int> {21, 22, 23};
        auto name = "output_name";
        auto test_file = TempFile("_temporary_file.csv");
        auto t = bmit::CsvTable<int>(name, test_file.path(), rows, cols, ';');

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == cols);
        }

        WHEN("Setting table values") {

            /* Set first row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row1[icol]);
                t.set_cell(0, icol, (void*) ptr);
            }
            /* Set second row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row2[icol]);
                t.set_cell(1, icol, (void*) ptr);
            }

            THEN("Table data can be written to file")
            {
                t.write();

                std::ifstream outfile(test_file.path());
                std::string header, line1, line2;
                std::getline(outfile, header);
                std::getline(outfile, line1);
                std::getline(outfile, line2);
                CHECK(line1 == "11;12;13");
                CHECK(line2 == "21;22;23");
                outfile.close();
            }
        }
    }

    GIVEN("A CsvTable and double data") {

        const int rows = 2;
        const int cols = 3;
        auto row1 = std::vector<double> {1.1, 1.2, 1.3};
        auto row2 = std::vector<double> {2.1, 2.2, 2.3};
        auto name = "output_name";
        auto test_file = TempFile("_temporary_file.csv");
        auto t = bmit::CsvTable<double>(name, test_file.path(), rows, cols, ';');

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == cols);
        }

        WHEN("Setting table values") {

            /* Set first row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row1[icol]);
                t.set_cell(0, icol, (void*) ptr);
            }
            /* Set second row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row2[icol]);
                t.set_cell(1, icol, (void*) ptr);
            }

            THEN("Table data can be written to file")
            {
                t.write();

                std::ifstream outfile(test_file.path());
                std::string header, line1, line2;
                std::getline(outfile, header);
                std::getline(outfile, line1);
                std::getline(outfile, line2);
                CHECK(line1 == "1.1;1.2;1.3");
                CHECK(line2 == "2.1;2.2;2.3");
                outfile.close();
            }
        }
    }

    GIVEN("A CsvTable and string data") {

        const int rows = 2;
        const int cols = 3;
        auto row1 = std::vector<std::string> {"one", "two", "three"};
        auto row2 = std::vector<std::string> {"four", "five", "six"};
        auto name = "output_name";
        auto test_file = TempFile("_temporary_file.csv");
        auto t = bmit::CsvTable<std::string>(name, test_file.path(), rows, cols, ';');

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == cols);
        }

        WHEN("Setting table values") {

            /* Set first row values */
            for(int icol = 0; icol < cols; icol++) {
                const char* ptr = row1[icol].c_str();
                t.set_cell(0, icol, (const void*) ptr);
            }
            /* Set second row values */
            for(int icol = 0; icol < cols; icol++) {
                const char* ptr = row2[icol].c_str();
                t.set_cell(1, icol, (const void*) ptr);
            }

            THEN("Table data can be written to file")
            {
                t.write();

                std::ifstream outfile(test_file.path());
                std::string header, line1, line2;
                std::getline(outfile, header);
                std::getline(outfile, line1);
                std::getline(outfile, line2);
                CHECK(line1 == "one;two;three");
                CHECK(line2 == "four;five;six");
                outfile.close();
            }
        }
    }
}


SCENARIO("Reading a SQL table") {

    GIVEN("A SQL table with integer data") {

        auto test_file = TempFile("_temporary_file.db");
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);

        sql = "\
            CREATE TABLE input_a_tbl (\
                col_1 INTEGER NOT NULL,\
                col_2 INTEGER NOT NULL,\
                col_3 INTEGER NOT NULL\
            );";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_a_tbl (col_1, col_2, col_3) VALUES\
                (11, 12, 13),\
                (21, 22, 23);";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(sqlite3_close(db) == SQLITE_OK);

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto table = "input_a_tbl";
            auto t = bmit::SqlTable<int>(name, path, table);

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.nb_cols() == 3);
                CHECK(t.nb_rows() == 2);
            }

            AND_THEN("Data can be accessed")
            {
                t.load();
                int* valp = nullptr;
                valp = (int*) t.get_cell(0, 0); CHECK(*valp == 11);
                valp = (int*) t.get_cell(0, 1); CHECK(*valp == 12);
                valp = (int*) t.get_cell(0, 2); CHECK(*valp == 13);
                valp = (int*) t.get_cell(1, 0); CHECK(*valp == 21);
                valp = (int*) t.get_cell(1, 1); CHECK(*valp == 22);
                valp = (int*) t.get_cell(1, 2); CHECK(*valp == 23);
            }
        }
    }

    GIVEN("A SQL table with double data") {

        auto test_file = TempFile("_temporary_file.db");
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);

        sql = "\
            CREATE TABLE input_a_tbl (\
                col_1 double NOT NULL,\
                col_2 double NOT NULL,\
                col_3 double NOT NULL\
            );";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_a_tbl (col_1, col_2, col_3) VALUES\
                (1.1, 1.2, 1.3),\
                (2.1, 2.2, 2.3);";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(sqlite3_close(db) == SQLITE_OK);

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto table = "input_a_tbl";
            auto t = bmit::SqlTable<double>(name, path, table);

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.nb_cols() == 3);
                CHECK(t.nb_rows() == 2);
            }

            AND_THEN("Data can be accessed")
            {
                t.load();
                double* valp = nullptr;
                valp = (double*) t.get_cell(0, 0); CHECK(*valp == 1.1);
                valp = (double*) t.get_cell(0, 1); CHECK(*valp == 1.2);
                valp = (double*) t.get_cell(0, 2); CHECK(*valp == 1.3);
                valp = (double*) t.get_cell(1, 0); CHECK(*valp == 2.1);
                valp = (double*) t.get_cell(1, 1); CHECK(*valp == 2.2);
                valp = (double*) t.get_cell(1, 2); CHECK(*valp == 2.3);
            }
        }
    }

    GIVEN("A SQL table with str data") {

        auto test_file = TempFile("_temporary_file.db");
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);

        sql = "\
            CREATE TABLE input_a_tbl (\
                col_1 text NOT NULL,\
                col_2 text NOT NULL,\
                col_3 text NOT NULL\
            );";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_a_tbl (col_1, col_2, col_3) VALUES\
                ('one', 'two', 'three'),\
                ('four', 'five', 'six');";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(sqlite3_close(db) == SQLITE_OK);

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto table = "input_a_tbl";
            auto t = bmit::SqlTable<std::string>(name, path, table);

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.nb_cols() == 3);
                CHECK(t.nb_rows() == 2);
            }

            AND_THEN("Data can be accessed")
            {
                t.load();
                char* ptr = nullptr;
                ptr = (char*) t.get_cell(0, 0); CHECK(std::string {ptr} == "one");
                ptr = (char*) t.get_cell(0, 1); CHECK(std::string {ptr} == "two");
                ptr = (char*) t.get_cell(0, 2); CHECK(std::string {ptr} == "three");
                ptr = (char*) t.get_cell(1, 0); CHECK(std::string {ptr} == "four");
                ptr = (char*) t.get_cell(1, 1); CHECK(std::string {ptr} == "five");
                ptr = (char*) t.get_cell(1, 2); CHECK(std::string {ptr} == "six");
            }
        }
    }
}


SCENARIO("Writing a SQL table") {

    GIVEN("A SqlTable and int data") {

        const int rows = 2;
        const int cols = 3;
        auto row1 = std::vector<int> {11, 12, 13};
        auto row2 = std::vector<int> {21, 22, 23};
        auto name = std::string("output_name");
        auto table = std::string("tbl_name");
        auto test_file = TempFile("_temporary_file.db");
        auto t = bmit::SqlTable<int>(name, test_file.path(), table, rows, cols);

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == cols);
        }

        WHEN("Setting table values") {

            /* Set first row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row1[icol]);
                t.set_cell(0, icol, ptr);
            }
            /* Set second row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row2[icol]);
                t.set_cell(1, icol, ptr);
            }

            THEN("Table data can be written to file")
            {
                t.write();

                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT * FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                std::vector<int> row1 (3);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                for (int i = 0; i < cols; i++) {
                    row1[i] = sqlite3_column_int(qry, i);
                }

                std::vector<int> row2 (3);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                for (int i = 0; i < cols; i++) {
                    row2[i] = sqlite3_column_int(qry, i);
                }

                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(row1 == std::vector<int> {11, 12, 13});
                CHECK(row2 == std::vector<int> {21, 22, 23});
            }
        }
    }

    GIVEN("A SqlTable and double data") {

        const int rows = 2;
        const int cols = 3;
        auto row1 = std::vector<double> {1.1, 1.2, 1.3};
        auto row2 = std::vector<double> {2.1, 2.2, 2.3};
        auto name = std::string("output_name");
        auto table = std::string("tbl_name");
        auto test_file = TempFile("_temporary_file.db");
        auto t = bmit::SqlTable<double>(name, test_file.path(), table, rows, cols);

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == cols);
        }

        WHEN("Setting table values") {

            /* Set first row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row1[icol]);
                t.set_cell(0, icol, ptr);
            }
            /* Set second row values */
            for(int icol = 0; icol < cols; icol++) {
                auto ptr = &(row2[icol]);
                t.set_cell(1, icol, ptr);
            }

            THEN("Table data can be written to file")
            {
                t.write();

                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT * FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                std::vector<double> row1 (3);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                for (int i = 0; i < cols; i++) {
                    row1[i] = sqlite3_column_double(qry, i);
                }

                std::vector<double> row2 (3);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                for (int i = 0; i < cols; i++) {
                    row2[i] = sqlite3_column_double(qry, i);
                }

                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(row1 == std::vector<double> {1.1, 1.2, 1.3});
                CHECK(row2 == std::vector<double> {2.1, 2.2, 2.3});
            }
        }
    }

    GIVEN("A SqlTable and string data") {

        const int rows = 2;
        const int cols = 3;
        auto row1 = std::vector<std::string> {"one", "two", "three"};
        auto row2 = std::vector<std::string> {"four", "five", "six"};
        auto name = std::string("output_name");
        auto table = std::string("tbl_name");
        auto test_file = TempFile("_temporary_file.db");
        auto t = bmit::SqlTable<std::string>(name, test_file.path(), table, rows, cols);

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == cols);
        }

        WHEN("Setting table values") {

            /* Set first row values */
            for(int icol = 0; icol < cols; icol++) {
                const char* ptr = row1[icol].c_str();
                t.set_cell(0, icol, (const void*) ptr);
            }
            /* Set second row values */
            for(int icol = 0; icol < cols; icol++) {
                const char* ptr = row2[icol].c_str();
                t.set_cell(1, icol, (const void*) ptr);
            }

            THEN("Table data can be written to file")
            {
                t.write();

                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT * FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                std::vector<std::string> row1 (3);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                for (int i = 0; i < cols; i++) {
                    auto c = sqlite3_column_text(qry, i);
                    row1[i] = std::string(reinterpret_cast<const char*>(c));
                }

                std::vector<std::string> row2 (3);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                for (int i = 0; i < cols; i++) {
                    auto c = sqlite3_column_text(qry, i);
                    row2[i] = std::string(reinterpret_cast<const char*>(c));
                }

                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(row1 == std::vector<std::string> {"one", "two", "three"});
                CHECK(row2 == std::vector<std::string> {"four", "five", "six"});
            }
        }
    }
}


SCENARIO("Reading a SQL column") {

    GIVEN("A SQL column with integer data and default primary key") {

        auto test_file = TempFile("_temporary_file.db");
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);

        sql = "\
            CREATE TABLE input_a_tbl (\
                id    INTEGER PRIMARY KEY,\
                col_1 INTEGER NOT NULL,\
                col_2 INTEGER NOT NULL,\
                col_3 INTEGER NOT NULL\
            );";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_a_tbl (id, col_1, col_2, col_3) VALUES\
                (1, 11, 12, 13),\
                (2, 21, 22, 23);";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(sqlite3_close(db) == SQLITE_OK);

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto table = "input_a_tbl";
            auto column = "col_2";
            auto t = bmit::SqlColumn<int>(name, path, table, column);

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.column() == column);
                CHECK(t.pk_name() == "id");
                CHECK(t.nb_cols() == 1);
                CHECK(t.nb_rows() == 2);
            }

            AND_THEN("Data can be accessed")
            {
                t.load();
                int* valp = nullptr;
                valp = (int*) t.get_cell(0, 0); CHECK(*valp == 12);
                valp = (int*) t.get_cell(1, 0); CHECK(*valp == 22);
            }
        }
    }

    GIVEN("A SQL column with double data and custom primary key") {

        auto test_file = TempFile("_temporary_file.db");
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);

        sql = "\
            CREATE TABLE input_a_tbl (\
                pk_id INTEGER PRIMARY KEY,\
                col_1 DOUBLE NOT NULL,\
                col_2 DOUBLE NOT NULL,\
                col_3 DOUBLE NOT NULL\
            );";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_a_tbl (pk_id, col_1, col_2, col_3) VALUES\
                (1, 1.1, 1.2, 1.3),\
                (2, 2.1, 2.2, 2.3);";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(sqlite3_close(db) == SQLITE_OK);

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto table = "input_a_tbl";
            auto column = "col_2";
            auto pk_name = "pk_id";
            auto t = bmit::SqlColumn<double>(name, path, table, column, pk_name);

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.column() == column);
                CHECK(t.pk_name() == pk_name);
                CHECK(t.nb_cols() == 1);
                CHECK(t.nb_rows() == 2);
            }

            AND_THEN("Data can be accessed")
            {
                t.load();
                double* valp = nullptr;
                valp = (double*) t.get_cell(0, 0); CHECK(*valp == 1.2);
                valp = (double*) t.get_cell(1, 0); CHECK(*valp == 2.2);
            }
        }
    }

    GIVEN("A SQL column with str data and no primary key") {

        auto test_file = TempFile("_temporary_file.db");
        std::string sql;
        sqlite3* db = nullptr;
        sqlite3_stmt* qry = nullptr;
        const char* qry_tail = nullptr;

        CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);

        sql = "\
            CREATE TABLE input_a_tbl (\
                col_1 TEXT NOT NULL,\
                col_2 TEXT NOT NULL,\
                col_3 TEXT NOT NULL\
            );";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        sql = "\
            INSERT INTO input_a_tbl (col_1, col_2, col_3) VALUES\
                ('one', 'two', 'three'),\
                ('four', 'five', 'six');";
        CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
        CHECK(sqlite3_step(qry) == SQLITE_DONE);
        CHECK(sqlite3_finalize(qry) == SQLITE_OK);

        CHECK(sqlite3_close(db) == SQLITE_OK);

        WHEN("Instantiating from file") {
            auto name = "input_a";
            auto path = test_file.path();
            auto table = "input_a_tbl";
            auto column = "col_2";
            auto t = bmit::SqlColumn<std::string>(name, path, table, column);

            THEN("Properties are set") {
                CHECK(t.name() == name);
                CHECK(t.path() == path);
                CHECK(t.column() == column);
                CHECK(t.nb_cols() == 1);
                CHECK(t.nb_rows() == 2);
            }

            AND_THEN("Data can be accessed")
            {
                t.load();
                char* ptr = nullptr;
                ptr = (char*) t.get_cell(0, 0); CHECK(std::string(ptr) == "two");
                ptr = (char*) t.get_cell(1, 0); CHECK(std::string(ptr) == "five");
            }
        }
    }
}


SCENARIO("Writing a SQL column") {

    GIVEN("A SqlColumn and int data with default primary key") {

        const int rows = 3;
        auto values = std::vector<int> {1, 2, 3};
        auto name = std::string("var_name");
        auto table = std::string("table_name");
        auto column = std::string("col_name") ;
        auto test_file = TempFile("_temporary_file.db");
        auto t = bmit::SqlColumn<int>(name, test_file.path(), table, column, rows);

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.column() == column);
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == 1);
        }

        WHEN("Setting table values") {

            int* ptr;
            ptr = &(values[0]); t.set_cell(0, 0, (const void*) ptr);
            ptr = &(values[1]); t.set_cell(1, 0, (const void*) ptr);
            ptr = &(values[2]); t.set_cell(2, 0, (const void*) ptr);

            THEN("Column data can be written as a new table")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // Call write
                t.write();

                // Check table was indeed created
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<int> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    vals[i] = sqlite3_column_int(qry, 0);
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);

                // Check table has a PK column with default name
                sql = "SELECT COUNT(id) FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                CHECK(sqlite3_column_int(qry, 0) == 3);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);

                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<int> {1, 2, 3});
            }

            THEN("Column data can be written to an existing table") {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // First create the destination table
                CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);
                sql = "DROP TABLE IF EXISTS table_name";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "\
                    CREATE TABLE table_name (\
                        id INTEGER PRIMARY KEY,\
                        other INTEGER NOT NULL\
                    );";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                qry = nullptr;
                qry_tail = nullptr;
                sql = "INSERT INTO table_name (other) VALUES (10), (30), (20);";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                // Call write
                t.write();

                // Check column was indeed created
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<int> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    vals[i] = sqlite3_column_int(qry, 0);
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<int> {1, 2, 3});
            }

            THEN("Column data can be written to an existing column") {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // First create the destination table
                CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);
                sql = "DROP TABLE IF EXISTS table_name";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "\
                    CREATE TABLE table_name (\
                        id INTEGER PRIMARY KEY,\
                        other INTEGER NOT NULL,\
                        col_name INTEGER\
                    );";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                qry = nullptr;
                qry_tail = nullptr;
                sql = "INSERT INTO table_name (other, col_name) VALUES (10, -1), (30, -2), (20, -3);";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                // Call write
                t.write();

                // Check column was indeed updated
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<int> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    vals[i] = sqlite3_column_int(qry, 0);
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<int> {1, 2, 3});
            }
        }
    }

    GIVEN("A SqlColumn and double data with custom primary key") {

        const int rows = 3;
        auto values = std::vector<double> {1.1, 2.2, 3.3};
        auto name = std::string("var_name");
        auto table = std::string("table_name");
        auto column = std::string("col_name") ;
        auto pk_name = std::string("pk_id") ;
        auto test_file = TempFile("_temporary_file.db");
        auto t = bmit::SqlColumn<double>(name, test_file.path(), table, column, rows, pk_name);

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.column() == column);
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == 1);
        }

        WHEN("Setting table values") {

            double* ptr;
            ptr = &(values[0]); t.set_cell(0, 0, (void*) ptr);
            ptr = &(values[1]); t.set_cell(1, 0, (void*) ptr);
            ptr = &(values[2]); t.set_cell(2, 0, (void*) ptr);

            THEN("Column data can be written as a new table")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // Call write
                t.write();

                // Check table was indeed created
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<double> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    vals[i] = sqlite3_column_double(qry, 0);
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);

                // Check table has a PK column with default name
                sql = "SELECT COUNT(" + pk_name + ") FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                CHECK(sqlite3_column_int(qry, 0) == 3);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);

                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<double> {1.1, 2.2, 3.3});
            }

            THEN("Column data can be written to an existing table")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // First create the destination table
                CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);
                sql = "DROP TABLE IF EXISTS table_name";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "\
                    CREATE TABLE table_name (\
                        pk_id INTEGER PRIMARY KEY,\
                        other INTEGER NOT NULL\
                    );";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "INSERT INTO table_name (other) VALUES (10), (30), (20);";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                // Call write
                t.write();

                // Check column was indeed created
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<double> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    vals[i] = sqlite3_column_double(qry, 0);
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<double> {1.1, 2.2, 3.3});
            }

            THEN("Column data can be written to an existing column")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // First create the destination table
                CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);
                sql = "DROP TABLE IF EXISTS table_name";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "\
                    CREATE TABLE table_name (\
                        pk_id INTEGER PRIMARY KEY,\
                        other INTEGER NOT NULL,\
                        col_name FLOAT\
                    );";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "INSERT INTO table_name (other, col_name) VALUES (10, -1.0), (30, -2.0), (20, -3.0);";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                // Call write
                t.write();

                // Check column was indeed updated
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<double> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    vals[i] = sqlite3_column_double(qry, 0);
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<double> {1.1, 2.2, 3.3});
            }
        }
    }

    GIVEN("A SqlColumn and str data with default primary key") {

        const int rows = 3;
        auto values = std::vector<std::string> {"one","two","three"};
        auto name = std::string("var_name");
        auto table = std::string("table_name");
        auto column = std::string("col_name") ;
        auto test_file = TempFile("_temporary_file.db");
        auto t = bmit::SqlColumn<std::string>(name, test_file.path(), table, column, rows);

        THEN("Properties are set") {

            CHECK(t.name() == name);
            CHECK(t.path() == test_file.path());
            CHECK(t.column() == column);
            CHECK(t.nb_rows() == rows);
            CHECK(t.nb_cols() == 1);
        }

        WHEN("Setting table values") {

            const char* ptr;
            ptr = values[0].c_str(); t.set_cell(0, 0, (const void*) ptr);
            ptr = values[1].c_str(); t.set_cell(1, 0, (const void*) ptr);
            ptr = values[2].c_str(); t.set_cell(2, 0, (const void*) ptr);

            THEN("Column data can be written as a new table")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // Call write
                t.write();

                // Check table was indeed created
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<std::string> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    auto c = sqlite3_column_text(qry, 0);
                    vals[i] = std::string(reinterpret_cast<const char*>(c));
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);

                // Check table has a PK column with default name
                sql = "SELECT COUNT(id) FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_ROW);
                CHECK(sqlite3_column_int(qry, 0) == 3);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);

                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<std::string> {"one","two","three"});
            }

            THEN("Column data can be written to an existing table")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // First create the destination table
                CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);
                sql = "DROP TABLE IF EXISTS table_name";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "\
                    CREATE TABLE table_name (\
                        id INTEGER PRIMARY KEY,\
                        other INTEGER NOT NULL\
                    );";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "INSERT INTO table_name (other) VALUES (10), (30), (20);";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                // Call write
                t.write();

                // Check column was indeed created
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<std::string> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    auto c = sqlite3_column_text(qry, 0);
                    vals[i] = std::string(reinterpret_cast<const char*>(c));
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<std::string> {"one","two","three"});
            }

            THEN("Column data can be written to an existing column")
            {
                std::string sql;
                sqlite3* db = nullptr;
                sqlite3_stmt* qry = nullptr;
                const char* qry_tail = nullptr;

                // First create the destination table
                CHECK(sqlite3_open(test_file.c_str(), &db) == SQLITE_OK);
                sql = "DROP TABLE IF EXISTS table_name";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "\
                    CREATE TABLE table_name (\
                        id INTEGER PRIMARY KEY,\
                        other INTEGER NOT NULL,\
                        col_name TEXT\
                    );";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                sql = "INSERT INTO table_name (other, col_name) VALUES (10, 'X'), (30, 'Y'), (20, 'Z');";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                // Call write
                t.write();

                // Check column was indeed updated
                CHECK(sqlite3_open_v2(test_file.c_str(), &db, SQLITE_OPEN_READONLY, nullptr) == SQLITE_OK);
                sql = "SELECT " + column + " FROM " + table + ";";
                CHECK(sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail) == SQLITE_OK);

                auto vals = std::vector<std::string> (rows);
                for (int i = 0; i < rows; i++) {
                    CHECK(sqlite3_step(qry) == SQLITE_ROW);
                    auto c = sqlite3_column_text(qry, 0);
                    vals[i] = std::string(reinterpret_cast<const char*>(c));
                }
                CHECK(sqlite3_step(qry) == SQLITE_DONE);
                CHECK(sqlite3_finalize(qry) == SQLITE_OK);
                CHECK(sqlite3_close(db) == SQLITE_OK);

                CHECK(vals == std::vector<std::string> {"one","two","three"});
            }
        }
    }
}
