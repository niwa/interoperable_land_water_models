#include <algorithm>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "table.h"

static int csv_table_row_count(const std::string& path, const char sep);
static int csv_table_col_count(const std::string& path, const char sep);

static int sql_table_row_count(sqlite3* db, const std::string& table);
static int sql_table_col_count(sqlite3* db, const std::string& table);
static int sql_column_row_count(sqlite3* db, const std::string& table,
                                             const std::string& column);

// CSV Table readonly ctor
template <class T>
bmit::CsvTable<T>::CsvTable(const std::string& name,
                            const std::string& path,
                            const char sep) {
    m_name = name;
    m_path = path;
    m_sep = sep;
    m_nb_rows = csv_table_row_count(path, sep);
    m_nb_cols = csv_table_col_count(path, sep);
}


// CSV Table read/write ctor
template <class T>
bmit::CsvTable<T>::CsvTable(const std::string& name,
                            const std::string& path,
                            const int rows,
                            const int cols,
                            const char sep) {
    m_name = name;
    m_path = path;
    m_sep = sep;
    if (rows == 0 || cols == 0) throw "Writable CsvTable dimensions cannot be 0";
    m_nb_rows = rows;
    m_nb_cols = cols;
    m_readonly = false;

    /* Initialize table data with default values */
    m_data = std::vector<T> (rows * cols);
}


template <class T>
void bmit::CsvTable<T>::load() {
    // Clear existing data.
    // To avoid blowing up table in case of successive calls (not that this should happen)
    m_data.clear();

    // We'll load all the table contents, so we might as well pre-allocate
    m_data.reserve(m_nb_rows * m_nb_cols);

    std::ifstream csvfile(m_path);
    std::string line;

    // Assuming header always present, so discarding first line.
    std::getline(csvfile, line);
    while (std::getline(csvfile, line)) {
        auto tokens = split_csv_line(line);
        for (const auto& token : tokens) {
            T value;
            std::stringstream{token} >> value;
            m_data.push_back(value);
        }
    }

    csvfile.close();
}


template <class T>
void bmit::CsvTable<T>::write() {
    if (m_readonly) throw "Trying to write to readonly CsvTable";

    // Generate header
    auto header = std::stringstream();
    header << "Col_1";
    for (int icol = 1; icol < m_nb_cols; icol++) {
        header << m_sep << "Col_" << icol+1;
    }
    header << "\n";

    // Write header
    std::ofstream csvfile;
    csvfile.open(m_path, std::ios::out);
    csvfile << header.str();

    // Write rows
    int index = 0;
    for (int irow = 0; irow < m_nb_rows; irow++) {
        // First column
        csvfile << m_data[index++];
        // Remaining columns
        for (int icol = 1; icol < m_nb_cols; icol++) {
            csvfile << m_sep << m_data[index++];
        }
        csvfile << "\n";
    }

    csvfile.close();
}


template <class T>
std::vector<std::string>
bmit::CsvTable<T>::split_csv_line(std::string& line) {
    auto tokens = std::vector<std::string> {};
    tokens.reserve(m_nb_cols);
    auto ss = std::stringstream(line);
    while (ss.good()) {
        std::string substr;
        getline(ss, substr, m_sep);
        tokens.push_back(substr);
    }
    return tokens;
}


template <class T>
bmit::SqlTable<T>::SqlTable(const std::string& name, const std::string& path) {
    m_name = name;
    m_path = path;

    auto ret = sqlite3_open_v2(m_path.c_str(), &m_db, SQLITE_OPEN_READONLY, nullptr);
    if (ret != SQLITE_OK) throw "Failed opening database";

    /* Get row and col count */
    m_nb_rows = sql_table_row_count(m_db, m_name);
    m_nb_cols = sql_table_col_count(m_db, m_name);
}

template <class T>
bmit::SqlTable<T>::SqlTable(const std::string& name, sqlite3* db) {
    m_name = name;
    m_db = db;

    /* Get row and col count */
    m_nb_rows = sql_table_row_count(m_db, m_name);
    m_nb_cols = sql_table_col_count(m_db, m_name);
}

template <class T>
bmit::SqlTable<T>::SqlTable(const std::string& name, const std::string& path, const int rows, const int cols) {
    m_readonly = false;
    m_name = name;
    m_path = path;
    m_nb_rows = rows;
    m_nb_cols = cols;

    auto ret = sqlite3_open(m_path.c_str(), &m_db); // Read/Write/Create
    if (ret != SQLITE_OK) throw "Failed opening database";

    /* Initialize table data with default values */
    m_data = std::vector<T> (rows * cols);
}

template <class T>
bmit::SqlTable<T>::SqlTable(const std::string& name, sqlite3* db, const int rows, const int cols) {
    m_readonly = false;
    m_name = name;
    m_db = db;
    m_nb_rows = rows;
    m_nb_cols = cols;

    /* Initialize table data with default values */
    m_data = std::vector<T> (rows * cols);
}

template <class T>
void bmit::SqlTable<T>::load() {
    // Clear existing data.
    // To avoid blowing up table in case of successive calls (not that this should happen)
    m_data.clear();

    // We'll load all the table contents, so we might as well pre-allocate
    m_data.reserve(m_nb_rows * m_nb_cols);

    int ret;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;
    auto sql = "SELECT * FROM " + m_name + ";";

    ret = sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail);
    if (ret != SQLITE_OK) throw "Failed preparing load query";

    while ( (ret = sqlite3_step(qry)) == SQLITE_ROW ) {
        for (int icol = 0; icol < m_nb_cols; icol++) {
            if constexpr (std::is_same<T, int>::value)
                m_data.push_back( sqlite3_column_int(qry, icol) );
            else if constexpr (std::is_same<T, double>::value)
                m_data.push_back( sqlite3_column_double(qry, icol) );
            else if constexpr (std::is_same<T, std::string>::value) {
                auto c = sqlite3_column_text(qry, icol);
                auto s = std::string(reinterpret_cast<const char*>(c));
                m_data.push_back(s);
            }
        }
    }
    if (ret != SQLITE_DONE) throw "Failed executing load query";

    ret = sqlite3_finalize(qry);
    if (ret != SQLITE_OK) throw "Failed finalizing load query";
}


template <class T>
void bmit::SqlTable<T>::write() {
    if (m_readonly) throw "Trying to write to readonly SqlTable";

    std::string sql;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;

    std::string sql_type;
    std::string sql_quote = "";
    if constexpr (std::is_same<T, int>::value) sql_type = "integer";
    if constexpr (std::is_same<T, double>::value) sql_type = "double";
    if constexpr (std::is_same<T, std::string>::value) {
        sql_type = "text";
        sql_quote = "'";
    }

    // Build CREATE TABLE statement
    auto sql_ss = std::stringstream();
    sql_ss << "CREATE TABLE " << m_name << "(";
    for (int icol = 0; icol < m_nb_cols; icol++) {
        sql_ss << "col_" << icol+1 << " " << sql_type << " NOT NULL";
        if (icol < m_nb_cols - 1) sql_ss << ",";
    }
    sql_ss << ");";
    sql = sql_ss.str();

    if(sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail) != SQLITE_OK)
        throw "Failed preparing create table query";

    if (sqlite3_step(qry) != SQLITE_DONE)
        throw "Failed executing create table query";

    if (sqlite3_finalize(qry) != SQLITE_OK)
        throw "Failed finalizing create table query";

    // Build INSERT statement
    sql_ss.str("");
    sql_ss.clear();
    sql_ss << "INSERT INTO " << m_name << " VALUES";
    auto row_sep = "";
    int index = 0;
    for (int irow = 0; irow < m_nb_rows; irow++) {
        sql_ss << row_sep << "(";
        auto val_sep = "";
        for (int icol = 0; icol < m_nb_cols; icol++) {
            sql_ss << val_sep << sql_quote << m_data[index++] << sql_quote;
            val_sep = ",";
        }
        sql_ss << ")";
        row_sep = ", ";
    }
    sql_ss << ";";
    sql = sql_ss.str();

    if (sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail) != SQLITE_OK)
        throw "Failed preparing insert query";

    if (sqlite3_step(qry) != SQLITE_DONE)
        throw "Failed executing insert query";

    if (sqlite3_finalize(qry) != SQLITE_OK)
        throw "Failed finalizing insert query";
}


template <class T>
bmit::SqlColumn<T>::SqlColumn(const std::string& name,
                              const std::string& path,
                              const std::string& column) {
    m_name = name;
    m_path = path;
    m_column = column;

    auto ret = sqlite3_open_v2(m_path.c_str(), &m_db, SQLITE_OPEN_READONLY, nullptr);
    if (ret != SQLITE_OK) throw "Failed opening database";

    /* Get row and col count */
    m_nb_rows = sql_column_row_count(m_db, m_name, m_column);
    m_nb_cols = 1;
}


template <class T>
bmit::SqlColumn<T>::SqlColumn(const std::string& name,
                              sqlite3* db,
                              const std::string& column) {
    m_name = name;
    m_db = db;
    m_column = column;

    /* Get row and col count */
    m_nb_rows = sql_column_row_count(m_db, m_name, m_column);
    m_nb_cols = 1;
}


template <class T>
bmit::SqlColumn<T>::SqlColumn(const std::string& name,
                              const std::string& path,
                              const std::string& column,
                              const int rows) {
    m_readonly = false;
    m_name = name;
    m_path = path;
    m_column = column;
    m_nb_rows = rows;
    m_nb_cols = 1;

    auto ret = sqlite3_open(m_path.c_str(), &m_db); // Read/Write/Create
    if (ret != SQLITE_OK) throw "Failed opening database";

    /* Initialize table data with default values */
    m_data = std::vector<T> (rows);
}


template <class T>
bmit::SqlColumn<T>::SqlColumn(const std::string& name,
                              sqlite3* db,
                              const std::string& column,
                              const int rows) {
    m_readonly = false;
    m_name = name;
    m_db = db;
    m_column = column;
    m_nb_rows = rows;
    m_nb_cols = 1;

    /* Initialize table data with default values */
    m_data = std::vector<T> (rows);
}


template <class T>
void bmit::SqlColumn<T>::load() {
    // Clear existing data.
    // To avoid blowing up table in case of successive calls (not that this should happen)
    m_data.clear();

    // We'll load all the table contents, so we might as well pre-allocate
    m_data.reserve(m_nb_rows * m_nb_cols);

    int ret;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;
    auto sql = "SELECT " + m_column + " FROM " + m_name + ";";

    ret = sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail);
    if (ret != SQLITE_OK) throw "Failed preparing column load query";

    while ( (ret = sqlite3_step(qry)) == SQLITE_ROW ) {
        if constexpr (std::is_same<T, int>::value)
            m_data.push_back( sqlite3_column_int(qry, 0) );
        else if constexpr (std::is_same<T, double>::value)
            m_data.push_back( sqlite3_column_double(qry, 0) );
        else if constexpr (std::is_same<T, std::string>::value) {
            auto c = sqlite3_column_text(qry, 0);
            auto s = std::string(reinterpret_cast<const char*>(c));
            m_data.push_back(s);
        }
    }
    if (ret != SQLITE_DONE) throw "Failed executing column load query";

    ret = sqlite3_finalize(qry);
    if (ret != SQLITE_OK) throw "Failed finalizing column load query";
}


template <class T>
void bmit::SqlColumn<T>::write() {
    if (m_readonly) throw "Trying to write to readonly SqlTable";

    std::string sql;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;

    std::string sql_type;
    std::string sql_quote = "";
    if constexpr (std::is_same<T, int>::value) sql_type = "integer";
    if constexpr (std::is_same<T, double>::value) sql_type = "double";
    if constexpr (std::is_same<T, std::string>::value) {
        sql_type = "text";
        sql_quote = "'";
    }

    // ToDo: create table if necessary

    // Build ALTER TABLE statement
    auto sql_ss = std::stringstream();
    sql_ss << "ALTER TABLE " << m_name
           << " ADD COLUMN " << m_column << " " << sql_type<< ";";
    sql = sql_ss.str();

    if(sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail) != SQLITE_OK)
        throw "Failed preparing alter table query";

    if (sqlite3_step(qry) != SQLITE_DONE)
        throw "Failed executing alter table query";

    if (sqlite3_finalize(qry) != SQLITE_OK)
        throw "Failed finalizing alter table query";

    // Build INSERT statement
    sql_ss.str("");
    sql_ss.clear();
    sql_ss << "INSERT INTO " << m_name << " (" << m_column << ") " << " VALUES";
    auto row_sep = "";
    int index = 0;
    for (int irow = 0; irow < m_nb_rows; irow++) {
        sql_ss << row_sep << "(" << sql_quote << m_data[index++] << sql_quote << ")";
        row_sep = ", ";
    }
    sql_ss << ";";
    sql = sql_ss.str();

    if (sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail) != SQLITE_OK)
        throw "Failed preparing column insert query";

    if (sqlite3_step(qry) != SQLITE_DONE)
        throw "Failed executing column insert query";

    if (sqlite3_finalize(qry) != SQLITE_OK)
        throw "Failed finalizing column insert query";
}


int csv_table_row_count(const std::string& path, const char sep) {
    int number_of_lines = 0;
    std::ifstream csvfile(path);
    std::string line;

    while (std::getline(csvfile, line))
        ++number_of_lines;

    csvfile.close();

    // Assuming header always present
    return number_of_lines - 1;
}


int csv_table_col_count(const std::string& path, const char sep) {
    // Read first line
    std::ifstream csvfile(path);
    if (!csvfile.is_open())
        throw std::runtime_error("Failed opening CSV input file");
    std::string line;
    std::getline(csvfile, line);
    csvfile.close();

    // Count columns
    // This is a naive implementation,
    // not accounting for escaped separators.
    auto n = (int) std::count(line.begin(), line.end(), sep);
    return n + 1;
}


int sql_table_row_count(sqlite3* db, const std::string& table) {
    int ret;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;
    auto sql = "SELECT COUNT(*) FROM " + table + ";";

    ret = sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail);
    if (ret != SQLITE_OK) throw "Failed preparing row count query";

    ret = sqlite3_step(qry);
    if (ret != SQLITE_ROW) throw "Failed executing row count query";

    auto count = sqlite3_column_int(qry, 0);

    ret = sqlite3_finalize(qry);
    if (ret != SQLITE_OK) throw "Failed finalizing row count query";

    return count;
}


int sql_table_col_count(sqlite3* db, const std::string& table) {
    int ret;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;
    auto sql = "SELECT * FROM " + table + " LIMIT 1;";

    ret = sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail);
    if (ret != SQLITE_OK) throw "Failed preparing col count query";

    ret = sqlite3_step(qry);
    if (ret != SQLITE_ROW) throw "Failed executing col count query";

    auto count = sqlite3_column_count(qry);

    ret = sqlite3_finalize(qry);
    if (ret != SQLITE_OK) throw "Failed finalizing col count query";

    return count;
}


int sql_column_row_count(sqlite3* db, const std::string& table,
                                      const std::string& column) {
    int ret;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;
    auto sql = "SELECT COUNT(" + column + ") FROM " + table + ";";

    ret = sqlite3_prepare_v2(db, sql.c_str(), -1, &qry, &qry_tail);
    if (ret != SQLITE_OK) throw "Failed preparing column record count query";

    ret = sqlite3_step(qry);
    if (ret != SQLITE_ROW) throw "Failed executing column record count query";

    auto count = sqlite3_column_int(qry, 0);

    ret = sqlite3_finalize(qry);
    if (ret != SQLITE_OK) throw "Failed finalizing column record count query";

    return count;
}


// Explicit instantiations
template class bmit::Table<int>;
template class bmit::Table<double>;
template class bmit::Table<std::string>;
template class bmit::CsvTable<int>;
template class bmit::CsvTable<double>;
template class bmit::CsvTable<std::string>;
template class bmit::SqlTable<int>;
template class bmit::SqlTable<double>;
template class bmit::SqlTable<std::string>;
template class bmit::SqlColumn<int>;
template class bmit::SqlColumn<double>;
template class bmit::SqlColumn<std::string>;
