#include <algorithm>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "table.h"

static int sql_table_row_count(sqlite3* db, const std::string& table);
static int sql_table_col_count(sqlite3* db, const std::string& table);

template <class T>
bmit::CsvTable<T>::CsvTable(const std::string& name, const std::string& path, const int rows, const int cols) {
    m_name = name;
    m_path = path;
    if (rows == 0 || cols == 0) throw "Writable CsvTable dimensions cannot be 0";
    m_nb_rows = rows;
    m_nb_cols = cols;
    m_writable = true;

    /* Initialize table data with default values */
    m_data = std::vector<std::vector<T>> (rows, std::vector<T>(cols));
}

template <class T>
void bmit::CsvTable<T>::load() {
    std::ifstream csvfile(m_path);
    std::string line;

    /* Assuming header always present */
    std::getline(csvfile, line);
    m_column_names = split_csv_line(line);
    m_nb_cols = m_column_names.size();

    while (std::getline(csvfile, line)) {
        auto tokens = split_csv_line(line);

        auto row = std::vector<T> (m_nb_cols);
        std::transform(tokens.begin(), tokens.end(), row.begin(), [](const std::string& token)
        {
            T value;
            std::stringstream{token} >> value;
            return value;
        });

        /* Append row row to table */
        m_data.push_back(row);
    }
    m_nb_rows = m_data.size();
}


template <class T>
void bmit::CsvTable<T>::write() {
    if (!m_writable) throw "Trying to write to non-writable CsvTable";

    /* Generate header */
    auto header = std::stringstream();
    header << "Col_1";
    for (int icol = 1; icol < m_nb_cols; icol++) {
        header << m_sep << "Col_" << icol+1;
    }
    header << "\n";

    /* Write header */
    std::ofstream csvfile;
    csvfile.open(m_path, std::ios::out);
    csvfile << header.str();

    /* Write rows */
    for (const auto& row : m_data) {
        /* First column */
        csvfile << row[0];
        /* Remaining columns */
        for (int icol = 1; icol < row.size();  icol++) {
            csvfile << m_sep << row[icol];
        }
        csvfile << "\n";
    }

    csvfile.close();
}


template <class T>
std::vector<T>
bmit::CsvTable<T>::get_row(const int index) {
    return m_data[index];
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
bmit::SqlTable<T>::SqlTable(const std::string& name, const std::string& path, const int rows, const int cols) {
    m_readonly = false;
    m_name = name;
    m_path = path;
    m_nb_rows = rows;
    m_nb_cols = cols;

    auto ret = sqlite3_open(m_path.c_str(), &m_db); // Read/Write/Create
    if (ret != SQLITE_OK) throw "Failed opening database";

    /* Initialize table data with default values */
    m_data = std::vector<std::vector<T>> (rows, std::vector<T>(cols));
}


template <class T>
void bmit::SqlTable<T>::load() {
    int ret;
    sqlite3_stmt* qry = nullptr;
    const char* qry_tail = nullptr;
    auto sql = "SELECT * FROM " + m_name + ";";

    ret = sqlite3_prepare_v2(m_db, sql.c_str(), -1, &qry, &qry_tail);
    if (ret != SQLITE_OK) throw "Failed preparing load query";

    while ( (ret = sqlite3_step(qry)) == SQLITE_ROW ) {
        auto row = std::vector<T> {};
        row.reserve(m_nb_cols);
        for (int icol = 0; icol < m_nb_cols; icol++) {
            if constexpr (std::is_same<T, int>::value)
                row.push_back( sqlite3_column_int(qry, icol) );
            else if constexpr (std::is_same<T, double>::value)
                row.push_back( sqlite3_column_double(qry, icol) );
            else if constexpr (std::is_same<T, std::string>::value) {
                auto c = sqlite3_column_text(qry, icol);
                auto s = std::string(reinterpret_cast<const char*>(c));
                row.push_back(s);
            }
        }
        m_data.push_back(row);
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

    sql_ss.str("");
    sql_ss.clear();
    sql_ss << "INSERT INTO " << m_name << " VALUES";
    auto row_sep = "";
    for (const auto& row : m_data) {
        sql_ss << row_sep << "(";
        auto val_sep = "";
        for (const auto& val : row) {
            sql_ss << val_sep << sql_quote << val << sql_quote;
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


// https://bytefreaks.net/programming-2/c/c-undefined-reference-to-templated-class-function
template class bmit::Table<int>;
template class bmit::Table<double>;
template class bmit::Table<std::string>;
template class bmit::CsvTable<int>;
template class bmit::CsvTable<double>;
template class bmit::CsvTable<std::string>;
template class bmit::SqlTable<int>;
template class bmit::SqlTable<double>;
template class bmit::SqlTable<std::string>;
