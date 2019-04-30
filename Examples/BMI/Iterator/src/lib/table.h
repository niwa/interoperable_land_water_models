/*
 * table.h - Provides access to 1D/2D datasets of various formats.
 *
 * ITable      Common interface for all dataset formats
 *    ^
 *    |
 *    |
 *  Table      Implements storage for all formats.
 *    ^
 *    |
 *    |
 * CsvTable
 * SqlTable    Implement IO aspects specific to each format.
 * CqlColumn
 */

#ifndef BMIT_TABLE_H
#define BMIT_TABLE_H

#include <string>
#include <vector>

#include "sqlite3.h"

#define DEFAULT_CSV_SEP ','

namespace bmit {

    // This is the interface shared by all tables (csv, sql, ...)
    class ITable {
    public:
        ITable() {};
        ITable(const ITable& other) = delete;
        ITable(const ITable&& other) = delete;
        ITable& operator=(const ITable& other) = delete;
        ITable&& operator=(const ITable&& other) = delete;
        virtual ~ITable() = default;

        virtual int nb_cols() = 0;
        virtual int nb_rows() = 0;
        virtual const std::string& name() = 0;

        // Loads all data into memory
        // Used on input tables only.
        virtual void load() = 0;

        // Returns pointer to cell data.
        // Row and column indexes are 0-based.
        // In targeted use case, returned pointer will be passed
        // directly to BMI get/set functions, which will now hot to cast it.
        virtual void* cell_ptr(const int irow, const int icol) = 0;

        // Write data file.
        // Used on output tables only.
        virtual void write() = 0;
    };

    // Implements common table functionality
    template<class T>
    class Table : public ITable {
    public:
        Table() {};
        Table(const Table& other) = delete;
        Table(const Table&& other) = delete;
        Table& operator=(const Table& other) = delete;
        Table&& operator=(const Table&& other) = delete;
        ~Table() = default;

        virtual int nb_cols() {return m_nb_cols;}
        virtual int nb_rows() {return m_nb_rows;}

        virtual void* cell_ptr(const int irow, const int icol) {return &(m_data[irow * m_nb_cols + icol]);};
    protected:
        // Default access is readonly
        bool m_readonly = true;

        // Data is stored in a single vector.
        // Column dimension changes fastest: row1-col1, row1-col2, ..., rowM-colN
        std::vector<T> m_data;
        int m_nb_rows = 0;
        int m_nb_cols = 0;
    };


    template<class T>
    class CsvTable : public Table<T> {
        using Table<T>::m_readonly;
        using Table<T>::m_data;
        using Table<T>::m_nb_rows;
        using Table<T>::m_nb_cols;
    public:
        // Readonly
        explicit CsvTable(
            const std::string& name,
            const std::string& path,
            const char sep = DEFAULT_CSV_SEP);

        // Read/Write
        explicit CsvTable(
            const std::string& name,
            const std::string& path,
            const int rows,
            const int cols,
            const char sep = DEFAULT_CSV_SEP);

        CsvTable(const CsvTable &) = delete;
        CsvTable(CsvTable &&) noexcept = delete;
        CsvTable& operator=(const CsvTable &) = delete;
        CsvTable& operator=(CsvTable &&) noexcept = delete;
        ~CsvTable() {};

        const std::string& path() {return m_path;}

        // ITable functions
        virtual const std::string& name() {return m_name;}
        virtual void load();
        virtual void write();
    private:
        std::string m_name;
        std::string m_path;
        char m_sep = DEFAULT_CSV_SEP;
        // Split CSV line in items
        std::vector<std::string> split_csv_line(std::string& line);
    };


    template<class T>
    class SqlTable : public Table<T> {
        using Table<T>::m_readonly;
        using Table<T>::m_data;
        using Table<T>::m_nb_rows;
        using Table<T>::m_nb_cols;
    public:
        /* Readonly from db file */
        explicit SqlTable(
            const std::string& name,
            const std::string& path);

        /* Readonly from db pointer */
        explicit SqlTable(const std::string& name, sqlite3* db);

        /* Read/Write from db file */
        explicit SqlTable(
            const std::string& name,
            const std::string& path,
            const int rows,
            const int cols);

        /* Read/Write from db pointer */
        explicit SqlTable(
            const std::string& name,
            sqlite3* db,
            const int rows,
            const int cols);

        SqlTable(const SqlTable &) = delete;
        SqlTable(SqlTable &&) noexcept = delete;
        SqlTable& operator=(const SqlTable &) = delete;
        SqlTable& operator=(SqlTable &&) noexcept = delete;
        ~SqlTable() {sqlite3_close(m_db);};

        const std::string& path() {return m_path;}

        // ITable functions
        virtual const std::string& name() {return m_name;}
        virtual void load();
        virtual void write();
    private:
        std::string m_name;
        std::string m_path;
        sqlite3* m_db = nullptr;
    };


    template<class T>
    class SqlColumn : public Table<T> {
        using Table<T>::m_readonly;
        using Table<T>::m_data;
        using Table<T>::m_nb_rows;
        using Table<T>::m_nb_cols;
    public:
        /* Readonly from db file */
        explicit SqlColumn(
            const std::string& name,
            const std::string& path,
            const std::string& column);

        /* Readonly from db pointer */
        explicit SqlColumn(
            const std::string& name,
            sqlite3* db,
            const std::string& column);

        /* Read/Write from db file */
        explicit SqlColumn(
            const std::string& name,
            const std::string& path,
            const std::string& column,
            const int rows);

        /* Read/Write from db pointer */
        explicit SqlColumn(
            const std::string& name,
            sqlite3* db,
            const std::string& column,
            const int rows);

        SqlColumn(const SqlColumn &) = delete;
        SqlColumn(SqlColumn &&) noexcept = delete;
        SqlColumn& operator=(const SqlColumn &) = delete;
        SqlColumn& operator=(SqlColumn &&) noexcept = delete;
        ~SqlColumn() {sqlite3_close(m_db);};

        const std::string& path() {return m_path;}
        const std::string& column() {return m_column;}

        // ITable functions
        virtual const std::string& name() {return m_name;}
        virtual void load();
        virtual void write();
    private:
        std::string m_name;
        std::string m_path;
        std::string m_column;
        sqlite3* m_db = nullptr;
    };
}

#endif // BMIT_TABLE_H
