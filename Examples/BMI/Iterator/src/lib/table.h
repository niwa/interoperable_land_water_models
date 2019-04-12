#ifndef BMIT_TABLE_H
#define BMIT_TABLE_H

#include <string>
#include <vector>

#include "sqlite3.h"

namespace bmit {

    // This is the interface shared by all tables (csv, sql, ...)
    class ITable {
    public:
        ITable() {};
        ITable(const ITable& other) = delete;
        ITable(const ITable&& other) = delete;
        ITable& operator=(const ITable& other) = delete;
        ITable&& operator=(const ITable&& other) = delete;
        ~ITable() = default;

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


    template<class T>
    class Table : public ITable {
    public:
        Table() {};
        Table(const Table& other) = delete;
        Table(const Table&& other) = delete;
        Table& operator=(const Table& other) = delete;
        Table&& operator=(const Table&& other) = delete;
        ~Table() = default;

        virtual void* cell_ptr(const int irow, const int icol) {return &(m_data[irow][icol]);};
    protected:
        std::vector<std::vector<T>> m_data;
    };


    template<class T>
    class CsvTable : public Table<T> {
        using Table<T>::m_data;
    public:
        /* Input tables */
        explicit CsvTable(
            const std::string& name,
            const std::string& path) : m_name(name), m_path(path) {};

        /* Output tables */
        explicit CsvTable(
            const std::string& name,
            const std::string& path,
            const int rows,
            const int cols);

        CsvTable(const CsvTable &) = delete;
        CsvTable(CsvTable &&) noexcept = delete;
        CsvTable& operator=(const CsvTable &) = delete;
        CsvTable& operator=(CsvTable &&) noexcept = delete;
        ~CsvTable() {};

        const std::string& path() {return m_path;}
        const std::vector<std::string>& column_names() {return m_column_names;}

        void load();
        void write();

        std::vector<T> get_row(const int index);

        // CSV separator
        char sep() {return m_sep;};
        void sep(const char sep) {this->m_sep = sep;};

        // ITable functions
        virtual int nb_cols() {return m_nb_cols;}
        virtual int nb_rows() {return m_nb_rows;}
        virtual const std::string& name() {return m_name;}
    private:
        bool m_writable = false;
        std::string m_name;
        std::string m_path;
        int m_nb_cols = 0;
        int m_nb_rows = 0;
        std::vector<std::string> m_column_names;
        char m_sep = ',';
        // Split CSV line in items
        std::vector<std::string> split_csv_line(std::string& line);
    };


    template<class T>
    class SqlTable : public Table<T> {
        using Table<T>::m_data;
    public:
        /* Readonly from db file */
        explicit SqlTable(
            const std::string& name,
            const std::string& path);

        /* Read/Write from db file */
        explicit SqlTable(
            const std::string& name,
            const std::string& path,
            const int rows,
            const int cols);

        SqlTable(const SqlTable &) = delete;
        SqlTable(SqlTable &&) noexcept = delete;
        SqlTable& operator=(const SqlTable &) = delete;
        SqlTable& operator=(SqlTable &&) noexcept = delete;
        ~SqlTable() {sqlite3_close(m_db);};

        const std::string& path() {return m_path;}

        void load();
        void write();

        // ITable functions
        virtual int nb_cols() {return m_nb_cols;}
        virtual int nb_rows() {return m_nb_rows;}
        virtual const std::string& name() {return m_name;}
    private:
        bool m_readonly = true;
        std::string m_name;
        std::string m_path;
        sqlite3* m_db = nullptr;
        int m_nb_cols = 0;
        int m_nb_rows = 0;
    };
}

#endif // BMIT_TABLE_H
