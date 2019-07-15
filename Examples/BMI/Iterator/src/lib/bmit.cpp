#include <algorithm>
#include <fstream>
#include <iterator>
#include <sstream>
#include <string>

#include <cassert>

#include "bmit.h"
#include "yaml-cpp/yaml.h"
#include "target.h"
#include "table.h"

#define BMIT_SQLITE_PTR_NAME "sqlite__pointer"


class _Iterator : public bmit::Iterator {
public:
    explicit _Iterator(const std::string &filename);

    ~_Iterator() override;

    void run() override;

    std::vector<int> get_var_shape(const std::string& name) override;

    int get_var_rank(const std::string& name) override;

    std::string get_var_type(const std::string& name) override;

    int get_var_count() override;

    std::string get_var_name(int index) override;

    void get_var(const std::string& name, void** ptr) override;

    void set_var(const std::string& name, void* ptr) override;
private:
    YAML::Node m_config;

    // The target BMI component
    bmit::Target* m_target;

    // Target input and output variable names
    std::vector<std::string> m_input_var_names;
    std::vector<std::string> m_output_var_names;

    // Needed to provide BMI rank/shape info
    size_t m_nb_rows = 0;
    size_t m_nb_cols = 0;

    // Some tables need a sqlite pointer
    sqlite3* m_sqlite_pointer = nullptr;

    // Checks iterator and target component inputs and outputs match
    // In/outputs counts and names must be identical
    void validate_configuration();

    // Sets number of rows/columns
    // Checks dimensions equal across all inputs
    void validate_inputs();

    // Access a table in readonly mode
    bmit::ITable* open_table(const std::string& name);

    // Create table to write to
    bmit::ITable* create_table(const std::string& name, const size_t rows, const size_t cols);
};


_Iterator::_Iterator(const std::string &filename) {
    m_config = YAML::LoadFile(filename);

    /* Load target library */
    auto lib_path = m_config["target"]["library"].as<std::string>();
    m_target = new bmit::Target(lib_path);

    /* Initialize target component */
    auto lib_cfg = m_config["target"]["config"].as<std::string>();
    if (m_target->initialize(lib_cfg) != 0) {
        throw std::runtime_error("Could not initialize target component");
    }

    /* Load variable names from config */
    auto inputs = m_config["inputs"];
    for (YAML::const_iterator it=inputs.begin(); it!=inputs.end();++it) {
        m_input_var_names.push_back(it->first.as<std::string>());
    }
    auto outputs = m_config["outputs"];
    for (YAML::const_iterator it=outputs.begin(); it!=outputs.end();++it) {
        m_output_var_names.push_back(it->first.as<std::string>());
    }

    /* Check configuration and inputs */
    try {
        validate_configuration();
        validate_inputs();
    }
    catch (std::exception&) {
        delete m_target;
        throw;
    }
}


_Iterator::~_Iterator() {
    delete m_target;
}


std::vector<int> _Iterator::get_var_shape(const std::string& name) {
    // SQLite pointer is a scalar
    if (name == BMIT_SQLITE_PTR_NAME) {
        return {1};
    }
    // One of the target variables.
    // All input and output tables/columns are required to have
    // identical dimensions. The first dimension is the number of
    // rows or records in table or column. For tables, the second
    // dimension is the number of columns.
    if (m_nb_cols == 1) return {(int) m_nb_rows};
    return {(int) m_nb_rows, (int) m_nb_cols};
}


int _Iterator::get_var_rank(const std::string& name) {
    // SQLite pointer is a scalar
    if (name == BMIT_SQLITE_PTR_NAME) return 1;
    // All input and output tables/columns are requireed to have
    // identical dimensions. So the rank is the same for all variables
    // and either 1 (for columns) or 2 (for tables)
    if (m_nb_cols == 1) return 1;
    return 2;
}


std::string _Iterator::get_var_type(const std::string& name) {
    if (name == BMIT_SQLITE_PTR_NAME) {
        // BMI has no 'pointer' type, so using int64 here.
        return "int64";
    } else {
        // Data types are same as for target component
        return m_target->get_var_type(name);
    }
}


int _Iterator::get_var_count() {
    // Variable names from target component + iterator's own sqlite pointer
    return m_target->get_var_count() + 1;
}


std::string _Iterator::get_var_name(int index) {
    int target_var_count = m_target->get_var_count();
    if (index < target_var_count) {
        // One of the target's variables
        return m_target->get_var_name(index);
    }

    if (index == target_var_count) {
        // Iterator's own sql pointer comes right after target variables
        return BMIT_SQLITE_PTR_NAME;
    }

    throw std::runtime_error("get_var_name index out of bounds");
}


void _Iterator::get_var(const std::string& name, void** ptr) {
    if (name == BMIT_SQLITE_PTR_NAME) {
        *ptr = (void*) &m_sqlite_pointer;
    }
    else if (m_config["inputs"][name] || m_config["outputs"][name]) {
        // So far, no use case where this would be needed.
        // To be implemented if required.
    }
}


void _Iterator::set_var(const std::string& name, void* ptr) {
    if (name == BMIT_SQLITE_PTR_NAME) {
        m_sqlite_pointer = (sqlite3*) ptr;
    }
    else if (m_config["inputs"][name] || m_config["outputs"][name]) {
        // So far, no use case where this would be needed.
        // To be implemented if required.
    }
}


void _Iterator::run() {
    /* Load input tables */
    auto input_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_input_var_names) {
        auto table = open_table(name);
        table->load();
        input_tables.push_back(table);
    }

    /* Create output tables */
    auto output_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_output_var_names) {
        output_tables.push_back(create_table(name, m_nb_rows, m_nb_cols));
    }

    /* Loop over input combinations */
    for (auto &name : m_output_var_names) {
        for (int irow = 0; irow < m_nb_rows; irow++) {
            for (int icol = 0; icol < m_nb_cols; icol++) {
                // Set target inputs
                for (const auto tbl : input_tables) {
                    const auto valp = tbl->get_cell(irow, icol);
                    m_target->set_var(tbl->name(), valp);
                }

                // Run target
                // Timestep should not matter (steady-state)
                m_target->update(0);

                // Copy target output to output tables
                for (const auto tbl : output_tables) {
                    void* ptr = nullptr;
                    m_target->get_var(tbl->name(), &ptr);
                    // ptr now points to target's internal mem,
                    // copy it's contents to iterator's output table
                    tbl->set_cell(irow, icol, ptr);
                }
            }
        }
    }

    /* Write output tables */
    for (const auto tbl : output_tables) {
        tbl->write();
    }

    /* Free tables */
    while (input_tables.size() != 0) {
        delete input_tables.back();
        input_tables.pop_back();
    }
    while (output_tables.size() != 0) {
        delete output_tables.back();
        output_tables.pop_back();
    }
}


void _Iterator::validate_configuration() {
    /* Collect all target variable names */
    int target_var_count = m_target->get_var_count();
    auto target_var_names = std::vector<std::string> {};
    for (int i = 0; i < target_var_count; i++) {
        auto name = m_target->get_var_name(i);
        target_var_names.push_back(name);
    }

    /* Check variable count */
    if (m_input_var_names.size() + m_output_var_names.size() != target_var_count) {
        throw std::runtime_error("Variable count mismatch between iterator and target dll");
    }

    /* Check variable names */
    auto inputs = m_config["inputs"];
    auto outputs = m_config["outputs"];
    for (const auto& name : target_var_names)
    {
        if (!inputs[name] && !outputs[name]) {
            throw std::runtime_error("Variable name mismatch between iterator and target dll");
        }
    }
}


void _Iterator::validate_inputs() {
    // Load all tables
    auto input_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_input_var_names) {
        input_tables.push_back(open_table(name));
    }

    // Get dimensions from first table
    assert(input_tables.size() > 0);
    m_nb_rows = input_tables[0]->nb_rows();
    m_nb_cols = input_tables[0]->nb_cols();

    // Check dimensions equal across all inputs and delete loaded tables
    bool rows_ok = true;
    bool cols_ok = true;
    while (input_tables.size() != 0) {
        if (input_tables.back()->nb_rows() != m_nb_rows) rows_ok = false;
        if (input_tables.back()->nb_cols() != m_nb_cols) cols_ok = false;
        delete input_tables.back();
        input_tables.pop_back();
    }

    if (!rows_ok)
        throw std::runtime_error("Number of rows not identical across inputs");

    if (!cols_ok)
        throw std::runtime_error("Number of columns not identical across inputs");
}


bmit::ITable* _Iterator::open_table(const std::string& name) {
    bmit::ITable* tbl = nullptr;
    auto cfg = m_config["inputs"][name];
    auto format = cfg["format"].as<std::string>();
    auto type = cfg["type"].as<std::string>();

    if (format == "sqlite" && cfg["path"] && cfg["column"]) {
        auto path = cfg["path"].as<std::string>();
        auto table = cfg["table"].as<std::string>();
        auto column = cfg["column"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlColumn<int>(name, path, table, column);
        } else if (type == "double") {
            tbl = new bmit::SqlColumn<double>(name, path, table, column);
        } else if (type == "str") {
            tbl = new bmit::SqlColumn<std::string>(name, path, table, column);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "sqlite" && cfg["path"]) {
        auto path = cfg["path"].as<std::string>();
        auto table = cfg["table"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlTable<int>(name, path, table);
        } else if (type == "double") {
            tbl = new bmit::SqlTable<double>(name, path, table);
        } else if (type == "str") {
            tbl = new bmit::SqlTable<std::string>(name, path, table);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "sqlite" && cfg["column"]) {
        if (m_sqlite_pointer == nullptr)
            throw std::runtime_error("SQLite pointer not set in iterator");
        auto column = cfg["column"].as<std::string>();
        auto table = cfg["table"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlColumn<int>(name, m_sqlite_pointer, table, column);
        } else if (type == "double") {
            tbl = new bmit::SqlColumn<double>(name, m_sqlite_pointer, table, column);
        } else if (type == "str") {
            tbl = new bmit::SqlColumn<std::string>(name, m_sqlite_pointer, table, column);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "sqlite") {
        if (m_sqlite_pointer == nullptr)
            throw std::runtime_error("SQLite pointer not set in iterator");
        auto table = cfg["table"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlTable<int>(name, m_sqlite_pointer, table);
        } else if (type == "double") {
            tbl = new bmit::SqlTable<double>(name, m_sqlite_pointer, table);
        } else if (type == "str") {
            tbl = new bmit::SqlTable<std::string>(name, m_sqlite_pointer, table);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "csv") {
        auto path = cfg["path"].as<std::string>();

        // Optional sep config
        auto sep = DEFAULT_CSV_SEP;
        if (cfg["sep"]) sep = cfg["sep"].as<char>();

        if (type == "int") {
            tbl = new bmit::CsvTable<int>(name, path, sep);
        } else if (type == "double") {
            tbl = new bmit::CsvTable<double>(name, path, sep);
        } else if (type == "str") {
            tbl = new bmit::CsvTable<std::string>(name, path, sep);
        }
        else throw std::runtime_error("Input type not implemented");
    }
    else throw std::runtime_error("Input format not implemented");

    return tbl;
}


bmit::ITable*
_Iterator::create_table(const std::string& name, const size_t rows, const size_t cols) {
    bmit::ITable* tbl = nullptr;
    auto cfg = m_config["outputs"][name];
    auto format = cfg["format"].as<std::string>();
    auto type = cfg["type"].as<std::string>();

    if (format == "sqlite" && cfg["path"] && cfg["column"]) {
        auto path = cfg["path"].as<std::string>();
        auto table = cfg["table"].as<std::string>();
        auto column = cfg["column"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlColumn<int>(name, path, table, column, rows);
        } else if (type == "double") {
            tbl = new bmit::SqlColumn<double>(name, path, table, column, rows);
        } else if (type == "str") {
            tbl = new bmit::SqlColumn<std::string>(name, path, table, column, rows);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "sqlite" && cfg["path"]) {
        auto path = cfg["path"].as<std::string>();
        auto table = cfg["table"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlTable<int>(name, path, table, rows, cols);
        } else if (type == "double") {
            tbl = new bmit::SqlTable<double>(name, path, table, rows, cols);
        } else if (type == "str") {
            tbl = new bmit::SqlTable<std::string>(name, path, table, rows, cols);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "sqlite" && cfg["column"]) {
        if (m_sqlite_pointer == nullptr)
            throw std::runtime_error("SQLite pointer not set in iterator");
        auto table = cfg["table"].as<std::string>();
        auto column = cfg["column"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlColumn<int>(name, m_sqlite_pointer, table, column, rows);
        } else if (type == "double") {
            tbl = new bmit::SqlColumn<double>(name, m_sqlite_pointer, table, column, rows);
        } else if (type == "str") {
            tbl = new bmit::SqlColumn<std::string>(name, m_sqlite_pointer, table, column, rows);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "sqlite") {
        if (m_sqlite_pointer == nullptr)
            throw std::runtime_error("SQLite pointer not set in iterator");
        auto table = cfg["table"].as<std::string>();

        if (type == "int") {
            tbl = new bmit::SqlTable<int>(name, m_sqlite_pointer, table, rows, cols);
        } else if (type == "double") {
            tbl = new bmit::SqlTable<double>(name, m_sqlite_pointer, table, rows, cols);
        } else if (type == "str") {
            tbl = new bmit::SqlTable<std::string>(name, m_sqlite_pointer, table, rows, cols);
        } else throw std::runtime_error("Input type not implemented");
    }
    else if (format == "csv") {
        auto path = cfg["path"].as<std::string>();

        // Optional sep config
        auto sep = DEFAULT_CSV_SEP;
        if (cfg["sep"]) sep = cfg["sep"].as<char>();

        if (type == "int") {
            tbl = new bmit::CsvTable<int>(name, path, rows, cols, sep);
        }
        else if (type == "double") {
            tbl = new bmit::CsvTable<double>(name, path, rows, cols, sep);
        }
        else if (type == "str") {
            tbl = new bmit::CsvTable<std::string>(name, path, rows, cols, sep);
        }
        else throw std::runtime_error("Output type not implemented");
    }
    else throw std::runtime_error("Output format not implemented");

    return tbl;
}


namespace bmit {
    Iterator* Iterator::Create(const std::string &filename) {
        return new _Iterator(filename);
    }

    void Iterator::Dispose(Iterator* it) {
        delete it;
    }

    Iterator::~Iterator() = default;
}


