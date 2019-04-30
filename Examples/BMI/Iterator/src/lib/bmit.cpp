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

    int count_inputs() override;

    int count_outputs() override;

    std::vector<std::string> get_input_names() override;

    std::vector<std::string> get_output_names() override;

    std::string get_var_type(const std::string& name) override;

    std::string get_var_units(const std::string& name) override;

    int get_var_itemsize(const std::string& name) override;

    int get_var_rank(const std::string& name) override;

    std::vector<int> get_var_size(const std::string& name) override;

    int get_var_nbytes(const std::string& name) override;

    void get_value(const std::string& name, void* buffer) override;

    void set_value(const std::string& name, void* buffer) override;
private:
    YAML::Node m_config;

    // The target BMI component
    bmit::Target* m_target;

    // Needed to provide BMI rank/size info
    int m_nb_rows = 0;
    int m_nb_cols = 0;

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
    bmit::ITable* create_table(const std::string& name, const int rows, const int cols);
};


_Iterator::_Iterator(const std::string &filename) {
    m_config = YAML::LoadFile(filename);

    /* Load target library */
    auto lib_path = m_config["target"]["library"].as<std::string>();
    m_target = new bmit::Target(lib_path);

    /* Initialize target component */
    auto lib_cfg = m_config["target"]["config"].as<std::string>();
    if (m_target->initialize(lib_cfg) != 0) {
        throw "Could not initialize target component";
    }

    /* Check configuration and inputs */
    try {
        validate_configuration();
        validate_inputs();
    }
    catch (std::exception& e) {
        delete m_target;
        throw;
    }
}


_Iterator::~_Iterator() {
    delete m_target;
}


int _Iterator::count_inputs() {
    // Number of input from target component + own sqlite pointer
    return m_target->get_input_var_name_count() + 1;
}


int _Iterator::count_outputs() {
    // Number of outputs is same as for target component
    return m_target->get_output_var_name_count();
}


std::vector<std::string> _Iterator::get_input_names() {
    // Input names from target component + own sqlite pointer
    auto names = m_target->get_input_var_names();
    names.push_back(BMIT_SQLITE_PTR_NAME);
    return names;
}


std::vector<std::string> _Iterator::get_output_names() {
    // Output names are same as for target component
    return m_target->get_output_var_names();
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


std::string _Iterator::get_var_units(const std::string& name) {
    if (name == BMIT_SQLITE_PTR_NAME)
        return "-";
    else
        return m_target->get_var_units(name);
}


int _Iterator::get_var_itemsize(const std::string& name) {
    // Using int* instead of sqlite*
    if (name == BMIT_SQLITE_PTR_NAME) return sizeof(sqlite3*);
    // Item sizes are same as for target component
    // (because types are same too)
    return m_target->get_var_itemsize(name);
}


int _Iterator::get_var_rank(const std::string& name) {
    // SQLite pointer is a scalar
    if (name == BMIT_SQLITE_PTR_NAME) return 0;
    // All input and output tables/columns are requireed to have
    // identical dimensions. So the rank is the same for all variables
    // and either 1 (for columns) or 2 (for tables)
    if (m_nb_cols == 1) return 1;
    return 2;
}


std::vector<int> _Iterator::get_var_size(const std::string& name) {
    // SQLite pointer is a scalar
    if (name == BMIT_SQLITE_PTR_NAME) return {1};
    // All input and output tables/columns are required to have
    // identical dimensions. The first dimension is the number of
    // rows or records in table or column. For tables, the second
    // dimension is the number of columns.
    if (m_nb_cols == 1) return {m_nb_rows};
    return {m_nb_rows, m_nb_cols};
}


int _Iterator::get_var_nbytes(const std::string& name) {
    // SQLite pointer is a scalar
    if (name == BMIT_SQLITE_PTR_NAME) {
        return _Iterator::get_var_itemsize(name);
    }
    // All input and output tables/columns are requireed to have
    // identical dimensions.
    return m_nb_rows * m_nb_cols * m_target->get_var_itemsize(name);
}


void _Iterator::get_value(const std::string& name, void* buffer) {
    if (name == BMIT_SQLITE_PTR_NAME) {
        buffer = (void*) m_sqlite_pointer;
    }
    else if (m_config["inputs"][name] || m_config["outputs"][name]) {
        // So far, no use case where this would be needed.
        // To be implemented if required.
    }
}


void _Iterator::set_value(const std::string& name, void* buffer) {
    if (name == BMIT_SQLITE_PTR_NAME) {
        m_sqlite_pointer = (sqlite3*) buffer;
    }
    else if (m_config["inputs"][name] || m_config["outputs"][name]) {
        // So far, no use case where this would be needed.
        // To be implemented if required.
    }
}


void _Iterator::run() {
    /* Load input tables */
    auto input_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_target->get_input_var_names()) {
        auto table = open_table(name);
        table->load();
        input_tables.push_back(table);
    }

    /* Create output tables */
    auto output_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_target->get_output_var_names()) {
        output_tables.push_back(create_table(name, m_nb_rows, m_nb_cols));
    }

    /* Loop over input combinations */
    for (auto &name : m_target->get_output_var_names()) {

        auto input_names = m_target->get_input_var_names();
        for (int irow = 0; irow < m_nb_rows; irow++) {
            for (int icol = 0; icol < m_nb_cols; icol++) {
                /* Set target inputs */
                for (const auto tbl : input_tables) {
                    auto valp = tbl->cell_ptr(irow, icol);
                    m_target->set_value(tbl->name(), valp);
                }

                /* Run target */
                m_target->update();

                /* Copy target output to output tables */
                for (const auto tbl : output_tables) {
                    m_target->get_value(
                        tbl->name(),
                        tbl->cell_ptr(irow, icol));
                }
            }
        }

        /* Write output tables */
        for (const auto tbl : output_tables) {
            tbl->write();
        }
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
    auto inputs = m_config["inputs"];
    auto outputs = m_config["outputs"];

    /* Check input count */
    auto nb_inputs = inputs.size();
    auto nb_target_inputs = m_target->get_input_var_name_count();
    if (nb_inputs != nb_target_inputs) {
        throw "input count mismatch";
    }

    /* Check output count */
    auto nb_outputs = outputs.size();
    auto nb_target_outputs = m_target->get_output_var_name_count();
    if (nb_outputs != nb_target_outputs) {
        throw "output count mismatch";
    }

    /* Check input names */
    {
        auto names = m_target->get_input_var_names();
        for (const auto& name : names)
        {
            if (!inputs[name]) throw "input name mismatch";
        }
    }

    /* Check output names */
    {
        auto names = m_target->get_output_var_names();
        for (const auto& name : names)
        {
            if (!outputs[name]) throw "output name mismatch";
        }
    }
}


void _Iterator::validate_inputs() {
    // Load all tables
    auto input_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_target->get_input_var_names()) {
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
    bmit::ITable* table = nullptr;
    auto cfg = m_config["inputs"][name];
    auto format = cfg["format"].as<std::string>();
    auto type = cfg["type"].as<std::string>();

    if (format == "sqlite" && cfg["path"]) {
        auto path = cfg["path"].as<std::string>();

        if (type == "int") {
            table = new bmit::SqlTable<int>(name, path);
        } else if (type == "double") {
            table = new bmit::SqlTable<double>(name, path);
        } else if (type == "str") {
            table = new bmit::SqlTable<std::string>(name, path);
        } else throw "Input type not implemented";
    }
    else if (format == "sqlite") {
        if (m_sqlite_pointer == nullptr)
            throw std::runtime_error("SQLite pointer not set in iterator");

        if (type == "int") {
            table = new bmit::SqlTable<int>(name, m_sqlite_pointer);
        } else if (type == "double") {
            table = new bmit::SqlTable<double>(name, m_sqlite_pointer);
        } else if (type == "str") {
            table = new bmit::SqlTable<std::string>(name, m_sqlite_pointer);
        } else throw "Input type not implemented";
    }
    else if (format == "csv") {
        auto path = cfg["path"].as<std::string>();

        // Optional sep config
        auto sep = DEFAULT_CSV_SEP;
        if (cfg["sep"]) sep = cfg["sep"].as<char>();

        if (type == "int") {
            table = new bmit::CsvTable<int>(name, path, sep);
        } else if (type == "double") {
            table = new bmit::CsvTable<double>(name, path, sep);
        } else if (type == "str") {
            table = new bmit::CsvTable<std::string>(name, path, sep);
        }
        else throw "Input type not implemented";
    }
    else throw "Input format not implemented";

    return table;
}


bmit::ITable*
_Iterator::create_table(const std::string& name, const int rows, const int cols) {
    bmit::ITable* table = nullptr;
    auto cfg = m_config["outputs"][name];
    auto format = cfg["format"].as<std::string>();
    auto type = cfg["type"].as<std::string>();

    if (format == "sqlite" && cfg["path"]) {
        auto path = cfg["path"].as<std::string>();

        if (type == "int") {
            table = new bmit::SqlTable<int>(name, path, rows, cols);
        } else if (type == "double") {
            table = new bmit::SqlTable<double>(name, path, rows, cols);
        } else if (type == "str") {
            table = new bmit::SqlTable<std::string>(name, path, rows, cols);
        } else throw "Input type not implemented";
    }
    else if (format == "sqlite") {
        if (m_sqlite_pointer == nullptr)
            throw std::runtime_error("SQLite pointer not set in iterator");

        if (type == "int") {
            table = new bmit::SqlTable<int>(name, m_sqlite_pointer, rows, cols);
        } else if (type == "double") {
            table = new bmit::SqlTable<double>(name, m_sqlite_pointer, rows, cols);
        } else if (type == "str") {
            table = new bmit::SqlTable<std::string>(name, m_sqlite_pointer, rows, cols);
        } else throw "Input type not implemented";
    }
    else if (format == "csv") {
        auto path = cfg["path"].as<std::string>();

        // Optional sep config
        auto sep = DEFAULT_CSV_SEP;
        if (cfg["sep"]) sep = cfg["sep"].as<char>();

        if (type == "int") {
            table = new bmit::CsvTable<int>(name, path, rows, cols, sep);
        }
        else if (type == "double") {
            table = new bmit::CsvTable<double>(name, path, rows, cols, sep);
        }
        else if (type == "str") {
            table = new bmit::CsvTable<std::string>(name, path, rows, cols, sep);
        }
        else throw "Output type not implemented";
    }
    else throw "Output format not implemented";

    return table;
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


