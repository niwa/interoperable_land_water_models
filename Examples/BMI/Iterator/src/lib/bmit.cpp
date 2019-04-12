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


class _Iterator : public bmit::Iterator {
public:
    explicit _Iterator(const std::string &filename);

    ~_Iterator() override;

    void run() override;
private:
    YAML::Node m_config;
    bmit::Target* m_target;

    void validate_configuration();
    bmit::ITable* load_input_table(const std::string& name);
    bmit::ITable* prep_output_table(const std::string& name, const int rows, const int cols);
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

    /* Check configs match */
    try {
        validate_configuration();
    }
    catch (std::exception& e) {
        delete m_target;
        throw;
    }

}


_Iterator::~_Iterator() {
    delete m_target;
}


void _Iterator::run() {
    /* Load input tables */
    auto input_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_target->get_input_var_names()) {
        input_tables.push_back(load_input_table(name));
    }

    /* All tables have same size, so get dimensinos from first input table */
    auto nb_cols = input_tables[0]->nb_cols();
    auto nb_rows = input_tables[0]->nb_rows();

    /* Create output tables */
    auto output_tables = std::vector<bmit::ITable*> {};
    for (auto &name : m_target->get_output_var_names()) {
        output_tables.push_back(prep_output_table(name, nb_rows, nb_cols));
    }

    /* Loop over input combinations */
    for (auto &name : m_target->get_output_var_names()) {
        assert (m_config["outputs"][name]["format"].as<std::string>() == "csv");

        /* Loop over input combinations */
        auto input_names = m_target->get_input_var_names();
        for (int irow = 0; irow < nb_rows; irow++) {
            for (int icol = 0; icol < nb_cols; icol++) {
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


bmit::ITable* _Iterator::load_input_table(const std::string& name) {
    auto cfg = m_config["inputs"][name];
    auto format = cfg["format"].as<std::string>();
    auto type = cfg["type"].as<std::string>();

    if (format == "csv") {
        auto path = cfg["path"].as<std::string>();
        auto table = new bmit::CsvTable<int>(name, path);

        if (type == "int") {
            auto table = new bmit::CsvTable<int>(name, path);
            if (cfg["sep"]) table->sep(cfg["sep"].as<char>());
            table->load();
            return table;
        }
        else if (type == "double") {
            auto table = new bmit::CsvTable<double>(name, path);
            if (cfg["sep"]) table->sep(cfg["sep"].as<char>());
            table->load();
            return table;
        }
        else if (type == "str") {
            auto table = new bmit::CsvTable<std::string>(name, path);
            if (cfg["sep"]) table->sep(cfg["sep"].as<char>());
            table->load();
            return table;
        }
        else throw "Input type not implemented";

    }
    else throw "Input format not implemented";
}


bmit::ITable*
_Iterator::prep_output_table(const std::string& name, const int rows, const int cols) {
    auto cfg = m_config["outputs"][name];
    auto format = cfg["format"].as<std::string>();
    auto type = cfg["type"].as<std::string>();

    if (format == "csv") {
        auto path = cfg["path"].as<std::string>();
        if (type == "int") {
            auto table = new bmit::CsvTable<int>(name, path, rows, cols);
            if (cfg["sep"]) table->sep(cfg["sep"].as<char>());
            return table;
        }
        else if (type == "double") {
            auto table = new bmit::CsvTable<double>(name, path, rows, cols);
            if (cfg["sep"]) table->sep(cfg["sep"].as<char>());
            return table;
        }
        else if (type == "str") {
            auto table = new bmit::CsvTable<std::string>(name, path, rows, cols);
            if (cfg["sep"]) table->sep(cfg["sep"].as<char>());
            return table;
        }
        else throw "Output type not implemented";
    }
    else throw "Output format not implemented";

    /* Make the compiler happy */
    return nullptr;
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


