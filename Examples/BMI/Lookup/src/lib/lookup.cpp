#include <iostream>
#include <vector>
#include <algorithm>
#include <stdexcept>

#include "lookup.h"
#include "yaml-cpp\yaml.h"


class _Lookup : public lup::Lookup {
public:
    explicit _Lookup(std::string &filename);

    ~_Lookup() override = default;

    int count_inputs() override {return (int) _inputs.size(); };

    int count_outputs() override {return (int) _outputs.size(); };

    int get_output_index(const std::string&) override;

    std::vector<std::string> get_input_names() override { return _inputs; };

    std::vector<std::string> get_output_names() override { return _outputs; };

    std::string get_var_type(std::string name) override { return _types[name]; }

    std::string get_var_units(std::string name) override { return _units[name]; }

    std::vector<double> get_values(std::vector<lup::Input> inputs) override;

private:
    std::vector<std::string> load_var_names(const std::string &key);

    void enforce_mappings_for_numeric_inputs();

    void load_var_attrs();

    std::string map_input(std::string const &name, std::string const &value);

    std::string map_input(std::string const &name, double const &value);

    YAML::Node _config;

    /* Variable properties*/
    std::vector<std::string> _inputs{};
    std::vector<std::string> _outputs{};
    std::map<std::string, std::string> _types{};
    std::map<std::string, std::string> _units{};
};


_Lookup::_Lookup(std::string &filename) {
    _config = YAML::LoadFile(filename);
    _inputs = load_var_names("inputs");
    _outputs = load_var_names("outputs");
    load_var_attrs();
    enforce_mappings_for_numeric_inputs();
}


std::string _Lookup::map_input(std::string const &name, std::string const &value) {
    auto maps = _config["mappings"];

    /* No mapping for current input */
    if (!maps[name]) return value;

    /* Error: input value not found in mapping */
    if (!maps[name][value]) throw std::invalid_argument("String input value not in mapping");

    /* OK, return mapped value */
    return maps[name][value].as<std::string>();
}


std::string _Lookup::map_input(std::string const &name, double const &value) {
    auto map = _config["mappings"][name];

    /* Ensure value not lower than lower bound */
    if (value < map.begin()->first.as<double>()) {
        throw std::invalid_argument("Double input value < lower mapping bound");
    }

    auto cls = std::string();
    for (YAML::const_iterator it = map.begin(); it != map.end(); ++it) {
        auto bound = it->first.as<double>();
        if (value < bound) break;
        cls = it->second.as<std::string>();
    }
    return cls;
}


int _Lookup::get_output_index(std::string const &name) {
    auto it = std::find(_outputs.begin(), _outputs.end(), name);
    if (it == _outputs.end()) {
        throw std::invalid_argument("Requested output does not exists");
    }
    return (int) (it - _outputs.begin());
}


std::vector<double> _Lookup::get_values(std::vector<lup::Input> inputs) {

    /* Map input values to lookup classes */
    auto classes = std::vector<std::string>{};
    for (const auto &inp : inputs) {
        auto cls = std::string();
        switch (inp.value.index()) {
            case 0:
                cls = map_input(inp.name, std::get<std::string>(inp.value));
                break;
            case 1:
                cls = map_input(inp.name, std::get<double>(inp.value));
                break;
            default:
                break;
        }
        classes.push_back(cls);
    }

    /* Get lookup node matching classes */
    YAML::Node node = _config["lookup"];
    for (const auto &cls : classes) {
        if (node[cls]) {
            node.reset(node[cls]);
        }
        else
        {
            node.reset(node["_"]);
        }
    }

    if (node.IsNull() || !node.IsDefined()) {
        //  TODO: log this (unknown lookup combination, falling back to default)
        //  TODO: implement default fallback
        return _config["fallback_output"].as<std::vector<double> >();
    }

    return node.as<std::vector<double> >();
}


std::vector<std::string> _Lookup::load_var_names(const std::string &key) {
    auto names = std::vector<std::string> {};
    auto map = _config[key];
    for (YAML::const_iterator it = map.begin(); it != map.end(); ++it) {
        auto name = it->first.as<std::string>();
        names.push_back(name);
    }
    return names;
}


void _Lookup::enforce_mappings_for_numeric_inputs() {
    auto maps = _config["mappings"];
    for (const auto &var : _inputs) {

        /* Assuming all input types other than string are numeric*/
        if (_types[var] == "str") continue;

        if (!maps || !maps[var]) {
            throw std::invalid_argument("Numeric inputs must have a mapping");
        }
    }
}


void _Lookup::load_var_attrs(){
    /* Inputs */
    for (const auto &var : _inputs) {
        auto node = _config["inputs"][var];
        _types[var] = node["type"].as<std::string>();
        if (node["units"]) {
            _units[var] = node["units"].as<std::string>();
        } else {
            _units[var] = "";
        }
    }

    /* Outputs */
    for (const auto &var : _outputs) {
        auto node = _config["outputs"][var];
        _types[var] = node["type"].as<std::string>();
        if (node["units"]) {
            _units[var] = node["units"].as<std::string>();
        } else {
            _units[var] = "";
        }
    }
}

namespace lup {
    Lookup *Lookup::Create(std::string filename) {
        return new _Lookup(filename);
    }

    void Lookup::Dispose(Lookup* lookup) {
        delete lookup;
    }

    Lookup::~Lookup() = default;
}
