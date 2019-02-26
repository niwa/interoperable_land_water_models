#ifndef LOOKUP_LIB_H
#define LOOKUP_LIB_H

#include <string>
#include <variant>
#include <vector>

namespace lup {

    struct Input {
        std::string name;
        std::variant<std::string, double> value;
    };


    class Lookup {
    public:
        static Lookup *Create(std::string filename);

        static void Dispose(Lookup* lookup);

        virtual ~Lookup() = 0;

        virtual int count_inputs() = 0;

        virtual int count_outputs() = 0;

        virtual std::vector<std::string> get_input_names() = 0;

        virtual std::vector<std::string> get_output_names() = 0;

        virtual std::string get_var_type(std::string) = 0;

        virtual std::string get_var_units(std::string) = 0;

        virtual double get_value(std::vector<lup::Input> inputs,
                                 const std::string &output_name) = 0;

        virtual std::vector<double> get_values(std::vector<lup::Input> inputs) = 0;

    };

}

#endif // LOOKUP_LIB_H
