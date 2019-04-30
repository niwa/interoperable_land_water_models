#ifndef BMIT_LIB_H
#define BMIT_LIB_H

#include <string>
#include <vector>

namespace bmit {

    class Iterator {
    public:
        static Iterator* Create(const std::string &filename);

        static void Dispose(Iterator* it);

        virtual ~Iterator() = 0;

        virtual void run() = 0;

        virtual int count_inputs() = 0;

        virtual int count_outputs() = 0;

        virtual std::vector<std::string> get_input_names() = 0;

        virtual std::vector<std::string> get_output_names() = 0;

        virtual std::string get_var_type(const std::string& name) = 0;

        virtual std::string get_var_units(const std::string& name) = 0;

        virtual int get_var_itemsize(const std::string& name) = 0;

        virtual int get_var_rank(const std::string& name) = 0;

        virtual std::vector<int> get_var_size(const std::string& name) = 0;

        virtual int get_var_nbytes(const std::string& name) = 0;

        virtual void get_value(const std::string& name, void* buffer) = 0;

        virtual void set_value(const std::string& name, void* buffer) = 0;
    };

}

#endif // BMIT_LIB_H
