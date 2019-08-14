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

        virtual std::vector<int> get_var_shape(const std::string& name) = 0;

        virtual int get_var_rank(const std::string& name) = 0;

        virtual std::string get_var_type(const std::string& name) = 0;

        virtual int get_var_count() = 0;

        virtual std::string get_var_name(int index) = 0;

        virtual void get_var(const std::string& name, void** ptr) = 0;

        virtual void set_var(const std::string& name, const void* ptr) = 0;
    };

}

#endif // BMIT_LIB_H
