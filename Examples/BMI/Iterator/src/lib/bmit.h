#ifndef BMIT_LIB_H
#define BMIT_LIB_H

#include <string>

namespace bmit {

    class Iterator {
    public:
        static Iterator* Create(const std::string &filename);

        static void Dispose(Iterator* it);

        virtual ~Iterator() = 0;

        virtual void run() = 0;
    };

}

#endif // BMIT_LIB_H
