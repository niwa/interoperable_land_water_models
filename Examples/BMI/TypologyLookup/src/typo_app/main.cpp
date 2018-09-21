#include <iostream>

#include "typo.h"

int main()
{
    std::cout << "Typology Lookup App v0.1\n";

    auto lookup = Typology::Lookup();

    std::cout << "'P' value for 'typo_1': " << lookup.GetLoad("typo_1", "P") << std::endl;

    double** table = lookup.GetTablePtr();

    std::cout << "Data @ (2,1): " << table[2][1] << std::endl;
}
