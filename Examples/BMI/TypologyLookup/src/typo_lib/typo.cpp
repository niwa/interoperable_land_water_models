#include <iostream>

#include "typo.h"


using namespace Typology;


Lookup::Lookup()
{
    // initialize table
    table = new double*[nbTypologies];
    table[0] = new double[nbTypologies * nbSubstances];
    for (int i=1; i<nbTypologies; i++)
    {
        table[i] = table[i-1] + nbSubstances;
    }

    // Fill table
    table[0][0] = 1.1;
    table[0][1] = 1.2;
    table[1][0] = 2.1;
    table[1][1] = 2.2;
    table[2][0] = 3.1;
    table[2][1] = 3.2;
}


Lookup::~Lookup(){}


double Lookup::GetLoad(std::string typology, std::string substance)
{
    // Find typology row index
    int irow = -1;
    for (int i=0; i<nbTypologies; i++)
    {
        if (typologies[i] == typology)
        {
            irow = i;
            break;
        }
    }
    if (irow == -1) return -999.;

    // Find substance column index
    int icol = -1;
    for (int i=0; i<nbSubstances; i++)
    {
        if (substances[i] == substance)
        {
            icol = i;
            break;
        }
    }

    if (icol == -1) return -999.;

    return table[irow][icol];
}


void Lookup::GetTable(double** values)
{
    for (int irow=0; irow<nbTypologies; irow++)
    {
        for (int icol=0; icol<nbSubstances; icol++)
        {
            values[irow][icol] = table[irow][icol];
        }
    }
}
