#include <fstream>
#include <iostream>


#include "typo.h"


using namespace Typology;


Lookup::Lookup(){}


Lookup::~Lookup(){}


void Lookup::Clear()
{
    if (nbTypologies == 0) return;

    delete[] table[0];
    delete[] table;

    typologies.clear();
    nbTypologies = 0;
}


void Lookup::LoadCsvTable(std::string csvFileName)
{
    /* Discard possible existing data */
    Clear();

    /* Read lines from csv file */
    std::vector<std::string> lines;
    {
        std::ifstream file(csvFileName);
        if (!file.is_open())
        {
            std::cout << "[WARNING] Could not open file " << csvFileName << "\n";
            return;
        }

        std::string line;
        while (std::getline(file, line))
        {
            lines.push_back(line);
        }

        /* Check if empty */
        if (lines.size() == 0)
        {
            std::cout << "[WARNING] csv file is empty\n";
            return;
        }

        /* Check if header only */
        if (lines.size() == 1)
        {
            std::cout << "[WARNING] csv file contains no data\n";
            return;
        }

        /* Check number of fields */
        int nbSeparators = 0;
        for (int i=0; i<lines[0].size(); i++)
        {
            if (lines[0][i] == ';') nbSeparators++;
        }
        if (nbSeparators != nbSubstances)
        {
            std::cout << "[WARNING] wrong file format\n";
            return;
        }
    }

    /* Parce csv lines into data table */
    {
        /* Allocate table */
        nbTypologies = lines.size() - 1; // -1 for header
        table = new double*[nbTypologies];
        table[0] = new double[nbTypologies * nbSubstances];
        for (int i=1; i<nbTypologies; i++)
        {
            table[i] = table[i-1] + nbSubstances;
        }

        /* Fill table */
        for (int iLine=1; iLine<lines.size(); iLine++)
        {

            std::string line = lines[iLine];
            int sepIndexes[nbSubstances];
            int iSep = 0;
            for (int iChar=0; iChar<line.size(); iChar++)
            {
                if (line[iChar] == ';') sepIndexes[iSep++] = iChar;
            }

            const int iRow = iLine-1;
            typologies.push_back(line.substr(0,sepIndexes[0]));
            table[iRow][0] = std::stod(
                line.substr(sepIndexes[0]+1, sepIndexes[1]-sepIndexes[0]));
            table[iRow][1] = std::stod(
                line.substr(sepIndexes[1]+1, line.size()-sepIndexes[2]));
        }
    }
}


double Lookup::GetValue(std::string typology, std::string substance)
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
