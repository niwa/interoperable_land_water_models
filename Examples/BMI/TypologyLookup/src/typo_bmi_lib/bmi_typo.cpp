#include <string>
#include <cstring>

#include "bmi_typo.h"

using namespace Typology;


void BmiLookup::
Initialize(std::string csv_file)
{
    if (csv_file != "")
    {
        _lookup.LoadCsvTable(csv_file);
    }
}


void BmiLookup::
GetVarType(const char * name, char * type)
{
    if (strcmp(name, "typology") == 0)
        strncpy(type, "byte", 2048);
    else if (strcmp(name, "nitrogen") == 0)
        strncpy(type, "double", 2048);
    else if (strcmp(name, "phosphorus") == 0)
        strncpy(type, "double", 2048);
    else if (strcmp(name, "table") == 0)
        strncpy(type, "double", 2048);
    else
        strncpy(type, "", 2048);
}


void BmiLookup::
GetVarItemsize(const char *name, int * itemsize)
{
    if (strcmp(name, "typology") == 0)
        *itemsize = sizeof(char);
    else if (strcmp(name, "nitrogen") == 0)
        *itemsize = sizeof(double);
    else if (strcmp(name, "phosphorus") == 0)
        *itemsize = sizeof(double);
    else if (strcmp(name, "table") == 0)
        *itemsize = sizeof(double);
    else
        *itemsize = 0;
}


void BmiLookup::
GetVarUnits(const char * name, char * units)
{
    if (strcmp(name, "typology") == 0)
        strncpy(units, "", 2048);
    else if (strcmp(name, "nitrogen") == 0)
        strncpy(units, "kg_ha-1", 2048);
    else if (strcmp(name, "phosphorus") == 0)
        strncpy(units, "kg_ha-1", 2048);
    else if (strcmp(name, "table") == 0)
        strncpy(units, "kg_ha-1", 2048);
    else
        strncpy(units, "", 2048);
}


void BmiLookup::
GetVarRank(const char *name, int * rank)
{
    if (strcmp(name, "typology") == 0)
        *rank = 0;
    else if (strcmp(name, "nitrogen") == 0)
        *rank = 0;
    else if (strcmp(name, "phosphorus") == 0)
        *rank = 0;
    else if (strcmp(name, "table") == 0)
        *rank = 2;
    else
        *rank = -1;
}


void BmiLookup::
GetVarSize(const char * name, int* size)
{
    int shape[2];
    this->GetGridShape(name, shape);
    *size = shape[0] * shape[1];

    if (strcmp(name, "typology") == 0)
        *size = 128;
    else if (strcmp(name, "nitrogen") == 0)
        *size = 1;
    else if (strcmp(name, "phosphorus") == 0)
        *size = 1;
    else if (strcmp(name, "table") == 0)
        *size = _lookup.GetNumberOfTypologies() * _lookup.GetNumberOfSubstances();
    else
        *size = 0;
}


void BmiLookup::
GetVarNbytes(const char * name, int* nbytes)
{
    int itemsize;
    int size;

    this->GetVarItemsize(name, &itemsize);
    this->GetVarSize(name, &size);
    *nbytes = itemsize * size;
}


void BmiLookup::
GetGridType (const char * name, char * type)
{
    strncpy(type, "", 2048);
}


void BmiLookup::
GetValue (const char * name, char *dest)
{
    char * src = NULL;
    int nbytes = 0;

    this->GetValuePtr(name, &src);
    this->GetVarNbytes(name, &nbytes);

    memcpy (dest, src, nbytes);
}


void BmiLookup::
GetValuePtr (const char * name, char **dest)
{
    if (strcmp(name, "typology") == 0)
        *dest = (char*) &(this->typology[0]);
    else if (strcmp(name, "nitrogen") == 0)
        *dest = (char*) &(this->nitrogen);
    else if (strcmp(name, "phosphorus") == 0)
        *dest = (char*) &(this->phosphorus);
    else if (strcmp(name, "table") == 0)
    {
        double** table = this->_lookup.GetTablePtr();
        *dest = (char*) table[0];
    }

    else
        *dest = NULL;
}


void BmiLookup::
SetValue (const char * name, char *src)
{
    char * dest = NULL;
    this->GetValuePtr(name, &dest);

    if (dest && strcmp(name, "typology") == 0)
    {
        int nbytes = 0;
        this->GetVarNbytes(name, &nbytes);
        memcpy(dest, src, nbytes);

        // Update lookup values to reflect change
        this->nitrogen = this->_lookup.GetValue(this->typology, "N");
        this->phosphorus = this->_lookup.GetValue(this->typology, "P");
    }
}


void BmiLookup::
GetComponentName (char * name)
{
    strncpy(name, "Typology lookup table", 2048);
}


void BmiLookup::
GetInputVarNameCount(int * count)
{
    *count = this->input_var_name_count;
}


void BmiLookup::
GetOutputVarNameCount(int * count)
{
    *count = this->output_var_name_count;
}


void BmiLookup::
GetInputVarNames (char **names)
{
    for (int i=0; i<this->input_var_name_count; i++)
    {
        strncpy(names[i], (const char *)this->input_var_names[i], 2048);
    }
}


void BmiLookup::
GetOutputVarNames (char **names)
{
    for (int i=0; i<this->output_var_name_count; i++)
    {
        strncpy(names[i], (const char *)this->output_var_names[i], 2048);
    }
}
