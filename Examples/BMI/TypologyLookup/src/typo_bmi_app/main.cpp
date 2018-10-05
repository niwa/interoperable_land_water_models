#include <iostream>
#include <cstring>

#include "bmi_typo.h"

int main()
{
    std::cout << "Typology Lookup BMI App v0.1\n";

    /*
        Create and initialize BMI component
    */

    auto lookup = Typology::BmiLookup();
    lookup.Initialize("typologies_test.csv");
    std::cout << "BMI component initialized.\n";


    /*
        Get and display BMI component name
    */
    {
        char name[2048];
        lookup.GetComponentName(&name[0]);
        std::cout << "BMI component name: '" << name << "'\n";
    }


    /*
        Discover name of in/output variables
    */
    int number_of_inputs;
    int number_of_outputs;
    char** input_names;
    char** output_names;
    {
        // Inputs
        lookup.GetInputVarNameCount(&number_of_inputs);
        std::cout << "Component has " << number_of_inputs << " input variable(s):\n";

        input_names = new char*[number_of_inputs];
        for (int i=0; i<number_of_inputs; i++)
        {
            input_names[i] = new char[2048];
        }

        lookup.GetInputVarNames(input_names);
        for (int i=0; i<number_of_inputs; i++)
        {
            std::cout << "\t" << i+1 << ". " << input_names[i] << "\n";
        }

        // Outputs
        lookup.GetOutputVarNameCount(&number_of_outputs);
        std::cout << "Component has " << number_of_outputs << " output variable(s):\n";

        output_names = new char*[number_of_outputs];
        for (int i=0; i<number_of_outputs; i++)
        {
            output_names[i] = new char[2048];
        }

        lookup.GetOutputVarNames(output_names);
        for (int i=0; i<number_of_outputs; i++)
        {
            std::cout << "\t" << i+1 << ". " << output_names[i] << "\n";
        }
    }

    /*
        Method 1: Retrieve single values by setting desired typology first.
    */
    {
        std::cout << "\nMethod 1: Retrieving single values\n";

        char typology[128];
        double n_value;
        double p_value;

        strncpy(typology, "typology_1", 128);
        std::cout << "Looking up values for typology '" << typology << "':\n";
        lookup.SetValue(input_names[0], typology);
        lookup.GetValue(output_names[0], (char*)&n_value);
        lookup.GetValue(output_names[1], (char*)&p_value);
        std::cout << "\tValue for " << output_names[0] << "\t: " << n_value << "\n";
        std::cout << "\tValue for " << output_names[1] << "\t: " << p_value << "\n";

        strncpy(typology, "typology_2", 128);
        std::cout << "Looking up values for typology '" << typology << "':\n";
        lookup.SetValue(input_names[0], typology);
        lookup.GetValue(output_names[0], (char*)&n_value);
        lookup.GetValue(output_names[1], (char*)&p_value);
        std::cout << "\tValue for " << output_names[0] << "\t: " << n_value << "\n";
        std::cout << "\tValue for " << output_names[1] << "\t: " << p_value << "\n";

        strncpy(typology, "typology_3", 128);
        std::cout << "Looking up values for typology '" << typology << "':\n";
        lookup.SetValue(input_names[0], typology);
        lookup.GetValue(output_names[0], (char*)&n_value);
        lookup.GetValue(output_names[1], (char*)&p_value);
        std::cout << "\tValue for " << output_names[0] << "\t: " << n_value << "\n";
        std::cout << "\tValue for " << output_names[1] << "\t: " << p_value << "\n";
    }


    /*
        Method 2: Retrieving the whole table at once.
    */
    {
        std::cout << "\nMethod 2: Retrieving whole table at once\n";

        double** table;
        int size;

        lookup.GetVarSize("table", &size);

        // Deriving number of substances and typologies
        // TODO: expose these through BMI variables (e.g. rows & header labels)
        int nb_of_substances = number_of_outputs - 1;
        int nb_of_typologies = size / nb_of_substances;

        table = new double*[nb_of_typologies];
        table[0] = new double[nb_of_typologies * nb_of_substances];
        for (int i=1; i<nb_of_typologies; i++)
        {
            table[i] = table[i-1] + nb_of_substances;
        }

        lookup.GetValue("table", (char*)table[0]);

        std::cout << "\t\tN\tP\n";
        std::cout << "\ttypology_1\t" << table[0][0] << "\t" << table[0][1] << "\n";
        std::cout << "\ttypology_2\t" << table[1][0] << "\t" << table[1][1] << "\n";
        std::cout << "\ttypology_3\t" << table[2][0] << "\t" << table[2][1] << "\n";
        std::cout << "Done.\n";

        delete[] table[0];
        delete[] table;
    }
}
