#include <array>
#include <string>
#include <vector>

namespace Typology
{
    class Lookup
    {
    public:
        Lookup();
        ~Lookup();

        int GetNumberOfTypologies() {return nbTypologies;};
        int GetNumberOfSubstances() {return nbSubstances;};


        /* Load lookup table from csv file.

           Csv file expected with ';' separator and header Typology;N;P
        */
        void LoadCsvTable(std::string csvFileName);



        /* Get load in kg/ha/yr.

           Returns load for given typology and substance or -999 if no match.
        */
        double GetValue(std::string typology, std::string substance);


        /* Get full data table.

           Table rows represent typologies; columns represent substances.
           Copies table values to preallocated double*.
           Size of values should be at least [nbTypologies][nbSubstances].
        */
        void GetTable(double** values);

        double** GetTablePtr(){return this->table;};

    private:
        /* Drop all previously loaded data.

        */
        void Clear();

    private:
        int nbTypologies = 0;
        static int constexpr nbSubstances = 2;
        std::vector<std::string> typologies;
        std::string substances[nbSubstances] {"N", "P"};
        double** table;
    };
}
