#include <string>
#include <array>

namespace Typology
{
    class Lookup
    {
    public:
        Lookup();
        ~Lookup();

        int GetNumberOfTypologies() {return nbTypologies;};
        int GetNumberOfSubstances() {return nbSubstances;};

        /* Get load in kg/ha/yr.

           Returns load for given typology and substance or -999 if no match.
        */
        double GetLoad(std::string typology, std::string substance);


        /* Get full data table.

           Table rows represent typologies; columns represent substances.
           Copies table values to preallocated double*.
           Size of values should be at least [nbTypologies][nbSubstances].
        */
        void GetTable(double** values);

        double** GetTablePtr(){return this->table;};

    private:
        static int constexpr nbTypologies = 3;
        static int constexpr nbSubstances = 2;
        std::string typologies[nbTypologies] {"typo_1", "typo_2", "typo_3"};
        std::string substances[nbSubstances] {"N", "P"};
        double** table;
    };
}
