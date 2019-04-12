#include <cstring>
#include <fstream>

#include "catch.hpp"
#include "bmi_summer.h"


SCENARIO("Adding integers") {

    GIVEN("An initialized bmi_summer instance") {

        CHECK(initialize(NULL) == 0);

        THEN("Numbers can be added") {
            int a = 1, b = 10;
            set_value("input_a", (char*) &a);
            set_value("input_b", (char*) &b);

            int out = -1;
            get_value("output_c", (char*) &out);
            CHECK(out == a + b);
        }
    }
}
