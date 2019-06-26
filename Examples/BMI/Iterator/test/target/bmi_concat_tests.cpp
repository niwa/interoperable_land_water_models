#include <cstring>
#include <fstream>
#include <string>

#include "catch.hpp"
#include "bmi.h"


SCENARIO("Adding integers") {

    GIVEN("An initialized bmi_concat instance") {

        CHECK(initialize(NULL) == 0);

        WHEN("Setting inputs") {
            std::string a = "input-";
            double b = 1.23;
            set_var("input_str", (void*) a.c_str());
            set_var("input_val", (void*) &b);

            THEN("Update populates outputs") {
                // bmi_concat's update doesn't use the timestep
                // so just passing 0 here
                CHECK(update(0) == 0);

                // output_str should contain both inputs concatenated
                // into a single string
                char* c;
                get_var("output_str", (void**) &c);
                CHECK(std::string {c} == "input-1.23");

                // output_len should be set to the length of output_str
                int* d;
                get_var("output_len", (void**) &d);
                CHECK(*d == 6 + 4);
            }
        }
    }
}
