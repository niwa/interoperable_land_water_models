A simple BMI example built around the Muskingum routing method

Contents:

* ChowEtAl-8.4.pdf -- Muskingum routing section from *Applied Hydrology* textbook. Includes an example problem.
* **libMuskingum** -- a static library that implements a Muskingum routing compute engine.
* **RoutingApp** -- a console application that does Muskingum routing from hand-entered inputs.
* **MuskingumBMI** -- a dynamic library that puts a (sort of) BMI-compliant wrapper around libMuskingum
* **include** -- holds headers for external packages used here (yaml.h)
* **lib** -- compiled libraries from other sources (yaml, again)
* **BMIRoutingApp** -- a console app that runs the Muskingum compute engine using BMI calls

