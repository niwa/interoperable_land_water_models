The C++ code in bmi_PySwitch compiles to bmi_PySwitch.dll in Windows -- bmi_PySwitch.so in \*nix.
The result is library that minimally implements BMI (OE version 1.0) and can be run from DIMR to switch a Python interpreter on or off.
[This page in the wiki](https://github.com/niwa/interoperable_land_water_models/wiki/Wrapping-Python-Models-for-Running-in-DIMR) explains what this is for.

The library doesn't actually load a configuration file in its `initialize(char*)` function. Instead of naming a configuration file, 
the argument provides an instruction: either "start" or "shutdown".
