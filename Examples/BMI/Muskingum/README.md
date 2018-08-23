A simple BMI example built around the Muskingum routing method

## Contents

* ChowEtAl-8.4.pdf -- Muskingum routing section from *Applied Hydrology* textbook. Includes an example problem.
* **libMuskingum** -- a static library that implements a Muskingum routing compute engine.
* **RoutingApp** -- a console application that does Muskingum routing from hand-entered inputs.
* **MuskingumBMI** -- a dynamic library that puts a (sort of) BMI-compliant wrapper around libMuskingum
* **include** -- holds headers for external packages used here (yaml.h)
* **lib** -- compiled libraries from other sources (yaml, again)
* **BMIRoutingApp** -- a console app that runs the Muskingum compute engine using BMI calls

## Notes

For this example, I uploaded the whole kaboodle of C++ source code, Visual Studio Projects and Solutions, intermediate files, and a couple of compiled libraries. I don't think this is a good idea for general practice. Here, I wanted to make every single thing I have done available for scrutiny -- either to set an example or to invite constructive criticism. If you're not using the same Visual Studio version that I am, though, this will be a pain in the neck. (That's VS 2017, Community Edition, v 15.5.6, to be ridiculously precise.)

CSDMS recommends YAML for storing configurations, so the MuskingumBMI.Initialize(configFile) method reads the model setup from a YAML file, and uses the standard yaml.dll C library to do it. I didn't include the YAML source, but you can find it [here](https://github.com/yaml/libyaml). I *did* include compiled yaml.lib and yaml.dll files in ./lib and yaml.h in ./include. Because I'm still clumsy with VS and libraries, I wasn't able to get my BMIRoutingApp to link dynamically to the yaml.dll in the ./lib directory, so there's a copy of yaml.dll in the BMIRoutingApp directory itself.

As of today (23 August 2018) I believe that the MuskingumBMI DLL correctly implements these methods from the BMI spec:

```C++
// BMI Model Control Functions
void Initialize(const char *config_file);
void Update();
void UpdateUntil(double then);
void Finalize();>

// BMI Time functions
double get_time_step();
std::string get_time_units();
double get_start_time();
double get_current_time();
double get_end_time();
```

In addition, the following methods are implemented, but I suspect that my example isn't using them as CSDMS intended, and they may not work with DIMR, which is, ultimately, the point of this exercise. 

```C++
// BMI Model Information Functions
int GetInputVarNameCount()
int GetOutputVarNameCount()
void GetInputVarNames(char * const * const names);
void GetOutputVarNames(char * const * const names);
```

Cheers
Tom
