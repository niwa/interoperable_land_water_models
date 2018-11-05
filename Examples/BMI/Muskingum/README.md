A simple BMI example built around the Muskingum routing method

## Contents

* ChowEtAl-8.4.pdf -- Muskingum routing section from *Applied Hydrology* textbook. Includes an example problem.
* **libMuskingum** -- a static library that implements a Muskingum routing compute engine.
* **RoutingApp** -- a console application that does Muskingum routing from hand-entered inputs.
* **MuskingumBMI** -- a dynamic library that puts a (sort of) BMI-compliant wrapper around libMuskingum
* **include** -- holds headers for external packages used here (yaml.h)
* **lib** -- compiled libraries (64-bit) from other sources (yaml, again)
* **lib32** -- 32-bit versions of compiled libraries from other sources
* **BMIRoutingApp** -- a console app that runs the Muskingum compute engine using BMI calls

## Notes

For this example, I uploaded the whole kaboodle of C++ source code, Visual Studio Projects and Solutions, intermediate files, and a couple of compiled libraries. I don't think this is a good idea for general practice. Here, I wanted to make every single thing I have done available for scrutiny -- either to set an example or to invite constructive criticism. If you're not using the same Visual Studio version that I am, though, this will be a pain in the neck. (That's VS 2017, Community Edition, v 15.5.6, to be ridiculously precise.)

CSDMS recommends YAML for storing configurations, so the `MuskingumBMI.Initialize(configFile)` method reads the model setup from a YAML file, and uses the standard yaml.dll C library to do it. I didn't include the YAML source, but you can find it [here](https://github.com/yaml/libyaml). I *did* include compiled yaml.lib and yaml.dll files in ./lib and yaml.h in ./include. Because I'm still clumsy with VS and libraries, I wasn't able to get my BMIRoutingApp to link dynamically to the yaml.dll in the ./lib directory, so there's a copy of yaml.dll in the BMIRoutingApp directory itself.

### 2 November 2018
A large number of revisions were uploaded today in light of lessons learned from linking to Delta Shell. The most important of these lessons is that both the C# `BasicModelInterfaceLibrary` class and the DIMR model-running program will link to a BMI-compliant model through the C interface given in the *bmi.h* header in Deltares's OpenEarth repository on GitHub. That's this file [here](https://github.com/openearth/bmi/blob/master/models/include/bmi.h). I modified *MuskingumBMI.h* to include *bmi.h* and implemented the functions named in *bmi.h* in *MuskingumBMI.cpp*, mostly by wrapping the C++ methods I'd written earlier.

Here are the function definitions from *bmi.h*. Note that even where the functions do exactly the same things that their corresponding CSDMS C++ methods do, there are differences in the function signatures. These are the ones that work with Delta Shell.

```C
	/* control functions. These return an error code. */
	BMI_API int initialize(const char *config_file);
	BMI_API int update(double dt);
	BMI_API int finalize();

	/* time control functions */
	BMI_API void get_start_time(double *t);
	BMI_API void get_end_time(double *t);
	BMI_API void get_current_time(double *t);
	BMI_API void get_time_step(double *dt);

	/* variable info */
	BMI_API void get_var_shape(const char *name, int shape[MAXDIMS]);
	BMI_API void get_var_rank(const char *name, int *rank);
	BMI_API void get_var_type(const char *name, char *type);
	BMI_API void get_var_count(int *count);
	BMI_API void get_var_name(int index, char *name);

	/* get a pointer pointer - a reference to a multidimensional array */
	BMI_API void get_var(const char *name, void **ptr);

	/* Set the variable from contiguous memory referenced to by ptr */
	BMI_API void set_var(const char *name, const void *ptr);

	/* Set a slice of the variable from contiguous memory using start / count multi-dimensional indices */
	BMI_API void set_var_slice(const char *name, const int *start, const int *count, const void *ptr);

	/* logger to be set from outside so we can log messages */
	typedef void (CALLCONV *Logger)(Level level, const char *msg);

	/* set logger by setting a pointer to the log function */
	BMI_API void set_logger(Logger logger);
```

The Muskingum example does not implement `set_var_slice` or `set_logger`.

I also switched the compile options in the VS solution from 32-bit to 64-bit. Deltares isn't going to support 32-bit versions of Delta Shell or DIMR much longer. I left the 32-bit yaml library in the repository, but renamed its folder to *lib32*. The *lib* folder now contains 64-bet versions of yaml.dll and yaml.lib.

### 23 August 2018
As of today I believe that the MuskingumBMI DLL correctly implements these methods from the BMI spec:

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
int GetInputVarNameCount();
int GetOutputVarNameCount();
void GetInputVarNames(char * const * const names);
void GetOutputVarNames(char * const * const names);
```

Cheers  
Tom
