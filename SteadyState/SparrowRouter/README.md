# Sparrow Router

A steady-state contaminant routing model for a surface drainage network.

## Contents

### Model
The transport model, prgrammed in Python. Runs in Python 3.6 and requires these modules:
* numpy
* pandas
* yaml
* sqlite3 

### Wrapper
The BMI-compliant wrapper is programmed in C++, using the [Pybind11 library](https://pybind11.readthedocs.io/en/stable/basics.html) to run the Python program. 

#### Toolchain

A set of [CMake](https://cmake.org/download/) files are provided to automate the build configuration.

The `wrapper\builds` directory ~~contains~~ will contain scripts that run CMake on Windows. Copy `settings.bat` to `_settings.bat` and edit it to reflect your systems paths.

The initial CMake build will only support MSVC compilers. The script `build_msvc.bat` will generate a Visual Studio `*.sln` which you can then open with Visual Studio to build the library. The resulting DLL files are buried under the builds direcrory. Running `install_msc.bat` copies them to the `lib` directory, so that they can be run with DIMR from the `dimr_runner` directory.
