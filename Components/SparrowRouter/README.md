# A Steady-State Contaminant Routing Model implemented in Python

Takes input fluxes to stream segments in a drainage network, and accumulates the flux down the network with decay.
The inputs are read in from a sql database containing segment information, as described in a configuration file, and outputs are placed into a further sql database. A BMI interface is provided for running the model, but input and output are file-based rather than through memory mapping.

The model is written in Python with a wrapper written in C++ to implement BMI.

## Pybind11

The C++ wrapper makes use of the Pybind11 library. See [this link](https://pybind11.readthedocs.io/en/stable/basics.html) for more.

## Toolchain

The BMI wrapper is built using CMake as build tool.

The `builds` directory contains scripts that run CMake on Windows. Copy `settings.bat` to `_settings.bat` and edit it to reflect your systems paths.

Currently, only MSVC compilers are supported. The script `build_msvc.bat` will generate a Visual Studio `*.sln` which you can then open and compile with Visual Studio. The resulting DLL files are buried under the builds direcrory. Running `install_msc.bat` copies them to the `lib` directory, so that they can be run with DIMR from the `dimr_runner` directory.

## CMake

CMake can be downloaded [here](https://cmake.org/download/).
