# Passing arrays between programs with DIMR and data files

ArrayBuilder and ArrayEcho are simple BMI-Compliant (mostly) programs that build and then display a 2D array.

ArrayBuilder creates an array using dimensions given in a yaml configuration file. The values in the array are updated at each time step. The program also reads a brief message from the configuration file and reverses its order at each time step.  At the end of each time step, the array and the message (in its current order) are written to a data file also named in the configuration file.

ArrayEcho also reads array dimensions and the name of a data file from its configuration file. At each of its time steps, ArrayEcho reads the message and the array values from the data file.

The configuration and execution of the two programs is controlled by DIMR, according to a configuration in an XML file named on the DIMR command line.

Both programs are written in C++.

## Toolchain

ArrayBuilder and ArrayEcho are compiled and linked using CMake as build tool.

The `builds` directory contains scripts that run CMake on Windows. Copy `settings.bat` to `_settings.bat` and edit it to reflect your systems paths.

Currently, only MSVC compilers are supported. The script `build_msvc.bat` will generate a Visual Studio `*.sln` which you can then open and compile with Visual Studio. The resulting DLL files are buried under the builds direcrory. Running `install_msc.bat` copies them to the `lib` directory, so that they can be run with DIMR from the `dimr_runner` directory.

## CMake

CMake can be downloaded from https://cmake.org/download/.
