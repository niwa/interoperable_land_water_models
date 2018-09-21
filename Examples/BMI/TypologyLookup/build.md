# Toolchain

The Typology Lookup uses CMake as build tool.

The `builds` directory contains a number of scripts to run CMake on Linus or Windows. Copy `settings.bat` to `_settings.bat` and edit it to reflect your systems paths.

On Windows you can choose between MSVC or GCC compilers. Use `build.bat` for GCC (MinGW) and `build_msvc` for MSVC. The latter will generate a Visual Studio `*.sln` which you can then open and compile with Visual Studio.

## CMake

CMake can be downloaded from https://cmake.org/download/.

## MinGW

MinGW is needed to use GCC on Windows.

Download and install from [Sourceforge](https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/installer/mingw-w64-install.exe/download). For more info on the project, visit http://mingw-w64.org.

During installation, choose the following settings:

- Architecture: x86_64
- Threads: win32
- Exception: seh

