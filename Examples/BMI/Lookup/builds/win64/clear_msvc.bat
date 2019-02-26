@echo off

set root=%~dp0
rmdir /Q/S %root%build_msvc\
rmdir /Q/S %root%bin_msvc\
