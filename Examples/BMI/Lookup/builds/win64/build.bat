@echo off

echo Building for win64

rem Directories
set root=%~dp0
set build_dir=%root%build
set bin_dir=%root%bin
set source_dir=%root%..\..

rem Load paths from local settings.bat
call %root%\_settings.bat

echo Build directory: %build_dir%
echo Bin directory: %bin_dir%
echo Source directory: %source_dir%
echo ------------------------------------------------------------

mkdir %build_dir%
cd %build_dir%

%cmake% -G "MinGW Makefiles" ^
        -D CMAKE_MAKE_PROGRAM=%mingw%/mingw32-make.exe ^
        -D CMAKE_C_COMPILER=%mingw%/gcc.exe ^
        -D CMAKE_CXX_COMPILER=%mingw%/g++.exe ^
        -D MINGW_BIN_DIR=%mingw%^
        %source_dir%

%mingw%\mingw32-make.exe

pause
