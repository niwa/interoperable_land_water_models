@echo off

echo Building for win64

rem Directories
set root=%~dp0
set build_dir=%root%build
set bin_dir=%root%bin_msvc
set source_dir=%root%..\..\src

rem Load paths from local settings.bat
call %root%\_settings.bat

rem Output dir
set build_dir=%build_dir%_msvc


echo Build directory: %build_dir%
echo Bin directory: %bin_dir%
echo Source directory: %source_dir%
echo ------------------------------------------------------------

mkdir %build_dir%
mkdir %bin_dir%
cd %build_dir%

%cmake% -G "Visual Studio 15 2017 Win64" %source_dir% ^
		-DCMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=TRUE ^
		-DBUILD_SHARED_LIBS=TRUE ^
        -DCMAKE_RUNTIME_OUTPUT_DIRECTORY=%bin_dir%

pause
