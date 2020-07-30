# Soil Moisture Accounting model
This Soil Moisture Accounting (SMA) repository contains code to be used in the interoperability project. It is structured as follows:

* C++ wrapper files (bmi folder)
* Python files containing the SMA model

The main C++ file is SMA\bmi\models\cpp\model-sma.cpp.
The main python file is SMA\rainfall_runoff.py.

The bmi folder contains the bmi-runner.exe.
The SMA dll can be found at SMA\bmi\models\vs2013\x64\Debug\model-cpp-sma.dll.
The config file is SMA\config.yaml and contains paths to the following netCDF files:
* in_netCDF: This is a netCDF file which contains climate and soil input data for the area of interest
* out_netCDF_template: A netCDF file which contains the correct structure (i.e. variables and dimensions) that will be used in the output netCDF file
* out_netCDF: The output netCDF file, which is created when the model runs

## Running the model
To run the model, open a command prompt and navigate to the SMA folder. Then run the following command:

     <path to SMA>\bmi\bmi-runner.exe <path to SMA>\bmi\models\vs2013\x64\Debug\model-cpp-sma.dll <path to SMA>\config.yaml

     e.g.

      C:\GitRepos\bmi\bmi-runner.exe C:\GitRepos\sma\bmi\models\vs2013\x64\Debug\model-cpp-sma.dll C:\GitRepos\SMA\config.yaml

It should produce the following output to the screen:

    Info: Logging attached to cxx model
    Info: initializing with config.yaml
    Debug: updating from 0 with dt: 1
    Debug: updating from 1 with dt: 1
    Debug: updating from 2 with dt: 1
    Debug: updating from 3 with dt: 1
    Debug: updating from 4 with dt: 1
    Debug: updating from 5 with dt: 1
    Debug: updating from 6 with dt: 1
    Debug: updating from 7 with dt: 1
    Debug: updating from 8 with dt: 1
    Debug: updating from 9 with dt: 1
    Debug: updating from 10 with dt: 1
    3.7091755188273936

The model reads in the input netCDF file and outputs to the output netCDF file after each time step.

For testing purposes, it's possible to run the model without using the C++ wrapper. To do this, run the following command:

    <path to Visual Studio Python>\python.exe" <path to SMA>\rainfall_runoff.py <path to SMA>\config.yaml

    e.g.

    "C:\Program Files (x86)\Microsoft Visual Studio\Shared\Python36_64\python.exe" c:\gitrepos\sma\rainfall_runoff.py C:\gitrepos\sma\config.yaml

## Viewing netCDF files
These are best opened and viewed in Panoply. NetCDFViewer is another option but it can only display the structure of the file and values for 1 dimensional variables, but can't do 2D.
