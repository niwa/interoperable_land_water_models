## Groundwater BMI

Author:   Mike Toews, GNS Science  
Date:     August 2019  

### Dependencies

* A Python interpreter -- currently set up to use `python36.dll`
* The Python environment must have PyYAML, flopy, netCDF4 and dependencies

### Testing Aparima case study

Build the GroundwaterBMI solution, and then:

Optional:

    set PYTHONHOME=C:\Program Files (x86)\Microsoft Visual Studio\Shared\Python36_64
    set PATH=%PATH%;%PYTHONHOME%

And then:

    cd misc
    bmi-runner.exe ..\x64\Release\GroundwaterBMI.dll aparima.yaml

or:

    python ../Test/bmi-runner.py x64/Release/GroundwaterBMI.dll misc/aparima.yaml

The full Aparima groundwater model is located in a separate repository, however the Python BMI interface is located in the `Aparima` subdirectory here. See `Aparima/put/run.py` for the relevant Python BMI interface called by GroundwaterBMI.
