# How to Run the DIMR Example in Delta Shell

## Launching Delta Shell
DIMR and the models contained in the example script embedded in dimr.dsproj depend on environment and path settings (especially for Python) that are different from the system defaults.

To have the correct environment settings available to Delta Shell, you can open a command terminal and run the script "C:\Models\Programs\ModelsEnv.bat" to make the model libraries and Python modules available. Launch Delta Shell from the terminal using the command

    > C:\Models\Programs\DeltaShell\1.3.0.40607\DeltaShell\DeltaShell.Gui.exe.

## Running the DIMR Project
From the Delta Shell window, open the project named dimr.dsproj in this directory. A python script to run the steady-state model sequence is embedded in the project. To run it, double click on the script (run_SS) to open it in the main panel. Click "Run Script" on the ribbon to activate the script.

## A Flaw in the DIMR Process

The script runs once, but crashes Delta Shell if it's run a second time. Delta Shell can clear cached variables following a run, and I tested the effect of doing that prior to the second run. Delta Shell crashed anyway. The crash seems to happen during the call to the initialize function of the SparrowRouter Python script. I added a log write to that function, and it doesn't create the log file as it should. I think this means that the initialize function isn't being called successfully from the DLL.

- The program immediately before Sparrow in the DIMR config is also a Python script called from a C++ DLL, and it works OK.
- Sparrow uses numpy, which can have memory problems when loaded, unloaded, and reloaded. However, the crash happens before any numpy methods are called, and the Python interpreter was shut down by the previous DIMR run, so the second run should have a fresh interpreter to work with.
- The PATH and PYTHONPATH environment variables are altered by the IronPython script. Repeated runs may lead to values like "C:\dev\Scripts;C:\dev\Scripts;" which are ugly but shouldn't be harmful.
    - I added a few lines to restore the PATH and PYTHONPATH variables to their original values at the end of the script. That made no difference.
- I have fiddled with a variety of logging methods and test configurations. Based on logging messages I've inserted in the initialize function of the bmi_sparrow library, it seems that the library is crashing as it's loading the SparrowRouter module during the second run. I suspect that the numpy module is responsible for this.
    - https://pybind11.readthedocs.io/en/master/advanced/embedding.html#interpreter-lifetime
- An alternative hypothesis: SparrowRouter.py uses global variables for state shared among the initialize, update, and finalize functions. The other python models embed these functions in an object, which is invoked from C++ code. Using an object, which can be garbage-collected as it goes out of scope, may be the better approach.
