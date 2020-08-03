# Seq1-SSM: Sequence 1, Steady State Models
This is a simple loosely coupled set of 2 models

* The Lookup model to estimate Nitrate loads per ecotope
* The aggregator model to average the Nitrate loads to catchments from ecotopes

## Run from DIMR
    > .\run_dimr.bat lookup-aggregatorDimrConfig.xml

If the environment settings are correct (Use C:\Models\Programs\ModelsEnv.bat to set.) DIMR can be run directly without using the batch file. This is convenient for setting command-line logging options, for example.

    > dimr -d 0 -l dimr.log lookup-aggregatorDimrConfig.xml
The example above sets debug level to 0 for maximum output, and writes to the log file "dimr.log."

## Attempt to Run from Iron Python
Attempted to create an Iron Python script to wrap each of the two models with a Delta Shell BasicModelInterfaceLibrary object and run them in sequence. Created a script 'Seq1.py' to do this, intending to run it as

    > DeltaShell.console -f Seq1.py
This attempt failed because DeltaShell can't support the use of two BasicModelInterfaceLibrary objects at the same time. This is explained in the emails between Deltares and Tom Evans, which can be found as PDFs in the 'DeltaresEmails' folder here.

This was the turning point in our decision to use DIMR to run model sequences instead of scripting them in Iron Python.
