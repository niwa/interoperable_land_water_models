# Seq2-SSM: Sequence 2, Steady State Models
Like Sequence 1, this is a loosely coupled set of steady-state models. It's longer than Sequence 1 and better demonstrates how a string of models can represtent multiple influences and processes.

* The Lookup model to estimate Nitrate loads per ecotope
* The aggregator model to average the Nitrate loads to catchments from ecotopes
* The Sparrow Router represents downstream surface transport as a steady-state process
* The groundwater model (modflow and flopy) represents sub-surface transport.

## Run from DIMR
Use C:\Models\Programs\ModelsEnv.bat to set environment variables and Python configuration.

    > dimr AparimaWGWDimrConfig.xml

 Capture logged information with these command-line options.

    > dimr -d 0 -l dimr.log AparimaWGWDimrConfig.xml
