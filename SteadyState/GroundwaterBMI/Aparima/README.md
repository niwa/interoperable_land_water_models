# Aparima
MODFLOW/MT3D groundwater model for Aparima, Southland, New Zealand.

This repo is similar to one prepared for Environment Southland, except some model data has been removed. This model is
not calibrated, and may only be used for research purposes.

## Background
Model initially build 18/04/2019, as steady-state, 2-layer MODFLOW-NWT,
built in flopy. Extended as a transient transport model.

# Basic Model Interface (BMI)
A BMI was implemented in Python (done here), used through a generic C++ wrapper, [GroundwaterBMI](https://github.com/niwa/interoperable_land_water_models/tree/master/SteadyState/GroundwaterBMI).

## Config file
For example, `aparima.yaml`
```yaml
---
run_dir: c:\Models\data\aparima-groundwater\run
args:
  timeword: steady
  common_dir: c:\Models\data\common
```
Other keyword options may also be used beneath `args`.

## Options
The following keyword options can be used for the `GroundwaterModel` constructor, or in the YAML config file.
* `timeword`: use either `steady` or `dynamic` time modes
* `run_dir`: internal simulation run directory with MODFLOW files
* `common_dir`: common directory with exchange files read/written for BMI run
* `nc_fname`: output a new netCDF file name
* `sqlite_fname`: output catchment info to a 'groundwater' table in a SQLite file
* `luci_nc_fname`: input LUCI netCDF file name
* `verbose`: 0 only errors/warnings, 1 (default) some info, 2 lots of info

To use these options with the command line interface, convert (e.g.) `run_dir = foodir` to `--sim-dir=foodir`.

## Run model

### From stand-alone Python console interface

    $ cd run
    $ python run.py --help
    $ python run.py --timeword=steady --verbose=0

### From interactive Python session

In a shell: `cd run` then use either `ipython` or `python`:
```python
import os
run_dir = r'c:\Models\data\aparima-groundwater\run'
os.chdir(run_dir)

import run

gm = run.GroundwaterModel('dynamic')
gm.update(1.0)  # 1 hour
gm.update(24.0)  # 1 day
gm.finalize()

# Or, initialize from YAML config file
gm = run.GroundwaterModel.initialize('/path/to/config.yaml')
```

### Using Deltares' BMI runner

    $ set PATH=C:\Program Files (x86)\Microsoft Visual Studio\Shared\Python36_64;%PATH%
    $ bmi-runner.exe GroundwaterBMI.dll misc\aparima.yaml

### Using Python BMI runner

    $ python bmi-runner.py GroundwaterBMI.dll misc\aparima.yaml

## Copying simulation files

This step is useful to isolate git-tracked files in `put` from any changes made while running the model. A copy of files are made in `run`, which is `run_dir` (or `--run-dir`) defined above.

### From \*NIX

    $ mkdir run
    $ cd run
    $ rsync -avh --delete ../put/ .

### From Windows

    > mkdir run
    > cd run
    > robocopy /MIR ../put .
