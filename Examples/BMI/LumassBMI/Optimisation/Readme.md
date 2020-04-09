## Optimisation

### Contents

- **AparimaOptimisation** -- Folder containing the sample optimisation data and LUMASS optimisation settings files (optimistion scenarios).

### Setup and configuration

0. Build the [LumassBMITest](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/src) application. Alternatively, you can use the version that is shipped with LUMASS, i.e. `test\LumassBMIText.exe` (s. link below).  

1. Download [LUMASS](https://bitbucket.org/landcareresearch/lumass/downloads/lumass-0.9.62.zip) and extract the zip file into a directory of your choice, e.g. `C:\opt`. Note that the zip file contains the directory `lumass-0.9.62`.

2. Download the Apariama data set and the LUMASS optimisation settings files from [AparimaOptimisation](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/Optimisation/AparimaOptimisation) into `D:\Temp\IOMTest` to create the following file structure
	
	```
	D:
	|-\Temp
	    |-\IOMTest
			|-\AparimaOptimisation
				|-\AparimaBatchScenarios
				|-\los_files
				|-\AparimaOptimisation.yaml
				|-\combParcLU.img
				|-\combParcLU.img.aux.xml
				|-\combParcLU.ldb
			...			
	```
	Note: The optimisation scenarios can also be run from within the repository or any other folder, provided that the [AparimaOptimisation\AparimaOptimisation.yaml](AparimaOptimisation/AparimaOptimisation.yaml) file is adjusted accordingly. 
 
  
3. Configure the [AparimaOptimisation.yaml](AparimaOptimisation/AparimaOptimisation.yaml) file to reflect your path settings.

	Relevant `AparimaOptimisation.yaml` file settings:
    - mode: "moso"
    - enginepath: "C:/opt/lumass-0.9.62/bin"
	
	  This is the path to the LUMASS folder containing the `LumassBMI.dll`, i.e. the BMI-compliant LUMASS engine library.
	
    - modelfile: "D:/Temp/IOMTest/AparimaOptimisation/los_files/Aparima_minNLeach.los"
    
	  The modelfile parameter specifies the LUMASS optimisation settings file (*.los) that defines the particular optimisation scenario to be run. Please refer to the
	  LUMASS [OptimisationHowTo](https://bitbucket.org/landcareresearch/lumass/downloads/OptimisationHowTo_1.2.zip)
	  for a reference description of this file.
	  
### Run a LUMASS optimsation scenario using the LumassBMITest application

*Instructions for Windows*
	  
1. Open a Windows CommandPrompt (type `cmd` in the Windows search bar to get a command prompt window) and execute the following command, e.g.
	```
	D:\Temp\IOMTest>C:\opt\lumass-0.9.62\test\LumassEnv.cmd 
	```
	**Note**: This resets the `PATH` environment variable to include LUMASS and its 
	dependencies. After running the command all other `PATH` settings
	are lost for that command prompt window. This is to make sure that there are no potential version conflicts
	with other libraries in the `PATH` that may overlap. System wide `PATH` settings are not affected by running the `LumassEnv.cmd` command file.
	
2.	Run the [Aparima_minNLeach.los](AparimaOptimisation\los_files\Aparima_minNLeach.los) optimisation scenario: 
	```
	D:\Temp\IOMTest>C:\opt\lumass-0.9.62\test\LumassBMITest.exe C:\opt\lumass-0.9.62\bin\LumassBMI.dll D:\Temp\IOMTest\AparimaOptimisation\AparimaOptimisation.yaml
	--INFO-- LumassBMI is now connected to BMI log function!
	--INFO-- LumassBMI: initialising...
	--INFO-- LUMASS Engine - Wed Jan 29 2020, 13:13:57
	--DEBUG-- Looking for YAML-based optimisation scenario configuration ...
	--INFO-- Parsing YAML ModelConifg::Settings ...
	--INFO--   ModelConfig::Settings::DataPath=D:/Temp/IOMTest/AparimaOptimisation
	--INFO-- LumassBMI: intitialisation complete!
	--INFO-- running model (mode=2) for time step 0
	Starting run=1

	Model name:  '' - run #1
	Objective:   Maximize(R0)

	SUBMITTED
	Model size:     1424 constraints,    4258 variables,         7271 non-zeros.
	Sets:                                   0 GUB,                  0 SOS.

	Using DUAL simplex for phase 1 and PRIMAL simplex for phase 2.
	The primal and dual simplex pricing strategy set to 'Devex'.

	Found feasibility by dual simplex after          1633 iter.

	Optimal solution        74466677.199 after       1892 iter.

	Relative numeric accuracy ||*|| = 2.69651e-16

 	 MEMO: lp_solve version 5.5.2.5 for 64 bit OS, with 64 bit REAL variables.
	      In the total iteration count 1892, 0 (0.0%) were bound flips.
          There were 10 refactorizations, 0 triggered by time and 0 by density.
	       ... on average 189.2 major pivots per refactorization. 
		  The largest [LUSOL v2.2.1.0] fact(B) had 4205 NZ entries, 1.0x largest 
		  basis. The constraint matrix inf-norm is 786, with a dynamic range of
		  786. Time to load data was 0.098 seconds, presolve used 0.005 seconds,
	       ... 0.302 seconds in simplex solver, in total 0.405 seconds.
	--INFO-- LumassBMI successfully finalised!
	--INFO-- LumassBMITest successfully completed!
	```