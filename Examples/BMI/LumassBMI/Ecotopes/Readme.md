## Ecotopes

### Contents

- **MakeEcotopes** -- Folder containing the LUMASS model files.

### Setup and configuration

0. Build the [LumassBMITest](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/src) application. Alternatively, you can use the version that is shipped with LUMASS, i.e. `test\LumassBMIText.exe` (s. link below).  

1. Download LUMASS`ftp://ftp.landcareresearch.co.nz/HerzigA/lumass-0.9.60-test.zip` 
   and extract the zip file into a directory of your choice, e.g. `C:\opt`. Note 
   the zip file contains the directory `lumass-0.9.60-test`.

2. Create the following directories in a folder of your choice, e.g. `D:\Temp\IOMTest`,
	- **Data** -- Folder for input files of the model.
	- **MakeEcotopes** -- Folder containing the LUMASS model files (copy content from above folder)
	- **working_directory** -- Folder for intermediary model files.

 
3. Download 
	- the input data `ftp://ftp.landcareresearch.co.nz/HerzigA/Data.zip` for the pre-configured model and extract the individual files into `D:\Temp\IOMTest\Data`
	- the LUMASS model files from [MakeEcotopes](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/Ecotopes/MakeEcotopes) into `D:\Temp\IOMTest\MakeEcotopes`.

	to create the following file structure
	
	```
	D:
	|-\Temp
	     |-\IOMTest
				|-\Data
					|-\ <copy  data from Data.zip here, s. below>
				|-\MakeEcotopes
					|-\ <copy LUMASS model files from  github repo here>
				|-\working_directory
	```
  
 
  
4. Configure the [MakeEcotopes](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/Ecotopes/MakeEcotopes) model to reflect your datasets and parameters. Note: To run the model with the provided datasets, you only have to adjust the `MakeEcotopes\MakeEcotopes.yaml` file.

5. Edit the `MakeEcotopes\MakeEcotopes.yaml` file:
		
	- adapt the `ParameterTable` parameter to the location
	  of the model repo, e.g. 
	  ```
	  D:/Temp/IOMTest/MakeEcotopes/EcotopesParas.xls
	  ```
		
	- adapt the `DataPath` parameter to point to the   
	  subdirectory containing the model input data; this 
	  is also where the ouput data is going to be stored
	  
	  ```
	  D:/Temp/IOMTest/Data
	  ```

### Run the MakeEcotopes model using the LumassBMITest application

*Instructions for Windows*
	  
1. 	WINDOWS ONLY: Open a Windows CommandPrompt and run
	```
	D:\Temp\IOMTest>C:\opt\lumass-0.9.60-test\test\LumassEnv.cmd 
	```
	This resets the PATH environment variable to include LUMASS and its 
	dependencies. Note that after running the command all other PATH settings
	are lost. This to make sure that there are no potential version conflicts
	with other libraries in the path that may overlap.
	
2.	Run the MakeEcotopes model: 
	```
	D:\Temp\IOMTest>D:\Temp\LumassBMITest\RelWithDebInfo\LumassBMITest.exe C:\opt\lumass-0.9.60-test\bin\LumassBMI.dll D:\Temp\IOMTest\MakeEcotopes\MakeEcotopes.yaml
	--INFO-- LumassBMI is now connected to BMI log function!
	--INFO-- LumassBMI: initialising...
	--INFO-- LUMASS Engine - Mon Jul 22 2019, 19:27:43

	--INFO-- Loading model 'D:/Temp/IOMTest/MakeEcotopes/MakeEcotopes.lmx' ...
	--INFO-- LumassBMI: intitialisation complete!
	--INFO-- running model (mode=1) for time step 0	
	...
	```


	  

