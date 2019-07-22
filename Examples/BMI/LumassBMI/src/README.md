## LumassBMITest application

** NOTE: Alternatively, you can use the version shipped with LUMASS. **
Jump straight to [Ecotopes](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/Ecotopes)!

### Build the LumassBMITest application
The LumassBMITest application (executable) is a sample
application that uses the BMI compliant LumassBMI.<so | dll>
library to run an arbitrary LUMASS model or optimisation
scenario.  

* Build instructions for Windows *
 
1. Copy the content of this directory to different directory
   of your choice, e.g. D:\Temp\LumassBMITest.
2. Open a Windows CommandPrompt and change into the build directory.
3. Configure the test application. Adapt your the -G parameter to select
    a suitable msvc compiler version.
	```
	D:\Temp\LumassBMITest\> cmake -S . -B . -G "Visual Studio 14 2015 Win64"
	```
4. Build the application. Note msbuild needs to be in your PATH!
	```
	D:\Temp\LumassBMITest\> msbuild LumassBMITest.sln -p:Configuration=RelWithDebInfo
	```
5. Test the whether the application was built correctly.
	```
	D:\Temp\LumassBMITest\>RelWithDebInfo\LumassBMITest.exe 
	Usage: LumassBMITest <LUMASS BMI library path> <yaml configuration file>
	Init of	
	```
	Note: We show how to properly invoke a model in section [Ecotopes](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/BMI/LumassBMI/Ecotopes).
	
	


