An example of using a web service to produce time-series data in response to a request from a Delta Shell application.

This time, the folder only contains the C# source, so it's much smaller than the BMI example posted in Examples/BMI/Muskingum. To make a running example, we'll need to do some work, but this is probably more portable than the BMI example. If you've already gone through the basic Delta Shell [plugin tutorial](https://publicwiki.deltares.nl/display/TOOLS/Create+a+new+Delta+Shell+plugin), much of the setup will already be in place. You can think of this as the next step after the tutorial.

1. Make sure that you have Visual Studio and the [NuGet packages for Delta Shell](https://github.com/niwa/interoperable_land_water_models/tree/master/Examples/DeltaShell/DeltaresNuGetPackages/1.3.0.40607/download_me_from.txt) on your computer. 
2. Make sure that you have a copy of this repository (or at least this folder) on your computer.
3. Launch Visual Studio, and from the **File** menu select **New => Project from Existing Code...**. Make the following selections as you go through the Wizard:
  * Project Type = Visual C#
  * Directory = \<path-to-Examples\>\DeltaShell\WebService\NiwaWebService\DeltaShell.Plugins.NiwaWebService (including sub-folders)
  * Project Name = DeltaShell.Plugins.NiwaWebService
  * Output Type = Class Library
4. Open the project properties editor. Set the assembly name to "DeltaShell.Plugins.NiwaWebService" and set the target framework to ".Net Framework 4.6.1"
5. In the Solution Explorer, select your project, then use the context menu to select "Manage NuGet Packages". Pick **DeltaShellApplicationPlugin** and install it in your project. Use the new package that Christophe made available (version 1.3.0.40607) if you can. This will also load log4net, Mono.Addins and PostSharp components, as well as the DeltaShell framework and application plugin packages. If you're not sure how to manage the NuGet packages, follow the example in the Delta Shell [plugin tutorial](https://publicwiki.deltares.nl/display/TOOLS/Create+a+new+Delta+Shell+plugin) using the instructions for a local installation of the NuGet packages.
6. Reload the solution when prompted.
7. Remove PostSharp references from the project, following the example in the tutorial if you haven't done this before. Short version: remove PostSharp from the References branch in the Solution Explorer tree, remove the reference to PostSharp from the packages.config file, and remove references to PostSharp from the Visual Studio project. For the last one, you'll have to unload the project from the solution, edit its configuration file and reload the project.
8. Build the project. The build will fail, but it will cause a bunch of libraries to be copied into the project's home folder (the one named "DeltaShell.Plugins.NiwaWebService"). 
9. In the solution explorer, under the **NiwaWebService** project, add references to these assemblies from the folder packages\DeltaShell.Framework.1.3.0.40607\lib\net40\DeltaShell:
  * DelftTools.Shell.Gui
  * DelftTools.Fuctions
  * DelftTools.Units
  * DelftTools.Utils  
Add this reference from system:
  * System.Drawing
Make sure that the "Copy Local" property is set to "False" of all the references that you've added
10. In the project properties editor, select the "Debug" options and set the "Start Action" to "Start with external program" and browse to the project's "bin" directory. Go two folders deeper into Debug\DeltaShell and select "DeltaShellGui.exe" as the starting program.
11. Rebuild the project and start the debugger. If you feel brave, you can just hit the "Start" icon on the toolbar.

  
