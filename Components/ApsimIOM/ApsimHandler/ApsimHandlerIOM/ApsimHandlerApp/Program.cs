using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Collections.Generic;
using ApsimHandlerIOM;

/// <summary></summary>
namespace ApsimHandlerApp
{
    /// <summary>
    /// The ApsimHandlerApp is an entry to call the MainHandler library, which will modify and run apsim simulations.
    /// </summary>
    class Program
    {
        /// <summary>Main entry to the program.</summary>
        /// <param name="args">List of arguments passed to the program.</param>
        /// <remarks>
        /// Expects two arguments:
        ///  - the full path to the yaml file with the configuration for the main ApsimHandler;
        ///  - the path to the folder where outputs will be saved (optional);
        /// </remarks>
        static void Main(string[] args)
        {
            // basic check of arguments
            if (args.Length == 0)
            {
                Console.WriteLine("  ApsimHandlerApp");
                Console.WriteLine("    Modifies a base Apsim simulation using a provided parameter set and runs the simulation");
                Console.WriteLine("     Arguments: ");
                Console.WriteLine("       - SettingsFile, a yaml file with path to the base simulation and the set of parameters to change");
                Console.WriteLine("       - OutputFolder (optional), a folder where the modified simulation will be saved and run");
                Console.WriteLine("          If no folder is given, the same as the yaml file will be used");
            }
            else
            {
                bool allowFeedBack = false;

                // initialise the main handler
                string configFilePath = args[0];
                string outputsPath = "";
                if (args.Length > 1)
                    outputsPath = args[1] + "\\";
                MainHandler taskHandler = new MainHandler(configFilePath, outputsPath, allowFeedBack);

                // check that all basic info is good
                if (taskHandler.InitialisationSuccesful)
                {
                    // trigger the creation and run the simulations
                    bool processedOK = false;
                    string mySimRootName = "myTestSimulation";
                    int testType = 4;

                    if (testType == 1)
                    { // create, run, and save outputs of one simulation
                        taskHandler.PrepareWeatherData();
                        taskHandler.PrepareSoilData();

                        processedOK = taskHandler.CreateApsimSimulation(mySimRootName + ".apsimx", "None", "None", "SomeWhere", "Test1", allowFeedBack);
                        if (processedOK)
                        {
                            processedOK = taskHandler.RunApsimSimulation(mySimRootName + ".apsimx", allowFeedBack);
                            if (processedOK)
                            {
                                taskHandler.SaveOutputsTable("myTestSimulation.db", "Report", "myOutputTest1.nc");
                            }
                        }
                        else
                        {
                            Console.WriteLine(" Simulation was not created!)");
                        }
                    }
                    else if (testType == 2)
                    { // create and run simulations for all locations defined in settings file. And save the outputs.
                        taskHandler.PrepareWeatherData();
                        taskHandler.PrepareSoilData();
                        string[] locationNames = taskHandler.GetLocationsNames().ToArray();
                        string[] clockSettingNames = taskHandler.GetClockSettingNames().ToArray();
                        string[] systemsNames = taskHandler.GetFarmingSystemsNames().ToArray();
                        taskHandler.RunSimulationsAndSaveOutputs(systemsNames[0], clockSettingNames[0], locationNames, mySimRootName, "Report", "myOutputTest2.nc", "ID", allowFeedBack);
                    }
                    else if (testType == 3)
                    { // Add new location and clock, run simulations and save outputs
                        taskHandler.PrepareWeatherData();
                        taskHandler.PrepareSoilData();
                        string newLocName = "SameWhere";
                        processedOK = taskHandler.AddLocationSetup(newLocName, -46.025f, 168.125f, "Pukemutu_6a1");
                        if (processedOK)
                        {
                            processedOK = taskHandler.PrepareSoilData(newLocName);
                            if (processedOK)
                            {
                                processedOK = taskHandler.PrepareWeatherData(newLocName);
                                if (processedOK)
                                {
                                    processedOK = taskHandler.AddClockSetup("SomeTest", "2000-01-01", "2001-06-30");
                                    if (processedOK)
                                    {
                                        string[] locationNames = taskHandler.GetLocationsNames().ToArray();
                                        string[] clockSettingNames = taskHandler.GetClockSettingNames().ToArray();
                                        string[] systemsNames = taskHandler.GetFarmingSystemsNames().ToArray();
                                        processedOK = taskHandler.RunSimulationsAndSaveOutputs(systemsNames[0], clockSettingNames[1], locationNames, mySimRootName, "Report", "myOutputTest3.nc", "LL", allowFeedBack);
                                    }
                                }
                            }
                        }
                    }
                    else if (testType == 4)
                    { // , run simulations and save outputs

                        //read sims table
                        processedOK = taskHandler.ReadSimulationTable(outputsPath+"Ecotopes14Oct.csv");
                        if (processedOK)
                        {
                            processedOK = taskHandler.RunSimulationsAndSaveOutputs(null, mySimRootName, "Report", "myOutputTest4.nc", "ID", allowFeedBack);
                        }
                    }

                    Console.Write("press any key...");
                    Console.ReadLine();
                }
                else
                {
                    Console.WriteLine("    MainHandler was not able to complete the task");
                    Console.ReadLine();
                }
            }
        }
    }
}
