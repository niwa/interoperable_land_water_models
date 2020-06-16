using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Collections.Generic;
using Microsoft.Research.Science.Data;
using Microsoft.Research.Science.Data.Imperative;
using YamlDotNet.RepresentationModel;
using Newtonsoft.Json.Linq;
using Newtonsoft.Json;
using System.Data.SQLite;
using ApsimWeatherConverter;
using ApsimSoilConverter;

namespace ApsimHandlerIOM
{
    /// <summary>Methods for creating and running ApsimX simulations.</summary>
    /// <remarks>
    /// This handler supports ApsimX simulations only, with files in json format.
    /// On initialisation this class calls reads a yaml config file which contains the paths to a few required files:
    ///  - the ApsimX executable;
    ///  - a base simulation which is used to create new ones;
    ///  - the list of netcdf files with weather data;
    ///  - a xml file with soil data (formatted for Apsim);
    ///  - a yaml file with the settings used to create new simulations;
    ///    this file may contain the settings for one simulation (simulationSetup), or a list of settings for some farming systems.
    /// The newly created simulation is saved and run in a specified location; the files are deleted after the outputs are collected.
    /// The outputs are collated in a single db file (netcdf format) and saved at the specified location.
    /// </remarks>
    public class MainHandler
    {
        /// <summary>Initialises the handler: gets the config file and checks its content.</summary>
        /// <remarks>
        /// The config file contains the addresses of a series of files that are used by the handler, 
        ///  the existence of these files are tested here; however, their validity is not checked.
        /// If no path is supplied for the outputs the same folder as the config file will be used.</remarks>
        /// <param name="configFileFullPath">Path to the yaml config file.</param>
        /// <param name="outputsPath">Path for the outputs (optional).</param>
        /// <param name="verboseEnabled">Flag whether info about configuration are to be printed in the console (optional).</param>
        public MainHandler(string configFileFullPath, string outputsPath = "", bool verboseEnabled = false)
        {
            // check the path to the yaml config file
            if (File.Exists(configFileFullPath))
            {
                // get the path to the output folder
                if (outputsPath != "")
                {
                    if (Directory.Exists(outputsPath))
                    {
                        outputFilePath = outputsPath;
                    }
                    else
                    {
                        outputsPath = "";
                    }
                }

                if (outputsPath == "")
                {
                    if (verboseEnabled)
                    {
                        Console.WriteLine("  Output folder not given or missing. Results will be saved in the same folder of the config file.");
                    }
                    int pathLength = configFileFullPath.LastIndexOf('\\');
                    outputFilePath = configFileFullPath.Substring(0, pathLength + 1);
                }

                // read config file and set up basic variables (links)
                InitialisationSuccesful = readConfigFile(configFileFullPath, verboseEnabled);

                // read the file with settings for new simulations
                if (InitialisationSuccesful)
                {
                    InitialisationSuccesful = readSettingsFile(apsimSettingsFileFullPath);
                }
            }
            else
            {
                Console.WriteLine("  Config yaml file not found!");
                InitialisationSuccesful = false;
            }
        }

        #region public and private variables

        /// <summary>Flag whether the basic info was interpreted successfully and the required files exist.</summary>
        public bool InitialisationSuccesful;
        /// <summary>Full path to the Apsim model executable.</summary>
        private string apsimModelFullPath;
        /// <summary>Full path to the base simulation file.</summary>
        private string apsimBaseSimulationFullPath;
        /// <summary>Full path to the file with simulation settings.</summary>
        public string apsimSettingsFileFullPath;
        /// <summary>Path to folder with the weather data (netcdf files).</summary>
        public string weatherDataPath;
        /// <summary>List with the names of each file containing weather data (by type)</summary>
        private string[] weatherDataFiles;
        /// <summary>Full path to the soil library data (xml file).</summary>
        private string soilLibraryFullPath;
        /// <summary>Path to the directory used to save the outputs.</summary>
        private string outputFilePath;

        /// <summary>List of basic settings for the locations to simulate (latitude, longitude, soil data).</summary>
        private List<ApsimLocationSettings> locationsSettings = new List<ApsimLocationSettings>();
        /// <summary>List of settings for simulation clock (start and end dates).</summary>
        private List<ApsimClockSettings> clockSettings = new List<ApsimClockSettings>();
        /// <summary>List of settings used to define the farming systems that can be used to modify the base simulation.</summary>
        private Dictionary<string, List<ApsimModelSettings>> farmingSystemsSettings = new Dictionary<string, List<ApsimModelSettings>>();
        /// <summary>List of weather files (met) available for use in Apsim simulations.</summary>
        private Dictionary<string, string> metFilesAddresses = new Dictionary<string, string>();
        /// <summary>List of soil parameters sets (json nodes) ready for use in Apsim simulations.</summary>
        private Dictionary<string, JObject> soilParameterisation = new Dictionary<string, JObject>();

        /// <summary>List of farming systems names (used for creating simulations)</summary>
        private List<IOMSimulationSetting> simulationsSettings = new List<IOMSimulationSetting>();

        /// <summary>List of tags for each weather variable.</summary>
        private string[] weatherVariableTags = { "rainfile", "tminfile", "tmaxfile", "sradfile", "windfile", "petfile", "vpfile", "rhfile" };
        /// <summary>List of names for each weather variable.</summary>
        private string[] weatherVariableNames = { "rainfall", "min. temperature", "max. temperature", "solar radiation", "wind speed", "pet", "vp", "rh" };

        /// <summary>List of names for the dimensions used to define the converted output database (a netcdf file).</summary>
        private string[] dimensionsNames = { "time", "latitude", "longitude" };
        /// <summary>List of units for the dimensions used to define the converted output database (a netcdf file).</summary>
        private string[] dimensionsUnits = { "number of days since 1959-12-31", "decimal degrees", "decimal degrees" };
        /// <summary>List of variables names to write down in the converted output database (a netcdf file).</summary>
        private string[] outputVariablesNames = { "Yield", "LeachedN" };
        /// <summary>List of units for the variables to write down in the converted output database (a netcdf file).</summary>
        private string[] outputVariablesUnits = { "kg/ha", "kg/ha" };
        /// <summary>List of variable required from Apsim, corresponding to the dimensions and outputVariables.</summary>
        private string[] apsimRequiredVariables = { "Date", "Latitude", "Longitude", "CropYield", "TotalLeachedN" };

        /// <summary>Date used as base to compute time variable for the netCDF files.</summary>
        private DateTime baseDate = new DateTime(1959, 12, 31);
        /// <summary>Value to be used when there is a missing value.</summary>
        private float missingValue = -9999F;

        #endregion

        #region public methods

        /// <summary>Gets the name and parameters (coordinates and soil name) of a given location and stores them.</summary>
        /// <param name="locationName">Name of location to store.</param>
        /// <param name="locationLatitude">Latitude (decimal degrees) of location (optional).</param>
        /// <param name="locationLongitude">Longitude (decimal degrees) of location (optional).</param>
        /// <param name="locationSoil">Name of the soil for this location (optional).</param>
        /// <returns>True if data was stored successfully, false otherwise.</returns>
        public bool AddLocationSetup(string locationName, float locationLatitude = float.NaN, float locationLongitude = float.NaN, string locationSoil = "")
        {
            bool dataOK = true;
            ApsimLocationSettings newLocation = new ApsimLocationSettings();
            newLocation.Name = locationName;

            // check and add the location coordinates
            if ((Math.Abs(locationLatitude) <= 90) && (Math.Abs(locationLongitude) <= 180))
            {
                newLocation.Latitude = locationLatitude;
                newLocation.Longitude = locationLongitude;
                newLocation.MetFileName = $"WeatherData({(locationLatitude * 1000).ToString("00000")}_{(locationLongitude * 1000).ToString("000000")}).met";
            }
            else if ((Math.Abs(locationLatitude) <= 90) || (Math.Abs(locationLongitude) <= 180))
            {
                Console.WriteLine("Coordinates given for location to simulate in the settings file are not valid");
                dataOK = false;
            }
            //else - No coordinates were given

            // check and add the soil name
            if (locationSoil != "")
            {
                newLocation.SoilName = locationSoil;
            }
            //else - No soil name was given

            if (dataOK)
            {
                if ((Math.Abs(newLocation.Latitude) <= 90) || (newLocation.SoilName.Length > 1))
                {  // at least on of the location settings (soil or weather) was set
                    locationsSettings.Add(newLocation);
                }
                else
                {
                    dataOK = false;
                }
            }

            return dataOK;
        }

        /// <summary>Gets the name and settings for Apsim simulation clock and stores them.</summary>
        /// <param name="name">Name of this setup instance.</param>
        /// <param name="startingDate">Date to start the simulation clock (optional).</param>
        /// <param name="finishingDate">Data to finish the simulation clock (optional).</param>
        /// <returns>True if data was stored successfully, false otherwise.</returns>
        public bool AddClockSetup(string name, string startingDate = "", string finishingDate = "")
        {
            bool dataOK = true;
            ApsimClockSettings newClockSettings = new ApsimClockSettings();
            newClockSettings.Name = name;
            newClockSettings.StartDate = startingDate;
            newClockSettings.EndDate = finishingDate;

            if (newClockSettings.Duration > 0)
            {
                clockSettings.Add(newClockSettings);
            }
            else
            {
                Console.WriteLine($" Clock parameters for {newClockSettings.Name} returned negative duration");
                dataOK = false;
            }

            return dataOK;
        }

        /// <summary>Gets the list of names for the locations with settings defined in this handler.</summary>
        /// <returns>List of names of available locations.</returns>
        public List<string> GetLocationsNames()
        {
            List<string> myList = new List<string>();
            foreach (var givenLocation in locationsSettings)
            {
                myList.Add(givenLocation.Name);
            }

            return myList;
        }

        /// <summary>Gets the list of names for the clock settings defined in this handler.</summary>
        /// <returns>List of names of available settings.</returns>
        public List<string> GetClockSettingNames()
        {
            List<string> myList = new List<string>();
            foreach (var givenSetting in clockSettings)
            {
                myList.Add(givenSetting.Name);
            }

            return myList;
        }

        /// <summary>Gets the list of names for the farming systems with settings defined in this handler.</summary>
        /// <returns>List of names of available farming systems.</returns>
        public List<string> GetFarmingSystemsNames()
        {
            List<string> myList = new List<string>();
            foreach (var givenSystem in farmingSystemsSettings)
            {
                myList.Add(givenSystem.Key.ToString());
            }

            return myList;
        }

        /// <summary>Prepares the weather data: reads necdf files, organizes the data, and saves to apsim met.</summary>
        /// <remarks>
        /// If no location is given, this will attempt to prepare data for all locations in locationsSettings;
        ///  provided that the coordinates are unique.
        /// </remarks>
        /// <param name="locationName">Name of the location for which the data is to be prepared.</param>
        /// <returns>True if weather data was found and a met file was saved, false otherwise.</returns>
        public bool PrepareWeatherData(string locationName = "all")
        {
            bool dataOK = true;
            if (locationName.ToLower() == "all")
            {
                List<string> uniqueWeatherLocations = locationsSettings.Select(s => s.MetFileName).Distinct().ToList();
                if (uniqueWeatherLocations.Count > 0)
                {
                    foreach (ApsimLocationSettings givenLocation in locationsSettings)
                    {
                        if (uniqueWeatherLocations.Contains(givenLocation.MetFileName))
                        {
                            float[] coordinates = new float[] { givenLocation.Latitude, givenLocation.Longitude };
                            dataOK = PrepareWeatherData(coordinates, givenLocation.MetFileName);
                            if (dataOK)
                            {
                                metFilesAddresses.Add(givenLocation.MetFileName, givenLocation.MetFileName);
                                uniqueWeatherLocations.Remove(givenLocation.MetFileName);
                                if (uniqueWeatherLocations.Count == 0)
                                {
                                    break;
                                }
                            }
                            else
                            {
                                break;
                            }
                        }
                        // else - No changes in weather files
                    }
                }
            }
            else
            {
                ApsimLocationSettings givenLocation = locationsSettings.Find(l => l.Name == locationName);
                if (givenLocation.MetFileName != null)
                {
                    float[] coordinates = new float[] { givenLocation.Latitude, givenLocation.Longitude };
                    dataOK = PrepareWeatherData(coordinates, givenLocation.MetFileName);
                }
                else
                {
                    Console.WriteLine("No coordinates were defined for location: {0}", locationName);
                    dataOK = false;
                }
            }

            return dataOK;
        }

        /// <summary>Prepares the weather data: reads necdf files, organizes the data, and saves to apsim met.</summary>
        /// <remarks>Data is prepared independent of location, defined by given coordinates (decimal degrees).</remarks>
        /// <param name="locationCoordinates">Array containing the latitude and longitude for which the data is to be prepared.</param>
        /// <param name="metFileName">Name of the weather file being created.</param>
        /// <returns>True if weather data was found and a file was saved, false otherwise.</returns>
        public bool PrepareWeatherData(float[] locationCoordinates, string metFileName)
        {
            bool dataOK = true;

            if (locationCoordinates.Length == 2)
            {
                string metFileData = NetcdfToMet.GetWeatherData(weatherDataPath, weatherDataFiles, locationCoordinates);
                dataOK = metFileData.Length > 1;
                if (dataOK)
                {
                    using (StreamWriter outFile = new StreamWriter(outputFilePath + metFileName))
                    {
                        outFile.WriteLine(metFileData);
                    }
                }
            }
            else
            {
                Console.WriteLine("Number of coordinates given was different than two");
                dataOK = false;
            }

            return dataOK;
        }

        /// <summary>Prepares the soil data: reads a xml library file, gather the data, converts to json, and stores it.</summary>
        /// <remarks>
        /// If no location is given, this will attempt to prepare data for all soils named in locationsSettings;
        ///  provided that the soil types are unique
        /// </remarks>
        /// <param name="locationName">Name of the location for which the data is to be prepared.</param>
        /// <returns>True if the soil was found and the data was converted, false otherwise.</returns>
        public bool PrepareSoilData(string locationName = "all")
        {
            bool dataOK = true;
            JObject soilData;
            List<string> uniqueSoilTypes = locationsSettings.Select(s => s.SoilName).Distinct().ToList();
            if (uniqueSoilTypes.Count > 0)
            {
                foreach (var givenLocation in locationsSettings)
                {
                    if (uniqueSoilTypes.Contains(givenLocation.SoilName))
                    {
                        soilData = GetSoilData(givenLocation.SoilName);
                        if (soilData == null)
                        {
                            dataOK = false;
                            break;
                        }
                        else
                        {
                            soilParameterisation.Add(givenLocation.SoilName, soilData);
                            uniqueSoilTypes.Remove(givenLocation.SoilName);
                            if (uniqueSoilTypes.Count == 0)
                            {
                                break;
                            }
                        }
                    }
                    // else - No changes in the soil
                }
            }

            return dataOK;
        }

        /// <summary>Gets the soil data: reads a xml library file, gather the data, and converts to json.</summary>
        /// <param name="soilName">Name of the soil for which the data is to be prepared.</param>
        /// <returns>A json node with the data prepared for Apsim.</returns>
        public JObject GetSoilData(string soilName)
        {
            JObject soilData = null;
            soilData = XmlToJson.GetConvertedSoil(soilLibraryFullPath, apsimBaseSimulationFullPath, soilName, true);

            return soilData;
        }

        /// <summary>Creates a new Apsim simulation file, based on given base simulation and model settings.</summary>
        /// <remarks>Only basic checks are performed, assumes that base simulation and model settings are correct.</remarks>
        /// <param name="newSimulationName">Name of the simulation file to be created.</param>
        /// <param name="systemSetupName">Name of farming systems settings to be used (optional).</param>
        /// <param name="clockSettingName">Name of the clock settings to be used (optional).</param>
        /// <param name="locationName">Name of the location settings to be used (optional).</param>
        /// <param name="verboseEnabled">Flag whether info about the changes are to be printed in the console (optional).</param>
        /// <returns>True if the creation was successful, false otherwise.</returns>
        public bool CreateApsimSimulation(string newSimulationName, string systemSetupName ="", string clockSettingName = "", string locationName = "", bool verboseEnabled = false)
        {
            bool simOK = true;
            List<ApsimModelSettings> newApsimModelSettings = null;
            ApsimClockSettings newApsimClockSettings = null;
            ApsimLocationSettings newApsimLocationSettings = null;

            // check that the base simulation exists
            string baseApsimSimulationFile = File.ReadAllText(apsimBaseSimulationFullPath);
            if (baseApsimSimulationFile.Length < 2)
            {
                Console.WriteLine(" base simulation file is empty");
                simOK = false;
            }

            // check whether there are location settings
            if ((locationName.ToLower() != "") && (locationName.ToLower() != "none"))
            {
                if (locationsSettings.Where(l => l.Name == locationName).Count() > 0)
                {
                    newApsimLocationSettings = locationsSettings.Find(l => l.Name == locationName);
                }
                else
                {
                    Console.WriteLine($"  location settings have not been initialised for \"{locationName}\"");
                    simOK = false;
                }
            }
            // else - No location (weather or soil) was set up

            // check whether there are clock settings
            if ((clockSettingName.ToLower() != "") && (clockSettingName.ToLower() != "none"))
            {
                if (clockSettings.Where(c => c.Name == clockSettingName).Count() > 0)
                {
                    newApsimClockSettings = clockSettings.Find(c => c.Name == clockSettingName);
                }
                else
                {
                    Console.WriteLine($"  clock settings have not been initialised for \"{clockSettingName}\"");
                    simOK = false;
                }
            }
            // else  -  No clock settings were given

            // check whether there are model settings
            if ((systemSetupName.ToLower() != "") && (systemSetupName.ToLower() != "none"))
            {
                if (farmingSystemsSettings.ContainsKey(systemSetupName))
                {
                    newApsimModelSettings = farmingSystemsSettings[systemSetupName];
                }
                else
                {
                    Console.WriteLine($"  simulation settings have not been initialised for \"{systemSetupName}\"");
                    simOK = false;
                }
            }
            //else - No model modifications were set up

            if (simOK)
            {
                if (verboseEnabled)
                { // write basic setup
                    Console.WriteLine("  Base simulation has been modified as follows:");
                    if (newApsimLocationSettings != null)
                    {
                        Console.WriteLine("    - New location: {0}", newApsimLocationSettings.Name);
                        if (newApsimLocationSettings.MetFileName != null)
                        {
                            Console.WriteLine("      .Latitude: {0}", newApsimLocationSettings.Latitude.ToString("#0.000"));
                            Console.WriteLine("      .Longitude: {0}", newApsimLocationSettings.Longitude.ToString("#0.000"));
                        }
                        if (newApsimLocationSettings.SoilName.Length > 0)
                        {
                            Console.WriteLine("      .Soil name: {0}", newApsimLocationSettings.SoilName);
                        }
                    }

                    if(newApsimClockSettings != null)
                    {
                        Console.WriteLine("    - New simulation clock settings: {0}", newApsimClockSettings.Name);
                        Console.WriteLine("      .Start date: {0}", newApsimClockSettings.StartDate);
                        Console.WriteLine("      .End date: {0}", newApsimClockSettings.EndDate);
                    }

                    if (newApsimModelSettings != null)
                    {
                        Console.WriteLine("    - Models with new parameters:");
                        foreach (ApsimModelSettings apsimModel in newApsimModelSettings)
                        {
                            if (apsimModel.Name == "*")
                            {
                                Console.WriteLine("      .{0}", apsimModel.Type);
                            }
                            else
                            {
                                Console.WriteLine("      .{0}, {1}", apsimModel.Type, apsimModel.Name);
                            }
                        }
                    }
                }

                // deserialise the base Apsim simulation file and get the list of simulations in it
                dynamic mySimFile = JsonConvert.DeserializeObject(baseApsimSimulationFile);
                List<JObject> mySimulations = findApsimSimulationModel("Models.Core.Simulation", "*", mySimFile);

                if (verboseEnabled)
                {
                    if (mySimulations.Count > 1)
                    {
                        Console.WriteLine("    the file contains {0} simulations", mySimulations.Count);
                    }
                    else
                    {
                        Console.WriteLine("    the file contains only one simulation");
                    }
                }

                // go through each simulation in the file and set them up.
                bool replacementsDoneSuccessfully = true;
                foreach (dynamic apsimSimulation in mySimulations)
                {
                    if (newApsimModelSettings != null)
                    {
                        // replace any model parameters according to the model settings given
                        foreach (var modelSettings in newApsimModelSettings)
                        {
                            replacementsDoneSuccessfully = setApsimModelParameters(apsimSimulation,
                                                                                   modelSettings.Type,
                                                                                   modelSettings.Name,
                                                                                   modelSettings.ParametersName,
                                                                                   modelSettings.ParametersValue);

                            if (!replacementsDoneSuccessfully)
                                break;
                        }
                    }

                    if (newApsimClockSettings != null)
                    {
                        replacementsDoneSuccessfully = setApsimModelParameters(apsimSimulation,
                                                                               "Clock",
                                                                               "*",
                                                                               newApsimClockSettings.ParametersName,
                                                                               newApsimClockSettings.ParametersValue);

                        if (!replacementsDoneSuccessfully)
                            break;
                    }

                    if (newApsimLocationSettings != null)
                    {
                        if (newApsimLocationSettings.MetFileName != null)
                        {
                            replacementsDoneSuccessfully = setApsimModelParameters(apsimSimulation,
                                                                                   "Weather",
                                                                                   "*",
                                                                                   new List<string> { "FileName" },
                                                                                   new List<string> { newApsimLocationSettings.MetFileName });
                        }

                        if (newApsimLocationSettings.SoilName.Length > 0)
                        {
                            replacementsDoneSuccessfully = setApsimModelParameters(apsimSimulation,
                                                                                   "Soils.Soil",
                                                                                   "*",
                                                                                   new List<string> { "SoilData" },
                                                                                   new List<string> { newApsimLocationSettings.SoilName });
                        }

                        if (!replacementsDoneSuccessfully)
                            break;
                    }

                    if (!replacementsDoneSuccessfully)
                        break;
                }

                if (replacementsDoneSuccessfully)
                {
                    // set the path for the new simulation file
                    string apsimNewSimulationFullPath = outputFilePath + newSimulationName;

                    // serialise and save the modified Apsim simulation file
                    string newApsimSimulationFile = JsonConvert.SerializeObject(mySimFile, Formatting.Indented);
                    File.WriteAllText(apsimNewSimulationFullPath, newApsimSimulationFile);
                }
            }

            return simOK;
        }

        /// <summary>Runs an Apsim simulation file.</summary>
        /// <param name="apsimSimulationFile">Name of the simulation file to be run.</param>
        /// <param name="verboseEnabled">Flag whether info about the simulation is to be printed in the console (optional).</param>
        /// <returns>True if the simulation run has finished, false if something went wrong.</returns>
        public bool RunApsimSimulation(string apsimSimulationFile, bool verboseEnabled = false)
        {
            bool runOK = false;
            string apsimSimulationFullPath = "";
            // check that the simulation file exists
            if (File.Exists(apsimSimulationFile))
            {
                apsimSimulationFullPath = apsimSimulationFile;
                runOK = true;
            }
            else if (File.Exists(outputFilePath + apsimSimulationFile))
            {
                apsimSimulationFullPath = outputFilePath + apsimSimulationFile;
                runOK = true;
            }

            // run the simulation
            if (runOK)
            {
                var myWatch = new Stopwatch();
                if (verboseEnabled)
                {
                    Console.WriteLine($"  Running: { apsimSimulationFullPath}");
                    myWatch.Start();
                }

                Process myProcess = new Process();
                myProcess.StartInfo.FileName = apsimModelFullPath;
                myProcess.StartInfo.Arguments = $"\"{ apsimSimulationFullPath }\"";
                runOK = myProcess.Start();
                myProcess.WaitForExit();
                if (myProcess.HasExited)
                {
                    runOK = (myProcess.ExitCode == 0);
                    if (verboseEnabled)
                    {
                        myWatch.Stop();
                        Console.WriteLine("  Simulation run completed!");
                        Console.WriteLine($"   exit code: {myProcess.ExitCode}");
                        Console.WriteLine($"   execution time: {myWatch.ElapsedMilliseconds} ms.");
                    }
                }
            }

            return runOK;
        }

        /// <summary>Saves the Apsim simulated data into a netCDF file (simple file with overwrite).</summary>
        /// <param name="apsimDBFileName">DB file with Apsim outputs (SQLite).</param>
        /// <param name="apsimDBTableName">Table within the DB file that contains the data.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be written.</param>
        public void SaveOutputsTable(string apsimDBFileName, string apsimDBTableName, string netCDFFileName)
        {
            System.Data.DataTable myData = gatherSimulatedData(apsimDBFileName, apsimDBTableName);
            DataSet myNetCDFConnection = createNetCDFFileConnection(netCDFFileName);
            if (myNetCDFConnection != null)
            {
                putDataToNetCDF(myNetCDFConnection, myData);
            }
            else
            {
                Console.WriteLine(" - Error: output data was not saved because netCDF file could not be reached");
            }

            myNetCDFConnection.Dispose();        
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsNames">List of farming systems (model settings) names to be used for creating new simulations.</param>
        /// <param name="clockSettingNames">List of clock settings (start and end date) names to be used for creating new simulations.</param>
        /// <param name="locationNames">List of locations (soil and weather) names to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string[] farmSystemsNames, string[] clockSettingNames, string[] locationNames,
                                                 string simulationRootName,string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            // check how many simulations will be run
            int nSims = locationNames.Length * farmSystemsNames.Length;
            int iSim = 0;
            int simsToRun = 0;
            int simsRunOK = 0;
            bool processedOK = false;

            //open connection to a netCDF file
            DataSet myNetCDFConnection = createNetCDFFileConnection(netCDFFileName);
            if (myNetCDFConnection != null)
            {
                for (int iLoc = 0; iLoc < locationNames.Length; iLoc++)
                {
                    for (int iClk = 0; iClk < clockSettingNames.Length; iClk++)
                    {
                        for (int iSys = 0; iSys < farmSystemsNames.Length; iSys++)
                        {
                            // create a new simulation
                            string thisSimulation = simulationRootName + "(" + iSim + ")";
                            processedOK = CreateApsimSimulation(thisSimulation + ".apsimx", farmSystemsNames[iSys], clockSettingNames[iClk], locationNames[iLoc], verboseEnabled);
                            simsToRun += 1;

                            if (processedOK)
                            {
                                // run the simulation
                                processedOK = RunApsimSimulation(thisSimulation + ".apsimx", verboseEnabled);

                                if (processedOK)
                                {
                                    // gather the outputs and add to netCDF file
                                    System.Data.DataTable thisData = gatherSimulatedData(thisSimulation + ".db", apsimDBTableName);
                                    appendDataToNetCDF(myNetCDFConnection, thisData);
                                    simsRunOK += 1;
                                }
                            }

                            iSim += 1;
                        }
                    }
                }
            }
            else
            {
                processedOK = false;
                Console.WriteLine(" - Error: output file was not saved");
            }

            myNetCDFConnection.Dispose();

            if (verboseEnabled)
            {
                if (simsToRun == simsRunOK)
                {
                    Console.WriteLine($" All {nSims} simulations ran successfully");
                }
                else
                {
                    Console.WriteLine($" Only {simsRunOK} simulations ran ok, {simsToRun - simsRunOK} simulations failed");
                }
            }

            return processedOK;
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsName">Name of the farming system (model settings) to be used for creating new simulations.</param>
        /// <param name="clockSettingNames">List of clock settings (start and end date) names to be used for creating new simulations.</param>
        /// <param name="locationNames">List of locations (soil and weather) names to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string farmSystemsName, string[] clockSettingNames, string[] locationNames,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] farmSystemsArray = new string[1];
            farmSystemsArray[0] = farmSystemsName;
            return RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingNames, locationNames, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsNames">List of farming systems (model settings) names to be used for creating new simulations.</param>
        /// <param name="clockSettingName">Name of clock settings (start and end date) to be used for creating new simulations.</param>
        /// <param name="locationNames">List of locations (soil and weather) names to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string[] farmSystemsNames, string clockSettingName, string[] locationNames,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] clockSettingsArray = new string[1];
            clockSettingsArray[0] = clockSettingName;
            return RunSimulationsAndSaveOutputs(farmSystemsNames, clockSettingsArray, locationNames, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsNames">List of farming systems (model settings) names to be used for creating new simulations.</param>
        /// <param name="clockSettingNames">List of clock settings (start and end date) names to be used for creating new simulations.</param>
        /// <param name="locationName">Name of the location (soil and weather) to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string[] farmSystemsNames, string[] clockSettingNames, string locationName,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] locationsArray = new string[1];
            locationsArray[0] = locationName;
            return RunSimulationsAndSaveOutputs(farmSystemsNames, clockSettingNames, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsName">Name of the farming system (model settings) to be used for creating new simulations.</param>
        /// <param name="clockSettingName">Name of clock settings (start and end date) to be used for creating the new simulations.</param>
        /// <param name="locationNames">List of locations (soil and weather) names to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string farmSystemsName, string clockSettingName, string[] locationNames,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] farmSystemsArray = new string[1];
            string[] clockSettingsArray = new string[1];
            farmSystemsArray[0] = farmSystemsName;
            clockSettingsArray[0] = clockSettingName;
            return RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingsArray, locationNames, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsName">Name of the farming system (model settings) to be used for creating the new simulation.</param>
        /// <param name="clockSettingNames">List of clock settings (start and end date) names to be used for creating new simulations.</param>
        /// <param name="locationName">Name of the location (soil and weather) to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string farmSystemsName, string[] clockSettingNames, string locationName,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] farmSystemsArray = new string[1];
            string[] locationsArray = new string[1];
            farmSystemsArray[0] = farmSystemsName;
            locationsArray[0] = locationName;
            return RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingNames, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs a set of Apsim simulations, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsNames">List of farming systems (model settings) names to be used for creating new simulations.</param>
        /// <param name="clockSettingName">Name of clock settings (start and end date) to be used for creating the new simulation.</param>
        /// <param name="locationName">Name of the location (soil and weather) to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root used to name the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulations results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string[] farmSystemsNames, string clockSettingName, string locationName,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] clockSettingsArray = new string[1];
            string[] locationsArray = new string[1];
            clockSettingsArray[0] = clockSettingName;
            locationsArray[0] = locationName;
            return RunSimulationsAndSaveOutputs(farmSystemsNames, clockSettingsArray, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs an Apsim simulation, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="farmSystemsName">Name of the farming system (model settings) to be used for creating the new simulation.</param>
        /// <param name="clockSettingName">Name of clock settings (start and end date) to be used for creating the new simulation.</param>
        /// <param name="locationName">Name of the location (soil and weather) to be used for creating the new simulation.</param>
        /// <param name="simulationRootName">Root name used to identify the new simulation.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulation results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(string farmSystemsName, string clockSettingName, string locationName,
                                                 string simulationRootName, string apsimDBTableName, string netCDFFileName,
                                                 bool verboseEnabled = false)
        {
            string[] farmSystemsArray = new string[1];
            string[] clockSettingsArray = new string[1];
            string[] locationsArray = new string[1];
            farmSystemsArray[0] = farmSystemsName;
            clockSettingsArray[0] = clockSettingName;
            locationsArray[0] = locationName;
            return RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingsArray, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
        }

        /// <summary>Creates and runs an Apsim simulation, then gathers and saves the required outputs to a netCDF file.</summary>
        /// <param name="simulationsToRun">List of settings (location, dates, model settings) to be used for creating new simulations.</param>
        /// <param name="simulationRootName">Root name used to identify the new simulations.</param>
        /// <param name="apsimDBTableName">Table within the apsim DB file with simulation results.</param>
        /// <param name="netCDFFileName">Name of netCDF file to be created to store the required outputs.</param>
        /// <param name="verboseEnabled">Flag whether feedback and progress info is written in the console (optional).</param>
        /// <returns>True if tasks are completed successfully, false otherwise.</returns>
        public bool RunSimulationsAndSaveOutputs(IOMSimulationSetting simulationToRun, string simulationRootName,
                                                 string apsimDBTableName, string netCDFFileName, bool verboseEnabled = false)
        {
            bool processedOK = true;
            string[] farmSystemsArray = new string[1];
            string[] clockSettingsArray = new string[1];
            string[] locationsArray = new string[1];
            if (simulationToRun == null)
            {// run all simulations defined in simulationsSettings
                processedOK = PrepareSoilData();
                if (processedOK)
                {
                    processedOK = PrepareWeatherData();
                    if (processedOK)
                    {
                        foreach (IOMSimulationSetting simToRun in simulationsSettings)
                        {
                            farmSystemsArray[0] = simToRun.FarmingSystemName;
                            clockSettingsArray[0] = simToRun.ClockSettingName;
                            locationsArray[0] = simToRun.LocationName;
                            processedOK = RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingsArray, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
                            if (processedOK)
                            {
                                // clean stuff
                            }
                        }
                    }
                }
            }
            else
            {
                processedOK = PrepareSoilData(simulationToRun.LocationName);
                if (processedOK)
                {
                    processedOK = PrepareWeatherData(simulationToRun.LocationName);
                    if (processedOK)
                    {
                        farmSystemsArray[0] = simulationToRun.FarmingSystemName;
                        clockSettingsArray[0] = simulationToRun.ClockSettingName;
                        locationsArray[0] = simulationToRun.LocationName;
                        processedOK = RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingsArray, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
                    }
                }
            }


            //RunSimulationsAndSaveOutputs(farmSystemsArray, clockSettingsArray, locationsArray, simulationRootName, apsimDBTableName, netCDFFileName, verboseEnabled);
            return processedOK;
        }

        /// <summary>Reads a table with basic data to be used for creating new simulations.</summary>
        /// <remarks>This method clears any previously existing location settings.</remarks>
        /// <param name="fileName">Name and path of a csv file containing the datatable.</param>
        /// <returns>True if some data was read, false otherwise.</returns>
        public bool ReadSimulationTable(string fileName)
        {
            bool readOK = false;
            var simsTable = DataSet.Open(fileName);
            double[] simsID = simsTable.GetData<double[]>("EcotopeID");
            string[] soilType = simsTable.GetData<string[]>("SMapSibling");
            double[] latitude = simsTable.GetData<double[]>("Latitude");
            double[] longitude = simsTable.GetData<double[]>("Longitude");
            string[] farmSystem = simsTable.GetData<string[]>("FarmingSystem");

            if (simsID.Length > 0)
            {
                // clear existing location settings and add the ones just given
                locationsSettings.Clear();
                IOMSimulationSetting newSimSetting;
                for (int i = 0; i < simsID.Length; i++)
                {
                    if (farmSystem[i].ToLower() != "none")
                    {
                        string simID = simsID[i].ToString("#0");
                        AddLocationSetup(simID, (float)latitude[i], (float)longitude[i], soilType[i]);
                        newSimSetting = new IOMSimulationSetting(simID, simID, clockSettings[0].Name, farmSystem[i]);
                        simulationsSettings.Add(newSimSetting);
                    }
                }

                readOK = true;
            }

            return readOK;
        }



        #endregion

        #region private properties and methods

        /// <summary>Date used as base to compute time for the netCDF files.</summary>
        public string BaseDate
        {
            get { return baseDate.ToString("yyyy-MM-dd"); }
            set { baseDate = DateTime.Parse(value); }
        }

        /// <summary>Reads a yaml file with the basic configuration for ApsimHandler.</summary>
        /// <remarks>
        /// The config file is expected to contains the following set of information:
        ///  - the name and path to the Apsim model executable;
        ///  - the name and path to an Apsim base simulation;
        ///  - the name and path to the simulation settings file;
        ///  - the name and path to the soil library file (xml);
        ///  - the names and path to the weather data files (netcdf);
        /// If an entry is supplied the code tests that the path exists, but this is ignored if no entry is given.
        /// </remarks>
        /// <param name="yamlFileFullPath">Full path to the yaml config file.</param>
        /// <param name="verboseEnabled">Flag whether info about files are to be printed in the console (optional).</param>
        /// <returns>True if all items found are ok, false otherwise.</returns>
        private bool readConfigFile(string yamlFileFullPath, bool verboseEnabled = false)
        {
            bool configOK = true;
            apsimModelFullPath = "";
            apsimBaseSimulationFullPath = "";
            apsimSettingsFileFullPath = "";
            weatherDataPath = "";
            soilLibraryFullPath = "";

            using (var yamlInput = new StreamReader(yamlFileFullPath))
            {
                // load the stream
                var newYamlStream = new YamlStream();
                newYamlStream.Load(yamlInput);
                var mappedSettings = (YamlMappingNode)newYamlStream.Documents[0].RootNode;

                // examine the yaml stream, get the paths to the model, base simulation, and other settings
                foreach (var entry in mappedSettings.Children)
                {
                    if (entry.Key.ToString().ToLower() == "apsimexe")
                    {
                        apsimModelFullPath = getFullFilePath((YamlMappingNode)mappedSettings.Children[entry.Key]);
                        if (File.Exists(apsimModelFullPath))
                        {
                            if (verboseEnabled)
                            {
                                Console.WriteLine($"  Apsim model: { apsimModelFullPath}");
                            }
                        }
                        else
                        {
                            if (apsimModelFullPath == "")
                            {
                                Console.WriteLine(" Could not read the name and path to the Apsim model executable in the yaml file");
                            }
                            else
                            {
                                Console.WriteLine(" The path to the Apsim model executable is not valid or the file does not exist");
                            }
                            configOK = false;
                            break;
                        }
                    }

                    if (entry.Key.ToString().ToLower() == "apsimbasesim")
                    {
                        apsimBaseSimulationFullPath = getFullFilePath((YamlMappingNode)mappedSettings.Children[entry.Key]);
                        if (File.Exists(apsimBaseSimulationFullPath))
                        {
                            if (verboseEnabled)
                            {
                                Console.WriteLine("  Base simulation: " + apsimBaseSimulationFullPath);
                            }
                        }
                        else
                        {
                            if (apsimBaseSimulationFullPath == "")
                            {
                                Console.WriteLine(" Could not find the name and path to the base simulation in the yaml file");
                            }
                            else
                            {
                                Console.WriteLine(" The path to the base simulation is not valid or the file does not exist");
                            }
                            configOK = false;
                            break;
                        }
                    }

                    if (entry.Key.ToString().ToLower() == "apsimsettingsfile")
                    {
                        apsimSettingsFileFullPath = getFullFilePath((YamlMappingNode)mappedSettings.Children[entry.Key]);
                        if (File.Exists(apsimSettingsFileFullPath))
                        {
                            if (verboseEnabled)
                            {
                                Console.WriteLine("  Simulation settings file: " + apsimSettingsFileFullPath);
                            }
                        }
                        else
                        {
                            if (apsimSettingsFileFullPath == "")
                            {
                                Console.WriteLine(" Could not find the name and path to the simulation settings file in the yaml file");
                            }
                            else
                            {
                                Console.WriteLine(" The path to the simulation settings file is not valid or the file does not exist");
                            }
                            configOK = false;
                            break;
                        }
                    }

                    if (entry.Key.ToString().ToLower() == "soildata")
                    {
                        soilLibraryFullPath = getFullFilePath((YamlMappingNode)mappedSettings.Children[entry.Key]);
                        if (File.Exists(soilLibraryFullPath))
                        {
                            if (verboseEnabled)
                            {
                                Console.WriteLine("  Soil library file: " + soilLibraryFullPath);
                            }
                        }
                        else
                        {
                            if (soilLibraryFullPath == "")
                            {
                                Console.WriteLine(" Could not find the name and path to the soil library in the yaml file");
                            }
                            else
                            {
                                Console.WriteLine(" The path to the soil library is not valid or the file does not exist");
                            }
                            configOK = false;
                            break;
                        }
                    }

                    if (entry.Key.ToString().ToLower() == "weatherdata")
                    {
                        var mappingNode = (YamlMappingNode)mappedSettings.Children[entry.Key];
                        int numberOfFiles = ((YamlMappingNode)entry.Value).Children.Count - 1;
                        weatherDataFiles = new string[numberOfFiles];
                        int i = 0;
                        foreach (var innerEntry in mappingNode.Children)
                        {
                            string varTag = innerEntry.Key.ToString().ToLower();
                            if (varTag == "path")
                            {
                                weatherDataPath = innerEntry.Value.ToString();
                            }
                            else
                            {
                                int varIndex = Array.IndexOf(weatherVariableTags, varTag);
                                if ((varIndex >= 0) && (varIndex < weatherVariableTags.Length))
                                {
                                    weatherDataFiles[i] = innerEntry.Value.ToString();
                                    if (File.Exists(weatherDataPath + weatherDataFiles[i]))
                                    {
                                        if (verboseEnabled)
                                        {
                                            Console.WriteLine($"  {weatherVariableNames[varIndex]} file: {soilLibraryFullPath}");
                                        }
                                    }
                                    else
                                    {
                                        Console.WriteLine($" Could not find {varTag}:{weatherDataFiles[i]} in the specified folder");
                                        configOK = false;
                                        break;
                                    }

                                    i += 1;
                                }
                                else
                                {
                                    Console.WriteLine($" The number of weather files specified or their tags were not given as expected");
                                    configOK = false;
                                    break;
                                }
                            }
                        }

                        if (!configOK)
                            break;
                    }

                    if (entry.Key.ToString().ToLower() == "outputsstructure")
                    {
                        string[] separators = { ", ", "; ", ",", ";" };
                        foreach (var innerEntry in (YamlMappingNode)entry.Value)
                        {
                            if (innerEntry.Key.ToString() == "dimensionsName")
                            {
                                dimensionsNames = innerEntry.Value.ToString().Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            }
                            else if (innerEntry.Key.ToString() == "dimensionsUnits")
                            {
                                dimensionsUnits = innerEntry.Value.ToString().Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            }
                            else if (innerEntry.Key.ToString() == "outputVariablesName")
                            {
                                outputVariablesNames = innerEntry.Value.ToString().Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            }
                            else if (innerEntry.Key.ToString() == "outptuVariablesUnits")
                            {
                                outputVariablesUnits = innerEntry.Value.ToString().Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            }
                            else if (innerEntry.Key.ToString() == "apsimCorrespondingVariables")
                            {
                                apsimRequiredVariables = innerEntry.Value.ToString().Split(separators, StringSplitOptions.RemoveEmptyEntries);
                            }
                        }


                        if (outputVariablesNames.Length != outputVariablesUnits.Length)
                        {
                            Console.WriteLine("  The number of names and units for the output variables does not match!");
                            configOK = false;
                            break;
                        }
                        if (dimensionsNames.Length != dimensionsUnits.Length)
                        {
                            Console.WriteLine("  The number of names and units for the output dimensions does not match!");
                            configOK = false;
                            break;
                        }

                        if (apsimRequiredVariables.Length != (dimensionsNames.Length + outputVariablesNames.Length))
                        {
                            Console.WriteLine("  The number of variables names from Apsim does not match the number of dimensions plus output variables!");
                            configOK = false;
                            break;
                        }

                        if (verboseEnabled)
                        {
                            Console.WriteLine("  The handler will output the following variables (units):");
                            for (int i = 0; i < outputVariablesNames.Length; i++)
                            {
                                Console.WriteLine("    - {0} ({1})", outputVariablesNames[i], outputVariablesUnits[i]);
                            }
                            Console.WriteLine("  Which are mapped to the following dimensions (units):");
                            for (int i = 0; i < dimensionsNames.Length; i++)
                            {
                                Console.WriteLine("    - {0} ({1})", dimensionsNames[i], dimensionsUnits[i]);
                            }
                            Console.WriteLine("  All these are based on apsim variables: " + string.Join(", ", apsimRequiredVariables));
                        }
                    }
                }
            }

            return configOK;
        }

        /// <summary>Reads a yaml file containing the settings that can be used to modify the base simulation, and stores them.</summary>
        /// <param name="yamlFileFullPath">Full path to the yaml settings file.</param>
        /// <returns>True if all items are found, false otherwise.</returns>
        /// <remarks>
        /// The settings file is assumed to contain the settings in one or more of three groups:
        ///  - locationSetup: list of info for each location to be simulated, including:
        ///   . Name of location;
        ///   . Coordinates (latitude and longitude) of the location (used to get the weather data);
        ///   . Name of soil type for the location (with actual data to be found in a given SoilLibrary);
        ///    The coordinates are assumed to follow NIWA's VSCN grid (spaced by 0.05 degrees, with a tolerance of 0.001);
        ///  - clockSetup: list of settings (start and end dates) for Apsim's simulation clock.
        ///  - simulationSetup: list of Apsim models (by type, plus name for managers) and the parameter settings to be used.
        ///    Only one level of parameterisation is accepted (e.g. Clock:StartDate is ok, Plant:Leaf:SLA is not);
        ///  - farmingSystemsSetup: list of models and parameter values that define one or more farming system.
        ///    This is equivalent to a list of simulationSetup's, and thus these are meant to be used as alternatives;
        /// </remarks>
        public bool readSettingsFile(string yamlFileFullPath)
        {
            bool dataOK = true;

            using (var yamlInput = new StreamReader(yamlFileFullPath))
            {
                // load the stream
                var newYamlStream = new YamlStream();
                newYamlStream.Load(yamlInput);
                var mappedSettings = (YamlMappingNode)newYamlStream.Documents[0].RootNode;

                // examine the yaml stream, get the name of model and parameters to change
                foreach (var entry in mappedSettings.Children)
                {
                    if (entry.Key.ToString().ToLower() == "locationsetup")
                    { // there is at least one set of coordinates
                        dataOK = gatherLocationInfo((YamlSequenceNode)entry.Value);
                    }
                    else if (entry.Key.ToString().ToLower() == "clocksetup")
                    {
                        dataOK = gatherClockSetup((YamlSequenceNode)entry.Value);
                    }
                    else if (entry.Key.ToString().ToLower() == "simulationsetup")
                    {
                        dataOK = gatherFarmingSystemSetup((YamlMappingNode)entry.Value, "BasicUserDefinedSystem");
                    }
                    else if (entry.Key.ToString().ToLower() == "farmingsystemssetup")
                    {
                        foreach (var givenFarmSystem in (YamlSequenceNode)entry.Value)
                        {
                            dataOK = gatherFarmingSystemSetup((YamlMappingNode)givenFarmSystem, givenFarmSystem["Name"].ToString());
                            if (!dataOK)
                                break;
                        }
                    }
                    // else - Nothing to be done
                }
            }

            return dataOK;
        }

        /// <summary>Gathers the name and parameters (coordinates and soil name) of given locations and stores them.</summary>
        /// <param name="locationsGiven">List of nodes with info about the locations given.</param>
        /// <returns>True if data was gathered successfully, false otherwise.</returns>
        private bool gatherLocationInfo(YamlSequenceNode locationsGiven)
        {
            bool dataOK = true;

            foreach (YamlMappingNode givenLocation in locationsGiven)
            {
                ApsimLocationSettings newLocation = new ApsimLocationSettings();
                newLocation.Name = givenLocation["Name"].ToString();

                // get the name of the location and initialise variables
                float newLatitude = -100;
                float newLongitude = -200;
                // get the values of each parameter
                foreach (var item in givenLocation.Children)
                {
                    if (item.Key.ToString().ToLower() == "latitude")
                    {
                        newLatitude = float.Parse(item.Value.ToString());
                    }
                    else if (item.Key.ToString().ToLower() == "longitude")
                    {
                        newLongitude = float.Parse(item.Value.ToString());
                    }
                    else if (item.Key.ToString().ToLower() == "soilname")
                    {
                        newLocation.SoilName = item.Value.ToString();
                    }
                }

                // check and add the location coordinates
                if ((Math.Abs(newLatitude) <= 90) && (Math.Abs(newLongitude) <= 180))
                {
                    newLocation.Latitude = newLatitude;
                    newLocation.Longitude = newLongitude;
                    newLocation.MetFileName = $"WeatherData({(newLatitude * 1000).ToString("00000")}_{(newLongitude * 1000).ToString("000000")}).met";
                }
                else if ((Math.Abs(newLatitude) <= 90) || (Math.Abs(newLongitude) <= 180))
                {
                    Console.WriteLine("Coordinates given for location to simulate in the settings file are not valid");
                    dataOK = false;
                }
                //else - No coordinates given

                if (dataOK)
                {
                    locationsSettings.Add(newLocation);
                }
                else
                {
                    break;
                }
            }

            return dataOK;
        }

        /// <summary>Gathers the settings for Apsim simulation clock and stores them.</summary>
        /// <param name="settingsGiven">Node (yaml) with the settings info.</param>
        /// <returns>True if data was gathered successfully, false otherwise.</returns>
        private bool gatherClockSetup(YamlSequenceNode settingsGiven)
        {
            bool dataOK = true;

            foreach (var parameterSet in settingsGiven.Children)
            {
                ApsimClockSettings newClockSettings = new ApsimClockSettings();
                foreach (var givenParameter in (YamlMappingNode)parameterSet)
                {
                    if (givenParameter.Key.ToString() == "Name")
                    {
                        newClockSettings.Name = givenParameter.Value.ToString();
                    }
                    else if (givenParameter.Key.ToString() == "StartDate")
                    {
                        newClockSettings.StartDate = givenParameter.Value.ToString();
                    }
                    else if (givenParameter.Key.ToString() == "EndDate")
                    {
                        newClockSettings.EndDate = givenParameter.Value.ToString();
                    }
                }

                if (newClockSettings.Duration > 0)
                {
                    clockSettings.Add(newClockSettings);
                }
                else
                {
                    Console.WriteLine($" Clock parameters for {newClockSettings.Name} returned negative duration");
                    dataOK = false;
                }
            }


            return dataOK;
        }

        /// <summary>Gathers the Apsim model settings for a given farming system and stores them.</summary>
        /// <param name="modelsToSetUp">Node (yaml) with the settings info.</param>
        /// <param name="farmingSystemName">Name of the system being prepared.</param>
        /// <returns>True if data was gathered successfully, false otherwise.</returns>
        private bool gatherFarmingSystemSetup(YamlMappingNode modelsToSetUp, string farmingSystemName)
        {
            bool dataOK = true;
            List<ApsimModelSettings> newFarmSystemSettings = new List<ApsimModelSettings>();
            foreach (var givenModel in modelsToSetUp.Children)
            {
                ApsimModelSettings newModelSettings = new ApsimModelSettings();
                string modelType = givenModel.Key.ToString();
                if (modelType == "Manager")
                {
                    var simManagers = (YamlSequenceNode)modelsToSetUp.Children[givenModel.Key];
                    foreach (YamlMappingNode givenManager in simManagers)
                    {
                        YamlMappingNode managerParams = (YamlMappingNode)givenManager.Children["Parameters"];
                        newModelSettings = gatherApsimModelSettings(managerParams, modelType);
                        if (newModelSettings != null)
                        {
                            newModelSettings.Name = givenManager["Name"].ToString();
                            newFarmSystemSettings.Add(newModelSettings);
                        }
                        else
                        {
                            Console.WriteLine($" Could not read the parameters for the manager \'{givenManager["Name"].ToString()}\' in the yaml file");
                            dataOK = false;
                        }
                    }
                }
                else // general model
                {
                    if (givenModel.Value.GetType().ToString().Contains("Mapping"))
                    {
                        newModelSettings = gatherApsimModelSettings((YamlMappingNode)givenModel.Value, modelType);
                        if (newModelSettings != null)
                        {
                            newFarmSystemSettings.Add(newModelSettings);
                        }
                        else
                        {
                            Console.WriteLine($" Could not read the parameters for the model \'{modelType}\' in the yaml file");
                            dataOK = false;
                        }
                    }
                }
            }

            // add the settings to list of farming systems
            if (dataOK)
            {
                farmingSystemsSettings.Add(farmingSystemName, newFarmSystemSettings);
            }

            return dataOK;
        }

        /// <summary>Gets the path and name of a file from a yaml node.</summary>
        /// <param name="mappingNode">Yaml node containing path and name of a file.</param>
        /// <returns>A string with the full path to a file.</returns>
        /// <remarks>Assumes that both path and filename are present, empty string is returned if not.</remarks>
        private string getFullFilePath(YamlMappingNode mappingNode)
        {
            string thePath = "";
            string theFile = "";
            foreach (var innerEntry in mappingNode.Children)
            {
                if (innerEntry.Key.ToString() == "path")
                {
                    thePath = innerEntry.Value.ToString();
                }
                else if (innerEntry.Key.ToString() == "fileName")
                {
                    theFile += innerEntry.Value.ToString();
                }
            }
            return thePath + theFile;
        }

        /// <summary>Gathers a set of model parameters that will be changed in an Apsim simulation.</summary>
        /// <param name="parametersGiven">Yaml node with the model parameters settings.</param>
        /// <param name="givenModelType">Type of model that is being set up.</param>
        /// <returns>The list of parameters that will be used to change the setup of an Apsim model.</returns>
        private ApsimModelSettings gatherApsimModelSettings(YamlMappingNode parametersGiven, string givenModelType)
        {
            ApsimModelSettings newApsimSettings = new ApsimModelSettings();
            newApsimSettings.Type = givenModelType;
            newApsimSettings.Name = "*";
            newApsimSettings.ParametersName = new List<string>();
            newApsimSettings.ParametersValue = new List<string>();
            foreach (var givenParameter in parametersGiven.Children)
            {
                newApsimSettings.ParametersName.Add(givenParameter.Key.ToString());
                newApsimSettings.ParametersValue.Add(givenParameter.Value.ToString());
            }

            return newApsimSettings;
        }

        /// <summary>Searches for a model (node) within an Apsim simulation file (JSON).</summary>
        /// <param name="itemType">Type of the model being searched for.</param>
        /// <param name="itemName">Name of the model being searched for (needed for managers).</param>
        /// <param name="jsonNode">Node, or part of an Apsim file, being analysed.</param>
        /// <returns>A list of all nodes found that match the model type and name.</returns>
        private static List<JObject> findApsimSimulationModel(string itemType, string itemName, JObject jsonNode)
        {
            List<JObject> result = new List<JObject>();
            foreach (dynamic item in jsonNode["Children"])
            {
                string myType = item["$type"];
                if (myType.Contains(itemType))
                {
                    if ((itemName == "*") || (item["Name"] == itemName))
                    {
                        result.Add(item);
                    }
                }
                else
                {
                    if (item["Children"] is System.Collections.IList)
                    {
                        result.AddRange(findApsimSimulationModel(itemType, itemName, item));
                    }
                }
            }

            return result;
        }

        /// <summary>Modifies the value of a given parameter in all instances of a model (name and type) in an Apsim simulation.</summary>
        /// <param name="mySimulation">Reference to a simulation (or part of one) that is being modified (json).</param>
        /// <param name="modelType">Type of model to be modified.</param>
        /// <param name="modelName">Name of the model to be modified (only needed for managers).</param>
        /// <param name="parametersName">List with the name of parameters to be modified.</param>
        /// <param name="parametersValue">List with the values to set each parameter to.</param>
        /// <returns>True if replacements are completed, false otherwise.</returns>
        private bool setApsimModelParameters(dynamic mySimulation, string modelType, string modelName, List<string> parametersName, List<string> parametersValue)
        {
            // look for the model to be set up
            List<JObject> modelsToSetUp = findApsimSimulationModel("Models." + modelType, modelName, mySimulation);
            if (modelsToSetUp.Count == 0)
            {
                if (modelType == "Manager")
                    Console.WriteLine($" Could not find the manager \"{ modelName }\" in { mySimulation["Name"] } simulation");
                else
                    Console.WriteLine($" Could not find the model \"{ modelType }\" in { mySimulation["Name"] } simulation");
                return false;
            }

            // apply the changes to each instance of the model
            if (modelType == "Manager")
            {
                foreach (dynamic managerInstance in modelsToSetUp)
                {
                    for (int i = 0; i < parametersName.Count; i++)
                    {
                        foreach (dynamic managerItem in managerInstance["Parameters"])
                        {
                            if (managerItem["Key"] == parametersName[i])
                            {
                                if (parametersValue[i] != "")
                                {
                                    managerItem["Value"] = parametersValue[i];
                                }
                            }
                        }
                    }
                }
            }
            if (modelType == "Soils.Soil")
            {
                foreach (JObject soilInstance in modelsToSetUp)
                {
                    foreach (KeyValuePair<string, JToken> item in soilInstance)
                    {
                        var theKey = item.Key.ToString();
                        if(soilParameterisation.ContainsKey(parametersValue[0]))
                        {
                            soilInstance[theKey] = soilParameterisation[parametersValue[0]];
                        }
                    }
                }
            }
            else  // general model
            {
                foreach (dynamic modelInstance in modelsToSetUp)
                {
                    for (int i = 0; i < parametersName.Count; i++)
                    {
                        if (parametersValue[i] != "")
                        {
                            modelInstance[parametersName[i]] = parametersValue[i];
                        }
                    }
                }
            }

            return true;
        }

        /// <summary>Gathers the values of a given list of outputs from an SQLite DB file.</summary>
        /// <param name="apsimDBFileName">Name of the DB file with the outputs from an Apsim simulation.</param>
        /// <param name="DBTableName">Name of the table within the file which contains the required outputs.</param>
        /// <param name="variablesList">List of names of the required outputs (optional - apsimRequiredVariables used as default).</param>
        /// <returns>A DataTable with the values of the required outputs.</returns>
        public System.Data.DataTable gatherSimulatedData(string apsimDBFileName, string DBTableName, string[] variablesList = null)
        {
            string connectionQuery = $"Data Source = " + outputFilePath + apsimDBFileName;
            if (variablesList == null)
            {
                variablesList = apsimRequiredVariables;
            }
            string outputsQuery = createQueryString(DBTableName, variablesList);
            SQLiteConnection myConnection = new SQLiteConnection(connectionQuery);
            myConnection.Open();
            SQLiteDataAdapter myAdapter = new SQLiteDataAdapter(outputsQuery, myConnection);
            System.Data.DataTable myData = new System.Data.DataTable();
            myAdapter.Fill(myData);
            myConnection.Close();
            myAdapter.Dispose();
            return myData;
        }

        /// <summary>Creates a string to query a database.</summary>
        /// <param name="DBTable">Name of the table within the DB file which will be queried.</param>
        /// <param name="variablesList">List of names of variables being queried.</param>
        /// <returns>A string with the formatted query string.</returns>
        private string createQueryString(string DBTable, string[] variablesList)
        {
            StringBuilder querryString = new StringBuilder("Select ");
            foreach (var variableName in variablesList)
            {
                querryString.Append(variableName);
                querryString.Append(",");
            }

            querryString.Remove(querryString.Length - 1, 1);
            querryString.Append(" from ");
            querryString.Append(DBTable);

            return querryString.ToString();
        }

        /// <summary>Creates and initialises a netCDF file where simulated outputs will be saved.</summary>
        /// <param name="outputDataFileName">Name of the netCDF file to be created.</param>
        /// <returns>A connection to the netCDF file just created.</returns>
        private DataSet createNetCDFFileConnection(string outputDataFileName)
        {
            // create the connection
            DataSet myFileConnection = null;
            try
            {
                myFileConnection = DataSet.Open(outputFilePath + outputDataFileName, ResourceOpenMode.Create);
            }
            catch
            {
                Console.WriteLine($"  could not access {outputFilePath}{outputDataFileName}");
                return null;
            }

            // initialise the dimensions
            int i = 0;
            float missingValue = -9999F;
            myFileConnection.Add<short[]>(dimensionsNames[i], dimensionsNames[i]);
            myFileConnection[dimensionsNames[i]].Metadata["MissingValue"] = (short)missingValue;
            myFileConnection[dimensionsNames[i]].Metadata["Units"] = dimensionsUnits[i];
            for (i = 1; i < dimensionsNames.Length; i++)
            {
                myFileConnection.Add<float[]>(dimensionsNames[i], dimensionsNames[i]);
                myFileConnection[dimensionsNames[i]].Metadata["MissingValue"] = missingValue;
                myFileConnection[dimensionsNames[i]].Metadata["Units"] = dimensionsUnits[i];
            }

            // initialise the variables
            for (i = 0; i < outputVariablesNames.Length; i++)
            {
                myFileConnection.Add<float[,,]>(outputVariablesNames[i], dimensionsNames);
                myFileConnection[outputVariablesNames[i]].Metadata["MissingValue"] = missingValue;
                myFileConnection[outputVariablesNames[i]].Metadata["Units"] = outputVariablesUnits[i];
            }

            // add some metadata
            myFileConnection.Metadata["Name"] = "ApsimOutputs";
            myFileConnection.Metadata["DisplayName"] = "Outputs from Apsim model";

            myFileConnection.IsAutocommitEnabled = false;

            return myFileConnection;
        }

        /// <summary>Saves the data from a single Apsim simulation into a netCDF file.</summary>
        /// <param name="netCDFConnection">Connection to the netCDF file.</param>
        /// <param name="simulatedData">DataTable with the simulated data.</param>
        /// <returns>True if data is saved, false otherwise.</returns>
        private bool putDataToNetCDF(DataSet netCDFConnection, System.Data.DataTable simulatedData)
        {
            bool appendOK = true;

            // get the values for the time dimension
            int numRows = simulatedData.Rows.Count;
            short[] timesSimulated = new short[numRows];
            for (int t = 0; t < numRows; t++)
            {
                DateTime thisDate = DateTime.Parse(simulatedData.Rows[t]["Date"].ToString());
                timesSimulated[t] = (short)(thisDate - baseDate).TotalDays;
            }

            // get the values for the spatial dimensions
            float[] latitudesSimulated = new float[] { float.Parse(simulatedData.Rows[0]["Latitude"].ToString()) };
            float[] longitudesSimulated = new float[] { float.Parse(simulatedData.Rows[0]["Longitude"].ToString()) };

            // put the values for the dimensions in the file
            netCDFConnection.PutData(dimensionsNames[0], timesSimulated);
            netCDFConnection.PutData(dimensionsNames[1], latitudesSimulated);
            netCDFConnection.PutData(dimensionsNames[2], longitudesSimulated);

            // get the values for the variables and put them in the file
            for (int iVar = 0; iVar < outputVariablesNames.Length; iVar++)
            {
                string variableSimulatedName = apsimRequiredVariables[dimensionsNames.Length + iVar];
                float [,,] valuesSimulated = new float[numRows, 1, 1];
                for (int iRow = 0; iRow < numRows; iRow++)
                {
                    valuesSimulated[iRow, 0, 0] = float.Parse(simulatedData.Rows[iRow][variableSimulatedName].ToString());
                }

                netCDFConnection.PutData(outputVariablesNames[iVar], valuesSimulated);
            }

            netCDFConnection.Commit();

            return appendOK;
        }

        /// <summary>Appends the data from an Apsim simulation into a netCDF file.</summary>
        /// <remarks>Assumes that the time span for all simulations are the same (initialisation based on first simulation only).</remarks>
        /// <param name="netCDFConnection">Connection to the netCDF file.</param>
        /// <param name="simulatedData">DataTable with the simulated data.</param>
        /// <returns>True if data was appended, false otherwise.</returns>
        private bool appendDataToNetCDF(DataSet netCDFConnection, System.Data.DataTable simulatedData)
        {
            bool appendOK = true;
            int numRows = simulatedData.Rows.Count;
            int newDimensionFlag = 0; // flag which spatial dimension to append (0=none, 1=lat, 2=long, 3=both)

            // check the values of the time dimension
            short[] timesRecorded = netCDFConnection.GetData<short[]>("time");
            if (timesRecorded.Length < 1)
            {
                short[] timesSimulated = new short[numRows];
                for (int t = 0; t < numRows; t++)
                {
                    DateTime thisDate = DateTime.Parse(simulatedData.Rows[t]["Date"].ToString());
                    timesSimulated[t] = (short)(thisDate - baseDate).TotalDays;
                }

                netCDFConnection.Append(dimensionsNames[0], timesSimulated);
            }

            // check the values of the latitude dimension
            int iLat = -1;
            float[] latitudesRecorded = netCDFConnection.GetData<float[]>("latitude");
            float[] latitudesSimulated = new float[] { float.Parse(simulatedData.Rows[0]["Latitude"].ToString()) };
            if (latitudesRecorded.Contains(latitudesSimulated[0]))
            { // latitude already recorded
                iLat = Array.IndexOf(latitudesRecorded, latitudesSimulated[0]);
            }
            else
            { // location with new latitude
                newDimensionFlag += 1;
                iLat = latitudesRecorded.Length;
                netCDFConnection.Append(dimensionsNames[1], latitudesSimulated[0]);
            }

            // check the values of the longitude dimension
            int iLong = -1;
            float[] longitudesRecorded = netCDFConnection.GetData<float[]>("longitude");
            float[] longitudesSimulated = new float[] { float.Parse(simulatedData.Rows[0]["Longitude"].ToString()) };
            if (longitudesRecorded.Contains(longitudesSimulated[0]))
            { // longitude already recorded
                iLong = Array.IndexOf(longitudesRecorded, longitudesSimulated[0]);
            }
            else
            { // location with new longitude
                newDimensionFlag += 2;
                iLong = longitudesRecorded.Length;
                netCDFConnection.Append(dimensionsNames[2], longitudesSimulated[0]);
            }

            // get the values for the variables and put them in the file
            for (int iVar = 0; iVar < outputVariablesNames.Length; iVar++)
            {
                string variableSimulatedName = apsimRequiredVariables[dimensionsNames.Length + iVar];
                float[,,] valuesSimulated;
                if (newDimensionFlag == 0)
                { // location given already exist, replace missing values with new values
                    valuesSimulated = netCDFConnection.GetData<float[,,]>(outputVariablesNames[iVar]);
                    for (int iRow = 0; iRow < numRows; iRow++)
                    {
                        valuesSimulated[iRow, iLat, iLong] = float.Parse(simulatedData.Rows[iRow][variableSimulatedName].ToString());
                    }

                    netCDFConnection.PutData(outputVariablesNames[iVar], valuesSimulated);
                }
                else if (newDimensionFlag == 1)
                { // new simulated latitude, append new 'data row', add values for given location and set as missing for all others
                    valuesSimulated = new float[numRows, 1, longitudesRecorded.Length];
                    for (int iRow = 0; iRow < numRows; iRow++)
                    {
                        for (int i = 0; i < longitudesRecorded.Length; i++)
                        {
                            if (i == iLong)
                            {
                                valuesSimulated[iRow, 0, i] = float.Parse(simulatedData.Rows[iRow][variableSimulatedName].ToString());
                            }
                            else
                            {
                                valuesSimulated[iRow, 0, i] = missingValue;
                            }
                        }
                    }

                    netCDFConnection.Append(outputVariablesNames[iVar], valuesSimulated, dimensionsNames[1]);
                }
                else if (newDimensionFlag == 2)
                { // new simulated longitude, append new 'data column', add values to given location, and set as missing for all others
                    valuesSimulated = new float[numRows, latitudesRecorded.Length, 1];
                    for (int iRow = 0; iRow < numRows; iRow++)
                    {
                        for (int i = 0; i < latitudesRecorded.Length; i++)
                        {
                            if (i == iLat)
                            {
                                valuesSimulated[iRow, i, 0] = float.Parse(simulatedData.Rows[iRow][variableSimulatedName].ToString());
                            }
                            else
                            {
                                valuesSimulated[iRow, i, 0] = missingValue;
                            }
                        }
                    }

                    netCDFConnection.Append(outputVariablesNames[iVar], valuesSimulated, dimensionsNames[2]);
                }
                else
                { // new simulated location, append new 'data row and column', add values to given location, and set as missing to all others
                    valuesSimulated = new float[numRows, 1, longitudesRecorded.Length];
                    for (int iRow = 0; iRow < numRows; iRow++)
                    {
                        for (int i = 0; i < longitudesRecorded.Length; i++)
                        {
                            valuesSimulated[iRow, 0, i] = missingValue;
                        }
                    }

                    netCDFConnection.Append(outputVariablesNames[iVar], valuesSimulated, dimensionsNames[1]);

                    valuesSimulated = new float[numRows, latitudesRecorded.Length + 1, 1];
                    for (int iRow = 0; iRow < numRows; iRow++)
                    {
                        for (int i = 0; i < latitudesRecorded.Length; i++)
                        {
                            valuesSimulated[iRow, i, 0] = missingValue;
                        }

                        valuesSimulated[iRow, iLat, 0] = float.Parse(simulatedData.Rows[iRow][variableSimulatedName].ToString());
                    }

                    netCDFConnection.Append(outputVariablesNames[iVar], valuesSimulated, dimensionsNames[2]);
                }
            }

            netCDFConnection.Commit();

            return appendOK;
        }

        #endregion
    }
}
