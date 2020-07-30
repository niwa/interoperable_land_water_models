using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using Microsoft.Research.Science.Data;
using Microsoft.Research.Science.Data.Imperative;

namespace ApsimWeatherConverter
{
    /// <summary>Methods for converting NIWA's weather data from netCDF to Apsim's .met format.</summary>
    public class NetcdfToMet
    {
        #region Internal variables

        /// <summary>List of weather variable names to use in error messages.</summary>
        private static string[] myWeatherVariableNames = new string[] { "Rainfall", "Minimum temperature", "Maximum temperature", "Solar radiation", "Wind speed", "PET", "Vapour pressure", "RH" };
        /// <summary>List of weather variable names as supplied by NIWA.</summary>
        private static string[] weatherVariableNamesNIWA = new string[] { "rain", "tmin", "tmax", "srad", "wind", "pet", "?", "rh" };
        /// <summary>List of units for each weather variable as supplied by NIWA.</summary>
        private static string[] weatherVariableUnitsNIWA = new string[] { "(mm)", "(K)", "(K)", "(W/m^2)", "(m/s)", "(mm)", "mbar", "(-)" };
        /// <summary>List of weather variable names as required by Apsim.</summary>
        private static string[] weatherVariableNamesAPSIM = new string[] { "rain", "mint", "maxt", "radn", "wind", "evap", "vp", "?" };
        /// <summary>List of units for each weather variable as required by Apsim.</summary>
        private static string[] weatherVariableUnitsAPSIM = new string[] { "(mm)", "(oC)", "(oC)", "(MJ/m^2)", "(m/s)", "(mm)", "mbar", "(-)" };
        /// <summary>Minimum value for each weather variable.</summary>
        private static float[] minimumValue = new float[] { 0.0F, -100.0F, -50.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F };
        /// <summary>Maximum value for each weather variable.</summary>
        private static float[] maximumValue = new float[] { 500.0F, 50.0F, 100.0F, 75.0F, 50.0F, 30.0F, 100.0F, 100.0F };
        /// <summary>Offset value for correcting each weather variable from NIWA to APSIM standards.</summary>
        private static float[] offsetValue = new float[] { 0.0F, -273.15F, -273.15F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F };
        /// <summary>Multiplier value for correcting each weather variable from NIWA to APSIM standards.</summary>
        private static float[] multiplierValue = new float[] { 1.0F, 1.0F, 1.0F, 0.0864F, 1.0F, 1.0F, 1.0F, 1.0F };
        /// <summary>The tolerance used when testing coordinates.</summary>
        /// <remarks>NIWA's grid is spaced by 0.05 degrees.</remarks>
        private static float coordinateTolerance = 0.001F;

        /// <summary>List of full paths of the weather files (netCDF).</summary>
        private static string[] weatherFiles;
        /// <summary>List of variables as given in the weatherFiles (should be a subset of dataTypesNIWA).</summary>
        private static string[] variablesInFile;
        /// <summary>List of the index for each variable being considered (matches for the default list).</summary>
        private static int[] variablesIndex;
        /// <summary>Lower time for which weather data is available, across all original files.</summary>
        private static int lowerTimeBound;
        /// <summary>Upper time for which weather data is available, across all original files.</summary>
        private static int upperTimeBound;
        /// <summary>List of coordinates (latitude and longitude) of the locations to output met file data.</summary>
        private static List<Coordinates> givenLocationsCoordinates;
        /// <summary>List of coordinates (latitude and longitude) of all locations with the weather data to output.</summary>
        private static List<Coordinates> weatherLocationsCoordinates = new List<Coordinates>();
        /// <summary>Path to the folder where the files with formatted data will be saved.</summary>
        private static string outputFolderPath;
        /// <summary>List of MetData structures with the weather data for each location.</summary>
        private static MetData[] weatherDatasets;

        #endregion

        #region Public methods

        /// <summary>Gets or sets the tolerance used to compare coordinates (latitude and longitude).</summary>
        public static double CoordinatesTolerance
        {
            get { return coordinateTolerance; }
            set { coordinateTolerance = (float)value; }
        }

        /// <summary>Convert the weather data for all locations in the netCDF files into .met files, ready for Apsim.</summary>
        /// <remarks>This converts all the data, thus creating a met file for each location for which data is available.</remarks>
        /// <param name="inputPath">Path to the folder containing the netCDF files with the weather data.</param>
        /// <param name="inputFileNames">List of file names with data for each weather variable.</param>
        /// <param name="outputPath">Folder in which the met files will be saved (optional).</param>
        /// <param name="fileFormat">Flag for the type/format of the file to be saved, "csv" or "met" (optional).</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files (optional).</param>
        /// <param name="skipLocationsWithMissingValues">Flag whether locations that do not have valid values should be ignored (optional).</param>
        /// <param name="checkAndFillWindData">Flag whether missing wind data before 1997 is filled in with averages (optional).</param>
        public static void ConvertFiles(string inputPath, string[] inputFileNames, string outputPath = "",
                                        string fileFormat = "met", bool matchTimeSpan = true, 
                                        bool skipLocationsWithMissingValues = true, bool checkAndFillWindData = false)
        {
            //Check that the input files exist and initialise the basic variable of the Converter
            initialiseConverter(inputPath, inputFileNames, outputPath, new float[] { -999F, -999F }, matchTimeSpan, skipLocationsWithMissingValues);

            //Call the methods to read and format the data (placed into weatherDatasets)
            weatherDatasets = new MetData[weatherLocationsCoordinates.Count];
            readNetCDFWeatherFiles(checkAndFillWindData);

            //Save the data
            foreach (MetData siteData in weatherDatasets)
            {
                string outputFile = $"weatherData({siteData.LatitudeNominal.ToString("#0.000")}_{siteData.LongitudeNominal.ToString("#0.000")})";
                writeWeatherData(siteData, outputPath, outputFile, fileFormat);
            }
        }

        /// <summary>Convert the weather data for a given location from netCDF files into a .met file, ready for Apsim.</summary>
        /// <remarks>This searches for a given location and then converts its data, thus creating a single met file.</remarks>
        /// <param name="inputPath">Path to the folder containing the netCDF files with the weather data.</param>
        /// <param name="inputFileNames">List of file names with data for each weather variable.</param>
        /// <param name="coordinates">Latitude and longitude (decimal degrees) of the location.</param>
        /// <param name="outputPath">Folder in which the met file will be saved (optional).</param>
        /// <param name="outputFileName">Name of the met file to be written (optional).</param>
        /// <param name="fileFormat">Flag for the type/format of the file to be saved, "csv" or "met" (optional).</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files (optional).</param>
        /// <param name="checkAndFillWindData">Flag whether missing wind data before 1997 is filled in with averages (optional).</param>
        public static void ConvertWeather(string inputPath, string[] inputFileNames, float[] coordinates,
                                          string outputPath = "", string outputFileName = "", string fileFormat = "met",
                                          bool matchTimeSpan = true, bool checkAndFillWindData = false)
        {
            //Check that the input files exist and initialise the basic variable of the Converter
            initialiseConverter(inputPath, inputFileNames, outputPath, coordinates, matchTimeSpan, true);

            //Call the methods to read and format the data
            weatherDatasets = new MetData[weatherLocationsCoordinates.Count];
            readNetCDFWeatherFiles(checkAndFillWindData);

            //Check file name and save the data
            if (outputFileName == "")
            {
                outputFileName = $"weatherData({weatherDatasets[0].LatitudeOrigin.ToString("#0.000")}_{weatherDatasets[0].LongitudeOrigin.ToString("#0.000")})";
            }

            if (fileFormat == "met")
            {
                if (!outputFileName.EndsWith(".met"))
                {
                    outputFileName = $"{outputFileName}.met";
                }
            }
            else
            { // check extension
                if (!outputFileName.EndsWith(".csv"))
                {
                    outputFileName = $"{outputFileName}.csv";
                }
            }

            writeWeatherData(weatherDatasets[0], outputPath, outputFileName, fileFormat);
        }

        /// <summary>Gets and converts the weather data for a given location from netCDF files into a format ready for Apsim.</summary>
        /// <remarks>This searches for a given location, gets and converts its data, thus creating a single met dataset.</remarks>
        /// <param name="inputPath">Path to the folder containing the netCDF files with the weather data.</param>
        /// <param name="inputFileNames">List of file names with data for each weather variable.</param>
        /// <param name="locationCoordinates">Latitude and longitude (decimal degrees) of the location.</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files (optional).</param>
        /// <param name="checkAndFillWindData">Flag whether missing wind data before 1997 is filled in with averages (optional).</param>
        /// <returns>A string with the weather data formatted in Apsim .met format.</returns>
        public static string GetWeatherData(string inputPath, string[] inputFileNames, float[] coordinates,
                                            bool matchTimeSpan = true, bool checkAndFillWindData = false)
        {
            string metFileData = "";

            if (coordinates.Length == 2)
            {
                //Check that the input files exist and initialise the basic variable of the Converter
                initialiseConverter(inputPath, inputFileNames, inputPath, coordinates, matchTimeSpan, true);

                //Call the methods to read and format the data
                weatherDatasets = new MetData[weatherLocationsCoordinates.Count];
                readNetCDFWeatherFiles(checkAndFillWindData);

                //call the method to put the data into .met format
                metFileData = getMetData(weatherDatasets[0]);

                return metFileData;
            }
            else
            {
                throw new Exception("The method GetWeatherData needs one and only one location (latitude and longitude).");
            }
        }

        /// <summary>Gets and writes the basic information about all locations in the weather data files.</summary>
        /// <remarks>
        /// The info gathered includes:
        ///  - the list of locations (latitude and longitude);
        ///  - the time interval for which the data is available;
        ///  - the summary of each variable (count, min-max, average);
        /// These are given for each variable and each location in the files.
        ///  </remarks>
        /// <param name="inputPath">Path to the folder containing the netCDF files with the weather data.</param>
        /// <param name="inputFileNames">List of file names with data for each weather variable.</param>
        /// <param name="outputPath">Folder in which the met file will be saved (optional).</param>
        /// <param name="outputFileName">Name of the summary file to be written (optional).</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files (optional).</param>
        /// <param name="summariseLocations">Flag whether to output the summary for each individual location (optional).</param>
        /// <param name="skipLocationsWithMissingValues">Flag whether locations that do not have valid values should be ignored (optional).</param>
        /// <param name="allowVerboseSummary">Flag whether to output detailed metadata info.</param>
        public static void GetDataInfo(string inputPath, string[] inputFileNames,
                                       string outputPath = "", string outputFileName = "",
                                       bool matchTimeSpan = true, bool summariseLocations = true,
                                       bool skipLocationsWithMissingValues = true, bool allowVerboseSummary = false)
        {
            //Check that the input files exist and initialise the basic variable of the Converter
            initialiseConverter(inputPath, inputFileNames, outputPath, new float[] { -999F, -999F }, matchTimeSpan, skipLocationsWithMissingValues);

            //Get the summary data
            List<string> summaryData = getSummaryData(summariseLocations, allowVerboseSummary);

            //Save the outputs
            if (outputFileName == "")
            {
                string fileTag;
                if (weatherFiles.Length == 1)
                {
                    fileTag = weatherVariableNamesAPSIM[Array.IndexOf(weatherVariableNamesNIWA, variablesInFile[0])];
                }
                else
                {
                    fileTag = "AllVariables";
                }
                outputFileName = $"DataSummary({fileTag})(AllLocations).txt";
            }

            using (StreamWriter outFile = new StreamWriter(outputPath + outputFileName))
            {
                foreach (var dataItem in summaryData)
                {
                    outFile.WriteLine(dataItem);
                    outFile.WriteLine();
                }
            }
        }

        /// <summary>Gets and writes the basic information for a given location in the weather data files.</summary>
        /// <remarks>
        /// The info gathered includes:
        ///  - the list of locations (latitude and longitude);
        ///  - the time interval for which the data is available;
        ///  - the summary of each variable (count, min-max, average);
        ///  </remarks>
        /// <param name="inputPath">Path to the folder containing the netCDF files with the weather data.</param>
        /// <param name="inputFileNames">List of file names with data for each weather variable.</param>
        /// <param name="coordinates">Latitude and longitude (decimal degrees) of the location.</param>
        /// <param name="outputPath">Folder in which the met file will be saved (optional).</param>
        /// <param name="outputFileName">Name of the summary file to be written (optional).</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files (optional).</param>
        /// <param name="allowVerboseSummary">Flag whether to output detailed metadata info (optional).</param>
        public static void GetDataInfo(string inputPath, string[] inputFileNames, float[] coordinates,
                                       string outputPath = "", string outputFileName = "",
                                       bool matchTimeSpan = true, bool allowVerboseSummary = false)
        {
            //Check that the input files exist and initialise the basic variable of the Converter
            initialiseConverter(inputPath, inputFileNames, outputPath, coordinates, matchTimeSpan, true);

            //Get the summary data
            List<string> summaryData = getSummaryData(false, allowVerboseSummary);

            //Save the outputs
            if (outputFileName == "")
            {
                string fileTag;
                if (weatherFiles.Length == 1)
                {
                    fileTag = weatherVariableNamesAPSIM[Array.IndexOf(weatherVariableNamesNIWA, variablesInFile[0])];
                }
                else
                {
                    fileTag = "AllVariables";
                }
                outputFileName = $"DataSummary({fileTag})({givenLocationsCoordinates[0].Latitude.ToString("#0.000")}_{givenLocationsCoordinates[0].Longitude.ToString("#0.000")}).txt";
            }

            using (StreamWriter outFile = new StreamWriter(outputPath + outputFileName))
            {
                foreach (var dataItem in summaryData)
                {
                    outFile.WriteLine(dataItem);
                    outFile.WriteLine();
                }
            }
        }

        #endregion

        #region Private methods

        /// <summary>Checks the inputs and initialise basic variable for the Converter.</summary>
        /// <remarks>
        /// This method performs the following steps:
        ///  - Check that the input folder exist and it has the required netCDF files;
        ///  - Check that the output path exist, if not given used inputPath, else throw exception;
        ///  - Check the time span in all files, gathering the lower and upper time bounds;
        ///  - Check and/or gather the coordinates for the locations available in the files;
        /// </remarks>
        /// <param name="inputPath">Path to the folder containing the netCDF files with the weather data.</param>
        /// <param name="inputFileNames">List of file names with data for each weather variable.</param>
        /// <param name="outputPath">Folder in which the met files will be saved.</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files.</param>
        /// <param name="skipLocationsWithMissingValues">Flag whether locations that do not have valid values should be ignored.</param>
        private static void initialiseConverter(string inputPath, string[] inputFileNames, string outputPath, float[] coordinates, bool matchTimeSpan, bool skipLocationsWithMissingValues)
        {
            //Check that the files with the weather data exist in the location given. Stores the paths in 'weatherFiles'
            string checkMessage = checkInputFiles(inputPath, inputFileNames, matchTimeSpan);
            if (checkMessage != "AllGood")
            {
                throw new Exception(checkMessage);
            }

            //Prepare output (NOTE: this does not check whether the old files will be always overwritten or not)
            if (outputPath == "")
            {
                outputFolderPath = inputPath;
            }
            else
            {
                if (Directory.Exists(outputPath))
                {
                    outputFolderPath = outputPath;
                }
                else
                {
                    throw new Exception($"The output folder \"{outputPath}\" was not found");
                    //Directory.CreateDirectory(outputFolderPath);
                }
            }

            //Check the time span across the weather data files
            if ((upperTimeBound - lowerTimeBound) < 0)
            {
                throw new Exception("The time span for which data is available across the netCDF files is zero.");
            }

            //Get and check the coordinates of all available locations
            weatherLocationsCoordinates = getLocationCoordinates(coordinates[0], coordinates[1], skipLocationsWithMissingValues);
            if (weatherLocationsCoordinates.Count < 1)
            {
                if (coordinates[0] <= -998f)
                {
                    throw new Exception("No location was found with data for all required variables.");
                }
                else
                {
                    throw new Exception($"Location with coordinates given ({coordinates[0].ToString("#0.000")},{coordinates[1].ToString("#0.000")}) was not found in all netCDF files.");
                }
            }
            else
            { // save values of given coordinates (will be used to write outputs)
                if (coordinates[0] < -998f)
                { // no coordinates given, using all available in the netCDF files
                    givenLocationsCoordinates = weatherLocationsCoordinates.ToList();
                }
                else
                { // one coordinate given, use it
                    givenLocationsCoordinates = new List<Coordinates>();
                    Coordinates newCoordinate = new Coordinates(coordinates[0], coordinates[1]);
                    givenLocationsCoordinates.Add(newCoordinate);
                }
            }
        }

        /// <summary>Checks that the folder and files with the weather data exist.</summary>
        /// <remarks>Also gets the variable names and the time span available in the files.</remarks>
        /// <param name="inputPath">Path to the folder containing the files with the weather data.</param>
        /// <param name="inputFiles">List of netCDF files with each data type.</param>
        /// <param name="matchTimeSpan">Flag whether to convert data only for dates present in all files.</param>
        /// <returns>A message with the error found, or 'AllGood' if all checks out.</returns>
        private static string checkInputFiles(string inputPath, string[] inputFiles, bool matchTimeSpan)
        {
            // set some initial values
            string checkMessage = "AllGood";
            if (matchTimeSpan)
            {
                lowerTimeBound = 0;
                upperTimeBound = 1000000;
            }
            else
            {
                lowerTimeBound = 1000000;
                upperTimeBound = 0;
            }
            List<string> tempWeatherFiles = new List<string>();
            List<string> tempWeaterVariable = new List<string>();
            List<int> tempVariableOrder = new List<int>();

            // check the folder
            if (Directory.Exists(inputPath))
            {
                // check each file and add to list
                for (int i = 0; i < inputFiles.Length; i++)
                {
                    if (File.Exists(inputPath + inputFiles[i]))
                    {
                        // store path to the file
                        tempWeatherFiles.Add(inputPath + inputFiles[i]);

                        // get the variable stored in the file
                        var inputDataset = DataSet.Open(tempWeatherFiles[i]);
                        var variablesList = inputDataset.Variables;
                        foreach (Variable variable in variablesList)
                        {
                            if (variable.Dimensions.Count >= 3)
                            {
                                int varIndex = Array.IndexOf(weatherVariableNamesNIWA, variable.Name);
                                if (varIndex >= 0)
                                {
                                    tempWeaterVariable.Add(variable.Name);
                                    tempVariableOrder.Add(varIndex);
                                }
                                else
                                {
                                    checkMessage = $"The variable \"{variable.Name}\" found in file \"{tempWeatherFiles[i]}\" is not a valid weather variable.";
                                    i = inputFiles.Length;
                                    break;
                                }
                            }
                        }

                        // get the time span of this file
                        var timeRead = inputDataset.GetData<short[]>("time");
                        if (matchTimeSpan)
                        {
                            lowerTimeBound = Math.Max(lowerTimeBound, timeRead.Min());
                            upperTimeBound = Math.Min(upperTimeBound, timeRead.Max());
                        }
                        else
                        {
                            lowerTimeBound = Math.Min(lowerTimeBound, timeRead.Min());
                            upperTimeBound = Math.Max(upperTimeBound, timeRead.Max());
                        }
                    }
                    else
                    {
                        checkMessage = $"The netCDF file \"{inputFiles[1]}\" with {myWeatherVariableNames[i]} data was not found on \"{inputPath}\"";
                        break;
                    }
                }

                // save the file names in the order given by dataTypes
                List<int> sortedList = tempVariableOrder.OrderBy(v => v).ToList();
                weatherFiles = new string[tempWeatherFiles.Count];
                variablesInFile = new string[tempWeatherFiles.Count];
                variablesIndex = new int[tempWeatherFiles.Count];
                for (int i = 0; i < tempWeatherFiles.Count; i++)
                {
                    int tempIndex = tempVariableOrder.IndexOf(sortedList[i]);
                    weatherFiles[i] = tempWeatherFiles[tempIndex];
                    variablesInFile[i] = tempWeaterVariable[tempIndex];
                    variablesIndex[i] = sortedList[i];
                }
            }
            else
            {
                checkMessage = $"The folder \"{inputPath}\" with netCDF files was not found";
            }

            return checkMessage;
        }

        /// <summary>Gets the list of locations (latitude and longitude) that exist in the netCDF weather files.</summary>
        /// <param name="latitudeSought">Latitude of the location sought after.</param>
        /// <param name="longitudeSought">Longitude of the location sought after.</param>
        /// <param name="skippingLocationsWithMissingValues">Flag whether locations that do not have valid values should be ignored.</param>
        /// <returns>A list with the latitude and longitude of locations found in the weather files.</returns>
        /// <remarks>
        /// If values for latitude and longitude are given, the returned list contains one item only, provided that the 
        ///  location exists in all the files. Otherwise (i.e. both latitude and longitude are equal to -999), the list contains 
        ///  the coordinates of all points in the netCDF files are retuned, except if skippingLocationsWithMissingValues.
        /// If skippingLocationsWithMissingValues, locations for which there are no data in all variables are discarded.
        /// If the number of locations is not the same in all netCDF files or the location is not found, an exception is thrown.
        /// </remarks>
        private static List<Coordinates> getLocationCoordinates(float latitudeSought, float longitudeSought, bool skippingLocationsWithMissingValues)
        {
            List<Coordinates> savedLocations = new List<Coordinates>();

            for (int i = 0; i < weatherFiles.Length; i++)
            {
                var inputDataset = DataSet.Open(weatherFiles[i]);
                var latitudesList = inputDataset.GetData<float[]>("latitude");
                var longitudesList = inputDataset.GetData<float[]>("longitude");
                short[] timesList = null;
                float[,,] valuesList = null;
                int iniIndex = -1;
                int maxIndex = -1;
                if (skippingLocationsWithMissingValues)
                {
                    timesList = inputDataset.GetData<short[]>("time");
                    valuesList = inputDataset.GetData<float[,,]>(variablesInFile[i]);
                    iniIndex = Math.Max(0, Array.IndexOf(timesList, lowerTimeBound));
                    maxIndex = Math.Max(timesList.Length, Array.IndexOf(timesList, upperTimeBound));
                }

                if ((latitudeSought < -998) || (longitudeSought < -998))
                { // No particular locations is being sought, gather all locations in the netCDF files
                    List<Coordinates> temporaryLocations = new List<Coordinates>();
                    for (int iLat = 0; iLat < latitudesList.Length; iLat++)
                    {
                        for (int iLong = 0; iLong < longitudesList.Length; iLong++)
                        {
                            if (skippingLocationsWithMissingValues)
                            { // skip location if it has only missing values (-9999)
                                for (int t = iniIndex; t < maxIndex; t++)
                                {
                                    if (valuesList[t, iLat, iLong] > -9998)
                                    { // we have at least one valid value
                                        temporaryLocations.Add(new Coordinates(latitudesList[iLat], longitudesList[iLong]));
                                        break;
                                    }
                                }
                            }
                            else
                            {
                                temporaryLocations.Add(new Coordinates(latitudesList[iLat], longitudesList[iLong]));
                            }
                        }
                    }

                    int checkedLocationsCount = 0;
                    if (savedLocations.Count == 0)
                    { //assign base locations
                        foreach (var location in temporaryLocations)
                        {
                            savedLocations.Add(location);
                            checkedLocationsCount += 1;
                        }
                    }
                    else
                    {
                        // get the list of locations that are not matched in both the saved location list and that a given variable
                        List<Coordinates> additionalLocations = temporaryLocations.Except(savedLocations, new LocationComparer()).ToList();
                        List<Coordinates> excessLocations = savedLocations.Except(temporaryLocations, new LocationComparer()).ToList();
                        if (skippingLocationsWithMissingValues)
                        { // only keep the values that are in both lists
                            savedLocations = savedLocations.Except(excessLocations).ToList();
                        }
                        else
                        { // add any new location
                            savedLocations.AddRange(additionalLocations);
                        }
                    }
                }
                else
                { // check that the location given exists in the netCDF files
                    int coordinatesFound = 0;
                    Coordinates foundLocation = new Coordinates(0, 0);
                    for (int iLat = 0; iLat < latitudesList.Length; iLat++)
                    {
                        if (Math.Abs(latitudesList[iLat] - latitudeSought) < coordinateTolerance)
                        {
                            coordinatesFound += 1;
                            foundLocation.Latitude = latitudesList[iLat];
                            break;
                        }
                    }
                    for (int iLong = 0; iLong < longitudesList.Length; iLong++)
                    {
                        if (Math.Abs(longitudesList[iLong] - longitudeSought) < coordinateTolerance)
                        {
                            coordinatesFound += 1;
                            foundLocation.Longitude = longitudesList[iLong];
                            break;
                        }
                    }

                    if (coordinatesFound == 2)
                    {
                        if (savedLocations.Count == 0)
                        {
                            savedLocations.Add(foundLocation);
                        }
                        //else, do nothing point already checked in
                    }
                    else
                    {
                        savedLocations.Clear();
                    }
                }

                if (savedLocations.Count == 0)
                {
                    throw new Exception($"The location with coordinates ({latitudeSought.ToString()}, {longitudeSought.ToString()}) was not found in the file {Path.GetFileName(weatherFiles[i])}!");
                }
            }

            return savedLocations;
        }

        /// <summary>Reads the weather files (netCDF) and extract the data for Apsim.</summary>
        /// <param name="checkAndFillWindData">Flag whether missing wind data before 1997 is filled in with averages.</param>
        /// <remarks>
        /// Will get the data for all sites in locationsCoordinates and put them in MetData structures.
        /// Will convert the data from NIWA's units to those required by APSIM.
        /// </remarks>
        private static void readNetCDFWeatherFiles(bool checkAndFillWindData)
        {
            // assumes all dates start in "1959-12-31 09:00:00.0 +12:00"
            DateTime baseDate = new DateTime(1959, 12, 31);

            // go through each weather variable file and get its values (time series) for each location
            for (int i = 0; i < weatherFiles.Length; i++)
            {
                // read the original data (netCDF)
                var inputDataset = DataSet.Open(weatherFiles[i]);
                var latitudeRead = inputDataset.GetData<float[]>("latitude");
                var longitudeRead = inputDataset.GetData<float[]>("longitude");
                var timeRead = inputDataset.GetData<short[]>("time");
                var dataRead = inputDataset.GetData<float[,,]>(variablesInFile[i]);
                inputDataset.Dispose();

                int varIndex = Array.IndexOf(weatherVariableNamesNIWA, variablesInFile[i]);
                int tt = 0;

                // go through each location given (could be only one)
                for (int iLoc = 0; iLoc < weatherLocationsCoordinates.Count; iLoc++)
                {
                    // get indices of selected location
                    int iLat = Array.IndexOf(latitudeRead, weatherLocationsCoordinates[iLoc].Latitude);
                    int iLong = Array.IndexOf(longitudeRead, weatherLocationsCoordinates[iLoc].Longitude);

                    // get the number of data points for the selected location
                    int nData = upperTimeBound - lowerTimeBound + 1;
                    if (weatherDatasets[iLoc] == null)
                    {
                        // initialise and add basic info
                        weatherDatasets[iLoc] = new MetData(variablesIndex, nData);
                        weatherDatasets[iLoc].LatitudeOrigin = weatherLocationsCoordinates[iLoc].Latitude;
                        weatherDatasets[iLoc].LongitudeOrigin = weatherLocationsCoordinates[iLoc].Longitude;
                        weatherDatasets[iLoc].LatitudeNominal = givenLocationsCoordinates[iLoc].Latitude;
                        weatherDatasets[iLoc].LongitudeNominal = givenLocationsCoordinates[iLoc].Longitude;
                        weatherDatasets[iLoc].Name = "None"; // could be the 'agent' ID

                        // add in the dates
                        tt = 0;
                        for (int recTime = lowerTimeBound; recTime <= upperTimeBound; recTime++)
                        {
                            weatherDatasets[iLoc].DateRecorded[tt] = baseDate.AddDays(recTime);
                            tt += 1;
                        }

                        // compute the auxiliary date variables
                        weatherDatasets[iLoc].SetDateRecords();
                    }

                    // get the data for the weather variable
                    var myVariable = weatherDatasets[iLoc].GetVariable(varIndex);
                    tt = 0;
                    for (int bt = 0; bt < timeRead.Length; bt++)
                    {
                        if ((timeRead[bt] >= lowerTimeBound) && (timeRead[bt] <= upperTimeBound))
                        {

                            if (dataRead[bt, iLat, iLong] > -9998)
                            { // do the appropriate conversion and check bounds
                                myVariable[tt] = dataRead[bt, iLat, iLong] * multiplierValue[varIndex] + offsetValue[varIndex];
                                myVariable[tt] = Math.Max(minimumValue[varIndex], Math.Min(maximumValue[varIndex], myVariable[tt]));
                            }
                            else
                            {
                                myVariable[tt] = float.NaN;
                            }

                            tt += 1;
                        }
                    }
                }
            }

            // final checks
            foreach (MetData siteData in weatherDatasets)
            {
                // check temperature and compute Apsim parameters (tav and amp)
                siteData.CheckApsimTemperatureParams();
                // check and fill the values of wind speed for 'old' dates
                if (checkAndFillWindData)
                {
                    siteData.CheckApsimWindspeed();
                }
            }
        }

        /// <summary>Writes the weather data in the appropriate file format.</summary>
        /// <param name="siteData">Dataset with the weather info for a given site.</param>
        /// <param name="outputPath">Directory path where the met file will be saved.</param>
        /// <param name="outputFileName">Name of the met file being saved.</param>
        /// <param name="fileFormat">Flag the type/format of file being saved ("csv" or "met").</param>
        private static void writeWeatherData(MetData siteData, string outputPath, string outputFileName, string fileFormat = "met")
        {
            // write output data
            if (fileFormat == "csv")
            {
                var Outputdataset = DataSet.Open(outputPath + outputFileName, ResourceOpenMode.Create);
                Outputdataset.Add<string[]>("date");
                Outputdataset.Add<int[]>("year");
                Outputdataset.Add<int[]>("month");
                Outputdataset.Add<int[]>("day");
                for (int i = 0; i < weatherFiles.Length; i++)
                {
                    Outputdataset.Add<float[]>(weatherVariableNamesAPSIM[variablesIndex[i]]);
                }

                Outputdataset.PutData("date", siteData.TheDateClassic);
                Outputdataset.PutData("year", siteData.TheYear);
                Outputdataset.PutData("month", siteData.TheMonth);
                Outputdataset.PutData("day", siteData.TheDayOfYear);
                for (int i = 0; i < weatherFiles.Length; i++)
                {
                    Outputdataset.PutData(weatherVariableNamesAPSIM[variablesIndex[i]], siteData.GetVariable(variablesIndex[i]));
                }
                Outputdataset.Dispose();
            }
            else
            { // write a .met file formatted for Apsim
                string metFileData = getMetData(siteData);
                using (StreamWriter outFile = new StreamWriter(outputPath + outputFileName))
                {
                    outFile.WriteLine(metFileData);
                }
            }
        }

        /// <summary>Gets the Apsim-formatted weather data for a given location.</summary>
        /// <param name="siteData">Dataset with the weather info for a given site.</param>
        /// <returns>A string with the formatted data.</returns>
        private static string getMetData(MetData siteData)
        {
            // write the main header
            StringBuilder metData = new StringBuilder("[weather.met.weather]");
            metData.AppendLine();
            metData.AppendLine();
            metData.AppendFormat("!Location:{0}", Environment.NewLine);
            metData.AppendFormat("Latitude = {0,-8:F3}  (DecimalDegrees){1}", siteData.LatitudeNominal, Environment.NewLine);
            metData.AppendFormat("Longitude = {0,-8:F3} (DecimalDegrees){1}", siteData.LongitudeNominal, Environment.NewLine);
            metData.AppendFormat("!Origin of data:{0}", Environment.NewLine);
            metData.AppendFormat("! Latitude = {0,-8:F3}  (DecimalDegrees){1}", siteData.LatitudeOrigin, Environment.NewLine);
            metData.AppendFormat("! Longitude = {0,-8:F3} (DecimalDegrees){1}", siteData.LongitudeOrigin, Environment.NewLine);
            metData.AppendLine();
            metData.AppendFormat("!Apsim general parameters:{0}", Environment.NewLine);
            metData.AppendFormat("tav = {0,5:F2}       !Annual average ambient temperature (oC){1}", siteData.Tav, Environment.NewLine);
            metData.AppendFormat("amp = {0,5:F2}       !Annual amplitude in mean monthly temperature (oC){1}", siteData.Amp, Environment.NewLine);
            metData.AppendLine();

            // write the data header
            metData.AppendFormat("{0,-12} {1,6} {2,5} {3,5}", "xDate", "year", "month", "day");
            for (int i = 0; i < weatherFiles.Length; i++)
            {
                metData.AppendFormat("{0,9}", weatherVariableNamesAPSIM[variablesIndex[i]]);
            }
            metData.AppendLine();

            // write the data units
            metData.AppendFormat("{0,-12} {1,6} {2,5} {3,5} ", "()", "()", "()", "()");
            for (int i = 0; i < weatherFiles.Length; i++)
            {
                metData.AppendFormat("{0,9}", weatherVariableUnitsAPSIM[variablesIndex[i]]);
            }
            metData.AppendLine();

            // write the data
            for (int t = 0; t < siteData.DateRecorded.Length; t++)
            {
                metData.AppendFormat("{0,-12} {1,6} {2,5} {3,5}", siteData.TheDateClassic[t], siteData.TheYear[t], siteData.TheMonth[t], siteData.TheDayOfYear[t]);
                for (int i = 0; i < weatherFiles.Length; i++)
                {
                    metData.AppendFormat("{0,9:F1}", siteData.GetValue(variablesIndex[i], t));
                }
                metData.AppendLine();
            }

            metData.Replace("-9999.0", "      ?").Replace("NaN", "  ?");
            return metData.ToString();
        }

        /// <summary>Gets a summary of the data within one or more netCDF weather files.</summary>
        /// <param name="addLocationSummary">Flag whether to add summary for each location in the files.</param>
        /// <param name="verboseEnabled">Flag whether to add detailed variable summary (metadata).</param>
        /// <returns>A list of strings with the summary of each file given and its weather variable.</returns>
        private static List<string> getSummaryData(bool addLocationSummary = true, bool verboseEnabled = false)
        {
            List<string> summaryData = new List<string>();

            // go through the list of variables and get their summary
            StringBuilder thisSummary = new StringBuilder();
            for (int i = 0; i < weatherFiles.Length; i++)
            {
                // initialise the output
                thisSummary.AppendFormat("Summary of file: {0}{1}", Path.GetFileName(weatherFiles[i]), Environment.NewLine);
                thisSummary.AppendLine();
                int varIndex = Array.IndexOf(weatherVariableNamesNIWA, variablesInFile[i]);
                thisSummary.AppendFormat(" weather variable in this file: {0}{1}", myWeatherVariableNames[varIndex], Environment.NewLine);
                thisSummary.AppendFormat("  which corresponds to NIWA's: {0}{1}", variablesInFile[i], Environment.NewLine);
                thisSummary.AppendFormat("  corresponding to APSIM's: {0}{1}", weatherVariableNamesAPSIM[varIndex], Environment.NewLine);

                // open the dataset file
                var inputDataset = DataSet.Open(weatherFiles[i]);
                var variablesList = inputDataset.Variables;
                if (verboseEnabled)
                { // write all metadata and variables list
                    var dimensionsList = inputDataset.Dimensions;
                    var metadata = inputDataset.Metadata;

                    thisSummary.AppendFormat(" the metadata:{0}", Environment.NewLine);
                    foreach (KeyValuePair<string, object> item in metadata)
                    {
                        thisSummary.AppendFormat("  {0}: {1}{2}", item.Key, item.Value.ToString(), Environment.NewLine);
                    }

                    thisSummary.AppendFormat(" full list of variables:{0}", Environment.NewLine);
                    foreach (Variable variable in variablesList)
                    {
                        thisSummary.AppendFormat("  variable: {0}, of type {1}, with dimensions {2}{3}",
                                                variable.Name, variable.TypeOfData.ToString(), variable.Dimensions.ToString(), Environment.NewLine);
                    }

                    thisSummary.AppendFormat(" list of dimension variables:{0}", Environment.NewLine);
                    foreach (Dimension dimension in dimensionsList)
                    {
                        thisSummary.AppendFormat("  dimension: {0}, of length {1}{2}", dimension.Name, dimension.Length, Environment.NewLine);
                    }

                    thisSummary.AppendLine();
                }

                //get the list dimensions
                var latitudeRead = inputDataset.GetData<float[]>("latitude");
                thisSummary.AppendFormat(" list of latitudes available: {0}{1}", string.Join(",", latitudeRead), Environment.NewLine);
                var longitudeRead = inputDataset.GetData<float[]>("longitude");
                thisSummary.AppendFormat(" list of longitudes available: {0}{1}", string.Join(",", longitudeRead), Environment.NewLine);
                thisSummary.AppendFormat("  total number of locations in the file: {0}{1}", latitudeRead.Length * longitudeRead.Length, Environment.NewLine);
                thisSummary.AppendFormat("  number of locations with valid values: {0}{1}", weatherLocationsCoordinates.Count, Environment.NewLine);
                var timeRead = inputDataset.GetData<short[]>("time");
                thisSummary.AppendFormat(" limits for the time variable: {0} and {1}{2}", timeRead.Min(), timeRead.Max(), Environment.NewLine);
                DateTime baseDate = new DateTime(1959, 12, 31); //assumes all dates start in "1959-12-31 09:00:00.0 +12:00"
                thisSummary.AppendFormat("  which correspond to dates between {0} and {1}{2}", baseDate.AddDays(timeRead.Min()).ToShortDateString(),
                                        baseDate.AddDays(timeRead.Max()).ToShortDateString(), Environment.NewLine);

                //get the general summary for the variable
                var dataRead = inputDataset.GetData<float[,,]>(variablesInFile[i]);
                float myValue = float.NaN;
                float minVal = 1000000F;
                float avgVal = 0.0F;
                float maxVal = -1000000F;
                int numValid = 0;
                int numMissing = 0;
                foreach (float dataItem in dataRead)
                {
                    if (dataItem < -9998F)
                    {
                        numMissing += 1;
                    }
                    else
                    {
                        numValid += 1;
                        myValue = dataItem * multiplierValue[varIndex] + offsetValue[varIndex];
                        avgVal += myValue;
                        minVal = Math.Min(minVal, myValue);
                        maxVal = Math.Max(maxVal, myValue);
                    }
                }

                if (numValid > 0)
                {
                    avgVal /= numValid;
                }
                else
                {
                    avgVal = -9999;
                    minVal = -9999;
                    maxVal = -9999;
                }

                thisSummary.AppendFormat(" summary of the variable across time and over all locations available in the file{0}", Environment.NewLine);
                thisSummary.AppendFormat("  minimum value: {0}{1}", minVal.ToString("#0.00"), Environment.NewLine);
                thisSummary.AppendFormat("  average value: {0}{1}", avgVal.ToString("#0.00"), Environment.NewLine);
                thisSummary.AppendFormat("  maximum value: {0}{1}", maxVal.ToString("#0.00"), Environment.NewLine);
                thisSummary.AppendFormat("  number of valid values: {0}{1}", numValid, Environment.NewLine);
                thisSummary.AppendFormat("  number of missing values: {0}{1}", numMissing, Environment.NewLine);

                //get the summary for the variable at each location
                if (addLocationSummary)
                {
                    thisSummary.AppendFormat(" data for each location available in the file:{0}", Environment.NewLine);
                    int totalVals = weatherLocationsCoordinates.Count;
                    if (verboseEnabled)
                    {
                        totalVals = latitudeRead.Length * longitudeRead.Length;
                    }
                    string[] locLatitude = new string[totalVals];
                    string[] locLongitude = new string[totalVals];
                    string[] varMin = new string[totalVals];
                    string[] varAvg = new string[totalVals];
                    string[] varMax = new string[totalVals];
                    string[] varVals = new string[totalVals];
                    string[] valsMissing = new string[totalVals];
                    int iLoc = 0;
                    Coordinates theCoordinates;
                    for (int iLat = 0; iLat < latitudeRead.Length; iLat++)
                    {
                        for (int iLong = 0; iLong < longitudeRead.Length; iLong++)
                        {
                            theCoordinates = new Coordinates(latitudeRead[iLat], longitudeRead[iLong]);
                            if (verboseEnabled || weatherLocationsCoordinates.Contains(theCoordinates, new LocationComparer()))
                            {
                                minVal = 1000000F;
                                avgVal = 0.0F;
                                maxVal = -1000000F;
                                numValid = 0;
                                numMissing = 0;
                                for (int iTime = 0; iTime < timeRead.Length; iTime++)
                                {
                                    if (dataRead[iTime, iLat, iLong] < -9998F)
                                    {
                                        numMissing += 1;
                                    }
                                    else
                                    {
                                        numValid += 1;
                                        myValue = dataRead[iTime, iLat, iLong] * multiplierValue[varIndex] + offsetValue[varIndex];
                                        avgVal += myValue;
                                        minVal = Math.Min(minVal, myValue);
                                        maxVal = Math.Max(maxVal, myValue);
                                    }
                                }

                                if (numValid > 0)
                                {
                                    avgVal /= numValid;
                                }
                                else
                                {
                                    avgVal = -9999;
                                    minVal = -9999;
                                    maxVal = -9999;
                                }

                                locLatitude[iLoc] = latitudeRead[iLat].ToString("#0.000");
                                locLongitude[iLoc] = longitudeRead[iLong].ToString("#0.000");
                                varMin[iLoc] = minVal.ToString("#0.00");
                                varAvg[iLoc] = avgVal.ToString("#0.00");
                                varMax[iLoc] = maxVal.ToString("#0.00");
                                varVals[iLoc] = numValid.ToString();
                                valsMissing[iLoc] = numMissing.ToString();
                                iLoc += 1;
                            }
                        }
                    }

                    thisSummary.AppendFormat("  location latitude: {0}{1}", string.Join(",", locLatitude), Environment.NewLine);
                    thisSummary.AppendFormat("  location longitude: {0}{1}", string.Join(",", locLongitude), Environment.NewLine);
                    thisSummary.AppendFormat("  minimum value: {0}{1}", string.Join(",", varMin), Environment.NewLine);
                    thisSummary.AppendFormat("  average value: {0}{1}", string.Join(",", varAvg), Environment.NewLine);
                    thisSummary.AppendFormat("  maximum value: {0}{1}", string.Join(",", varMax), Environment.NewLine);
                    thisSummary.AppendFormat("  number of values: {0}{1}", string.Join(",", varVals), Environment.NewLine);
                    thisSummary.AppendFormat("  missing values: {0}{1}", string.Join(",", valsMissing), Environment.NewLine);
                }

                //close the dataset
                inputDataset.Dispose();

                //add data to summary
                summaryData.Add(thisSummary.ToString());
                thisSummary.Clear();
            }

            return summaryData;
        }

        #endregion
    }
}
