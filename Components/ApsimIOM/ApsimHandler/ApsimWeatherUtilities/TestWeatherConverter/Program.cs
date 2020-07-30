using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using ApsimWeatherConverter;

namespace TestWeatherConverter
{
    class Program
    {
        static void Main(string[] args)
        {
            // Reads a series of netcdf files with weather data, convert them and save as .met file for use in Apsim

            string inputPath = @"H:\My Documents\RCichota\Projects\InteroperableModels\GitHub\Weather\";
            string rainFile = "rain_vclim_clidb_1972010100_2018101500_south-island_p05_daily_Aparima.nc";
            string tminFile = "tmin_vclim_clidb_1972010100_2018071400_south-island_p05_daily_Aparima.nc";
            string tmaxFile = "tmax_vclim_clidb_1972010100_2018071400_south-island_p05_daily_Aparima.nc";
            string sradFile = "srad_vclim_clidb_1972010100_2018071400_south-island_p05_daily_Aparima.nc";
            string windFile = "wind_vclim_clidb_1972010100_2018071400_south-island_p05_daily_Aparima.nc";
            string petFile = "pet_vclim_clidb_1972010100_2018063000_south-island_p05_daily_Aparima.nc";
            //string[] inputFiles = new string[] { rainFile, tminFile, tmaxFile, sradFile, windFile, petFile };
            string[] inputFiles = new string[] { rainFile, tminFile, tmaxFile, sradFile};
            string outputPath = inputPath;

            // test convert all files
            //NetcdfToMet.ConvertFiles(inputPath, inputFiles, outputPath);

            //test convert one file
            float[] locationCoordinate = new float[2];
            // near Wheys Bush
            locationCoordinate[0] = -46.025F;
            locationCoordinate[1] = 168.125F;

            //locationCoordinate[0] = -45.625F;
            //locationCoordinate[1] = 167.625F;

            //  not supplying any name
            //            NetcdfToMet.ConvertWeather(inputPath, inputFiles, locationCoordinate, outputPath);

            //  supplying a name
            //string outputFile = "WeatherDataTest";
            //outputFile = "";
            //NetcdfToMet.ConvertWeather(inputPath, inputFiles, locationCoordinate, outputPath, outputFile, "met");

            // test convert to csv
            //outputFile = "";
            //NetcdfToMet.ConvertWeather(inputPath, inputFiles, locationCoordinate, outputPath, outputFile, "csv");

            //test getting data only
            string outputFile = "testData.met";
            NetcdfToMet.CoordinatesTolerance = 0.025f;
            //locationCoordinate[0] = -46.275F;
            string metFileData = NetcdfToMet.GetWeatherData(inputPath, inputFiles, locationCoordinate);
            using (StreamWriter outFile = new StreamWriter(outputPath + outputFile))
            {
                outFile.WriteLine(metFileData);
            }

            // test getting the summary
         //   string[] myFiles = new string[] { rainFile, tminFile, tmaxFile, sradFile, windFile, petFile };
            //string[] myFiles = new string[] { rainFile, tminFile, tmaxFile, sradFile, windFile };
            //string[] myFiles = new string[] { tminFile, tmaxFile, rainFile, sradFile, windFile };
            //string[] myFiles = new string[] { rainFile};
            //string[] myFiles = new string[] {windFile };
         //   string outputFileName = "testSummary.txt";
            //NetcdfToMet.GetDataInfo(inputPath, myFiles, inputPath, outputFileName);
         //   NetcdfToMet.GetDataInfo(inputPath, myFiles, inputPath, outputFileName, false, true, true, true);

            Console.WriteLine("Weather file conversion finished");
            Console.Write(" press any key...");
            Console.ReadLine();
        }
    }
}
