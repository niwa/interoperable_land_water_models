using System;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using Newtonsoft.Json.Linq;
using ApsimSoilConverter;

namespace TestSoilConversion
{
    class Program
    {
        static void Main(string[] args)
        {
            // Reads an Apsim soil library file (.soils), convert it and save as json file, based on a baseTemplate

            /*string inputPath = @"H:\My Documents\RCichota\Projects\Shiny-ClimateChange\GlobalParameters(Copy)\";
            string inputFile = "MasterSoilApsimLibrary.soils";
            string baseFileFullPath = @"\\storage.powerplant.pfr.co.nz\input\projects\ShinyClimateChange\rawData\soils\BaseSoil(2019.07.18).apsimx";
            string libraryFileFullPath = @"\\storage.powerplant.pfr.co.nz\input\projects\ShinyClimateChange\processedData\soils\2019-08-28_MasterSoilApsimLibrary.soils";
            */

            /*string inputPath = @"H:\My Documents\RCichota\Projects\InteroperableModels\GitHub\Soils\";
            string inputFile = "Aparima_SMap_SoilwatLibrary.soils";
            string baseFileFullPath = @"H:\My Documents\RCichota\Projects\InteroperableModels\GitHub\ApsimIOM\Config\base.apsimx";
            string outputFile = "AparimaApsimXSoilLibrary.apsimx";*/

            string inputPath = @"H:\My Documents\RCichota\Projects\InteroperableModels\GitHub\Soils\";
            string inputFile = args[0] + ".soils";
            string libraryFileFullPath = inputPath + inputFile;
            string baseFile = args[1] + ".apsimx";
            string outputPath = inputPath;
            string baseFileFullPath = inputPath + baseFile;
            string outputFile = "MasterApsimXSoilLibrary.apsimx";
            string outputFileFullPath = inputPath + "NewLibrary.apsimx";

            string soilSought = "All";
            //soilSought = "Aparima_6a1";
            //XmlToJson.ConvertFile(libraryFileFullPath, baseFileFullPath, outputFileFullPath);
            XmlToJson.ConvertFile(libraryFileFullPath, baseFileFullPath, outputFileFullPath,soilSought,true);
            //XmlToJson.ConvertFile(inputPath + inputFile, inputPath + baseFile, outputPath + outputFile, soilSought);
            /*
            string soilSought = "Canterbury_M ";//"Kawek_4a1";//
            JObject mySoil = XmlToJson.GetConvertedSoil(inputPath + inputFile, inputPath + baseFile, soilSought);
            /**/

            Console.WriteLine("Soil conversion finished");
            Console.Write(" press any key...");
            Console.ReadLine();
        }
    }
}
