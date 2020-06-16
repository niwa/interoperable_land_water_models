using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Collections.Generic;
using System.Xml;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace ApsimSoilConverter
{
    /// <summary>Methods for converting soil data from a classic Apsim format (xml) into an ApsimX format (json).</summary>
    public class XmlToJson
    {
        #region Public Methods

        /// <summary>Converts soil data of an xml Apsim soil library (.soils) and saves it in json format.</summary>
        /// <param name="xmlSoilLibraryFile">Path of the xml file with the soil library to be converted.</param>
        /// <param name="jsonBaseSoilFile">Path of the json file with the base soil, used as template during the conversion.</param>
        /// <param name="jsonSoilLibraryFile">Name and path to the file that will be saved with the converted soils (optional).</param>
        /// <param name="soilSought">Name of the soil to be converted (optional = "All").</param>
        public static void ConvertFile(string xmlSoilLibraryFile, string jsonBaseSoilFile, string jsonSoilLibraryFile = "", string soilSought = "All", bool complementSoilCropInfo = false)
        {
            // check that the files and paths given exist
            if (!File.Exists(xmlSoilLibraryFile))
            {
                throw new Exception($"ConvertXmlToJson - The file {Path.GetFileName(xmlSoilLibraryFile)} was not found in {Path.GetFullPath(xmlSoilLibraryFile)}!");
            }

            if (!File.Exists(jsonBaseSoilFile))
            {
                throw new Exception($"ConvertXmlToJson - The file {Path.GetFileName(jsonBaseSoilFile)} was not found in {Path.GetFullPath(jsonBaseSoilFile)}!");
            }

            if (jsonSoilLibraryFile == "")
            { // save the file in the same location and with the same name of the xml file (different extension)
                jsonSoilLibraryFile = Path.GetDirectoryName(xmlSoilLibraryFile) + "\\" + Path.GetFileNameWithoutExtension(xmlSoilLibraryFile) + ".apsimx";
            }
            else if (!Directory.Exists(Path.GetDirectoryName(jsonSoilLibraryFile)))
            {
                throw new Exception($"ConvertXmlToJson - The path to save the converted soils ({Path.GetDirectoryName(jsonSoilLibraryFile)}) was not found!");
            }

            // call the methods to read, format, and copy the data
            JObject baseSoil = getBaseSoil(jsonBaseSoilFile);
            XmlNodeList soilsToConvert = getSoilLibrary(xmlSoilLibraryFile, soilSought);
            JArray outputSoils = convertSoils(soilsToConvert, baseSoil, complementSoilCropInfo);

            // put the soil info into a jobject
            JObject outputSoilLibrary = new JObject();
            outputSoilLibrary.Add("$type", "Models.Core.Simulations, Models");
            outputSoilLibrary.Add("Name", "Soils");
            outputSoilLibrary.Add("Children", outputSoils);

            // serialise and save the soil data as an ApsimX file
            string serialisedLibrary = JsonConvert.SerializeObject(outputSoilLibrary, Newtonsoft.Json.Formatting.Indented);
            File.WriteAllText(jsonSoilLibraryFile, serialisedLibrary);
        }

        /// <summary>Gets a soil node converted from a xml Apsim soil library (.soils) into json format.</summary>
        /// <param name="xmlSoilLibraryFile">Path of the xml file with the soil data to be converted.</param>
        /// <param name="jsonBaseSoilFile">Path of the json file with the base soil, used as template during the conversion.</param>
        /// <param name="soilSought">Name of the soil to be converted.</param>
        /// <returns>A jobject with the soil data formatted for use in ApsimX.</returns>
        public static JObject GetConvertedSoil(string xmlSoilLibraryFile, string jsonBaseSoilFile, string soilSought, bool complementSoilCropInfo = false)
        {
            // check that the files given exist
            if (!File.Exists(xmlSoilLibraryFile))
            {
                throw new Exception($"ConvertXmlToJson - The file {Path.GetFileName(xmlSoilLibraryFile)} was not found in {Path.GetFullPath(xmlSoilLibraryFile)}!");
            }

            if (!File.Exists(jsonBaseSoilFile))
            {
                throw new Exception($"ConvertXmlToJson - The file {Path.GetFileName(jsonBaseSoilFile)} was not found in {Path.GetFullPath(jsonBaseSoilFile)}!");
            }

            // check that soil sought is  not 'all'
            if (soilSought.ToLower() == "all")
            {
                throw new Exception("ConvertXmlToJson - Cannot use \"All\" as name of soil to convert in this method.");
            }

            // call the methods to read, format, and copy the data
            JObject baseSoil = getBaseSoil(jsonBaseSoilFile);
            XmlNodeList soilToConvert = getSoilLibrary(xmlSoilLibraryFile, soilSought);
            JArray outputSoils = convertSoils(soilToConvert, baseSoil, complementSoilCropInfo);

            // return the soil info as a jobject
            return (JObject)outputSoils[0];
        }

        /// <summary>Gets a soil node converted from a xml Apsim soil library (.soils) into json format.</summary>
        /// <param name="xmlSoilLibraryFile">Path of the xml file with the soil data to be converted.</param>
        /// <param name="jsonBaseSoilNode">Soil node (json) to be used as template during the conversion.</param>
        /// <param name="soilSought">Name of the soil to be converted.</param>
        /// <returns>A jobject with the soil data formatted for use in ApsimX.</returns>
        /// <remarks>Assumes that the json node is a valid soil data structure.</remarks>
        public static JObject GetConvertedSoil(string xmlSoilLibraryFile, JObject jsonBaseSoilNode, string soilSought, bool complementSoilCropInfo = false)
        {
            // check that the file given exist
            if (!File.Exists(xmlSoilLibraryFile))
            {
                throw new Exception($"ConvertXmlToJson - The file {Path.GetFileName(xmlSoilLibraryFile)} was not found in {Path.GetFullPath(xmlSoilLibraryFile)}!");
            }

            // check that soil sought is  not 'all'
            if (soilSought.ToLower() == "all")
            {
                throw new Exception("ConvertXmlToJson - Cannot use \"All\" as name of soil to convert in this method.");
            }

            // call the methods to read, format, and copy the data
            XmlNodeList soilToConvert = getSoilLibrary(xmlSoilLibraryFile, soilSought);
            JArray outputSoils = convertSoils(soilToConvert, jsonBaseSoilNode, complementSoilCropInfo);

            // return the soil info as a jobject
            return (JObject)outputSoils[0];
        }

        #endregion

        #region Private Methods

        /// <summary>Gets the json node with the base soil structure.</summary>
        /// <param name="baseSoilPath">Path to the baseSoil file.</param>
        /// <returns>A json object with the base soil structure.</returns>
        private static JObject getBaseSoil(string baseSoilPath)
        {
            JObject baseSoil = null;

            // open the base file
            string soilFile = File.ReadAllText(baseSoilPath);
            JObject mySoilFile = (JObject)JsonConvert.DeserializeObject(soilFile);

            // get the list with all soil nodes (this may be simplified if it can safely assume that the json file has only one soil)
            List<JObject> mySoils = findJsonNodes("$type", "Models.Soils.Soil", mySoilFile);

            // get the base soil node
            if (mySoils.Count>0)
            { // there is at least one soil, select the first one
                baseSoil = (JObject)mySoils[0].DeepClone();
            }
            else
            {
                throw new Exception($"ConvertXmlToJson - No valid soil node was found in the file {Path.GetFileName(baseSoilPath)}");
            }
            
            return baseSoil;
        }

        /// <summary>Finds all nodes within a json object that contain a key with a given value.</summary>
        /// <remarks>Assumes general format of an ApsimX json file. Search is recursive within all children in the jobject.</remarks>
        /// <param name="itemKey">Key to be used as selection criterion (full description is needed).</param>
        /// <param name="itemValue">Value of the key that triggers a selection (partial description suffices).</param>
        /// <param name="jsonObject">Object (json) in which the nodes will be searched.</param>
        /// <returns>A list with all nodes found within the jobject that meet the criterion given.</returns>
        private static List<JObject> findJsonNodes(string itemKey, string itemValue, JObject jsonObject)
        {
            List<JObject> nodeList = new List<JObject>();
            foreach (dynamic item in jsonObject["Children"])
            {
                string itemType = item[itemKey];
                if (itemType.Contains(itemValue))
                {
                    nodeList.Add((JObject)item);
                }
                else
                {
                    if (item["Children"] is System.Collections.IList)
                    {
                        nodeList.AddRange(findJsonNodes(itemKey, itemValue, (JObject)item));
                    }
                }
            }

            return nodeList;
        }

        /// <summary>Gets a list of soil nodes from the soil library file (xml).</summary>
        /// <param name="soilLibraryFile">Path to the soil library file.</param>
        /// <param name="soilSought">Name of the soil being sought (optional = "All").</param>
        /// <returns>An xml list with all the soil nodes that match the name sought, or all the soils if no name is supplied.</returns>
        private static XmlNodeList getSoilLibrary(string soilLibraryFile, string soilSought = "All")
        {
            XmlNodeList soilList;

            // open the soils library
            XmlDocument soilLibrary = new XmlDocument();
            soilLibrary.Load(soilLibraryFile);

            // get all soils that match the name sought
            if (soilSought.ToLower() == "all")
            {
                soilList = soilLibrary.SelectNodes("//Soil");
            }
            else
            {
                soilList = soilLibrary.SelectNodes($"//Soil[@name='{soilSought}']");
                // this works, but it is dangerous as the same substring could be used for different soils (e.g. 'SiltClay' and 'SiltClayLoam')
                //soilList = soilLibrary.SelectNodes($"//Soil[contains(@name,'{soilSought}')]");

                if (soilList.Count > 1)
                {
                    throw new Exception($"More than one soil with the name \"{soilSought}\" was found in the library {Path.GetFileName(soilLibraryFile)}");
                }
            }

            // check that the result is within expectations
            if (soilList.Count == 0)
            {
                throw new Exception($"The soil \"{soilSought}\" was not found in the library {Path.GetFileName(soilLibraryFile)}");
            }
            else if (soilList.Count > 1)
            {
                if (soilSought.ToLower() != "all")
                {
                    throw new Exception($"More than one soil named \"{soilSought}\" was found in the library {Path.GetFileName(soilLibraryFile)}");
                }
            }

            return soilList;
        }

        /// <summary>Converts one or more soils from xml into json format.</summary>
        /// <param name="soilLibrary">Xml list with the soil nodes to convert.</param>
        /// <param name="baseSoil">Json object to be used as template in the conversion.</param>
        /// <returns>A jobject with one or more soils in json ApsimX format.</returns>
        private static JArray convertSoils(XmlNodeList soilLibrary, JObject baseSoil, bool addComplementarySoilCropInfo = false)
        {
            JArray outputSoils = new JArray();

            foreach (XmlNode soil in soilLibrary)
            {
                // convert the soil from xml to json
                JObject soilSelected = (JObject)JsonConvert.DeserializeObject(JsonConvert.SerializeXmlNode(soil));
                // peal off the excess node layers
                soilSelected = (JObject)soilSelected.First.First;

                // format the selected soil to ApsimX standards
                JObject soilFormatted = formatSoilData(soilSelected);

                // clone the base soil, add new data to this copy and keep the original as template
                JObject outputSoil = (JObject)baseSoil.DeepClone();

                // replace the available data from the selected soil into the base soil and check SoilCrop
                outputSoil = copySoilData(outputSoil, soilFormatted);
                if (addComplementarySoilCropInfo)
                {
                    addSoilCropInfo(outputSoil, "WhiteClover", "ItalianRyegrass");
                    addSoilCropInfo(outputSoil, "FodderBeet", "Potato");
                }

                // add the soil data to the output list
                outputSoils.Add(outputSoil);
            }

            return outputSoils;
        }

        /// <summary>Formats the soil data into Apsim standards.</summary>
        /// <remarks>
        /// There are several quirks and exceptions that need addressing beyond converting from xml to json:
        /// - Need to check the name of keys and values, including case:
        ///   - Add 'Soils.' to each model;
        ///   - Fix the names of 'InitialWater' and 'InitialNitrogen';
        ///   - Ensure 'Memo' is in capital and fix the name and text tags (@name to Name. #text to Text);
        /// - SoilCrop data are all put together (as jarray) and added into the 'children' node;
        /// - Memo nodes also need to be added to the 'children' node (jarray);
        /// - Arrays need some adjusting (remove 'double' or 'string' tag), if double then convert to numbers;
        /// - SoilCN and RootWt need to be filled in for all soil layers (There was only one value is in old versions):
        ///   - SoilCN value is maintained, simply copied across all layer;
        ///   - RootWt is distributed to all layers using an exponential function to partition the original value (total amount);
        /// - Need to check that SoilCrop and Memo are added as jarray ('children' node), and check that only one 'children' node is used.
        ///     jarray is not created in the basic conversion if there is only one crop, and never for memos;
        /// - Need to check that the layered soil parameters are arrays (this doesn't happen in the basic conversion if there is only one value);
        /// - Need to ensure that the case of the value of 'RelativeTo' is correct in InitialWater (should be "LL15");
        /// - Need to check the units of the initial N (xml uses a string, while json uses an integer!? - 'ppm'=0; 'kgha'=1);
        /// </remarks>
        /// <param name="soilDataNode">Node with soil data, from a simple conversion from xml to json.</param>
        /// <returns>The node conforming to ApsimX standards in json format.</returns>
        private static JObject formatSoilData(JObject soilDataNode)
        {
            JObject formattedSoil = new JObject();
            JArray childrenNode = new JArray();

            // add the model type and name of the soil
            formattedSoil.Add("$type", "Models.Soils.Soil, Models");
            formattedSoil.Add("Name", soilDataNode.GetValue("@name"));

            // go through each data item and gather the appropriate values
            foreach (KeyValuePair<string, JToken> soilItem in soilDataNode)
            {
                if (soilItem.Value.HasValues)
                { // this is a main soil info node ("Analysis", "SoilWater", etc.) or memo
                  // gather the data and add to a JArray ('children' node)
                    JObject soilNodeData = new JObject();
                    string nodeName;
                    string nodeType;

                    // get, check, and add node/model type and name
                    nodeType = soilItem.Key;
                    if (nodeType.Contains("Sample"))
                    {
                        nodeName = "InitialNitrogen";
                        nodeType = "Soils." + nodeType;
                    }
                    else if (nodeType.ToLower().Contains("memo"))
                    {
                        nodeName = soilItem.Value.SelectToken("@name").ToString();
                        nodeType = "Memo";
                    }
                    else
                    {
                        nodeName = nodeType;
                        nodeType = "Soils." + nodeType;
                    }

                    soilNodeData.Add("$type", "Models." + nodeType + ", Models");
                    soilNodeData.Add("Name", nodeName);

                    // go through each item in the node and get the appropriate values
                    //TODO: handle SWIM3 node (or specifically its sub-nodes)
                    if (nodeType == "Memo")
                    {
                        soilNodeData.Add("Text", soilItem.Value.SelectToken("#text"));
                    }

                    foreach (KeyValuePair<string, JToken> item in (JObject)soilItem.Value)
                    {
                        if (item.Value.HasValues)
                        { //this is a data array ("Depth", "DUL", etc.) or a node ("SoilCrop", "Memo", etc.)
                            if (item.Key == "SoilCrop")
                            { // this is a node with the parameters for crops
                              // gather the data into json nodes and add to a JArray ('children' node)
                                JArray cropNodes = new JArray();

                                // go through each crop and get their parameters
                                if (item.Value.GetType().ToString().Contains("JArray"))
                                { // there is an array with crops
                                    foreach (JObject cropNode in item.Value)
                                    {
                                        cropNodes.Add(formatSoilCropInfo(cropNode));
                                    }
                                }
                                else
                                { // there is only one crop
                                    cropNodes.Add(formatSoilCropInfo((JObject)item.Value));
                                }

                                // add the crop data as items of the children node
                                JArray existingChildren = (JArray)soilNodeData["Children"];
                                if (existingChildren == null)
                                {
                                    soilNodeData.Add("Children", cropNodes);
                                }
                                else
                                {
                                    existingChildren.Merge(cropNodes);
                                }
                            }
                            else if (item.Key.ToLower() == "memo")
                            { // this is a memo node
                                JObject memoNode = new JObject();
                                memoNode.Add("$type", "Models.Memo, Models");
                                memoNode.Add("Name", item.Value.SelectToken("@name"));
                                memoNode.Add("Text", item.Value.SelectToken("#text"));

                                // add the memo as an item of the children node
                                JArray existingChildren = (JArray)soilNodeData["Children"];
                                if (existingChildren == null)
                                {
                                    soilNodeData.Add("Children", new JArray(memoNode));
                                }
                                else
                                {
                                    existingChildren.Add(memoNode);
                                }
                            }
                            else
                            { //this is a simple array structure, get the appropriate values (strip 'double' and 'string')
                                JArray valueArray = new JArray();
                                if (item.Value.SelectToken("double") != null)
                                {
                                    if(item.Value.SelectToken("double").GetType().ToString().Contains("JArray"))
                                    { // in most cases this is an Array
                                        valueArray.Add(item.Value.SelectToken("double").ToObject<double[]>());
                                    }
                                    else
                                    { // but sometimes it is a simple value
                                        valueArray.Add(item.Value.SelectToken("double").ToObject<double>());
                                    }
                                }
                                else if (item.Value.SelectToken("string") != null)
                                {
                                    if (item.Value.SelectToken("string").GetType().ToString().Contains("JArray"))
                                    { // in most cases this is an Array
                                        valueArray.Add(item.Value.SelectToken("string").ToObject<string[]>());
                                    }
                                    else
                                    { // but sometimes it is a simple value
                                        valueArray.Add(item.Value.SelectToken("string").ToObject<string>());
                                    }
                                }
                                //else - should not happened arrays are either double or strings

                                soilNodeData.Add(item.Key, valueArray);
                            }
                        }
                        else
                        {
                            if ((item.Key == "RootWt") || (item.Key == "SoilCN"))
                            {
                                // these parameters were a single value but now are given as an array with a value for each layer
                                // create a new array and populate using Apsim's approach (exponential function for RootWt, constant for soilCN)
                                JToken soilProfile = soilItem.Value.SelectToken("Thickness.double");

                                JArray valueArray = new JArray();
                                if (item.Key == "RootWt")
                                {
                                    double[] fomValues = fomDistribution(soilProfile.ToObject<double[]>(), (double)item.Value);
                                    foreach (var fomValue in fomValues)
                                    {
                                        valueArray.Add(fomValue);
                                    }
                                }
                                else
                                {
                                    foreach (var layer in soilProfile)
                                    {
                                        valueArray.Add((double)item.Value);
                                    }
                                }

                                soilNodeData.Add(item.Key, valueArray);
                            }
                            else if (item.Key == "RelativeTo")
                            { // this is value in the InitialWater node, value is case sensitive (important if it is LL15)
                                if (item.Value.ToString().ToLower() == "ll15")
                                {
                                    soilNodeData.Add(item.Key, "LL15");
                                }
                                else
                                {
                                    soilNodeData.Add(item.Key, item.Value);
                                }
                            }
                            else if (item.Key == "NH4Units" || item.Key == "NO3Units")
                            { // units have to be interpreted ('ppm'=0; 'kgha'=1)
                                if ((item.Value.ToString().ToLower() == "kgha") || (item.Value.ToString() == "1"))
                                {
                                    soilNodeData.Add(item.Key, 1);
                                }
                                else
                                {
                                    soilNodeData.Add(item.Key, 0);
                                }
                            }
                            else
                            { //this is a simple value, just add it as is
                                soilNodeData.Add(item.Key, item.Value);
                            }
                        }
                    }

                    // re-check initial soil N units (because they might be missing, which means 'ppm'=0)
                    if (soilItem.Key.ToString().Contains("Sample"))
                    {
                        JToken nUnits = soilItem.Value.SelectToken("NH4Units");
                        if (nUnits == null)
                        {
                            soilNodeData.Add("NH4Units", 0);
                        }

                        nUnits = soilItem.Value.SelectToken("NO3Units");
                        if (nUnits == null)
                        {
                            soilNodeData.Add("NO3Units", 0);
                        }
                    }

                    // add the data to the ChildrenNode
                    childrenNode.Add(soilNodeData);
                }
                else
                {
                    //this is a simple value, just keep it
                    formattedSoil.Add(soilItem.Key, soilItem.Value);
                }
            }

            // add the children node
            formattedSoil.Add("Children", childrenNode);

            return formattedSoil;
        }

        /// <summary>Computes the distribution of FOM in the soil profile.</summary>
        /// <remarks>Uses the same function as ApsimX (and classic), distributing over the whole profile, regardless the depth.</remarks>
        /// <param name="soilProfile">Array with info about soil layers' thickness.</param>
        /// <param name="fomAmount">Total amount of FOM, to be distributed over the profile.</param>
        /// <returns>An array with the values of FOM for each soil layer.</returns>
        private static double[] fomDistribution(double[] soilProfile, double fomAmount)
        {
            double[] fomArray = new double[soilProfile.Length];
            double profileDepth = soilProfile.Sum();
            double depthFromSurface = 0;
            double fracLayer = 1.0;

            // calculate the general distribution over depth
            for (int layer = 0; layer < soilProfile.Length; layer++)
            {
                if (profileDepth - depthFromSurface < soilProfile[layer])
                {
                    fracLayer = (profileDepth - depthFromSurface) / soilProfile[layer];
                }
                depthFromSurface += soilProfile[layer];
                if (depthFromSurface <= profileDepth)
                {
                    fomArray[layer] = fracLayer * Math.Exp(-3.0 * Math.Min(1.0, depthFromSurface / profileDepth));
                }
            }

            // get the actual FOM distribution (with fractions normalised so that the values add up to one)
            double totalFraction = fomArray.Sum();
            for (int layer = 0; layer < soilProfile.Length; layer++)
            {
                fomArray[layer] = fomAmount * fomArray[layer] / totalFraction;
            }

            return fomArray;
        }

        /// <summary>Formats the soilCrop data into ApsimX standards.</summary>
        /// <param name="cropNode">Node with the soilCrop data, from a simple conversion from xml to json</param>
        /// <returns>The node conforming to ApsimX in json format.</returns>
        private static JObject formatSoilCropInfo(JObject cropNode)
        {
            JObject cropInfo = new JObject();
            // add type and name
            cropInfo.Add("$type", "Models.Soils.SoilCrop, Models");
            cropInfo.Add("Name", cropNode.GetValue("@name") + "Soil");

            // go through each subnode and get the appropriate values
            foreach (KeyValuePair<string, JToken> subNode in cropNode)
            {
                if (subNode.Value.HasValues)
                {
                    JArray valueArray = new JArray();
                    if (subNode.Value.SelectToken("double") != null)
                    {
                        if (subNode.Value.SelectToken("double").GetType().ToString().Contains("JArray"))
                        { // in most cases this is an Array
                            valueArray.Add(subNode.Value.SelectToken("double").ToObject<double[]>());
                        }
                        else
                        { // but sometimes it is a simple value
                            valueArray.Add(subNode.Value.SelectToken("double").ToObject<double>());
                        }
                    }
                    else if (subNode.Value.SelectToken("string") != null)
                    {
                        valueArray = (JArray)subNode.Value.SelectToken("string");
                    }
                    //else - should not happened

                    cropInfo.Add(subNode.Key, valueArray);
                }
                // Crop nodes should have no simple data, other than name
            }

            return cropInfo;
        }

        /// <summary>Copies the relevant soil data from one json soil node to another.</summary>
        /// <remarks>
        /// Values are only copied for matching nodes, i.e. the nodes in baseSoil that also exist in inputSoil,
        ///  otherwise values in baseSoil are kept and those is inputSoil are ignored. With exceptions:
        /// - All values for SoilCrop are moved across from inputSoil to baseSoil (full replacement);
        /// - All Memos in the root node or in any matching node are also copied into the baseSoil (added);
        /// - The node LayerStructure, if present in inputSoil, is copied into the baseSoil;
        /// - If a 'Depth' array exists in baseSoil it is deleted (probably redundant as this should not have been here);
        /// - If the inputSoil does not have values for KLAT, any values in the baseSoil is deleted;
        /// 
        /// Need to skip 'children' of SoilNitrogen (probably also SWIM3 and SoilNutrient - TODO)
        /// </remarks>
        /// <param name="baseSoil">Base or template node that is being modified.</param>
        /// <param name="inputSoil">Node that contains the data to be copied over.</param>
        /// <returns>The base node with data copied from inputSoil.</returns>
        private static JObject copySoilData(JObject baseSoil, JObject inputSoil)
        {
            string nodePath;

            // go through all nodes and copy values of items that exist in both files
            foreach (KeyValuePair<string, JToken> soilDataItem in baseSoil)
            {
                nodePath = soilDataItem.Key;
                if (nodePath == "Children")
                { // this is the collection of soil sub-nodes ("Analysis", "SoilWater", etc.)
                    int childIndex = 0;
                    foreach (JToken baseSoilNode in soilDataItem.Value)
                    { // go through each node and replace the values
                        JToken selectedSoilNode = inputSoil.SelectToken("$.Children[?(@.$type == '"
                                                + baseSoilNode.SelectToken("$type").ToString() + "')]");
                        JToken depthItem = null;
                        if (selectedSoilNode != null)
                        { // the node exists in the soil selected from the library
                            foreach (KeyValuePair<string, JToken> item in (JObject)baseSoilNode)
                            {
                                if (item.Key == "Children")
                                {
                                    if (item.Value.HasValues && !baseSoilNode.ToString().Contains("SoilNitrogen"))
                                    { // this is a memo or the collection of crop data
                                        nodePath = "Children[" + childIndex + "]." + item.Key; // check for Memo
                                        baseSoil.SelectToken(nodePath).Replace(selectedSoilNode.SelectToken(item.Key));
                                    }
                                }
                                else
                                {
                                    nodePath = "Children[" + childIndex + "]." + item.Key;
                                    if (selectedSoilNode.SelectToken(item.Key) != null)
                                    {
                                        baseSoil.SelectToken(nodePath).Replace(selectedSoilNode.SelectToken(item.Key));
                                    }
                                    else if (item.Key.Equals("KLAT"))
                                    { // delete existing values
                                        baseSoil.SelectToken(nodePath).Replace(null);
                                    }

                                    // check whether there is a depth item, it needs to be deleted (didn't exist in the xml file)
                                    if (item.Key.Equals("Depth"))
                                    {
                                        depthItem = baseSoil.SelectToken(nodePath).Parent;
                                    }
                                }
                            }
                        }

                        // delete the depth item, if present
                        if (depthItem != null)
                        {
                            depthItem.Remove();
                            depthItem = null;
                        }

                        childIndex += 1;
                    }

                    // check whether there is a layerStructure. If so, get it and copy across
                    JToken selectedNode = inputSoil.SelectToken("$.Children[?(@.$type == 'Models.Soils.LayerStructure, Models')]");
                    if (selectedNode != null)
                    {
                        JArray soiChildren = (JArray)baseSoil.SelectToken("$.Children");
                        soiChildren.Add(selectedNode);
                    }

                    // check whether there is a Memo at this level. If so, get it and copy across
                    selectedNode = inputSoil.SelectToken("$.Children[?(@.$type == 'Models.Memo, Models')]");
                    if (selectedNode != null)
                    {
                        JArray soiChildren = (JArray)baseSoil.SelectToken("$.Children");
                        soiChildren.Add(selectedNode);
                    }
                }
                else
                { // this is a simple node, copy the value if it exist in inputSoil
                    if (inputSoil.GetValue(nodePath) != null)
                    {
                        baseSoil.SelectToken(nodePath).Replace(inputSoil.SelectToken(nodePath));
                    }
                    else if (nodePath == "LocalName")
                    {
                        baseSoil.SelectToken(nodePath).Replace(inputSoil["Name"]);
                    }
                }
            }

            return baseSoil;
        }

        /// <summary>Adds a SoilCrop node for a new crop, cloning a given baseCrop.</summary>
        /// <remarks>Addition only happens if the soilData does not already have that crop info.</remarks>
        /// <param name="soilData">Soil data node to which new crop data is added.</param>
        /// <param name="newCrop">Name of new crop.</param>
        /// <param name="baseCrop">Name of base crop.</param>
        private static void addSoilCropInfo(JObject soilData, string newCrop, string baseCrop)
        {
            var soilCropInfoList1 = soilData.SelectToken("$.Children[?(@.$type == 'Models.Soils.Water, Models')]");
            JArray soilCropInfoList = (JArray)soilCropInfoList1.SelectToken("$.Children");
            bool cropNotFound = true;
            int numFound = soilCropInfoList.Where(c => c["Name"].ToString() == newCrop + "Soil").Count();
            if (numFound == 0)
            {
                foreach (var item in soilCropInfoList)
                {
                    if (item["Name"].ToString() == baseCrop + "Soil")
                    {
                        var newItem = item.DeepClone();
                        newItem["Name"] = newCrop + "Soil";
                        soilCropInfoList.Add(newItem);
                        cropNotFound = false;
                        break;
                    }
                }

                if (cropNotFound)
                {
                    throw new Exception($"Could not find SoilCrop data for \"{baseCrop}\", which is used as base to add \"{newCrop}\".");
                }
            }
        }

        #endregion
    }
}
