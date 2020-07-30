using System;
using System.Collections.Generic;
using System.Json;
using System.Text.RegularExpressions;


namespace SOSClientJSON.Utils
{
    /// <summary>
    /// Static class that has lots of utilities to create and parse requests to SOS
    /// </summary>
    class JSONUtils
    {
        /// <summary>
        /// Test query to the SOS server
        /// </summary>
        /// <returns>String of the query</returns>
        public static string BuildJSONSOSTestRequest()
        {
            var requestObject = new JsonObject
            {
                { "request", new JsonPrimitive("GetCapabilities") },
                { "service", new JsonPrimitive("SOS") },
            };
            return requestObject.ToString();
        }

        /// <summary>
        /// Creates a request for time series to a SOS server
        /// </summary>
        /// <param name="procedure"></param>
        /// <param name="observedProperty">The property we want to extract</param>
        /// <param name="featureOfInterest">ID of the sensor</param>
        /// <param name="phenomenonTime">Array containing start and end time</param>
        /// <returns></returns>
        public static string BuildTimeSeriesRequest(string procedure, string observedProperty, string featureOfInterest, string[] phenomenonTime)
        {
            var requestObject = new JsonObject
            {
                { "request", new JsonPrimitive("GetObservation") },
                { "service", new JsonPrimitive("SOS") },
                { "version", new JsonPrimitive("2.0.0") },
                { "procedure", new JsonPrimitive(procedure) },
                { "observedProperty", new JsonPrimitive(observedProperty) },
                { "featureOfInterest", new JsonPrimitive(featureOfInterest) }
            };
            var temporalFilterDuring = new JsonObject
            {
                { "ref", new JsonPrimitive("om:phenomenonTime") }
            };
            JsonArray time = new JsonArray
            {
                new JsonPrimitive(phenomenonTime[0]),
                new JsonPrimitive(phenomenonTime[1])
            };
            temporalFilterDuring.Add("value", time);
            var temporalFilter = new JsonObject
            {
                { "during", temporalFilterDuring }
            };

            requestObject.Add("temporalFilter", temporalFilter);
            return requestObject.ToString();
        }

        /// <summary>
        /// Create a data availability JSON object for a query to SOS
        /// </summary>
        /// <param name="procedure"></param>
        /// <param name="observedProperty">QR or HG (quantity and height of gauge resp.)</param>
        /// <param name="featureOfInterest">ID of the sensor</param>
        /// <returns></returns>
        public static string BuildDataAvailabilityRequest(string procedure, string observedProperty, string featureOfInterest)
        {
            var requestObject = new JsonObject
            {
                { "request", new JsonPrimitive("GetDataAvailability") },
                { "service", new JsonPrimitive("SOS") },
                { "version", new JsonPrimitive("2.0.0") },
                { "procedure", new JsonPrimitive(procedure) },
                { "observedProperty", new JsonPrimitive(observedProperty) },
                { "featureOfInterest", new JsonPrimitive(featureOfInterest) }
            };
            return requestObject.ToString();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public static string BuildGetCapabilitiesRequest()
        {
            var requestObject = new JsonObject
            {
                { "request", new JsonPrimitive("GetCapabilities") },
                { "service", new JsonPrimitive("SOS") },
                { "sections", new JsonArray( new JsonValue[] { new JsonPrimitive("Contents") } ) }
            };
            return requestObject.ToString();
        }


        /// <summary>
        /// Create a data availability JSON object for a query to SOS. Variant for multiple procedures, properties and features
        /// </summary>
        /// <param name="procedures">Array of procedures</param>
        /// <param name="observedProperties">Array of properties</param>
        /// <param name="featuresOfInterest">Array of features of interest</param>
        /// <returns></returns>
        public static string BuildDataAvailabilityRequest(string[] procedures, string[] observedProperties, string[] featuresOfInterest)
        {
            var proceduresJSON = new JsonArray();
            var observedPropsJSON = new JsonArray();
            var featuresJSON = new JsonArray();
            foreach (string procedure in procedures) {
                proceduresJSON.Add(new JsonPrimitive(procedure));
            }
            foreach (string property in observedProperties)
            {
                proceduresJSON.Add(new JsonPrimitive(property));
            }
            foreach (string feature in featuresOfInterest)
            {
                proceduresJSON.Add(new JsonPrimitive(feature));
            }
            var requestObject = new JsonObject
            {
                { "request", new JsonPrimitive("GetDataAvailability") },
                { "service", new JsonPrimitive("SOS") },
                { "version", new JsonPrimitive("2.0.0") },
                { "procedure", proceduresJSON },
                { "observedProperty", observedPropsJSON },
                { "featureOfInterest", featuresJSON }
            };
            return requestObject.ToString();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="capabilitiesResult"></param>
        /// <returns></returns>
        public static StationsData ExtractStationData(string capabilitiesResult)
        {
            JsonValue values = JsonValue.Parse(capabilitiesResult);
            StationsData stationsData = new StationsData();
            if (values.ContainsKey("contents"))
            {
                var contents = values["contents"];
                for (int i = 0; i < contents.Count; i++)
                {
                    var entry = contents[i];
                    var identifier = entry["identifier"].ToString().Trim(new char[] { '"' });
                    var property = identifier.Split('.')[0];
                    var station = identifier.Split('@')[1];

                    if (stationsData.HasStation(station))
                    {
                        stationsData.AddCapabilityToExistingStation(station, property);
                    }
                    else
                    {
                        if (entry.ContainsKey("observedArea"))
                        {
                            var location = entry["observedArea"]["lowerLeft"];
                            var longitude = location[1];
                            var latitude = location[0];
                            stationsData.AddNewStation(station, latitude, longitude, property);
                        }
                    }
                }
            }

            Console.WriteLine(stationsData);
            return stationsData;
        }


        /// <summary>
        /// Create a TimeSeriesObject having a name, coordinates and time series from a JSON query result
        /// </summary>
        /// <param name="timeSeriesJsonResult">Input JSON having the result of a time series query</param>
        /// <returns>A TimeSeriesObject</returns>
        public static TimeSeriesObject ExtractTimeSeries(string timeSeriesJsonResult)
        {
            JsonValue values = JsonValue.Parse(timeSeriesJsonResult);
            TimeSeriesObject timeSeries = new TimeSeriesObject();
            var observations = values["observations"][0];
            if (observations.ContainsKey("featureOfInterest"))
            {
                var featureOfInterest = observations["featureOfInterest"];
                timeSeries.Name = featureOfInterest["name"];
                var coordinates = featureOfInterest["geometry"]["coordinates"];
                timeSeries.Coordinates = new Decimal[2] { coordinates[0], coordinates[1] };
            }
            if (observations.ContainsKey("result"))
            {
                // TODO: this code assumes that the resulting time-series have been aggregated.
                Dictionary<string, Decimal> series = new Dictionary<string, decimal>();
                var results = observations["result"]["values"];
                for (int i = 0; i < results.Count; i++)
                {
                    var entry = results[i];
                    series.Add(entry[0], entry[1]);
                }
                timeSeries.TimeSeries = series;
            }

            return timeSeries;
        }
    }
}
