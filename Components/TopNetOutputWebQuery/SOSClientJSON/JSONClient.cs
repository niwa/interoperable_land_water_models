using System.Text;
using System.Net.Http;

namespace SOSClientJSON
{
    /// <summary>
    /// Class for a JSON client to perform requests to the SOS server
    /// </summary>
    public class JSONClient
    {
        private readonly string BaseURL;

        /// <summary>
        /// Create a new instance of JSONClient
        /// </summary>
        /// <param name="BaseURL">URL that will be used for the queries</param>
        public JSONClient(string BaseURL)
        {
            this.BaseURL = BaseURL;
        }

        /// <summary>
        /// This returns the JSON corresponding to a simple test request
        /// </summary>
        /// <returns></returns>
        public string PerformTestRequest(string procedure)
        {
            var requestObject = Utils.JSONUtils.BuildJSONSOSTestRequest();
            var result = PerformRequest(requestObject);
            return result;
        }

        /// <summary>
        /// Create a data availability type of request
        /// </summary>
        /// <param name="observableProperty"></param>
        /// <param name="id"></param>
        /// <returns>JSON string for the query</returns>
        public string PerformDataAvailabilityRequest(string procedure, string observableProperty, string id)
        {
            var requestObject = Utils.JSONUtils.BuildDataAvailabilityRequest(procedure, observableProperty, id);
            var result = PerformRequest(requestObject);
            return result;
        }

        /// <summary>
        /// Perform a time series type of request to the SOS server. Everything gets packed on a TimeSeriesObject
        /// </summary>
        /// <param name="observableProperty"></param>
        /// <param name="id"></param>
        /// <param name="startTime"></param>
        /// <param name="endTime"></param>
        /// <returns></returns>
        public Utils.TimeSeriesObject PerformTimeSeriesRequest(string observableProperty, string id, string startTime, string endTime)
        {
            string[] time = new string[2] { startTime, endTime };
            var requestObject = Utils.JSONUtils.BuildTimeSeriesRequest("Hydrometric_Station", observableProperty, id, time);
            var result = PerformRequest(requestObject);
            return Utils.JSONUtils.ExtractTimeSeries(result);
            
        }

        public Utils.StationsData PerformCapabilitiesRequest()
        {
            string requestObject = Utils.JSONUtils.BuildGetCapabilitiesRequest();
            var result = PerformRequest(requestObject);
            return Utils.JSONUtils.ExtractStationData(result);
        }

        /// <summary>
        /// The actual request to the server POSTing the request object. TODO: there is no exception handling
        /// </summary>
        /// <param name="requestObject"></param>
        /// <returns></returns>
        public string PerformRequest(string requestObject)
        {
            HttpClient httpClient = new HttpClient();
            var content = new StringContent(requestObject, Encoding.UTF8, "application/json");
            var response = httpClient.PostAsync(BaseURL, content).Result;
            var result = "";
            if (response.IsSuccessStatusCode)
            {
                var textContent = response.Content.ReadAsStringAsync().Result;
                result = textContent;
            }
            else
            {
                // TODO: catch exceptions above, this is just a placeholder
                result = "ERROR";
            }
            return result;
        }
    }

}
