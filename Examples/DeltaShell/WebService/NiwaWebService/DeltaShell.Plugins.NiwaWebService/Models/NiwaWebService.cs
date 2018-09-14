using System;
using System.Collections.Generic;
using System.Net;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Schema;
using System.Runtime.Serialization;
using System.Threading.Tasks;
using System.Text;

using DelftTools.Functions;
using DelftTools.Functions.Generic;
using DelftTools.Shell.Core.Workflow;
using DelftTools.Shell.Core.Workflow.DataItems;
using DelftTools.Units;

using log4net;

/* JSON and Sensor Observatin Service library references 
 * using System.Runtime.Serialization.Json;
 * using SosLib;
 */

namespace DeltaShell.Plugins.NiwaWebService.Models
{
    /// <summary>
    /// "Models" flows by retrieving flow time series from a web service.
    /// 
    public class NiwaWebService : ModelBase
    {
        private static readonly ILog Log = LogManager.GetLogger(typeof(NiwaWebService));     // handle for writing log messages
        private readonly Uri serviceUri;
        private string WsRequest { get; set; }

        public NiwaWebService()
        {
            // serviceUri = new Uri("http://wellsensorobsp.niwa.co.nz:8080/52n-sos-aquarius-webapp/service");
            serviceUri = new Uri("http://hydro-sos.niwa.co.nz/");
            // request sensor descriptions
            StringBuilder builder = new StringBuilder("?service=SOS&version=2.0.0");
            builder.Append("&request=DescribeSensor&procedure=Hydrometric_Station");
            builder.Append("&procedureDescriptionFormat=http://www.opengis.net/sensorML/1.0.1");
            WsRequest = builder.ToString();

            AddDataItemSet<TimeSeries>(new List<TimeSeries>(), "Flows", DataItemRole.Output, "FlowTag", false);
            AddDataItemSet<TimeSeries>(new List<TimeSeries>(), "Stages", DataItemRole.Output, "StageTag", false);
        }

        protected override void OnInitialize()
        {
            if (!IsValidUri(serviceUri))
            {
                Log.Error($"{serviceUri} is not a valid web service identifier");
                throw new InvalidOperationException($"{serviceUri} is not a valid web service identifier");
            }
            Log.Debug($"Initializing Web Service model for {serviceUri}.");
        }

        protected override void OnExecute()
        {
            // ExecuteWithWebRequest();
            ExecuteWithWebClient();
        }

        // Uses System.Net.WebClient to send a request to NIWA's hydro-sos server
        // and retrieve a block of XML containing stage and discharge reports for 
        // a list of gages
        private void ExecuteWithWebClient()
        {
            string xmlContent;
            // build a request for observed stages and discharges as key-value pairs (KVPs)
            // for demo purposes, the gage IDs and time window are hard-coded
            StringBuilder builder = new StringBuilder("?service=SOS&version=2.0.0");
            builder.Append("&request=GetObservation");
            builder.Append("&featureOfInterest=91401,91403");
            builder.Append("&procedure=Hydrometric_Station");
            builder.Append("&temporalFilter=om:phenomenonTime,");
            builder.Append("2017-02-01T00:00:00Z/");
            builder.Append("2017-02-02T00:00:00Z");
            WsRequest = builder.ToString();
            try
            {
                using (WebClient client = new WebClient())
                {
                    xmlContent = client.DownloadString(serviceUri + WsRequest);
                }
            }
            catch (WebException we)
            {
                throw we;
            }

            List<TimeSeries> tsList = TSfromSosXmlString(xmlContent);

            var flowItems = this.GetDataItemSetByTag("FlowTag").AsEventedList<TimeSeries>();
            var stageItems = this.GetDataItemSetByTag("StageTag").AsEventedList<TimeSeries>();
            flowItems.Clear();
            stageItems.Clear();

            foreach (TimeSeries ts in tsList)
            {
                var sample = ts.Components.FirstOrDefault();
                if (sample.Name.ToUpper().Contains("HEIGHT") ||
                    sample.Name.ToUpper().Contains("STAGE"))
                    stageItems.Add(ts);
                if (sample.Name.ToUpper().Contains("DISCHARGE") ||
                    sample.Name.ToUpper().Contains("FLOW"))
                    flowItems.Add(ts);
            }
            Status = ActivityStatus.Done;
        }

        private static bool IsValidUri(Uri uri)
        {
            // Uri uri;
            // if (Uri.TryCreate(uriString, UriKind.RelativeOrAbsolute, out uri))
            // {
            if (Dns.GetHostAddresses(uri.DnsSafeHost).Length > 0)
            {
                return true;
            }
            // }
            return false;
        }

        public static List<TimeSeries> TSfromSosXmlString(string XmlContent)
        {
            //          Introducing an error into the XML for a test
            /*          Char[] sep = { ':' };
                        var woo = XmlContent.Split(sep, 2);
                        waterMLcontent = woo[0] + "x:" + woo[1];
            */
            MemoryStream XmlStream = new MemoryStream(System.Text.Encoding.Default.GetBytes(XmlContent));
            return TSfromSosXmlStream(XmlStream);

        }

        public static List<TimeSeries>TSfromSosXmlStream(Stream XmlStream)
        {
            //Create setting for XML reader based on SOS observation schema 
            // TODO: This is time-consuming because adding the schema requires a 
            // connection to the net. Could this be done in an initialization
            // phase and possibly spun off onto an ansychronous thread?
            XmlReaderSettings sosXmlSettings = new XmlReaderSettings();
            try
            {
                sosXmlSettings.Schemas.Add("http://www.opengis.net/sos/2.0", "http://schemas.opengis.net/sos/2.0/sos.xsd");
                sosXmlSettings.Schemas.Add("http://www.opengis.net/waterml/2.0", "http://schemas.opengis.net/waterml/2.0/waterml2.xsd");
                // Using local file reference, e.g. "C:/DSTest/WSModel/DeltaShell.Plugins.WSModel/Resources/sos.xsd" doesn't
                // make the load appreciably faster. Probably because the SOS definition itself contains outside references that 
                // have to be resolved and looked up over the net.
                sosXmlSettings.ValidationEventHandler += new ValidationEventHandler(ValidationEventHandler);
                sosXmlSettings.ValidationFlags = sosXmlSettings.ValidationFlags | XmlSchemaValidationFlags.ReportValidationWarnings;
                sosXmlSettings.ValidationType = ValidationType.Schema;
            }
            catch (Exception e)
            {
                //Console.Write(e.ToString());
                Log.Error(e.Message);
                throw e;
            }
            XmlReader reader = XmlReader.Create(XmlStream, sosXmlSettings);

            // Create an empty list of time series objects to be returned on successful 
            // decoding of the contents of the waterML string
            List<TimeSeries> result = new List<TimeSeries>();

            try
            {
                // Obtain the XML elements from the input string
                var xElements = XDocument.Load(reader).Descendants();

                // Obtain the observationData tags (SOS namespace)
                var observationDataElements = xElements.Where(element => element.Name.LocalName == "observationData");
                foreach (var odElement in observationDataElements)
                {
                    var observationMembers = odElement.Descendants().Where(element => element.Name.LocalName == "OM_Observation");
                    foreach (var observationMember in observationMembers)
                    {
                        var descendents = observationMember.Descendants();
                        var observedProperty = descendents.Where(element => element.Name.LocalName == "observedProperty").FirstOrDefault();
                        var parameter = descendents.Where(element => element.Name.LocalName == "parameter").FirstOrDefault();
                        string paramName = parameter.Value;
                        // var sampledFeature = descendents.Where(element => element.Name.LocalName == "sampledFeature").FirstOrDefault();
                        // string paramName = observedProperty == null ?
                        //     "parameter" : observedProperty.Attributes().Where(e => e.Name.LocalName == "title").FirstOrDefault().Value;
                        // string locName = sampledFeature == null ? 
                        //     "location" : sampledFeature.Attributes().Where(e => e.Name.LocalName == "title").FirstOrDefault().Value;

                        string tsCat = "category";
                        if (observedProperty.ToString().ToUpper().Contains("HEIGHT") ||
                            observedProperty.ToString().ToUpper().Contains("STAGE"))
                            tsCat = "Stage";
                        if (observedProperty.ToString().ToUpper().Contains("DISCHARGE") ||
                            observedProperty.ToString().ToUpper().Contains("FLOW"))
                            tsCat = "Flow";
                        TimeSeries fts = new TimeSeries { Components = { new Variable<double>(tsCat) } };
                        fts.Name = paramName.Trim();

                        var measurements = observationMember.Descendants().Where(element => element.Name.LocalName == "MeasurementTVP");
                        // Get the corresponding time and value for each measurement tag
                        foreach (var measurement in measurements)
                        {
                            var time = DateTime.Parse(measurement.Elements().First(e => e.Name.LocalName == "time").Value);
                            var value = double.Parse(measurement.Elements().First(e => e.Name.LocalName == "value").Value);
                            fts[time] = value;
                        }
                        //TimeSeries fts = (TimeSeries)Flow.Clone(true);
                        result.Add(fts);
                    }
                }
            }
            catch (XmlException x)
            {
                //Console.Write(x.ToString());
                Log.Error(x.Message);
                throw x;
            }
            catch (Exception x)
            {
                //Console.Write(x.ToString());
                Log.Error(x.Message);
                throw x;
            }
            return result;
        }

        public static void ValidationEventHandler(object sender, System.Xml.Schema.ValidationEventArgs args)
        {
            if (args.Severity == XmlSeverityType.Warning)
                //Console.Write("\nWARNING: ");
                Log.Warn(args.Message);
            else if (args.Severity == XmlSeverityType.Error)
                //Console.Write("\nERROR: ");
                Log.Error(args.Message);

            //Console.WriteLine(args.Message);
        }

        /*
        * Blocking out the JSON reading functions 

        // Uses System.Net.WebRequest to send a request to NIWA's hydro-sos server
        // and retrieve a JSON response containing stage and discharge reports for 
        // a list of gages
        private void ExecuteWithWebRequest()
        {
            string operation = "GetObservation";
            string returnAs = "json";
            string postAs = "xml";
            List<TimeSeries> tsList = null; // populated from web service's response
            try
            {
                // var request = WebRequest.Create(serviceUri + WsRequest) as HttpWebRequest;
                // request.Method = "GET";

                // request for sensor descriptions -- copied from example generated at NIWA site
                // http://wellsensorobsp.niwa.co.nz:8080/52n-sos-clidb-webapp/client
                // Available operations include: GetObservation, GetFeatureOfInterest, 
                // GetDataAvailability, GetCapabilities DescribeSensor

                // crudely building the request XML as string literals
                StringBuilder msgBuilder 
                    = new StringBuilder("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                string tmp = $"<sos:{operation} service=\"SOS\" version=\"2.0.0\" ";
                msgBuilder.Append(tmp);
                msgBuilder.Append("xmlns:sos=\"http://www.opengis.net/sos/2.0\" ");
                msgBuilder.Append("xmlns:fes=\"http://www.opengis.net/fes/2.0\" ");
                msgBuilder.Append("xmlns:gml=\"http://www.opengis.net/gml/3.2\" ");
                msgBuilder.Append("xmlns:swe=\"http://www.opengis.net/swe/2.0\" ");
                msgBuilder.Append("xmlns:xlink=\"http://www.w3.org/1999/xlink\" ");
                msgBuilder.Append("xmlns:swes=\"http://www.opengis.net/swes/2.0\" ");
                msgBuilder.Append("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ");
                msgBuilder.Append("xsi:schemaLocation=\"http://www.opengis.net/sos/2.0 ");
                msgBuilder.Append("http://schemas.opengis.net/sos/2.0/sos.xsd\"> ");


                // Aquarius
                msgBuilder.Append("<sos:procedure>Hydrometric_Station</sos:procedure> ");
                msgBuilder.Append("<sos:offering>HG.Master@91401</sos:offering> ");
                msgBuilder.Append("<sos:offering>QR.Master@91401</sos:offering> ");
                msgBuilder.Append("<sos:offering>HG.Master@91403</sos:offering> ");
                msgBuilder.Append("<sos:offering>QR.Master@91403</sos:offering> ");
                msgBuilder.Append("<sos:observedProperty>QR</sos:observedProperty> ");
                msgBuilder.Append("<sos:observedProperty>HG</sos:observedProperty> ");
                msgBuilder.Append("<sos:temporalFilter> ");
                msgBuilder.Append("<fes:During> ");
                msgBuilder.Append("<fes:ValueReference>phenomenonTime</fes:ValueReference> ");
                msgBuilder.Append("<gml:TimePeriod gml:id=\"tp_1\"> ");
                msgBuilder.Append("<gml:beginPosition>2017-02-01T00:00:00.000+13:00</gml:beginPosition> ");
                msgBuilder.Append("<gml:endPosition>2017-02-02T00:00:00.000+13:00</gml:endPosition> ");
                msgBuilder.Append("</gml:TimePeriod> ");
                msgBuilder.Append("</fes:During> ");
                msgBuilder.Append("</sos:temporalFilter> ");
                msgBuilder.Append("<sos:featureOfInterest>91401</sos:featureOfInterest> ");
                msgBuilder.Append("<sos:featureOfInterest>91403</sos:featureOfInterest> ");

                msgBuilder.Append("<sos:responseFormat>http://www.opengis.net/om/2.0</sos:responseFormat> ");
                msgBuilder.Append("</sos:GetObservation>");

                string msg = msgBuilder.ToString();
                var msgData = Encoding.UTF8.GetBytes(msgBuilder.ToString());
                var request = WebRequest.Create(serviceUri) as HttpWebRequest;
                request.Method = "POST";
                request.ContentType = "application/" + postAs;
                request.Accept = "application/" + returnAs;

                var reqStream = request.GetRequestStream();
                reqStream.Write(msgData, 0, msgData.Length);

                // var req = request.ToString();
                var response = request.GetResponse() as HttpWebResponse;
                var resStream = response.GetResponseStream();

                // Un-comment one of two ways to proceed:
                // 1. feed the stream directly to the decoder
                // tsList = TSfromSosJsonStream(resStream);
                // 2. copy the stream's contents to string, write them to a file,
                //      then decode the string
                // StreamReader respReader = new StreamReader(resStream);
                // responseContent = respReader.ReadToEnd();
                // File.WriteAllText($@"C:\temp\{operation}.{returnAs}", responseContent);
                // tsList = TSfromSosJsonString(responseContent);

                resStream.Close();
                reqStream.Close();
            }
            catch (Exception ex)
            {
                // Console.Write(ex.ToString());
                Log.Error(ex.Message);
                throw ex;
            }
            var flowItems = this.GetDataItemSetByTag("FlowTag").AsEventedList<TimeSeries>();
            var stageItems = this.GetDataItemSetByTag("StageTag").AsEventedList<TimeSeries>();
            flowItems.Clear();
            stageItems.Clear();

            foreach (TimeSeries ts in tsList)
            {
                var sample = ts.Components.FirstOrDefault();
                if (sample.Name.ToUpper().Contains("HEIGHT") ||
                    sample.Name.ToUpper().Contains("STAGE"))
                    stageItems.Add(ts);
                if (sample.Name.ToUpper().Contains("DISCHARGE") ||
                    sample.Name.ToUpper().Contains("FLOW"))
                    flowItems.Add(ts);
            }
            Status = ActivityStatus.Done;
        }


        // creates a list of TimeSeries objects from a JSON string
        public static List<TimeSeries> TSfromSosJsonString(string jsonSosContent)
        {
            //Create a memory stream and reader based from the json input 
            MemoryStream jsonStream = new MemoryStream(System.Text.Encoding.Default.GetBytes(jsonSosContent));
            List<TimeSeries> result;

            try
            {
                result = TSfromSosJsonStream(jsonStream);
            }
            catch (Exception x)
            {
                //Console.Write(x.ToString());
                Log.Error(x.Message);
                throw x;
            }
            return result;
        }

        public static List<TimeSeries> TSfromSosJsonStream(Stream jsonStream)
        {
            // Create an empty list of time series objects to be returned on successful 
            // decoding of the contents of the json string
            List<TimeSeries> result = new List<TimeSeries>();

            try
            {
                var formatter = new DataContractJsonSerializer(typeof(ObservationSequences));
                ObservationSequences sequences = formatter.ReadObject(jsonStream) as ObservationSequences;
                var seqEnum = sequences.TheSequences as IEnumerable<ObservationSequence>;
                //var obsSets = (from o in obsEnum
                //              select new { o.Foi.Identifier, o.ObservableProperty, o.Foi.Name, o.Result.Unit }).Distinct();
                jsonStream.Close();

                foreach (var seq in seqEnum)
                {
                    Unit obsUnit = new Unit(seq.SequenceResult.Units);
                    TimeSeries fts = new TimeSeries { Components = { new Variable<double>(seq.ObservableProperty, obsUnit) } };
                    fts.Name = $"{seq.ObservableProperty}@{seq.Foi.Name}";
                    // Get the corresponding time and value for each measurement
                    for (int i = 0; i < seq.SequenceResult.Values.Length; i++)
                    {
                        fts[seq.SequenceResult.Values[i].time] = seq.SequenceResult.Values[i].quantity;
                    }
                    //TimeSeries fts = (TimeSeries)Flow.Clone(true);
                    result.Add(fts);
                    }
                        Log.Info($"Read {result.Count} time series from JSON message.");
                }
            catch (Exception x)
            {
                //Console.Write(x.ToString());
                Log.Error(x.Message);
                throw x;
            }
            return result;
        }
        JSON methods end here */

    }
}
