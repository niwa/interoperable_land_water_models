using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Xml.Linq;
using DelftTools.Functions;
using DelftTools.Functions.Generic;
using DelftTools.Shell.Core;
using log4net;
namespace DeltaShell.Plugins.VolumeModel.Importers
{
    /// <summary>
    /// Importer for importing WaterML2 data to time series objects
    /// </summary>
    public class WaterML2TimeSeriesImporter : IFileImporter
    {
        private static readonly ILog Log = LogManager.GetLogger(typeof(WaterML2TimeSeriesImporter)); // Handle for writing log messages
        /// <summary>
        /// The name of the importer
        /// </summary>
        /// <remarks>Used in importer selection dialogs</remarks>
        public string Name
        {
            get { return "WaterML2 time series importer"; }
        }
        /// <summary>
        /// The category of the importer
        /// </summary>
        /// <remarks>Used in importer selection dialogs</remarks>
        public string Category
        {
            get { return "Volume model importers"; }
        }
        /// <summary>
        /// The image of the importer
        /// </summary>
        /// <remarks>Used in importer selection dialogs</remarks>
        public Bitmap Image
        {
            get { return DeltaShellPlugins.VolumeModel.Properties.Resources.weather_rain; }
        }
        /// <summary>
        /// The data types supported by the importer
        /// </summary>
        public IEnumerable<Type> SupportedItemTypes
        {
            get { yield return typeof(TimeSeries); }
        }
        /// <summary>
        /// Indicates that the importer can import at root level (folder/project). In other
        /// words, indicates that the <see cref="ImportItem"/> method can be called without
        /// specifying a time series target...
        /// </summary>
        public bool CanImportOnRootLevel
        {
            get { return true; }
        }
        /// <summary>
        /// The file filter of the importer
        /// </summary>
        /// <remarks>Used in file selection dialogs</remarks>
        public string FileFilter
        {
            get { return "WaterML2 files|*.XML"; }
        }
        /// <summary>
        /// Path where external data files can be copied into
        /// </summary>
        /// <remarks>Not relevant in this tutorial</remarks>
        public string TargetDataDirectory { get; set; }
        /// <summary>
        /// Whether or not an import task should be cancelled
        /// </summary>
        /// <remarks>Not part of this tutorial</remarks>
        public bool ShouldCancel { get; set; }
        /// <summary>
        /// Fired when progress has been changed
        /// </summary>
        /// <remarks>Not part in this tutorial</remarks>
        public ImportProgressChangedDelegate ProgressChanged { get; set; }
        /// <summary>
        /// Whether or not to try and open a view for the imported item
        /// </summary>
        public bool OpenViewAfterImport
        {
            get { return true; }
        }
        /// <summary>
        /// Check if an import can be done on the targetObject
        /// </summary>
        /// <param name="targetObject">Object to check</param>
        public bool CanImportOn(object targetObject)
        {
            return targetObject is TimeSeries;
        }
        /// <summary>
        /// Imports WaterML2 data from the file with path <paramref name="path"/> to the
        /// time series <paramref name="target"/>
        /// </summary>
        /// <remarks>
        /// The target parameter is optional. If a target time series is specified, the
        /// importer should import the WaterML2 data to this existing time series. When
        //  no target is set, the importer should create a new time series.
        /// </remarks>
        public object ImportItem(string path, object target = null)
        {
            // Check the file path
            if (!File.Exists(path))
            {
                Log.Error("File does not exist");
                return null;
            }
            // Obtain a new time series or check the provided target for being a time series
            var timeSeries = target == null
                ? new TimeSeries { Name = Path.GetFileNameWithoutExtension(path), Components = { new Variable<double>() } }
                : target as TimeSeries;
            if (timeSeries == null)
            {
                Log.Error("Target is of the wrong type (should be time series)");
                return null;
            }
            // Load the WaterML2 XML document
            var doc = XDocument.Load(path);
            // Obtain the document elements
            var xElements = doc.Descendants();
            // Obtain the measurement TVP tags
            var measurements = xElements.Where(element => element.Name.LocalName == "MeasurementTVP");
            // Get the corresponding time and value for each measurement tag
            foreach (var measurement in measurements)
            {
                var time = DateTime.Parse(measurement.Elements().First(e => e.Name.LocalName == "time").Value);
                var value = double.Parse(measurement.Elements().First(e => e.Name.LocalName == "value").Value);
                timeSeries[time] = value;
            }
            // Return the time series
            return timeSeries;
        }
    }
}
