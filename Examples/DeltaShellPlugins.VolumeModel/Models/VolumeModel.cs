using System;
using System.Linq;
using DelftTools.Functions;
using DelftTools.Functions.Generic;
using DelftTools.Shell.Core.Workflow;
using DelftTools.Shell.Core.Workflow.DataItems;
using GeoAPI.Extensions.Feature;
using GeoAPI.Geometries;
using NetTopologySuite.Extensions.Coverages;
using System.IO;




namespace DeltaShell.Plugins.VolumeModel.Models
{
  
    public class VolumeModel : ModelBase
    {
        private readonly DrainageBasin basin;
        private readonly TimeSeries precipitation;
        private readonly FeatureCoverage volume;
       

        /// <summary>
        /// Creates a volume model
        /// </summary>
        public VolumeModel()
        {

            // Create the input items of the volume model
            basin = new DrainageBasin();
            precipitation = new TimeSeries { Components = { new Variable<double>("Precipitation") } };
            // Declare a two dimensional array.
            //double[,] array1 = new double[2, 3];

            // Create the output item of the volume model
            volume = new FeatureCoverage("Output data")
            {
                IsTimeDependent = true,
                Arguments = { new Variable<IFeature>("Catchment") { FixedSize = 0 } },
                Components = { new Variable<double>("Volume") }
            };

            

            // Wrap fields as input/output data items

            DataItems.Add(new DataItem(precipitation, "Precipitation", typeof(TimeSeries), DataItemRole.Input, "PrecipitationTag"));
            DataItems.Add(new DataItem(basin, "Basin", typeof(DrainageBasin), DataItemRole.Input, "BasinTag"));
            DataItems.Add(new DataItem(volume, "NIWA_model_value", typeof(FeatureCoverage), DataItemRole.Output, "VolumeTag"));
        }
       
        /// The precipitation time series: P = P(t) [L/T]. Input of the model.
        /// </summary>
        public TimeSeries Precipitation
        {
            get { return precipitation; }
        }
        /// <summary>
        /// The drainage basin (set of catchments). Input of the model.
        /// </summary>
        public DrainageBasin Basin
        {
            get { return basin; }
        }
        /// <summary>
        /// Time-dependent feature coverage containing the volume of water per catchment: V = V(t, c) [L3/T]. Output of the model.
        /// </summary>
        public FeatureCoverage Volume
        {
            get { return volume; }
        }
        
        /// <summary>
        /// The initialization of model runs
        /// </summary>
        /// 
        
        protected override void OnInitialize()
        {
            
            // Clear any previous output
            volume.Clear();
            // Ensure the coordinate system of the volume output is the same as the catchments input (basin)
            volume.CoordinateSystem = (GeoAPI.Extensions.CoordinateSystems.ICoordinateSystem)basin.CoordinateSystem;
            // Ensure at least one catchment and one precipitation value is present
            ValidateInputData();
            // Initialize the output feature coverage
            volume.Features.AddRange(basin.Catchments);
            volume.FeatureVariable.FixedSize = basin.Catchments.Count();
            volume.FeatureVariable.AddValues(basin.Catchments);
            
          
        }
        /// <summary>
        /// The actual calculation during model run
        /// </summary>
        protected override void OnExecute()
        {
            // set up the array here
            // Loop all times
            double[,] Sary = new double[15, basin.Catchments.Count];
            //read from csv file for now 
            // Get the file's text.
            string result = Path.GetTempPath();

            var path = result+@"\test2.csv";
            string whole_file = System.IO.File.ReadAllText(path);
            
            // Split into lines.
            whole_file = whole_file.Replace('\n', '\r');
            string[] lines = whole_file.Split(new char[] { '\r' },
                StringSplitOptions.RemoveEmptyEntries);

            // See how many rows and columns there are.
            int num_rows = lines.Length;
            int num_cols = lines[0].Split(',').Length;

            // Allocate the data array.
            string[,] values = new string[num_rows, num_cols];

            // Load the array.
            for (int r = 0; r < num_rows; r++)
            {
                string[] line_r = lines[r].Split(',');
                for (int c = 0; c < num_cols; c++)
                {
                    Sary[c, r] = Convert.ToDouble(line_r[c]);
                }
            }

            int j = 0;
            int jj = 0;
            foreach (var time in precipitation.Time.Values)             
                {
                    // Obtain the precipitation value for the current time
                    var p = (double)precipitation[time];
                
                // Calculate a volume value for every catchment based on catchment area and precipitation value
                //var volumes = basin.Catchments.Select(f => f.Geometry).Select(pol => pol.Area * p)
                //var volumes = basin.Catchments.Select(f => p);
                // 
                //double[,] Sary = new double[15, basin.Catchments.Count];
                //Random rand = new Random(Guid.NewGuid().GetHashCode());
              
                //for (int i = 0; i < basin.Catchments.Count; i++)
                 //   Sary[j, i] = rand.NextDouble();
                //Random rand = new Random(Guid.NewGuid().GetHashCode());
                double[] colval = new double[basin.Catchments.Count];
               
                
                for (int i = 0; i < basin.Catchments.Count; i++)
                    colval[i] = Sary[jj, i];
                jj = jj + 1;
                //var volumes = Array1;
                // Add the calculated volume values to the output feature coverage
                volume[time] = colval;
                }
            
            Status = ActivityStatus.Done;
        }
        private void ValidateInputData()
        {
            var hasCatchments = basin.Catchments.Any();
            var hasPrecipitationData = precipitation.Time.Values.Any();
            if (!hasCatchments && !hasPrecipitationData)
            {
                throw new InvalidOperationException("At least one catchment and one precipitation value should be present");
            }
            if (!hasCatchments)
            {
                throw new InvalidOperationException("At least one catchment should be present");
            }
            if (!basin.Catchments.All(c => c.Geometry is IPolygon || c.Geometry is IMultiPolygon))
            {
                throw new InvalidOperationException("All catchment features should be polygons");
            }
            if (!hasPrecipitationData)
            {
                throw new InvalidOperationException("At least one precipitation value should be present");
            }
        }
    }
}
