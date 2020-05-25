using System.Collections;
using System.ComponentModel;
using DeltaShell.Plugins.VolumeModel.Models;
using NetTopologySuite.Extensions.Features;
using SharpMap.Data.Providers;
namespace DeltaShell.Plugins.VolumeModel.Layers
{
    /// <summary>
    /// Defines a feature collection (used by layers for rendering) for a DrainageBasin
    /// </summary>
    public class DrainageBasinFeatureCollection : FeatureCollection
    {
        private readonly DrainageBasin drainageBasin;
        public DrainageBasinFeatureCollection(DrainageBasin drainageBasin)
            : base((IList)drainageBasin.Catchments, typeof(Feature))
        {
            this.drainageBasin = drainageBasin;
            // copy coordinatesystem and for monitor changes of the coordinatesystem
            CoordinateSystem = (GeoAPI.Extensions.CoordinateSystems.ICoordinateSystem)drainageBasin.CoordinateSystem;
            drainageBasin.PropertyChanged += DrainageBasinPropertyChanged;
        }
        private void DrainageBasinPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if (e.PropertyName == "CoordinateSystem")
            {
                CoordinateSystem = (GeoAPI.Extensions.CoordinateSystems.ICoordinateSystem)drainageBasin.CoordinateSystem;
            }
        }
        public override void Dispose()
        {
            // Desubscribe from drainageBasin so this DrainageBasinFeatureCollection can be disposed
            drainageBasin.PropertyChanged -= DrainageBasinPropertyChanged;
            base.Dispose();
        }
    }
}
