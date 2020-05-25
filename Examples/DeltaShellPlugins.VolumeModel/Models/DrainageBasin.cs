using System.Collections.Generic;
using System.ComponentModel;
using DelftTools.Utils.Collections;
using DelftTools.Utils.Collections.Generic;
using GeoAPI.CoordinateSystems;
using GeoAPI.Extensions.Feature;
namespace DeltaShell.Plugins.VolumeModel.Models
{
    /// <summary>
    /// Drainage basin containing a set of catchments and a coordinatesystem.
    /// Implements INotifyPropertyChanged and INotifyCollectionChanged to handle events
    /// </summary>
    public class DrainageBasin : INotifyPropertyChanged, INotifyCollectionChanged
    {
        private ICoordinateSystem coordinateSystem;
        private readonly IEventedList<IFeature> catchments;
        public DrainageBasin()
        {
            // Add an empty (evented) list of features and subscribe to changes (bubble changes)
            catchments = new EventedList<IFeature>();
            catchments.CollectionChanged += (s, e) =>
            {
                if (CollectionChanged != null)
                {
                    CollectionChanged(s, e);
                }
            };
        }
        public ICoordinateSystem CoordinateSystem
        {
            get { return coordinateSystem; }
            set
            {
                coordinateSystem = value;
                // invoke property changed event after setting coordinateSystem
                if (PropertyChanged != null)
                {
                    PropertyChanged(this, new PropertyChangedEventArgs("CoordinateSystem"));
                }
            }
        }
        public IList<IFeature> Catchments
        {
            get { return catchments; }
        }
        public event PropertyChangedEventHandler PropertyChanged;
        public event NotifyCollectionChangedEventHandler CollectionChanged;
    }
}
