using System.Collections.Generic;
using DelftTools.Shell.Gui;
using DeltaShell.Plugins.VolumeModel.Models;
using SharpMap.Api.Layers;
using SharpMap.Layers;
namespace DeltaShell.Plugins.VolumeModel.Layers
{
    public class VolumeModelMapLayerProvider : IMapLayerProvider
    {
        /// <summary>
        /// Defines that layers can be provided for volume models and DrainageBasins
        /// </summary>
        public bool CanCreateLayerFor(object data, object parentData)
        {
            return data is Models.VolumeModel ||
                   data is DrainageBasin;
        }
        /// <summary>
        /// Creates a volume model group layer and DrainageBasin layer
        /// </summary>
        public ILayer CreateLayer(object data, object parentData)
        {
            var volumeModel = data as Models.VolumeModel;
            if (volumeModel != null)
            {
                return new GroupLayer(volumeModel.Name);
            }
            var drainageBasin = data as DrainageBasin;
            if (drainageBasin != null)
            {
                return new VectorLayer("Drainage basin")
                {
                    DataSource = new DrainageBasinFeatureCollection(drainageBasin)
                };
            }
            return null;
        }
        /// <summary>
        /// Returns all children for which a child layer should be created in volume model group layers
        /// </summary>
        public IEnumerable<object> ChildLayerObjects(object data)
        {
            var volumeModel = data as Models.VolumeModel;
            if (volumeModel != null)
            {
                // In the end a child layer should be created for both the basin input data and the volume output data
                yield return volumeModel.Basin;
                yield return volumeModel.Volume;
            }
        }
    }
}